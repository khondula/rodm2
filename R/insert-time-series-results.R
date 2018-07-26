
#' Insert time series data values to ODM2 database
#'
#' @param db database connection
#' @param datavalues data frame with columns "Timestamp" with
#'    POSIXct YYYY-MM-DD H:M:S format, and column names corresponding to variable names
#' @param method code for method used to collect data
#' @param site_code sampling feature code at which data were collected
#' @param variables a named list defining units for variable names in datavalues data frame
#'    with format list("variablen1ame" = "units1name", "variable2name" = "units2name")
#' @param sampledmedium term from controlled vocabulary for medium sampled eg. Air, Water, Soil
#' @param processinglevel code for processing level. will be added to processinglevels table if new.
#'   defaults to "Raw data'.
#' @param actionby (optional) the person who performed the action
#' @param equipment (optional) the equipment used to collect the data
#' @param aggregationstatisticcv term from controlled vocabulary
#' @param zlocation (optional) z location offset
#' @param zlocationunits (optional but required if zlocation is set) name of units of z location offset
#' @param ... parameters to pass to various db_describe_ functions
#'
#' @return true if successful
#' @export
#'
#' @examples
#' \dontrun{
#' db <- rodm2::create_sqlite(connect = TRUE)
#' tsrv <- data.frame(Timestamp = "2018-06-27 13:55:00",
#' "Wind direction" = 180, "Wind speed" = 1, "Wind gust speed" = 2)
#' db_insert_results_ts(db = db, datavalues = tsrv,
#' method = "SonicAnemometer", site_code = "BB2",
#' variables = list("Wind direction" = "Degree",
#' "Wind speed" = "Meter per Second", "Wind gust speed" = "Meter per Second"),
#' processinglevel = "Raw data",
#' sampledmedium = "Air")
#' }
db_insert_results_ts <- function(db,
                                 datavalues,
                                 method,
                                 site_code,
                                 variables,
                                 sampledmedium,
                                 processinglevel = "Raw data",
                                 actionby = NULL,
                                 equipment = NULL,
                                 aggregationstatisticcv = NULL,
                                 zlocation = NULL,
                                 zlocationunits = NULL, ...){
  # if (!class(db) %in% c("SQLiteConnection", "PostgreSQLConnection")) {
  #   stop("sorry, only sqlite and postgresql database connections are supported so far")}

  if (!class(db) %in% c("SQLiteConnection")) {
    stop("sorry, only sqlite database connections are supported so far")}

  # check type of database object
  if (class(db) == "SQLiteConnection"){
    # check for method and add if not in there
    if(!(method %in% rodm2::db_get_methods(db))){
      rodm2::db_describe_method(db, methodname = method, methodcode = method,
                                methodtypecv = "Instrument deployment")
    }

    # make sure site is in sampling features table
    if(!site_code %in%
       DBI::dbGetQuery(db, "SELECT samplingfeaturecode
                       FROM samplingfeatures
                       WHERE samplingfeaturetypecv = 'Site'")){
      rodm2::db_describe_site(db, site_code)
  }

    if(!processinglevel %in%
       DBI::dbGetQuery(db, "SELECT processinglevelcode from processinglevels")){
      sql <- "insert into processinglevels (processinglevelcode) VALUES (:processinglevel)"
      sql <- RSQLite::dbSendQuery(db, sql)
      RSQLite::dbBind(sql, params = list(processinglevel = processinglevel))
      RSQLite::dbClearResult(res = sql)
    }

    # check that all variables are in variables table
    vars_to_add <- setdiff(names(variables), rodm2::db_get_variables(db)$VariableNameCV)
    for(newvar in vars_to_add){
      rodm2::db_describe_variable(db, "Unknown", newvar, newvar)
    }

    sql1 <- 'INSERT into actions
(actiontypecv, methodid, begindatetime, begindatetimeutcoffset, enddatetime)
    VALUES
    ("Instrument deployment",
    (SELECT methodid from methods WHERE methodcode = :method),
    DATETIME(:begindatetime), :begindatetimeutcoffset, DATETIME(:enddatetime))'

    sql1 <- RSQLite::dbSendQuery(db, sql1)
    RSQLite::dbBind(sql1, params = list(method = method,
                                        begindatetime = format(datavalues[["Timestamp"]][1],
                                                               "%Y-%m-%d %H:%M:%S"),
                                        begindatetimeutcoffset = as.integer(format(datavalues[["Timestamp"]][1],
                                                                                   "%z")),
                                        enddatetime = format(datavalues[["Timestamp"]][nrow(datavalues)],
                                                             "%Y-%m-%d %H:%M:%S")))
    RSQLite::dbClearResult(res = sql1)
    newactionid <- as.integer(RSQLite::dbGetQuery(db, "SELECT LAST_INSERT_ROWID()"))

    if(!is.null(actionby)){
      if(!(actionby %in% rodm2::db_get_people(db))){
        rodm2::db_describe_person(db, PersonFirstName = actionby, ...)
      }
      sql2 <- RSQLite::dbSendStatement(db, 'INSERT into actionby (actionid, affiliationid, isactionlead)
                                       VALUES
                                       (:newactionid,
                                       (SELECT affiliationid FROM affiliations
                                       WHERE personid = (SELECT personid FROM people WHERE personfirstname = :actionby)),
                                       "TRUE")')
      RSQLite::dbBind(sql2, params = list(newactionid = newactionid,
                                          actionby = actionby))
    }

    if(!is.null(equipment)){
      # db get equipment function
      # describe equipment
    }


    # insert new feature action
    sql3 <- RSQLite::dbSendStatement(db, 'INSERT into featureactions
                                     (actionid, samplingfeatureid)
                                     VALUES
                                     (:newactionid,
                                     (SELECT samplingfeatureid
                                     FROM samplingfeatures
                                     WHERE samplingfeaturecode = :site_code))')
    RSQLite::dbBind(sql3, params = list(newactionid = newactionid,
                                        site_code = site_code))
    RSQLite::dbClearResult(res = sql3)
    newfaid <- as.integer(dbGetQuery(db, "SELECT LAST_INSERT_ROWID()"))

    # add result! new result for each
    newresultids <- c()
    # newresultids <- vector(mode = "integer", length = length(variables))
    for(i in names(variables)){
      sql4 <- RSQLite::dbSendStatement(db, 'INSERT into results
                                       (resultuuid, featureactionid, resulttypecv,
                                       variableid, unitsid, processinglevelid,
                                       sampledmediumcv, valuecount)
                                       VALUES
                                       (:uuid, :newfaid, :resulttypecv,
                                       (SELECT variableid FROM variables WHERE variablecode = :variablecode),
                                       (SELECT unitsid FROM units WHERE unitsname = :units),
                                       (SELECT processinglevelid FROM processinglevels WHERE processinglevelcode = :processinglevel),
                                       :sampledmedium, :valuecount)')
      RSQLite::dbBind(sql4, params = list(uuid = uuid::UUIDgenerate(),
                                          newfaid = newfaid,
                                          resulttypecv = 'Time series coverage',
                                          variablecode = i,
                                          units = variables[[i]],
                                          processinglevel = processinglevel,
                                          sampledmedium = sampledmedium,
                                          valuecount = nrow(datavalues)
      ))
      RSQLite::dbClearResult(res = sql4)
      newresultids <- append(newresultids, as.integer(dbGetQuery(db, "SELECT LAST_INSERT_ROWID()")))
    }
    names(newresultids) <- names(variables)

    # for each of the new results, insert into time series results
    for(i in unname(newresultids)){
      sql5 <- 'INSERT into timeseriesresults
      (resultid, aggregationstatisticcv, zlocation, zlocationunitsid)
      VALUES (:resultid, :aggstatcv, :zlocation,
      (SELECT unitsid FROM units WHERE unitsname = :zlocationunits))'
      sql5 <- RSQLite::dbSendStatement(db, sql5)
      RSQLite::dbBind(sql5, params = list(resultid = i,
                                          aggstatcv = ifelse(is.null(aggregationstatisticcv),
                                          "Unknown", aggregationstatisticcv),
                                          zlocation = ifelse(is.null(zlocation), "", zlocation),
                                          zlocationunits = ifelse(is.null(zlocationunits), "", zlocationunits)))
      RSQLite::dbClearResult(res = sql5)
    }

    # then insert into timeseriesresultvalues
    for(i in names(newresultids)){
      # subset data values
      datavalues_var <- datavalues[, c("Timestamp", i)]
      # make data frame to append
      names(datavalues_var) <- c("valuedatetime", "datavalue")
      datavalues_var$valuedatetimeutcoffset = as.integer(format(datavalues_var$valuedatetime, "%z"))
      datavalues_var$valuedatetime <- format(datavalues_var$valuedatetime, "%Y-%m-%d %H:%M:%S")
      datavalues_var$resultid <- as.integer(newresultids[[i]])
      datavalues_var$censorcodecv = "Unknown"
      datavalues_var$qualitycodecv = "Unknown" # allow for column to be specified...
      datavalues_var$timeaggregationinterval = 5
      timeaggunitsid <- RSQLite::dbGetQuery(db, "select unitsid from units where unitsname = 'Minute'")
      datavalues_var$timeaggregationintervalunitsid = as.integer(timeaggunitsid)
      # append
      RSQLite::dbAppendTable(db, "timeseriesresultvalues", datavalues_var)
    }

  }

}


