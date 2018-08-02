# input data frame: site, datetime, variable

#' Insert time series data values to ODM2 database
#'
#' @param db database connection
#' @param datavalues data frame with columns "Timestamp" with
#'    POSIXct YYYY-MM-DD H:M:S format, and column names corresponding to variable names
#' @param method code for method used to collect data
#' @param variables a named list of lists defining variable names, units, and columns in datavalues data frame
#'    with format list("variablen1ame" = list(units = 'unitsname', column = 'colname', dataqualitycol = 'qualcode'))
#' @param sampledmedium term from controlled vocabulary for medium sampled eg. Air, Water, Soil
#' @param processinglevel code for processing level. will be added to processinglevels table if new.
#'   defaults to "Raw data'.
#' @param actionby (optional) the person who performed the action
#' @param equipment_name (optional) the equipment used to collect the data
#' @param zlocation (optional) z location offset
#' @param zlocationunits (optional but required if zlocation is set) name of units of z location offset
#' @param ... parameters to pass to various db_describe_ functions
#' @param site_code_col name of column with site codes. defaults to "site"
#' @param time_aggregation_interval
#' @param aggregationstatistic
#' @param methodtypecv
#'
#' @return true if successful
#' @export
#'
#' @examples
#' \dontrun{
#' # make database and data frame
#' db <- rodm2::create_sqlite(connect = TRUE)
#' mrv <- data.frame(Timestamp = "2018-06-27 13:55:00",
#' "vwc" = 68.3, site = "BB2")
#'
#' db_insert_results_m(db = db, datavalues = mrv,
#'     method = "soilmoisture", site_code_col = "site",
#'     variables = list("Volumetric water content" = list("vwc"," Percent")),
#'     sampledmedium = "Soil")
#' }
#'
db_insert_results_m <- function(db,
                                 datavalues,
                                 method,
                                 site_code_col = "site",
                                 variables,
                                 sampledmedium,
                                 processinglevel = "Raw data",
                                 aggregationstatistic = "Unknown",
                                 time_aggregation_interval = list(1, "Minute"),
                                 methodtypecv = 'Instrument deployment',
                                 actionby = NULL,
                                 equipment_name = NULL,
                                 zlocation = NULL,
                                 zlocationunits = NULL, ...){
  # if (!class(db) %in% c("SQLiteConnection", "PostgreSQLConnection")) {
  #   stop("sorry, only sqlite and postgresql database connections are supported so far")}

  if (!class(db) %in% c("SQLiteConnection", "PostgreSQLConnection")) {
    stop("sorry, only sqlite and postgres database connections are supported so far")}

  # check for method and add if not in there
  if(!(method %in% rodm2::db_get_methods(db))){
    rodm2::db_describe_method(db, methodname = method, methodcode = method,
                              methodtypecv = 'Instrument deployment')
  }


  # check that all variables are in variables table
  vars_to_add <- setdiff(names(variables), rodm2::db_get_variables(db)[[1]])
  for(newvar in vars_to_add){
    rodm2::db_describe_variable(db, "Unknown", newvar, newvar)
  }

  # check type of database object
  if (class(db) == "SQLiteConnection"){
    if(!(method %in% rodm2::db_get_methods(db))){
      rodm2::db_describe_method(db, methodname = method, methodcode = method,
                                methodtypecv = methodtypecv)
    }

    # make sure all sites in sampling features table
    for(site_code in mrv[[site_code_col]]){
      if(!site_code %in% rodm2::db_get_sites(db)){
        rodm2::db_describe_site(db, site_code)
      }
    }

    sql <- "INSERT or IGNORE into processinglevels (processinglevelcode) VALUES (:processinglevel)"
    sql <- RSQLite::dbSendQuery(db, sql)
    RSQLite::dbBind(sql, params = list(processinglevel = processinglevel))
    RSQLite::dbClearResult(res = sql)

    # check that all variables are in variables table
    for(newvar in names(variables)){
      sql <- "INSERT OR IGNORE into variables
      (variabletypecv, variablecode, variablenamecv, nodatavalue)
      VALUES
      (:variabletypecv, :variablecode, :variablenamecv, :nodatavalue)"
      sql <- RSQLite::dbSendQuery(db, sql)
      RSQLite::dbBind(sql, params = list(variabletypecv = "Unknown",
                                         variablecode = newvar,
                                         variablenamecv = newvar,
                                         nodatavalue = '-9999'))
      RSQLite::dbClearResult(res = sql)

      if(!is.null(variables[[newvar]]$nodatavalue)){
        sql <- RSQLite::dbSendQuery(db,
                                    "UPDATE variables SET nodatavalue = :nodatavalue
                                    WHERE variablecode = :variablecode")
        RSQLite::dbBind(sql, params = list(nodatavalue = variables[[newvar]]$nodatavalue,
                                           variablecode = newvar))
        RSQLite::dbClearResult(res = sql)
      }
      if(!is.null(variables[[newvar]]$variabletypecv)){
        sql <- RSQLite::dbSendQuery(db,
                                    "UPDATE variables SET variabletypecv = :variabletypecv
                                    WHERE variablecode = :variablecode")
        RSQLite::dbBind(sql, params = list(variabletypecv = variables[[newvar]]$variabletypecv,
                                           variablecode = newvar))
        RSQLite::dbClearResult(res = sql)
      }
      if(!is.null(variables[[newvar]]$variabledefinition)){
        sql <- RSQLite::dbSendQuery(db,
                                    "UPDATE variables SET variabledefinition = :variabledefinition
                                    WHERE variablecode = :variablecode")
        RSQLite::dbBind(sql, params = list(variabledefinition = variables[[newvar]]$variabledefinition,
                                           variablecode = newvar))
        RSQLite::dbClearResult(res = sql)
      }
      message(paste(newvar, "has been added to the Variables table."))
    }

    #######################################
    # for each ROW of MRV data frame

    db_insert_one_mrv <- function(mrv_id){
      sql1 <- RSQLite::dbSendQuery(db, 'INSERT into actions
                                   (actiontypecv, methodid, begindatetime, begindatetimeutcoffset)
                                   VALUES
                                   ("Instrument deployment",
                                   (SELECT methodid from methods WHERE methodcode = :method),
                                   DATETIME(:begindatetime), :begindatetimeutcoffset)')

      RSQLite::dbBind(sql1, params = list(method = method,
                                          begindatetime = format(as.POSIXct(
                                            datavalues[["Timestamp"]][mrv_id]), "%Y-%m-%d %H:%M:%S"),
                                          begindatetimeutcoffset = as.integer(format(as.POSIXct(
                                            datavalues[["Timestamp"]][mrv_id]), "%z"))))
      RSQLite::dbClearResult(res = sql1)
      newactionid <- as.integer(RSQLite::dbGetQuery(db, "SELECT LAST_INSERT_ROWID()"))

      ##################
      if(!is.null(actionby)){
        if(!(actionby %in% rodm2::db_get_people(db))){
          rodm2::db_describe_person(db, PersonFirstName = actionby, ...)
        }
        sql2 <- RSQLite::dbSendStatement(db, 'INSERT into actionby
                                         (actionid, affiliationid, isactionlead)
                                         VALUES
                                         (:newactionid,
                                         (SELECT affiliationid FROM affiliations
                                         WHERE personid = (SELECT personid FROM people
                                         WHERE personfirstname = :actionby)),
                                         "TRUE")')
        RSQLite::dbBind(sql2, params = list(newactionid = newactionid,
                                            actionby = actionby))
        RSQLite::dbClearResult(res = sql2)
      }
      ###########################
      if(!is.null(equipment_name)){
        # db describe equipment function
        if(!(equipment_name %in% rodm2::db_get_equipment(db))){
          rodm2::db_describe_equipment(db, equip_name = equipment_name, ...)
        }
        # add to equipment used
        sql2b <- RSQLite::dbSendStatement(db, 'INSERT into equipmentused (actionid, equipmentid)
                                          VALUES
                                          (:newactionid,
                                          (SELECT equipmentid FROM equipment
                                          WHERE equipmentcode = :equipmentcode))')
        RSQLite::dbBind(sql2b, params = list(newactionid = newactionid,
                                             equipmentcode = equipment_name))
        RSQLite::dbClearResult(res = sql2b)
      }
      ################################
      # insert new feature action
      sql3 <- RSQLite::dbSendStatement(db, 'INSERT into featureactions
                                       (actionid, samplingfeatureid)
                                       VALUES
                                       (:newactionid,
                                       (SELECT samplingfeatureid
                                       FROM samplingfeatures
                                       WHERE samplingfeaturecode = :site_code))')
      RSQLite::dbBind(sql3, params = list(newactionid = newactionid,
                                          site_code = mrv[[site_code_col]][mrv_id]))
      RSQLite::dbClearResult(res = sql3)
      newfaid <- as.integer(DBI::dbGetQuery(db, "SELECT LAST_INSERT_ROWID()"))
      #################################

      # add result! new result for each variable
      # for variables list, if no 'column', make col = name
      for(j in names(variables)){
        if(!"column" %in% names(variables[[j]])){
          variables[[j]][["column"]] <- j
        }
      }

      newresultids <- c()

      for(j in names(variables)){
        sql4 <- RSQLite::dbSendStatement(db, 'INSERT into results
                                         (resultuuid, featureactionid, resulttypecv,
                                         variableid, unitsid, processinglevelid,
                                         sampledmediumcv, valuecount)
                                         VALUES
                                         (:uuid, :newfaid, :resulttypecv,
                                         (SELECT variableid FROM variables
                                         WHERE variablenamecv = :variablenamecv),
                                         (SELECT unitsid FROM units WHERE unitsname = :units),
                                         (SELECT processinglevelid FROM processinglevels
                                         WHERE processinglevelcode = :processinglevel),
                                         :sampledmedium, :valuecount)')
        RSQLite::dbBind(sql4, params = list(uuid = uuid::UUIDgenerate(),
                                            newfaid = newfaid,
                                            resulttypecv = 'Measurement',
                                            variablenamecv = j,
                                            units = variables[[j]][["units"]],
                                            processinglevel = processinglevel,
                                            sampledmedium = sampledmedium,
                                            valuecount = 1
        ))
        RSQLite::dbClearResult(res = sql4)
        newresultids <- append(newresultids, as.integer(DBI::dbGetQuery(db, "SELECT LAST_INSERT_ROWID()")))
      }
      names(newresultids) <- names(variables)
      #######################################

      # for each of the new results, insert into measurement results
      for(j in names(newresultids)){

        qualitycodecv <- dplyr::select(datavalues, variables[[j]][["qualitycodecol"]])

        if(is.null(variables[[j]][["qualitycodecol"]])){
          qualitycodecv <- "Unknown"
        }

        censorcodecv <- dplyr::select(datavalues, variables[[j]][["censorcodecol"]])

        if(is.null(variables[[j]][["censorcodecol"]])){
          censorcodecv <- "Unknown"
        }

        sql5 <- 'INSERT into measurementresults
        (resultid, aggregationstatisticcv, zlocation, zlocationunitsid,
        timeaggregationinterval, timeaggregationintervalunitsid, censorcodecv, qualitycodecv)
        VALUES (:resultid, :aggstatcv, :zlocation,
        (SELECT unitsid FROM units WHERE unitsname = :zlocationunits),
        :timeaggregationinterval,
        (SELECT unitsid FROM units WHERE unitsname = :timeaggregationintervalunits),
        :censorcodecv, :qualitycodecv)'
        sql5 <- RSQLite::dbSendStatement(db, sql5)
        RSQLite::dbBind(sql5, params = list(resultid = newresultids[[j]],
                                            aggstatcv = ifelse(is.null(aggregationstatistic),
                                                               "Unknown", aggregationstatistic),
                                            zlocation = ifelse(is.null(zlocation), "", zlocation),
                                            zlocationunits = ifelse(is.null(zlocationunits),
                                                                    "", zlocationunits),
                                            timeaggregationinterval = time_aggregation_interval[[1]],
                                            timeaggregationintervalunits = time_aggregation_interval[[2]],
                                            censorcodecv = ifelse(is.null(variables[[j]]$censorcodecol),
                                                                  "Unknown", variables[[j]]$censorcodecol),
                                            qualitycodecv = ifelse(is.null(variables[[j]]$qualitycodecol),
                                                                   "Unknown", variables[[j]]$qualitycodecol)
        ))
        RSQLite::dbClearResult(res = sql5)
      }
      ################################
      # then insert into measurementresultvalues
      for(j in names(newresultids)){
        # subset data values
        var_colname <- variables[[j]]$column
        # make data frame to append
        datavalues_var <- datavalues %>%
          dplyr::slice(mrv_id) %>%
          dplyr::select(Timestamp, var_colname) %>%
          dplyr::rename(valuedatetime = Timestamp,
                        datavalue = var_colname) %>%
          dplyr::mutate(valuedatetimeutcoffset = as.integer(format(as.POSIXct(valuedatetime), "%z")),
                        valuedatetime = format(as.POSIXct(valuedatetime), "%Y-%m-%d %H:%M:%S"),
                        resultid = as.integer(newresultids[[j]]))
        # append
        RSQLite::dbAppendTable(db, "measurementresultvalues", datavalues_var)
      }
    }

    purrr::map(1:nrow(mrv), db_insert_one_mrv)
  }

  if (class(db) == "PostgreSQLConnection"){
    # make sure site is in sampling features table
  }


}


