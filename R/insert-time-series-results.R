
#' Insert time series data values to ODM2 database
#'
#' @param db database connection
#' @param datavalues data frame with columns "Timestamp" with
#'    POSIXct YYYY-MM-DD H:M:S format, and column names corresponding to variable names
#' @param method code for method used to collect data
#' @param site_code sampling feature code at which data were collected
#' @param variables a named list of lists defining variable names, units, and columns in datavalues data frame
#'    with format list("variablen1ame" = list(units = 'unitsname', column = 'colname', dataqualitycol = 'qualcode'))
#' @param sampledmedium term from controlled vocabulary for medium sampled eg. Air, Water, Soil
#' @param processinglevel code for processing level. will be added to processinglevels table if new.
#'   defaults to "Raw data'.
#' @param actionby (optional) the person who performed the action
#' @param equipment_name (optional) the equipment used to collect the data
#' @param aggregationstatisticcv term from controlled vocabulary
#' @param zlocation (optional) z location offset
#' @param zlocationunits (optional but required if zlocation is set) name of units of z location offset
#' @param ... parameters to pass to various db_describe_ functions
#'
#' @details
#' Timezone: The values in the "Timestamp" column must have time in the format YYYY-MM-DD HH:MM:SS,
#' but they can be character data or POSIXct. If they are character data, the time zone
#' will be your computer's local time, i.e. whatever is returned from Sys.time().
#' To specify a different time zone, such as if you are uploading data that was collected
#' before or after daylight savings time or you are using UTC, make sure that the dates
#' in the Timestamp column are of POSIXct format with a timezone specified.
#' You can check the timezone of an object using the function lubridate::tz().
#' In the database, datetime values are stored in one column and timezones are stored in
#' a separate column (as "UTC offset"). In the upload function, the UTC offset is an intger
#' determined by using format(as.POSIXct(x), "%z").
#'
#' @return true if successful
#' @export
#'
#' @examples
#' #db <- rodm2::create_sqlite(connect = TRUE)
#'
#' #tsrv <-data.frame(
#'  # Timestamp = c("2018-06-27 13:45:00", "2018-06-27 13:55:00"),
#'   #"wd" = c(180,170),
#'  # "ws" = c(1, 1.5),
#'  # "gustspeed" = c(2, 2.5))
#'
#' #db_insert_results_ts(
#'  # db = db,
#'  # datavalues = tsrv,
#'  # method = "SonicAnemometer",
#'  # site_code = "BB2",
#'  #  variables = list("Wind direction" = list("wd", "Degree"),
#'  #                   "Wind speed" = list("ws", "Meter per Second"),
#'  #                    "Wind gust speed" = list("gustspeed", "Meter per Second")),
#'  # sampledmedium = "Air")
#'
db_insert_results_ts <- function(db,
                                 datavalues,
                                 method,
                                 site_code,
                                 variables,
                                 sampledmedium,
                                 processinglevel = "Raw data",
                                 actionby = NULL,
                                 equipment_name = NULL,
                                 aggregationstatisticcv = NULL,
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

    # make sure site is in sampling features table
    if(!site_code %in% rodm2::db_get_sites(db)){
      rodm2::db_describe_site(db, site_code)
  }

    # make sure processing level is in processinglevel table
    sql <- "INSERT or IGNORE into processinglevels (processinglevelcode) VALUES (:processinglevel)"
    sql <- RSQLite::dbSendQuery(db, sql)
    RSQLite::dbBind(sql, params = list(processinglevel = processinglevel))
    RSQLite::dbClearResult(res = sql)

    # check that all variables are in variables table
    # vars_to_add <- setdiff(names(variables), rodm2::db_get_variables(db)$VariableNameCV)
    # for(newvar in vars_to_add){
    #   rodm2::db_describe_variable(db, "Unknown", newvar, newvar)
    # }
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
    }

    sql1 <- 'INSERT into actions
(actiontypecv, methodid, begindatetime, begindatetimeutcoffset, enddatetime)
    VALUES
    ("Instrument deployment",
    (SELECT methodid from methods WHERE methodcode = :method),
    DATETIME(:begindatetime), :begindatetimeutcoffset, DATETIME(:enddatetime))'

    sql1 <- RSQLite::dbSendQuery(db, sql1)
    RSQLite::dbBind(sql1, params = list(method = method,
                                        begindatetime = format(as.POSIXct(datavalues[["Timestamp"]][1]),
                                                               "%Y-%m-%d %H:%M:%S"),
                                        begindatetimeutcoffset = as.integer(substr(format(as.POSIXct(
                                          datavalues[["Timestamp"]][1]),
                                                                                   "%z"),1,3)),
                                        enddatetime = format(as.POSIXct(
                                          datavalues[["Timestamp"]][nrow(datavalues)]),
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
      RSQLite::dbClearResult(res = sql2)
    }

    if(!is.null(equipment_name)){
      # db describe equipment function
      if(!(equipment_name %in% rodm2::db_get_equipment(db))){
        rodm2::db_describe_equipment(db, equip_name = equipment_name, ...)
      }
      # add to equipment used
      sql2b <- RSQLite::dbSendStatement(db, 'INSERT into equipmentused (actionid, equipmentid)
                                       VALUES
                                       (:newactionid,
                                       (SELECT equipmentid FROM equipment WHERE equipmentcode = :equipmentcode))')
      RSQLite::dbBind(sql2b, params = list(newactionid = newactionid,
                                           equipmentcode = equipment_name))
      RSQLite::dbClearResult(res = sql2b)
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
    newfaid <- as.integer(DBI::dbGetQuery(db, "SELECT LAST_INSERT_ROWID()"))

    # add result! new result for each
    # for variables list, if no 'column', make col = name
    for(i in names(variables)){
      if(!"column" %in% names(variables[[i]])){
        variables[[i]][["column"]] <- i
      }
    }

    newresultids <- c()
    # newresultids <- vector(mode = "integer", length = length(variables))
    for(i in names(variables)){
      sql4 <- RSQLite::dbSendStatement(db, 'INSERT into results
                                       (resultuuid, featureactionid, resulttypecv,
                                       variableid, unitsid, processinglevelid,
                                       sampledmediumcv, valuecount)
                                       VALUES
                                       (:uuid, :newfaid, :resulttypecv,
                                       (SELECT variableid FROM variables WHERE variablenamecv = :variablenamecv),
                                       (SELECT unitsid FROM units WHERE unitsname = :units),
                                       (SELECT processinglevelid FROM processinglevels WHERE processinglevelcode = :processinglevel),
                                       :sampledmedium, :valuecount)')
      RSQLite::dbBind(sql4, params = list(uuid = uuid::UUIDgenerate(),
                                          newfaid = newfaid,
                                          resulttypecv = 'Time series coverage',
                                          variablenamecv = i,
                                          units = variables[[i]][["units"]],
                                          processinglevel = processinglevel,
                                          sampledmedium = sampledmedium,
                                          valuecount = nrow(datavalues)
      ))
      RSQLite::dbClearResult(res = sql4)
      newresultids <- append(newresultids, as.integer(DBI::dbGetQuery(db, "SELECT LAST_INSERT_ROWID()")))
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
      var_colname <- variables[[i]][["column"]]
      qualitycodecv <- dplyr::select(datavalues, variables[[i]][["qualitycodecol"]])

      if(is.null(variables[[i]][["qualitycodecol"]])){
        qualitycodecv <- "Unknown"
      }
      censorcodecv <- dplyr::select(datavalues, variables[[i]][["censorcodecol"]])

      if(is.null(variables[[i]][["censorcodecol"]])){
        censorcodecv <- "Unknown"
      }

      timeagg_seconds <-purrr::map_dbl(lubridate::int_diff(datavalues$Timestamp),
                                       .f = lubridate::as.duration)
      timeagg_mins <- c(timeagg_seconds[1], timeagg_seconds)/60
      timeaggunitsid <- RSQLite::dbGetQuery(db,
                                            "select unitsid from units where unitsname = 'Minute'")
      # make data frame to append
      datavalues_var <- datavalues %>%
        dplyr::select(Timestamp, var_colname) %>%
        dplyr::rename(valuedatetime = Timestamp,
                      datavalue = var_colname) %>%
        dplyr::mutate(valuedatetimeutcoffset = as.integer(substr(format(as.POSIXct(valuedatetime), "%z"),1,3)),
                      valuedatetime = format(as.POSIXct(valuedatetime), "%Y-%m-%d %H:%M:%S"),
                      resultid = as.integer(newresultids[[i]]),
                      censorcodecv = censorcodecv[[1]],
                      qualitycodecv = qualitycodecv[[1]],
                      timeaggregationinterval = timeagg_mins,
                      timeaggregationintervalunitsid = as.integer(timeaggunitsid))
      # append
      RSQLite::dbAppendTable(db, "timeseriesresultvalues", datavalues_var)

    }

  }

  if (class(db) == "PostgreSQLConnection"){
    # make sure site is in sampling features table
    if(!site_code %in% db_get_sites(db)){
      rodm2::db_describe_site(db, site_code)
  }

    if(!processinglevel %in%
       DBI::dbGetQuery(db, "SELECT definition from odm2.processinglevels")$definition){
      sql <- "INSERT into odm2.processinglevels (processinglevelcode, defintion)
      VALUES (?processinglevel, ?processinglevel)"
      sql <- DBI::sqlInterpolate(db, sql, processinglevel = processinglevel)
      RPostgreSQL::dbGetQuery(db, sql)
    }

    sql <- DBI::sqlInterpolate(db,
                               'WITH newact AS (
                               INSERT into odm2.actions
                               (actiontypecv, methodid, begindatetime,
                               begindatetimeutcoffset, enddatetime)
                               VALUES
                               (?actiontype,
                               (SELECT methodid FROM odm2.methods WHERE methodcode = ?method),
                               ?begindatetime,
                               ?begindatetimeutcoffset,
                               ?enddatetime)
                               RETURNING actionid)

                               INSERT into odm2.featureactions
                               (actionid, samplingfeatureid)
                               VALUES
                               ((SELECT newact.actionid FROM newact),
                               (SELECT samplingfeatureid FROM odm2.samplingfeatures
                               WHERE samplingfeaturecode = ?site_code))
                               RETURNING actionid, featureactionid',

                               actiontype = "Instrument deployment",
                               method = method,
                               begindatetime = format(as.POSIXct(
                                 datavalues[["Timestamp"]][1]), "%Y-%m-%d %H:%M:%S"),
                               begindatetimeutcoffset = as.integer(substr(format(as.POSIXct(
                                 datavalues[["Timestamp"]][1]), "%z"),1,3)),
                               enddatetime = format(as.POSIXct(
                                 datavalues[["Timestamp"]][nrow(datavalues)]),
                                 "%Y-%m-%d %H:%M:%S"),
                               site_code = site_code
       )

    new_fa <- RPostgreSQL::dbGetQuery(db, sql)
    newactionid <- new_fa$actionid
    newfaid <- new_fa$featureactionid

    if(!is.null(actionby)){
      if(!(actionby %in% rodm2::db_get_people(db))){
        rodm2::db_describe_person(db, PersonFirstName = actionby, ...)
      }
      sql2 <- DBI::sqlInterpolate(db,
                                  'INSERT into odm2.actionby
                                  (actionid, affiliationid, isactionlead)
                                  VALUES
                                  (?newactionid,
                                  (SELECT affiliationid FROM odm2.affiliations
                                  WHERE personid =
                                  (SELECT personid FROM odm2.people WHERE personfirstname = ?actionby)),
                                  ?isactionlead)',
                                  newactionid = newactionid,
                                  actionby = actionby,
                                  isactionlead = "TRUE")
      RPostgreSQL::dbGetQuery(db, sql2)
    }

    if(!is.null(equipment_name)){
      # db describe equipment function
      if(!(equipment_name %in% rodm2::db_get_equipment(db))){
        rodm2::db_describe_equipment(db, equip_name = equipment_name, ...)
      }
      # add to equipment used
      sql2b <- DBI::sqlInterpolate(db, 'INSERT into odm2.equipmentused (actionid, equipmentid)
                                   VALUES
                                   (?newactionid,
                                   (SELECT equipmentid
                                   FROM odm2.equipment WHERE equipmentcode = ?equipmentcode))',
                                   newactionid = newactionid,
                                   equipmentcode = equipment_name)
      RPostgreSQL::dbGetQuery(db, sql2b)
    }

    # add result! new result for each
    newresultids <- c()
    for(i in names(variables)){
      if(!"column" %in% names(variables[[i]])){
        variables[[i]][["column"]] <- i
      }
    }

    for(i in names(variables)){
      sql <- DBI::sqlInterpolate(db,
                                 'INSERT into odm2.results
                                 (resultuuid, featureactionid, resulttypecv,
                                 variableid, unitsid, processinglevelid,
                                 sampledmediumcv, valuecount)
                                 VALUES
                                 (?uuid, ?newfaid, ?resulttypecv,
                                 (SELECT variableid
                                 FROM odm2.variables
                                 WHERE variablenamecv = ?variablenamecv),
                                 (SELECT unitsid FROM odm2.units WHERE unitsname = ?units),
                                 (SELECT processinglevelid
                                 FROM odm2.processinglevels
                                 WHERE definition = ?processinglevel),
                                 ?sampledmedium,
                                 ?valuecount
                                 )
                                 RETURNING resultid',
                                 uuid = uuid::UUIDgenerate(),
                                 newfaid = newfaid,
                                 resulttypecv = 'Time series coverage',
                                 variablenamecv = i,
                                 units = variables[[i]][["units"]],
                                 processinglevel = processinglevel,
                                 sampledmedium = sampledmedium,
                                 valuecount = nrow(datavalues))

      newresult <- RPostgreSQL::dbGetQuery(db, sql)
      newresultids <- append(newresultids, as.integer(newresult))
    }

    # for each of the new results, insert into time series results
    for(i in unname(newresultids)){
      sql5 <- 'INSERT into odm2.timeseriesresults
      (resultid, aggregationstatisticcv)
      VALUES (?resultid, ?aggstatcv)'
      sql5 <- DBI::sqlInterpolate(db, sql5,
                                  resultid = i,
                                  aggstatcv = ifelse(is.null(aggregationstatisticcv),
                                                     "Unknown", aggregationstatisticcv))
      RPostgreSQL::dbGetQuery(db, sql5)

      if(!is.null(zlocation)){
        sql <- DBI::sqlInterpolate(db,
                                   'UPDATE odm2.timeseriesresults
                                   SET zlocation = ?zlocation,
                                   zlocationunitsid = (SELECT unitsid FROM odm2.units WHERE unitsname = ?zlocationunits)
                                   WHERE resultid = ?resultid)',
                                   zlocation = zlocation, zlocationunits = zlocationunits, resultid = i)
        RPostgreSQL::dbGetQuery(db, sql)
      }
    }

    # then insert into timeseriesresultvalues
    names(newresultids) <- names(variables)

    for(i in names(newresultids)){
      # subset data values
      var_colname <- variables[[i]][["column"]]
      qualitycodecv <- dplyr::select(datavalues, variables[[i]][["qualitycodecol"]])

      if(is.null(variables[[i]][["qualitycodecol"]])){
        qualitycodecv <- "Unknown"
      }

      censorcodecv <- dplyr::select(datavalues, variables[[i]][["censorcodecol"]])

      if(is.null(variables[[i]][["censorcodecol"]])){
        censorcodecv <- "Unknown"
      }

      timeagg_seconds <-purrr::map_dbl(lubridate::int_diff(datavalues$Timestamp),
                                       .f = lubridate::as.duration)
      timeagg_mins <- c(timeagg_seconds[1], timeagg_seconds)/60
      timeaggunitsid <- RPostgreSQL::dbGetQuery(db,
                                            "select unitsid from odm2.units where unitsname = 'Minute'")
      # make data frame to append
      datavalues_var <- datavalues %>%
        dplyr::select(Timestamp, var_colname) %>%
        dplyr::rename(valuedatetime = Timestamp,
                      datavalue = var_colname) %>%
        dplyr::mutate(valuedatetimeutcoffset = as.integer(substr(format(as.POSIXct(valuedatetime), "%z"),1,3)),
                      valuedatetime = format(as.POSIXct(valuedatetime), "%Y-%m-%d %H:%M:%S"),
                      resultid = as.integer(newresultids[[i]]),
                      censorcodecv = censorcodecv[[1]],
                      qualitycodecv = qualitycodecv[[1]],
                      timeaggregationinterval = timeagg_mins,
                      timeaggregationintervalunitsid = as.integer(timeaggunitsid))
      # append
      DBI::dbWriteTable(db, c("odm2", "timeseriesresultvalues"),
                        datavalues_var, row.names = FALSE,
                        overwrite = FALSE,
                        append = TRUE)
    }
  }


}


