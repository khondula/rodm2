# input data frame: site, datetime, variable

#' Insert sample result data values to ODM2 database
#'
#' @param db database connection
#' @param datavalues data frame with data to upload columns "Timestamp" with
#'    YYYY-MM-DD H:M:S format, and column names corresponding to variable names
#' @param variables
#' a named list of lists defining variable names, units, and columns in
#' datavalues data frame. Create with \code{\link{make_vars_list}} or see Details.
#' @param sampledmedium term from \href{http://vocabulary.odm2.org/medium}{
#' controlled vocabulary} for medium sampled eg. air or soil
#' @param processinglevel code for processing level. will be added to processinglevels table if new.
#'   defaults to "Raw data'.
#' @param zlocationunits name of units of z location offset FROM CONTROLLED VOCAB
#' @param ... parameters to pass to various db_describe_ functions
#' @param site_code_col name of column with site codes. defaults to "Site"
#' @param time_aggregation_interval defaults to unknown
#' @param field_method
#' short description of field method to collect new samples.
#' Dont include for new data about existing samples.
#' @param lab_method short description of lab method. only use if lab_method_col is NULL
#' @param sample_code_col name of column in input data frame with sample ID. defeaults to "Sample"
#' @param aggregationstatistic \href{http://vocabulary.odm2.org/aggregationstatistic}{
#' controlled vocabulary}, defaults to unknown
#' @param field_actionby optional first name of lead for field action, must already be in people table
#' @param field_equipment_name optional code name of equipment used for field action, must already be in equipment table
#' @param lab_actionby optional first name of lead for lab action, must already be in people table
#' @param lab_equipment_name optional code name of equipment used for lab action, must already be in equipment table
#' @param lab_method_col specify the column name of the lab method. only use if lab method is NULL
#' @param zlocation_col specify column name of the zlocation
#' @param zinterval_col specify column name of the z aggregation interval
#'
#' @return true if successful
#' @export
#' @family insert data functions

#' @examples
#' \dontrun{
#' # make database and data frame
#' db <- rodm2::create_sqlite(connect = TRUE)
#' mrv <- data.frame(Timestamp = "2018-06-27 13:55:00",
#' "pH" = 4.5, Site = "BB2", Sample = "CEM 1", stringsAsFactors = FALSE)
#'
#' db_insert_results_sample(db = db, datavalues = soil_ph,
#'     field_method = "1 inch 30 cm soil core",
#'     lab_method = "Soil pH CEM",
#'     variables = list("pH" = list(column = "pH", units = "pH")),
#'     sampledmedium = "Soil")
#' }
#'
db_insert_results_samples_profile <- function(db,
                                datavalues,
                                field_method = NULL,
                                lab_method = NULL,
                                lab_method_col = NULL,
                                variables,
                                sampledmedium,
                                site_code_col = "Site",
                                sample_code_col = "Sample",
                                processinglevel = "Raw data",
                                aggregationstatistic = "Unknown",
                                time_aggregation_interval = list(1, "Minute"),
                                field_actionby = NULL,
                                field_equipment_name = NULL,
                                lab_actionby = NULL,
                                lab_equipment_name = NULL,
                                zlocation_col = NULL,
                                zinterval_col = NULL,
                                zlocationunits, ...){
  # if (!class(db) %in% c("SQLiteConnection", "PostgreSQLConnection")) {
  #   stop("sorry, only sqlite and postgresql database connections are supported so far")}

  if (!class(db) %in% c("SQLiteConnection")) {
    stop("sorry, only sqlite database connections are supported so far")}
  # if (!class(db) %in% c("SQLiteConnection", "PostgreSQLConnection")) {
  #   stop("sorry, only sqlite and postgres database connections are supported so far")}

  # check for methods and add if not in there
  # add if not null field method
  if(!is.null(field_method)){
    if(!(field_method %in% rodm2::db_get_methods(db))){
      rodm2::db_describe_method(db, methodname = field_method,
                                methodcode = field_method,
                                methodtypecv = 'Specimen collection')}
  }

  if(!is.null(lab_method)){
    if(!(lab_method %in% rodm2::db_get_methods(db))){
      rodm2::db_describe_method(db, methodname = lab_method,
                                methodcode = lab_method,
                                methodtypecv = 'Specimen analysis')
    }

  }


  # if lab methods column is null, add methodcode column to datavalues
  if(is.null(lab_method_col)){
    datavalues$lab_method_code <- lab_method
    lab_method_col <- "lab_method_code"
  }

  # make sure all sites in sampling features table
  for(site_code in datavalues[[site_code_col]]){
    if(!site_code %in% rodm2::db_get_sites(db)){
      rodm2::db_describe_site(db, site_code)
    }
  }
  # make sure processinglevels in database
  rodm2::insert_processinglevel(db, processinglevel)
  # check that all variables are in variables table
  rodm2::check_variables_list(db = db, variables = variables)

  # check type of database object
  if (class(db) == "SQLiteConnection"){

    # add column with zlocation units id
    unitsid <- RSQLite::dbGetQuery(db, sprintf("SELECT unitsid FROM units WHERE unitsname = \'%s\'", zlocationunits))
    datavalues$zlocationunitsid <- as.integer(unitsid)


    #######################################
    # for each ROW of samples data frame
    db_insert_one_sample <- function(sample_id){
      sample_sf_id <- c()
      field_actionid <- c()
      ### for new samples only, based on argument supplied for field_method
      if(!is.null(field_method)){
        # add sample collection
        sql <- 'INSERT into actions
        (actiontypecv, methodid, begindatetime, begindatetimeutcoffset)
        VALUES
        (:actiontypecv,
        (SELECT methodid FROM methods WHERE methodcode = :field_method),
        :begindatetime,
        :utcoffset)'
        sql <- RSQLite::dbSendQuery(db, sql)
        RSQLite::dbBind(sql, params = list(
          actiontypecv = 'Specimen collection',
          field_method = field_method,
          begindatetime = datavalues[["Timestamp"]][sample_id],
          utcoffset = as.integer(substr(
            format(as.POSIXct(datavalues[["Timestamp"]][sample_id]), "%z"), 1, 3))
        ))
        RSQLite::dbClearResult(res = sql)
        field_actionid <- as.integer(RSQLite::dbGetQuery(db, "SELECT LAST_INSERT_ROWID()"))

        # field_actionby
        if(!is.null(field_actionby)){

          sql <- RSQLite::dbSendStatement(db, 'INSERT into actionby (actionid, affiliationid, isactionlead)
                                          VALUES
                                          (:newactionid,
                                          (SELECT affiliationid FROM affiliations
                                          WHERE personid = (SELECT personid FROM people WHERE personfirstname = :actionby)),
                                          "TRUE")')
          RSQLite::dbBind(sql, params = list(newactionid = field_actionid,
                                             actionby = field_actionby))
          RSQLite::dbClearResult(res = sql)
        }
        # field_equipmentused
        if(!is.null(field_equipment_name)){
          sql <- RSQLite::dbSendStatement(db, 'INSERT into equipmentused (actionid, equipmentid)
                                          VALUES
                                          (:newactionid,
                                          (SELECT equipmentid FROM equipment WHERE equipmentcode = :equipmentcode))')
          RSQLite::dbBind(sql, params = list(newactionid = field_actionid,
                                             equipmentcode = field_equipment_name))
          RSQLite::dbClearResult(res = sql)
        }

        # add sample
        sql <- 'INSERT or IGNORE into samplingfeatures
        (samplingfeatureuuid, samplingfeaturetypecv, samplingfeaturecode)
        VALUES
        (:samplingfeatureuuid, :samplingfeaturetypecv, :samplingfeaturecode)'
        sql <- RSQLite::dbSendQuery(db, sql)
        RSQLite::dbBind(sql, params = list(
          samplingfeatureuuid = uuid::UUIDgenerate(),
          samplingfeaturetypecv = "Specimen",
          samplingfeaturecode = datavalues[[sample_code_col]][sample_id]
        ))
        RSQLite::dbClearResult(res = sql)
        sample_sf_id <- as.integer(RSQLite::dbGetQuery(db, "SELECT LAST_INSERT_ROWID()"))

        # add featureaction for sample collection
        sql <- 'INSERT into featureactions
        (samplingfeatureid, actionid)
        VALUES
        (:samplingfeatureid, :actionid)'
        sql <- RSQLite::dbSendQuery(db, sql)
        RSQLite::dbBind(sql, params = list(
          samplingfeatureid = sample_sf_id,
          actionid = field_actionid
        ))
        RSQLite::dbClearResult(res = sql)
        field_fa_id <- as.integer(DBI::dbGetQuery(db, "SELECT LAST_INSERT_ROWID()"))

        # add was collected at relationship
        sql <- 'INSERT into relatedfeatures
        (samplingfeatureid, relationshiptypecv, relatedfeatureid)
        VALUES
        (:samplingfeatureid, :relationshiptypecv,
        (SELECT samplingfeatureid FROM samplingfeatures WHERE
        samplingfeaturecode = :site_code))'
        sql <- RSQLite::dbSendQuery(db, sql)
        RSQLite::dbBind(sql, params = list(
          samplingfeatureid = sample_sf_id,
          relationshiptypecv = "Was collected at",
          site_code = datavalues[[site_code_col]][sample_id]
        ))
        RSQLite::dbClearResult(res = sql)
      }
      ### for existing samples, get sample ID
      if(is.null(sample_sf_id)){
        sql <- paste0("SELECT samplingfeatureid FROM samplingfeatures WHERE samplingfeaturecode = '",
                      datavalues[[sample_code_col]][sample_id], "'")
        sample_sf_id <- as.integer(RSQLite::dbGetQuery(db, sql))
      }

      # add lab action
      sql <- 'INSERT into actions
      (actiontypecv, methodid, begindatetime, begindatetimeutcoffset)
      VALUES
      (:actiontypecv,
      (SELECT methodid FROM methods WHERE methodcode = :lab_method),
      :begindatetime,
      :utcoffset)'
      sql <- RSQLite::dbSendQuery(db, sql)
      RSQLite::dbBind(sql, params = list(
        actiontypecv = 'Specimen analysis',
        lab_method = datavalues[[lab_method_col]],
        begindatetime = ifelse(
          is.null(datavalues[["Timestamp_analysis"]][sample_id]),
          as.character(Sys.time()),
          datavalues[["Timestamp_analysis"]][sample_id]),
        utcoffset = as.integer(substr(
          format(as.POSIXct(datavalues[["Timestamp"]][sample_id]), "%z"), 1, 3))
      ))
      RSQLite::dbClearResult(res = sql)
      lab_actionid <- as.integer(RSQLite::dbGetQuery(db, "SELECT LAST_INSERT_ROWID()"))

      # lab_actionby
      if(!is.null(lab_actionby)){
        sql <- RSQLite::dbSendStatement(db, 'INSERT into actionby (actionid, affiliationid, isactionlead)
                                        VALUES
                                        (:newactionid,
                                        (SELECT affiliationid FROM affiliations
                                        WHERE personid = (SELECT personid FROM people WHERE personfirstname = :actionby)),
                                        "TRUE")')
        RSQLite::dbBind(sql, params = list(newactionid = lab_actionid,
                                           actionby = lab_actionby))
        RSQLite::dbClearResult(res = sql)
      }
      # lab_equipmentused
      if(!is.null(lab_equipment_name)){
        sql <- RSQLite::dbSendStatement(db, 'INSERT into equipmentused (actionid, equipmentid)
                                        VALUES
                                        (:newactionid,
                                        (SELECT equipmentid FROM equipment WHERE equipmentcode = :equipmentcode))')
        RSQLite::dbBind(sql, params = list(newactionid = lab_actionid,
                                           equipmentcode = lab_equipment_name))
        RSQLite::dbClearResult(res = sql)
      }

      # relate field and lab action
      # if field method wasn't supplied, need to get field action id
      # of sample collection action
      if(is.null(field_actionid)){
        sql <- paste0('SELECT actionid FROM featureactions WHERE samplingfeatureid = (SELECT samplingfeatureid FROM samplingfeatures WHERE samplingfeaturecode =',
                      '"',datavalues[[sample_code_col]][sample_id],
                      '") AND actionid IN (SELECT actionid FROM actions WHERE actiontypecv = "Specimen collection")')
        field_actionid <- as.integer(RSQLite::dbGetQuery(db, sql))
      }

      sql <- 'INSERT into relatedactions
      (actionid, relationshiptypecv, relatedactionid)
      VALUES
      (:lab_actionid, :relationshiptypecv,
      :field_actionid)'
      sql <- RSQLite::dbSendStatement(db, sql)
      RSQLite::dbBind(sql, params = list(
        lab_actionid = lab_actionid,
        relationshiptypecv = "Is related to",
        field_actionid = field_actionid
      ))
      RSQLite::dbClearResult(res = sql)

      # lab feature action
      sql <- 'INSERT into featureactions
      (samplingfeatureid, actionid)
      VALUES
      (:samplingfeatureid, :actionid)'
      sql <- RSQLite::dbSendQuery(db, sql)
      RSQLite::dbBind(sql, params = list(
        samplingfeatureid = sample_sf_id,
        actionid = lab_actionid
      ))
      RSQLite::dbClearResult(res = sql)
      newfaid <- as.integer(DBI::dbGetQuery(db, "SELECT LAST_INSERT_ROWID()"))

      # insert profile result related to lab feature action
      # add result! new result for each variable
      # for variables list, if no 'column', make col = name
      for(j in names(variables)){
        if(!"column" %in% names(variables[[j]])){
          variables[[j]][["column"]] <- j }
      }
      newresultids <- c()
      for(j in names(variables)){
        sql <- RSQLite::dbSendStatement(db, 'INSERT into results
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
        RSQLite::dbBind(sql, params = list(uuid = uuid::UUIDgenerate(),
                                           newfaid = newfaid,
                                           resulttypecv = 'Profile coverage',
                                           variablenamecv = j,
                                           units = variables[[j]][["units"]],
                                           processinglevel = processinglevel,
                                           sampledmedium = sampledmedium,
                                           valuecount = 1
        ))
        RSQLite::dbClearResult(res = sql)
        newresultids <- append(newresultids, as.integer(DBI::dbGetQuery(db, "SELECT LAST_INSERT_ROWID()")))
      }
      names(newresultids) <- names(variables)
      #######################################
      # for each of the new results, insert into PROFILE result results
      for(j in names(newresultids)){
        qualitycodecv <- dplyr::select(datavalues, variables[[j]][["qualitycodecol"]])
        if(is.null(variables[[j]][["qualitycodecol"]])){
          qualitycodecv <- "Unknown"
        }
        censorcodecv <- dplyr::select(datavalues, variables[[j]][["censorcodecol"]])
        if(is.null(variables[[j]][["censorcodecol"]])){
          censorcodecv <- "Unknown"
        }
        sql5 <- 'INSERT into profileresults
        (resultid, aggregationstatisticcv)
        VALUES (:resultid, :aggstatcv)'
        sql5 <- RSQLite::dbSendStatement(db, sql5)
        RSQLite::dbBind(sql5, params = list(resultid = newresultids[[j]],
                                            aggstatcv = ifelse(is.null(aggregationstatistic),
                                                               "Unknown", aggregationstatistic))
        )
        RSQLite::dbClearResult(res = sql5)
      }
      ################################
      # then insert into profileresultvalues
      for(j in names(newresultids)){
        # subset data values
        var_colname <- variables[[j]]$column
        # make data frame to append
        datavalues_var <- datavalues %>%
          dplyr::slice(sample_id) %>%
          dplyr::select(Timestamp, var_colname, zlocation_col, zinterval_col,
                        zlocationunitsid) %>%
          dplyr::rename(valuedatetime = Timestamp,
                        datavalue = var_colname,
                        zlocation = zlocation_col,
                        zaggregationinterval = zinterval_col) %>%
          dplyr::mutate(valuedatetimeutcoffset = as.integer(substr(
            format(as.POSIXct(valuedatetime), "%z"),1,3)),
            valuedatetime = format(as.POSIXct(valuedatetime), "%Y-%m-%d %H:%M:%S"),
            resultid = as.integer(newresultids[[j]])) %>%
          dplyr::mutate(censorcodecv = censorcodecv,
                        qualitycodecv = qualitycodecv,
                        timeaggregationinterval = 1,
                        timeaggregationintervalunitsid = 1060)
        # append
        RSQLite::dbAppendTable(db, "profileresultvalues", datavalues_var)
      }
      message(paste("Sample", datavalues[[sample_code_col]][sample_id], "and associated data have been entered."))

    }

    purrr::map(1:nrow(datavalues), db_insert_one_sample)
  }

  if (class(db) == "PostgreSQLConnection"){

    message("Coming soon!")

    #######################################
    # for each ROW of sample data data frame


  }

  }


