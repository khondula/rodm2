# input data frame: site, datetime, variable

#' Insert sample result data values to ODM2 database
#'
#' @param db database connection
#' @param datavalues data frame with columns "Timestamp" with
#'    YYYY-MM-DD H:M:S format, and column names corresponding to variable names
#' @param variables a named list of lists defining variable names, units, and columns in datavalues data frame
#'    with format list("variablen1ame" = list(units = 'unitsname', column = 'colname', dataqualitycol = 'qualcode'))
#' @param sampledmedium term from controlled vocabulary for medium sampled eg. Air, Water, Soil
#' @param processinglevel code for processing level. will be added to processinglevels table if new.
#'   defaults to "Raw data'.
#' @param zlocation (optional) z location offset
#' @param zlocationunits (optional but required if zlocation is set) name of units of z location offset
#' @param ... parameters to pass to various db_describe_ functions
#' @param site_code_col name of column with site codes. defaults to "Site"
#' @param time_aggregation_interval defaults to unknown
#' @param field_method short description of field method
#' @param lab_method short description of lab method
#' @param sample_code_col name of column in input data frame with sample ID. defeaults to "Sample"
#' @param aggregationstatistic defaults to unknown
#' @param field_actionby optional first name of lead for field action, must already be in people table
#' @param field_equipment_name optional code name of equipment used for field action, must already be in equipment table
#' @param lab_actionby optional first name of lead for lab action, must already be in people table
#' @param lab_equipment_name optional code name of equipment used for lab action, must already be in equipment table
#'
#' @return true if successful
#' @export
#'
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
db_insert_results_samples <- function(db,
                                datavalues,
                                field_method,
                                lab_method,
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
                                zlocation = NULL,
                                zlocationunits = NULL, ...){
  # if (!class(db) %in% c("SQLiteConnection", "PostgreSQLConnection")) {
  #   stop("sorry, only sqlite and postgresql database connections are supported so far")}

  if (!class(db) %in% c("SQLiteConnection", "PostgreSQLConnection")) {
    stop("sorry, only sqlite and postgres database connections are supported so far")}

  # check for method and add if not in there
  if(!(field_method %in% rodm2::db_get_methods(db))){
    rodm2::db_describe_method(db, methodname = field_method,
                              methodcode = field_method,
                              methodtypecv = 'Specimen collection')
  }
  if(!(lab_method %in% rodm2::db_get_methods(db))){
    rodm2::db_describe_method(db, methodname = lab_method,
                              methodcode = lab_method,
                              methodtypecv = 'Specimen analysis')
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


    #######################################
    # for each ROW of samples data frame

    db_insert_one_sample <- function(sample_id){

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
        lab_method = lab_method,
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
      sql <- 'INSERT into relatedactions
(actionid, relationshiptypecv, relatedactionid)
      VALUES
      (:lab_actionid, :relationshiptypecv, :field_actionid)'
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

      # insert measurement result related to lab feature action
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
                                           resulttypecv = 'Measurement',
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
          dplyr::slice(sample_id) %>%
          dplyr::select(Timestamp, var_colname) %>%
          dplyr::rename(valuedatetime = Timestamp,
                        datavalue = var_colname) %>%
          dplyr::mutate(valuedatetimeutcoffset = as.integer(substr(
            format(as.POSIXct(valuedatetime), "%z"),1,3)),
            valuedatetime = format(as.POSIXct(valuedatetime), "%Y-%m-%d %H:%M:%S"),
            resultid = as.integer(newresultids[[j]]))
        # append
        RSQLite::dbAppendTable(db, "measurementresultvalues", datavalues_var)
      }
    }
    purrr::map(1:nrow(datavalues), db_insert_one_sample)
  }

  if (class(db) == "PostgreSQLConnection"){


    #######################################
    # for each ROW of sample data data frame

    db_insert_one_sample_postgres <- function(sample_id){

      # insert field action and sample and field feature action
      sql <- DBI::sqlInterpolate(db,
                                 'WITH fieldact AS (INSERT into odm2.actions
                                 (actiontypecv, methodid, begindatetime, begindatetimeutcoffset)
                                 VALUES
                                 (?actiontypecv,
                                 (SELECT methodid FROM odm2.methods WHERE methodcode = ?field_method),
                                 ?begindatetime,
                                 ?utcoffset)
                                 RETURNING actionid),

                                 newsample AS (
                                 INSERT into odm2.samplingfeatures
(samplingfeatureuuid, samplingfeaturetypecv, samplingfeaturecode)
VALUES
(?samplingfeatureuuid, ?samplingfeaturetypecv, ?samplecode)
RETURNING samplingfeatureid
                                 ),
newrel AS (
INSERT into odm2.relatedfeatures
(samplingfeatureid, relationshiptypecv, relatedfeatureid)
VALUES
(?samplecode, ?relationshiptypecv, ?site_code)
)

INSERT into odm2.featureactions (samplingfeatureid, actionid)
VALUES (newsample.samplingfeatureid, fieldact.actionid)
RETURNING featureactionid, actionid',
                                 actiontypecv = 'Specimen collection',
                                 field_method = field_method,
                                 begindatetime = datavalues[["Timestamp"]][sample_id],
                                 utcoffset = as.integer(substr(
                                   format(as.POSIXct(datavalues[["Timestamp"]][sample_id]), "%z"), 1, 3)),
                                 samplingfeatureuuid = uuid::UUIDgenerate(),
                                 samplingfeaturetypecv = 'Specimen',
                                 samplecode = datavalues[[sample_code_col]][sample_id],
                                 relationshiptypecv = 'Was collected at',
                                 site_code = datavalues[[site_code_col]][sample_id]
                                 )
      field_fa <- RPostgreSQL::dbGetQuery(db, sql)
      field_fa_id <- field_fa$featureactionid
      field_action_id <- field_fa$actionid

# field actionby
      if(!is.null(field_actionby)){

        sql <- DBI::sqlInterpolate(db,
                                    'INSERT into odm2.actionby
                                    (actionid, affiliationid, isactionlead)
                                    VALUES
                                    (?newactionid,
                                    (SELECT affiliationid FROM odm2.affiliations
                                    WHERE personid =
                                    (SELECT personid FROM odm2.people WHERE personfirstname = ?actionby)),
                                    ?isactionlead)',
                                    newactionid = field_action_id,
                                    actionby = field_actionby,
                                    isactionlead = "TRUE")
        RPostgreSQL::dbGetQuery(db, sql)
      }
# field equipmentused
      if(!is.null(field_equipment_name)){
        sql <- DBI::sqlInterpolate(db, 'INSERT into odm2.equipmentused (actionid, equipmentid)
                                     VALUES
                                     (?newactionid,
                                     (SELECT equipmentid
                                     FROM odm2.equipment WHERE equipmentcode = ?equipmentcode))',
                                     newactionid = field_action_id,
                                     equipmentcode = field_equipment_name)
        RPostgreSQL::dbGetQuery(db, sql)
      }

      # insert lab action and action relation and lab feature action
sql <- DBI::sqlInterpolate(db,
                           'WITH labact AS (INSERT into odm2.actions
                                 (actiontypecv, methodid, begindatetime, begindatetimeutcoffset)
                           VALUES
                           (?actiontypecv,
                           (SELECT methodid FROM odm2.methods WHERE methodcode = ?lab_method),
                           ?begindatetime,
                           ?utcoffset)
                           RETURNING actionid),
WITH newrel AS (
INSERT into odm2.relatedactions (actionid, relationshiptypecv, relatedactionid)
VALUES
(labact.actionid, ?relationshiptypecv, ?field_fa_id)
),

INSERT into odm2.featureactions (samplingfeatureid, actionid)
VALUES (
(SELECT samplingfeatureid FROM odm2.samplingfeatures WHERE samplingfeaturecode = ?samplecode), labact.actionid)
RETURNING featureactionid, actionid
',
actiontypecv = 'Specimen analysis',
lab_method = lab_method,
begindatetime = ifelse(),
utcoffset = ifelse(),
relationshiptypecv = 'Is related to',
field_fa_id = field_fa_id,
samplecode = datavalues[[sample_code_col]][sample_id]
)
newfa <- RPostgreSQL::dbGetQuery(db, sql)
newfaid <- newfa$featureactionid
lab_actionid <- newfa$actionid

# lab actionby
if(!is.null(lab_actionby)){

  sql <- DBI::sqlInterpolate(db,
                             'INSERT into odm2.actionby
                             (actionid, affiliationid, isactionlead)
                             VALUES
                             (?newactionid,
                             (SELECT affiliationid FROM odm2.affiliations
                             WHERE personid =
                             (SELECT personid FROM odm2.people WHERE personfirstname = ?actionby)),
                             ?isactionlead)',
                             newactionid = lab_actionid,
                             actionby = lab_actionby,
                             isactionlead = "TRUE")
  RPostgreSQL::dbGetQuery(db, sql)
}
# lab equipmentused
if(!is.null(lab_equipment_name)){
  sql <- DBI::sqlInterpolate(db, 'INSERT into odm2.equipmentused (actionid, equipmentid)
                             VALUES
                             (?newactionid,
                             (SELECT equipmentid
                             FROM odm2.equipment WHERE equipmentcode = ?equipmentcode))',
                             newactionid = lab_actionid,
                             equipmentcode = lab_equipment_name)
  RPostgreSQL::dbGetQuery(db, sql)
}

      # for each variable insert measurement results

# add result! new result for each variable
# for variables list, if no 'column', make col = name
for(j in names(variables)){
  if(!"column" %in% names(variables[[j]])){
    variables[[j]][["column"]] <- j
  }
}

newresultids <- c()

for(j in names(variables)){
  sql4 <- DBI::sqlInterpolate(db, 'INSERT into odm2.results
                              (resultuuid, featureactionid, resulttypecv,
                              variableid, unitsid, processinglevelid,
                              sampledmediumcv, valuecount)
                              VALUES
                              (?uuid, ?newfaid, ?resulttypecv,
                              (SELECT variableid FROM odm2.variables
                              WHERE variablenamecv = ?variablenamecv),
                              (SELECT unitsid FROM odm2.units WHERE unitsname = ?units),
                              (SELECT processinglevelid FROM odm2.processinglevels
                              WHERE definition = ?processinglevel),
                              ?sampledmedium, ?valuecount)
                              RETURNING resultid',
                              uuid = uuid::UUIDgenerate(),
                              newfaid = newfaid,
                              resulttypecv = 'Measurement',
                              variablenamecv = j,
                              units = variables[[j]][["units"]],
                              processinglevel = processinglevel,
                              sampledmedium = sampledmedium,
                              valuecount = 1)

  newresult <- RPostgreSQL::dbGetQuery(db, sql4)
  newresultids <- append(newresultids, as.integer(newresult))
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

  sql5 <- 'INSERT into odm2.measurementresults
  (resultid, aggregationstatisticcv,
  timeaggregationinterval, timeaggregationintervalunitsid, censorcodecv, qualitycodecv)
  VALUES (?resultid, ?aggstatcv,
  ?timeaggregationinterval,
  (SELECT unitsid FROM odm2.units WHERE unitsname = ?timeaggregationintervalunits),
  ?censorcodecv, ?qualitycodecv)'
  sql5 <- DBI::sqlInterpolate(db, sql5,
                              resultid = newresultids[[j]],
                              aggstatcv = ifelse(is.null(aggregationstatistic),
                                                 "Unknown", aggregationstatistic),
                              timeaggregationinterval = time_aggregation_interval[[1]],
                              timeaggregationintervalunits = time_aggregation_interval[[2]],
                              censorcodecv = ifelse(is.null(variables[[j]]$censorcodecol),
                                                    "Unknown", variables[[j]]$censorcodecol),
                              qualitycodecv = ifelse(is.null(variables[[j]]$qualitycodecol),
                                                     "Unknown", variables[[j]]$qualitycodecol)
  )
  RPostgreSQL::dbGetQuery(db, sql5)

  if(!is.null(zlocation)){
    sql <- DBI::sqlInterpolate(db,
                               'UPDATE odm2.measurementresults
                               SET zlocation = ?zlocation,
                               zlocationunitsid = (SELECT unitsid FROM odm2.units WHERE unitsname = ?zlocationunits)
                               WHERE resultid = ?resultid)',
                               zlocation = zlocation,
                               zlocationunits = zlocationunits, resultid = j)
    RPostgreSQL::dbGetQuery(db, sql)}
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
    dplyr::mutate(valuedatetimeutcoffset = as.integer(substr(
      format(as.POSIXct(valuedatetime), "%z"), 1,3)),
      valuedatetime = format(as.POSIXct(valuedatetime), "%Y-%m-%d %H:%M:%S"),
      resultid = as.integer(newresultids[[j]]))
  # append
  DBI::dbWriteTable(db, c("odm2", "measurementresultvalues"),
                    datavalues_var, row.names = FALSE,
                    overwrite = FALSE,
                    append = TRUE)
}
    }


    purrr::map(1:nrow(datavalues), db_insert_one_sample_postgres)

  }

  }

