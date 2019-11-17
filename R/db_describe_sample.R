

#' Add details about a new sample
#'
#' @param db database connection
#' @param sample_code sampling feature code for sample
#' @param site_code sampling feature code for where sample was collected from
#'
#' @return message that sample was added to sampling feature table
#' @export
#'
#' @examples
#'
#' db <- create_sqlite(connect = TRUE)
#' db_describe_site(db, site_code = "site1")
#'
#' db_describe_sample(db,
#' sample_code = "sample001",
#' site_code = "site1")
#'
db_describe_sample <- function(db,
                               sample_code,
                               site_code){

  if (!class(db) %in% c("SQLiteConnection")) {
    stop("sorry, only sqlite database connections are supported so far")}

  site_code <- handle_new_site(db, site_code = site_code)
  # add sample
  sql <- 'INSERT or IGNORE into samplingfeatures
  (samplingfeatureuuid, samplingfeaturetypecv, samplingfeaturecode)
  VALUES
  (:samplingfeatureuuid, :samplingfeaturetypecv, :samplingfeaturecode)'

  sql <- RSQLite::dbSendQuery(db, sql)
  RSQLite::dbBind(sql, params = list(
    samplingfeatureuuid = uuid::UUIDgenerate(),
    samplingfeaturetypecv = "Specimen",
    samplingfeaturecode = sample_code
  ))

  RSQLite::dbClearResult(res = sql)

  sample_sf_id <- as.integer(RSQLite::dbGetQuery(db, "SELECT LAST_INSERT_ROWID()"))

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
    site_code = site_code
  ))

  RSQLite::dbClearResult(res = sql)

  message(glue::glue("Sample {sample_code} has been entered into the samplingfeatures table."))

}

#' Get list of sample codes currently in database
#'
#' @param db database connection object
#'
#' @return the current values in the samplingfeaturecode column
#' @export
#'
#' @examples
#' db <- create_sqlite(connect = TRUE)
#' db_describe_site(db, site_code = "site1")
#' db_describe_sample(db, sample_code = "samp01", site_code = "site1")
#' db_get_samples(db)
#'
db_get_samples <- function(db){
  if (!class(db) %in% c("SQLiteConnection", "PostgreSQLConnection")) {
    stop("sorry, only sqlite and postgresql database connections are supported so far")}
  current_samples <- c()
  if (class(db) == "SQLiteConnection"){
    current_samples <- DBI::dbGetQuery(db, "SELECT samplingfeaturecode FROM samplingfeatures
                                       WHERE samplingfeaturetypecv = 'Specimen'")[[1]]
    # RSQLite::dbClearResult()
  }
  if (class(db) == "PostgreSQLConnection"){
    current_samples <- DBI::dbGetQuery(db, "SELECT samplingfeaturecode FROM odm2.samplingfeatures
                                       WHERE samplingfeaturetypecv = 'Specimen'")[[1]]
  }
  return(current_samples)
  }


