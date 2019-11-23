#' Describe a new site
#'
#' @param db database connection object
#' @param site_code unique short code name (required)
#' @param site_name optional longer site name
#' @param site_description optional longer site description
#' @param sitetypecv
#' samplingfeature type from
#'  \href{http://vocabulary.odm2.org/samplingfeaturetype/}{controlled vocab},
#'  defaults to "Site"
#'
#' @return message that site was added
#' @export
#' @family describe functions

#' @examples
#' db <- create_sqlite(connect = TRUE)
#' db_describe_site(db, site_code = "site1")
db_describe_site <- function(db,
                             site_code,
                             sitetypecv = 'Site',
                             site_name = NULL,
                             site_description = NULL){

  if (!class(db) %in% c("SQLiteConnection", "PostgreSQLConnection")) {
    stop("sorry, only sqlite and postgresql database connections are supported so far")}


  # check type of database object
  if (class(db) == "SQLiteConnection"){
    sql1 <- RSQLite::dbSendStatement(db,
                                     'INSERT or IGNORE INTO samplingfeatures
                                     (samplingfeatureuuid, samplingfeaturetypecv, samplingfeaturecode)
                                     VALUES
                                     (:samplingfeatureuuid, :samplingfeaturetypecv, :samplingfeaturecode)')
    RSQLite::dbBind(sql1, param = list(samplingfeatureuuid = uuid::UUIDgenerate(),
                                       samplingfeaturetypecv = sitetypecv,
                                       samplingfeaturecode = site_code))
    RSQLite::dbClearResult(res = sql1)
    if(!is.null(site_name)){
      sql2 <- RSQLite::dbSendStatement(db,
                                       'UPDATE samplingfeatures
                                       SET samplingfeaturename = :sitename
                                       WHERE samplingfeaturecode = :samplingfeaturecode')
      RSQLite::dbBind(sql2, param = list(sitename = site_name,
                                         samplingfeaturecode = site_code))
      RSQLite::dbClearResult(res = sql2)
    }
    if(!is.null(site_description)){
      sql3 <- RSQLite::dbSendStatement(db,
                                       'UPDATE samplingfeatures
                                       SET samplingfeaturedescription = :sitedescription
                                       WHERE samplingfeaturecode = :samplingfeaturecode')
      RSQLite::dbBind(sql3, param = list(sitedescription = site_description,
                                         samplingfeaturecode = site_code))
      RSQLite::dbClearResult(res = sql3)
    }
    message(glue::glue("Site {site_code} has been entered into the samplingfeatures table."))
  }

  # check type of database object
  if (class(db) == "PostgreSQLConnection"){

    sql1 <- DBI::sqlInterpolate(db,
                                'INSERT INTO odm2.samplingfeatures
                                     (samplingfeatureuuid, samplingfeaturetypecv, samplingfeaturecode)
                                     VALUES
                                     (?samplingfeatureuuid, ?samplingfeaturetypecv, ?samplingfeaturecode)',
              samplingfeatureuuid = uuid::UUIDgenerate(),
              samplingfeaturetypecv = 'Site',
              samplingfeaturecode = site_code)

    RPostgreSQL::dbGetQuery(db, sql1)

    if(!is.null(site_name)){
      sql2 <- DBI::sqlInterpolate(db,
                                       'UPDATE odm2.samplingfeatures
                                       SET samplingfeaturename = ?sitename
                                       WHERE samplingfeaturecode = ?samplingfeaturecode',
                                  sitename = site_name,
                                         samplingfeaturecode = site_code)
      RPostgreSQL::dbGetQuery(db, sql2)
    }
    if(!is.null(site_description)){
      sql3 <- DBI::sqlInterpolate(db,
                                       'UPDATE odm2.samplingfeatures
                                       SET samplingfeaturedescription = ?sitedescription
                                       WHERE samplingfeaturecode = ?samplingfeaturecode',
                                  sitedescription = site_description,
                                         samplingfeaturecode = site_code)
      RPostgreSQL::dbGetQuery(db, sql3)
    }
    message(paste("Site", site_code, "has been entered into the samplingfeatures table."))
  }
}



#' Get list of sampling feature codes currently in database
#'
#' @param db database connection object
#'
#' @return Sampling feature codes for everything that is not
#' sampling feature type 'Specimen'
#' @export
#'
#' @examples
#' db <- create_sqlite(connect = TRUE)
#' db_describe_site(db, site_code = "site1")
#' db_get_sites(db)
#'
db_get_sites <- function(db){
  if (!class(db) %in% c("SQLiteConnection", "PostgreSQLConnection")) {
    stop("sorry, only sqlite and postgresql database connections are supported so far")}
  current_sites <- c()
  if (class(db) == "SQLiteConnection"){
    current_sites <- DBI::dbGetQuery(db, "SELECT samplingfeaturecode FROM samplingfeatures
                                     WHERE samplingfeaturetypecv != 'Specimen'")[[1]]
    # RSQLite::dbClearResult()
  }
  if (class(db) == "PostgreSQLConnection"){
    current_sites <- DBI::dbGetQuery(db, "SELECT samplingfeaturecode FROM odm2.samplingfeatures
                                     WHERE samplingfeaturetypecv != 'Specimen'")[[1]]
  }
  return(current_sites)
}

