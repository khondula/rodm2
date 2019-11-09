#' Describe a new site
#'
#' @param db database connection object
#' @param site_code unique short code name (required)
#' @param site_name optional longer site name
#' @param site_description optional longer site description
#' @param sitetypecv samplingfeature type from [controlled vocab](http://vocabulary.odm2.org/samplingfeaturetype/) such as "Site" or "Weather station"
#'
#' @return message that site was added
#' @export
#'
#' @examples
#' # just add site name
#' #db <- rodm2::connect_sqlite()
#' #db_describe_site(db, site_code = "new_site")
db_describe_site <- function(db, site_code, sitetypecv = 'Site',
                             site_name = NULL, site_description = NULL){

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
    message(paste("Site", site_code, "has been entered into the samplingfeatures table."))
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



#' Get list of site codes currently in database
#'
#' @param db database connection object
#'
#' @return the current values in the samplingfeaturecode column
#' @export
#'
#' @examples
#' #db_get_sites(db)
db_get_sites <- function(db){
  if (!class(db) %in% c("SQLiteConnection", "PostgreSQLConnection")) {
    stop("sorry, only sqlite and postgresql database connections are supported so far")}
  current_sites <- c()
  if (class(db) == "SQLiteConnection"){
    current_sites <- DBI::dbGetQuery(db, "SELECT samplingfeaturecode FROM samplingfeatures
                                     WHERE samplingfeaturetypecv = 'Site'")[[1]]
    # RSQLite::dbClearResult()
  }
  if (class(db) == "PostgreSQLConnection"){
    current_sites <- DBI::dbGetQuery(db, "SELECT samplingfeaturecode FROM odm2.samplingfeatures
                                     WHERE samplingfeaturetypecv = 'Site'")[[1]]
  }
  return(current_sites)
}

#' Get list of sample codes currently in database
#'
#' @param db database connection object
#'
#' @return the current values in the samplingfeaturecode column
#' @export
#'
#' @examples
#' #db_get_samples(db)
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

