#' Describe a new site
#'
#' @param db database connection object
#' @param site_code unique short code name (required)
#' @param site_name optional longer site name
#' @param site_description optional longer site description
#'
#' @return message that site was added
#' @export
#'
#' @examples
#' # just add site name
#' db <- rodm2::create_sqlite(connect = TRUE)
#' db_describe_site(db, site_code = "new_site")
db_describe_site <- function(db, site_code, site_name = NULL, site_description = NULL){

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
                                       samplingfeaturetypecv = 'Site',
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

#' Describe a new site with geometry
#'
#' @param db database connection
#' @param site_code unique short code name for site or maybe column of sf with site code
#' @param site_geometry sf object with point coordinates of site
#' @param sitetype term from sitetype controlled vocabulary
#' @param ... additional parameters to db_describe_site (site_name, site_description)
#'
#' @return message that site has been added
#' @export
#'
#' @examples
#' # add a site with coordinate information
#' db <- rodm2::create_sqlite(connect = TRUE)
#' pt1 <- sf::st_sfc(sf::st_point(c(-76.503394, 38.976546)),crs = sf::st_crs(4326))
#' site_sf <- sf::st_sf(data.frame(site_code = "my_site", geom = pt1))
#' db_describe_site_geom(db, site_code = "my_code", site_geometry = site_sf, sitetype = 'Unknown')
db_describe_site_geom <- function(db,
                                  site_code,
                                  site_geometry,
                                  sitetype, ...){
  # if (!class(db) %in% c("SQLiteConnection", "PostgreSQLConnection")) {
  #   stop("sorry, only sqlite and postgresql database connections are supported so far")}

  if (!class(db) %in% c("SQLiteConnection")) {
    stop("sorry, only sqlite database connections are supported so far")}

  # check type of database object
  if (class(db) == "SQLiteConnection"){

    # if () {
    #   stop("Please supply site_geometry as an sf object, eg. using sf::st_as_sf().")}
    if (is.na(sf::st_crs(site_geometry))) {
      stop("Missing coordinate reference system. Please set crs of site_geometry.")}


    rodm2::db_describe_site(db, site_code = site_code, ...)

    sql2 <- RSQLite::dbSendStatement(db,
                "UPDATE samplingfeatures SET featuregeometry = :featuregeometry
                WHERE samplingfeaturecode = :sitecode")
    RSQLite::dbBind(sql2, param = list(featuregeometry = sf::st_as_text(site_geometry$geometry),
                                      sitecode = site_code))
    RSQLite::dbClearResult(res = sql2)

    sql3 <- RSQLite::dbSendStatement(db,
                                    'INSERT or IGNORE INTO spatialreferences
                                    (srscode, srsname)
                                    VALUES
                                    (:srscode,
                                    :srsname)')
    RSQLite::dbBind(sql3, param = list(srscode = as.character(sf::st_crs(site_geometry)[1]),
                                      srsname = paste('EPSG:', sf::st_crs(site_geometry)[1])))
    RSQLite::dbClearResult(res = sql3)

    sql4 <- RSQLite::dbSendStatement(db,
                                     'INSERT INTO sites
                                    (samplingfeatureid,
                                     sitetypecv,
                                     latitude, longitude,
                                     spatialreferenceid)
                                     VALUES (
                                     (SELECT samplingfeatureid
                                     FROM samplingfeatures
                                     WHERE samplingfeaturecode = :sitecode),
                                     :sitetypecv,
                                     :latitude, :longitude,
                                     (SELECT spatialreferenceid
                                     FROM spatialreferences
                                     WHERE srscode = :srscode))')
    RSQLite::dbBind(sql4, param = list(sitecode = site_code,
                                      sitetypecv = sitetype,
                                      latitude = sf::st_coordinates(site_geometry)[,"Y"],
                                      longitude = sf::st_coordinates(site_geometry)[,"X"],
                                      srscode = as.character(sf::st_crs(site_geometry)[1])))
    RSQLite::dbClearResult(res = sql4)
  }


}

#' Get list of site codes currently in database
#'
#' @param db
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
    RSQLite::dbClearResult()
  }
  if (class(db) == "PostgreSQLConnection"){
    current_sites <- DBI::dbGetQuery(db, "SELECT samplingfeaturecode FROM odm2.samplingfeatures
                                     WHERE samplingfeaturetypecv = 'Site'")[[1]]
  }
  return(current_sites)
}

#' Get list of sample codes currently in database
#'
#' @param db
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

