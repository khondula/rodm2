#' Describe a new site
#'
#' @param db database connection
#' @param sitecode unique short code name for site
#' @param site_geometry sf object with point coordinates of site
#' @param sitetype term from sitetype controlled vocabulary
#'
#' @return message that site has been added
#' @export
#'
#' @examples
#' # just add site name
#' db <- create_sqlite(connect = TRUE)
#' db_describe_site(db, sitecode = "new_site")
#' # add a site with coordinate information
#' db_describe_site(db, sitecode = "new_site2", geometry = site_sf, sitetype = 'Unknown')
db_describe_site <- function(db,
                             sitecode,
                             site_geometry = NULL,
                             sitetype = NULL){
  # if (!class(db) %in% c("SQLiteConnection", "PostgreSQLConnection")) {
  #   stop("sorry, only sqlite and postgresql database connections are supported so far")}

  if (!class(db) %in% c("SQLiteConnection")) {
    stop("sorry, only sqlite database connections are supported so far")}

  # check type of database object
  if (class(db) == "SQLiteConnection"){
    sql1 <- RSQLite::dbSendStatement(db,
        'INSERT or IGNORE INTO samplingfeatures
        (samplingfeatureuuid, samplingfeaturetypecv, samplingfeaturecode)
        VALUES
        (:samplingfeatureuuid, :samplingfeaturetypecv, :samplingfeaturecode)')
    RSQLite::dbBind(sql1, param = list(samplingfeatureuuid = uuid::UUIDgenerate(),
                                      samplingfeaturetypecv = 'Site',
                                      samplingfeaturecode = sitecode))
    RSQLite::dbClearResult(res = sql1)
    message(paste("Site", sitecode, "has been entered into the samplingfeatures table."))
  }

  if(!is.null(sitetype) | !is.null(site_geometry)){
    if (!c("sf") %in% class(site_geometry)) {
      stop("Please supply site_geometry as an sf object, eg. using sf::st_as_sf().")}
    if (is.na(st_crs(site_geometry))) {
      stop("Missing coordinate reference system. Please set crs of site_geometry.")}

    sql2 <- RSQLite::dbSendStatement(db,
                "UPDATE samplingfeatures SET featuregeometry = :featuregeometry
                WHERE samplingfeaturecode = :sitecode")
    RSQLite::dbBind(sql2, param = list(featuregeometry = st_as_text(site_geometry$geometry),
                                      sitecode = sitecode))
    RSQLite::dbClearResult(res = sql2)

    sql3 <- RSQLite::dbSendStatement(db,
                                    'INSERT or IGNORE INTO spatialreferences
                                    (srscode, srsname, srsdescription)
                                    VALUES
                                    (:srscode,
                                    :srsname,
                                    :srsdescription)')
    RSQLite::dbBind(sql3, param = list(srscode = as.character(sf::st_crs(site_geometry)[1]),
                                      srsname = paste('EPSG:', sf::st_crs(site_geometry)[1]),
                                      srsdescription = srsdescription))
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
    RSQLite::dbBind(sql4, param = list(sitecode = sitecode,
                                      sitetypecv = sitetype,
                                      latitude = st_coordinates(site_geometry)[,"Y"],
                                      longitude = st_coordinates(site_geometry)[,"X"],
                                      srscode = as.character(st_crs(site_geometry)[1])))
    RSQLite::dbClearResult(res = sql4)
  }


}

