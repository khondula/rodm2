
# lat is column POINT_Y
# lon is column POINT_X

#' Insert new sites from a new_sites table
#'
#' @param x
#'
#' @return TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' db_insert_sites_latlon(new_sites)
#' }
db_insert_sites_latlon <- function(x){
  sql_blanks <- 'WITH
  newsf AS (
  INSERT INTO odm2.samplingfeatures (
  samplingfeatureuuid,
  samplingfeaturetypecv,
  samplingfeaturecode,
  samplingfeaturedescription)
  VALUES (
  \'%s\',
  \'%s\',
  \'%s\',
  \'%s\')
  RETURNING samplingfeatureid)

  INSERT INTO odm2.sites (
  samplingfeatureid,
  sitetypecv,
  latitude,
  longitude,
  spatialreferenceid)
  VALUES (
  (SELECT newsf.samplingfeatureid FROM newsf),
  \'%s\',
  \'%s\',
  \'%s\',
  (SELECT spatialreferenceid FROM odm2.spatialreferences WHERE srscode = \'%s\'))
  '
   sql <- sprintf(sql_blanks,
                   new_sites$samplingfeatureuuid[x],
                   "Site",
                   new_sites$samplingfeaturecode[x],
                   new_sites$samplingfeaturedescription[x],
                   new_sites$sitetypecv[x],
                   new_sites$POINT_Y[x],
                   new_sites$POINT_X[x],
                   new_sites$srscode[x])
    sql <- gsub("\n", "", sql)
    RPostgreSQL::dbGetQuery(db, sql)

}
