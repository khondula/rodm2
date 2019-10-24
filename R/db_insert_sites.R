#' Insert new sites from a table
#'
#' @param x
#'
#' @return TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' db_insert_sites(new_sites)
#' }
db_insert_sites <- function(db,
                            new_sites,
                            SamplingFeatureTypeCV = "site",
                            code_name_column = "SamplingFeatureCode",
                            long_name_column = NULL,
                            description_column = NULL,
                            geometry_wkt_column = NULL,
                            elevation_column = NULL,
                            add_uuids = TRUE){

  if (!class(db) %in% c("SQLiteConnection", "PostgreSQLConnection")) {
    stop("sorry, only sqlite and postgresql database connections are supported so far")}

  # add UUIDs to new_sites data frame if add_uuids = true


  # check type of database object
  if (class(db) == "SQLiteConnection"){

    sql1 <- RSQLite::dbSendStatement(db,
                                     'INSERT into samplingfeatures
                                     (samplingfeatureuuid,
                                      samplingfeaturetypecv,
                                     samplingfeaturecode,
                                     samplingfeaturedescription)
                                     VALUES
                                     (:samplingfeatureuuid,
                                      :samplingfeaturetypecv,
                                     :samplingfeaturecode,
                                     :samplingfeaturedescription)')
    RSQLite::dbBind(sql1, param = list(variabletypecv = variabletypecv,
                                       variablecode = variablecode,
                                       variablenamecv = variablenamecv,
                                       nodatavalue = nodatavalue))
    RSQLite::dbClearResult(res = sql1)
  }

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
