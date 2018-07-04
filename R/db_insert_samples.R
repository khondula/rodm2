
#' Insert new samples from a new_samples dataframe
#'
#' @param x
#'
#' @return TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' db_insert_samples(1)
#' }
db_insert_samples <- function(x){

sql_blanks <- 'WITH
  newsf AS (
  INSERT INTO odm2.samplingfeatures (
    samplingfeaturecode,
    samplingfeaturename,
    samplingfeaturedescription,
    samplingfeaturetypecv,
    samplingfeatureuuid)
  VALUES (
    \'%s\',
    \'%s\',
    \'%s\',
    \'%s\',
    \'%s\')
  RETURNING samplingfeatureid),

  newspecimen AS (
    INSERT into odm2.specimens (
      samplingfeatureid,
      specimentypecv,
      specimenmediumcv,
      isfieldspecimen)
    VALUES (
      (SELECT newsf.samplingfeatureid from newsf),
      \'%s\',
      \'%s\',
      \'%s\')
    ),

  newrelation AS (
    INSERT INTO odm2.relatedfeatures (
      samplingfeatureid,
      relationshiptypecv,
      relatedfeatureid)
    VALUES (
      (SELECT newsf.samplingfeatureid FROM newsf),
      \'%s\',
      (SELECT samplingfeatureid FROM odm2.samplingfeatures WHERE samplingfeaturecode = \'%s\'))),

  newact AS (
    INSERT INTO odm2.actions (
      actiontypecv,
      methodid,
      begindatetime,
      begindatetimeutcoffset)
    VALUES (
      \'%s\',
      (SELECT methodid FROM odm2.methods WHERE methodcode = \'%s\'),
      \'%s\',
      \'%s\')
      RETURNING actionid)

    INSERT into odm2.featureactions (
      samplingfeatureid,
      actionid)
    VALUES (
      (SELECT newsf.samplingfeatureid FROM newsf),
      (SELECT newact.actionid FROM newact))
    RETURNING featureactionid'

sql <- sprintf(sql_blanks,
        new_samples$sample_code[x],
        new_samples$sample_name[x],
        new_samples$sample_description[x],
        "Specimen",
        new_samples$new_samples_uuid[x],
        specimentypecv,
        new_samples$specimenmediumcv[x],
        isfieldspecimen,
        "Was collected at",
        new_samples$origin_site[x],
        "Specimen collection",
        new_samples$methodcode[x],
        new_samples$datetime[x],
        utcoffset)

sql <- gsub("\n", "", sql)

RPostgreSQL::dbGetQuery(db, sql)

}
