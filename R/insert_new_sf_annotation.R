
#' Insert new samplingfeature annotation into samplingfeatureannotations table
#'
#' @param samplingfeaturecode
#' @param annotationtext
#'
#' @return
#' @export
#'
#' @examples
insert_new_sf_annotation <- function(samplingfeaturecode,
                                     annotationtext){

  sql1 <- RSQLite::dbSendQuery(db, 'SELECT annotationid FROM annotations
                               WHERE annotationtext = :annotationtext')
  RSQLite::dbBind(sql1, params = list(annotationtext = annotationtext))
  my_annotationid <-  RSQLite::dbFetch(sql1)
  RSQLite::dbClearResult(sql1)

  sql2 <- RSQLite::dbSendQuery(db, 'SELECT samplingfeatureid FROM samplingfeatures
                               WHERE samplingfeaturecode = :samplingfeaturecode')
  RSQLite::dbBind(sql2, params = list(samplingfeaturecode = samplingfeaturecode))
  my_sfid <-  RSQLite::dbFetch(sql2)
  RSQLite::dbClearResult(sql2)

  my_annotationid <- as.integer(my_annotationid)
  my_sfid <- as.integer(my_sfid)

  sql3 <- RSQLite::dbSendStatement(db, 'INSERT INTO samplingfeatureannotations
                                   (SamplingFeatureID, AnnotationID)
                                    VALUES
                                   (:samplingfeatureid, :annotationid)')
  RSQLite::dbBind(sql3, params = list(samplingfeatureid = my_sfid, annotationid = my_annotationid))
  RSQLite::dbClearResult(sql3)

}

#' Insert new annotation into annotations table
#'
#' @param annotationtypecv
#' @param annotationtext
#'
#' @return
#' @export
#'
#' @examples
#'
insert_new_annotation <- function(annotationtypecv,
                                  annotationtext){
  sql1 <- RSQLite::dbSendStatement(db, 'INSERT INTO annotations
                                   (annotationtypecv, annotationtext)
                                   VALUES
                                   (:annotationtypecv, :annotationtext)')
  RSQLite::dbBind(sql1, params = list(annotationtypecv = annotationtypecv, annotationtext = annotationtext))
  RSQLite::dbClearResult(sql1)

}
