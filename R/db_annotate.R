
#' Insert new annotation
#'
#' @param annotationtext Text of annotation
#' @param db database connecton object
#' @param type annotation type from [controlled vocab](http://vocabulary.odm2.org/annotationtype/)
#' @param annotationcode annotation codename
#' @param object An existing object in the database to annotate such as a site or specimen code.
#'
#' @return TRUE if successful
#' @export
#' @details This function adds an existing annotation to an existing feature in the database
#' such as a site code, sampling feature, or action. Once objects are annotated, they can be
#' more easily queried as a group based on these labels or tags that are not otherwise
#' represented in the database structure.
#' Either an annotation code or annotation text must be provided to identify the annotation
#'
#' @examples
#' # db_annotate(db, object = "Site 001", annotationtext = "Riparian wells")
db_annotate <- function(db,
                        object,
                        annotationtext = NULL,
                        type = "Site group",
                        annotationcode = NULL){

  if (!class(db) %in% c("SQLiteConnection")) {
    stop("sorry, only sqlite database connections are supported so far")}

  sf_annotation_types <- c("Site group", "Sampling feature annotation",
                           "Site annotation", "Specimen annotation",
                           "Specimen group")

  action_annotation_types <- c("Action annotation", "Action group")

  if (!type %in% sf_annotation_types) {
    stop("sorry, only sampling feature annotations supported so far")}

  if(!any(!is.null(annotationcode), !is.null(annotationtext))){
    stop("Please supply either an annotation code or annotation text")
  }

  # annotation text is required, if null use annotation code
  if(is.null(annotationtext)){
    annotationtext <- annotationcode
  }

  # annotation code needs to be not null for sql
  if(is.null(annotationcode)){
    annotationcode <- ""
  }

  if(!type %in% suppressMessages(get_cv_terms("annotationtype"))){
    stop("Please use an annotation type from the CV.\nPrint options in console using: get_cv_terms(\"annotationtype\", quietly = FALSE)")
  }

  # first get annotation id based on type and code or text
  sql1 <- RSQLite::dbSendQuery(db, 'SELECT annotationid FROM annotations
                               WHERE annotationtypecv = :annotationtypecv
AND (annotationtext = :annotationtext
                               OR annotationcode = :annotationcode)')
  RSQLite::dbBind(sql1, params = list(annotationtypecv = type,
                                      annotationtext = annotationtext,
                                      annotationcode = annotationcode))
  my_annotationid <-  RSQLite::dbFetch(sql1)
  my_annotationid <- my_annotationid[[1]][1] # only pick first one if multiple returned
  RSQLite::dbClearResult(sql1)

  # then get the feature to apply the annotation to
  if(type %in% sf_annotation_types){
    sql2 <- RSQLite::dbSendQuery(db, 'SELECT samplingfeatureid FROM samplingfeatures
                               WHERE samplingfeaturecode = :samplingfeaturecode')
    RSQLite::dbBind(sql2, params = list(samplingfeaturecode = object))
    my_sfid <-  RSQLite::dbFetch(sql2)
    RSQLite::dbClearResult(sql2)

    my_annotationid <- as.integer(my_annotationid)
    my_sfid <- as.integer(my_sfid)

    # insert into appropriate bridge table to apply annotation
    sql3 <- RSQLite::dbSendStatement(db, 'INSERT OR IGNORE INTO samplingfeatureannotations
                                   (SamplingFeatureID, AnnotationID)
                                    VALUES
                                   (:samplingfeatureid, :annotationid)')
    RSQLite::dbBind(sql3, params = list(samplingfeatureid = my_sfid, annotationid = my_annotationid))
    RSQLite::dbClearResult(sql3)

    message(paste(object, "has been labeled with annotation text", annotationtext))
  }



}
