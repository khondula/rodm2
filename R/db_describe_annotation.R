#' Add a new annotation text
#'
#' @param annotationtypecv annotation type from controlled vocab
#' @param annotationtext text string of annotation
#'
#' @return TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' db_describe_annotaion("samplingfeature annotation", "primary sites")
#' }
#'
db_describe_annotation <- function(annotationtypecv,
                                  annotationtext){
  sql <- sprintf('INSERT INTO odm2.annotations (annotationtypecv, annotationtext)
                 VALUES (\'%s\', \'%s\')',
                 annotationtypecv, annotationtext)
  sql <- gsub("\n", "", sql)
  RPostgreSQL::dbGetQuery(db, sql)
}

db_describe_annotation_sqlite <- function(annotationtypecv,
                              annotationtext){
  sql <- sprintf('INSERT INTO annotations (annotationtypecv, annotationtext)
                 VALUES (\'%s\', \'%s\')',
                 annotationtypecv, annotationtext)
  sql <- gsub("\n", "", sql)
  dbExecute(db, sql)
}
