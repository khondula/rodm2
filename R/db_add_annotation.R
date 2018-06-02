#' Add a new annotation
#'
#' @param annotationtypecv annotation type from controlled vocab
#' @param annotationtext text string of annotation
#'
#' @return TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' db_add_annotaion("samplingfeature annotation", "primary sites")
#' }
#'
db_add_annotation <- function(annotationtypecv,
                                  annotationtext){
  sql <- sprintf('INSERT INTO odm2.annotations (annotationtypecv, annotationtext) 
                 VALUES (\'%s\', \'%s\')', 
                 annotationtypecv, annotationtext)
  sql <- gsub("\n", "", sql)
  RPostgreSQL::dbGetQuery(db, sql)
}