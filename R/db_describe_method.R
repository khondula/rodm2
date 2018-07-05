#' Describe a new method
#'
#' @param methodname full name of method
#' @param methodcode short codename for method
#' @param methodtypecv method type from controlled vocabulary
#' @param methoddescription longer description
#'
#' @return TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' db_describe_method("new method", "new", "Field Activity", "doing stuf")
#' }
db_describe_method <- function(methodname, methodcode, methodtypecv, methoddescription){
  sql <- sprintf("INSERT INTO odm2.methods
                 (methodtypecv, methodcode, methodname, methoddescription)
                 VALUES
                 ('%s', '%s', '%s', '%s')",
                 methodtypecv, methodcode, methodname, methoddescription)
  sql <- gsub("\n", "", sql)
  RPostgreSQL::dbGetQuery(db, sql)
}
