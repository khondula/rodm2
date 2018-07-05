#' Describe a new variable
#'
#' @param variabletypecv variable type from controlled vocab
#' @param variablecode short codename for variable
#' @param variablenamecv variable name from controlled vocab
#' @param variabledefinition longer definition
#' @param nodatavalue no data value
#'
#' @return TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' db_describe_variable()
#' }

db_describe_variable <- function(variabletypecv, variablecode, variablenamecv, variabledefinition, nodatavalue = -9999){
  sql <- sprintf("INSERT INTO odm2.variables
                 (variabletypecv, variablecode, variablenamecv, variabledefinition, nodatavalue)
                 VALUES
                 ('%s', '%s', '%s', '%s', '%s')",
                 variabletypecv, variablecode, variablenamecv, variabledefinition, nodatavalue)
  sql <- gsub("\n", "", sql)
  RPostgreSQL::dbGetQuery(db, sql)
}
