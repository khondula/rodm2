#' Insert a new variable
#'
#' @param variabletypecv 
#' @param variablecode 
#' @param variablenamecv 
#' @param variabledefinition 
#' @param nodatavalue 
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' db_add_variable()
#' }

db_add_variable <- function(variabletypecv, variablecode, variablenamecv, variabledefinition, nodatavalue = -9999){
  sql <- sprintf("INSERT INTO odm2.variables 
                 (variabletypecv, variablecode, variablenamecv, variabledefinition, nodatavalue) 
                 VALUES 
                 ('%s', '%s', '%s', '%s', '%s')",
                 variabletypecv, variablecode, variablenamecv, variabledefinition, nodatavalue)
  sql <- gsub("\n", "", sql)
  RPostgreSQL::dbGetQuery(db, sql)
}