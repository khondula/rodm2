#' Describe a new variable
#'
#' @param variabletypecv variable type from controlled vocab
#' @param variablecode short codename for variable
#' @param variablenamecv variable name from controlled vocab
#' @param variabledefinition longer definition
#' @param nodatavalue no data value
#' @param db database connection object
#'
#' @return TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' db_describe_variable()
#' }

db_describe_variable <- function(db,
                                 variabletypecv,
                                 variablecode,
                                 variablenamecv,
                                 variabledefinition,
                                 nodatavalue = -9999){

  if (!class(db) %in% c("SQLiteConnection", "PostgreSQLConnection")) {
    stop("sorry, only sqlite and postgresql database connections are supported so far")}

  # check type of database object
  if (class(db) == "SQLiteConnection"){

    sql1 <- RSQLite::dbSendStatement(db,
                                     'INSERT into variables
                                    (variabletypecv, variablecode,
                                    variablenamecv, variabledefinition,
                                     nodatavalue)
                                     VALUES
                                     (:variabletypecv, :variablecode,
                                     :variablenamecv, :variabledefinition,
                                     :nodatavalue)')
    RSQLite::dbBind(sql1, param = list(variabletypecv = variabletypecv,
                                       variablecode = variablecode,
                                       variablenamecv = variablenamecv,
                                       variabledefinition = variabledefinition,
                                       nodatavalue = nodatavalue))
    RSQLite::dbClearResult(res = sql1)
    message(paste(variablenamecv, "has been added to the Variables table."))

  }

  if (class(db) == "PostgreSQLConnection"){

  sql <- DBI::sqlInterpolate("INSERT INTO odm2.variables
                 (variabletypecv, variablecode,
                  variablenamecv, variabledefinition,
                  nodatavalue)
                 VALUES
                 (?variabletypecv, ?variablecode,
                  ?variablenamecv, ?variabledefinition,
                  ?nodatavalue)",
                  variabletypecv = variabletypecv,
                  variablecode = variablecode,
                  variablenamecv = variablenamecv,
                  variabledefinition = variabledefinition,
                  nodatavalue = nodatavalue)

  RPostgreSQL::dbGetQuery(db, sql)
  message(paste(variablenamecv, "has been added to the Variables table."))
  }
}

#' Get list of variables currently in database
#'
#' @param db
#'
#' @return the current values in the variablenamecv column
#' @export
#'
#' @examples
#' #db_get_variables(db)
db_get_variables <- function(db){
  if (!class(db) %in% c("SQLiteConnection", "PostgreSQLConnection")) {
    stop("sorry, only sqlite and postgresql database connections are supported so far")}
  if (class(db) == "SQLiteConnection"){
    current_variables <- DBI::dbGetQuery(db, "SELECT variablenamecv FROM variables")
  }
  if (class(db) == "PosgreSQLConnection"){
    current_variables <- DBI::dbGetQuery(db, "SELECT variablenamecv FROM odm2.variables")
  }
  return(current_variables)
}
