#' Describe a new variable
#'
#' @param variabletypecv variable type from \href{http://vocabulary.odm2.org/variabletype}{controlled vocabulary}
#' @param variablecode short codename for variable
#' @param variablenamecv variable name from \href{http://vocabulary.odm2.org/variablename}{controlled vocabulary}
#' @param variabledefinition optional longer definition
#' @param nodatavalue no data value
#' @param db database connection object
#'
#' @return TRUE if successful
#' @export
#' @family describe functions

#' @examples
#' \dontrun{
#' db_describe_variable()
#' }

db_describe_variable <- function(db,
                                 variablecode,
                                 variabletypecv,
                                 variablenamecv,
                                 variabledefinition = NULL,
                                 nodatavalue = -9999){

  if (!class(db) %in% c("SQLiteConnection", "PostgreSQLConnection")) {
    stop("sorry, only sqlite and postgresql database connections are supported so far")}

  # check type of database object
  if (class(db) == "SQLiteConnection"){

    sql1 <- RSQLite::dbSendStatement(db,
                                     'INSERT into variables
                                    (variabletypecv, variablecode,
                                    variablenamecv,
                                     nodatavalue)
                                     VALUES
                                     (:variabletypecv, :variablecode,
                                     :variablenamecv,
                                     :nodatavalue)')
    RSQLite::dbBind(sql1, param = list(variabletypecv = variabletypecv,
                                       variablecode = variablecode,
                                       variablenamecv = variablenamecv,
                                       nodatavalue = nodatavalue))
    RSQLite::dbClearResult(res = sql1)
    if(!is.null(variabledefinition)){
      sql2 <- RSQLite::dbSendStatement(db, "UPDATE variables
                                       SET variabledefinition = :variabledefinition
                                       WHERE variablecode = :variablecode")
      RSQLite::dbBind(sql2, params = list(variabledefinition = variabledefinition,
                                          variablecode = variablecode))
      RSQLite::dbClearResult(res = sql2)
    }
    message(paste(variablenamecv, "has been added to the Variables table as", variablecode,"."))

  }

  if (class(db) == "PostgreSQLConnection"){

  sql <- DBI::sqlInterpolate(db, "INSERT INTO odm2.variables
                 (variabletypecv, variablecode,
                  variablenamecv,
                  nodatavalue)
                 VALUES
                 (?variabletypecv, ?variablecode,
                  ?variablenamecv,
                  ?nodatavalue)",
                  variabletypecv = variabletypecv,
                  variablecode = variablecode,
                  variablenamecv = variablenamecv,
                  nodatavalue = nodatavalue)

  RPostgreSQL::dbGetQuery(db, sql)
  if(!is.null(variabledefinition)){
    sql2 <- DBI::sqlInterpolate(db, "UPDATE odm2.variables
                                  SET variabledefinition = ?variabledefinition
                                  WHERE variablecode = ?variablecode",
                                variabledefinition = variabledefinition,
                                variablecode = variablecode)
    RPostgreSQL::dbGetQuery(db, sql2)
  }
  message(paste(variablenamecv, "has been added to the Variables table as", variablecode,"."))
  }
}

#' Get list of variables currently in database
#'
#' @param db database connection object
#'
#' @return the current values in the variablenamecv column
#' @export
#'
#' @examples
#' #db_get_variables(db)
db_get_variables <- function(db){
  if (!class(db) %in% c("SQLiteConnection", "PostgreSQLConnection")) {
    stop("sorry, only sqlite and postgresql database connections are supported so far")}
  current_variables <- c()
  if (class(db) == "SQLiteConnection"){
    current_variables <- DBI::dbGetQuery(db, "SELECT variablecode FROM variables")
    # RSQLite::dbClearResult()
  }
  if (class(db) == "PostgreSQLConnection"){
    current_variables <- DBI::dbGetQuery(db, "SELECT variablecode FROM odm2.variables")
  }
  return(current_variables[[1]])
}
