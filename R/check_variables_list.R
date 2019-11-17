#' Check that the names of a variables list are in a database
#'
#' @param db database connection
#' @param variables named list of variables
#'
#' @return messages about each new variable entered
#' @export
#'
#' @examples
#' #check_variables_list(db, varables = vars_list)
check_variables_list <- function(db, variables){
  if (class(db) == "SQLiteConnection"){
    # check that all variables are in variables table

    # check that all variables are in variables table
    for(newvar in names(variables)){
      sql <- "INSERT OR IGNORE into variables
      (variabletypecv, variablecode, variablenamecv, nodatavalue)
      VALUES
      (:variabletypecv, :variablecode, :variablenamecv, :nodatavalue)"
      sql <- RSQLite::dbSendQuery(db, sql)
      RSQLite::dbBind(sql, params = list(variabletypecv = "Unknown",
                                         variablecode = newvar,
                                         variablenamecv = variables[[newvar]][["name"]],
                                         nodatavalue = '-9999'))
      RSQLite::dbClearResult(res = sql)

      if(!is.null(variables[[newvar]]$nodatavalue)){
        sql <- RSQLite::dbSendQuery(db,
                                    "UPDATE variables SET nodatavalue = :nodatavalue
                                    WHERE variablecode = :variablecode")
        RSQLite::dbBind(sql, params = list(nodatavalue = variables[[newvar]]$nodatavalue,
                                           variablecode = newvar))
        RSQLite::dbClearResult(res = sql)
      }
      if(!is.null(variables[[newvar]]$variabletypecv)){
        sql <- RSQLite::dbSendQuery(db,
                                    "UPDATE variables SET variabletypecv = :variabletypecv
                                    WHERE variablecode = :variablecode")
        RSQLite::dbBind(sql, params = list(variabletypecv = variables[[newvar]]$variabletypecv,
                                           variablecode = newvar))
        RSQLite::dbClearResult(res = sql)
      }
      if(!is.null(variables[[newvar]]$variabledefinition)){
        sql <- RSQLite::dbSendQuery(db,
                                    "UPDATE variables SET variabledefinition = :variabledefinition
                                    WHERE variablecode = :variablecode")
        RSQLite::dbBind(sql, params = list(variabledefinition = variables[[newvar]]$variabledefinition,
                                           variablecode = newvar))
        RSQLite::dbClearResult(res = sql)
      }
      message(paste(newvar, "has been added to the Variables table."))
    }

  }
  if (class(db) == "PostgreSQLConnection"){
    vars_to_add <- setdiff(names(variables), rodm2::db_get_variables(db)[[1]])
    for(newvar in vars_to_add){
      sql <- "INSERT into odm2.variables
      (variabletypecv, variablecode, variablenamecv, nodatavalue)
      VALUES
      (?variabletypecv, ?variablecode, ?variablenamecv, ?nodatavalue)"
      sql <- DBI::sqlInterpolate(db, sql,
                                 variabletypecv = "Unknown",
                                 variablecode = newvar,
                                 variablenamecv = variables[[newvar]][["name"]],
                                 nodatavalue = "-9999")
      RPostgreSQL::dbGetQuery(db, sql)

      if(!is.null(variables[[newvar]]$nodatavalue)){
        sql <- DBI::sqlInterpolate(db,
                                   "UPDATE odm2.variables SET nodatavalue = ?nodatavalue
                                   WHERE variablecode = ?variablecode",
                                   nodatavalue = variables[[newvar]]$nodatavalue,
                                   variablecode = newvar)
        RPostgreSQL::dbGetQuery(db, sql)
      }

      if(!is.null(variables[[newvar]]$variabletypecv)){
        sql <- DBI::sqlInterpolate(db,
                                   "UPDATE odm2.variables SET variabletypecv = ?variabletypecv
                                   WHERE variablecode = ?variablecode",
                                   variabletypecv = variables[[newvar]]$variabletypecv,
                                   variablecode = newvar)
        RPostgreSQL::dbGetQuery(db, sql)
      }

      if(!is.null(variables[[newvar]]$variabledefinition)){
        sql <- DBI::sqlInterpolate(db,
                                   "UPDATE odm2.variables SET variabledefinition = ?variabledefinition
                                   WHERE variablecode = ?variablecode",
                                   variabledefinition = variables[[newvar]]$variabledefinition,
                                   variablecode = newvar)
        RPostgreSQL::dbGetQuery(db, sql)
      }
      message(paste(newvar, "has been added to the Variables table."))
    }
  }
}
