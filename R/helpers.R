#' Search for site names with a pattern
#'
#' Uses agrep to search the samplingfeaturecode column of the samplingfeature table
#'
#' @param ... extra parameters to pass to agrep
#' @param x pattern to search for
#' @param db database connection
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @return matching elements if the samplingfeaturecode vector
#'
#' @examples
#' \dontrun{
#' get_site_names_like("weather")
#' # Pass extra arguments to agrep
#' get_site_names_like("weather", max.distance = 2)
#' }
#' @export

get_site_names_like <- function(x, db, ...){
  samplingfeatures <- c()
  if (class(db) == "SQLiteConnection"){
    samplingfeatures <- RSQLite::dbGetQuery(db, "SELECT samplingfeaturecode FROM samplingfeatures")
  }
  if (class(db) == "PostgreSQLConnection"){
    samplingfeatures <- RPostgreSQL::dbGetQuery(db, "SELECT samplingfeaturecode FROM odm2.samplingfeatures")
  }
  agrep(pattern = x,
        samplingfeatures$samplingfeaturecode,
        ignore.case = TRUE, value = TRUE, ...)
}

#' Compare sampling feature names to existing ones
#'
#' Returns a data frame specifying whether each element of new_codes exists in samplingfeatures
#'
#' @param new_codes vector of names to compare with samplingfeaturecode vector
#' @param db database connection (defaults to db)
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#'
#' @return data frame with new_codes and in_db column
#'
#' @examples
#' \dontrun{
#' #
#' check_samplingfeaturecodes(new_codes = new_sites$sites)
#' }
#'
#' @export

check_samplingfeaturecodes <- function(new_codes, db = db){
  samplingfeatures <- c()
  if (class(db) == "SQLiteConnection"){
    samplingfeatures <- RSQLite::dbGetQuery(db, "SELECT samplingfeaturecode FROM samplingfeatures")
  }
  if (class(db) == "PostgreSQLConnection"){
    samplingfeatures <- RPostgreSQL::dbGetQuery(db, "SELECT samplingfeaturecode FROM odm2.samplingfeatures")
  }

    data.frame(new_codes = unique(new_codes),
             in_db = unique(new_codes) %in%
               samplingfeatures$samplingfeaturecode) %>%
    dplyr::filter(!in_db) %>% dplyr::arrange(new_codes)
}

#' Insert new processinglevel
#'
#' @param db database connection
#' @param processinglevel processinglevel code
#'
#' @return message if succesful
#' @export
#'
#' @examples
#' #insert_processinglevel(db, "Quality controlled data")
#'
insert_processinglevel <- function(db, processinglevel){
  if (class(db) == "SQLiteConnection"){
  sql <- "INSERT or IGNORE into processinglevels (processinglevelcode) VALUES (:processinglevel)"
  sql <- RSQLite::dbSendQuery(db, sql)
  RSQLite::dbBind(sql, params = list(processinglevel = processinglevel))
  RSQLite::dbClearResult(res = sql)
  }

  if (class(db) == "PostgreSQLConnection"){
    if(!processinglevel %in%
     DBI::dbGetQuery(db, "SELECT definition from odm2.processinglevels")$definition){
    sql <- "INSERT into odm2.processinglevels (processinglevelcode, defintion)
    VALUES (?processinglevel, ?processinglevel)"
    sql <- DBI::sqlInterpolate(db, sql, processinglevel = processinglevel)
    RPostgreSQL::dbGetQuery(db, sql)
    }
  }
}

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
                                         variablenamecv = newvar,
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
                                 variablenamecv = newvar,
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
