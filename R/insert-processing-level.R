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
