#' Describe a new method
#'
#' @param methodname full name of method
#' @param methodcode short codename for method
#' @param methodtypecv method type from controlled vocabulary
#' @param methoddescription longer description
#' @param db database connection object
#'
#' @return TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' db_describe_method("new method", "new", "Field Activity", "doing stuf")
#' }
db_describe_method <- function(db,
                               methodname,
                               methodcode,
                               methodtypecv,
                               methoddescription){

  if (!class(db) %in% c("SQLiteConnection", "PostgreSQLConnection")) {
    stop("sorry, only sqlite and postgresql database connections are supported so far")}

  # check type of database object
  if (class(db) == "SQLiteConnection"){

    sql1 <- RSQLite::dbSendStatement(db,
              'INSERT INTO methods
              (methodtypecv, methodcode, methodname, methoddescription)
              VALUES
              (:methodtypecv, :methodcode, :methodname, :methoddescription)')
    RSQLite::dbBind(sql1, param = list(methodtypecv = methodtypecv,
                                       methodcode = methodcode,
                                       methodname = methodname,
                                       methoddescription = methoddescription))
    RSQLite::dbClearResult(res = sql1)
    message(paste(methodname, "has been added to the Methods table."))

  }

  if (class(db) == "PostgreSQLConnection"){
    sql <- DBI::sqlInterpolate("INSERT INTO odm2.methods
                 (methodtypecv, methodcode,
                  methodname, methoddescription)
                 VALUES
                 (?methodtypecv, ?methodcode,
                  ?methodname, ?methoddescription)",
                methodtypecv = methodtypecv,
                methodcode = methodcode,
                methodname = methodname,
                methoddescription = methoddescription)

    RPostgreSQL::dbGetQuery(db, sql)
    message(paste(methodname, "has been added to the Methods table."))
  }
}


#' Get list of methods currently in database
#'
#' @param db
#'
#' @return the current values in the methodname column of the methods table
#' @export
#'
#' @examples
#' #db_get_methods(db)
db_get_methods <- function(db){
  if (!class(db) %in% c("SQLiteConnection", "PostgreSQLConnection")) {
    stop("sorry, only sqlite and postgresql database connections are supported so far")}
  if (class(db) == "SQLiteConnection"){
    current_methods <- DBI::dbGetQuery(db, "SELECT methodname FROM methods")
  }
  if (class(db) == "PosgreSQLConnection"){
    current_methods <- DBI::dbGetQuery(db, "SELECT methodname FROM odm2.methods")
  }
  return(current_methods)
}
