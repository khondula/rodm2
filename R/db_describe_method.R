#' Describe a new method
#'
#' @param methodname full name of method
#' @param methodcode short codename for method
#' @param methodtypecv method type from controlled vocabulary
#' @param methoddescription optional longer description
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
                               methoddescription = NULL){

  if (!class(db) %in% c("SQLiteConnection", "PostgreSQLConnection")) {
    stop("sorry, only sqlite and postgresql database connections are supported so far")}

  # check type of database object
  if (class(db) == "SQLiteConnection"){

    sql1 <- RSQLite::dbSendStatement(db,
              'INSERT INTO methods
              (methodtypecv, methodcode, methodname)
              VALUES
              (:methodtypecv, :methodcode, :methodname)')
    RSQLite::dbBind(sql1, param = list(methodtypecv = methodtypecv,
                                       methodcode = methodcode,
                                       methodname = methodname))
    RSQLite::dbClearResult(res = sql1)

    if(!is.null(methoddescription)){
      sql2 <- RSQLite::dbSendStatement(db, "UPDATE methods SET methoddescription = :methoddescription WHERE methodcode = :methodcode")
      RSQLite::dbBind(sql2, params = list(methoddescription = methoddescription,
                                          methodcode = methodcode))
      RSQLite::dbClearResult(res = sql2)
    }
    message(paste(methodname, "has been added to the Methods table."))

  }

  if (class(db) == "PostgreSQLConnection"){
    sql <- DBI::sqlInterpolate(db, "INSERT INTO odm2.methods
                  (methodtypecv, methodcode,
                  methodname)
                  VALUES
                  (?methodtypecv, ?methodcode,
                  ?methodname)",
                methodtypecv = methodtypecv,
                methodcode = methodcode,
                methodname = methodname)

    RPostgreSQL::dbGetQuery(db, sql)

    if(!is.null(methoddescription)){
      sql2 <- DBI::sqlInterpolate(db, "UPDATE odm2.methods
                                  SET methoddescription = ?methoddescription
                                  WHERE methodcode = :methodcode",
                                  methoddescription = methoddescription,
                                          methodcode = methodcode)
      RPostgreSQL::dbGetQuery(db, sql2)
    }

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
  current_methods <- c()
  if (class(db) == "SQLiteConnection"){
    current_methods <- DBI::dbGetQuery(db, "SELECT methodcode FROM methods")$MethodCode
  }
  if (class(db) == "PostgreSQLConnection"){
    current_methods <- DBI::dbGetQuery(db, "SELECT methodcode FROM odm2.methods")$methodcode
  }
  return(current_methods)
}
