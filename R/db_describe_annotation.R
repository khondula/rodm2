#' Add a new annotation text
#'
#' @param annotationtypecv annotation type from controlled vocab
#' @param annotationtext text string of annotation
#' @param db database connection object
#'
#' @return TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' db_describe_annotaion("samplingfeature annotation", "primary sites")
#' }
#'
db_describe_annotation <- function(db,
                                   annotationtypecv,
                                  annotationtext){

  if (!class(db) %in% c("SQLiteConnection", "PostgreSQLConnection")) {
    stop("sorry, only sqlite and postgresql database connections are supported so far")}

  # check type of database object
  if (class(db) == "SQLiteConnection"){
    sql <- RSQLite::dbSendStatement(db,
  'INSERT INTO annotations (annotationtypecv, annotationtext)
                 VALUES (:annotationtypecv, :annotationtext)')
    RSQLite::dbBind(sql, param = list(annotationtypecv = annotationtypecv,
                                      annotationtext = annotationtext))
    RSQLite::dbClearResult(res = sql)
    message(paste(annotationtext, "has been entered into the annotations table."))
  }

  if (class(db) == "PostgreSQLConnection"){

    sql <- DBI::sqlInterpolate(db,
                               'INSERT into odm2.annotations
                               (annotationtypecv, annotationtext)
                               VALUES (?annotationtypecv, ?annotationtext)',
                               annotationtypecv = annotationtypecv,
                               annotationtext = annotationtext)

    RPostgreSQL::dbGetQuery(db, sql)
    message(paste(annotationtext, "has been entered into the annotations table."))

  }

}
