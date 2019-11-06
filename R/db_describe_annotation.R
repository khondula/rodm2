#' Add a new annotation text
#'
#' @param annotationtypecv annotation type from [controlled vocab](http://vocabulary.odm2.org/annotationtype/)
#' @param annotationtext text string of annotation
#' @param db database connection object
#' @param annotationcode Optional short codename for annotation
#'
#' @return TRUE if successful
#' @export
#' @details Use this function to add new annotations such as the name of a Site group.
#'
#' @examples
#' #db <- create_sqlite(dir = tempdir())
#' #db_describe_annotaion(db, "Site group", "Riparian wells")
#' #db_describe_annotaion(db, "Specimen group", "January sampling campaign", annotationcode = "Jan")
#'
db_describe_annotation <- function(db,
                                  annotationtypecv,
                                  annotationtext = NULL,
                                  annotationcode = NULL){

  if (!class(db) %in% c("SQLiteConnection", "PostgreSQLConnection")) {
    stop("sorry, only sqlite and postgresql database connections are supported so far")}

  if(!any(!is.null(annotationcode), !is.null(annotationtext))){
    stop("Please supply either an annotation code or annotation text")
  }

  # annotation text is required, if null use annotation code
  if(is.null(annotationtext)){
    annotationtext <- annotationcode
  }

  # annotation code needs to be not null for sql
  if(is.null(annotationcode)){
    annotationcode <- ""
  }

  # check type of database object
  if (class(db) == "SQLiteConnection"){

    sql <- RSQLite::dbSendStatement(db,
  'INSERT OR IGNORE INTO annotations (annotationtypecv, annotationtext, annotationcode)
                 VALUES (:annotationtypecv, :annotationtext, :annotationcode)')

    RSQLite::dbBind(sql, param = list(annotationtypecv = annotationtypecv,
                                      annotationtext = annotationtext,
                                      annotationcode = annotationcode))
    RSQLite::dbClearResult(res = sql)

    message(paste(annotationtext, "has been entered into the Annotations table.\nUse db_annotate to tag items with annotations."))
  }

  if (class(db) == "PostgreSQLConnection"){

    sql <- DBI::sqlInterpolate(db,
                               'INSERT into odm2.annotations
                               (annotationtypecv, annotationtext)
                               VALUES (?annotationtypecv, ?annotationtext, ?annotationcode)',
                               annotationtypecv = annotationtypecv,
                               annotationtext = annotationtext,
                               annotationcode = annotationcode)

    RPostgreSQL::dbGetQuery(db, sql)
    message(paste(annotationtext, "has been entered into the Annotations table.\nUse db_annotate to tag items with annotations."))

  }

}

#' Get list of annotations currently in database
#'
#' @param db database connection object
#'
#' @return a dataframe with the current values in the annotation code and annotation text columns
#' @export
#'
#' @examples
#' #db_get_annotations(db)
db_get_annotations <- function(db){
  if (!class(db) %in% c("SQLiteConnection", "PostgreSQLConnection")) {
    stop("sorry, only sqlite and postgresql database connections are supported so far")}
  current_notes <- c()
  if (class(db) == "SQLiteConnection"){
    current_notes <- DBI::dbGetQuery(db, "SELECT annotationcode, annotationtext FROM annotations")
  }
  if (class(db) == "PostgreSQLConnection"){
    current_notes <- DBI::dbGetQuery(db, "SELECT annotationcode, annotationtext FROM odm2.annotations")
  }
  return(current_notes)
}
