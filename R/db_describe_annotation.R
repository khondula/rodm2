#' Add a new annotation text
#'
#' @param db database connection object
#' @param annotationtext text string of annotation.
#' @param annotationtypecv annotation type from \href{http://vocabulary.odm2.org/annotationtype/}{controlled vocab}
#' @param annotationcode Optional short codename for annotation
#'
#' @return message if successful
#' @export
#' @details
#' Use this function to add new annotations such as the name of a Site group.
#' Annotation text is required but if an annotation code is provided then that
#' will be used as the annotation text as well.
#'
#' @family describe functions
#' @examples
#' db <- create_sqlite(connect = TRUE)
#'
#' db_describe_annotation(db,
#' annotationtext = "Riparian wells",
#' annotationtypecv = "Site group")
#'
#' db_describe_annotation(db,
#' annotationtext = "January sampling campaign",
#' annotationtypecv = "Specimen group",
#' annotationcode = "Jan")
#'
db_describe_annotation <- function(db,
                                  annotationtext = NULL,
                                  annotationtypecv = NULL,
                                  annotationcode = NULL){

  if (!class(db) %in% c("SQLiteConnection", "PostgreSQLConnection")) {
    stop("sorry, only sqlite and postgresql database connections are supported so far")}

  if(!any(!is.null(annotationcode), !is.null(annotationtext))){
    stop("Please supply either an annotation code or annotation text")
  }

  # check if annotationtext already exists in database
  notes_in_db <- RSQLite::dbGetQuery(db, sprintf("SELECT annotationtext from annotations
                      where annotationtypecv = \'%s\'", annotationtypecv))[[1]]

  if(annotationtext %in% notes_in_db){
    stop(paste(annotationtypecv, annotationtext, "already in database."))
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

    all_notetypes <- suppressMessages(get_cv_terms("annotationtype", quietly = TRUE))
    if(any(is.null(annotationtypecv), !annotationtypecv %in% all_notetypes)){
      selection_id <- suppressMessages(menu(choices = all_notetypes,
                                            graphics = FALSE,
                                            title = paste("Please select annotation type from CV or type 0 to quit")))
      annotationtypecv <- all_notetypes[selection_id]
      if(selection_id == 0){stop("See existing annotations using db_get_annotations() or add with db_describe_annotation()")}
    }

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
#' db <- create_sqlite(connect = TRUE)
#'
#' db_describe_annotation(db,
#' annotationtext = "Riparian wells",
#' annotationtypecv = "Site group")
#'
#' db_get_annotations(db)
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
