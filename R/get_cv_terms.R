#' Print a list of controlled vocabulary terms
#'
#' @param cvtype the name of the controlled vocab eg. "resulttype" or "methodtype"
#' @param quietly whether to print in console or not
#'
#' @return a vector of the Names column from the controlled vocab table
#' @details
#' This function returns a character vector of terms from a selected
#' controlled vocabularly and, optionally, prints those names in the R console.
#' Controlled vocabularies are hosted at \href{http://vocabulary.odm2.org/}{
#' http://vocabulary.odm2.org/} as well as pre-populated in the database
#' template in rodm2. This function relies on the CV terms in the
#' pre-populated ODM2 database template.
#'
#' @export
#'
#' @examples
#' get_cv_terms("resulttype")
#' resultterms <- get_cv_terms("resulttype")
get_cv_terms <- function(cvtype = c("units", "variablename", "samplingfeaturetype",
                                    "medium", "methodtype", "actiontype", "annotationtype",
                                    "relationshiptype",
                                    "censorcode", "dataqualitytype", "datasettype",
                                    "directivetype", "elevationdatum", "equipmenttype",
                                    "organizationtype", "aggregationstatistic",
                                    "propertydatatype", "qualitycode",
                                    "resulttype", "samplingfeaturegeotype",
                                    "sitetype", "spatialoffsettype", "speciation", "specimentype",
                                    "status", "taxonomicclassifiertype", "unitstype",
                                    "variabletype"), quietly = FALSE){
  tmp <- tempdir()
  if(file.exists(file.path(tmp, "odm2.sqlite"))){
    db <- rodm2::connect_sqlite()
  } else {
    db <- rodm2::create_sqlite(dir = tmp, connect = TRUE)
  }
  sql <- DBI::sqlInterpolate(db, paste0("SELECT Name from cv_", cvtype))
  if(cvtype == "units"){
    sql <- DBI::sqlInterpolate(db, "SELECT unitsname from units")
  }
  cv <- RSQLite::dbGetQuery(db, sql)
  RSQLite::dbDisconnect(db)
  cv_paste1 <- noquote(paste(cv))
  cv_paste <- gsub(cv_paste1, pattern = "\n", replacement = "", fixed = TRUE)
  cv_sub <- substr(cv_paste, start = 3, stop = nchar(cv)-1)
  cv_message <- paste(crayon::cyan(cvtype, "controlled vocabulary terms:"), cv_sub)
  if(!quietly){writeLines(cv_message)}
  file.remove(file.path(tmp, "odm2.sqlite"))
  message('use quietly = FALSE to print out all controlled vocabulary terms')
  invisible(cv[[1]])
}
