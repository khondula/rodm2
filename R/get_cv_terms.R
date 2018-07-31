#' Print a list of controlled vocabulary terms
#'
#' @param cvtype the name of the controlled vocab eg. "resulttype" or "methodtype"
#'
#' @return a vector of the Names column from the controlled vocab table
#' @export
#'
#' @examples
#' get_cv_terms("resulttype")
#' resultterms <- get_cv_terms("resulttype")
get_cv_terms <- function(cvtype){
  tmp <- tempdir()
  db <- rodm2::create_sqlite(dir = tmp, connect = TRUE)
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
  writeLines(cv_message)
  file.remove(file.path(tmp, "odm2.sqlite"))
  invisible(cv[[1]])
}
