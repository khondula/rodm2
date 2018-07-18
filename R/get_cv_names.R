#' Print a list of controlled vocabulary terms
#'
#' @param cvtype the name of the controlled vocab eg. "resulttype" or "methodtype"
#'
#' @return a vector of the Names column from the CV_%cvtype% table
#' @export
#'
#' @examples
#' get_cv_names("resulttype")
#' resultterms <- get_cv_names("resulttype")
get_cv_names <- function(cvtype){
  tmp <- tempdir()
  if(!exists("db")){
    rodm2::create_sqlite(dir = tmp)
    db <- DBI::dbConnect(RSQLite::SQLite(), file.path(tmp, "odm2.sqlite"))
  }
  sql <- DBI::sqlInterpolate(db, paste0("SELECT Name from cv_", cvtype))
  cv <- RSQLite::dbGetQuery(db, sql)
  cv_paste1 <- noquote(paste(cv))
  cv_paste <- gsub(cv_paste1, pattern = "\n", replacement = "", fixed = TRUE)
  cv_sub <- substr(cv_paste, start = 3, stop = nchar(cv)-2)
  cv_message <- paste(crayon::cyan(cvtype, "controlled vocabulary terms:"), cv_sub)
  writeLines(cv_message)
  invisible(cv)
}
