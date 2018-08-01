#' Search for site names with a pattern
#'
#' Uses agrep to search the samplingfeaturecode column of the samplingfeature table
#'
#' @param ... extra parameters to pass to agrep
#' @param x
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
