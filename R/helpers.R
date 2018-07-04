#' Search for site names with a pattern
#'
#' Uses agrep to search the samplingfeaturecode column of the samplingfeature table
#'
#' @param method2order method to order colors (\code{"hsv"} or \code{"cluster"})
#' @param ... extra parameters to pass to agrep
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

get_site_names_like <- function(x, ...){
  agrep(pattern = x,
        samplingfeatures$samplingfeaturecode,
        ignore.case = TRUE, value = TRUE, ...)
}

#' Compare sampling feature names to existing ones
#'
#' Returns a data frame specifying whether each element of new_codes exists in samplingfeatures
#'
#' @param new_codes vector of names to compare with samplingfeaturecode vector
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

check_samplingfeaturecodes <- function(new_codes){
  if(!exists("samplingfeatures")){stop("samplingfeatures data frame missing")}

  data.frame(new_codes = unique(new_codes),
             in_db = unique(new_codes) %in%
               samplingfeatures$samplingfeaturecode) %>%
    dplyr::filter(!in_db) %>% dplyr::arrange(new_codes)
}
