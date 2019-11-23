# Controlled vocab checkers

#' Menu functinon to check that sampled medium term is in controlled vocabulary
#'
#' @param sampledmedium term to check
#' @param db database connection
#'
#' @return sampled medium
#'
#' @examples
#' # medium <- check_medium_cv(db, "air)
check_medium_cv <- function(db, sampledmedium){
  all_mediums <- RSQLite::dbGetQuery(db, "SELECT term from cv_medium")[["Term"]]
  if(!sampledmedium %in% all_mediums){
    selection_id <- suppressMessages(utils::menu(choices = all_mediums,
                                          graphics = FALSE,
                title = paste("Please select sampled medium from CV")))
    sampledmedium <- all_mediums[selection_id]
  }
  return(sampledmedium)
}

#' Menu function to check that method type is in controlled vocabulary
#'
#' @param methodtypecv method type to check
#' @param db database connection

#' @return method type
#'
#' @examples
#' # methodcode <- check_methodtype_cv()
check_methodtype_cv <- function(db, methodtypecv){
  all_methodtypes <- RSQLite::dbGetQuery(db,
          "SELECT term from cv_methodtype")[["Term"]]
  while(!methodtypecv %in% all_methodtypes){
    methodtypecv_id <- suppressMessages(menu(choices = all_methodtypes,
                                             graphics = FALSE,
                      title = paste("Please select method type from CV")))
    methodtypecv <- all_methodtypes[methodtypecv_id]
  }
  return(methodtypecv)
}
