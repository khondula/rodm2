# new site and method and variable handling

handle_new_method <- function(methodcode, methodtypecv){

  all_methods <- RSQLite::dbGetQuery(db, "SELECT methodcode from methods")[[1]]
  if(!methodcode %in% all_methods){
  selection_id <- suppressMessages(menu(choices = c(all_methods, paste("Add", methodcode,"as new method")),
                                        graphics = FALSE,
                                        title = paste("Method code not in database. Select option below or type 0 to quit: ")))
  if (selection_id == 0) {
    stop("See existing method codes using db_get_methods()")
  } else if (selection_id == length(all_methods)+1) {
    rodm2::db_describe_method(db,
                              methodcode = methodcode,
                              methodname = readline("Supply new method name: "),
                              methodtypecv = methodtypecv)
  } else {
    methodcode <- all_methods[selection_id]
  }

  }
  return(methodcode)
}

handle_new_site <- function(site_code){
  all_site <- RSQLite::dbGetQuery(db, "SELECT samplingfeaturecode from samplingfeatures")[[1]]
  if(!site_code %in% all_site){
    selection_id <- suppressMessages(menu(choices = c(all_site, paste("Add", site_code,"as new site")),
                                          graphics = FALSE,
                                          title = paste("Site code not in database. Select option below or type 0 to quit: ")))
    if (selection_id == 0) {
      stop("See existing method codes using db_get_sites()")
    } else if (selection_id == length(all_site)+1) {
      if(!exists("sitetypecv")){
        all_sftypes <- RSQLite::dbGetQuery(db, "SELECT term from cv_samplingfeaturetype")[["Term"]]
        selection_id <- suppressMessages(menu(choices = all_sftypes,
                                              graphics = FALSE,
                                              title = paste("Please select site type for", site_code, "from CV")))
        sitetypecv <- all_sftypes[selection_id]
      }
      rodm2::db_describe_site(db,
                              site_code = site_code,
                              site_name = readline(paste("Supply site name for new site code", site_code,": ")),
                              sitetypecv = sitetypecv)
    } else {
      site_code <- all_site[selection_id]
    }

  }
  return(site_code)
}
