# function to query results of a given variable type from a site

#' Title
#'
#' @param db
#' @param site_code
#' @param variable_code
#' @param result_type
#'
#' @return
#' @export
#'
#' @examples
db_get_results <- function(db,
                           site_code = NULL,
                           variable_code = NULL,
                           result_type = c("sample", "ts", "measurement")){

    # Check if db is compatable
  if (!class(db) %in% c("SQLiteConnection")) {
    stop("sorry, only sqlite database connections are supported so far")}

  if(result_type == "sample"){
    stop("to be implemented after lunch")

  }
  if(result_type == "measurement"){
    stop("to be implemented after lunch")

  }

  # if site code is provided, check to make sure it is in the database
  if (!is.null(site_code) & any(!site_code %in% rodm2::db_get_sites(db))) {
    stop("provided site_code not in database. Check site codes with db_get_sites")
  }

  # if site code is not provided, assume data for all sites
  if(is.null(site_code)) {
    site_code <- rodm2::db_get_sites(db)
  }

  # if variable code is provided, check to make sure it is in the database
  if (!is.null(variable_code) & any(!variable_code %in% rodm2::db_get_variables(db)[[1]])) {
    stop("provided variable_code not in database. Check variable names with db_get_variables")
  }

  # if site code is not provided, assume data for all sites
  if(is.null(variable_code)) {
    variable_code <- RSQLite::dbGetQuery(db, "SELECT variablecode from variables")
    variable_code <- unique(variable_code[[1]][!is.null(variable_code)])
  }

  # Retreive Sampling Feature ID for site code
  sf_id <- RSQLite::dbGetQuery(db,"SELECT SamplingFeatureID
                               FROM SamplingFeatures
                               WHERE SamplingFeatureCode IN (:x)",
                               params=list(x=site_code))

  sf_id_integer <- as.integer(sf_id[["SamplingFeatureID"]])
  sf_id_integer <- as.character(sf_id_integer)
  # sf_id_integer <- paste0(sf_id_integer, collapse = ",")

  #Retreive Feature Action ID[s] for site code
  FeatureActionID <- RSQLite::dbGetQuery(db,
                                         "SELECT FeatureActionID
                                         FROM FeatureActions
                                         WHERE SamplingFeatureID IN (:x)",
                                         params=list(x=sf_id_integer))

  fa_id_integer <- as.integer(FeatureActionID[["FeatureActionID"]])
  fa_id_integer <- as.character(fa_id_integer)
  # fa_id_integer <- sprintf('%s', fa_id_integer)
  # fa_id_integer <- paste0(sQuote(fa_id_integer), collapse = ",")

  # Retreive variable id for variables
  var_id <- RSQLite::dbGetQuery(db,"SELECT variableid
                               FROM variables
                               WHERE variablecode IN (:x)",
                                params=list(x=variable_code))

  var_id_integer <- as.integer(var_id[["VariableID"]])
  var_id_integer <- as.character(var_id_integer)
  # var_id_integer <- sprintf('%s', var_id_integer)
  # var_id_integer <- paste0(sQuote(var_id_integer), collapse = ",")

  #Retreive Result ID[s] for each feature action

  resultids_fa <- RSQLite::dbGetQuery(db,
                                      "SELECT ResultID FROM Results
                      WHERE FeatureActionID IN (:x)",
                                      params = list(x = fa_id_integer))
  resultids_var <- RSQLite::dbGetQuery(db,
                                       "SELECT ResultID FROM Results
                      WHERE VariableID IN (:x)",
                                       params = list(x = var_id_integer))


  result_id_integer <- intersect(resultids_fa[[1]], resultids_var[[1]])

  #Retreive Result values
  if(result_type == "ts"){

    results_data <- RSQLite::dbGetQuery(db,
                                        "SELECT tsrv.ValueDateTime, tsrv.DataValue, units.unitsname,
var.variablenamecv, sf.samplingfeaturecode, pl.ProcessingLevelCode
                                        FROM TimeSeriesResultValues tsrv
LEFT JOIN results res ON res.resultid = tsrv.resultid
LEFT JOIN featureactions fa ON fa.featureactionid = res.featureactionid
LEFT JOIN samplingfeatures sf ON sf.samplingfeatureid = fa.samplingfeatureid
LEFT JOIN variables var ON var.variableid = res.variableid
LEFT JOIN units ON units.unitsid = res.unitsid
LEFT JOIN processinglevels pl ON pl.processinglevelid = res.processinglevelid
                                        WHERE tsrv.ResultID IN (:x)",
                                        params=list(x=result_id_integer))
  }

  return(results_data)
}
