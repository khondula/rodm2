#' Function to retrieve time series data with variablecode waterLevel from a specified sampling feature
#'
#' @param db database connection object
#' @param site_code
#'
#' @return
#' @export
#'
#' @examples
#' db <- rodm2::create_sqlite(connect = TRUE)
#'
#' tsrv <-data.frame(
#'  Timestamp = c("2018-06-27 13:45:00", "2018-06-27 13:55:00"),
#'   "wl" = c(1, 1.5))
#'
#' db_insert_results_ts(
#'   db = db,
#'   datavalues = tsrv,
#'   method = "pressureTransducer",
#'   site_code = "BB2",
#'    variables = list("Water level" = list(column = "wl", units = "Meter")),
#'   sampledmedium = "Liquid Aqueous")
#'
#'  db_get_water_level_ts(db, "BB2")
#'
db_get_water_level_ts <- function(db, site_code){

  #Check if db is compatable
  if (!class(db) %in% c("SQLiteConnection", "PostgreSQLConnection")) {
    stop("sorry, only sqlite and postgresql database connections are supported so far")}

  #SQLite Database
  if(class(db) == "SQLiteConnection"){

    #Retreive Sampling Feature ID for site code
    SamplingFeatureID <- RSQLite::dbGetQuery(db,
                                           "SELECT SamplingFeatureID FROM SamplingFeatures WHERE SamplingFeatureCode = :x",
                                           params=list(x=site_code))

    #Retreive Feature Action ID[s] for site code
    FeatureActionID <- RSQLite::dbGetQuery(db,
                                         "SELECT FeatureActionID FROM FeatureActions WHERE SamplingFeatureID = :x",
                                         params=list(x=SamplingFeatureID[,1]))

    #Retreive Result ID[s] for each feature action
    ResultID <- RSQLite::dbGetQuery(db,
                                  "SELECT ResultID FROM Results WHERE FeatureActionID = :x",
                                  params=list(x=FeatureActionID[,1]))

    #Retreive Result values
    Values <- RSQLite::dbGetQuery(db,
                                "SELECT ValueDateTime, DataValue
                                FROM TimeSeriesResultValues
                                WHERE ResultID = :x",
                                params=list(x=ResultID[,1]))

    #Clean up values df
    Values$ValueDateTime<-as.POSIXct(Values$ValueDateTime)
    colnames(Values)<-c("Timestamp", "waterLevel")

    #Export Values
    Values
  }

  if(class(db) == "PostgreSQLConnection"){

    Values <- RPostgreSQL::dbGetQuery(db,
                                    paste0("SELECT datavalue, valuedatetime, res.resultid, var.variablecode, sf.samplingfeaturecode, ppl.personfirstname
                                           FROM odm2.timeseriesresultvalues AS tsrv
                                           INNER JOIN odm2.results AS res ON res.resultid = tsrv.resultid
                                           INNER JOIN odm2.variables AS var ON var.variableid = res.variableid
                                           INNER JOIN odm2.units AS u ON u.unitsid = res.unitsid
                                           INNER JOIN odm2.featureactions AS fa ON fa.featureactionid = res.featureactionid
                                           INNER JOIN odm2.samplingfeatures AS sf ON sf.samplingfeatureid = fa.samplingfeatureid
                                           INNER JOIN odm2.actions AS act ON act.actionid = fa.actionid
                                           LEFT JOIN odm2.actionby AS ab ON ab.actionid = act.actionid
                                           LEFT JOIN odm2.affiliations AS aff ON aff.affiliationid = ab.affiliationid
                                           LEFT JOIN odm2.people AS ppl ON ppl.personid = aff.personid
                                           WHERE var.variablecode = 'waterLevel' AND sf.samplingfeaturecode = '",
                                           site_code,"'"))

    #Clean up values df
    if(length(Values)!=0){
      Values <- Values[,c("valuedatetime","datavalue")]
      Values$valuedatetime <- as.POSIXct(Values$valuedatetime)
      colnames(Values) <- c("Timestamp", "waterLevel")
    }

    #Export Values
    return(Values)
  }
}
