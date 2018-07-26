devtools::install_github("khondula/rodm2")

# need to have database and data frame
db <- rodm2::create_sqlite(connect = TRUE)
tsrv <- readr::read_csv("qb_wind_trim.csv")
head(tsrv)
names(tsrv) <- c("Timestamp", "Wind direction", "Wind speed", "Wind gust speed", "tempdegreeC", "site")
get_cv_terms("methodtype")
datavalues = tsrv
method = "ATMOS22"
site_code = "QB"
variables = list("Wind speed" = "Meter per Second",
                 "Wind gust speed" = "Meter per Second",
                 "Wind direction" = "Degree")
processinglevel = "Raw data"
sampledmedium = "Air"

aggregationstatisticcv = NULL
zlocation = NULL
zlocationunits = "Meter"
# dbGetQuery(db, "insert into processinglevels (processinglevelcode) VALUES ('Raw data')")
# units = c("meters per second", "meters per second", "degrees")
# actionby = "Kelly" # also need last name and email
# PersonLastName = "Hondula"
# PrimaryEmail = "khondula@sesync.org"
# equipment = NULL

# check for method and add if not in there
if(!(method %in% rodm2::db_get_methods(db))){
  rodm2::db_describe_method(db, methodname = method, methodcode = method, 
                            methodtypecv = "Instrument deployment")
}

sql1 <- 'INSERT into actions
(actiontypecv, methodid, begindatetime, begindatetimeutcoffset, enddatetime)
VALUES 
("Instrument deployment",
(SELECT methodid from methods WHERE methodcode = :method),
DATETIME(:begindatetime), :begindatetimeutcoffset, DATETIME(:enddatetime))'

sql1 <- RSQLite::dbSendQuery(db, sql1)
RSQLite::dbBind(sql1, params = list(method = method,
                                    begindatetime = format(datavalues[["Timestamp"]][1],
                                                           "%Y-%m-%d %H:%M:%S"),
                                    begindatetimeutcoffset = as.integer(format(datavalues[["Timestamp"]][1], 
                                                                               "%z")),
                                    enddatetime = format(datavalues[["Timestamp"]][nrow(datavalues)], 
                                                         "%Y-%m-%d %H:%M:%S")))
RSQLite::dbClearResult(res = sql1)
newactionid <- as.integer(dbGetQuery(db, "SELECT LAST_INSERT_ROWID()"))

if(!is.null(actionby)){
  if(!(actionby %in% rodm2::db_get_people(db))){
    rodm2::db_describe_person(db, PersonFirstName = actionby, 
                              PersonLastName = PersonLastName, PrimaryEmail = PrimaryEmail)
  }
  sql2 <- RSQLite::dbSendStatement(db, 'INSERT into actionby (actionid, affiliationid, isactionlead)
                                   VALUES
                                   (:newactionid, 
(SELECT affiliationid FROM affiliations 
WHERE personid = (SELECT personid FROM people WHERE personfirstname = :actionby)),
"TRUE")')
  RSQLite::dbBind(sql2, params = list(newactionid = newactionid,
                                      actionby = actionby))
}

if(!is.null(equipment)){
 # db get equipment function
  # describe equipment
}

# make sure site is in sampling features table
if(!site_code %in% 
   DBI::dbGetQuery(db, "SELECT samplingfeaturecode 
                   FROM samplingfeatures 
                   WHERE samplingfeaturetypecv = 'Site'")){
  rodm2::db_describe_site(db, site_code)
}
# insert new feature action
sql3 <- RSQLite::dbSendStatement(db, 'INSERT into featureactions
                                 (actionid, samplingfeatureid)
                                 VALUES
                                 (:newactionid,
                                 (SELECT samplingfeatureid 
                                 FROM samplingfeatures 
                                 WHERE samplingfeaturecode = :site_name))')
RSQLite::dbBind(sql3, params = list(newactionid = newactionid,
                                    site_name = site_name))
RSQLite::dbClearResult(res = sql2)
newfaid <- as.integer(dbGetQuery(db, "SELECT LAST_INSERT_ROWID()"))

# check that all variables are in variables table
vars_to_add <- setdiff(names(variables), db_get_variables(db))
for(newvar in vars_to_add){
  db_describe_variable(db, "Unknown", newvar, newvar)
}

# add result! new result for each
newresultids <- c()
# newresultids <- vector(mode = "integer", length = length(variables))
for(i in names(variables)){
  sql4 <- RSQLite::dbSendStatement(db, 'INSERT into results
                                   (resultuuid, featureactionid, resulttypecv,
                                   variableid, unitsid, processinglevelid,
                                   sampledmediumcv, valuecount)
                                   VALUES
                                   (:uuid, :newfaid, :resulttypecv,
                                   (SELECT variableid FROM variables WHERE variablecode = :variablecode),
                                   (SELECT unitsid FROM units WHERE unitsname = :units),
                                   (SELECT processinglevelid FROM processinglevels WHERE processinglevelcode = :processinglevel),
                                   :sampledmedium, :valuecount)')
  RSQLite::dbBind(sql4, params = list(uuid = uuid::UUIDgenerate(),
                                      newfaid = newfaid,
                                      resulttypecv = 'Time series coverage',
                                      variablecode = i,
                                      units = variables[[i]],
                                      processinglevel = processinglevel,
                                      sampledmedium = sampledmedium,
                                      valuecount = nrow(datavalues)
                                      ))
  RSQLite::dbClearResult(res = sql4)
  newresultids <- append(newresultids, as.integer(dbGetQuery(db, "SELECT LAST_INSERT_ROWID()")))
}
names(newresultids) <- names(variables)

# for each of the new results, insert into time series results
for(i in unname(newresultids)){
  sql5 <- RSQLite::dbSendStatement(db, 'INSERT into timeseriesresults 
(resultid, aggregationstatisticcv, zlocation, zlocationunitsid)
  VALUES (:resultid, :aggregationstatisticcv, :zlocation, 
                                   (SELECT unitsid FROM units WHERE unitsname = :zlocationunits))')
  RSQLite::dbBind(sql5, params = list(resultid = i,
                                      aggregationstatisticcv = ifelse(is.null(aggregationstatisticcv), 
                                                                      "Unknown", aggregationstatisticcv),
                                      zlocation = ifelse(is.null(zlocation), "", zlocation),
                                      zlocationunits = ifelse(is.null(zlocationunits), "", zlocationunits)))
  RSQLite::dbClearResult(res = sql5)
}

# then insert into timeseriesresultvalues finalllyyyy

for(i in names(newresultids)){
  # subset data values 
  datavalues_var <- datavalues[, c("Timestamp", i)]
  # make data frame to append
  names(datavalues_var) <- c("valuedatetime", "datavalue")
  datavalues_var$valuedatetimeutcoffset = as.integer(format(datavalues_var$valuedatetime, "%z"))
  datavalues_var$valuedatetime <- format(datavalues_var$valuedatetime, "%Y-%m-%d %H:%M:%S")
  datavalues_var$resultid <- as.integer(newresultids[[i]])
  datavalues_var$censorcodecv = "Unknown"
  datavalues_var$qualitycodecv = "Unknown" # allow for column to be specified...
  datavalues_var$timeaggregationinterval = 5
  timeaggunitsid <- dbGetQuery(db, "select unitsid from units where unitsname = 'Minute'")
  datavalues_var$timeaggregationintervalunitsid = as.integer(timeaggunitsid)
  # append
  RSQLite::dbAppendTable(db, "timeseriesresultvalues", datavalues_var)
}
