# for adding isotope sample results

db_insert_sample_results <- function(x){

sql_blanks <- 'WITH newact AS (
  INSERT into odm2.actions (
    actiontypecv,
    methodid,
    begindatetime,
    begindatetimeutcoffset)
  VALUES (
    \'%s\',
    (SELECT methodid FROM odm2.methods WHERE methodcode = \'%s\'),
    \'%s\',
    \'%s\')
  RETURNING actionid),

  newfa AS (
  INSERT into odm2.featureactions (
    samplingfeatureid,
    actionid)
  VALUES (
    (SELECT samplingfeatureid FROM odm2.samplingfeatures WHERE samplingfeaturecode = \'%s\'),
    (SELECT newact.actionid FROM newact))
  RETURNING featureactionid),

  newresult AS (
  INSERT INTO odm2.results (
    featureactionid,
    resultuuid,
    resulttypecv,
    variableid,
    unitsid,
    processinglevelid,
    sampledmediumcv,
    valuecount)
  VALUES (
    (SELECT newfa.featureactionid FROM newfa),
    \'%s\',
    \'%s\',
    (SELECT variableid FROM odm2.variables WHERE variablecode = \'%s\'),
    (SELECT unitsid FROM odm2.units WHERE unitsabbreviation = \'%s\'),
    \'%s\',
    \'%s\',
    \'%s\'),
  (
    (SELECT newfa.featureactionid FROM newfa),
    \'%s\',
    \'%s\',
    (SELECT variableid FROM odm2.variables WHERE variablecode = \'%s\'),
    (SELECT unitsid FROM odm2.units WHERE unitsabbreviation = \'%s\'),
    \'%s\',
    \'%s\',
    \'%s\')
  RETURNING resultid, variableid),

  newmr1 AS (
  INSERT INTO odm2.measurementresults (
    resultid,
    censorcodecv,
    qualitycodecv,
    aggregationstatisticcv,
    timeaggregationinterval,
    timeaggregationintervalunitsid)
  VALUES (
    (SELECT newresult.resultid FROM newresult WHERE variableid = (SELECT variableid FROM odm2.variables WHERE variablecode = \'%s\')),
    \'%s\',
    \'%s\',
    \'%s\',
    \'%s\',
    \'%s\')),

  newmrv1 AS (
  INSERT INTO odm2.measurementresultvalues (
    resultid,
    datavalue,
    valuedatetime,
    valuedatetimeutcoffset)
  VALUES (
    (SELECT newresult.resultid FROM newresult WHERE variableid = (SELECT variableid FROM odm2.variables WHERE variablecode = \'%s\')),
    \'%s\',
    \'%s\',
    \'%s\')),

  newmr2 AS (
  INSERT INTO odm2.measurementresults (
    resultid,
    censorcodecv,
    qualitycodecv,
    aggregationstatisticcv,
    timeaggregationinterval,
    timeaggregationintervalunitsid)
  VALUES (
    (SELECT newresult.resultid FROM newresult WHERE variableid = (SELECT variableid FROM odm2.variables WHERE variablecode = \'%s\')),
    \'%s\',
    \'%s\',
    \'%s\',
    \'%s\',
    \'%s\'))

  INSERT INTO odm2.measurementresultvalues (
    resultid,
    datavalue,
    valuedatetime,
    valuedatetimeutcoffset)
  VALUES (
    (SELECT newresult.resultid FROM newresult WHERE variableid = (SELECT variableid FROM odm2.variables WHERE variablecode = \'%s\')),
    \'%s\',
    \'%s\',
    \'%s\')
  RETURNING resultid
'

  sql <- sprintf(sql_blanks,
               actiontypecv, methodcode,
               new_sample_results$analysis_datetime[x], utcoffset,
               new_sample_results$sample_code[x],
               # results
               uuid::UUIDgenerate(), resulttypecv,
               variablecode1, # d18O
               unitsabbreviation,
               processlinglevelid, sampledmediumcv, valuecount,
               uuid::UUIDgenerate(), resulttypecv,
               variablecode2, # dD
               unitsabbreviation,
               processlinglevelid, sampledmediumcv, valuecount,
               # Measurement results - d180
               variablecode1, censorcodecv, qualitycodecv,
               aggregationstatisticcv, timeaggregationinterval, timeaggregationintervalunitsid,
               variablecode1,
               new_sample_results$`d 18O`[x],
               new_sample_results$collection_datetime[x], utcoffset,
               # Measurement results - dD
               variablecode2, censorcodecv, qualitycodecv,
               aggregationstatisticcv, timeaggregationinterval, timeaggregationintervalunitsid,
               variablecode2,
               new_sample_results$`d D`[x],
               new_sample_results$collection_datetime[x], utcoffset)

  sql <- gsub("\n", "", sql)

  RPostregreSQL::dbGetQuery(db, sql)

}
