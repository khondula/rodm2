test_that("db_annotate adds site group", {
  db <- rodm2::create_sqlite(connect = TRUE)
  myannotationtext = "test site group"
  mysitecode = "testsite"

  suppressMessages(db_describe_annotation(db, "Site group", myannotationtext))
  suppressMessages(db_describe_site(db, site_code = mysitecode))

  suppressMessages(db_annotate(db,
              object = mysitecode,
              annotationtext = myannotationtext))

  bridge_query <- RSQLite::dbGetQuery(db,
                                  "SELECT sf.SamplingFeaturecode, ann.annotationtext
                                  FROM SamplingFeatureAnnotations sfa
                                  left join samplingfeatures sf ON sf.samplingfeatureid = sfa.samplingfeatureid
                                  left join annotations ann ON ann.annotationid = sfa.annotationid
                                  WHERE sf.SamplingFeatureCode = :x
                                  AND ann.annotationtext = :y",
                                  params=list(x=mysitecode, y = myannotationtext))

  expect_true(all(myannotationtext %in% bridge_query[["AnnotationText"]],
                  mysitecode %in% bridge_query[["SamplingFeatureCode"]]))
})
