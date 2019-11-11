test_that("describe annotation works", {
  db <- rodm2::create_sqlite(connect = TRUE)
  myannotationtext = "test site group"
  suppressMessages(db_describe_annotation(db, "Site group", myannotationtext))
  query_result <- RSQLite::dbGetQuery(db,
                                      "SELECT ann.annotationtext
                                      FROM annotations ann
                                      WHERE ann.annotationtext = :x",
                                      params=list(x = myannotationtext))

  expect_true(myannotationtext %in% query_result[["AnnotationText"]])
})
