test_that("connect sqlite works", {
  expect_true(RSQLite::dbIsValid(suppressMessages(connect_sqlite())))
})

test_that("create sqlite works", {
  expect_true(RSQLite::dbIsValid(create_sqlite(connect = TRUE)))

  # expect_error(create_sqlite())
})
