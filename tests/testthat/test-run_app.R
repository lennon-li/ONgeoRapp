test_that("the standalone app entry point exists", {
  expect_true(file.exists(testthat::test_path("..", "..", "app.R")))
})

test_that("app.R parses without syntax errors", {
  app_path <- testthat::test_path("..", "..", "app.R")
  expect_error(parse(app_path), NA)
})
