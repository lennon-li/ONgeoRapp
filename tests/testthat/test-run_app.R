test_that("run_app is exported and the shiny app directory exists", {
  expect_true(is.function(ONgeoRapp::run_app))
  app_dir <- system.file("shiny", package = "ONgeoRapp")
  expect_true(nzchar(app_dir))
  expect_true(file.exists(file.path(app_dir, "app.R")))
})

test_that("inst/shiny/app.R parses without syntax errors", {
  app_path <- system.file("shiny", "app.R", package = "ONgeoRapp")
  expect_error(parse(app_path), NA)
})
