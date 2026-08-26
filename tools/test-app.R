app_root <- normalizePath(".", mustWork = TRUE)
Sys.setenv(ONgeoRAPP_ROOT = app_root)
options(ONgeoRapp.app_root = app_root)
testthat::test_dir("tests/testthat", reporter = "progress")
