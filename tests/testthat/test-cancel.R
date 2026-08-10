# Cancel used to bump the generation counter and nothing else: the future ran
# to completion in a worker, and because ExtendedTask queues invocations the
# next Preview waited behind the abandoned run. These tests cover the
# cooperative-cancellation sentinel that replaced that (see new_cancel_path()).

test_that("request_cancel creates the sentinel and tolerates no path", {
  env <- load_shiny_app_env()
  path <- withr::local_tempfile(fileext = ".txt")

  expect_false(file.exists(path))
  env$request_cancel(path)
  expect_true(file.exists(path))

  # A run that never got a path (or a stale NULL reactiveVal) must not error.
  expect_silent(env$request_cancel(NULL))
  expect_silent(env$request_cancel(character(0)))
})

test_that("new_cancel_path hands out distinct paths", {
  env <- load_shiny_app_env()
  paths <- replicate(3, env$new_cancel_path())
  expect_length(unique(paths), 3L)
  expect_false(any(file.exists(paths)))
})

test_that("Cancel writes the sentinel for whichever task is running", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  testthat::local_mocked_bindings(
    list_sources = shiny_fixture_registry,
    get_source = shiny_fixture_metadata,
    .package = "ONgeoR"
  )
  server <- load_shiny_server()

  shiny::testServer(server, {
    preview_path <- withr::local_tempfile(fileext = ".txt")
    preview_cancel_path(preview_path)
    preview_state("running")
    session$setInputs(cancel_task_btn = 1)

    expect_true(file.exists(preview_path))
    expect_equal(preview_state(), "cancelled")
    # The user must be TOLD the wait exists. Cancellation is cooperative, so
    # "Cancelled." on its own is a lie while a download is still finishing.
    expect_match(preview_state_detail(), "^Cancelling\\.")
    expect_match(preview_state_detail(), "cannot be interrupted")
    expect_match(preview_state_detail(), "close and restart the app")

    link_path <- withr::local_tempfile(fileext = ".txt")
    link_cancel_path(link_path)
    link_state("running")
    session$setInputs(cancel_task_btn = 2)

    expect_true(file.exists(link_path))
    expect_equal(link_state(), "cancelled")
    expect_match(link_state_detail(), "^Cancelling\\.")
    expect_match(link_state_detail(), "close and restart the app")
  })
})

# The load-bearing test: prove the WORKER actually stops, rather than trusting
# that writing a file somewhere has an effect. new_cancel_path() is stubbed to a
# known path, and the first retrieval creates it - standing in for a user who
# hits Cancel while the first layer is downloading. The second retrieval must
# then never happen.
test_that("a cancelled preview aborts before the next retrieval", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  env <- load_shiny_app_env()
  cancel_file <- withr::local_tempfile(fileext = ".txt")
  env$new_cancel_path <- function() cancel_file

  layers <- shiny_fixture_layers()
  seen <- new.env(parent = emptyenv())
  seen$n <- 0L

  testthat::local_mocked_bindings(
    list_sources = shiny_fixture_registry,
    get_source = shiny_fixture_metadata,
    retrieve_source = function(source_id, refresh = FALSE, ...) {
      seen$n <- seen$n + 1L
      if (seen$n == 1L) {
        file.create(cancel_file)
      }
      layers[[source_id]]
    },
    .package = "ONgeoR"
  )
  use_sequential_futures()

  shiny::testServer(env$server, {
    session$setInputs(
      base_layer = "base_polygon",
      overlay_source = "overlay_point",
      preview_btn = 1
    )
    # shiny warns "An error occurred when invoking the ExtendedTask" whenever a
    # task throws. That is precisely what cancellation does here, so the warning
    # is the expected signal, not noise - suppressed so the suite stays clean
    # rather than carrying a warning that looks like a real problem.
    expect_identical(
      suppressWarnings(wait_for_extended_task(preview_task, session)),
      "error"
    )

    # Exactly one retrieval: the checkpoint after it saw the sentinel and threw
    # before the target layer was fetched.
    expect_equal(seen$n, 1L)
    # And the run is reported as cancelled, not failed.
    expect_equal(preview_state(), "cancelled")
    expect_null(cw_result$base_sf)
  })
})

test_that("an uncancelled preview still retrieves both layers", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  layers <- shiny_fixture_layers()
  seen <- new.env(parent = emptyenv())
  seen$n <- 0L

  testthat::local_mocked_bindings(
    list_sources = shiny_fixture_registry,
    get_source = shiny_fixture_metadata,
    retrieve_source = function(source_id, refresh = FALSE, ...) {
      seen$n <- seen$n + 1L
      layers[[source_id]]
    },
    .package = "ONgeoR"
  )
  server <- load_shiny_server()
  use_sequential_futures()

  shiny::testServer(server, {
    session$setInputs(
      base_layer = "base_polygon",
      overlay_source = "overlay_point",
      preview_btn = 1
    )
    expect_identical(wait_for_extended_task(preview_task, session), "success")
    expect_equal(seen$n, 2L)
    expect_equal(preview_state(), "completed")
  })
})
