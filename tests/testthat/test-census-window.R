# Census layers are the only sources that accept a bbox, and nothing in the app
# used to pass one: Aggregate Dissemination Area pulled all 1,679 features
# province-wide and Dissemination Area all 20,465. The window is an explicit,
# visible control - silent windowing would hand back a partial province that
# looks like a complete answer.

test_that("map_view_bbox converts leaflet bounds and rejects unusable ones", {
  env <- load_shiny_app_env()

  expect_equal(
    env$map_view_bbox(list(west = -76, south = 45.2, east = -75.4, north = 45.6)),
    c(-76, 45.2, -75.4, 45.6)
  )
  # Extra members are fine; leaflet sends what it sends.
  expect_equal(
    env$map_view_bbox(list(north = 45.6, east = -75.4, south = 45.2, west = -76, zoom = 9)),
    c(-76, 45.2, -75.4, 45.6)
  )

  expect_null(env$map_view_bbox(NULL))
  expect_null(env$map_view_bbox(list(west = -76, south = 45.2)))
  expect_null(env$map_view_bbox(list(west = -76, south = 45.2, east = -76, north = 45.6)))
  expect_null(env$map_view_bbox(list(west = -75, south = 45.2, east = -76, north = 45.6)))
  expect_null(env$map_view_bbox(list(west = NA, south = 45.2, east = -75.4, north = 45.6)))
})

test_that("only census sources are windowable, and big ones default to windowed", {
  registry <- tibble::tibble(
    source_id = c("census_da_2021", "census_pr_2021", "phu_boundaries"),
    name = c("DA", "PR", "PHU"),
    geography_type = c("boundary", "boundary", "boundary"),
    feature_count = c(20465L, 1L, 29L)
  )
  testthat::local_mocked_bindings(list_sources = function() registry, .package = "ONgeoR")
  env <- load_shiny_app_env()

  expect_true(env$is_census_source("census_da_2021"))
  expect_false(env$is_census_source("phu_boundaries"))
  expect_false(env$is_census_source(NULL))

  # 20,465 features: windowed by default. 1 feature: not worth windowing.
  expect_true(env$census_window_default("census_da_2021"))
  expect_false(env$census_window_default("census_pr_2021"))
  # Never for a non-census layer, whatever its size.
  expect_false(env$census_window_default("phu_boundaries"))
})

test_that("the bbox reaches retrieve_census only when the window is on", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  layers <- shiny_fixture_layers()
  registry <- tibble::tibble(
    source_id = c("census_da_2021", "overlay_point"),
    name = c("DA", "Overlay point"),
    geography_type = c("boundary", "facility"),
    feature_count = c(20465L, 10L)
  )
  seen <- new.env(parent = emptyenv())
  seen$census_bbox <- "never called"
  seen$plain <- character()

  testthat::local_mocked_bindings(
    list_sources = function() registry,
    get_source = function(source_id) shiny_fixture_metadata(
      if (source_id == "census_da_2021") "base_polygon" else "overlay_point"
    ),
    retrieve_source = function(source_id, refresh = FALSE, ...) {
      seen$plain <- c(seen$plain, source_id)
      layers[[if (source_id == "census_da_2021") "base_polygon" else "overlay_point"]]
    },
    retrieve_census = function(source_id, bbox = NULL, ...) {
      seen$census_bbox <- bbox
      layers[["base_polygon"]]
    },
    .package = "ONgeoR"
  )
  server <- load_shiny_server()
  use_sequential_futures()

  shiny::testServer(server, {
    session$setInputs(
      base_layer = "census_da_2021",
      overlay_source = "overlay_point",
      cw_map_bounds = list(west = -76, south = 45.2, east = -75.4, north = 45.6)
    )

    # Window ON: retrieve_census receives the visible extent, and the phase log
    # says so rather than pretending a province-wide pull happened.
    session$setInputs(limit_to_view = TRUE)
    session$setInputs(preview_btn = 1)
    expect_identical(wait_for_extended_task(preview_task, session), "success")
    expect_equal(seen$census_bbox, c(-76, 45.2, -75.4, 45.6))
    expect_true(any(grepl("current map view only", preview_progress_log(), fixed = TRUE)))

    # Window OFF: the plain province-wide path, no bbox anywhere.
    seen$census_bbox <- "never called"
    session$setInputs(limit_to_view = FALSE)
    session$setInputs(preview_btn = 2)
    expect_identical(wait_for_extended_task(preview_task, session), "success")
    expect_identical(seen$census_bbox, "never called")
    expect_true("census_da_2021" %in% seen$plain)
    expect_false(any(grepl("current map view only", preview_progress_log(), fixed = TRUE)))
  })
})
