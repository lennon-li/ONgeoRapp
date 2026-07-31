test_that("source selections update geometry and relationship displays", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  testthat::local_mocked_bindings(
    list_sources = shiny_fixture_registry,
    get_source = shiny_fixture_metadata,
    .package = "ONgeoR"
  )
  server <- load_shiny_server()

  shiny::testServer(server, {
    expect_match(rendered_html(output$link_task_status), "Idle")
    session$setInputs(
      base_layer = "base_polygon",
      overlay_source = "overlay_point"
    )

    expect_match(rendered_html(output$base_geom_badge), "Polygon")
    expect_match(rendered_html(output$overlay_geom_badge), "Point")
    expect_match(
      rendered_html(output$link_relationship),
      "Point-in-boundary containment"
    )
    expect_null(cw_result$crosswalk)
    expect_null(cw_result$previewed)
  })
})

test_that("changing a selection invalidates preview-based Link gating", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  layers <- shiny_fixture_layers()
  testthat::local_mocked_bindings(
    list_sources = shiny_fixture_registry,
    get_source = shiny_fixture_metadata,
    retrieve_source = function(source_id, refresh = FALSE, ...) {
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
    expect_identical(
      wait_for_extended_task(preview_task, session),
      "success"
    )
    expect_match(rendered_html(output$build_btn_ui), "id=\"build_btn\"")
    expect_false(grepl("disabled", rendered_html(output$build_btn_ui)))

    session$setInputs(overlay_source = "other_polygon")

    expect_match(rendered_html(output$build_btn_ui), "disabled")
    expect_null(cw_result$previewed)
  })
})

test_that("Join asks for confirmation before doing any work", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  layers <- shiny_fixture_layers()
  crosswalk_calls <- 0L
  testthat::local_mocked_bindings(
    list_sources = shiny_fixture_registry,
    get_source = shiny_fixture_metadata,
    retrieve_source = function(source_id, refresh = FALSE, ...) {
      layers[[source_id]]
    },
    build_crosswalk = function(from, to, ...) {
      crosswalk_calls <<- crosswalk_calls + 1L
      tibble::tibble(from_id = 1:2, to_id = c("P1", "P2"))
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
    expect_identical(
      wait_for_extended_task(preview_task, session),
      "success"
    )

    # Clicking Join only raises the confirmation; nothing is computed yet.
    session$setInputs(build_btn = 1)
    expect_identical(crosswalk_calls, 0L)
    expect_null(cw_result$crosswalk)

    # Confirming is what runs it.
    session$setInputs(confirm_join_btn = 1)
    expect_identical(wait_for_extended_task(build_task, session), "success")
    expect_identical(crosswalk_calls, 1L)
    expect_s3_class(cw_result$crosswalk, "data.frame")
  })
})

test_that("preview then Link produces a crosswalk and enables downloads", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  layers <- shiny_fixture_layers()
  testthat::local_mocked_bindings(
    list_sources = shiny_fixture_registry,
    get_source = shiny_fixture_metadata,
    retrieve_source = function(source_id, refresh = FALSE, ...) {
      layers[[source_id]]
    },
    build_crosswalk = function(from, to, ...) {
      tibble::tibble(from_id = 1:2, to_id = c("P1", "P2"))
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
    expect_identical(
      wait_for_extended_task(preview_task, session),
      "success"
    )

    session$setInputs(confirm_join_btn = 1)
    expect_identical(wait_for_extended_task(build_task, session), "success")

    expect_s3_class(cw_result$crosswalk, "data.frame")
    expect_gt(nrow(cw_result$crosswalk), 0)
    downloads <- rendered_html(output$link_downloads_ui)
    expect_match(downloads, "id=\"dl_cw_csv\"")
    expect_match(downloads, "id=\"dl_cw_script\"")

    expected_file <- withr::local_tempfile(fileext = ".csv")
    utils::write.csv(cw_result$crosswalk, expected_file, row.names = FALSE)
    actual_file <- output$dl_cw_csv
    expect_equal(
      readBin(actual_file, what = "raw", n = file.info(actual_file)$size),
      readBin(expected_file, what = "raw", n = file.info(expected_file)$size)
    )
  })
})

test_that("Link discards a completion invalidated by changed inputs", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  layers <- shiny_fixture_layers()
  testthat::local_mocked_bindings(
    list_sources = shiny_fixture_registry,
    get_source = shiny_fixture_metadata,
    retrieve_source = function(source_id, refresh = FALSE, ...) {
      Sys.sleep(0.05)
      layers[[source_id]]
    },
    build_crosswalk = function(from, to, ...) {
      tibble::tibble(from_id = 1L, to_id = "P1")
    },
    .package = "ONgeoR"
  )
  server <- load_shiny_server()
  use_sequential_futures()

  shiny::testServer(server, {
    session$setInputs(
      base_layer = "base_polygon",
      overlay_source = "overlay_point",
      confirm_join_btn = 1
    )
    expect_match(rendered_html(output$link_task_status), "Running")

    # Changing a picker mid-run invalidates the in-flight link (there is no
    # match-rule input to change now); the stale result must be discarded.
    # other_polygon keeps the pair on the build_crosswalk (point x polygon) path.
    session$setInputs(base_layer = "other_polygon")
    expect_match(rendered_html(output$link_task_status), "Cancelled")

    expect_identical(wait_for_extended_task(build_task, session), "success")
    expect_null(cw_result$crosswalk)
    expect_null(cw_result$linked)
    expect_match(rendered_html(output$link_downloads_ui), "disabled")
    expect_match(rendered_html(output$link_task_status), "Cancelled")
  })
})

test_that("Link surfaces retrieval failures without retaining results", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  testthat::local_mocked_bindings(
    list_sources = shiny_fixture_registry,
    get_source = shiny_fixture_metadata,
    retrieve_source = function(source_id, refresh = FALSE, ...) {
      rlang::abort("Fixture retrieval failed.")
    },
    .package = "ONgeoR"
  )
  server <- load_shiny_server()
  use_sequential_futures()

  shiny::testServer(server, {
    session$setInputs(
      base_layer = "base_polygon",
      overlay_source = "overlay_point",
      confirm_join_btn = 1
    )
    expect_warning(
      wait_for_extended_task(build_task, session),
      "An error occurred when invoking the ExtendedTask",
      fixed = TRUE
    )
    expect_null(cw_result$crosswalk)
    expect_match(rendered_html(output$link_task_status), "Failed")
    expect_match(
      rendered_html(output$link_task_status),
      "Fixture retrieval failed",
      fixed = TRUE
    )
  })
})

test_that("repeat Link runs use cached retrieval without re-fetching", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  layers <- shiny_fixture_layers()
  retrieval_count <- 0L
  testthat::local_mocked_bindings(
    list_sources = shiny_fixture_registry,
    get_source = shiny_fixture_metadata,
    retrieve_source = function(source_id, refresh = FALSE, ...) {
      Sys.sleep(0.1)
      retrieval_count <<- retrieval_count + 1L
      layers[[source_id]]
    },
    build_crosswalk = function(from, to, ...) {
      tibble::tibble(from_id = 1L, to_id = "P1")
    },
    .package = "ONgeoR"
  )
  server <- load_shiny_server()
  use_sequential_futures()

  shiny::testServer(server, {
    session$setInputs(
      base_layer = "base_polygon",
      overlay_source = "overlay_point"
    )

    first_started <- Sys.time()
    session$setInputs(confirm_join_btn = 1)
    expect_identical(wait_for_extended_task(build_task, session), "success")
    first_elapsed <- as.numeric(difftime(
      Sys.time(), first_started, units = "secs"
    ))

    second_started <- Sys.time()
    session$setInputs(confirm_join_btn = 2)
    expect_identical(wait_for_extended_task(build_task, session), "success")
    second_elapsed <- as.numeric(difftime(
      Sys.time(), second_started, units = "secs"
    ))

    expect_equal(retrieval_count, 2L)
    expect_lt(second_elapsed, first_elapsed)
    expect_match(rendered_html(output$link_task_status), "Completed")
    expect_match(
      rendered_html(output$link_task_status),
      "no work restarted"
    )
  })
})

test_that("Link passes a point overlay as the crosswalk from layer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  layers <- shiny_fixture_layers()
  calls <- new.env(parent = emptyenv())
  calls$from <- NULL
  calls$to <- NULL
  testthat::local_mocked_bindings(
    list_sources = shiny_fixture_registry,
    get_source = shiny_fixture_metadata,
    retrieve_source = function(source_id, refresh = FALSE, ...) {
      layers[[source_id]]
    },
    build_crosswalk = function(from, to, ...) {
      calls$from <- from
      calls$to <- to
      tibble::tibble(from_id = 1L, to_id = "P1")
    },
    .package = "ONgeoR"
  )
  server <- load_shiny_server()
  use_sequential_futures()

  shiny::testServer(server, {
    session$setInputs(
      base_layer = "base_polygon",
      overlay_source = "overlay_point",
      confirm_join_btn = 1
    )
    expect_identical(wait_for_extended_task(build_task, session), "success")
  })

  expect_identical(calls$from, layers$overlay_point)
  expect_identical(calls$to, layers$base_polygon)

  # SKIPPED: app.R always passes overlay_sf as `from` for vector links, so a
  # point in the base picker is passed as `to`. Covering the universal rule in
  # both picker orders requires the forbidden app.R behavior change.
})

test_that("two point layers preview and join on the main tab as a nearest result", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  # The shared fixture registry has only one point source, so define a
  # two-point registry and layer set locally to exercise the point-to-point
  # path end to end on the main tab. The two layers carry distinct attribute
  # column names (point_id vs station_id): nearest() column-binds both layers'
  # attributes, so a shared name would collide - exactly as the registered
  # point sources (different LIO id fields) do not collide in practice.
  points_a <- fixture_points()
  points_b <- fixture_provenance(sf::st_as_sf(
    tibble::tibble(
      station_id = 101:105,
      lon = c(0.30, 1.30, 2.30, 0.60, 1.60),
      lat = c(0.30, 0.30, 0.30, 0.60, 0.60)
    ),
    coords = c("lon", "lat"), crs = 4326
  ))
  registry <- tibble::tibble(
    source_id = c("points_a", "points_b"),
    name = c("Points A", "Points B"),
    geography_type = c("facility", "facility"),
    feature_count = c(nrow(points_a), nrow(points_b))
  )
  point_layers <- list(points_a = points_a, points_b = points_b)
  metadata <- function(source_id) {
    row <- registry[registry$source_id == source_id, , drop = FALSE]
    list(
      name = row$name[[1]],
      service_layer = source_id,
      geography_type = row$geography_type[[1]],
      feature_count = row$feature_count[[1]],
      key_fields = list(id = "point_id", name = "point_id"),
      license = "test fixture",
      source_url = "https://example.test/ongeor-fixture"
    )
  }
  testthat::local_mocked_bindings(
    list_sources = function() registry,
    get_source = metadata,
    retrieve_source = function(source_id, refresh = FALSE, ...) {
      point_layers[[source_id]]
    },
    .package = "ONgeoR"
  )
  server <- load_shiny_server()
  use_sequential_futures()

  shiny::testServer(server, {
    session$setInputs(
      base_layer = "points_a",
      overlay_source = "points_b",
      preview_btn = 1
    )
    expect_identical(wait_for_extended_task(preview_task, session), "success")

    # Join is enabled for two point layers once a preview has succeeded.
    expect_match(rendered_html(output$build_btn_ui), "id=\"build_btn\"")
    expect_false(grepl("disabled", rendered_html(output$build_btn_ui)))

    session$setInputs(confirm_join_btn = 1)
    expect_identical(wait_for_extended_task(build_task, session), "success")

    # A nearest result: the pair table is canonical (one row per target).
    expect_s3_class(cw_result$crosswalk, "data.frame")
    expect_s3_class(cw_result$pairs, "data.frame")
    expect_gt(nrow(cw_result$pairs), 0)
    expect_true(all(cw_result$pairs$relation == "nearest"))
    expect_equal(nrow(cw_result$pairs), nrow(points_b))
    # Was a characterization test for a real bug: summarise_by_target()
    # collapsed a nearest pair table to 0 rows, because nearest pairs carry NA
    # share_of_target and which.max(NA) is integer(0). That silently emptied the
    # point-to-point mapping.csv download. Fixed, so this now asserts the
    # one-row-per-target guarantee the function exists to provide.
    expect_equal(nrow(cw_result$crosswalk), nrow(points_b))
  })
})

test_that("every offered raster palette actually renders a raster layer", {
  # leaflet::colorNumeric() accepts a wrong-case palette name when the palette
  # function is BUILT, and only fails later when addRasterImage() applies it -
  # so a bad default renders an empty map with no error surfaced in the UI.
  # The app shipped "Viridis"/"Magma" (invalid; leaflet wants them lowercase)
  # as the raster choices, with "Viridis" as the default, which silently broke
  # every raster preview. Guard each offered value end-to-end.
  env <- load_shiny_app_env()
  # Needs a real CRS and extent: addRasterImage() reprojects to EPSG:3857,
  # and a CRS-less raster fails in terra with "warp failure" before the
  # palette is ever applied.
  raster <- terra::rast(
    nrows = 4, ncols = 4,
    xmin = -80, xmax = -79, ymin = 43, ymax = 44,
    crs = "EPSG:4326", vals = seq_len(16)
  )

  # The default, with no style inputs registered yet.
  default_style <- env$read_layer_style(list(), "overlay", "raster")
  expect_no_error(
    env$add_styled_sf_layer(
      leaflet::leaflet(), raster, "Overlay source", default_style
    )
  )

  for (palette in c("viridis", "magma", "Blues")) {
    style <- env$read_layer_style(
      list(overlay_raster_palette = palette), "overlay", "raster"
    )
    expect_identical(style$raster_palette, palette)
    expect_no_error(
      env$add_styled_sf_layer(
        leaflet::leaflet(), raster, "Overlay source", style
      )
    )
  }
})

test_that("rasters are packed across the future boundary and survive", {
  # A SpatRaster is an external pointer; returning one straight out of a
  # multisession future delivers a NULL pointer, and the first later use dies
  # with "NULL value passed as symbol address". That broke every raster
  # preview in the live app (empty map, ~32 KB map.html) while sf pairings
  # were fine and every in-process/sequential test still passed. Guard the
  # wrap/unwrap contract directly so it cannot silently regress.
  env <- load_shiny_app_env()
  raster <- terra::rast(
    nrows = 4, ncols = 4,
    xmin = -80, xmax = -79, ymin = 43, ymax = 44,
    crs = "EPSG:4326", vals = seq_len(16)
  )

  packed <- env$pack_spatial(raster)
  expect_s4_class(packed, "PackedSpatRaster")

  restored <- env$unpack_spatial(packed)
  expect_s4_class(restored, "SpatRaster")
  expect_equal(as.integer(terra::values(restored)[, 1]), seq_len(16))

  # Non-raster payloads must pass through both helpers untouched.
  sf_layer <- shiny_fixture_layers()$base_polygon
  expect_identical(env$pack_spatial(sf_layer), sf_layer)
  expect_identical(env$unpack_spatial(sf_layer), sf_layer)

  # A packed raster must survive an actual serialize/unserialize round trip,
  # which is what crossing a worker boundary really does.
  round_tripped <- env$unpack_spatial(unserialize(serialize(packed, NULL)))
  expect_s4_class(round_tripped, "SpatRaster")
  expect_equal(as.integer(terra::values(round_tripped)[, 1]), seq_len(16))
})

# --- Map furniture (PHU_simple) -------------------------------------
# The Link tab map draws bundled furniture layers at load: PHU_simple
# (checked) at the bottom of the overlay list, with
# PHU_simple suppressed while the full-resolution phu_boundaries source is
# selected.

# Overlay groups of a built leaflet widget, in layer-control order. The
# widget stores every operation as list(method = ..., args = ...) with
# positional args; addLayersControl's second arg is overlayGroups.
furniture_test_overlay_groups <- function(map) {
  controls <- Filter(
    function(call) identical(call$method, "addLayersControl"),
    map$x$calls
  )
  expect_length(controls, 1)
  unlist(controls[[1]]$args[[2]])
}

# Groups hidden at render time (unchecked in the layer control).
furniture_test_hidden_groups <- function(map) {
  hidden <- Filter(
    function(call) identical(call$method, "hideGroup"),
    map$x$calls
  )
  unlist(lapply(hidden, function(call) call$args[[1]]))
}

test_that("PHU_simple furniture is on the map at load with no user input", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  testthat::local_mocked_bindings(
    list_sources = shiny_fixture_registry,
    get_source = shiny_fixture_metadata,
    .package = "ONgeoR"
  )
  server <- load_shiny_server()

  shiny::testServer(server, {
    # testServer starts with inputs UNSET, which is NOT the app's startup
    # state: the real UI initialises base_layer to "phu_boundaries" (its
    # selectInput default). Setting the real defaults here is the whole point
    # of this test - an earlier version left inputs NULL, passed, and missed a
    # bug where the app hid this layer on every fresh load because the
    # suppression rule keyed off the SELECTION rather than what was drawn.
    session$setInputs(
      base_layer = "phu_boundaries",
      overlay_source = "moh_service_locations"
    )
    map <- link_map()
    # Defaults selected but nothing previewed: furniture must still be drawn.
    expect_identical(furniture_test_overlay_groups(map), c("PHU_simple", "HIVE"))
    # PHU_simple starts checked; HIVE starts hidden.
    expect_false("PHU_simple" %in% furniture_test_hidden_groups(map))
    expect_true("HIVE" %in% furniture_test_hidden_groups(map))
  })
})

# An empty overlay set used to reach addLayersControl as NULL, because
# names(list()) is NULL and c(NULL, NULL) stays NULL. Leaflet renders that as
# a single checkbox labelled "null" - visible in the running app, invisible to
# every test that only asked whether specific groups were present.
test_that("an empty overlay set produces no overlay entries, not a 'null' one", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  testthat::local_mocked_bindings(
    list_sources = shiny_fixture_registry,
    get_source = shiny_fixture_metadata,
    .package = "ONgeoR"
  )
  app_env <- load_shiny_app_env()

  map <- app_env$render_styled_map(list(), list(), furniture = list())
  control <- Filter(
    function(call) identical(call$method, "addLayersControl"),
    map$x$calls
  )[[1]]
  raw_overlay <- control$args[[2]]

  # Assert on the RAW argument, not on unlist(): NULL and character(0) both
  # unlist to length zero, so a length check cannot tell them apart. The
  # difference is only visible in the browser, where leaflet renders a NULL
  # overlayGroups as a checkbox labelled "null". A length-only version of this
  # test passed against the bug it was written for.
  expect_false(is.null(raw_overlay))
  expect_type(raw_overlay, "character")
  expect_length(raw_overlay, 0L)
})

# No layer control anywhere may carry a literal "null" group, whatever the
# selection state.
test_that("no overlay group is ever literally 'null'", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  testthat::local_mocked_bindings(
    list_sources = shiny_fixture_registry,
    get_source = shiny_fixture_metadata,
    .package = "ONgeoR"
  )
  server <- load_shiny_server()

  shiny::testServer(server, {
    for (base in c("phu_boundaries", "base_polygon")) {
      session$setInputs(base_layer = base, overlay_source = "overlay_point")
      groups <- furniture_test_overlay_groups(link_map())
      expect_false(any(tolower(as.character(groups)) == "null"))
      expect_false(any(is.na(groups)))
    }
  })
})

test_that("PHU_simple furniture is suppressed when phu_boundaries is selected", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  testthat::local_mocked_bindings(
    list_sources = shiny_fixture_registry,
    get_source = shiny_fixture_metadata,
    .package = "ONgeoR"
  )
  server <- load_shiny_server()

  # The rule is keyed on what is DRAWN, so exercise it where it lives. Passing
  # a preview of phu_boundaries through testServer is not possible: the fixture
  # registry defines only base_polygon/overlay_point/other_polygon.
  app_env <- load_shiny_app_env()

  expect_named(app_env$furniture_layers(character(0)), c("PHU_simple", "HIVE"))
  expect_named(
    app_env$furniture_layers(c("phu_boundaries", "moh_service_locations")),
    "HIVE"
  )
  expect_named(
    app_env$furniture_layers(c("base_polygon", "phu_boundaries")),
    "HIVE"
  )
  expect_length(
    app_env$furniture_layers(c("phu_boundaries", "hive")),
    0L
  )

  # REGRESSION GUARD for the bug found by running the app 2026-07-20:
  # phu_boundaries is the app's DEFAULT base layer, so keying suppression off
  # the selection hid the furniture on every fresh load. Selecting it without
  # a preview must NOT suppress - nothing is drawn until a preview succeeds.
  shiny::testServer(server, {
    session$setInputs(
      base_layer = "phu_boundaries",
      overlay_source = "overlay_point"
    )
    expect_identical(
      furniture_test_overlay_groups(link_map()),
      c("PHU_simple", "HIVE")
    )
  })
})

# HIVE is drawn as furniture (unchecked) so the user can toggle it on
# without a retrieval round-trip. It is suppressed when the live hive
# source is actually drawn as a selected layer.
test_that("hive furniture is present but hidden at load", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  testthat::local_mocked_bindings(
    list_sources = shiny_fixture_registry,
    get_source = shiny_fixture_metadata,
    .package = "ONgeoR"
  )
  server <- load_shiny_server()

  shiny::testServer(server, {
    map <- link_map()
    expect_true("HIVE" %in% furniture_test_overlay_groups(map))
    expect_true("HIVE" %in% furniture_test_hidden_groups(map))
  })
})

test_that("map.html download bundles PHU_simple alongside the two sources", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  layers <- shiny_fixture_layers()
  testthat::local_mocked_bindings(
    list_sources = shiny_fixture_registry,
    get_source = shiny_fixture_metadata,
    retrieve_source = function(source_id, refresh = FALSE, ...) {
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
    expect_identical(
      wait_for_extended_task(preview_task, session),
      "success"
    )

    # Active sources first, furniture pinned to the bottom of the overlay
    # list.
    expect_identical(
      furniture_test_overlay_groups(link_map()),
      c("Source layer", "Target layer", "PHU_simple", "HIVE")
    )

    map_file <- output$dl_cw_map
    html <- paste(readLines(map_file, warn = FALSE), collapse = "\n")
    expect_match(html, "PHU_simple", fixed = TRUE)
    expect_match(html, "Source layer", fixed = TRUE)
    expect_match(html, "Target layer", fixed = TRUE)

    # The addition is surfaced in the download UI, not silent.
    expect_match(
      rendered_html(output$link_downloads_ui),
      "PHU_simple",
      fixed = TRUE
    )
  })
})
