test_that("layer pickers offer only geometry-appropriate choices", {
  testthat::local_mocked_bindings(
    list_sources = shiny_fixture_registry,
    get_source = shiny_fixture_metadata,
    .package = "ONgeoR"
  )
  app_env <- load_shiny_app_env()

  expect_named(app_env$source_choices_grouped(), "Polygons")
  expect_setequal(
    unname(unlist(app_env$source_choices_grouped())),
    c("base_polygon", "other_polygon")
  )
  expect_named(app_env$target_choices_grouped(), "Points")
  expect_setequal(
    unname(unlist(app_env$target_choices_grouped())),
    c("overlay_point", "postal_upload")
  )
})

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

    actual_file <- output$dl_cw_csv
    expect_setequal(
      utils::unzip(actual_file, list = TRUE)$Name,
      c("mapping.csv", "pairs.csv")
    )
  })
})

test_that("postal preview streams honest phases and ends done, no title banner", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  layers <- shiny_fixture_layers()
  postal_sf <- sf::st_as_sf(
    tibble::tibble(
      postal_code = c("K1A 0B1", "M5V 2T6"),
      point_source = c("geonames", "lia"),
      point_method = c("place", "address"),
      lon = c(-75.7, -79.4),
      lat = c(45.4, 43.7)
    ),
    coords = c("lon", "lat"), crs = 4326
  )
  testthat::local_mocked_bindings(
    list_sources = shiny_fixture_registry,
    get_source = shiny_fixture_metadata,
    retrieve_source = function(source_id, refresh = FALSE, ...) {
      Sys.sleep(0.05)
      layers[[source_id]]
    },
    resolve_postal_points = function(x, as_sf = FALSE) postal_sf,
    .package = "ONgeoR"
  )
  server <- load_shiny_server()
  use_sequential_futures()

  shiny::testServer(server, {
    progress_messages <- list()
    session$sendCustomMessage <- function(type, message) {
      progress_messages[[length(progress_messages) + 1L]] <<- list(
        type = type,
        message = message
      )
    }

    session$setInputs(
      base_layer = "base_polygon",
      overlay_source = "postal_upload"
    )
    csv_file <- withr::local_tempfile(fileext = ".csv")
    utils::write.csv(
      data.frame(postal = c("K1A 0B1", "M5V 2T6")),
      csv_file,
      row.names = FALSE
    )
    session$setInputs(postal_file = list(
      name = "postal-codes.csv",
      datapath = csv_file
    ))
    session$flushReact()
    session$setInputs(postal_column = "postal")
    session$flushReact()
    expect_match(rendered_html(output$preview_task_status), "Idle")

    session$setInputs(preview_btn = 1)
    running <- rendered_html(output$preview_task_status)
    expect_match(running, "Running", fixed = TRUE)
    expect_match(running, 'data-state="running"', fixed = TRUE)
    expect_false(grepl("progress-bar-animated", running, fixed = TRUE))
    expect_true(nzchar(preview_progress_path()))

    # Completion is owned by the main process the moment the future resolves:
    # the mapping phase is appended, the layers are unpacked, and the terminal
    # dialog push (done = TRUE) goes out in the same reactive cycle. There is no
    # browser render signal to wait for, so the dialog cannot hang on a spinner.
    expect_identical(wait_for_extended_task(preview_task, session), "success")
    expect_identical(preview_state(), "completed")
    expect_identical(
      preview_progress_log(),
      c(
        "Retrieving source data...",
        "Retrieving target data...",
        "Preparing layers for the map...",
        "Mapping data..."
      )
    )

    done <- rendered_html(output$preview_task_status)
    expect_match(done, "Completed", fixed = TRUE)
    expect_false(grepl("progress-bar", done, fixed = TRUE))

    # The dialog ends in a done state: the last ongeor-progress message the
    # browser receives has done = TRUE, and no completion message carries a
    # title claiming the run is "complete" (the honest live log is the content;
    # a success push has no title banner at all).
    #
    # NOTE on how it gets there: a successful preview no longer declares itself
    # done when the map payload is sent - it waits for input$cw_map_rendered,
    # with a render_ack_timeout_secs cap. testServer runs a MOCK CLOCK, so that
    # cap elapses immediately here and supplies the terminal push. In a live
    # session the browser's acknowledgement arrives first. Either way the run
    # ends done = TRUE, which is what this test pins. The ack path itself is
    # covered in test-progress-dialog.R.
    preview_messages <- Filter(
      function(x) identical(x$type, "ongeor-progress"),
      progress_messages
    )
    expect_true(isTRUE(preview_messages[[length(preview_messages)]]$message$done))
    done_messages <- Filter(
      function(x) isTRUE(x$message$done),
      preview_messages
    )
    expect_gte(length(done_messages), 1L)
    for (msg in done_messages) {
      expect_false(grepl("complete", msg$message$title %||% "", ignore.case = TRUE))
    }

    # The phase file is cleaned up once the task settles.
    expect_false(file.exists(preview_progress_path()))
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
# The map draws the bundled PHU_simple furniture layer at load, suppressing it
# while either full-resolution PHU vintage is drawn.

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
    expect_identical(furniture_test_overlay_groups(map), "PHU_simple")
    expect_false("PHU_simple" %in% furniture_test_hidden_groups(map))
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

test_that("PHU_simple furniture is suppressed for either drawn PHU vintage", {
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

  expect_named(app_env$furniture_layers(character(0)), "PHU_simple")
  expect_length(app_env$furniture_layers(c("phu_boundaries", "moh_service_locations")), 0L)
  expect_length(app_env$furniture_layers(c("base_polygon", "phu_boundaries")), 0L)
  expect_length(app_env$furniture_layers("phu_boundaries_pre2025"), 0L)

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
      "PHU_simple"
    )
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
    # list. Each slot carries the layer's own name, so the layers control
    # says WHICH layer it means rather than only which slot.
    expect_identical(
      furniture_test_overlay_groups(link_map()),
      c("Source layer - Base polygon", "Target layer - Overlay point",
        "PHU_simple")
    )

    map_file <- output$dl_cw_map
    html <- paste(readLines(map_file, warn = FALSE), collapse = "\n")
    expect_match(html, "PHU_simple", fixed = TRUE)
    expect_match(html, "Source layer - Base polygon", fixed = TRUE)
    expect_match(html, "Target layer - Overlay point", fixed = TRUE)

    # The addition is surfaced in the download UI, not silent.
    expect_match(
      rendered_html(output$link_downloads_ui),
      "PHU_simple",
      fixed = TRUE
    )
  })
})

# --- Postal upload -------------------------------------------------

test_that("auto-detection picks the right postal code column", {
  env <- load_shiny_app_env()
  df <- data.frame(
    name = c("A", "B", "C"),
    postal = c("K1A 0B1", "M5V 2T6", "N2L 3G1"),
    other = c("foo", "bar", "baz"),
    stringsAsFactors = FALSE
  )
  result <- env$detect_postal_column(df)
  expect_identical(result$column, "postal")
  expect_gte(result$score, 0.8)
})

test_that("auto-detection declines when no column qualifies", {
  env <- load_shiny_app_env()
  df <- data.frame(
    name = c("A", "B", "C"),
    value = c("foo", "bar", "baz"),
    stringsAsFactors = FALSE
  )
  result <- env$detect_postal_column(df)
  expect_null(result$column)
  expect_lt(result$score, 0.8)
})

test_that("user override is honored over auto-detected column", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  postal_sf <- sf::st_as_sf(
    tibble::tibble(
      postal_code = c("K1A 0B1", "M5V 2T6"),
      point_source = c("geonames", "geonames"),
      point_method = c("place", "place"),
      lon = c(-75.7, -79.4),
      lat = c(45.4, 43.7)
    ),
    coords = c("lon", "lat"), crs = 4326
  )
  testthat::local_mocked_bindings(
    list_sources = shiny_fixture_registry,
    get_source = shiny_fixture_metadata,
    resolve_postal_points = function(x, as_sf = FALSE) {
      postal_sf[seq_along(x), , drop = FALSE]
    },
    .package = "ONgeoR"
  )
  server <- load_shiny_server()

  shiny::testServer(server, {
    session$setInputs(
      base_layer = "base_polygon",
      overlay_source = "postal_upload"
    )
    csv_file <- withr::local_tempfile(fileext = ".csv")
    utils::write.csv(
      data.frame(
        wrong = c("aaa", "bbb"),
        right_col = c("K1A 0B1", "M5V 2T6"),
        stringsAsFactors = FALSE
      ),
      csv_file, row.names = FALSE
    )
    session$setInputs(postal_file = list(
      name = "test.csv",
      datapath = csv_file
    ))
    session$flushReact()
    session$setInputs(postal_column = "right_col")
    session$flushReact()
    pr <- postal_result()
    expect_s3_class(pr$sf, "sf")
    expect_equal(pr$n_input, 2L)
  })
})

test_that("successful postal upload produces an sf POINT layer used as target", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  layers <- shiny_fixture_layers()
  postal_sf <- sf::st_as_sf(
    tibble::tibble(
      postal_code = c("K1A 0B1", "M5V 2T6", "N2L 3G1"),
      point_source = c("geonames", "lia", "lia"),
      point_method = c("place", "address", "address"),
      lon = c(-75.7, -79.4, -80.5),
      lat = c(45.4, 43.7, 43.5)
    ),
    coords = c("lon", "lat"), crs = 4326
  )
  testthat::local_mocked_bindings(
    list_sources = shiny_fixture_registry,
    get_source = shiny_fixture_metadata,
    retrieve_source = function(source_id, refresh = FALSE, ...) {
      layers[[source_id]]
    },
    resolve_postal_points = function(x, as_sf = FALSE) {
      postal_sf
    },
    build_crosswalk = function(from, to, ...) {
      tibble::tibble(from_id = seq_len(nrow(from)), to_id = seq_len(nrow(to)))
    },
    .package = "ONgeoR"
  )
  server <- load_shiny_server()
  use_sequential_futures()

  shiny::testServer(server, {
    session$setInputs(
      base_layer = "base_polygon",
      overlay_source = "postal_upload"
    )
    csv_file <- withr::local_tempfile(fileext = ".csv")
    utils::write.csv(
      data.frame(postal = c("K1A 0B1", "M5V 2T6", "N2L 3G1"), stringsAsFactors = FALSE),
      csv_file, row.names = FALSE
    )
    session$setInputs(postal_file = list(name = "test.csv", datapath = csv_file))
    session$flushReact()
    session$setInputs(postal_column = "postal")
    session$flushReact()
    pr <- postal_result()
    expect_s3_class(pr$sf, "sf")
    expect_equal(pr$n_placed, 3L)

    session$setInputs(preview_btn = 1)
    expect_identical(wait_for_extended_task(preview_task, session), "success")
    expect_s3_class(cw_result$overlay_sf, "sf")
    geom_types <- unique(as.character(sf::st_geometry_type(cw_result$overlay_sf)))
    expect_true(all(geom_types %in% c("POINT", "MULTIPOINT")))
  })
})

test_that("unmatched postal codes are reported in status text", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  postal_sf <- sf::st_as_sf(
    tibble::tibble(
      postal_code = "K1A 0B1",
      point_source = "geonames",
      point_method = "place",
      lon = -75.7,
      lat = 45.4
    ),
    coords = c("lon", "lat"), crs = 4326
  )
  testthat::local_mocked_bindings(
    list_sources = shiny_fixture_registry,
    get_source = shiny_fixture_metadata,
    resolve_postal_points = function(x, as_sf = FALSE) {
      postal_sf
    },
    .package = "ONgeoR"
  )
  server <- load_shiny_server()

  shiny::testServer(server, {
    session$setInputs(
      base_layer = "base_polygon",
      overlay_source = "postal_upload"
    )
    csv_file <- withr::local_tempfile(fileext = ".csv")
    utils::write.csv(
      data.frame(postal = c("K1A 0B1", "INVALID", "XXX"), stringsAsFactors = FALSE),
      csv_file, row.names = FALSE
    )
    session$setInputs(postal_file = list(name = "test.csv", datapath = csv_file))
    session$flushReact()
    session$setInputs(postal_column = "postal")
    session$flushReact()
    pr <- postal_result()
    expect_equal(pr$n_input, 3L)
    expect_equal(pr$n_placed, 1L)
    expect_equal(pr$n_unmatched, 2L)

    status_html <- rendered_html(output$postal_status_ui)
    expect_match(status_html, "3")
    expect_match(status_html, "1")
    expect_match(status_html, "2")
    expect_match(status_html, "unmatched")
  })
})

test_that("postal resolver failures show their cause instead of unmatched counts", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  testthat::local_mocked_bindings(
    list_sources = shiny_fixture_registry,
    get_source = shiny_fixture_metadata,
    resolve_postal_points = function(x, as_sf = FALSE) {
      stop("Postal resolver unavailable")
    },
    .package = "ONgeoR"
  )
  server <- load_shiny_server()

  shiny::testServer(server, {
    session$setInputs(
      base_layer = "base_polygon",
      overlay_source = "postal_upload"
    )
    csv_file <- withr::local_tempfile(fileext = ".csv")
    utils::write.csv(
      data.frame(postal = c("K1A 0B1", "M5V 2T6"), stringsAsFactors = FALSE),
      csv_file, row.names = FALSE
    )
    session$setInputs(postal_file = list(name = "test.csv", datapath = csv_file))
    session$flushReact()
    session$setInputs(postal_column = "postal")
    session$flushReact()

    pr <- postal_result()
    expect_false(is.null(pr$error))
    expect_match(pr$error$message, "Postal resolver unavailable", fixed = TRUE)
    status_html <- rendered_html(output$postal_status_ui)
    expect_match(status_html, "Postal resolver unavailable", fixed = TRUE)
    expect_false(grepl("0 placed", status_html, fixed = TRUE))
  })
})

test_that("partial postal placement remains a successful unmatched result", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  postal_sf <- sf::st_as_sf(
    tibble::tibble(
      postal_code = "K1A 0B1",
      point_source = "geonames",
      lon = -75.7,
      lat = 45.4
    ),
    coords = c("lon", "lat"), crs = 4326
  )
  testthat::local_mocked_bindings(
    list_sources = shiny_fixture_registry,
    get_source = shiny_fixture_metadata,
    resolve_postal_points = function(x, as_sf = FALSE) postal_sf,
    .package = "ONgeoR"
  )
  server <- load_shiny_server()

  shiny::testServer(server, {
    session$setInputs(
      base_layer = "base_polygon",
      overlay_source = "postal_upload"
    )
    csv_file <- withr::local_tempfile(fileext = ".csv")
    utils::write.csv(
      data.frame(postal = c("K1A 0B1", "INVALID"), stringsAsFactors = FALSE),
      csv_file, row.names = FALSE
    )
    session$setInputs(postal_file = list(name = "test.csv", datapath = csv_file))
    session$flushReact()
    session$setInputs(postal_column = "postal")
    session$flushReact()

    pr <- postal_result()
    expect_null(pr$error)
    expect_equal(pr$n_placed, 1L)
    expect_equal(pr$n_unmatched, 1L)
    status_html <- rendered_html(output$postal_status_ui)
    expect_match(status_html, "2 input rows, 1 placed, 1 unmatched.", fixed = TRUE)
  })
})

test_that("unreadable file yields a status message and no crash", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  testthat::local_mocked_bindings(
    list_sources = shiny_fixture_registry,
    get_source = shiny_fixture_metadata,
    .package = "ONgeoR"
  )
  server <- load_shiny_server()

  shiny::testServer(server, {
    session$setInputs(
      base_layer = "base_polygon",
      overlay_source = "postal_upload"
    )
    bad_file <- withr::local_tempfile(fileext = ".parquet")
    writeLines("not a real file", bad_file)
    session$setInputs(postal_file = list(name = "data.parquet", datapath = bad_file))
    session$flushReact()
    fr <- postal_file_result()
    expect_null(fr$df)
    expect_match(fr$error, "Unsupported|parquet", ignore.case = TRUE)

    column_html <- rendered_html(output$postal_column_ui)
    expect_match(column_html, "Unsupported|parquet", ignore.case = TRUE)
  })
})

test_that("selecting postal_upload does not error in geom_kind or geo_badge", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  testthat::local_mocked_bindings(
    list_sources = shiny_fixture_registry,
    get_source = shiny_fixture_metadata,
    .package = "ONgeoR"
  )
  server <- load_shiny_server()

  shiny::testServer(server, {
    session$setInputs(
      base_layer = "base_polygon",
      overlay_source = "postal_upload"
    )
    badge_html <- rendered_html(output$overlay_geom_badge)
    expect_match(badge_html, "Point")
    expect_match(badge_html, "geo-point")

    rel_html <- rendered_html(output$link_relationship)
    expect_match(rel_html, "Point-in-boundary containment")
  })
})

# --- Target-geometry download (target.gpkg) ------------------------------

test_that("best-match collapse keeps one deterministic row per target", {
  env <- load_shiny_app_env()
  pairs <- tibble::tibble(
    from_id = c("A", "A", "A", "B", "B", "C", "C"),
    coverage = c(0.2, 0.9, 0.5, NA, NA, NA, NA),
    match_distance_km = c(NA, NA, NA, 5, 2, NA, NA),
    to_id = c("x1", "x2", "x3", "y1", "y2", "z1", "z2")
  )
  out <- env$collapse_crosswalk_best_match(pairs)
  expect_equal(nrow(out), 3L)
  expect_equal(out$from_id, c("A", "B", "C"))
  # coverage path: highest coverage wins (A keeps the 0.9 row).
  expect_equal(out$to_id[out$from_id == "A"], "x2")
  # distance fallback: coverage all NA, lowest distance wins (B keeps 2).
  expect_equal(out$to_id[out$from_id == "B"], "y2")
  # tie: coverage and distance both NA, first occurrence wins (C keeps z1).
  expect_equal(out$to_id[out$from_id == "C"], "z1")
  # deterministic across runs.
  expect_identical(out, env$collapse_crosswalk_best_match(pairs))
})

# --- Progress reporting and explicit failures -----------------------------

test_that("the status line reports state plainly, with no bar", {
  # The animated bar, green banner, phase log and Cancel all moved into the
  # progress dialog: Lennon asked for them in a pop up rather than the control
  # panel, where they were "hard to see".
  env <- load_shiny_app_env()
  running <- rendered_html(env$task_status_ui("running", "Fetching source layer."))
  expect_match(running, "Fetching source layer.", fixed = TRUE)
  expect_match(running, "Running", fixed = TRUE)
  expect_false(grepl("progress-bar-animated", running, fixed = TRUE))
  for (state in c("idle", "failed", "cancelled", "completed")) {
    html <- rendered_html(env$task_status_ui(state, "x"))
    expect_false(grepl("progress-bar-animated", html, fixed = TRUE))
  }
})

test_that("the phase log accumulates in order and tolerates a missing file", {
  env <- load_shiny_app_env()
  path <- env$new_progress_path()
  expect_null(env$read_progress_phases(path))       # not written yet
  cat("Retrieving source data...\n", file = path, append = TRUE)
  cat("Retrieving target data...\n", file = path, append = TRUE)
  cat("Joining layers...\n", file = path, append = TRUE)
  expect_equal(
    env$read_progress_phases(path),
    c("Retrieving source data...", "Retrieving target data...", "Joining layers...")
  )
  expect_equal(env$read_progress_phase(path), "Joining layers...")
  unlink(path)
  expect_null(env$read_progress_phases(path))
  expect_null(env$read_progress_phase(path))
  expect_null(env$read_progress_phase(NULL))
})

test_that("the progress dialog is a bare phase list with cancel and a hidden OK", {
  # The dialog shell is static: its phase list is filled in from the browser by
  # the "ongeor-progress" custom message, because re-rendering anything inside
  # a live Shiny modal tore the modal out of the DOM (measured 2026-08-06).
  # There is deliberately no title banner: the live log IS the content, each
  # step spinning while it runs and flipping to a check + "done" when the next
  # step starts or the run finishes.
  #
  # On progress bars (2026-08-07, revised): the original rule here was "no
  # progress bar at all", because the bar that shipped implied progress while a
  # run was actually hung. Lennon asked for a visible sign of life after a
  # first-time layer download ran for 10+ minutes looking like a failure. The
  # rule is therefore narrowed, not dropped: an INDETERMINATE activity bar plus
  # an elapsed clock is allowed, because neither can state something false; a
  # DETERMINATE bar is still banned, because the work (a paginated network
  # download and a spatial join) exposes no fraction to report honestly.
  env <- load_shiny_app_env()
  html <- rendered_html(env$task_progress_modal("Join"))
  expect_false(grepl("alert-success", html, fixed = TRUE))
  expect_false(grepl("running...", html, fixed = TRUE))
  # Indeterminate only: striped + animated, never a reported value.
  expect_match(html, "progress-bar-animated", fixed = TRUE)
  expect_false(grepl("aria-valuenow", html, fixed = TRUE))
  # Full-width stripes are the indeterminate idiom; any other width would be a
  # claim about how far along the run is.
  expect_match(html, "width: 100%", fixed = TRUE)
  expect_match(html, 'id="task_elapsed"', fixed = TRUE)
  expect_match(html, 'id="task_hint"', fixed = TRUE)
  expect_match(html, 'id="task_phase_list"', fixed = TRUE)
  expect_match(html, 'id="cancel_task_btn"', fixed = TRUE)
  # OK ships hidden and is revealed by the browser when the task finishes, so
  # the dialog waits for the user instead of vanishing.
  expect_match(html, 'id="progress_ok_btn"', fixed = TRUE)
  expect_match(html, "d-none", fixed = TRUE)
})

test_that("retrieval failures are classified from the discarded parent", {
  env <- load_shiny_app_env()
  # ONgeoR's shape: a generic top-level message with the real cause as parent.
  wrapped <- function(parent_msg) {
    rlang::catch_cnd(rlang::abort(
      "Could not retrieve source 'X' (layer 'Y'). Retry later;",
      parent = rlang::catch_cnd(rlang::abort(parent_msg))
    ))
  }

  dns <- env$describe_retrieval_failure(wrapped("Could not resolve host: ws.lioservices.lrc.gov.on.ca"))
  expect_match(dns$message, "could not be", fixed = TRUE)
  expect_false(dns$retry)
  expect_match(dns$message, "Retrying now will not help", fixed = TRUE)

  refused <- env$describe_retrieval_failure(wrapped("Failed to connect to host port 443"))
  expect_false(refused$retry)
  expect_match(refused$message, "refused the connection", fixed = TRUE)

  proxy <- env$describe_retrieval_failure(wrapped("Received HTTP code 407 from proxy after CONNECT"))
  expect_false(proxy$retry)
  expect_match(proxy$message, "proxy", fixed = TRUE)

  # Transient causes are the only ones that advise a retry.
  server <- env$describe_retrieval_failure(wrapped("HTTP 503 Service Unavailable"))
  expect_true(server$retry)
  expect_match(server$message, "Retrying in a few minutes", fixed = TRUE)

  timeout <- env$describe_retrieval_failure(wrapped("Timeout was reached"))
  expect_true(timeout$retry)

  # The raw chain is preserved for the technical-detail block.
  expect_match(dns$detail, "Could not resolve host", fixed = TRUE)
  expect_match(dns$detail, "Could not retrieve source", fixed = TRUE)
})

test_that("an unclassified failure keeps its original message", {
  env <- load_shiny_app_env()
  cnd <- rlang::catch_cnd(rlang::abort("Fixture retrieval failed"))
  described <- env$describe_retrieval_failure(cnd)
  # No invented cause, no retry advice either way.
  expect_equal(described$message, "Fixture retrieval failed")
  expect_true(is.na(described$retry))
})

test_that("linked aggregation reduces cells to one row per target", {
  env <- load_shiny_app_env()
  # Three targets; target 1 has three cells, target 2 has one, target 3 none.
  link_attrs <- data.frame(
    band_1 = c(10, 20, 30, 7),
    band_2 = c(1, NA, 5, 2),
    retrieved_at = rep("2026-08-06", 4),
    stringsAsFactors = FALSE
  )
  row_of_target <- c(1L, 1L, 1L, 2L)

  out <- env$aggregate_linked_by_target(link_attrs, row_of_target, 3L)

  expect_equal(nrow(out), 3L)
  # numeric columns reduce to the mean across the target's cells.
  expect_equal(out$band_1, c(20, 7, NA))
  # NA cells are skipped rather than poisoning the mean.
  expect_equal(out$band_2, c(3, 2, NA))
  # non-numeric provenance keeps its first value; unmatched target gets NA.
  expect_equal(out$retrieved_at, c("2026-08-06", "2026-08-06", NA))
  # cell counts, including the unmatched target.
  expect_equal(out$linked_cells, c(3L, 1L, 0L))
})

test_that("linked aggregation ignores cells matching no target", {
  env <- load_shiny_app_env()
  link_attrs <- data.frame(band_1 = c(10, 99, 20))
  # The middle cell fell outside every target, so match() left NA.
  out <- env$aggregate_linked_by_target(link_attrs, c(1L, NA, 1L), 1L)

  expect_equal(nrow(out), 1L)
  expect_equal(out$band_1, 15)
  # the out-of-target cell is excluded from both the mean and the count.
  expect_equal(out$linked_cells, 2L)
})

test_that("build_crosswalk merge collapses pairs and joins on from_id_col", {
  env <- load_shiny_app_env()
  target <- fixture_points()
  cw <- tibble::tibble(
    from_id = c("1", "1", "2"),
    from_name = c("p1", "p1", "p2"),
    from_source = "src",
    to_id = c("P1", "P2", "P1"),
    to_name = c("U1", "U2", "U1"),
    to_source = "base",
    match_method = "within",
    match_distance_km = NA_real_,
    coverage = c(0.3, 0.9, NA),
    from_id_col = "point_id",
    to_id_col = "PHU_ID",
    source_url_from = "https://example.test",
    source_url_to = "https://example.test",
    retrieved_at = as.POSIXct("2026-08-06", tz = "UTC")
  )
  merged <- env$merge_target_attributes(target, crosswalk = cw)
  expect_s3_class(merged, "sf")
  expect_equal(nrow(merged), nrow(target))
  # point 1 had two pairs; the 0.9-coverage row (to_id P2) wins.
  expect_equal(merged$to_id[merged$point_id == 1], "P2")
  expect_equal(merged$to_id[merged$point_id == 2], "P1")
  # unmatched targets keep their geometry with NA attributes.
  expect_true(is.na(merged$to_id[merged$point_id == 3]))
})

test_that("target.gpkg round-trips target geometry with merged attributes", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  drivers <- sf::st_drivers()
  skip_if(!("GPKG" %in% drivers$name), "GPKG driver not available")

  # Two overlapping polygon layers in long-lat (4326), mirroring
  # fixture_overlap_layers()'s shares (F1 fully in T1; F2 split 0.25/0.75
  # across T1/T2) at a 0.1-degree scale. 4326 keeps the preview's map
  # render warning-free; build_intersection() reprojects internally for its
  # area arithmetic.
  lonlat_square <- function(x1, x2, y1, y2) sf::st_polygon(list(rbind(
    c(x1, y1), c(x2, y1), c(x2, y2), c(x1, y2), c(x1, y1)
  )))
  src_poly <- fixture_provenance(sf::st_sf(
    SRC_ID = c("F1", "F2"),
    SRC_NAME = c("From 1", "From 2"),
    geometry = sf::st_sfc(
      lonlat_square(-79.3, -79.2, 44.0, 44.1),
      lonlat_square(-79.2, -79.1, 44.0, 44.1),
      crs = 4326
    )
  ))
  tgt_poly <- fixture_provenance(sf::st_sf(
    TGT_ID = c("T1", "T2"),
    TGT_NAME = c("To 1", "To 2"),
    geometry = sf::st_sfc(
      lonlat_square(-79.3, -79.175, 44.0, 44.1),
      lonlat_square(-79.175, -79.1, 44.0, 44.1),
      crs = 4326
    )
  ))
  registry <- tibble::tibble(
    source_id = c("src_poly", "tgt_poly"),
    name = c("Source Poly", "Target Poly"),
    geography_type = c("boundary", "boundary"),
    feature_count = c(nrow(src_poly), nrow(tgt_poly))
  )
  poly_layers <- list(src_poly = src_poly, tgt_poly = tgt_poly)
  metadata <- function(source_id) {
    row <- registry[registry$source_id == source_id, , drop = FALSE]
    list(
      name = row$name[[1]],
      service_layer = source_id,
      geography_type = row$geography_type[[1]],
      feature_count = row$feature_count[[1]],
      key_fields = list(id = "id", name = "name"),
      license = "test fixture",
      source_url = "https://example.test/ongeor-fixture"
    )
  }
  testthat::local_mocked_bindings(
    list_sources = function() registry,
    get_source = metadata,
    retrieve_source = function(source_id, refresh = FALSE, ...) {
      poly_layers[[source_id]]
    },
    .package = "ONgeoR"
  )
  server <- load_shiny_server()
  use_sequential_futures()

  shiny::testServer(server, {
    session$setInputs(
      base_layer = "src_poly",
      overlay_source = "tgt_poly",
      preview_btn = 1
    )
    expect_identical(wait_for_extended_task(preview_task, session), "success")

    session$setInputs(confirm_join_btn = 1)
    expect_identical(wait_for_extended_task(build_task, session), "success")
    expect_s3_class(cw_result$crosswalk, "data.frame")
    expect_s3_class(cw_result$overlay_sf, "sf")

    # The Shape archive is ready for a vector target with a joined table.
    expect_match(
      rendered_html(output$link_downloads_ui),
      "id=\"dl_cw_target\""
    )

    shapes_zip <- output$dl_cw_target
    expect_true(file.exists(shapes_zip))
    shapes_dir <- withr::local_tempdir()
    utils::unzip(shapes_zip, exdir = shapes_dir)
    back <- sf::st_read(file.path(shapes_dir, "target.gpkg"), layer = "target", quiet = TRUE)
    expect_s3_class(back, "sf")
    # Exactly one feature per target feature - catches a wrong join key.
    expect_equal(nrow(back), nrow(tgt_poly))
    expect_true("dominant_source_id" %in% names(back))
    expect_false(any(is.na(back$dominant_source_id)))
    # Target's own attributes survive alongside the merged ones.
    expect_true("TGT_ID" %in% names(back))
  })
})

test_that("Shapes download stays disabled for a raster target", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  # The merge itself refuses a raster target (no attribute table to merge).
  env <- load_shiny_app_env()
  raster <- terra::rast(
    nrows = 4, ncols = 4,
    xmin = -80, xmax = -79, ymin = 43, ymax = 44,
    crs = "EPSG:4326", vals = seq_len(16)
  )
  expect_null(env$merge_target_attributes(raster, crosswalk = tibble::tibble(x = 1)))

  testthat::local_mocked_bindings(
    list_sources = shiny_fixture_registry,
    get_source = shiny_fixture_metadata,
    .package = "ONgeoR"
  )
  server <- load_shiny_server()

  shiny::testServer(server, {
    session$setInputs(
      base_layer = "base_polygon",
      overlay_source = "overlay_point"
    )
    cw_result$overlay_sf <- raster
    cw_result$linked <- tibble::tibble(pm25 = 1:3)
    session$flushReact()

    html <- rendered_html(output$link_downloads_ui)
    # No ready (id) Shapes button: a raster target has nothing to merge onto.
    expect_false(grepl("id=\"dl_cw_target\"", html, fixed = TRUE))
    # ...while the linked.csv Table download is ready.
    expect_match(html, "id=\"dl_cw_csv\"", fixed = TRUE)
  })
})

test_that("downloads render as a uniform four-button two-column grid", {
  env <- load_shiny_app_env()
  items <- list(
    list(id = "dl_cw_csv", label = "Table", title = "target_tables.zip", ready = TRUE),
    list(id = "dl_cw_target", label = "Shape", title = "target_shapes.zip", ready = TRUE),
    list(id = "dl_cw_map", label = "Map", title = "map.html", ready = TRUE),
    list(id = "dl_cw_script", label = "Script", title = "reproduce.R", ready = FALSE)
  )
  html <- rendered_html(env$download_or_disabled(items))
  # Two-column grid wrapper and one col-6 cell per button.
  expect_match(html, "class=\"row g-1\"", fixed = TRUE)
  expect_equal(length(gregexpr("col-6", html, fixed = TRUE)[[1]]), 4L)
  # Four buttons in spec order: Table, Shape, Map, Script.
  pos <- function(s) regexpr(s, html, fixed = TRUE)
  labels <- c("Table", "Shape", "Map", "Script")
  positions <- vapply(labels, pos, integer(1))
  expect_true(all(positions > 0L))
  expect_identical(order(positions), 1:4)
  # Real filenames carried as tooltips.
  expect_match(html, "title=\"target_shapes.zip\"", fixed = TRUE)
  expect_match(html, "title=\"target_tables.zip\"", fixed = TRUE)
  expect_match(html, "title=\"map.html\"", fixed = TRUE)
  # Every cell uses the same visual treatment; availability only adds disabled.
  expect_equal(
    length(gregexpr("download-grid-button", html, fixed = TRUE)[[1]]),
    4L
  )
  expect_match(html, "id=\"dl_cw_target\"", fixed = TRUE)
  expect_match(html, "id=\"dl_cw_map\"", fixed = TRUE)
  expect_match(html, "id=\"dl_cw_csv\"", fixed = TRUE)
  expect_false(grepl("id=\"dl_cw_script\"", html, fixed = TRUE))
})
