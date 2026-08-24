# Two map defects from the same report: "the tiles are not always drawing
# properly...they come back empty sometimes", and "after previewing and
# joining, I switched to the data tab then back to the map and re-ran preview
# on map. The features (neither source nor target) showed up."

test_that("legend group names carry the layer's own name", {
  env <- load_shiny_app_env()
  testthat::local_mocked_bindings(
    get_source = shiny_fixture_metadata,
    .package = "ONgeoR"
  )

  expect_identical(
    env$preview_group_labels(c("base_polygon", "overlay_point")),
    list(base = "Source layer - Base polygon",
      overlay = "Target layer - Overlay point")
  )
})

test_that("group names fall back to the bare slot before a preview", {
  env <- load_shiny_app_env()
  # Both the pre-preview map (furniture only) and an unresolvable id must
  # still produce a usable group name rather than "Source layer - NA".
  expect_identical(
    env$preview_group_labels(NULL),
    list(base = "Source layer", overlay = "Target layer")
  )
  expect_identical(env$layer_group_label("Source layer", "no_such_source"),
    "Source layer")
})

test_that("a repeat preview of the same pair still redraws the map", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  layers <- shiny_fixture_layers()
  testthat::local_mocked_bindings(
    list_sources = shiny_fixture_registry,
    get_source = shiny_fixture_metadata,
    retrieve_source = function(source_id, refresh = FALSE, ...) layers[[source_id]],
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
    first_token <- cw_result$render_token
    first_sf <- cw_result$base_sf

    session$setInputs(build_btn = 1, confirm_join_btn = 1)
    expect_identical(wait_for_extended_task(build_task, session), "success")

    session$setInputs(preview_btn = 2)
    expect_identical(wait_for_extended_task(preview_task, session), "success")

    # The layers themselves are written back UNCHANGED - an sf layer
    # round-trips through the future bit-identically - and shiny's
    # reactiveValues dedupes on identical(), so without a token nothing the
    # map reads would have invalidated and the browser would have been sent
    # no payload at all.
    expect_identical(cw_result$base_sf, first_sf)
    expect_gt(cw_result$render_token, first_token)
    expect_s3_class(link_map(), "leaflet")
  })
})

test_that("a transient NULL picker does not wipe the previewed layers", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  layers <- shiny_fixture_layers()
  testthat::local_mocked_bindings(
    list_sources = shiny_fixture_registry,
    get_source = shiny_fixture_metadata,
    retrieve_source = function(source_id, refresh = FALSE, ...) layers[[source_id]],
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
    expect_false(is.null(cw_result$base_sf))

    # The echo of an updateSelectInput() can momentarily deliver a NULL
    # picker. Treated as a real change it discards both previewed layers and
    # bumps the generation counter, and the map redraws with tiles and
    # furniture and no features - which is the reported symptom.
    session$setInputs(base_layer = NULL)
    session$flushReact()

    expect_false(is.null(cw_result$base_sf))
    expect_false(is.null(cw_result$overlay_sf))
    expect_false(is.null(cw_result$previewed))
  })
})
