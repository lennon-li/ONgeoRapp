# The stakeholder report behind these tests: "after joining, the data table
# does not seem to contain all the fields. If the target has 5 attributes and
# the source has 10, I would expect the final joined target to contain at
# least 15 fields." It did not, and that was by construction - the Data tab
# rendered build_crosswalk()'s fixed provenance table, which carries neither
# layer's attributes.

test_that("merge_source_attributes appends the matched source feature's columns", {
  env <- load_shiny_app_env()

  target <- sf::st_sf(
    point_id = c("A", "B"),
    geometry = sf::st_sfc(sf::st_point(c(0.5, 0.5)), sf::st_point(c(1.5, 0.5)),
      crs = 4326)
  )
  # What merge_target_attributes() produces: the target plus the collapsed
  # crosswalk row, including to_id and to_id_col.
  merged <- target
  merged$to_id <- c("P1", "P2")
  merged$to_id_col <- "PHU_ID"

  source_sf <- fixture_polygons()

  out <- env$merge_source_attributes(merged, source_sf, data.frame(
    to_id = c("P1", "P2"), to_id_col = "PHU_ID", stringsAsFactors = FALSE
  ))

  expect_true("src_PHU_NAME_ENG" %in% names(out))
  expect_identical(
    out$src_PHU_NAME_ENG,
    c("Fixture Health Unit 1", "Fixture Health Unit 2")
  )
  # The key itself is already carried as to_id; repeating it adds nothing.
  expect_false("src_PHU_ID" %in% names(out))
})

test_that("merge_source_attributes reports an unmatched target as NA, not a dropped row", {
  env <- load_shiny_app_env()

  merged <- sf::st_sf(
    point_id = c("A", "B"),
    to_id = c("P1", "NOPE"),
    to_id_col = "PHU_ID",
    geometry = sf::st_sfc(sf::st_point(c(0.5, 0.5)), sf::st_point(c(9, 9)),
      crs = 4326)
  )

  out <- env$merge_source_attributes(merged, fixture_polygons(), data.frame(
    to_id = c("P1", "NOPE"), to_id_col = "PHU_ID", stringsAsFactors = FALSE
  ))

  expect_identical(nrow(out), 2L)
  expect_identical(out$src_PHU_NAME_ENG, c("Fixture Health Unit 1", NA))
})

test_that("merge_source_attributes leaves the layer alone when it cannot key", {
  env <- load_shiny_app_env()
  merged <- fixture_points()

  # No crosswalk, no to_id column, and a raster source: each must be a
  # pass-through rather than an error, because the Data tab calls this on
  # every run including the ones with nothing to add.
  expect_identical(env$merge_source_attributes(merged, fixture_polygons(), NULL), merged)
  expect_identical(
    env$merge_source_attributes(merged, fixture_polygons(), data.frame(x = 1)),
    merged
  )
  expect_null(env$merge_source_attributes(NULL, fixture_polygons(), NULL))
})

test_that("the Data tab shows the joined layer's attributes, not the crosswalk", {
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

    session$setInputs(build_btn = 1, confirm_join_btn = 1)
    expect_identical(wait_for_extended_task(build_task, session), "success")

    merged <- joined_target()
    expect_s3_class(merged, "sf")

    # The target's own attribute survives...
    expect_true("point_id" %in% names(merged))
    # ...the join provenance is there...
    expect_true("to_id" %in% names(merged))
    # ...and so is the SOURCE layer's attribute, which is the whole point.
    expect_true("src_PHU_NAME_ENG" %in% names(merged))

    # The old behaviour: the crosswalk alone never carried either layer's
    # attributes, so it could not have answered the report.
    expect_false("point_id" %in% names(cw_result$crosswalk))
    expect_false("PHU_NAME_ENG" %in% names(cw_result$crosswalk))
  })
})
