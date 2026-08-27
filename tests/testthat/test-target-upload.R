# The target slot's "Use my own file" was a non-functional placeholder that
# also fought with the "Upload postal codes" dropdown selection. These cover
# the reader's point-only contract and the mutual exclusion.

write_temp_layer <- function(x, ext = "geojson") {
  path <- withr::local_tempfile(fileext = paste0(".", ext),
    .local_envir = parent.frame())
  sf::st_write(x, path, quiet = TRUE, delete_dsn = TRUE)
  path
}

test_that("read_uploaded_layer accepts a point layer and returns EPSG:4326", {
  env <- load_shiny_app_env()
  path <- write_temp_layer(fixture_points())

  layer <- env$read_uploaded_layer("points.geojson", path)

  expect_s3_class(layer, "sf")
  expect_identical(sf::st_crs(layer)$epsg, 4326L)
  expect_true(all(as.character(sf::st_geometry_type(layer)) == "POINT"))
  expect_identical(nrow(layer), 10L)
})

test_that("read_uploaded_layer reprojects rather than assuming 4326", {
  env <- load_shiny_app_env()
  projected <- sf::st_transform(fixture_points(), 3347)
  path <- write_temp_layer(projected, "gpkg")

  layer <- env$read_uploaded_layer("points.gpkg", path)

  expect_identical(sf::st_crs(layer)$epsg, 4326L)
})

test_that("read_uploaded_layer rejects a polygon target, naming the geometry found", {
  env <- load_shiny_app_env()
  path <- write_temp_layer(fixture_polygons())

  expect_error(
    env$read_uploaded_layer("shapes.geojson", path),
    "must be points.*POLYGON"
  )
})

test_that("read_uploaded_layer rejects an unsupported extension", {
  env <- load_shiny_app_env()
  path <- withr::local_tempfile(fileext = ".txt")
  writeLines("not a layer", path)

  expect_error(
    env$read_uploaded_layer("notes.txt", path),
    "Unsupported file type '.txt'"
  )
})

test_that("read_uploaded_layer reads a zipped shapefile", {
  skip_if_not_installed("zip")
  env <- load_shiny_app_env()

  dir <- withr::local_tempdir()
  sf::st_write(fixture_points(), file.path(dir, "pts.shp"), quiet = TRUE)
  archive <- withr::local_tempfile(fileext = ".zip")
  zip::zip(archive, files = list.files(dir), root = dir)

  layer <- env$read_uploaded_layer("pts.zip", archive)
  expect_identical(nrow(layer), 10L)
})

test_that("a zip with no shapefile fails with a specific message", {
  skip_if_not_installed("zip")
  env <- load_shiny_app_env()

  dir <- withr::local_tempdir()
  writeLines("nope", file.path(dir, "readme.txt"))
  archive <- withr::local_tempfile(fileext = ".zip")
  zip::zip(archive, files = list.files(dir), root = dir)

  expect_error(env$read_uploaded_layer("bundle.zip", archive), "no .shp file")
})

test_that("the postal dropdown wins over a left-over own-file tick", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  testthat::local_mocked_bindings(
    list_sources = shiny_fixture_registry,
    get_source = shiny_fixture_metadata,
    .package = "ONgeoR"
  )
  server <- load_shiny_server()

  shiny::testServer(server, {
    session$setInputs(base_layer = "base_polygon", overlay_source = "overlay_point")
    session$setInputs(overlay_upload_own = TRUE)
    expect_true(own_upload_active())
    expect_identical(effective_overlay_id(), "own_upload")

    # Selecting the postal dropdown must not leave BOTH routes claiming the
    # target slot - the checkbox is hidden, and a hidden conditionalPanel
    # keeps whatever value it had.
    session$setInputs(overlay_source = "postal_upload")
    expect_false(own_upload_active())
    expect_identical(effective_overlay_id(), "postal_upload")

    # Selecting the postal route must also render the file chooser. A missing
    # postal_upload_ui output leaves the selection looking valid while making
    # it impossible to provide the input file.
    postal_ui <- rendered_html(output$postal_upload_ui)
    expect_match(postal_ui, 'id="postal_file"', fixed = TRUE)
    expect_match(postal_ui, "Browse", fixed = TRUE)
  })
})

test_that("an uploaded target is named and typed as a point layer", {
  env <- load_shiny_app_env()
  expect_identical(env$geom_kind("own_upload"), "point")
  expect_identical(env$layer_display_name("own_upload"), "Uploaded point layer")
  expect_identical(
    env$source_geom_label("own_upload"),
    list(text = "Point", class = "geo-point")
  )
})
