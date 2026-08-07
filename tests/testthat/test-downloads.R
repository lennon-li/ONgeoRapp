# Downloads built on merge_target_attributes(): the merged object behind the
# Shapes downloads must carry the target layer's own attributes, the
# attributes joined in from the source layer, and the join-process fields,
# and the Shapefile zip must ship a field-name map that matches what sf
# actually wrote to the .dbf.

downloads_crosswalk_fixture <- function() {
  tibble::tibble(
    from_id = c("1", "1", "2"),
    from_name = c("p1", "p1", "p2"),
    from_source = "overlay_layer",
    to_id = c("P1", "P2", "P1"),
    to_name = c("U1", "U2", "U1"),
    to_source = "base_layer",
    match_method = "within",
    match_distance_km = c(NA_real_, NA_real_, 0.12),
    coverage = c(0.3, 0.9, NA_real_),
    from_id_col = "point_id",
    to_id_col = "PHU_ID",
    source_url_from = "https://example.test",
    source_url_to = "https://example.test",
    retrieved_at = as.POSIXct("2026-08-06 00:00:00", tz = "UTC")
  )
}

test_that("merged download object carries target, source, and join attributes", {
  env <- load_shiny_app_env()
  target <- fixture_points()
  merged <- env$merge_target_attributes(
    target, crosswalk = downloads_crosswalk_fixture()
  )
  expect_s3_class(merged, "sf")
  # Exactly one row per target feature - catches a wrong join key.
  expect_equal(nrow(merged), nrow(target))

  # Every attribute the target layer already had survives the merge.
  target_attrs <- setdiff(names(target), attr(target, "sf_column"))
  expect_true(all(target_attrs %in% names(merged)))

  # Every attribute joined in from the source (to_*) layer is carried.
  source_attrs <- c("to_id", "to_name", "to_source", "source_url_to")
  expect_true(all(source_attrs %in% names(merged)))

  # The join-process fields ride along as well.
  join_fields <- c(
    "from_id", "from_name", "from_source", "from_id_col", "to_id_col",
    "match_method", "match_distance_km", "coverage",
    "source_url_from", "retrieved_at"
  )
  expect_true(all(join_fields %in% names(merged)))

  # Names alone are not enough: point 1 has two candidate pairs, and the
  # 0.9-coverage row (to P2 / U2) must win; point 3 is unmatched, keeps its
  # geometry, and gets NA source attributes.
  expect_equal(merged$to_name[merged$point_id == 1], "U2")
  expect_equal(merged$to_name[merged$point_id == 2], "U1")
  expect_equal(merged$match_distance_km[merged$point_id == 2], 0.12)
  expect_true(is.na(merged$to_name[merged$point_id == 3]))
  expect_true(is.na(merged$to_name[merged$point_id == 10]))
})

test_that("target_shapes.zip carries GeoPackage, shapefile sidecars, and a field-name map", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if(
    !nzchar(Sys.which("zip")) && !nzchar(Sys.getenv("R_ZIPCMD")),
    "zip executable not available"
  )
  drivers <- sf::st_drivers()
  skip_if(!("ESRI Shapefile" %in% drivers$name), "ESRI Shapefile driver not available")

  # The same overlapping polygon pair the target.gpkg round-trip test uses.
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
  env <- load_shiny_app_env()
  server <- env$server
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

    # Shape combines the GeoPackage and shapefile formats in one archive.
    expect_match(
      rendered_html(output$link_downloads_ui),
      "id=\"dl_cw_target\""
    )

    zip_file <- output$dl_cw_target
    expect_true(file.exists(zip_file))

    exdir <- tempfile("shp_unzip")
    dir.create(exdir)
    withr::defer(unlink(exdir, recursive = TRUE))
    utils::unzip(zip_file, exdir = exdir)
    entries <- list.files(exdir)
    expect_true(all(
      c("target.gpkg", "target.shp", "target.shx", "target.dbf", "target.prj",
        "field_names.csv") %in% entries
    ))

    field_names <- utils::read.csv(file.path(exdir, "field_names.csv"))
    back <- sf::st_read(file.path(exdir, "target.shp"), quiet = TRUE)
    expect_s3_class(back, "sf")
    expect_equal(nrow(back), nrow(tgt_poly))
    # Exactly one field row per attribute column, excluding the geometry.
    stored <- setdiff(names(back), attr(back, "sf_column"))
    expect_equal(nrow(field_names), length(stored))
    # The map records what sf actually wrote to the .dbf, in order, and can
    # therefore recover the real names without guessing.
    expect_equal(field_names$truncated, stored)
    expect_true(all(nchar(field_names$truncated) <= 10L))
    expect_equal(anyDuplicated(field_names$truncated), 0L)
    # Names longer than 10 characters (match_distance_km among them) must be
    # both present and truncated.
    expect_true(any(nchar(field_names$original) > 10L))
    expect_true("match_distance_km" %in% field_names$original)
    long <- nchar(field_names$original) > 10L
    expect_true(all(field_names$truncated[long] != field_names$original[long]))
  })
})

test_that("target_tables.zip carries results and pairs tables", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  layers <- shiny_fixture_layers()
  testthat::local_mocked_bindings(
    list_sources = shiny_fixture_registry,
    get_source = shiny_fixture_metadata,
    retrieve_source = function(source_id, refresh = FALSE, ...) layers[[source_id]],
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
    expect_identical(wait_for_extended_task(preview_task, session), "success")
    session$setInputs(confirm_join_btn = 1)
    expect_identical(wait_for_extended_task(build_task, session), "success")

    zip_file <- output$dl_cw_csv
    expect_true(file.exists(zip_file))
    expect_setequal(
      utils::unzip(zip_file, list = TRUE)$Name,
      c("mapping.csv", "pairs.csv")
    )
  })
})

test_that("zip_directory_to writes to an extensionless path", {
  # Regression gate for the 2026-08-06 browser failure. downloadHandler hands
  # its content function an EXTENSIONLESS tempfile, but utils::zip() appends
  # ".zip" unless the target already ends in it -- so zipping straight onto
  # that path wrote "<tmp>.zip", left "<tmp>" absent, and the browser download
  # failed while the unit test passed. The test passed because testServer
  # supplies a path matching the declared filename, which does end in ".zip";
  # it agreed with itself. This asserts the case the app actually hits.
  env <- load_shiny_app_env()
  zip_directory_to <- env$zip_directory_to

  staging <- withr::local_tempdir()
  writeLines("a", file.path(staging, "a.txt"))
  writeLines("b", file.path(staging, "b.txt"))

  dest <- withr::local_tempfile()            # no extension, as Shiny passes
  expect_false(grepl("\\.zip$", dest))

  zip_directory_to(staging, dest)

  expect_true(file.exists(dest))
  expect_gt(file.size(dest), 0)
  expect_false(file.exists(paste0(dest, ".zip")))
  expect_setequal(utils::unzip(dest, list = TRUE)$Name, c("a.txt", "b.txt"))

  # The working directory must survive the call.
  wd_before <- getwd()
  zip_directory_to(staging, withr::local_tempfile())
  expect_identical(getwd(), wd_before)
})
