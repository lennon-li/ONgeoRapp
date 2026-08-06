fixture_provenance <- function(x) {
  attr(x, "source_url") <- "https://example.test/ongeor-fixture"
  attr(x, "source_name") <- "ONgeoR deterministic fixture"
  attr(x, "retrieved_at") <- as.POSIXct("2026-07-18 00:00:00", tz = "UTC")
  x
}

fixture_polygons <- function() {
  square <- function(x) sf::st_polygon(list(rbind(
    c(x, 0), c(x + 1, 0), c(x + 1, 1), c(x, 1), c(x, 0)
  )))
  fixture_provenance(sf::st_sf(
    PHU_ID = paste0("P", 1:3),
    PHU_NAME_ENG = paste("Fixture Health Unit", 1:3),
    geometry = sf::st_sfc(square(0), square(1), square(2), crs = 4326)
  ))
}

fixture_points <- function() {
  # Points 1-3, 4-6, and 7-9 are strictly inside P1, P2, and P3;
  # point 10 is outside all fixture polygons.
  fixture_provenance(sf::st_as_sf(
    tibble::tibble(
      point_id = 1:10,
      lon = c(0.25, 0.50, 0.75, 1.25, 1.50, 1.75, 2.25, 2.50, 2.75, 3.50),
      lat = c(0.25, 0.50, 0.75, 0.25, 0.50, 0.75, 0.25, 0.50, 0.75, 0.50)
    ),
    coords = c("lon", "lat"), crs = 4326
  ))
}

fixture_overlap_layers <- function() {
  square <- function(x) sf::st_polygon(list(rbind(
    c(x, 0), c(x + 1, 0), c(x + 1, 1), c(x, 1), c(x, 0)
  )))
  rectangle <- function(x1, x2) sf::st_polygon(list(rbind(
    c(x1, 0), c(x2, 0), c(x2, 1), c(x1, 1), c(x1, 0)
  )))

  # Expected intersection shares: from F1 -> to T1 = 1; from F2 ->
  # to T1 = 0.25 and to T2 = 0.75 (all areas have height 1).
  list(
    from = fixture_provenance(sf::st_sf(
      from_id = c("F1", "F2"),
      from_name = c("From 1", "From 2"),
      geometry = sf::st_sfc(square(0), square(1), crs = 3347)
    )),
    to = fixture_provenance(sf::st_sf(
      to_id = c("T1", "T2"),
      to_name = c("To 1", "To 2"),
      geometry = sf::st_sfc(rectangle(0, 1.25), rectangle(1.25, 2), crs = 3347)
    ))
  )
}
