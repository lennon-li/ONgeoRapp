square_ring <- function(origin, size) {
  pts <- rbind(origin, origin + c(size, 0), origin + c(size, size),
               origin + c(0, size), origin)
  dimnames(pts) <- NULL
  pts
}

circle_ring <- function(center, radius, n = 64L) {
  theta <- seq(0, 2 * pi, length.out = n)
  pts <- cbind(center[1] + radius * cos(theta), center[2] + radius * sin(theta))
  pts <- rbind(pts, pts[1L, , drop = FALSE])
  dimnames(pts) <- NULL
  pts
}

simplify_env <- function() {
  load_shiny_app_env()
}

test_that("POLYGON layer with a hole simplifies without error", {
  env <- simplify_env()
  hole <- list(
    square_ring(c(0, 0), 10000),
    square_ring(c(2000, 2000), 2000)
  )
  far <- list(square_ring(c(1e5, 1e5), 10000))
  x <- sf::st_sf(
    id = c("A", "B"),
    geometry = sf::st_sfc(sf::st_polygon(hole), sf::st_polygon(far), crs = 3161)
  )
  x <- sf::st_transform(x, 4326)

  out <- env$simplify_for_export(x)

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 2L)
  expect_true(all(sf::st_geometry_type(out) == "MULTIPOLYGON"))
})

test_that("mixed POLYGON and MULTIPOLYGON layer is actually simplified", {
  env <- simplify_env()
  poly <- sf::st_polygon(list(circle_ring(c(0, 0), 5000)))
  mpoly <- sf::st_multipolygon(list(
    list(circle_ring(c(90000, 90000), 5000)),
    list(circle_ring(c(110000, 110000), 5000))
  ))
  x <- sf::st_sf(
    id = c("P", "MP"),
    geometry = sf::st_sfc(poly, mpoly, crs = 3161)
  )
  expect_setequal(
    as.character(sf::st_geometry_type(sf::st_sfc(poly, mpoly))),
    c("POLYGON", "MULTIPOLYGON")
  )

  out <- env$simplify_for_export(x)

  expect_equal(nrow(out), 2L)
  n_before <- nrow(sf::st_coordinates(sf::st_cast(sf::st_geometry(x), "MULTIPOLYGON")))
  n_after <- nrow(sf::st_coordinates(sf::st_cast(sf::st_geometry(out), "MULTIPOLYGON")))
  expect_lt(n_after, n_before)
})

test_that("POINT layer passes through unchanged", {
  env <- simplify_env()
  pts <- sf::st_sf(
    id = c("a", "b"),
    geometry = sf::st_sfc(sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), crs = 4326)
  )

  out <- env$simplify_for_export(pts)

  expect_identical(out, pts)
})

test_that("every input row survives simplification", {
  env <- simplify_env()
  tiny <- list(
    square_ring(c(0, 0), 800),
    square_ring(c(200, 200), 200)
  )
  regular <- list(square_ring(c(50000, 50000), 9000))
  x <- sf::st_sf(
    id = c("tiny_with_hole", "regular"),
    geometry = sf::st_sfc(sf::st_polygon(tiny), sf::st_polygon(regular), crs = 3161)
  )

  out <- env$simplify_for_export(sf::st_transform(x, 4326))

  expect_equal(nrow(out), nrow(x))
  expect_equal(out$id, x$id)
})
