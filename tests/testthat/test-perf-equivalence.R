reference_collapse_crosswalk_best_match <- function(crosswalk) {
  n <- nrow(crosswalk)
  if (n == 0L) {
    return(crosswalk)
  }
  ids <- as.character(crosswalk$from_id)
  coverage <- if (!is.null(crosswalk$coverage)) {
    crosswalk$coverage
  } else {
    rep(NA_real_, n)
  }
  distance <- if (!is.null(crosswalk$match_distance_km)) {
    crosswalk$match_distance_km
  } else {
    rep(NA_real_, n)
  }
  unique_ids <- unique(ids)
  keep <- integer(length(unique_ids))
  for (u in seq_along(unique_ids)) {
    rows <- if (is.na(unique_ids[u])) which(is.na(ids)) else which(ids == unique_ids[u])
    cov <- coverage[rows]
    if (any(!is.na(cov))) {
      keep[u] <- rows[which(!is.na(cov))][which.max(cov[!is.na(cov)])]
    } else {
      dist <- distance[rows]
      if (any(!is.na(dist))) {
        keep[u] <- rows[which(!is.na(dist))][which.min(dist[!is.na(dist)])]
      } else {
        keep[u] <- rows[1L]
      }
    }
  }
  crosswalk[sort(keep), , drop = FALSE]
}

reference_aggregate_linked_by_target <- function(link_attrs, row_of_target,
                                                n_targets) {
  rows_for <- function(i) which(row_of_target == i)
  reduced <- lapply(link_attrs, function(column) {
    if (is.numeric(column)) {
      vapply(seq_len(n_targets), function(i) {
        values <- column[rows_for(i)]
        if (all(is.na(values))) NA_real_ else mean(values, na.rm = TRUE)
      }, numeric(1))
    } else {
      vapply(seq_len(n_targets), function(i) {
        values <- column[rows_for(i)]
        if (length(values) == 0L) NA_character_ else as.character(values[1L])
      }, character(1))
    }
  })
  out <- if (length(reduced) > 0L) {
    as.data.frame(reduced, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    data.frame(row.names = seq_len(n_targets))
  }
  out$linked_cells <- tabulate(row_of_target, nbins = n_targets)
  rownames(out) <- NULL
  out
}

reference_nearest_connectors <- function(source_sf, target_sf, pairs) {
  pairs <- pairs[!is.na(pairs$source_id) & !is.na(pairs$target_id), , drop = FALSE]
  if (nrow(pairs) == 0) {
    return(NULL)
  }
  source_id_col <- ONgeoR::layer_id_col(source_sf)
  target_id_col <- ONgeoR::layer_id_col(target_sf)
  source_idx <- match(pairs$source_id, as.character(source_sf[[source_id_col]]))
  target_idx <- match(pairs$target_id, as.character(target_sf[[target_id_col]]))
  keep <- !is.na(source_idx) & !is.na(target_idx)
  if (!any(keep)) {
    return(NULL)
  }
  source_geom <- sf::st_geometry(source_sf)[source_idx[keep]]
  target_geom <- sf::st_geometry(target_sf)[target_idx[keep]]
  lines <- sf::st_sfc(
    lapply(seq_along(source_geom), function(i) {
      sf::st_nearest_points(source_geom[i], target_geom[i], pairwise = TRUE)[[1]]
    }),
    crs = sf::st_crs(source_sf)
  )
  sf::st_sf(geometry = lines)
}

test_that("collapse_crosswalk_best_match is reference-equivalent", {
  env <- load_shiny_app_env()
  set.seed(20260809)
  crosswalk <- data.frame(
    from_id = c("coverage_tie", "coverage_tie", "distance", "distance",
      "all_na", "all_na", NA, NA, "coverage", "coverage"),
    coverage = c(0.8, 0.8, NA, NA, NA, NA, 0.4, 0.4, 0.1, 0.9),
    match_distance_km = c(9, 1, 3, 3, NA, NA, 8, 2, 6, 7),
    payload = seq_len(10),
    stringsAsFactors = FALSE
  )
  crosswalk <- crosswalk[sample(nrow(crosswalk)), , drop = FALSE]

  expect_equal(
    env$collapse_crosswalk_best_match(crosswalk),
    reference_collapse_crosswalk_best_match(crosswalk)
  )

  missing_metrics <- data.frame(
    from_id = sample(c("a", "a", "b", "b", NA, NA)),
    payload = sample(6),
    stringsAsFactors = FALSE
  )
  expect_equal(
    env$collapse_crosswalk_best_match(missing_metrics),
    reference_collapse_crosswalk_best_match(missing_metrics)
  )

  empty <- crosswalk[0, , drop = FALSE]
  expect_equal(env$collapse_crosswalk_best_match(empty), empty)
})

test_that("aggregate_linked_by_target is reference-equivalent", {
  env <- load_shiny_app_env()
  set.seed(20260809)
  row_of_target <- c(1L, 1L, 2L, 2L, NA_integer_, 4L)
  link_attrs <- data.frame(
    all_na_numeric = c(NA_real_, NA_real_, 10, 20, 30, 40),
    numeric_value = c(1, 3, NA, 7, 9, 11),
    label = c("first", "second", "third", "fourth", "unmatched", "sixth"),
    stringsAsFactors = FALSE
  )
  order <- sample(seq_along(row_of_target))
  row_of_target <- row_of_target[order]
  link_attrs <- link_attrs[order, , drop = FALSE]

  expect_equal(
    env$aggregate_linked_by_target(link_attrs, row_of_target, 5L),
    reference_aggregate_linked_by_target(link_attrs, row_of_target, 5L)
  )

  no_columns <- data.frame(row.names = seq_along(row_of_target))
  expect_equal(
    env$aggregate_linked_by_target(no_columns, row_of_target, 5L),
    reference_aggregate_linked_by_target(no_columns, row_of_target, 5L)
  )
})

test_that("nearest_connectors vectorization is reference-equivalent", {
  env <- load_shiny_app_env()
  source <- sf::st_as_sf(
    data.frame(id = c("s1", "s2"), x = c(-79.4, -79.2), y = c(43.7, 43.8)),
    coords = c("x", "y"), crs = 4326
  )
  target <- sf::st_as_sf(
    data.frame(id = c("t1", "t2"), x = c(-79.3, -79.1), y = c(43.75, 43.85)),
    coords = c("x", "y"), crs = 4326
  )
  pairs <- data.frame(
    source_id = c("s2", "s1", NA, "missing"),
    target_id = c("t1", "t2", "t2", "t1")
  )

  actual <- env$nearest_connectors(source, target, pairs)
  expected <- reference_nearest_connectors(source, target, pairs)

  expect_equal(nrow(actual), nrow(expected))
  expect_equal(sf::st_crs(actual), sf::st_crs(expected))
  expect_true(all(lengths(sf::st_equals(actual, expected)) == 1L))
})
