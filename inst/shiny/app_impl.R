library(shiny)
library(bslib)
library(leaflet)
library(promises)
library(future)
library(DT)

# ExtendedTask (used for all async flows below) needs shiny >= 1.8.0.
if (utils::packageVersion("shiny") < "1.8.0") {
  stop(
    "The ONgeoR Shiny app requires shiny >= 1.8.0 (for ExtendedTask); ",
    "installed: ", utils::packageVersion("shiny")
  )
}

# plan() returns the previous plan; restore it when the app stops so a
# multisession plan does not leak into the caller's R session.
.previous_future_plan <- future::plan(future::multisession)
shiny::onStop(function() future::plan(.previous_future_plan))

`%||%` <- function(a, b) if (is.null(a)) b else a

# A SpatRaster is an external pointer to a C++ object, and that pointer does
# NOT survive being returned from a background future worker: it arrives NULL,
# and the first later use dies with "NULL value passed as symbol address".
# sf layers are plain R data and cross the boundary fine, which is why only
# raster pairings were affected. terra's supported way to move a raster
# between processes is to pack it with wrap() before it leaves the worker and
# restore it with unwrap() on arrival. Apply these at every future boundary.
pack_spatial <- function(x) {
  if (inherits(x, "SpatRaster")) terra::wrap(x) else x
}
unpack_spatial <- function(x) {
  if (inherits(x, "PackedSpatRaster")) terra::unwrap(x) else x
}

source_choices <- function() {
  sources <- ONgeoR::list_sources()
  stats::setNames(sources$source_id, source_choice_labels(sources))
}

# Maps a source's registry geography_type to the display-label type suffix.
source_type_suffix <- function(geography_type) {
  switch(geography_type,
    boundary = "Polygon",
    facility = "Point",
    raster   = "Raster",
    geography_type)
}

# Appends "(Type)" to each source's display name, e.g.
# "MOH Service Location (Point)". Used by both the flat and grouped choice
# builders so labels stay consistent everywhere a source picker appears.
source_choice_labels <- function(sources) {
  sprintf("%s (%s)", sources$name, vapply(sources$geography_type, source_type_suffix, character(1)))
}

# Grouped-choices form for use with selectInput's optgroup support:
# list("Polygons" = c(label = id, ...), "Points" = c(...), "Rasters" = c(...)).
# The source picker accepts only boundary and raster layers; facilities belong
# in the point-first target picker instead.
source_choices_grouped <- function() {
  sources <- ONgeoR::list_sources()
  sources <- sources[sources$geography_type %in% c("boundary", "raster"), ]
  labels <- source_choice_labels(sources)
  values <- stats::setNames(sources$source_id, labels)

  group_of <- vapply(sources$geography_type, function(gt) {
    switch(gt,
      boundary = "Polygons",
      raster = "Rasters")
  }, character(1))

  group_order <- c("Polygons", "Rasters")
  groups <- lapply(group_order, function(g) values[group_of == g])
  names(groups) <- group_order
  groups[lengths(groups) > 0]
}

target_choices_grouped <- function(include_open_postal = TRUE) {
  sources <- ONgeoR::list_sources()
  sources <- sources[sources$geography_type == "facility", ]
  if (!isTRUE(include_open_postal)) {
    sources <- sources[sources$source_id != "postal_points", , drop = FALSE]
  }
  labels <- source_choice_labels(sources)
  points <- stats::setNames(sources$source_id, labels)
  list("Points" = c(points, "Upload postal codes" = "postal_upload"))
}

# `overlay_source` remains the internal layer id used throughout the app. The
# mode is a separate UI concern so an uploaded file can never compete with a
# registry selection in the same target slot.
target_input_mode <- function(input) {
  mode <- input$overlay_target_mode
  if (!is.null(mode) && nzchar(mode)) return(mode)
  if (isTRUE(input$overlay_upload_own)) return("own_file")
  if (identical(input$overlay_source, "postal_upload")) return("postal_upload")
  if (identical(input$overlay_source, "postal_points")) return("open_postal")
  "registry"
}

target_layer_display_name <- function(target_id, own_file = NULL) {
  if (is.null(target_id) || !nzchar(target_id)) return("Target layer")
  if (identical(target_id, "postal_upload")) return("Uploaded postal codes")
  if (identical(target_id, "custom_target")) {
    if (!is.null(own_file) && nzchar(own_file$name %||% "")) {
      return(own_file$name)
    }
    return("Uploaded target file")
  }
  tryCatch(
    ONgeoR::get_source(target_id)$name,
    error = function(e) target_id
  )
}

source_layer_display_name <- function(source_id) {
  if (is.null(source_id) || !nzchar(source_id)) return("Source layer")
  if (identical(source_id, "postal_upload")) return("Uploaded postal codes")
  if (identical(source_id, "custom_target")) return("Uploaded target file")
  tryCatch(
    ONgeoR::get_source(source_id)$name,
    error = function(e) source_id
  )
}

# Removes `exclude_id` from whichever group of a grouped-choices list
# contains it, then drops any group left empty - used by the mutual-exclusion
# observers so the two source pickers can never hold the same value while
# still presenting grouped (optgroup) choices.
remove_choice_grouped <- function(groups, exclude_id) {
  groups <- lapply(groups, function(g) g[g != exclude_id])
  groups[lengths(groups) > 0]
}

# First value in a grouped-choices list, in group order - used to pick a
# fallback selection when the previously selected id has been excluded.
first_choice_grouped <- function(groups) {
  for (g in groups) {
    if (length(g) > 0) return(g[[1]])
  }
  NULL
}

# Maps a source's registry geography_type to a small geometry kind token used
# to drive the per-layer style controls and the relationship line.
geom_kind <- function(source_id) {
  if (source_id %in% c("postal_upload", "postal_points", "custom_target")) {
    return("point")
  }
  switch(ONgeoR::get_source(source_id)$geography_type,
    boundary = "polygon",
    facility = "point",
    raster   = "raster",
    "polygon")
}

# Same kind token, derived from an already-retrieved layer object, so the style
# read for the map always matches the geometry that will actually be drawn.
layer_geom <- function(layer) {
  if (inherits(layer, "SpatRaster")) {
    return("raster")
  }
  geometry_types <- unique(as.character(sf::st_geometry_type(layer)))
  if (all(geometry_types %in% c("POINT", "MULTIPOINT"))) "point" else "polygon"
}

# Colored badge descriptor for a source's registry geography_type.
source_geom_label <- function(source_id) {
  if (source_id %in% c("postal_upload", "postal_points", "custom_target")) {
    return(list(text = "Point", class = "geo-point"))
  }
  gt <- ONgeoR::get_source(source_id)$geography_type
  switch(gt,
    boundary = list(text = "Polygon", class = "geo-polygon"),
    facility = list(text = "Point",   class = "geo-point"),
    raster   = list(text = "Raster",  class = "geo-raster"),
    list(text = gt, class = "geo-other"))
}

geo_badge <- function(source_id) {
  lbl <- source_geom_label(source_id)
  tags$span(class = paste("geo-badge", lbl$class), lbl$text)
}

# Full explanatory text for how a given pair of geometry kinds is matched.
# Shown in the Join confirmation modal (see join_confirm_modal); preview no
# longer raises a modal at all. `kinds` is the unsorted
# c(source_kind, target_kind) pair.
pairing_explanation_text <- function(kinds) {
  if ("raster" %in% kinds) {
    paste("Raster linking samples cell values - no match rule to choose.",
      "The output is a linked values table, not a crosswalk.")
  } else if (all(kinds == "point")) {
    paste("Both layers are points. Each target point is matched to its",
      "single nearest source point; the geometry pair alone selects this",
      "nearest match, with no rule to choose.")
  } else if ("point" %in% kinds && "polygon" %in% kinds) {
    "One layer is point facilities; each point is matched to the boundary it falls inside."
  } else {
    paste("Both layers are boundaries. Every overlapping pair is kept, with",
      "the share of each target covered and the share of each source",
      "falling inside; the geometry pair alone selects this intersection,",
      "with no rule to choose.")
  }
}

# build_crosswalk() always returns this fixed column set (see R/crosswalk.R):
# from_id, from_name, from_source, to_id, to_name, to_source, match_method,
# match_distance_km, coverage, from_id_col, to_id_col, source_url_from,
# source_url_to, retrieved_at. Naming them here keeps the quoted result width
# honest instead of guessing.
crosswalk_result_columns <- 14L

# The intersection and nearest target-level tables carry every attribute of
# both layers (prefixed src_/tgt_), so their width depends on the layers being
# joined rather than being a fixed constant. summarise_by_target() emits 13
# fixed columns, one src_*/tgt_* column per attribute of each layer, and 4
# provenance columns; the build_crosswalk path keeps its fixed schema above.
result_column_width <- function(source_sf, target_sf, kinds) {
  if ("raster" %in% kinds) {
    return(NA_integer_)
  }
  n_attr <- function(layer) max(ncol(layer) - 1L, 0L)
  if (all(kinds == "polygon") || all(kinds == "point")) {
    return(17L + n_attr(source_sf) + n_attr(target_sf))
  }
  crosswalk_result_columns + n_attr(source_sf) + n_attr(target_sf)
}

# Human-readable "N features x M attributes" for a retrieved layer. Attributes
# excludes the geometry column, which is what a user counts as a column in the
# downloaded CSV.
layer_dimensions <- function(layer) {
  if (is.null(layer)) {
    return("not loaded")
  }
  if (inherits(layer, "SpatRaster")) {
    return(sprintf("raster, %s x %s cells, %s layer(s)",
      format(terra::nrow(layer), big.mark = ","),
      format(terra::ncol(layer), big.mark = ","),
      terra::nlyr(layer)))
  }
  n_attr <- max(ncol(layer) - 1L, 0L)
  sprintf("%s features x %s attributes",
    format(nrow(layer), big.mark = ","), format(n_attr, big.mark = ","))
}

# Confirmation shown when the user clicks Join, replacing the modal that used
# to fire on Preview. Join is gated on a successful preview of the current
# pair, so both layers are already retrieved and their real names and
# dimensions can be quoted here rather than described in the abstract.
join_confirm_modal <- function(source_id, target_id, source_sf, target_sf,
                               kinds) {
  source_name <- source_layer_display_name(source_id)
  target_name <- target_layer_display_name(target_id)
  is_raster <- inherits(target_sf, "SpatRaster") ||
    inherits(source_sf, "SpatRaster")
  n_target <- if (inherits(target_sf, "SpatRaster")) NA_integer_ else nrow(target_sf)

  result_dim <- if (is_raster) {
    paste("A linked values table sampled from the raster - one row per",
      "sampled feature. Not a fixed-width crosswalk.")
  } else {
    sprintf("%s rows x %s columns - one row per target feature.",
      format(n_target, big.mark = ","),
      result_column_width(source_sf, target_sf, kinds))
  }

  labelled <- function(role, name, layer) {
    tags$li(
      tags$strong(paste0(role, ": ")), name,
      tags$span(class = "text-muted", paste0(" (", layer_dimensions(layer), ")"))
    )
  }

  modalDialog(
    title = NULL,
    tags$div(class = "info-modal",
      tags$div(class = "info-modal-header",
        tags$img(src = "logo.png", class = "info-modal-logo"),
        tags$span(class = "info-modal-icon", HTML("&#8505;")),
        tags$h4("Spatial join")
      ),
      tags$ul(
        labelled("Source layer", source_name, source_sf),
        labelled("Target layer", target_name, target_sf)
      ),
      tags$p(sprintf(
        paste("Each feature in %s (target) will be matched against the %s",
          "(source) layer, and the source layer's identifying attributes",
          "added to it."),
        target_name, source_name
      )),
      tags$p(tags$strong("Result: "), result_dim),
      tags$p(class = "text-muted", pairing_explanation_text(kinds)),
      tags$p(tags$strong("Run this join?"))
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("confirm_join_btn", "Run join", class = "btn-primary")
    ),
    easyClose = FALSE
  )
}

# One-line plain-language description of how two geometry kinds relate.
relationship_text <- function(a, b) {
  kinds <- c(a, b)
  if (all(kinds == "polygon")) {
    "Polygon overlap"
  } else if ("raster" %in% kinds && "polygon" %in% kinds) {
    "Raster-to-boundary"
  } else if ("raster" %in% kinds && "point" %in% kinds) {
    "Point-to-raster sampling"
  } else if (all(kinds == "raster")) {
    "Raster-to-raster"
  } else if ("point" %in% kinds && "polygon" %in% kinds) {
    "Point-in-boundary containment"
  } else {
    "Point-to-point"
  }
}

# Owner-approved combination matrix. Rendered verbatim in the in-app "?" help
# modal and mirrored in the crosswalks vignette + build_crosswalk()/link()
# roxygen so all three places tell the same story. Rows are built from
# link_matrix_df() (the single source of truth) rather than literals. ASCII only.
link_matrix_table <- function() {
  matrix_df <- ONgeoR:::link_matrix_df()
  rows <- lapply(seq_len(nrow(matrix_df)), function(i) {
    tags$tr(
      tags$td(sprintf("%s x %s", matrix_df$source_kind[i], matrix_df$target_kind[i])),
      tags$td(matrix_df$mode[i]),
      tags$td(matrix_df$what_it_does[i]),
      tags$td(matrix_df$output[i])
    )
  })
  tagList(
    tags$table(class = "link-matrix",
      tags$thead(tags$tr(
        tags$th("Layer types"), tags$th("Mode"),
        tags$th("What linking does"), tags$th("Output")
      )),
      tags$tbody(rows)
    ),
    tags$p(class = "text-muted",
      paste("Linking never creates or emits geometry - overlap areas are",
        "internal arithmetic only. The geometry pair alone decides the",
        "operation; there is no match rule to choose."))
  )
}

color_choices <- c(
  "Blue" = "#2a78d6", "Green" = "#1baf7a", "Orange" = "#eb6834",
  "Red" = "#e34948", "Purple" = "#4a3aa7", "Black" = "#1a1a1a",
  "Gray" = "#898781"
)

# basemap_groups defines the display order for the native Leaflet control.
# "None" intentionally has no associated tile layer.
basemap_groups <- c(
  "Light", "Dark", "OpenStreetMap", "Satellite",
  "Topographic", "Streets", "Voyager", "None"
)

# Emits the geometry-specific style controls for a single layer slot. `prefix`
# namespaces the input IDs (base/overlay/src/tgt); `geom` selects which set of
# controls to show; `accent` (a color from color_choices) is the default for
# that layer slot's primary color input(s), so a slot's map style starts out
# matching its slot-accent color (blue for base/src, green for overlay/tgt).
# Rendered dynamically so switching a source's geometry swaps its controls.
layer_style_controls <- function(prefix, geom, accent = "#2a78d6") {
  if (identical(geom, "raster")) {
    tagList(
      # Values must be spelled exactly as leaflet::colorNumeric() expects:
      # viridis palettes are lowercase ("viridis"/"magma"), while RColorBrewer
      # names are capitalized ("Blues"). A wrong case does NOT fail when the
      # palette function is built - only later, when addRasterImage() applies
      # it - so the map silently renders empty. Labels stay title-case.
      selectInput(paste0(prefix, "_raster_palette"), "Palette",
        choices = c("Viridis" = "viridis", "Magma" = "magma", "Blues" = "Blues"),
        selected = "viridis"),
      sliderInput(paste0(prefix, "_raster_opacity"), "Layer opacity",
        min = 0, max = 1, value = 0.8, step = 0.05, ticks = FALSE)
    )
  } else if (identical(geom, "point")) {
    tagList(
      selectInput(paste0(prefix, "_point_color"), "Point color",
        choices = color_choices, selected = accent),
      sliderInput(paste0(prefix, "_point_size"), "Point size",
        min = 2, max = 12, value = 5, step = 1, ticks = FALSE),
      selectInput(paste0(prefix, "_point_shape"), "Point shape",
        choices = c("Circle" = "circle", "Square" = "square"), selected = "circle")
    )
  } else {
    tagList(
      selectInput(paste0(prefix, "_line_color"), "Boundary line color",
        choices = color_choices, selected = accent),
      selectInput(paste0(prefix, "_fill_color"), "Boundary fill color",
        choices = color_choices, selected = accent),
      sliderInput(paste0(prefix, "_fill_opacity"), "Fill opacity",
        min = 0, max = 1, value = 0.25, step = 0.05, ticks = FALSE)
    )
  }
}

# Reads back the style list for one layer slot, keyed on the layer's actual
# geometry. Every field falls back to a default so the map never fails when an
# input has not been rendered yet (e.g. a picker changed after a build).
# `accent` mirrors the default passed to layer_style_controls() for the same
# slot, so the fallback color agrees with what the (not-yet-rendered) control
# would have defaulted to.
read_layer_style <- function(input, prefix, geom, accent = "#2a78d6") {
  g <- function(suffix) input[[paste0(prefix, "_", suffix)]]
  if (identical(geom, "raster")) {
    list(
      raster_palette = g("raster_palette") %||% "viridis",
      raster_opacity = g("raster_opacity") %||% 0.8
    )
  } else if (identical(geom, "point")) {
    list(
      point_color = g("point_color") %||% accent,
      point_size = g("point_size") %||% 5,
      point_shape = g("point_shape") %||% "circle"
    )
  } else {
    list(
      line_color = g("line_color") %||% accent,
      fill_color = g("fill_color") %||% accent,
      fill_opacity = g("fill_opacity") %||% 0.25
    )
  }
}

detect_postal_column <- function(df) {
  postal_pattern <- "^[A-Z][0-9][A-Z] [0-9][A-Z][0-9]$"
  name_pattern <- "(?i)^(postal|postal_?code|pc|post_?code|zip)$"
  scores <- vapply(names(df), function(col) {
    vals <- as.character(df[[col]])
    vals <- vals[!is.na(vals) & nzchar(vals)]
    if (length(vals) == 0L) return(0)
    normalized <- ONgeoR::normalize_postal_code(vals)
    sum(grepl(postal_pattern, normalized)) / length(vals)
  }, numeric(1))
  if (length(scores) == 0L || max(scores) < 0.8) {
    return(list(column = NULL, score = if (length(scores) > 0L) max(scores) else 0))
  }
  best_score <- max(scores)
  candidates <- names(scores)[scores == best_score]
  if (length(candidates) == 1L) {
    return(list(column = candidates, score = best_score))
  }
  name_match <- grepl(name_pattern, candidates, perl = TRUE)
  if (any(name_match)) {
    return(list(column = candidates[which(name_match)[1]], score = best_score))
  }
  list(column = candidates[1], score = best_score)
}

read_uploaded_file <- function(name, datapath) {
  ext <- tolower(tools::file_ext(name))
  switch(ext,
    csv  = utils::read.csv(datapath, stringsAsFactors = FALSE, check.names = FALSE),
    xlsx = as.data.frame(readxl::read_excel(datapath)),
    xls  = as.data.frame(readxl::read_excel(datapath)),
    NULL)
}

# Read and validate a user-supplied target. Custom targets are deliberately
# narrower than registry targets: only non-empty point vectors with a CRS are
# safe to pass to the point-first target workflow. The returned list keeps
# validation errors available to the UI instead of turning an upload failure
# into an opaque async-task error.
read_uploaded_point_vector <- function(name, datapath) {
  ext <- tolower(tools::file_ext(name))
  raster_exts <- c("tif", "tiff", "img", "grd", "nc")
  if (ext %in% raster_exts) {
    return(list(
      sf = NULL,
      error = "Raster files are not supported for a custom target. Upload a point vector file."
    ))
  }
  if (!ext %in% c("geojson", "json", "gpkg", "shp", "zip")) {
    return(list(
      sf = NULL,
      error = "Unsupported target file type. Upload GeoJSON, GeoPackage, or a zipped shapefile containing points."
    ))
  }

  temp_dir <- NULL
  path <- datapath
  if (identical(ext, "zip")) {
    temp_dir <- tempfile("ongeor_target_")
    dir.create(temp_dir)
    on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = TRUE)
    extracted <- tryCatch(
      utils::unzip(datapath, exdir = temp_dir),
      error = function(e) e
    )
    if (inherits(extracted, "error")) {
      return(list(sf = NULL, error = paste("Could not read the uploaded ZIP:", conditionMessage(extracted))))
    }
    candidates <- list.files(
      temp_dir, recursive = TRUE, full.names = TRUE,
      pattern = "\\.(geojson|json|gpkg|shp)$", ignore.case = TRUE
    )
    if (length(candidates) == 0L) {
      return(list(
        sf = NULL,
        error = "The ZIP contains no supported point-vector file. Include a GeoJSON, GeoPackage, or shapefile."
      ))
    }
    path <- candidates[[1]]
  }

  layer <- tryCatch(
    sf::st_read(path, quiet = TRUE),
    error = function(e) e
  )
  if (inherits(layer, "error")) {
    return(list(sf = NULL, error = paste("Could not read the target file:", conditionMessage(layer))))
  }
  if (!inherits(layer, "sf")) {
    return(list(sf = NULL, error = "The uploaded file is not a vector layer."))
  }
  if (nrow(layer) == 0L) {
    return(list(sf = NULL, error = "The uploaded target contains no features."))
  }
  geometry_types <- unique(as.character(sf::st_geometry_type(layer)))
  if (!all(geometry_types %in% c("POINT", "MULTIPOINT"))) {
    return(list(
      sf = NULL,
      error = "Custom targets must contain point features only; polygons and other geometry types are not supported."
    ))
  }
  empty <- sf::st_is_empty(layer)
  if (any(empty)) {
    return(list(sf = NULL, error = "The uploaded target contains empty point geometries."))
  }
  if (is.na(sf::st_crs(layer))) {
    return(list(
      sf = NULL,
      error = "The uploaded target has no coordinate reference system. Assign a CRS and upload it again."
    ))
  }
  list(sf = layer, error = NULL)
}

# Renders real (clickable) downloadButtons when an item's `ready` field is
# TRUE, otherwise visually-matching but non-functional disabled buttons - so
# each sidebar download is only ever clickable once the map/data it points to
# exists. Readiness is per-item (item$ready) so, e.g., map.html can be ready
# before the result tables. Buttons sit in a two-column grid (row g-1 / col-6)
# with one shared treatment; unavailable items retain their tooltip and differ
# only by their disabled state.
# Zip everything in `dir` and place the archive at EXACTLY `dest`.
#
# utils::zip() appends ".zip" whenever the target does not already end in
# it, and downloadHandler hands its content function an EXTENSIONLESS
# tempfile. Zipping straight onto that path therefore writes "<tmp>.zip"
# and leaves "<tmp>" absent, so Shiny serves nothing and the browser
# download fails -- observed 2026-08-06 in headless Chrome. The unit test
# missed it because testServer supplies a path matching the declared
# filename, which does end in .zip, so the test agreed with itself.
#
# Kept as a named helper rather than inline so the extensionless case can
# be asserted directly in tests.
zip_directory_to <- function(dir, dest) {
  zip_path <- tempfile(fileext = ".zip")
  old_wd <- setwd(dir)
  on.exit(setwd(old_wd), add = TRUE, after = FALSE)
  utils::zip(zip_path, files = list.files(dir))
  setwd(old_wd)
  if (!file.exists(zip_path)) {
    rlang::abort("Failed to build the shapefile archive.")
  }
  file.copy(zip_path, dest, overwrite = TRUE)
  invisible(dest)
}

download_or_disabled <- function(items) {
  tagList(div(class = "row g-1", lapply(items, function(item) {
    div(class = "col-6",
      if (isTRUE(item$ready)) {
        downloadButton(item$id, item$label,
          class = "btn-primary w-100 mb-1 download-grid-button",
          style = "height: 2.5rem;", title = item$title)
      } else {
        tags$button(
          item$label, type = "button",
          class = "btn btn-primary w-100 mb-1 download-grid-button",
          style = "height: 2.5rem;", title = item$title, disabled = "disabled"
        )
      }
    )
  })))
}

# Reduces a pair-level build_crosswalk() table to one best row per target.
# Direction trap: the app always calls build_crosswalk(from = overlay,
# to = base), so the UI Target layer's id is `from_id` here, not `to_id`.
# Winner per target: the highest `coverage`; groups whose rows all carry NA
# coverage (nearest-style rows) fall back to the lowest `match_distance_km`;
# any remaining tie keeps the first row, so the output is deterministic
# across runs. Non-winning pairs stay available in pairs.csv.
collapse_crosswalk_best_match <- function(crosswalk) {
  n <- nrow(crosswalk)
  if (n == 0L) {
    return(crosswalk)
  }
  ids <- as.character(crosswalk$from_id)
  coverage <- if ("coverage" %in% names(crosswalk)) crosswalk$coverage else rep(NA_real_, n)
  distance <- if ("match_distance_km" %in% names(crosswalk)) {
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
  # sort() restores original row order of the winning rows; deterministic.
  crosswalk[sort(keep), , drop = FALSE]
}

# `build_crosswalk()` intentionally returns a compact public assignment schema.
# The app's mixed point/boundary path needs the feature attributes as well, so
# enrich that result here without changing ONgeoR's public function. The app's
# direction convention is `from = target` and `to = source`; the id-column
# fields in the crosswalk are therefore used to recover the corresponding
# rows from the retrieved layers.
#
# The result is aligned to the target layer rather than to the pair table:
# duplicate matches are reduced using the same deterministic winner used by
# the shape export, and targets absent from the crosswalk remain as unmatched
# rows with their target attributes and NA source attributes.
normalize_mixed_crosswalk <- function(crosswalk, source_sf, target_sf) {
  if (is.null(crosswalk) || !inherits(source_sf, "sf") ||
      !inherits(target_sf, "sf") || nrow(crosswalk) == 0L) {
    return(crosswalk)
  }
  if (identical(layer_geom(source_sf), layer_geom(target_sf))) {
    return(crosswalk)
  }
  if (!"from_id" %in% names(crosswalk)) {
    return(crosswalk)
  }

  source_data <- sf::st_drop_geometry(source_sf)
  target_data <- sf::st_drop_geometry(target_sf)
  id_from_crosswalk <- function(table, column, fallback, data) {
    value <- if (column %in% names(table)) table[[column]][1L] else NA_character_
    if (is.na(value) || !nzchar(as.character(value)) ||
        !as.character(value) %in% names(data)) {
      value <- fallback
    }
    value
  }
  target_id_col <- id_from_crosswalk(
    crosswalk, "from_id_col", ONgeoR::layer_id_col(target_sf), target_data
  )
  source_id_col <- id_from_crosswalk(
    crosswalk, "to_id_col", ONgeoR::layer_id_col(source_sf), source_data
  )

  collapsed <- collapse_crosswalk_best_match(crosswalk)
  target_ids <- as.character(target_data[[target_id_col]])
  aligned <- collapsed[match(target_ids, as.character(collapsed$from_id)), , drop = FALSE]
  aligned <- as.data.frame(aligned, stringsAsFactors = FALSE, check.names = FALSE)
  aligned$from_id <- target_ids
  if (!"from_id_col" %in% names(aligned)) {
    aligned$from_id_col <- target_id_col
  } else {
    aligned$from_id_col[] <- target_id_col
  }
  if (!"to_id_col" %in% names(aligned)) {
    aligned$to_id_col <- source_id_col
  } else {
    aligned$to_id_col[] <- source_id_col
  }

  # Fill the target-side descriptive fields for an unmatched target when the
  # compact crosswalk omitted that feature entirely. Existing match and
  # provenance values are left untouched.
  target_name_col <- tryCatch(ONgeoR::layer_name_col(target_sf), error = function(e) NULL)
  if (!is.null(target_name_col) && target_name_col %in% names(target_data)) {
    target_names <- as.character(target_data[[target_name_col]])
    if (!"from_name" %in% names(aligned)) {
      aligned$from_name <- target_names
    } else {
      missing_name <- is.na(aligned$from_name) | !nzchar(as.character(aligned$from_name))
      aligned$from_name[missing_name] <- target_names[missing_name]
    }
  }

  source_ids <- as.character(source_data[[source_id_col]])
  source_index <- if ("to_id" %in% names(aligned)) {
    match(as.character(aligned$to_id), source_ids)
  } else {
    rep(NA_integer_, nrow(aligned))
  }
  source_attrs <- source_data[source_index, , drop = FALSE]
  target_attrs <- target_data
  names(source_attrs) <- paste0("src_", names(source_attrs))
  names(target_attrs) <- paste0("tgt_", names(target_attrs))
  rownames(aligned) <- NULL
  rownames(source_attrs) <- NULL
  rownames(target_attrs) <- NULL
  cbind(aligned, source_attrs, target_attrs)
}

# link() returns one row per sampled cell or point, so a polygon target would
# otherwise appear in the exported layer once per cell (1260 rows for 2 targets
# in a representative run) - which breaks symbology in desktop GIS and makes
# the file inconsistent with the other run types, all of which are one row per
# target. Reduce to one row per target, aligned to `target_sf` row order so the
# geometry never needs reordering: numeric columns become their mean across the
# target's cells, non-numeric columns (run provenance, constant within a
# target) keep their first value, and `linked_cells` records how many rows were
# reduced. Targets with no linked rows get NA attributes and a count of 0.
aggregate_linked_by_target <- function(link_attrs, row_of_target, n_targets) {
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
  # tabulate() ignores NA, which is exactly the unmatched-target case.
  out$linked_cells <- tabulate(row_of_target, nbins = n_targets)
  rownames(out) <- NULL
  out
}

# Builds the "Shapes" download content: the UI Target layer's own geometry
# with the joined attributes merged on. Dispatches on the table schema,
# because the join key differs by run type (all verified empirically against
# real ONgeoR 0.4.0 output before implementation):
#   * build_crosswalk table (carries from_id + from_id_col): collapse to the
#     best row per from_id, then join from_id against the target column named
#     by the table's own from_id_col (the app passes overlay as `from`).
#   * summarise_by_target table (carries target_id): already one row per
#     target; join target_id against the target sf's resolved id column.
#   * raster linked table (neither): carries the target's id column under its
#     raw name (or `.y`-suffixed after an st_join collision), one row per
#     sampled point/cell, so it is aggregated to one row per target by
#     aggregate_linked_by_target() before merging.
# Returns NULL when there is nothing to merge; the download guard req()s it,
# and the button readiness mirrors the same condition.
merge_target_attributes <- function(target_sf, crosswalk = NULL, linked = NULL) {
  if (is.null(target_sf) || inherits(target_sf, "SpatRaster")) {
    # A raster target has no attribute table to merge onto.
    return(NULL)
  }
  has_rows <- function(x) !is.null(x) && nrow(x) > 0L

  if (has_rows(crosswalk) && "from_id" %in% names(crosswalk)) {
    table <- collapse_crosswalk_best_match(crosswalk)
    id_col <- if ("from_id_col" %in% names(table)) table$from_id_col[1L] else NA_character_
    if (is.na(id_col) || !id_col %in% names(target_sf)) {
      rlang::abort(paste(
        "Cannot merge the crosswalk onto the target layer: the id column",
        "recorded in the crosswalk (from_id_col) is missing from the",
        "target layer."
      ))
    }
    idx <- match(as.character(target_sf[[id_col]]), as.character(table$from_id))
    attrs <- as.data.frame(table[idx, , drop = FALSE])
  } else if (has_rows(crosswalk) && "target_id" %in% names(crosswalk)) {
    id_col <- ONgeoR::layer_id_col(target_sf)
    idx <- match(as.character(target_sf[[id_col]]), as.character(crosswalk$target_id))
    attrs <- as.data.frame(crosswalk[idx, , drop = FALSE])
  } else if (has_rows(linked)) {
    id_col <- ONgeoR::layer_id_col(target_sf)
    join_col <- if (id_col %in% names(linked)) {
      id_col
    } else if (paste0(id_col, ".y") %in% names(linked)) {
      # st_join's collision suffixing puts the target copy under `.y`.
      paste0(id_col, ".y")
    } else {
      rlang::abort(paste(
        "Cannot merge the linked table onto the target layer: the target",
        "id column is not present in the linked values table."
      ))
    }
    key_values <- as.character(target_sf[[id_col]])
    row_of_target <- match(as.character(linked[[join_col]]), key_values)
    # The linked table re-states the target's own attribute columns; the
    # geometry already carries those, so keep only the new columns (sampled
    # values and provenance).
    target_copy <- names(linked) %in% names(target_sf) |
      sub("\\.y$", "", names(linked)) %in% names(target_sf)
    link_attrs <- as.data.frame(linked[, !target_copy, drop = FALSE])
    attrs <- aggregate_linked_by_target(
      link_attrs, row_of_target, nrow(target_sf)
    )
  } else {
    return(NULL)
  }

  collisions <- intersect(names(attrs), names(target_sf))
  if (length(collisions) > 0L) {
    rlang::abort(sprintf(
      paste("Cannot merge the joined attributes onto the target layer:",
        "column name collision (%s)."),
      paste(collisions, collapse = ", ")
    ))
  }
  for (nm in names(attrs)) {
    target_sf[[nm]] <- attrs[[nm]]
  }
  target_sf
}

# --- Map-view windowing for census layers ---------------------------------
#
# retrieve_census() accepts a bbox and filters server-side, but nothing in the
# app passed one, so Aggregate Dissemination Area pulled all 1,679 features
# province-wide and Dissemination Area all 20,465. Measured on the cache: the
# ADA layer is 6,004,902 bytes province-wide against 44,922 bytes for a
# city-sized window - 134x.
#
# Only census_* sources support this. ONgeoR::retrieve_source() does NOT
# forward a bbox (see its census_ branch), so the windowed call goes to
# retrieve_census() directly rather than changing the CRAN-bound library.
is_census_source <- function(source_id) {
  !is.null(source_id) && length(source_id) == 1L && !is.na(source_id) &&
    startsWith(source_id, "census_")
}

# Layers this size are the ones that make the app feel broken, so the window is
# offered pre-ticked for them and left off for the small ones.
census_window_threshold <- 1000

census_window_default <- function(source_id) {
  if (!is_census_source(source_id)) return(FALSE)
  reg <- tryCatch(ONgeoR::list_sources(), error = function(e) NULL)
  if (is.null(reg) || is.null(reg$feature_count)) return(FALSE)
  n <- suppressWarnings(as.numeric(reg$feature_count[match(source_id, reg$source_id)]))
  isTRUE(n >= census_window_threshold)
}

# leaflet publishes the visible extent as input$<id>_bounds. Returns the
# EPSG:4326 xmin, ymin, xmax, ymax vector retrieve_census() expects, or NULL if
# the map has not reported a usable extent yet.
map_view_bbox <- function(bounds) {
  if (!is.list(bounds)) return(NULL)
  needed <- c("west", "south", "east", "north")
  if (!all(needed %in% names(bounds))) return(NULL)
  v <- suppressWarnings(as.numeric(unlist(bounds[needed], use.names = FALSE)))
  if (length(v) != 4L || any(!is.finite(v))) return(NULL)
  if (v[1] >= v[3] || v[2] >= v[4]) return(NULL)
  v
}

# One honest line about what this run will cost, sized from the registry's own
# feature counts. Deliberately not an estimate in seconds: the dominant cost is
# a one-time network download whose duration depends on the network, and a
# wrong number is worse than an honest "this may take minutes".
run_hint_text <- function(ids) {
  ids <- ids[!is.na(ids)]
  ids <- setdiff(ids[nzchar(ids)], "postal_upload")
  if (!length(ids)) return("")
  reg <- tryCatch(ONgeoR::list_sources(), error = function(e) NULL)
  if (is.null(reg) || is.null(reg$feature_count)) return("")
  counts <- suppressWarnings(as.numeric(
    reg$feature_count[match(ids, reg$source_id)]
  ))
  counts <- counts[!is.na(counts)]
  if (!length(counts)) return("")
  biggest <- max(counts)
  if (biggest >= 1000) {
    sprintf(paste(
      "Heads up: the largest layer in this run has %s features. The first time",
      "it is downloaded this can take several minutes. It is cached afterwards,",
      "so the same layer is fast next time. Good moment for a coffee - the",
      "elapsed clock below keeps ticking while it works."
    ), format(biggest, big.mark = ","))
  } else {
    paste(
      "Layers are downloaded once and cached, so the first run on a layer is the",
      "slow one. The elapsed clock below keeps ticking while it works."
    )
  }
}

task_status_ui <- function(state, detail = NULL, phases = character()) {
  labels <- c(
    idle = "Idle",
    running = "Running",
    failed = "Failed",
    cancelled = "Cancelled",
    completed = "Completed"
  )
  label <- unname(labels[[state]])
  # Deliberately plain. The banner, progress bar, phase log and Cancel all
  # live in the progress dialog now -- Lennon: "it should not be in control
  # panel, hard to see, should be in a pop up box". `phases` is accepted and
  # ignored so existing callers and tests keep working.
  tags$p(
    class = paste("task-status text-muted", paste0("task-status-", state)),
    `data-state` = state,
    tags$strong(paste0(label, ".")),
    if (!is.null(detail)) paste(" ", detail)
  )
}

# The progress dialog SHELL. Shown exactly once per run and never re-rendered
# by Shiny.
#
# Everything inside is updated from the browser by the "ongeor-progress"
# custom message below. That indirection is not decoration: re-issuing
# showModal() on each phase tick rebuilt the dialog twice a second (present in
# 4 of 60 samples in headless Chrome), and putting a uiOutput() inside a live
# modal tore the modal out of the DOM after its first render, with no
# removeModal() from this app and no server error. Updating innerHTML from JS
# leaves the dialog element untouched, so it survives.
task_progress_modal <- function(operation) {
  # No title: the live log IS the content. Each step shows a spinner while it
  # runs and flips to a check + "done" when the next step starts or the whole
  # run finishes; OK is revealed only at genuine completion. `operation` is kept
  # for the existing callers but is no longer displayed.
  modalDialog(
    title = NULL,
    tags$ul(id = "task_phase_list", class = "task-phase-log list-unstyled mb-0"),
    # What to expect, and proof the app is alive. The elapsed counter is the
    # honest substitute for a percentage bar: the work is a network download
    # and a spatial join with no reportable fraction, so a filling bar would be
    # inventing progress. A ticking clock cannot be wrong.
    tags$div(id = "task_hint", class = "task-hint text-muted small mt-3"),
    tags$div(class = "d-flex align-items-center mt-2",
      tags$div(id = "task_activity",
        class = "progress flex-grow-1 me-3",
        style = "height: 4px;",
        tags$div(
          class = paste("progress-bar progress-bar-striped",
            "progress-bar-animated bg-secondary"),
          style = "width: 100%;"
        )
      ),
      tags$span(id = "task_elapsed", class = "text-muted small font-monospace")
    ),
    footer = tagList(
      actionButton("cancel_task_btn", "Cancel", class = "btn-outline-secondary"),
      actionButton("progress_ok_btn", "OK", class = "btn-primary d-none")
    ),
    easyClose = FALSE
  )
}

# Browser-side updater. Renders the phase list as a live checklist: every step
# except the currently running one shows a check and "done"; the running step
# shows a spinner. When the run finishes (m.done), every step is marked done and
# OK is revealed. A failed/cancelled run appends a red status line. There is no
# title banner and no fake progress bar.
task_progress_js <- tags$script(HTML("
(function(){
  function esc(s){ return String(s).replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;'); }
  var latest = null;
  // Render the buffered state into the dialog. Returns false if the dialog DOM
  // is not present yet, so the caller can retry: a fast run can complete before
  // showModal() has rendered, and a dropped message would hang the dialog.
  function render(){
    var list = document.getElementById('task_phase_list');
    if (!list) return false;
    var m = latest; if (!m) return true;
    var html = '';
    for (var i = 0; i < m.phases.length; i++) {
      var label = esc(m.phases[i]).replace(/\\s*\\.\\.\\.$/, '');
      var done = m.done || (i !== m.phases.length - 1);
      if (done) {
        html += '<li class=\"task-phase task-phase-done mb-1\">' +
                '<span class=\"task-phase-check text-success me-2\">\\u2713</span>' +
                label + ' &mdash; <span class=\"text-muted\">done</span></li>';
      } else {
        html += '<li class=\"task-phase task-phase-active mb-1\">' +
                '<span class=\"spinner-border spinner-border-sm text-secondary me-2\" role=\"status\" aria-hidden=\"true\"></span>' +
                label + '&hellip;</li>';
      }
    }
    if (m.done && !m.ok && m.title) {
      html += '<li class=\"task-phase text-danger mt-2\"><strong>' + esc(m.title) + '</strong></li>';
    }
    list.innerHTML = html;
    var hint = document.getElementById('task_hint');
    if (hint) hint.innerHTML = m.done ? '' : esc(m.hint || '');
    var bar = document.getElementById('task_activity');
    if (bar) bar.style.display = m.done ? 'none' : '';
    var cancel = document.getElementById('cancel_task_btn');
    var okb = document.getElementById('progress_ok_btn');
    if (m.done) {
      if (cancel) cancel.classList.add('d-none');
      if (okb) okb.classList.remove('d-none');
    }
    startClock(m.done);
    return true;
  }

  // Elapsed time, ticked in the browser. Server-side ticking would need a
  // round trip per second and would freeze exactly when the page is busy -
  // i.e. when the user most needs to see that something is still alive.
  var t0 = null, timer = null;
  function fmt(s){
    var m = Math.floor(s / 60), r = s % 60;
    return m + ':' + (r < 10 ? '0' : '') + r;
  }
  function startClock(done){
    var el = document.getElementById('task_elapsed');
    if (!el) return;
    if (done) {
      if (timer) { clearInterval(timer); timer = null; }
      if (t0) el.textContent = fmt(Math.round((Date.now() - t0) / 1000)) + ' elapsed';
      return;
    }
    if (timer) return;
    t0 = Date.now();
    el.textContent = '0:00';
    timer = setInterval(function(){
      var e = document.getElementById('task_elapsed');
      if (!e) { clearInterval(timer); timer = null; return; }
      e.textContent = fmt(Math.round((Date.now() - t0) / 1000));
    }, 1000);
  }
  Shiny.addCustomMessageHandler('ongeor-progress', function(m){
    latest = m;
    if (render()) return;
    var tries = 0;
    var iv = setInterval(function(){
      if (render() || ++tries > 60) clearInterval(iv);
    }, 40);
  });
})();
"))

# --- Phase reporting across the future boundary ---------------------------
# Fetch and join run inside a future in a separate R process, which cannot
# write to a reactiveVal. The task writes its current phase to a plain file
# whose path is passed in (a character scalar exports cleanly through future's
# globals scan; a closure would not - see the pack() notes on the tasks). The
# main session polls the file while the task runs.
new_progress_path <- function() {
  tempfile(pattern = "ongeor-progress-", fileext = ".txt")
}

read_progress_phase <- function(path) {
  phases <- read_progress_phases(path)
  if (is.null(phases)) NULL else phases[length(phases)]
}

read_progress_phases <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    return(NULL)
  }
  phases <- tryCatch(readLines(path, warn = FALSE), error = function(e) character())
  phases <- phases[nzchar(phases)]
  if (length(phases) == 0L) NULL else phases
}


# --- Explicit retrieval failures ------------------------------------------
# ONgeoR raises "Could not retrieve source 'X' ... Retry later" and attaches
# the real curl/HTTP condition as `parent`. conditionMessage() returns only the
# top level, so every network cause used to surface as the same generic text
# advising a retry - wrong advice for a DNS failure, a refused connection or a
# proxy block. Walk the chain, classify it, and say what actually happened.
condition_chain_text <- function(cnd) {
  parts <- character()
  seen <- 0L
  while (!is.null(cnd) && seen < 10L) {
    parts <- c(parts, tryCatch(conditionMessage(cnd), error = function(e) ""))
    cnd <- cnd$parent
    seen <- seen + 1L
  }
  paste(parts[nzchar(parts)], collapse = " | ")
}

describe_retrieval_failure <- function(cnd) {
  raw <- condition_chain_text(cnd)
  low <- tolower(raw)
  hit <- function(pattern) grepl(pattern, low, perl = TRUE)

  # Proxy is checked before the generic connection failures: a blocked proxy
  # often also reports a connect failure, and the proxy cause is the useful one.
  if (hit("proxy|http code 407|407 proxy")) {
    msg <- paste("A network proxy blocked the request to the data service.",
      "This is common on corporate networks.")
    retry <- FALSE
  } else if (hit("could not resolve|resolving timed out|name or service not known|nodename nor servname")) {
    msg <- paste("Cannot reach the data service: the address could not be",
      "resolved. Check your network or VPN connection.")
    retry <- FALSE
  } else if (hit("connection refused|failed to connect|could ?n[o']?t connect|connection reset")) {
    msg <- paste("The data service refused the connection. It may be offline,",
      "or a firewall may be blocking it.")
    retry <- FALSE
  } else if (hit("timed out|timeout was reached|operation timed out")) {
    msg <- "The data service did not respond in time."
    retry <- TRUE
  } else if (hit("ssl|certificate|tls")) {
    msg <- paste("The secure connection to the data service could not be",
      "established (TLS/certificate problem).")
    retry <- FALSE
  } else if (hit("http 5[0-9]{2}|status 5[0-9]{2}")) {
    msg <- "The data service returned a server error."
    retry <- TRUE
  } else if (hit("http 40[34]|status 40[34]|not found")) {
    msg <- paste("The requested layer is not available at the recorded",
      "address; it may have been moved or withdrawn.")
    retry <- FALSE
  } else {
    # Unclassified: keep the original message rather than inventing one.
    msg <- tryCatch(conditionMessage(cnd), error = function(e) raw)
    retry <- NA
  }

  advice <- if (isTRUE(retry)) {
    " Retrying in a few minutes may succeed."
  } else if (isFALSE(retry)) {
    " Retrying now will not help until the cause above is resolved."
  } else {
    ""
  }
  list(message = paste0(msg, advice), detail = raw, retry = retry)
}

# Notification carrying the plain-language cause, with the raw condition chain
# tucked into a <details> block so it is available for debugging without
# putting curl text in front of a non-technical user.
retrieval_failure_notification <- function(described) {
  tagList(
    tags$strong(described$message),
    if (nzchar(described$detail)) {
      tags$details(
        class = "mt-2",
        tags$summary("Technical detail"),
        tags$pre(class = "small mb-0", style = "white-space: pre-wrap;",
          described$detail)
      )
    }
  )
}

# Adds every real basemap choice as its own named tile group. "Light" is
# added first, so it is the default visible base layer.
base_leaflet_layers <- function(map) {
  map <- leaflet::addProviderTiles(map, "CartoDB.Positron", group = "Light")
  map <- leaflet::addProviderTiles(map, "CartoDB.DarkMatter", group = "Dark")
  map <- leaflet::addProviderTiles(map, "OpenStreetMap.Mapnik", group = "OpenStreetMap")
  map <- leaflet::addProviderTiles(map, "Esri.WorldImagery", group = "Satellite")
  map <- leaflet::addProviderTiles(map, "Esri.WorldTopoMap", group = "Topographic")
  map <- leaflet::addProviderTiles(map, "Esri.WorldStreetMap", group = "Streets")
  map <- leaflet::addProviderTiles(map, "CartoDB.Voyager", group = "Voyager")
  map
}

add_styled_sf_layer <- function(map, layer, group, style) {
  if (inherits(layer, "SpatRaster")) {
    if (!requireNamespace("terra", quietly = TRUE)) {
      rlang::abort("Package 'terra' is required to render raster layers.")
    }
    map <- leaflet::addRasterImage(
      map, layer, group = group,
      opacity = style$raster_opacity,
      colors = leaflet::colorNumeric(
        style$raster_palette, terra::values(layer), na.color = "transparent"
      )
    )
    return(map)
  }

  name_col <- ONgeoR::guess_name_col(layer)
  geometry_types <- unique(as.character(sf::st_geometry_type(layer)))
  polygon_types <- c("POLYGON", "MULTIPOLYGON", "GEOMETRYCOLLECTION")

  if (all(geometry_types %in% c("POINT", "MULTIPOINT"))) {
    popups <- as.character(layer[[name_col]])
    if (identical(style$point_shape, "square")) {
      buf_deg <- style$point_size * 0.0015
      coords <- sf::st_coordinates(layer)
      map <- leaflet::addRectangles(
        map,
        lng1 = coords[, 1] - buf_deg, lat1 = coords[, 2] - buf_deg,
        lng2 = coords[, 1] + buf_deg, lat2 = coords[, 2] + buf_deg,
        group = group, popup = popups,
        color = style$point_color, fillColor = style$point_color,
        fillOpacity = 0.8, weight = 1
      )
    } else {
      map <- leaflet::addCircleMarkers(
        map,
        data = layer, group = group, popup = popups,
        radius = style$point_size, stroke = FALSE,
        fillColor = style$point_color, fillOpacity = 0.8
      )
    }
  } else if (all(geometry_types %in% polygon_types)) {
    polygon_layer <- if ("GEOMETRYCOLLECTION" %in% geometry_types) {
      ONgeoR::extract_polygon_collection(layer)
    } else {
      layer
    }
    polygon_layer <- polygon_layer[!sf::st_is_empty(polygon_layer), ]
    popups <- as.character(polygon_layer[[name_col]])
    map <- leaflet::addPolygons(
      map,
      data = polygon_layer, group = group, popup = popups,
      weight = 2, color = style$line_color,
      fillColor = style$fill_color, fillOpacity = style$fill_opacity
    )
  } else {
    rlang::abort(sprintf(
      "Layer '%s' has unsupported geometry type(s): %s.",
      group, paste(geometry_types, collapse = ", ")
    ))
  }
  map
}

# --- Map furniture -------------------------------------------------------
# "Furniture" layers are bundled reference outlines drawn on the
# map at all times: they render at app load, before any preview, so the
# Leaflet widget exists immediately with no retrieval and no network call
# (both datasets ship in inst/extdata). They use a fixed context style -
# thin grey outline, no fill, no popup - and are deliberately NOT routed
# through layer_style_controls()/read_layer_style(): those controls belong
# to user-selected sources. Data is loaded once per R process and cached.
.furniture_cache <- new.env(parent = emptyenv())

furniture_layer <- function(id) {
  if (!exists(id, envir = .furniture_cache)) {
    .furniture_cache[[id]] <- switch(id,
      PHU_simple = ONgeoR::retrieve_phu_simple(),
      HIVE = ONgeoR::retrieve_hive(),
      rlang::abort(sprintf("Unknown furniture layer '%s'.", id))
    )
  }
  .furniture_cache[[id]]
}

# PHU_simple ships in inst/extdata and renders at app load with no network
# call. It is suppressed when either live PHU vintage is actually drawn.
furniture_layers <- function(selected_ids = character()) {
  layers <- list()
  if (!any(c("phu_boundaries", "phu_boundaries_pre2025") %in% selected_ids)) {
    layers[["PHU_simple"]] <- furniture_layer("PHU_simple")
  }
  layers
}

add_furniture_layer <- function(map, layer, group) {
  leaflet::addPolygons(
    map,
    data = layer, group = group,
    weight = 1, color = "#898781", fill = FALSE
  )
}

# `styles` is a named list parallel to `layers`: styles[[nm]] is the per-layer
# style for layers[[nm]]. `furniture` is an optional named list of bundled
# reference layers, drawn in the fixed furniture style and appended after
# the styled source layers so furniture always sits at the bottom of the
# overlay list.
render_styled_map <- function(layers, styles, add_control = TRUE,
                              furniture = list(), layer_labels = NULL) {
  map <- base_leaflet_layers(leaflet::leaflet())
  for (nm in names(layers)) {
    display_name <- layer_labels[[nm]] %||% nm
    map <- add_styled_sf_layer(map, layers[[nm]], display_name, styles[[nm]])
  }
  for (nm in names(furniture)) {
    map <- add_furniture_layer(map, furniture[[nm]], nm)
  }
  # Only "Light" (added first, above) starts visible; hide the other real
  # tile groups so the layers control's radio behavior starts from a single
  # clean default instead of stacking all tile layers. HIVE furniture also
  # starts hidden (unchecked) - its geometry ships to the browser but is
  # not rendered until the user toggles it on.
  map <- leaflet::hideGroup(
    map,
    c(
      "Dark", "OpenStreetMap", "Satellite",
      "Topographic", "Streets", "Voyager",
      "HIVE"
    )
  )
  if (add_control) {
    # names() on an empty list is NULL, and c(NULL, NULL) stays NULL, which
    # leaflet renders as a single checkbox labelled "null". Normalise to
    # character(0) so an empty overlay set produces no overlay entries.
    overlay_groups <- unname(c(
      vapply(names(layers), function(nm) layer_labels[[nm]] %||% nm, character(1)),
      names(furniture)
    ))
    if (is.null(overlay_groups)) {
      overlay_groups <- character(0)
    }
    map <- leaflet::addLayersControl(
      map,
      baseGroups = basemap_groups,
      overlayGroups = overlay_groups,
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )
  }
  map
}

add_nearest_connectors <- function(map, layers, connectors, conn_style,
                                   furniture = list(), layer_labels = NULL) {
  overlay_groups <- c(
    vapply(names(layers), function(nm) layer_labels[[nm]] %||% nm, character(1)),
    names(furniture)
  )
  if (!is.null(connectors) && nrow(connectors) > 0) {
    map <- leaflet::addPolylines(
      map,
      data = connectors, group = "Connections",
      color = conn_style$color, weight = conn_style$weight,
      opacity = conn_style$opacity, dashArray = "4,4"
    )
    overlay_groups <- c(overlay_groups, "Connections")
  }

  leaflet::addLayersControl(
    map,
    baseGroups = basemap_groups,
    overlayGroups = overlay_groups,
    options = leaflet::layersControlOptions(collapsed = FALSE)
  )
}

# TRUE when a completed link run produced a nearest (point-to-point) result,
# whose pair table carries relation == "nearest". Drives the connector lines
# drawn on the main map for a point-to-point join.
is_nearest_result <- function(pairs) {
  !is.null(pairs) && "relation" %in% colnames(pairs) &&
    any(pairs$relation == "nearest", na.rm = TRUE)
}

# Builds the connector lines for a nearest result: one line per matched
# target/source point pair. The pair table carries the matched ids; they are
# resolved back to point geometries through each layer's id column.
nearest_connectors <- function(source_sf, target_sf, pairs) {
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

ui <- bslib::page_fillable(
  window_title = "ONgeoR",
  theme = bslib::bs_theme(version = 5, primary = "#2a78d6", success = "#0ca30c"),
  tags$head(tags$link(rel = "stylesheet", href = "theme.css"), task_progress_js),
  # The logo lives at the top of the sidebar, not in a page header. The app is
  # a single-purpose linking interface, so the layout can fill the page
  # directly and a header bar would only cost the map vertical space.
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      tags$div(class = "sidebar-brand",
        tags$img(src = "logo.png", alt = "ONgeoR")),
      tags$div(class = "slot-block slot-overlay",
        radioButtons("overlay_target_mode", "1. Target input",
          choices = c(
            "Registry layer" = "registry",
            "Uploaded postal codes" = "postal_upload",
            "Use my own file" = "own_file"
          ),
          selected = "registry"),
        conditionalPanel(
          "input.overlay_target_mode == 'registry'",
          selectInput("overlay_source", "Target layer (points)",
            choices = target_choices_grouped(include_open_postal = TRUE),
            selected = "moh_service_locations")
        ),
        tags$div(class = "slot-meta",
          uiOutput("overlay_geom_badge")
        ),
        conditionalPanel(
          "input.overlay_target_mode == 'own_file'",
          fileInput("overlay_own_file", "Point target file", buttonLabel = "Browse...",
            accept = c(".geojson", ".json", ".gpkg", ".shp", ".zip"),
            placeholder = "GeoJSON, GeoPackage, or zipped shapefile"),
          uiOutput("overlay_own_status_ui")
        ),
        uiOutput("postal_upload_ui")
      ),
      tags$div(class = "slot-block slot-base",
        selectInput("base_layer", "2. Source layer to join from", choices = source_choices_grouped(), selected = "phu_boundaries"),
        uiOutput("census_window_ui"),
        tags$div(class = "slot-meta",
          uiOutput("base_geom_badge")
        )
      ),
      uiOutput("link_relationship"),
      uiOutput("link_method_ui"),
      actionButton("preview_btn", "Preview on map", class = "btn-preview w-100 mb-1"),
      uiOutput("preview_task_status"),
      uiOutput("build_btn_ui"),
      uiOutput("link_task_status"),
      tags$hr(),
      bslib::accordion(
        open = FALSE,
        bslib::accordion_panel(
          tags$span(class = "slot-title slot-title-base", "Source layer style"),
          uiOutput("base_style_ui"),
          value = "Source layer style"
        ),
        bslib::accordion_panel(
          tags$span(class = "slot-title slot-title-overlay", "Target layer style"),
          uiOutput("overlay_style_ui"),
          value = "Target layer style"
        )
      ),
      tags$hr(),
      tags$strong("Downloads"),
      uiOutput("link_downloads_ui")
    ),
    bslib::navset_tab(
      id = "main_tabs",
      bslib::nav_panel(
        "Map",
        # Chrome above the map is now only the inner Map/Data strip. The
        # sidebar logo is outside the map column, so 130px leaves room for
        # the remaining page and inner-navigation chrome.
        leafletOutput("cw_map", height = "calc(100vh - 130px)")
      ),
      bslib::nav_panel(
        "Data",
        value = "data",
        DT::dataTableOutput("cw_table")
      )
    )
  )
)

server <- function(input, output, session) {

  # Keep results unavailable until a join has actually completed. nav_hide()
  # and nav_show() are bslib's supported wrappers around Shiny tab updates.
  bslib::nav_hide("main_tabs", "data", session = session)

  # --- Async tasks (ExtendedTask; requires shiny >= 1.8.0) -----------

  preview_task <- shiny::ExtendedTask$new(function(base_id, overlay_id,
                                                     generation,
                                                     overlay_layer = NULL,
                                                     progress_path = NULL,
                                                     bbox = NULL) {
    promises::future_promise({
      # Defined INSIDE the future block on purpose. The app-level
      # pack_spatial() is a closure over the app source environment, which
      # holds the multisession plan object; future's globals scan walks that
      # enclosure, finds an externalptr, and refuses to export the function
      # ("Detected a non-exportable reference"). A local copy has the future
      # block itself as its enclosure and exports cleanly.
      pack <- function(x) if (inherits(x, "SpatRaster")) terra::wrap(x) else x
      # Same reasoning: a local writer over a plain path, not a closure.
      note <- function(phase) {
        if (!is.null(progress_path)) {
          try(cat(
            phase, "\n", sep = "", file = progress_path, append = TRUE
          ), silent = TRUE)
        }
      }
      # Defined inside the future for the same reason as pack()/note(): a local
      # copy exports cleanly. `bbox` is a plain numeric vector, so it crosses
      # the boundary without packing.
      fetch <- function(id) {
        if (!is.null(bbox) && startsWith(id, "census_")) {
          ONgeoR::retrieve_census(id, bbox = bbox)
        } else {
          ONgeoR::retrieve_source(id)
        }
      }
      note(if (is.null(bbox)) {
        "Retrieving source data..."
      } else {
        "Retrieving source data (current map view only)..."
      })
      base_sf    <- fetch(base_id)
      note("Retrieving target data...")
      overlay_sf <- if (!is.null(overlay_layer)) overlay_layer else fetch(overlay_id)
      note("Preparing layers for the map...")
      list(base_sf = pack(base_sf),
           overlay_sf = pack(overlay_sf),
           base_id = base_id, overlay_id = overlay_id,
           generation = generation)
    })
  })

  build_task <- shiny::ExtendedTask$new(function(base_id, overlay_id,
                                                   generation,
                                                   overlay_layer = NULL,
                                                   progress_path = NULL,
                                                   bbox = NULL) {
    promises::future_promise({
      # Local copies, not the app-level pack_spatial()/layer_geom() - see the
      # note in preview_task: those closures enclose the multisession plan and
      # are rejected by future's non-exportable-globals check.
      pack <- function(x) if (inherits(x, "SpatRaster")) terra::wrap(x) else x
      note <- function(phase) {
        if (!is.null(progress_path)) {
          try(cat(
            phase, "\n", sep = "", file = progress_path, append = TRUE
          ), silent = TRUE)
        }
      }
      kind_of <- function(layer) {
        if (inherits(layer, "SpatRaster")) return("raster")
        types <- unique(as.character(sf::st_geometry_type(layer)))
        if (all(types %in% c("POINT", "MULTIPOINT"))) "point" else "polygon"
      }
      fetch <- function(id) {
        if (!is.null(bbox) && startsWith(id, "census_")) {
          ONgeoR::retrieve_census(id, bbox = bbox)
        } else {
          ONgeoR::retrieve_source(id)
        }
      }
      note(if (is.null(bbox)) {
        "Retrieving source data..."
      } else {
        "Retrieving source data (current map view only)..."
      })
      base_sf    <- fetch(base_id)
      note("Retrieving target data...")
      overlay_sf <- if (!is.null(overlay_layer)) overlay_layer else fetch(overlay_id)
      note("Joining layers...")
      base_kind    <- kind_of(base_sf)
      overlay_kind <- kind_of(overlay_sf)
      if (base_kind == "raster" || overlay_kind == "raster") {
        # Rasters are not crosswalk-able; route to link(), which is
        # raster-aware. Order so the raster sits where link()'s reduction is
        # semantically right: a raster SOURCE reduces to cell-centroid points
        # (raster + polygon case, cells into boundaries), a raster TARGET
        # reduces to cell polygons (point + raster case, sampling).
        # Both-raster is aborted by link() itself and surfaces via the
        # tryCatch in the status observer.
        if ("polygon" %in% c(base_kind, overlay_kind)) {
          if (base_kind == "raster") {
            from_sf <- base_sf; to_sf <- overlay_sf
          } else {
            from_sf <- overlay_sf; to_sf <- base_sf
          }
        } else {
          if (base_kind == "raster") {
            from_sf <- overlay_sf; to_sf <- base_sf
          } else {
            from_sf <- base_sf; to_sf <- overlay_sf
          }
        }
        note("Building results table...")
        linked <- ONgeoR::link(from_sf, to_sf, predicate = "within")
        list(crosswalk = NULL, linked = linked, pairs = NULL,
             base_sf = pack(base_sf),
             overlay_sf = pack(overlay_sf),
             generation = generation)
      } else if (base_kind == "polygon" && overlay_kind == "polygon") {
        # Polygon x polygon: intersection. The pair table is canonical; the
        # target-level table summarises it to one row per target. The Target
        # layer (overlay) is the index unit the result has one row per, so it
        # is build_intersection()'s `target` argument (base is `source`).
        pairs <- ONgeoR::build_intersection(base_sf, overlay_sf)
        note("Building results table...")
        crosswalk <- ONgeoR::summarise_by_target(pairs)
        list(crosswalk = crosswalk, linked = NULL, pairs = pairs,
             base_sf = pack(base_sf),
             overlay_sf = pack(overlay_sf),
             generation = generation)
      } else if (base_kind == "point" && overlay_kind == "point") {
        # Point x point: nearest matching, folded into the main flow. Each
        # target point (overlay) is matched to its nearest source point (base),
        # so overlay is build_nearest_pairs()'s `target` argument.
        pairs <- ONgeoR::build_nearest_pairs(base_sf, overlay_sf)
        note("Building results table...")
        crosswalk <- ONgeoR::summarise_by_target(pairs)
        list(crosswalk = crosswalk, linked = NULL, pairs = pairs,
             base_sf = pack(base_sf),
             overlay_sf = pack(overlay_sf),
             generation = generation)
      } else {
        # Universal direction rule: every crosswalk row assigns an overlay
        # unit to the base polygon it belongs to (overlay is always from,
        # base always to) - e.g. each airport polygon is assigned to its
        # health unit, never the reverse. Point-in-boundary containment has
        # no rule to choose, so it runs build_crosswalk()'s default (within).
        from_sf   <- overlay_sf
        to_sf     <- base_sf
        note("Building results table...")
        crosswalk <- ONgeoR::build_crosswalk(from_sf, to_sf)
        list(crosswalk = crosswalk, linked = NULL, pairs = NULL,
             base_sf = pack(base_sf),
             overlay_sf = pack(overlay_sf),
             generation = generation)
      }
    })
  })

  # --- Layer pickers & preview ---------------------------------------

  # Keep the hidden internal target id synchronized with the explicit target
  # mode. Registry/open-postal/uploaded-postal/custom-file are mutually
  # exclusive by construction; switching mode also resets the prior source
  # selection so a stale postal upload or custom file cannot be submitted.
  observeEvent(input$overlay_target_mode, {
    mode <- target_input_mode(input)
    if (identical(mode, "registry")) {
      groups <- remove_choice_grouped(
        target_choices_grouped(include_open_postal = TRUE), input$base_layer
      )
      updateSelectInput(
        session, "overlay_source", choices = groups,
        selected = if (input$overlay_source %in% unlist(groups)) {
          input$overlay_source
        } else {
          first_choice_grouped(groups)
        }
      )
    } else {
      selected <- switch(mode,
        open_postal = "postal_points",
        postal_upload = "postal_upload",
        own_file = "custom_target",
        NULL)
      labels <- switch(mode,
        open_postal = "Open postal codes",
        postal_upload = "Uploaded postal codes",
        own_file = "Uploaded point target",
        "Target layer")
      updateSelectInput(session, "overlay_source",
        choices = stats::setNames(selected, labels), selected = selected)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$base_layer, {
    if (identical(target_input_mode(input), "own_file")) return()
    include_open <- TRUE
    groups <- remove_choice_grouped(
      target_choices_grouped(include_open_postal = include_open), input$base_layer
    )
    selected <- if (input$overlay_source %in% unlist(groups)) {
      input$overlay_source
    } else {
      first_choice_grouped(groups)
    }
    updateSelectInput(session, "overlay_source", choices = groups, selected = selected)
  }, ignoreInit = TRUE)

  observeEvent(input$overlay_source, {
    if (identical(target_input_mode(input), "own_file")) return()
    groups <- remove_choice_grouped(source_choices_grouped(), input$overlay_source)
    selected <- if (input$base_layer %in% unlist(groups)) {
      input$base_layer
    } else {
      first_choice_grouped(groups)
    }
    updateSelectInput(session, "base_layer", choices = groups, selected = selected)
  }, ignoreInit = TRUE)

  # Geometry-type feedback badges, reactive the moment a picker changes.
  output$base_geom_badge <- renderUI({
    req(input$base_layer)
    geo_badge(input$base_layer)
  })
  output$overlay_geom_badge <- renderUI({
    req(input$overlay_source)
    geo_badge(input$overlay_source)
  })
  output$link_relationship <- renderUI({
    req(input$base_layer, input$overlay_source)
    tags$p(class = "geo-relationship text-muted",
      relationship_text(geom_kind(input$base_layer), geom_kind(input$overlay_source)))
  })

  # Per-layer style controls, driven by each selected source's geometry.
  output$base_style_ui <- renderUI({
    req(input$base_layer)
    layer_style_controls("base", geom_kind(input$base_layer), accent = "#2a78d6")
  })
  output$overlay_style_ui <- renderUI({
    req(input$overlay_source)
    layer_style_controls("overlay", geom_kind(input$overlay_source), accent = "#1baf7a")
  })

  output$link_method_ui <- renderUI({
    req(input$base_layer, input$overlay_source)
    base_k <- geom_kind(input$base_layer)
    overlay_k <- geom_kind(input$overlay_source)
    help_link <- actionLink("method_help", "?", class = "method-help",
      title = "How linking works, by layer types")
    # No match rule to choose: the geometry pair alone decides the operation.
    # State what will happen, using the same words as the help matrix.
    matrix_df <- ONgeoR:::link_matrix_df()
    row <- matrix_df[matrix_df$source_kind == base_k &
      matrix_df$target_kind == overlay_k, , drop = FALSE]
    what <- if (nrow(row) > 0) row$what_it_does[1] else ""
    tagList(
      tags$p(class = "text-muted", what),
      help_link
    )
  })

  observeEvent(input$method_help, {
    showModal(modalDialog(
      title = "How linking works, by layer types",
      link_matrix_table(),
      easyClose = TRUE, size = "l", footer = modalButton("Close")
    ))
  })

  cw_result <- reactiveValues(crosswalk = NULL, linked = NULL, pairs = NULL,
    base_sf = NULL, overlay_sf = NULL, previewed = NULL)
  preview_generation <- reactiveVal(0L)
  preview_active_generation <- reactiveVal(NULL)
  link_generation <- reactiveVal(0L)
  link_active_generation <- reactiveVal(NULL)
  link_active_inputs <- reactiveVal(NULL)
  link_state <- reactiveVal("idle")
  link_state_detail <- reactiveVal(NULL)
  # Preview has its own status line: it is the longer wait of the two and
  # previously reported nothing but a button label.
  preview_state <- reactiveVal("idle")
  preview_state_detail <- reactiveVal(NULL)
  # Paths the running tasks write their current phase to (see
  # new_progress_path / read_progress_phase).
  preview_progress_path <- reactiveVal(NULL)
  link_progress_path <- reactiveVal(NULL)
  preview_progress_log <- reactiveVal(character())
  link_progress_log <- reactiveVal(character())

  # --- Postal upload handling ------------------------------------------

  postal_file_result <- reactive({
    req(input$postal_file)
    tryCatch({
      df <- read_uploaded_file(input$postal_file$name, input$postal_file$datapath)
      if (is.null(df)) {
        list(df = NULL, error = "Unsupported file type. Upload a .csv, .xlsx, or .xls file.")
      } else {
        list(df = as.data.frame(df, stringsAsFactors = FALSE), error = NULL)
      }
    }, error = function(e) {
      list(df = NULL, error = conditionMessage(e))
    })
  })

  postal_result <- reactive({
    fr <- postal_file_result()
    req(fr$df)
    col <- input$postal_column
    req(col, nzchar(col))
    if (!col %in% names(fr$df)) return(NULL)
    codes <- as.character(fr$df[[col]])
    n_input <- length(codes)
    sf_layer <- tryCatch(
      withCallingHandlers(
        ONgeoR::resolve_postal_points(codes, as_sf = TRUE),
        warning = function(w) invokeRestart("muffleWarning")
      ),
      error = function(e) NULL
    )
    n_placed <- if (!is.null(sf_layer)) nrow(sf_layer) else 0L
    n_geonames <- if (!is.null(sf_layer) && "point_source" %in% names(sf_layer)) {
      sum(sf_layer$point_source == "geonames", na.rm = TRUE)
    } else {
      0L
    }
    list(
      sf = sf_layer,
      n_input = n_input,
      n_placed = n_placed,
      n_unmatched = n_input - n_placed,
      n_geonames = n_geonames
    )
  })

  own_target_result <- reactive({
    file <- input$overlay_own_file
    if (is.null(file) || !nzchar(file$datapath %||% "")) {
      return(list(sf = NULL, error = "Choose a point vector file to use as the target."))
    }
    read_uploaded_point_vector(file$name, file$datapath)
  })

  output$overlay_own_status_ui <- renderUI({
    result <- own_target_result()
    if (!is.null(result$error)) {
      return(tags$p(class = "text-danger", result$error))
    }
    tags$p(class = "text-success",
      sprintf("Ready: %s point features.", format(nrow(result$sf), big.mark = ",")))
  })

  overlay_request <- reactive({
    mode <- target_input_mode(input)
    if (identical(mode, "own_file")) {
      result <- own_target_result()
      return(list(id = "custom_target", layer = result$sf, error = result$error))
    }
    if (identical(mode, "postal_upload")) {
      if (is.null(input$postal_file) || is.null(input$postal_column) ||
          !nzchar(input$postal_column)) {
        return(list(
          id = "postal_upload", layer = NULL,
          error = "Upload postal codes and select a valid postal code column first."
        ))
      }
      result <- postal_result()
      if (is.null(result) || is.null(result$sf)) {
        return(list(
          id = "postal_upload", layer = NULL,
          error = "Upload postal codes and select a valid postal code column first."
        ))
      }
      return(list(id = "postal_upload", layer = result$sf, error = NULL))
    }
    id <- if (identical(mode, "open_postal")) "postal_points" else input$overlay_source
    list(id = id, layer = NULL, error = NULL)
  })

  observeEvent(list(input$postal_file, input$postal_column), {
    if (identical(target_input_mode(input), "postal_upload")) {
      preview_generation(preview_generation() + 1L)
      link_generation(link_generation() + 1L)
      cw_result$crosswalk <- NULL
      cw_result$linked <- NULL
      cw_result$pairs <- NULL
      cw_result$base_sf <- NULL
      cw_result$overlay_sf <- NULL
      cw_result$previewed <- NULL
    }
  }, ignoreInit = TRUE)

  output$postal_upload_ui <- renderUI({
    req(identical(target_input_mode(input), "postal_upload"))
    tagList(
      fileInput("postal_file", "Upload postal codes",
        accept = c(".csv", ".xlsx", ".xls"),
        buttonLabel = "Browse...",
        placeholder = "CSV or Excel file"),
      uiOutput("postal_column_ui"),
      uiOutput("postal_status_ui")
    )
  })

  output$postal_column_ui <- renderUI({
    fr <- postal_file_result()
    if (!is.null(fr$error)) {
      return(tags$p(class = "text-muted", fr$error))
    }
    req(fr$df)
    detection <- detect_postal_column(fr$df)
    cols <- stats::setNames(names(fr$df), names(fr$df))
    selected <- if (!is.null(detection$column)) detection$column else ""
    score_text <- if (!is.null(detection$column)) {
      sprintf("Detected: %s (%.0f%% match rate)", detection$column, detection$score * 100)
    } else {
      "No column with >= 80% postal code match. Select one manually."
    }
    tagList(
      selectInput("postal_column", "Postal code column",
        choices = c("Select a column..." = "", cols),
        selected = selected),
      tags$p(class = "text-muted", score_text)
    )
  })

  output$postal_status_ui <- renderUI({
    fr <- postal_file_result()
    if (!is.null(fr$error)) return(NULL)
    pr <- postal_result()
    req(pr)
    status <- tags$p(class = "text-muted",
      sprintf("%s input rows, %s placed, %s unmatched.",
        format(pr$n_input, big.mark = ","),
        format(pr$n_placed, big.mark = ","),
        format(pr$n_unmatched, big.mark = ",")))
    if (pr$n_geonames > 0L) {
      status <- tagList(status,
        tags$p(class = "text-muted",
          sprintf(
            "%s points use Geonames place-level coordinates, not address-level. In rural areas these can be kilometres off.",
            format(pr$n_geonames, big.mark = ","))))
    }
    status
  })

  output$link_task_status <- renderUI({
    task_status_ui(link_state(), link_state_detail(), link_progress_log())
  })

  output$preview_task_status <- renderUI({
    task_status_ui(preview_state(), preview_state_detail(), preview_progress_log())
  })

  # Shown only for census layers, which are the only sources that accept a
  # bbox. Deliberately an explicit, visible control rather than silent
  # windowing: a quietly partial province is the "silent success" failure this
  # codebase keeps hitting - the user would get a fast, plausible, wrong answer.
  output$census_window_ui <- renderUI({
    req(input$base_layer)
    if (!is_census_source(input$base_layer)) return(NULL)
    tagList(
      checkboxInput(
        "limit_to_view",
        "Limit to current map view",
        value = census_window_default(input$base_layer)
      ),
      tags$p(class = "text-muted small mt-n2",
        paste("Retrieves only the features intersecting the visible map extent.",
          "Much faster for fine geographies - and the results cover only what",
          "you can see. Untick for the whole province."))
    )
  })

  # NULL means "no window": either the layer does not support one, the user
  # unticked it, or the map has not reported an extent yet.
  requested_bbox <- function() {
    if (!isTRUE(isolate(input$limit_to_view))) return(NULL)
    if (!is_census_source(isolate(input$base_layer))) return(NULL)
    map_view_bbox(isolate(input$cw_map_bounds))
  }

  # Poll the running tasks' phase files and push the phase into the status
  # line. A future cannot write to a reactiveVal, so the file is the channel;
  # this observer is the only thing that reads it. It reschedules itself only
  # while something is running, so an idle app does no polling.
  observe({
    running <- identical(preview_state(), "running") ||
      identical(link_state(), "running")
    if (!running) {
      return()
    }
    invalidateLater(500)
    if (identical(preview_state(), "running")) {
      phases <- read_progress_phases(isolate(preview_progress_path()))
      if (!is.null(phases)) preview_progress_log(phases)
      phase <- if (is.null(phases)) NULL else phases[length(phases)]
      if (!is.null(phase) && !identical(phase, isolate(preview_state_detail()))) {
        preview_state_detail(phase)
      }
      # Never push an empty list: the browser handler assigns innerHTML
      # unconditionally, so an empty push ERASES whatever the dialog is
      # showing. Before the worker writes its first phase line this would
      # blank the seeded "Starting" line twice a second.
      if (!is.null(phases)) push_progress(phases, done = FALSE)
    }
    if (identical(link_state(), "running")) {
      phases <- read_progress_phases(isolate(link_progress_path()))
      if (!is.null(phases)) link_progress_log(phases)
      phase <- if (is.null(phases)) NULL else phases[length(phases)]
      if (!is.null(phase) && !identical(phase, isolate(link_state_detail()))) {
        link_state_detail(phase)
      }
      if (!is.null(phases)) push_progress(phases, done = FALSE)
    }
  })

  # --- progress dialog -----------------------------------------------------
  #
  # Shown once when work starts, updated from the browser, and dismissed by
  # the user with OK. It is never re-rendered by Shiny while open; see the
  # note on task_progress_modal() for the two approaches that failed.
  # What the user should expect from THIS run. Computed once when the dialog
  # opens, in a plain environment, because push_progress() is also called from
  # a later::later() callback where there is no reactive context to read
  # inputs from.
  dialog_hint <- new.env(parent = emptyenv())
  dialog_hint$text <- ""

  push_progress <- function(phases, done, ok = TRUE, title = NULL) {
    session$sendCustomMessage("ongeor-progress", list(
      phases = as.list(as.character(phases)),
      done = isTRUE(done),
      ok = isTRUE(ok),
      title = title %||% "",
      hint = dialog_hint$text %||% ""
    ))
  }

  progress_dialog_open <- reactiveVal(FALSE)

  open_progress_dialog <- function(op) {
    dialog_hint$text <- tryCatch(
      run_hint_text(c(isolate(input$base_layer), isolate(input$overlay_source))),
      error = function(e) ""
    )
    showModal(task_progress_modal(op))
    progress_dialog_open(TRUE)
    # Seed one visible phase immediately. The dialog's body is an empty <ul>
    # until a push fills it, and the worker's first phase line does not exist
    # until the future starts, so without this the user is shown a blank box
    # with a lone Cancel button and no evidence anything is happening.
    push_progress("Starting", done = FALSE)
  }

  # --- Preview render acknowledgement --------------------------------------
  #
  # A preview is finished when the BROWSER has drawn the map, not when the
  # server has sent it. The terminal push is therefore owned by
  # input$cw_map_rendered (emitted by the widget's onRender above), with a cap
  # so a signal that never arrives cannot hang the dialog open forever.
  #
  # Deliberately NOT reactive. This state is entered from inside the
  # ExtendedTask promise-resolution flush, and a reactiveVal written there
  # invalidates its observers without ever re-flushing them - the trap that
  # made the previous render-signal attempt hang and got it deleted on
  # 2026-08-07. A plain environment plus later::later() avoids reactivity, and
  # the acknowledging observer is driven by a client input, which always
  # starts its own flush.
  render_ack_timeout_secs <- 20
  render_ack <- new.env(parent = emptyenv())
  render_ack$awaiting <- FALSE
  render_ack$phases <- character()
  render_ack$token <- 0L

  finish_preview_render <- function(token = NULL) {
    if (!isTRUE(render_ack$awaiting)) return(invisible(FALSE))
    # A stale timer from a superseded preview must not close a newer dialog.
    if (!is.null(token) && !identical(token, render_ack$token)) {
      return(invisible(FALSE))
    }
    render_ack$awaiting <- FALSE
    try(push_progress(render_ack$phases, done = TRUE, ok = TRUE, title = NULL),
      silent = TRUE)
    invisible(TRUE)
  }

  await_preview_render <- function(phases) {
    render_ack$awaiting <- TRUE
    render_ack$phases <- phases
    render_ack$token <- render_ack$token + 1L
    token <- render_ack$token
    # Last phase keeps spinning until the browser confirms the draw.
    push_progress(phases, done = FALSE)
    later::later(function() finish_preview_render(token), render_ack_timeout_secs)
    invisible(NULL)
  }

  # Test seam: exercise the cap without waiting out the timer.
  force_render_ack_timeout <- function() finish_preview_render()

  observeEvent(input$cw_map_rendered, {
    finish_preview_render()
  }, ignoreInit = TRUE)

  # Preview opens its own dialog. Join reuses the confirmation dialog already
  # on screen -- Lennon: "combine with the existing pop up" -- so the confirm
  # box becomes the progress box in place rather than closing and reopening.
  observeEvent(preview_state(), {
    if (identical(preview_state(), "running") && !isTRUE(isolate(progress_dialog_open()))) {
      open_progress_dialog("Preview")
    }
  })

  # Finish: swap the banner, stop the bar, reveal OK. The dialog stays until
  # the user dismisses it.
  finish_progress <- function(state, phases, what) {
    if (!isTRUE(isolate(progress_dialog_open()))) return()
    title <- switch(state,
      completed = if (identical(what, "Preview")) {
        "Both layers are on the map."
      } else {
        paste(what, "complete.")
      },
      failed    = paste(what, "failed."),
      cancelled = paste(what, "cancelled."),
      paste(what, state))
    push_progress(phases, done = TRUE,
      ok = identical(state, "completed"), title = title)
  }

  observeEvent(preview_state(), {
    st <- preview_state()
    if (st %in% c("completed", "failed", "cancelled")) {
      unlink(isolate(preview_progress_path()) %||% character())
      # "completed" is deliberately NOT finished here. A successful preview is
      # only done once the browser reports the map drawn (await_preview_render),
      # and this observer must not pre-empt that with a premature done = TRUE.
      # It never re-fires in a live session anyway - it is invalidated inside
      # the promise-resolution flush - but testServer flushes manually, so
      # leaving it in place made the dialog honest in production and dishonest
      # under test, which is the wrong way round.
      if (!identical(st, "completed")) {
        finish_progress(st, isolate(preview_progress_log()), "Preview")
      }
    }
  }, ignoreInit = TRUE)

  observeEvent(link_state(), {
    st <- link_state()
    if (st %in% c("completed", "failed", "cancelled")) {
      finish_progress(st, isolate(link_progress_log()), "Join")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$progress_ok_btn, {
    progress_dialog_open(FALSE)
    removeModal()
  })

  observeEvent(input$cancel_task_btn, {
    if (identical(link_state(), "running")) {
      link_generation(link_generation() + 1L)
      link_state("cancelled")
      link_state_detail("Cancelled; the previous run was discarded.")
    } else if (identical(preview_state(), "running")) {
      preview_generation(preview_generation() + 1L)
      preview_state("cancelled")
      preview_state_detail("Cancelled; the previous preview was discarded.")
    }
  })

  # Last pair this observer actually acted on, used to tell a real user change
  # from an echo. The mutual-exclusion observers above call updateSelectInput()
  # on the opposite picker whenever either changes, and updating `choices` makes
  # the client re-send that input's value even when the selection is identical.
  # Every echo re-entered this observer and bumped preview_generation(), so an
  # echo arriving during a retrieval invalidated the in-flight preview: the
  # result was dropped by the generation guards in the status observer, with no
  # notification, the button reset to "Preview on map" and Join stayed greyed
  # out. That is the intermittent "clicking Preview does nothing".
  selected_pair <- reactiveVal(NULL)

  observeEvent(list(input$base_layer, input$overlay_source), {
    pair <- c(input$base_layer, input$overlay_source)
    if (identical(selected_pair(), pair)) {
      return()
    }
    selected_pair(pair)
    preview_generation(preview_generation() + 1L)
    link_generation(link_generation() + 1L)
    if (identical(link_state(), "running")) {
      link_state("cancelled")
      link_state_detail("Inputs changed; the previous run was discarded.")
    } else if (!identical(link_state(), "cancelled")) {
      link_state("idle")
      link_state_detail(NULL)
    }
    if (identical(preview_state(), "running")) {
      preview_state("cancelled")
      preview_state_detail("Inputs changed; the previous preview was discarded.")
    } else if (!identical(preview_state(), "cancelled")) {
      preview_state("idle")
      preview_state_detail(NULL)
    }
    cw_result$crosswalk <- NULL
    cw_result$linked <- NULL
    cw_result$pairs <- NULL
    cw_result$base_sf <- NULL
    cw_result$overlay_sf <- NULL
    cw_result$previewed <- NULL
  }, ignoreInit = TRUE)

  # Link is gated on having previewed the CURRENT pair: enabled only when a
  # preview succeeded for exactly today's base_layer/overlay_source values
  # (so changing either picker re-greys it). Every geometry pair is linkable
  # now - point-to-point runs nearest matching - so there is no pair gate.
  output$build_btn_ui <- renderUI({
    req(input$base_layer, input$overlay_source)
    previewed_current <- identical(cw_result$previewed, c(input$base_layer, input$overlay_source))
    build_running <- identical(link_state(), "running")

    if (build_running) {
      tags$button("Running...", type = "button",
        class = "btn btn-primary w-100", disabled = "disabled")
    } else if (previewed_current) {
      actionButton("build_btn", "Join", class = "btn-primary w-100")
    } else {
      tags$button("Join", type = "button", class = "btn btn-primary w-100", disabled = "disabled")
    }
  })

  # Retrieves and maps the two selected layers without linking them, so users
  # can see what they picked before committing to a (possibly slow) link run.
  # Previewing is just mapping - no containment semantics, no raster/method
  # branching - since it never calls build_crosswalk()/link().
  observe({
    label <- if (identical(preview_task$status(), "running")) "Running..." else "Preview on map"
    updateActionButton(session, "preview_btn", label = label)
  })

  observeEvent(input$preview_btn, {
    req(input$base_layer, input$overlay_source)
    target <- overlay_request()
    if (!is.null(target$error) || is.null(target$layer) &&
        target$id %in% c("postal_upload", "custom_target")) {
      showNotification(target$error %||% "The target layer is not ready.",
        type = "error", duration = NULL)
      return()
    }
    generation <- preview_generation()
    preview_active_generation(generation)
    path <- new_progress_path()
    preview_progress_path(path)
    preview_progress_log(character())
    preview_state("running")
    preview_state_detail("Starting.")
    preview_task$invoke(input$base_layer, input$overlay_source, generation,
      target$layer, path, requested_bbox())
  })

  observeEvent(preview_task$status(), {
    s <- preview_task$status()
    if (!s %in% c("success", "error")) return()
    if (!identical(preview_active_generation(), preview_generation())) return()
    result <- tryCatch(preview_task$result(), error = function(e) e)
    if (inherits(result, "error")) {
      described <- describe_retrieval_failure(result)
      unlink(isolate(preview_progress_path()) %||% character())
      preview_state("failed")
      preview_state_detail(described$message)
      showNotification(retrieval_failure_notification(described),
        type = "error", duration = NULL)
      # Terminal push made directly: an observeEvent(preview_state()) fired from
      # this ExtendedTask promise-resolution flush does not re-run, so the modal
      # would otherwise hang on the last spinner. See the success path below.
      push_progress(isolate(preview_progress_log()), done = TRUE, ok = FALSE,
        title = "Preview failed.")
      return()
    }
    if (!identical(result$generation, preview_generation())) return()

    # The mapping phase is owned by the main process, after the future returns.
    # Record it in the phase log so the dialog shows it, then unpack the layers,
    # hand them to Leaflet, declare completion, and push the terminal dialog
    # state -- all in this one reactive cycle. The terminal push is made here
    # directly rather than via the observeEvent(preview_state()) below, because a
    # reactiveVal write inside this ExtendedTask promise-resolution flush does
    # not re-run its observers (see the push_progress call at the end of this
    # block). The map data goes out in the same flush, so the dialog's final
    # "done"/OK and the map appear together, driven entirely by the main R
    # process, so it cannot hang waiting on a browser render signal.
    path <- isolate(preview_progress_path())
    if (!is.null(path)) {
      try(cat("Mapping data...\n", file = path, append = TRUE), silent = TRUE)
    }
    phases <- read_progress_phases(path)
    if (is.null(phases)) {
      phases <- c(isolate(preview_progress_log()), "Mapping data...")
    }
    preview_progress_log(phases)

    cw_result$base_sf <- unpack_spatial(result$base_sf)
    cw_result$overlay_sf <- unpack_spatial(result$overlay_sf)
    # A fresh preview invalidates any stale link results - the Data tab
    # goes empty and the crosswalk/linked-csv and reproduce.R downloads
    # disable until Link is run again.
    cw_result$crosswalk <- NULL
    cw_result$linked <- NULL
    cw_result$pairs <- NULL
    # Records exactly what was previewed, so the Join button's renderUI
    # can require the current picker values to match before enabling -
    # changing either picker after a preview re-greys Link. Only set on
    # success; an error path below leaves this untouched.
    cw_result$previewed <- c(result$base_id, result$overlay_id)
    preview_state("completed")
    preview_state_detail("Both layers are on the map.")
    # The terminal push is NOT made here. The map payload goes out in this same
    # flush, but the browser then blocks for seconds drawing it, so declaring
    # "done" now is a claim the user can watch being false. await_preview_render()
    # keeps the last phase spinning and hands the terminal push to
    # input$cw_map_rendered (or to the cap). It is still not routed through an
    # observeEvent(preview_state()): observers invalidated inside this
    # promise-resolution flush are never re-flushed.
    await_preview_render(phases)
    unlink(isolate(preview_progress_path()) %||% character())
  }, ignoreInit = TRUE)

  # Join asks first. build_btn only opens the confirmation; confirm_join_btn
  # below does the work, so nothing is retrieved or computed until the user
  # agrees to the join semantics spelled out in the modal.
  observeEvent(input$build_btn, {
    req(input$base_layer, input$overlay_source)
    showModal(join_confirm_modal(
      source_id = input$base_layer,
      target_id = input$overlay_source,
      source_sf = cw_result$base_sf,
      target_sf = cw_result$overlay_sf,
      kinds = c(geom_kind(input$base_layer), geom_kind(input$overlay_source))
    ))
  })

  observeEvent(input$confirm_join_btn, {
    # Every guard below runs BEFORE the dialog is opened. Opening it first put a
    # box on screen that three separate early returns then abandoned: no task
    # was invoked, no phase was ever written, and finish_progress() never fired,
    # so the user was left with a permanently blank dialog whose only control
    # was Cancel. Reproduced by joining the same pair twice.
    req(input$base_layer, input$overlay_source)
    requested_inputs <- list(
      base_layer = input$base_layer,
      overlay_source = input$overlay_source
    )
    target <- overlay_request()
    if (!is.null(target$error) || is.null(target$layer) &&
        target$id %in% c("postal_upload", "custom_target")) {
      removeModal()
      showNotification(target$error %||% "The target layer is not ready.",
        type = "error", duration = NULL)
      return()
    }
    has_current_result <- !is.null(cw_result$crosswalk) ||
      !is.null(cw_result$linked)
    if (identical(link_state(), "completed") &&
        identical(link_active_inputs(), requested_inputs) &&
        has_current_result) {
      # Nothing to run, but the confirmation dialog is still on screen and must
      # be replaced by something the user can dismiss - not a blank box.
      link_state_detail("Current results are already ready; no work restarted.")
      open_progress_dialog("Join")
      push_progress("Results are already up to date", done = TRUE, ok = TRUE,
        title = NULL)
      return()
    }
    # Do NOT removeModal() here: the confirmation dialog is replaced in place
    # by the progress dialog, so the user never sees a flash of bare page.
    open_progress_dialog("Join")
    generation <- link_generation()
    link_active_generation(generation)
    link_active_inputs(requested_inputs)
    link_state("running")
    link_state_detail("Starting.")
    link_progress_path(new_progress_path())
    link_progress_log(character())
    cw_result$crosswalk <- NULL
    cw_result$linked <- NULL
    cw_result$pairs <- NULL
    build_task$invoke(
      input$base_layer,
      input$overlay_source,
      generation,
      target$layer,
      isolate(link_progress_path()),
      requested_bbox()
    )
  })

  observeEvent(build_task$status(), {
    s <- build_task$status()
    if (!s %in% c("success", "error")) return()
    current_inputs <- list(
      base_layer = input$base_layer,
      overlay_source = input$overlay_source
    )
    if (!identical(link_active_inputs(), current_inputs)) {
      link_state("cancelled")
      link_state_detail("Inputs changed; the previous run was discarded.")
      cw_result$crosswalk <- NULL
      cw_result$linked <- NULL
      cw_result$pairs <- NULL
      return()
    }
    if (!identical(link_active_generation(), link_generation())) return()
    unlink(isolate(link_progress_path()) %||% character())
    result <- tryCatch(build_task$result(), error = function(e) e)
    if (inherits(result, "error")) {
      described <- describe_retrieval_failure(result)
      link_state("failed")
      link_state_detail(described$message)
      showNotification(retrieval_failure_notification(described),
        type = "error", duration = NULL)
      return()
    }
    if (!identical(result$generation, link_generation())) return()
    base_sf <- unpack_spatial(result$base_sf)
    overlay_sf <- unpack_spatial(result$overlay_sf)
    cw_result$crosswalk <- normalize_mixed_crosswalk(
      result$crosswalk, base_sf, overlay_sf
    )
    cw_result$linked      <- result$linked
    cw_result$pairs       <- result$pairs
    cw_result$base_sf     <- base_sf
    cw_result$overlay_sf  <- overlay_sf
    link_state("completed")
    link_state_detail("Results and downloads are ready.")
    bslib::nav_show("main_tabs", "data", session = session)
  }, ignoreInit = TRUE)

  # The map renders at app load - before any preview - carrying
  # only the furniture layers, so the Leaflet widget always exists. After a
  # preview the two styled sources join it, above the tiles and above the
  # furniture in the overlay list. The view is pinned to the Ontario-wide
  # extent of the bundled PHU outline on every render and is never re-fit
  # to the selected sources' extent.
  link_map <- reactive({
    layers <- list()
    styles <- list()
    layer_labels <- c(
      "Source layer" = source_layer_display_name(input$base_layer),
      "Target layer" = target_layer_display_name(
        input$overlay_source, input$overlay_own_file
      )
    )
    if (!is.null(cw_result$base_sf) && !is.null(cw_result$overlay_sf)) {
      layers <- list("Source layer" = cw_result$base_sf, "Target layer" = cw_result$overlay_sf)
      styles <- list(
        "Source layer" = read_layer_style(input, "base", layer_geom(cw_result$base_sf), accent = "#2a78d6"),
        "Target layer" = read_layer_style(input, "overlay", layer_geom(cw_result$overlay_sf), accent = "#1baf7a")
      )
    }
    # Suppress PHU_simple only when phu_boundaries is actually DRAWN, not
    # merely selected in a picker. phu_boundaries is the app's default base
    # layer, so keying suppression off the selection hid the furniture on
    # every fresh load - the exact case it exists for. Nothing is drawn from
    # a selection until a preview succeeds.
    drawn_ids <- if (length(layers)) cw_result$previewed else character(0)
    furniture <- furniture_layers(drawn_ids)
    # A nearest (point-to-point) result draws connector lines between matched
    # points; add_nearest_connectors() also owns the layers control in that
    # case, so render_styled_map() skips its own (exactly one control either way).
    nearest_run <- is_nearest_result(cw_result$pairs)
    map <- render_styled_map(
      layers, styles,
      add_control = !nearest_run,
      furniture = furniture,
      layer_labels = layer_labels
    )
    if (nearest_run) {
      connectors <- nearest_connectors(cw_result$base_sf, cw_result$overlay_sf, cw_result$pairs)
      conn_style <- list(color = "#52514e", weight = 1, opacity = 0.7)
      map <- add_nearest_connectors(
        map, layers, connectors, conn_style, furniture = furniture,
        layer_labels = layer_labels
      )
    }
    ontario <- sf::st_bbox(furniture_layer("PHU_simple"))
    # Ontario's full bbox runs up to Hudson Bay, so fitting it exactly leaves
    # the populated south small on screen. Tighten the fitted extent toward
    # its centre so the default view starts a little closer. Raise
    # `zoom_inset` to zoom in further, set it to 0 to fit the whole province.
    zoom_inset <- 0.12
    inset_x <- (ontario[["xmax"]] - ontario[["xmin"]]) * zoom_inset / 2
    inset_y <- (ontario[["ymax"]] - ontario[["ymin"]]) * zoom_inset / 2
    map <- leaflet::fitBounds(
      map,
      lng1 = ontario[["xmin"]] + inset_x, lat1 = ontario[["ymin"]] + inset_y,
      lng2 = ontario[["xmax"]] - inset_x, lat2 = ontario[["ymax"]] - inset_y
    )
    map
  })

  # The widget reports back when it has finished drawing. Leaflet renders the
  # whole layer set synchronously on the browser's single JS thread, so a
  # preview of 11,625 points blocks for seconds AFTER the server has sent the
  # payload. Without this signal the dialog claimed "Mapping data - done" and
  # revealed OK at send time, and the user's OK click then queued behind the
  # render - the map appeared long before the dialog would close.
  #
  # requestAnimationFrame + setTimeout(0) defers the signal past the paint that
  # follows the render call, so the acknowledgement means "drawn", not "queued".
  output$cw_map <- renderLeaflet({
    htmlwidgets::onRender(link_map(), "
      function(el, x) {
        var map = this;
        var resizeMap = function() {
          if (!el || el.offsetParent === null || !map || !map.invalidateSize) return;
          map.invalidateSize({pan: false, animate: false});
        };
        // The widget is initially rendered while the Map tab is visible, but
        // it can be hidden while Data is active. Leaflet measures a hidden
        // container as zero-sized and then retains that measurement when the
        // tab is shown again, which makes the tile pane appear while the
        // feature panes are outside the visible map. Re-measure after the
        // Bootstrap tab transition and after a window resize. The handler is
        // scoped to this widget element so a re-render cannot register it
        // twice on the same node.
        if (!el.__ongeorMapLifecycleBound) {
          el.__ongeorMapLifecycleBound = true;
          document.addEventListener('shown.bs.tab', resizeMap);
          window.addEventListener('resize', resizeMap);
        }
        requestAnimationFrame(function() {
          setTimeout(resizeMap, 0);
        });
        requestAnimationFrame(function() {
          setTimeout(function() {
            if (window.Shiny && Shiny.setInputValue) {
              Shiny.setInputValue('cw_map_rendered', Date.now(), {priority: 'event'});
            }
          }, 0);
        });
      }")
  })
  # Shows whichever mode the last run produced: the target-level table
  # (build_crosswalk, or summarise_by_target() for intersection/nearest) or a
  # linked values table (raster runs via link()).
  output$cw_table <- DT::renderDataTable({
    tbl <- cw_result$crosswalk %||% cw_result$linked
    req(tbl)
    tbl
  }, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 25))

  shp_field_names <- function(nms) {
    out <- character(0)
    for (nm in nms) {
      candidate <- substr(nm, 1L, 10L)
      if (candidate %in% out) {
        k <- 1L
        repeat {
          candidate <- sprintf("%s%03d", substr(nm, 1L, 7L), k)
          if (!candidate %in% out) break
          k <- k + 1L
        }
      }
      out <- c(out, candidate)
    }
    out
  }

  # Shape preserves both GeoPackage and Shapefile exports in one archive.
  output$dl_cw_target <- downloadHandler(
    filename = function() "target_shapes.zip",
    content = function(file) {
      merged <- merge_target_attributes(
        cw_result$overlay_sf, cw_result$crosswalk, cw_result$linked
      )
      req(merged)
      staging <- tempfile("shapes_stage")
      dir.create(staging)
      on.exit(unlink(staging, recursive = TRUE), add = TRUE)
      # GPKG refuses to write into an existing file, so let delete_dsn clear it.
      sf::st_write(
        merged, file.path(staging, "target.gpkg"), layer = "target", driver = "GPKG",
        delete_dsn = TRUE, quiet = TRUE
      )
      original <- setdiff(names(merged), attr(merged, "sf_column"))
      all_names <- names(merged)
      all_names[match(original, all_names)] <- shp_field_names(original)
      names(merged) <- all_names
      shp_path <- file.path(staging, "target.shp")
      suppressWarnings(sf::st_write(merged, shp_path, quiet = TRUE))
      stored <- sf::st_read(shp_path, quiet = TRUE)
      utils::write.csv(
        data.frame(
          original = original,
          truncated = setdiff(names(stored), attr(stored, "sf_column"))
        ),
        file.path(staging, "field_names.csv"),
        row.names = FALSE
      )
      zip_directory_to(staging, file)
    }
  )

  output$dl_cw_csv <- downloadHandler(
    filename = function() "target_tables.zip",
    content = function(file) {
      tbl <- cw_result$crosswalk %||% cw_result$linked
      req(tbl)
      staging <- tempfile("tables_stage")
      dir.create(staging)
      on.exit(unlink(staging, recursive = TRUE), add = TRUE)
      table_name <- if (!is.null(cw_result$linked)) "linked.csv" else "mapping.csv"
      utils::write.csv(tbl, file.path(staging, table_name), row.names = FALSE)
      utils::write.csv(
        cw_result$pairs %||% data.frame(),
        file.path(staging, "pairs.csv"), row.names = FALSE
      )
      zip_directory_to(staging, file)
    }
  )
  output$dl_cw_map <- downloadHandler(
    filename = function() "map.html",
    content = function(file) {
      req(cw_result$base_sf, cw_result$overlay_sf)
      htmlwidgets::saveWidget(link_map(), file, selfcontained = TRUE)
    }
  )
  output$dl_cw_script <- downloadHandler(
    filename = function() "reproduce.R",
    content = function(file) {
      req(input$base_layer, input$overlay_source)
      # Mirror the build task's universal direction rule (overlay is always
      # `from`, base always `to`). Containment linking has no rule to choose,
      # so the script records build_crosswalk()'s default (within). Only
      # build_crosswalk runs offer this script - see link_downloads_ui.
      writeLines(
        ONgeoR::render_reproducer_script(
          input$overlay_source, input$base_layer, ".",
          method = "within"
        ),
        file
      )
    }
  )

  output$link_downloads_ui <- renderUI({
    has_rows <- function(x) !is.null(x) && nrow(x) > 0
    link_ready <- has_rows(cw_result$crosswalk) || has_rows(cw_result$linked)
    # The Shape download merges the joined attributes onto the target
    # geometry, so it needs both a vector target layer and a joined table.
    # A raster target (SpatRaster) has no attribute table to merge onto, so
    # it stays disabled.
    shapes_ready <- link_ready && !is.null(cw_result$overlay_sf) &&
      !inherits(cw_result$overlay_sf, "SpatRaster")
    tagList(
      # A compact 2x2 grid keeps each deliverable at the same visual weight.
      download_or_disabled(list(
        list(id = "dl_cw_csv", label = "Table", title = "target_tables.zip",
          ready = link_ready),
        list(id = "dl_cw_target", label = "Shape", title = "target_shapes.zip",
          ready = shapes_ready),
        list(id = "dl_cw_map", label = "Map", title = "map.html",
          ready = !is.null(cw_result$base_sf)),
        # reproduce.R renders a build_crosswalk script only. Raster runs
        # produce a linked values table through link(), and intersection /
        # nearest runs produce tables the script cannot rebuild, so it stays
        # disabled for all of those (a populated pairs table marks the latter).
        list(id = "dl_cw_script", label = "Script", title = "reproduce.R",
          ready = has_rows(cw_result$crosswalk) && is.null(cw_result$pairs) &&
            !identical(input$overlay_source, "postal_upload"))
      )),
      # The exported map carries the furniture layers too; say so here so
      # the addition is not silent.
      tags$p(class = "text-muted",
        paste("map.html also includes the bundled PHU_simple and HIVE",
          "reference layers (each hidden only while its full-resolution",
          "source counterpart is selected)."))
    )
  })

}

shiny::shinyApp(ui, server)
