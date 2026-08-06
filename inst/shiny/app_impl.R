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

target_choices_grouped <- function() {
  sources <- ONgeoR::list_sources()
  sources <- sources[sources$geography_type == "facility", ]
  labels <- source_choice_labels(sources)
  points <- stats::setNames(sources$source_id, labels)
  list("Points" = c(points, "Upload postal codes" = "postal_upload"))
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
  if (identical(source_id, "postal_upload")) return("point")
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
  if (identical(source_id, "postal_upload")) {
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
  if (all(kinds == "polygon") || all(kinds == "point")) {
    n_attr <- function(layer) max(ncol(layer) - 1L, 0L)
    return(17L + n_attr(source_sf) + n_attr(target_sf))
  }
  crosswalk_result_columns
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
  source_name <- if (identical(source_id, "postal_upload")) {
    "Uploaded postal codes"
  } else {
    ONgeoR::get_source(source_id)$name
  }
  target_name <- if (identical(target_id, "postal_upload")) {
    "Uploaded postal codes"
  } else {
    ONgeoR::get_source(target_id)$name
  }
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

# Renders real (clickable) downloadButtons when an item's `ready` field is
# TRUE, otherwise visually-matching but non-functional disabled buttons - so
# each sidebar download is only ever clickable once the map/data it points to
# exists. Readiness is per-item (item$ready) so, e.g., map.html can be ready
# before mapping.csv is. A ready button lights up in the primary accent so
# the user can see at a glance which downloads are available. Buttons sit in
# a two-column grid (row g-1 / col-6); labels are shortened and each item's
# real filename is carried in item$title as a tooltip.
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
          class = "btn-primary w-100 mb-1", title = item$title)
      } else {
        tags$button(
          item$label, type = "button",
          class = "btn btn-outline-secondary w-100 mb-1",
          title = item$title, disabled = "disabled"
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
  coverage <- if (!is.null(crosswalk$coverage)) crosswalk$coverage else rep(NA_real_, n)
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
  # sort() restores original row order of the winning rows; deterministic.
  crosswalk[sort(keep), , drop = FALSE]
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

task_status_ui <- function(state, detail = NULL) {
  labels <- c(
    idle = "Idle",
    running = "Running",
    failed = "Failed",
    cancelled = "Cancelled",
    completed = "Completed"
  )
  label <- unname(labels[[state]])
  tagList(
    tags$p(
      class = paste("task-status text-muted", paste0("task-status-", state)),
      `data-state` = state,
      tags$strong(paste0(label, ".")),
      if (!is.null(detail)) paste(" ", detail)
    ),
    # Indeterminate bar only while work is in flight. The app cannot know a
    # true percentage - retrieval is paginated inside ONgeoR and the join is
    # a single opaque call - so an animated bar plus the phase label in
    # `detail` is the honest signal: something is happening, and this is what.
    if (identical(state, "running")) {
      tags$div(
        class = "progress task-progress", style = "height: 6px;",
        role = "progressbar", `aria-label` = "Task in progress",
        tags$div(
          class = "progress-bar progress-bar-striped progress-bar-animated",
          style = "width: 100%;"
        )
      )
    }
  )
}

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
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    return(NULL)
  }
  line <- tryCatch(readLines(path, warn = FALSE), error = function(e) character())
  line <- line[nzchar(line)]
  if (length(line) == 0L) NULL else line[length(line)]
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
render_styled_map <- function(layers, styles, add_control = TRUE, furniture = list()) {
  map <- base_leaflet_layers(leaflet::leaflet())
  for (nm in names(layers)) {
    map <- add_styled_sf_layer(map, layers[[nm]], nm, styles[[nm]])
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
    overlay_groups <- c(names(layers), names(furniture))
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

add_nearest_connectors <- function(map, layers, connectors, conn_style, furniture = list()) {
  overlay_groups <- c(names(layers), names(furniture))
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
  tags$head(tags$link(rel = "stylesheet", href = "theme.css")),
  # The logo lives at the top of the sidebar, not in a page header. The app is
  # a single-purpose linking interface, so the layout can fill the page
  # directly and a header bar would only cost the map vertical space.
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      tags$div(class = "sidebar-brand",
        tags$img(src = "logo.png", alt = "ONgeoR")),
      tags$div(class = "slot-block slot-overlay",
        selectInput("overlay_source", "1. Target layer (points)", choices = target_choices_grouped(), selected = "moh_service_locations"),
        tags$div(class = "slot-meta",
          uiOutput("overlay_geom_badge"),
          checkboxInput("overlay_upload_own", "Use my own file", FALSE)
        ),
        conditionalPanel(
          "input.overlay_upload_own",
          fileInput("overlay_own_file", NULL, buttonLabel = "Browse...",
            placeholder = "GeoJSON, GeoPackage, zipped shapefile, or GeoTIFF"),
          selectInput("overlay_own_type", "Layer type",
            c("Polygon" = "polygon", "Point" = "point", "Raster" = "raster")),
          tags$p(class = "text-muted", "Upload support is coming soon - this does not affect linking yet.")
        ),
        uiOutput("postal_upload_ui")
      ),
      tags$div(class = "slot-block slot-base",
        selectInput("base_layer", "2. Source layer to join from", choices = source_choices_grouped(), selected = "phu_boundaries"),
        tags$div(class = "slot-meta",
          uiOutput("base_geom_badge"),
          checkboxInput("base_upload_own", "Use my own file", FALSE)
        ),
        conditionalPanel(
          "input.base_upload_own",
          fileInput("base_own_file", NULL, buttonLabel = "Browse...",
            placeholder = "GeoJSON, GeoPackage, zipped shapefile, or GeoTIFF"),
          selectInput("base_own_type", "Layer type",
            c("Polygon" = "polygon", "Point" = "point", "Raster" = "raster")),
          tags$p(class = "text-muted", "Upload support is coming soon - this does not affect linking yet.")
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
                                                     postal_layer = NULL,
                                                     progress_path = NULL) {
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
          try(writeLines(phase, progress_path), silent = TRUE)
        }
      }
      note("Fetching source layer.")
      base_sf    <- ONgeoR::retrieve_source(base_id)
      note("Fetching target layer.")
      overlay_sf <- if (!is.null(postal_layer)) postal_layer else ONgeoR::retrieve_source(overlay_id)
      note("Preparing layers for the map.")
      list(base_sf = pack(base_sf),
           overlay_sf = pack(overlay_sf),
           base_id = base_id, overlay_id = overlay_id,
           generation = generation)
    })
  })

  build_task <- shiny::ExtendedTask$new(function(base_id, overlay_id,
                                                   generation,
                                                   postal_layer = NULL,
                                                   progress_path = NULL) {
    promises::future_promise({
      # Local copies, not the app-level pack_spatial()/layer_geom() - see the
      # note in preview_task: those closures enclose the multisession plan and
      # are rejected by future's non-exportable-globals check.
      pack <- function(x) if (inherits(x, "SpatRaster")) terra::wrap(x) else x
      note <- function(phase) {
        if (!is.null(progress_path)) {
          try(writeLines(phase, progress_path), silent = TRUE)
        }
      }
      kind_of <- function(layer) {
        if (inherits(layer, "SpatRaster")) return("raster")
        types <- unique(as.character(sf::st_geometry_type(layer)))
        if (all(types %in% c("POINT", "MULTIPOINT"))) "point" else "polygon"
      }
      note("Fetching source layer.")
      base_sf    <- ONgeoR::retrieve_source(base_id)
      note("Fetching target layer.")
      overlay_sf <- if (!is.null(postal_layer)) postal_layer else ONgeoR::retrieve_source(overlay_id)
      note("Joining layers.")
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
        crosswalk <- ONgeoR::build_crosswalk(from_sf, to_sf)
        list(crosswalk = crosswalk, linked = NULL, pairs = NULL,
             base_sf = pack(base_sf),
             overlay_sf = pack(overlay_sf),
             generation = generation)
      }
    })
  })

  # --- Layer pickers & preview ---------------------------------------

  observeEvent(input$base_layer, {
    groups <- remove_choice_grouped(target_choices_grouped(), input$base_layer)
    selected <- if (input$overlay_source %in% unlist(groups)) {
      input$overlay_source
    } else {
      first_choice_grouped(groups)
    }
    updateSelectInput(session, "overlay_source", choices = groups, selected = selected)
  }, ignoreInit = TRUE)

  observeEvent(input$overlay_source, {
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

  observeEvent(list(input$postal_file, input$postal_column), {
    if (identical(input$overlay_source, "postal_upload")) {
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
    req(identical(input$overlay_source, "postal_upload"))
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
    task_status_ui(link_state(), link_state_detail())
  })

  output$preview_task_status <- renderUI({
    task_status_ui(preview_state(), preview_state_detail())
  })

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
      phase <- read_progress_phase(isolate(preview_progress_path()))
      if (!is.null(phase) && !identical(phase, isolate(preview_state_detail()))) {
        preview_state_detail(phase)
      }
    }
    if (identical(link_state(), "running")) {
      phase <- read_progress_phase(isolate(link_progress_path()))
      if (!is.null(phase) && !identical(phase, isolate(link_state_detail()))) {
        link_state_detail(phase)
      }
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
    postal_layer <- NULL
    if (identical(input$overlay_source, "postal_upload")) {
      pr <- postal_result()
      req(pr, pr$sf)
      postal_layer <- pr$sf
    }
    generation <- preview_generation()
    preview_active_generation(generation)
    path <- new_progress_path()
    preview_progress_path(path)
    preview_state("running")
    preview_state_detail("Starting.")
    preview_task$invoke(input$base_layer, input$overlay_source, generation,
      postal_layer, path)
  })

  observeEvent(preview_task$status(), {
    s <- preview_task$status()
    if (!s %in% c("success", "error")) return()
    if (!identical(preview_active_generation(), preview_generation())) return()
    unlink(isolate(preview_progress_path()) %||% character())
    result <- tryCatch(preview_task$result(), error = function(e) e)
    if (inherits(result, "error")) {
      described <- describe_retrieval_failure(result)
      preview_state("failed")
      preview_state_detail(described$message)
      showNotification(retrieval_failure_notification(described),
        type = "error", duration = NULL)
      return()
    }
    preview_state("completed")
    preview_state_detail("Both layers are on the map.")
    if (!identical(result$generation, preview_generation())) return()
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
    # No modal here: previewing is a look, not a commitment, so it should not
    # interrupt. The pairing explanation now appears on Join instead, where the
    # user is actually about to spend time and needs to confirm the semantics.
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
    removeModal()
    req(input$base_layer, input$overlay_source)
    requested_inputs <- list(
      base_layer = input$base_layer,
      overlay_source = input$overlay_source
    )
    has_current_result <- !is.null(cw_result$crosswalk) ||
      !is.null(cw_result$linked)
    if (identical(link_state(), "completed") &&
        identical(link_active_inputs(), requested_inputs) &&
        has_current_result) {
      link_state_detail("Current results are already ready; no work restarted.")
      return()
    }
    generation <- link_generation()
    link_active_generation(generation)
    link_active_inputs(requested_inputs)
    link_state("running")
    link_state_detail("Starting.")
    link_progress_path(new_progress_path())
    cw_result$crosswalk <- NULL
    cw_result$linked <- NULL
    cw_result$pairs <- NULL
    postal_layer <- NULL
    if (identical(input$overlay_source, "postal_upload")) {
      pr <- postal_result()
      req(pr, pr$sf)
      postal_layer <- pr$sf
    }
    build_task$invoke(
      input$base_layer,
      input$overlay_source,
      generation,
      postal_layer,
      isolate(link_progress_path())
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
    cw_result$crosswalk   <- result$crosswalk
    cw_result$linked      <- result$linked
    cw_result$pairs       <- result$pairs
    cw_result$base_sf     <- unpack_spatial(result$base_sf)
    cw_result$overlay_sf  <- unpack_spatial(result$overlay_sf)
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
      furniture = furniture
    )
    if (nearest_run) {
      connectors <- nearest_connectors(cw_result$base_sf, cw_result$overlay_sf, cw_result$pairs)
      conn_style <- list(color = "#52514e", weight = 1, opacity = 0.7)
      map <- add_nearest_connectors(map, layers, connectors, conn_style, furniture = furniture)
    }
    ontario <- sf::st_bbox(furniture_layer("PHU_simple"))
    # Ontario's full bbox runs up to Hudson Bay, so fitting it exactly leaves
    # the populated south small on screen. Tighten the fitted extent toward
    # its centre so the default view starts a little closer. Raise
    # `zoom_inset` to zoom in further, set it to 0 to fit the whole province.
    zoom_inset <- 0.12
    inset_x <- (ontario[["xmax"]] - ontario[["xmin"]]) * zoom_inset / 2
    inset_y <- (ontario[["ymax"]] - ontario[["ymin"]]) * zoom_inset / 2
    leaflet::fitBounds(
      map,
      lng1 = ontario[["xmin"]] + inset_x, lat1 = ontario[["ymin"]] + inset_y,
      lng2 = ontario[["xmax"]] - inset_x, lat2 = ontario[["ymax"]] - inset_y
    )
  })

  output$cw_map <- renderLeaflet({
    link_map()
  })
  # Shows whichever mode the last run produced: the target-level table
  # (build_crosswalk, or summarise_by_target() for intersection/nearest) or a
  # linked values table (raster runs via link()).
  output$cw_table <- DT::renderDataTable({
    tbl <- cw_result$crosswalk %||% cw_result$linked
    req(tbl)
    tbl
  }, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 25))

  # Target-layer geometry with the joined attributes merged on, as a
  # GeoPackage (chosen over shapefile because shapefile truncates field names
  # to 10 characters, which would mangle match_distance_km and the src_*/tgt_*
  # attributes summarise_by_target() emits). Geometry must come from
  # cw_result$overlay_sf: the internal names are inverted relative to the UI
  # labels (base_sf is the UI "Source layer", overlay_sf the "Target layer").
  output$dl_cw_target <- downloadHandler(
    filename = function() "target.gpkg",
    content = function(file) {
      merged <- merge_target_attributes(
        cw_result$overlay_sf, cw_result$crosswalk, cw_result$linked
      )
      req(merged)
      # Shiny hands the handler a pre-created temp path; GPKG refuses to write
      # into an existing file, so let delete_dsn clear it first.
      sf::st_write(
        merged, file, layer = "target", driver = "GPKG",
        delete_dsn = TRUE, quiet = TRUE
      )
    }
  )

  # ESRI Shapefile variant of the merge, for tools that cannot read
  # GeoPackage. Shapefile is lossier than GPKG: field names are limited to
  # 10 characters and it cannot distinguish "" from NA. Attribute names are
  # therefore shortened to unique 10-character names before writing (sf's own
  # abbreviation can emit names longer than 10 characters that collide once
  # GDAL truncates them further, which hard-fails the write), and the zip
  # also ships field_names.csv mapping each original name to the name
  # actually found in the written .dbf. A shapefile is 4+ sidecar files
  # (.shp/.shx/.dbf/.prj), so the download is a zip.
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
  output$dl_cw_shp <- downloadHandler(
    filename = function() "target_shapefile.zip",
    content = function(file) {
      merged <- merge_target_attributes(
        cw_result$overlay_sf, cw_result$crosswalk, cw_result$linked
      )
      req(merged)
      original <- setdiff(names(merged), attr(merged, "sf_column"))
      all_names <- names(merged)
      all_names[match(original, all_names)] <- shp_field_names(original)
      names(merged) <- all_names
      staging <- tempfile("shp_stage")
      dir.create(staging)
      on.exit(unlink(staging, recursive = TRUE), add = TRUE)
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
    filename = function() if (!is.null(cw_result$linked)) "linked.csv" else "mapping.csv",
    content = function(file) {
      tbl <- cw_result$crosswalk %||% cw_result$linked
      req(tbl)
      utils::write.csv(tbl, file, row.names = FALSE)
    }
  )
  # The pair-level table (one row per overlapping pair, or per matched point)
  # behind the intersection and nearest results. Only those runs populate it;
  # build_crosswalk and raster runs have no separate pair table.
  output$dl_cw_pairs <- downloadHandler(
    filename = function() "pairs.csv",
    content = function(file) {
      req(cw_result$pairs)
      utils::write.csv(cw_result$pairs, file, row.names = FALSE)
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
    linked_run <- !is.null(cw_result$linked)
    link_ready <- has_rows(cw_result$crosswalk) || has_rows(cw_result$linked)
    csv_label <- if (linked_run) "linked.csv" else "mapping.csv"
    # The Shapes download merges the joined attributes onto the target
    # geometry, so it needs both a vector target layer and a joined table.
    # A raster target (SpatRaster) has no attribute table to merge onto, so
    # it stays disabled.
    shapes_ready <- link_ready && !is.null(cw_result$overlay_sf) &&
      !inherits(cw_result$overlay_sf, "SpatRaster")
    tagList(
      # Two-column grid; SHP sits beside Shapes so the two vector formats
      # pair off and the six buttons fill three even rows.
      download_or_disabled(list(
        list(id = "dl_cw_target", label = "Shapes", title = "target.gpkg",
          ready = shapes_ready),
        list(id = "dl_cw_shp", label = "SHP", title = "target_shapefile.zip",
          ready = shapes_ready),
        list(id = "dl_cw_map", label = "Map", title = "map.html",
          ready = !is.null(cw_result$base_sf)),
        list(id = "dl_cw_csv", label = "Table", title = csv_label,
          ready = link_ready),
        list(id = "dl_cw_pairs", label = "Pairs", title = "pairs.csv",
          ready = has_rows(cw_result$pairs)),
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
