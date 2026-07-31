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
# Any geography_type outside boundary/facility/raster is bucketed into an
# "Other" group, which is only added if such sources exist.
source_choices_grouped <- function() {
  sources <- ONgeoR::list_sources()
  labels <- source_choice_labels(sources)
  values <- stats::setNames(sources$source_id, labels)

  group_of <- vapply(sources$geography_type, function(gt) {
    switch(gt,
      boundary = "Polygons",
      facility = "Points",
      raster   = "Rasters",
      "Other")
  }, character(1))

  group_order <- c("Polygons", "Points", "Rasters", "Other")
  groups <- lapply(group_order, function(g) values[group_of == g])
  names(groups) <- group_order
  groups[lengths(groups) > 0]
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
  source_name <- ONgeoR::get_source(source_id)$name
  target_name <- ONgeoR::get_source(target_id)$name
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

# Renders real (clickable) downloadButtons when an item's `ready` field is
# TRUE, otherwise visually-matching but non-functional disabled buttons - so
# each sidebar download is only ever clickable once the map/data it points to
# exists. Readiness is per-item (item$ready) so, e.g., map.html can be ready
# before mapping.csv is. A ready button lights up in the primary accent so
# the user can see at a glance which downloads are available.
download_or_disabled <- function(items) {
  tagList(lapply(items, function(item) {
    if (isTRUE(item$ready)) {
      downloadButton(item$id, item$label, class = "btn-primary w-100 mb-1")
    } else {
      tags$button(
        item$label, type = "button", class = "btn btn-outline-secondary w-100 mb-1",
        disabled = "disabled"
      )
    }
  }))
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
  tags$p(
    class = paste("task-status text-muted", paste0("task-status-", state)),
    `data-state` = state,
    tags$strong(paste0(label, ".")),
    if (!is.null(detail)) paste(" ", detail)
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
# "Furniture" layers are bundled reference outlines drawn on the Link tab
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

# Furniture layers ship in inst/extdata and render at app load with no
# network call. PHU_simple starts visible; HIVE starts hidden (unchecked
# in the layers control) but is always present so the user can toggle it
# on without a retrieval round-trip. Each is suppressed when its live,
# full-resolution counterpart is actually drawn as a selected source.
furniture_layers <- function(selected_ids = character()) {
  layers <- list()
  if (!"phu_boundaries" %in% selected_ids) {
    layers[["PHU_simple"]] <- furniture_layer("PHU_simple")
  }
  if (!"hive" %in% selected_ids) {
    layers[["HIVE"]] <- furniture_layer("HIVE")
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
  source_id_col <- ONgeoR:::layer_id_col(source_sf)
  target_id_col <- ONgeoR:::layer_id_col(target_sf)
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

ui <- bslib::page_sidebar(
  window_title = "ONgeoR",
  theme = bslib::bs_theme(version = 5, primary = "#2a78d6", success = "#0ca30c"),
  fillable = FALSE,
  sidebar = bslib::sidebar(
    width = 300,
    tags$div(
      class = "sidebar-brand",
      tags$img(src = "logo.png", alt = "ONgeoR")
    ),
    tags$div(class = "slot-block slot-base",
      selectInput("base_layer", "Source layer", choices = source_choices_grouped(), selected = "phu_boundaries"),
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
    tags$div(class = "slot-block slot-overlay",
      selectInput("overlay_source", "Target layer", choices = source_choices_grouped(), selected = "moh_service_locations"),
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
      )
    ),
    uiOutput("link_relationship"),
    uiOutput("link_method_ui"),
    actionButton("preview_btn", "Preview on map", class = "btn-preview w-100 mb-1"),
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
  tags$head(tags$link(rel = "stylesheet", href = "theme.css")),
  bslib::navset_tab(
    bslib::nav_panel(
      "Map",
      leafletOutput("cw_map", height = "calc(100vh - 60px)")
    ),
    bslib::nav_panel(
      "Data",
      DT::dataTableOutput("cw_table")
    )
  )
)

server <- function(input, output, session) {

  # --- Async tasks (ExtendedTask; requires shiny >= 1.8.0) -----------

  preview_task <- shiny::ExtendedTask$new(function(base_id, overlay_id,
                                                     generation) {
    promises::future_promise({
      # Defined INSIDE the future block on purpose. The app-level
      # pack_spatial() is a closure over the app source environment, which
      # holds the multisession plan object; future's globals scan walks that
      # enclosure, finds an externalptr, and refuses to export the function
      # ("Detected a non-exportable reference"). A local copy has the future
      # block itself as its enclosure and exports cleanly.
      pack <- function(x) if (inherits(x, "SpatRaster")) terra::wrap(x) else x
      base_sf    <- ONgeoR::retrieve_source(base_id)
      overlay_sf <- ONgeoR::retrieve_source(overlay_id)
      list(base_sf = pack(base_sf),
           overlay_sf = pack(overlay_sf),
           base_id = base_id, overlay_id = overlay_id,
           generation = generation)
    })
  })

  build_task <- shiny::ExtendedTask$new(function(base_id, overlay_id,
                                                   generation) {
    promises::future_promise({
      # Local copies, not the app-level pack_spatial()/layer_geom() - see the
      # note in preview_task: those closures enclose the multisession plan and
      # are rejected by future's non-exportable-globals check.
      pack <- function(x) if (inherits(x, "SpatRaster")) terra::wrap(x) else x
      kind_of <- function(layer) {
        if (inherits(layer, "SpatRaster")) return("raster")
        types <- unique(as.character(sf::st_geometry_type(layer)))
        if (all(types %in% c("POINT", "MULTIPOINT"))) "point" else "polygon"
      }
      base_sf    <- ONgeoR::retrieve_source(base_id)
      overlay_sf <- ONgeoR::retrieve_source(overlay_id)
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

  # --- Link tab -------------------------------------------------------

  observeEvent(input$base_layer, {
    groups <- remove_choice_grouped(source_choices_grouped(), input$base_layer)
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

  output$link_task_status <- renderUI({
    task_status_ui(link_state(), link_state_detail())
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
    generation <- preview_generation()
    preview_active_generation(generation)
    preview_task$invoke(input$base_layer, input$overlay_source, generation)
  })

  observeEvent(preview_task$status(), {
    s <- preview_task$status()
    if (!s %in% c("success", "error")) return()
    if (!identical(preview_active_generation(), preview_generation())) return()
    result <- tryCatch(preview_task$result(), error = function(e) e)
    if (inherits(result, "error")) {
      showNotification(conditionMessage(result), type = "error", duration = NULL)
      return()
    }
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
    link_state_detail("Linking selected layers.")
    cw_result$crosswalk <- NULL
    cw_result$linked <- NULL
    cw_result$pairs <- NULL
    build_task$invoke(
      input$base_layer,
      input$overlay_source,
      generation
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
    result <- tryCatch(build_task$result(), error = function(e) e)
    if (inherits(result, "error")) {
      link_state("failed")
      link_state_detail(conditionMessage(result))
      showNotification(conditionMessage(result), type = "error", duration = NULL)
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
  }, ignoreInit = TRUE)

  # The Link tab map renders at app load - before any preview - carrying
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
    leaflet::fitBounds(
      map,
      lng1 = ontario[["xmin"]], lat1 = ontario[["ymin"]],
      lng2 = ontario[["xmax"]], lat2 = ontario[["ymax"]]
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
    tagList(
      download_or_disabled(list(
        list(id = "dl_cw_map", label = "map.html", ready = !is.null(cw_result$base_sf)),
        list(id = "dl_cw_csv", label = csv_label, ready = link_ready),
        list(id = "dl_cw_pairs", label = "pairs.csv", ready = has_rows(cw_result$pairs)),
        # reproduce.R renders a build_crosswalk script only. Raster runs
        # produce a linked values table through link(), and intersection /
        # nearest runs produce tables the script cannot rebuild, so it stays
        # disabled for all of those (a populated pairs table marks the latter).
        list(id = "dl_cw_script", label = "reproduce.R",
          ready = has_rows(cw_result$crosswalk) && is.null(cw_result$pairs))
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
