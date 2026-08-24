testthat::skip_if_not_installed("shinytest2")
testthat::skip_on_cran()

chrome <- tryCatch(
  suppressWarnings(chromote::find_chrome()),
  error = function(cnd) NULL
)
if (is.null(chrome) || !nzchar(chrome)) {
  testthat::skip("Chrome is not available to chromote.")
}

# AppDriver launches the app in a child R process, which resolves ONgeoR from
# the installed library, not from devtools::load_all() in this session. Under
# R CMD check (and CI) the package is installed, so the smoke runs there.
if (exists(".__DEVTOOLS__", envir = asNamespace("ONgeoR"))) {
  testthat::skip(
    "browser smoke needs an installed ONgeoR (runs under R CMD check/CI)"
  )
}

# Chrome leaves a scoped temp directory behind for every launch and never
# reaps it. These accumulate at ~2 per run, and once enough of them pile up in
# TMPDIR, Chrome startup slows until AppDriver's load_timeout expires - the app
# starts fine and simply never signals ready, with no error anywhere.
#
# Measured 2026-07-20, same commit and same machine, varying only TMPDIR
# contents: with 221 leftover directories the suite failed 3 runs out of 3
# (1, 2 and 1 failures); with 0 it passed 22/22 three times running. This is
# also the "detritus in the temp directory" NOTE from R CMD check - that NOTE
# was reporting the fault, not cosmetic noise.
#
# Only directories created during this file are removed, so a developer's own
# Chrome session (which uses the same TMPDIR prefix on Linux) is left alone.
# NB: Chrome writes into TMPDIR itself, not into R's per-session tempdir()
# subdirectory, so clean dirname(tempdir()) - cleaning tempdir() finds nothing.
chrome_tmp_pattern <- "^\\.?com\\.google\\.Chrome"
chrome_tmp_root <- dirname(tempdir())
chrome_tmp_dirs <- function() {
  list.files(
    chrome_tmp_root,
    pattern = chrome_tmp_pattern,
    all.files = TRUE,
    full.names = TRUE
  )
}
chrome_tmp_before <- chrome_tmp_dirs()
withr::defer(
  {
    leaked <- setdiff(chrome_tmp_dirs(), chrome_tmp_before)
    if (length(leaked)) {
      unlink(leaked, recursive = TRUE, force = TRUE)
    }
  },
  envir = testthat::teardown_env()
)

test_that("Shiny app boots and exposes its offline workflow controls", {
  app <- shinytest2::AppDriver$new(
    app_dir = dirname(shiny_app_file()),
    name = "app-smoke",
    variant = NULL,
    load_timeout = 120 * 1000
  )
  withr::defer(app$stop())

  # The branding sits at the top of the sidebar, wrapped in .sidebar-brand -
  # never in a page header.
  sidebar <- app$get_html(".sidebar")
  expect_match(sidebar, "sidebar-brand", fixed = TRUE)
  expect_match(sidebar, "logo.png", fixed = TRUE)

  # No header bar of any kind: neither a navbar element nor the .app-brand div
  # that briefly carried the logo above the content. A header costs vertical
  # space the map needs.
  expect_true(app$get_js(
    "document.querySelector('div.navbar, nav.navbar, .app-brand') === null"
  ))

  # The page is a single-purpose linking interface, so it has no top-level
  # section strip. The nested Map/Data strip remains inside the sidebar layout.
  expect_equal(
    app$get_js(
      "document.querySelector('.top-nav') === null"
    ),
    TRUE
  )
  expect_equal(
    app$get_js(
      "Array.from(document.querySelectorAll('.bslib-sidebar-layout .nav-tabs > li'))
         .map(function(li) { return li.textContent.trim(); }).join('|')"
    ),
    "Map|Data"
  )

  expect_match(app$get_html("#base_layer"), "base_layer", fixed = TRUE)
  expect_match(
    app$get_html("#overlay_source"),
    "overlay_source",
    fixed = TRUE
  )
  expect_true(app$get_js(
    "document.querySelector('#build_btn_ui button').disabled"
  ))

  # The map is rendered before any preview and can later be hidden by the Data
  # tab. The client hook must re-measure Leaflet after the tab becomes visible
  # so feature panes are not retained outside the visible map viewport.
  app_impl <- paste(
    readLines(file.path(dirname(shiny_app_file()), "app_impl.R"), warn = FALSE),
    collapse = "\n"
  )
  expect_match(app_impl, "invalidateSize", fixed = TRUE)
  expect_match(app_impl, "shown.bs.tab", fixed = TRUE)
})

# The map renders at app load with its furniture layers (the
# bundled PHU_simple boundary outline and the HIVE grid) and no preview, so
# the Leaflet widget and its basemap control exist immediately with no
# retrieval and no network. Basemap switching is therefore exercised on the
# initial furniture map directly and this test runs offline - there is no
# opt-in gate.
test_that("native map control switches every core basemap including None", {
  app <- shinytest2::AppDriver$new(
    app_dir = dirname(shiny_app_file()),
    name = "app-basemap-switching",
    variant = NULL,
    load_timeout = 180 * 1000
  )
  withr::defer(app$stop())

  app$wait_for_js(
    paste0(
      "document.querySelector(",
      "'#cw_map .leaflet-control-layers-base') !== null"
    ),
    timeout = 60 * 1000
  )

  label_selector <- "#cw_map .leaflet-control-layers-base label"
  tile_selector <- "#cw_map .leaflet-tile-pane img.leaflet-tile"

  select_basemap <- function(group, has_tiles) {
    click_script <- sprintf(
      paste0(
        "(function() {",
        "const labels = Array.from(document.querySelectorAll('%s'));",
        "const label = labels.find(function(item) {",
        "return item.textContent.trim() === '%s';",
        "});",
        "if (!label) return false;",
        "label.querySelector('input').click();",
        "return true;",
        "})()"
      ),
      label_selector,
      group
    )
    expect_true(app$get_js(click_script))

    selected_script <- sprintf(
      paste0(
        "(function() {",
        "const labels = Array.from(document.querySelectorAll('%s'));",
        "const selected = labels.find(function(item) {",
        "return item.querySelector('input').checked;",
        "});",
        "return selected ? selected.textContent.trim() : null;",
        "})()"
      ),
      label_selector
    )
    app$wait_for_js(
      sprintf("%s === '%s'", selected_script, group),
      timeout = 20 * 1000
    )
    expect_identical(app$get_js(selected_script), group)

    tile_count_script <- sprintf(
      "document.querySelectorAll('%s').length",
      tile_selector
    )
    if (has_tiles) {
      app$wait_for_js(
        sprintf("%s > 0", tile_count_script),
        timeout = 20 * 1000
      )
      expect_gt(app$get_js(tile_count_script), 0)
    } else {
      app$wait_for_js(
        sprintf("%s === 0", tile_count_script),
        timeout = 20 * 1000
      )
      expect_equal(app$get_js(tile_count_script), 0)
    }
  }

  select_basemap("Dark", has_tiles = TRUE)
  select_basemap("Light", has_tiles = TRUE)
  select_basemap("OpenStreetMap", has_tiles = TRUE)
  select_basemap("Satellite", has_tiles = TRUE)
  select_basemap("None", has_tiles = FALSE)
})
