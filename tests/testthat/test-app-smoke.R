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

  # The branding moved out of the (now removed) header bar and into the top of
  # the sidebar, wrapped in .sidebar-brand. bslib renders the sidebar column as
  # .bslib-sidebar-layout > .sidebar; .sidebar alone is unambiguous here since
  # the page has exactly one sidebar. Assert the logo lives there.
  sidebar <- app$get_html(".sidebar")
  expect_match(sidebar, "sidebar-brand", fixed = TRUE)
  expect_match(sidebar, "logo.png", fixed = TRUE)

  # With no title argument, page_sidebar() renders no header bar at all - that
  # is the point of the change. Assert neither flavour of navbar element (the
  # div.navbar page_sidebar used to emit, nor a nav.navbar) is present.
  expect_true(app$get_js(
    "document.querySelector('div.navbar, nav.navbar') === null"
  ))

  # The app still has only the Map and Data views, so the old single-item
  # "Link" tab must not have come back; scope the check to the tab strip so
  # unrelated controls cannot trip it.
  expect_no_match(app$get_html(".nav-tabs"), "Link", fixed = TRUE)

  expect_match(app$get_html("#base_layer"), "base_layer", fixed = TRUE)
  expect_match(
    app$get_html("#overlay_source"),
    "overlay_source",
    fixed = TRUE
  )
  expect_true(app$get_js(
    "document.querySelector('#build_btn_ui button').disabled"
  ))
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
