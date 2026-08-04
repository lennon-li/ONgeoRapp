#!/usr/bin/env Rscript
# tools/take_screenshots.R
#
# Generates the four man/figures/ screenshots for the ONgeoR pkgdown site.
# Run from the project root:
#   Rscript tools/take_screenshots.R
#
# Requirements:
#   - shinytest2 >= 0.5.1, chromote, shiny, bslib, DT, leaflet installed
#   - Google Chrome available (headless)
#   - Cache at tools::R_user_dir("ONgeoR","cache") warm for
#     phu_boundaries and moh_service_locations (do NOT pass refresh=TRUE)
#   - ONgeoR installed (install.packages(".", repos=NULL, type="source"))
#
# Capture targets
#   man/figures/app-link-tab.png      app at load; Join button greyed out
#   man/figures/app-preview-map.png   After successful preview; both layers on Map
#   man/figures/app-join-confirm.png  Join confirmation modal
#   man/figures/app-data-tab.png      Data sub-tab after join completes
#
# shinytest2 0.5.1 API notes:
#   - AppDriver$new(..., wait = FALSE) skips the initial wait_for_idle() call
#     (the app's ExtendedTask async machinery keeps Shiny "busy" at startup).
#   - get_js(script) evaluates a JS EXPRESSION (not a function body) and
#     returns its value.  Do NOT use `return` statements.
#   - run_js(script) evaluates a JS statement for side-effects.
#   - Page.navigate has a 10s default timeout; raise Chromote$default_timeout
#     to 60 s BEFORE creating the AppDriver.

message("=== ONgeoR screenshot generator ===")

stopifnot(
  requireNamespace("shinytest2", quietly = TRUE),
  requireNamespace("chromote",   quietly = TRUE)
)

# Resolve the Shiny app directory.
source_app <- file.path(getwd(), "inst", "shiny")
if (dir.exists(source_app)) {
  app_dir <- source_app
} else {
  app_dir <- system.file("shiny", package = "ONgeoRapp", mustWork = TRUE)
}
message("App dir: ", app_dir)

fig_dir <- file.path(getwd(), "man", "figures")
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------------
# Helper: unlink before screenshot (AppDriver errors on existing file).
# ---------------------------------------------------------------------------
clean_shot <- function(app, path, delay = 0.5) {
  unlink(path)
  if (delay > 0) Sys.sleep(delay)
  app$get_screenshot(path)
  message("  captured: ", path)
  invisible(path)
}

# Null coalescing operator (not in base R before 4.4)
`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1L && is.na(a))) b else a

# ---------------------------------------------------------------------------
# JS helper: evaluate a JS expression (no `return` keyword), return value.
# ---------------------------------------------------------------------------
js_get <- function(app, expr) {
  tryCatch(app$get_js(expr), error = function(e) {
    message("  [js_get error] ", e$message)
    NULL
  })
}

# ---------------------------------------------------------------------------
# Configure chromote: --no-sandbox and raise default_timeout from 10 s to 60 s.
# ---------------------------------------------------------------------------
{
  cr <- chromote::Chromote$new(
    chromote::Chrome$new(
      args = c(chromote::get_chrome_args(), "--no-sandbox", "--disable-dev-shm-usage")
    )
  )
  cr$default_timeout <- 60
  chromote::set_default_chromote_object(cr)
  message("Chromote configured: --no-sandbox, default_timeout=60s")
}

# ---------------------------------------------------------------------------
# Start the app.
# wait = FALSE: skip the initial wait_for_idle(); the app's ExtendedTask
# async machinery keeps Shiny "busy" at startup which would cause wait_for_idle
# to error immediately.
# ---------------------------------------------------------------------------
message("Starting AppDriver (1440x900, wait=FALSE) ...")
app <- shinytest2::AppDriver$new(
  app_dir,
  width        = 1440,
  height       = 900,
  load_timeout = 90000,
  timeout      = 180000,
  wait         = FALSE
)
message("AppDriver started.")

on.exit({
  message("Stopping app ...")
  try(app$stop(), silent = TRUE)
}, add = TRUE)

# Wait until the session is ready: preview_btn must exist and be enabled.
# A blind sleep is not enough - clicks issued before the observers are bound
# are lost and preview never starts.
{
  waited <- 0
  repeat {
    Sys.sleep(2)
    waited <- waited + 2
    ready <- js_get(app,
      "(function() {
         var b = document.getElementById('preview_btn');
         return (b && !b.disabled) ? 'yes' : 'no';
       })()"
    )
    if (identical(ready, "yes")) break
    if (waited >= 60) stop("Preview button never became enabled.")
  }
  message("Session ready after ", waited, " s.")
}

# ---------------------------------------------------------------------------
# Shot 1: app-link-tab.png
# Initial state: Join button is disabled (no preview has run yet).
# ---------------------------------------------------------------------------
message("--- Shot 1: app-link-tab.png (initial state, Join disabled) ---")
Sys.sleep(1)
clean_shot(app, file.path(fig_dir, "app-link-tab.png"), delay = 0)

# ---------------------------------------------------------------------------
# Click Preview on map and wait for it to finish.
#
# The server observer sets the preview button label to "Running..." while the
# ExtendedTask runs, and back to "Preview on map" when done.  When preview
# succeeds, renderUI emits an actionButton with id="build_btn" (the enabled
# Join button); before that, it emits a plain disabled <button> without that id.
#
# Poll get_js("document.getElementById('build_btn') ? 'enabled' : 'absent'")
# which reliably transitions from "absent" to "enabled" after a successful preview.
# Budget: 300 s.  A warm cache completes in ~60 s, but bailing out early
# yields a partial screenshot set, so this limit is a backstop against a
# genuine hang, not a performance expectation.
# ---------------------------------------------------------------------------
message("--- Clicking 'Preview on map' ---")
app$click("preview_btn")
message("Clicked. Polling for Join button to become enabled (up to 300 s) ...")

waited  <- 0
interval <- 5
limit    <- 300

repeat {
  Sys.sleep(interval)
  waited <- waited + interval

  join_state <- js_get(app,
    "document.getElementById('build_btn') ? 'enabled' : 'absent'"
  )
  preview_label <- js_get(app,
    "(document.getElementById('preview_btn') || {innerText: '?'}).innerText"
  )

  message(sprintf("  [%3ds] build_btn=%s  preview_label=%s", waited,
                  if (is.null(join_state)) "NULL" else join_state,
                  if (is.null(preview_label)) "NULL" else preview_label))

  if (identical(join_state, "enabled")) {
    message("  Preview complete: Join button is enabled.")
    break
  }

  if (waited >= limit) {
    message("WARNING: Preview did not finish within ", limit, " s.")
    message("  Captured: man/figures/app-link-tab.png")
    message("  Tip: ensure cache is warm for phu_boundaries + moh_service_locations.")
    quit(save = "no", status = 0)
  }
}

# Allow leaflet tiles a moment to render before screenshot
Sys.sleep(3)

# ---------------------------------------------------------------------------
# Shot 2: app-preview-map.png
# Map sub-tab with both layers drawn.
# ---------------------------------------------------------------------------
message("--- Shot 2: app-preview-map.png (Map tab, both layers drawn) ---")
clean_shot(app, file.path(fig_dir, "app-preview-map.png"), delay = 1)

# ---------------------------------------------------------------------------
# Shot 3: app-join-confirm.png
# Click the enabled Join button to open the confirmation modal.
# ---------------------------------------------------------------------------
message("--- Clicking Join to open confirmation modal ---")
app$click("build_btn")
Sys.sleep(2)   # give the modal time to render

message("--- Shot 3: app-join-confirm.png (modal open) ---")
clean_shot(app, file.path(fig_dir, "app-join-confirm.png"), delay = 0.5)

# ---------------------------------------------------------------------------
# Shot 4: app-data-tab.png
# Confirm the join, wait for it to complete, switch to the Data sub-tab.
# ---------------------------------------------------------------------------
message("--- Clicking 'Run join' to confirm ---")
app$click("confirm_join_btn")

message("Waiting for join to complete (up to 240 s) ...")
waited <- 0
repeat {
  Sys.sleep(5)
  waited <- waited + 5

  # The confirmed download button for the CSV result is dl_cw_csv.
  # It transitions from disabled-wrapper to an actual downloadButton anchor
  # once the crosswalk is ready.  Check for the enabled anchor tag:
  dl_ready <- js_get(app,
    "(function() {
       var el = document.getElementById('dl_cw_csv');
       return el ? 'yes' : 'no';
     })()"
  )

  # Also probe link_task_status text (shows "Results and downloads are ready.")
  status_text <- js_get(app,
    "(document.getElementById('link_task_status') || {innerText: ''}).innerText.trim()"
  )

  message(sprintf("  [%3ds] dl_cw_csv=%s  status=%s", waited,
                  if (is.null(dl_ready)) "NULL" else dl_ready,
                  if (is.null(status_text) || nchar(status_text) == 0) "(empty)" else status_text))

  if (identical(dl_ready, "yes") ||
      grepl("ready", status_text %||% "", ignore.case = TRUE)) {
    message("  Join complete.")
    break
  }

  if (waited >= 240) {
    message("WARNING: Join did not complete within 240 s.")
    message("  Captured: app-link-tab.png, app-preview-map.png, app-join-confirm.png.")
    quit(save = "no", status = 0)
  }
}

# Click the Data sub-tab
message("--- Switching to Data sub-tab ---")
tryCatch(
  app$run_js(
    "var tabs = document.querySelectorAll('.nav-link');
     for (var i = 0; i < tabs.length; i++) {
       if ((tabs[i].innerText || tabs[i].textContent || '').trim() === 'Data') {
         tabs[i].click(); break;
       }
     }"
  ),
  error = function(e) message("  run_js error: ", e$message)
)
Sys.sleep(2)

message("--- Shot 4: app-data-tab.png (Data tab, result table) ---")
clean_shot(app, file.path(fig_dir, "app-data-tab.png"), delay = 0.5)

# ---------------------------------------------------------------------------
# Done
# ---------------------------------------------------------------------------
message("")
message("=== All screenshots saved to ", fig_dir, "/ ===")
cat(paste(list.files(fig_dir, pattern = "\\.png$", full.names = TRUE), collapse = "\n"), "\n")
