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
# Progress-dialog helpers.
#
# Since 8fc00a5 / 686394f every run opens a MODAL progress dialog that stays on
# screen until the user clicks OK. That changed what this script has to do in
# two ways, and both were breaking it:
#
#   1. The dialog is the app's own completion signal. OK (#progress_ok_btn) has
#      class d-none for the whole run and is revealed only when the run is
#      genuinely done - for a preview, only after the BROWSER reports the map
#      drawn (cw_map_rendered). That is a stronger oracle than the presence of
#      #build_btn, which says the server sent a result, not that it is visible.
#   2. A modal left open sits on top of everything, so any screenshot taken
#      while it is up shows the dialog instead of the app, and clicks aimed at
#      the page behind it are swallowed by the backdrop.
# ---------------------------------------------------------------------------
dialog_done <- function(app) {
  identical(js_get(app,
    "(function() {
       var ok = document.getElementById('progress_ok_btn');
       return (ok && !ok.classList.contains('d-none')) ? 'yes' : 'no';
     })()"), "yes")
}

# Wait for the dialog to report completion. Returns TRUE on success, FALSE on
# timeout, so the caller decides whether a partial set is acceptable.
wait_for_dialog <- function(app, what, limit = 300, interval = 5) {
  waited <- 0
  repeat {
    Sys.sleep(interval)
    waited <- waited + interval
    done <- dialog_done(app)
    phases <- js_get(app,
      "(document.getElementById('task_phase_list') || {innerText: ''}).innerText.replace(/\\s+/g, ' ').trim()")
    message(sprintf("  [%3ds] %s done=%s | %s", waited, what,
      if (done) "yes" else "no",
      substr(if (is.null(phases) || !nzchar(phases)) "(no phases yet)" else phases, 1, 90)))
    if (done) return(TRUE)
    if (waited >= limit) {
      message("WARNING: ", what, " did not finish within ", limit, " s.")
      return(FALSE)
    }
  }
}

# Click OK and wait for the modal to actually leave the DOM. Bootstrap animates
# the fade-out, so a fixed sleep either wastes time or shoots the backdrop.
dismiss_dialog <- function(app, limit = 30) {
  if (!dialog_done(app)) message("  note: dismissing a dialog that has not reported done")
  try(app$click("progress_ok_btn"), silent = TRUE)
  waited <- 0
  repeat {
    Sys.sleep(1)
    waited <- waited + 1
    gone <- js_get(app,
      "(function() {
         var m = document.querySelectorAll('.modal.show, .modal-backdrop');
         return m.length === 0 ? 'yes' : 'no';
       })()")
    if (identical(gone, "yes")) {
      message("  dialog dismissed after ", waited, " s.")
      return(invisible(TRUE))
    }
    if (waited >= limit) {
      message("  WARNING: modal still present after ", limit, " s.")
      return(invisible(FALSE))
    }
  }
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
message("Clicked. Waiting for the progress dialog to report completion (up to 300 s) ...")

if (!wait_for_dialog(app, "preview", limit = 300)) {
  message("  Captured: man/figures/app-link-tab.png")
  message("  Tip: ensure cache is warm for phu_boundaries + moh_service_locations.")
  quit(save = "no", status = 0)
}

# Cross-check the server agrees the Join button now exists. If the dialog says
# done but build_btn is absent, something is wrong that a screenshot would hide.
join_state <- js_get(app, "document.getElementById('build_btn') ? 'enabled' : 'absent'")
message("  build_btn after completion: ", join_state %||% "NULL")
if (!identical(join_state, "enabled")) {
  stop("Dialog reported done but #build_btn is absent - refusing to capture a misleading shot.")
}

dismiss_dialog(app)

# Leaflet tiles finish drawing after the dialog closes.
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

# The confirmation box becomes the progress box in place, so the same dialog
# oracle applies to the join.
message("Waiting for the join to report completion (up to 240 s) ...")
if (!wait_for_dialog(app, "join", limit = 240)) {
  message("  Captured: app-link-tab.png, app-preview-map.png, app-join-confirm.png.")
  quit(save = "no", status = 0)
}

status_text <- js_get(app,
  "(document.getElementById('link_task_status') || {innerText: ''}).innerText.trim()")
message("  link_task_status: ", if (is.null(status_text) || !nzchar(status_text)) "(empty)" else status_text)

dismiss_dialog(app)

# ---------------------------------------------------------------------------
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
