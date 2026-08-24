#!/usr/bin/env Rscript
# tools/verify_postal_upload.R
#
# End-to-end verification of the postal-code upload path: real CSV in ->
# real ONgeoR::resolve_postal_points() -> real Preview -> real Join -> real
# rows in the Data tab. Every other test covering this path (see
# tests/testthat/test-shiny-server.R) mocks resolve_postal_points(), so this
# is the only place that has ever driven the whole thing for real.
#
# Deliberately NOT part of tests/testthat/: it hits live external services
# (the OPCC M1 postal-centroid release and the LIO PHU boundary service),
# same as tools/take_screenshots.R, and this suite's other
# AppDriver/testServer tests all mock retrieve_source()/retrieve_census() so
# R CMD check and CI stay network- and cache-independent. Run this by hand
# when the postal-upload path changes, not as a CI gate.
#
# Run from the project root:
#   Rscript tools/verify_postal_upload.R
#
# Requirements:
#   - shinytest2, chromote, ONgeoR (installed, not devtools::load_all - the
#     app runs in a child process that resolves ONgeoR from the library)
#   - Google Chrome available (headless)
#   - Network access to opendata sources for resolve_postal_points() (OPCC
#     M1) and the default source/target pair (PHU boundaries, MOH service
#     locations) - a warm cache at tools::R_user_dir("ONgeoR", "cache") skips
#     the network round-trip but is not required
#
# Exit status: 0 and "=== PASS ===" on success; a stop() with a specific
# reason otherwise (each step names what it was waiting for).

message("=== ONgeoRapp postal-upload end-to-end verification ===")

stopifnot(
  requireNamespace("shinytest2", quietly = TRUE),
  requireNamespace("chromote",   quietly = TRUE)
)

app_dir <- file.path(getwd(), "inst", "shiny")
if (!dir.exists(app_dir)) {
  app_dir <- system.file("shiny", package = "ONgeoRapp", mustWork = TRUE)
}
message("App dir: ", app_dir)

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1L && is.na(a))) b else a
js_get <- function(app, expr) {
  tryCatch(app$get_js(expr), error = function(e) {
    message("  [js_get error] ", e$message)
    NULL
  })
}

# A handful of real Ontario postal codes spanning several PHUs, so the
# preview draws points in more than one place on the map.
csv_path <- tempfile(fileext = ".csv")
utils::write.csv(
  data.frame(
    name = c("Ottawa City Hall", "Toronto Downtown", "Waterloo", "Hamilton", "Kingston"),
    postal_code = c("K1A 0B1", "M5V 2T6", "N2L 3G1", "L8S 4K1", "K7L 3N6"),
    stringsAsFactors = FALSE
  ),
  csv_path, row.names = FALSE
)
on.exit(unlink(csv_path), add = TRUE)
message("Sample CSV: ", csv_path, " (5 real postal codes)")

# Same --no-sandbox / longer default_timeout pattern as take_screenshots.R.
{
  cr <- chromote::Chromote$new(
    chromote::Chrome$new(
      args = c(chromote::get_chrome_args(), "--no-sandbox", "--disable-dev-shm-usage")
    )
  )
  cr$default_timeout <- 60
  chromote::set_default_chromote_object(cr)
}

message("Starting AppDriver ...")
app <- shinytest2::AppDriver$new(
  app_dir,
  width        = 1440,
  height       = 900,
  load_timeout = 90000,
  timeout      = 180000,
  wait         = FALSE
)
on.exit(try(app$stop(), silent = TRUE), add = TRUE)

# Wait until the session is ready: preview_btn must exist and be enabled -
# see take_screenshots.R's comment on why a blind sleep is not enough.
waited <- 0
repeat {
  Sys.sleep(2); waited <- waited + 2
  ready <- js_get(app, "(function() {
    var b = document.getElementById('preview_btn');
    return (b && !b.disabled) ? 'yes' : 'no';
  })()")
  if (identical(ready, "yes")) break
  if (waited >= 60) stop("Preview button never became enabled.")
}
message("Session ready after ", waited, " s.")

# --- Step 1: select "Upload postal codes" as the target layer --------------
message("--- Selecting overlay_source = postal_upload ---")
app$set_inputs(overlay_source = "postal_upload")
app$wait_for_js("document.getElementById('postal_file') !== null", timeout = 15000)

# --- Step 2: upload the real CSV --------------------------------------------
message("--- Uploading sample postal codes ---")
app$upload_file(postal_file = csv_path)
app$wait_for_js("document.getElementById('postal_column') !== null", timeout = 15000)

detected_col <- js_get(app, "document.getElementById('postal_column').value")
message("Auto-detected postal column: ", detected_col %||% "NULL")
if (!identical(detected_col, "postal_code")) {
  stop("Column auto-detection picked '", detected_col %||% "NULL", "', expected 'postal_code'.")
}

# --- Step 3: wait for the real resolve_postal_points() call to finish ------
message("--- Waiting for postal status UI (real resolve_postal_points() call) ---")
waited <- 0
repeat {
  Sys.sleep(1); waited <- waited + 1
  status_text <- js_get(app,
    "(document.getElementById('postal_status_ui') || {innerText: ''}).innerText.trim()")
  if (!is.null(status_text) && nzchar(status_text)) break
  if (waited >= 30) stop("postal_status_ui never populated.")
}
message("postal_status_ui: ", status_text)
if (!grepl("5 input rows, 5 placed, 0 unmatched", status_text, fixed = TRUE)) {
  stop("Unexpected resolution result: ", status_text)
}

# --- Step 4: leave base_layer at its default (phu_boundaries) and preview --
message("--- Clicking Preview on map ---")
app$click("preview_btn")

waited <- 0; interval <- 3; limit <- 180
repeat {
  Sys.sleep(interval); waited <- waited + interval
  join_state <- js_get(app, "document.getElementById('build_btn') ? 'enabled' : 'absent'")
  message(sprintf("  [%3ds] build_btn=%s", waited, join_state %||% "NULL"))
  if (identical(join_state, "enabled")) break
  if (waited >= limit) stop("Preview never completed (build_btn stayed absent).")
}
message("Preview complete: Join button enabled.")

point_count <- js_get(app,
  "document.querySelectorAll('#cw_map .leaflet-marker-icon, #cw_map .leaflet-interactive').length")
message("Map interactive/marker elements after preview: ", point_count %||% "NULL")
if (is.null(point_count) || point_count <= 0) {
  stop("Preview completed but the map has no interactive/marker elements.")
}

# --- Step 5: run the join end-to-end ----------------------------------------
message("--- Clicking Join, then confirming ---")
app$click("build_btn")
Sys.sleep(2)
app$click("confirm_join_btn")

waited <- 0
repeat {
  Sys.sleep(3); waited <- waited + 3
  dl_ready <- js_get(app, "document.getElementById('dl_cw_csv') ? 'yes' : 'no'")
  status_text <- js_get(app,
    "(document.getElementById('link_task_status') || {innerText: ''}).innerText.trim()")
  message(sprintf("  [%3ds] dl_cw_csv=%s status=%s", waited, dl_ready %||% "NULL",
                   if (is.null(status_text) || !nzchar(status_text)) "(empty)" else status_text))
  if (identical(dl_ready, "yes") || grepl("ready", status_text %||% "", ignore.case = TRUE)) break
  if (waited >= 120) stop("Join never completed.")
}
message("Join complete.")

# --- Step 6: switch to the Data tab and confirm real rows are present ------
message("--- Switching to Data tab ---")
app$run_js(
  "var tabs = document.querySelectorAll('.nav-link');
   for (var i = 0; i < tabs.length; i++) {
     if ((tabs[i].innerText || tabs[i].textContent || '').trim() === 'Data') {
       tabs[i].click(); break;
     }
   }"
)
Sys.sleep(2)

row_count <- js_get(app, "document.querySelectorAll('.dataTable tbody tr').length")
message("Data table row count: ", row_count %||% "NULL")
if (is.null(row_count) || row_count != 5) {
  stop("Expected exactly 5 rows in the Data tab, got ", row_count %||% "NULL")
}

message("")
message("=== PASS: postal upload -> resolve -> preview -> join -> Data tab, all real, all headless. ===")
message("Input codes: 5. Rows in output table: ", row_count)
