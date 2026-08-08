shiny_fixture_registry <- function() {
  tibble::tibble(
    source_id = c("base_polygon", "overlay_point", "other_polygon"),
    name = c("Base polygon", "Overlay point", "Other polygon"),
    geography_type = c("boundary", "facility", "boundary"),
    feature_count = c(3L, 10L, 2L)
  )
}

shiny_fixture_metadata <- function(source_id) {
  registry <- shiny_fixture_registry()
  row <- registry[registry$source_id == source_id, , drop = FALSE]
  if (nrow(row) != 1L) {
    rlang::abort(sprintf("Unknown fixture source '%s'.", source_id))
  }

  list(
    name = row$name[[1]],
    service_layer = source_id,
    geography_type = row$geography_type[[1]],
    feature_count = row$feature_count[[1]],
    key_fields = list(id = "fixture_id", name = "fixture_name"),
    license = "test fixture",
    source_url = "https://example.test/ongeor-fixture"
  )
}

shiny_fixture_layers <- function() {
  overlap <- fixture_overlap_layers()
  list(
    base_polygon = fixture_polygons(),
    overlay_point = fixture_points(),
    other_polygon = overlap$from
  )
}

shiny_app_file <- function() {
  candidates <- c(
    testthat::test_path("..", "..", "inst", "shiny", "app.R"),
    file.path(getwd(), "inst", "shiny", "app.R"),
    system.file("shiny", "app.R", package = "ONgeoRapp")
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0L) {
    rlang::abort("Could not locate inst/shiny/app.R.")
  }
  normalizePath(existing[[1]], mustWork = TRUE)
}

load_shiny_app_env <- function() {
  previous_plan <- future::plan()
  on.exit(future::plan(previous_plan), add = TRUE)

  # app.R defers the multisession plan to ensure_async_plan() at invoke time.
  # Neutralize it so test tasks stay in-process (sequential plan set by
  # use_sequential_futures()) and mocked ONgeoR bindings remain visible.
  testthat::local_mocked_bindings(
    multisession = future::sequential,
    .package = "future"
  )

  env <- new.env(parent = globalenv())
  withCallingHandlers(
    sys.source(shiny_app_file(), envir = env),
    warning = function(cnd) {
      if (grepl("Unable to infer a `window_title`", conditionMessage(cnd))) {
        invokeRestart("muffleWarning")
      }
    }
  )
  if (exists("ensure_async_plan", envir = env, inherits = FALSE)) {
    env$ensure_async_plan <- function() invisible(NULL)
  }
  env
}

load_shiny_server <- function() {
  load_shiny_app_env()$server
}

use_sequential_futures <- function(env = parent.frame()) {
  previous_plan <- future::plan(future::sequential)
  withr::defer(future::plan(previous_plan), envir = env)
  invisible(previous_plan)
}

wait_for_extended_task <- function(task, session, timeout = 5) {
  deadline <- Sys.time() + timeout
  repeat {
    later::run_now(timeoutSecs = 0.05)
    session$flushReact()
    status <- shiny::isolate(task$status())
    if (status %in% c("success", "error")) {
      later::run_now(timeoutSecs = 0.01)
      # The task's status observer appends the mapping phase, unpacks the
      # layers, declares completion and pushes the terminal dialog state all in
      # one cycle. Flush once for that observer and again for the outputs it
      # invalidates (the map, the status line).
      session$flushReact()
      session$flushReact()
      return(status)
    }
    if (Sys.time() >= deadline) {
      rlang::abort(sprintf(
        "ExtendedTask did not finish within %s seconds (status: %s).",
        timeout,
        status
      ))
    }
  }
}

rendered_html <- function(value) {
  paste(as.character(value), collapse = "")
}
