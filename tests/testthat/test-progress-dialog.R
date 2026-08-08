# The progress dialog must never make a claim the user can see is false. Two
# defects found during Lennon's 2026-08-07 walkthrough are gated here:
#
#   1. Clicking Join a second time with unchanged inputs opened the dialog and
#      then returned early, leaving a permanently blank box with only Cancel.
#   2. The preview declared "Mapping data - done" and revealed OK when the map
#      payload was SENT, not when the browser had drawn it, so the map appeared
#      seconds after the dialog said it was finished.

progress_pushes <- function(messages) {
  Filter(function(x) identical(x$type, "ongeor-progress"), messages)
}

last_push <- function(messages) {
  pushes <- progress_pushes(messages)
  if (length(pushes) == 0L) NULL else pushes[[length(pushes)]]$message
}

setup_join_app <- function() {
  layers <- shiny_fixture_layers()
  testthat::local_mocked_bindings(
    list_sources = shiny_fixture_registry,
    get_source = shiny_fixture_metadata,
    retrieve_source = function(source_id, refresh = FALSE, ...) layers[[source_id]],
    .package = "ONgeoR",
    .env = parent.frame()
  )
  load_shiny_server()
}

test_that("a repeated Join with unchanged inputs never leaves a blank dialog", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  server <- setup_join_app()
  use_sequential_futures()

  shiny::testServer(server, {
    messages <- list()
    session$sendCustomMessage <- function(type, message) {
      messages[[length(messages) + 1L]] <<- list(type = type, message = message)
    }

    session$setInputs(base_layer = "base_polygon", overlay_source = "overlay_point")
    session$setInputs(preview_btn = 1)
    expect_identical(wait_for_extended_task(preview_task, session), "success")
    session$setInputs(cw_map_rendered = 1)
    session$flushReact()

    session$setInputs(build_btn = 1)
    session$setInputs(confirm_join_btn = 1)
    expect_identical(wait_for_extended_task(build_task, session), "success")
    expect_identical(link_state(), "completed")

    # Second Join, same inputs: the app short-circuits because the results are
    # already current. The dialog must still reach a terminal state the user can
    # dismiss - never an empty box whose only control is Cancel.
    messages <- list()
    session$setInputs(build_btn = 2)
    session$setInputs(confirm_join_btn = 2)
    session$flushReact()

    final <- last_push(messages)
    expect_false(is.null(final))
    expect_true(isTRUE(final$done))
    expect_true(isTRUE(final$ok))
    expect_gt(length(final$phases), 0L)
  })
})

test_that("no progress push ever blanks a dialog that already has phases", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  server <- setup_join_app()
  use_sequential_futures()

  shiny::testServer(server, {
    messages <- list()
    session$sendCustomMessage <- function(type, message) {
      messages[[length(messages) + 1L]] <<- list(type = type, message = message)
    }

    session$setInputs(base_layer = "base_polygon", overlay_source = "overlay_point")
    session$setInputs(preview_btn = 1)
    expect_identical(wait_for_extended_task(preview_task, session), "success")
    session$setInputs(cw_map_rendered = 1)
    session$flushReact()

    pushes <- progress_pushes(messages)
    expect_gt(length(pushes), 0L)
    seen_phases <- FALSE
    for (p in pushes) {
      n <- length(p$message$phases)
      if (n > 0L) seen_phases <- TRUE
      # Once the dialog has shown real phases, a later push must not empty it.
      if (seen_phases) expect_gt(n, 0L)
    }
  })
})

test_that("preview completion waits for the browser render acknowledgement", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  server <- setup_join_app()
  use_sequential_futures()

  shiny::testServer(server, {
    messages <- list()
    session$sendCustomMessage <- function(type, message) {
      messages[[length(messages) + 1L]] <<- list(type = type, message = message)
    }

    session$setInputs(base_layer = "base_polygon", overlay_source = "overlay_point")
    session$setInputs(preview_btn = 1)
    expect_identical(wait_for_extended_task(preview_task, session), "success")

    # The task has resolved and the map data has been sent, but the browser has
    # not reported that it finished drawing. OK must NOT be revealed yet.
    expect_false(isTRUE(last_push(messages)$done))

    # The map widget acknowledges its render; only now is the run complete.
    session$setInputs(cw_map_rendered = 1)
    session$flushReact()

    final <- last_push(messages)
    expect_true(isTRUE(final$done))
    expect_true(isTRUE(final$ok))
    expect_true("Mapping data..." %in% unlist(final$phases))
  })
})

test_that("a missing render acknowledgement cannot hang the dialog forever", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  server <- setup_join_app()
  use_sequential_futures()

  shiny::testServer(server, {
    messages <- list()
    session$sendCustomMessage <- function(type, message) {
      messages[[length(messages) + 1L]] <<- list(type = type, message = message)
    }

    session$setInputs(base_layer = "base_polygon", overlay_source = "overlay_point")
    session$setInputs(preview_btn = 1)
    expect_identical(wait_for_extended_task(preview_task, session), "success")
    expect_false(isTRUE(last_push(messages)$done))

    # The browser never acknowledges. The cap must still finish the dialog.
    expect_true(exists("render_ack_timeout_secs"))
    expect_true(is.numeric(render_ack_timeout_secs))
    force_render_ack_timeout()
    session$flushReact()

    expect_true(isTRUE(last_push(messages)$done))
  })
})

test_that("the dialog warns about big layers and never claims a fake percentage", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  server <- setup_join_app()
  use_sequential_futures()

  shiny::testServer(server, {
    messages <- list()
    session$sendCustomMessage <- function(type, message) {
      messages[[length(messages) + 1L]] <<- list(type = type, message = message)
    }

    # The fixture registry's largest layer is small, so the general
    # cache-warming hint applies rather than the multi-minute warning.
    session$setInputs(base_layer = "base_polygon", overlay_source = "overlay_point")
    session$setInputs(preview_btn = 1)
    session$flushReact()

    hints <- vapply(
      progress_pushes(messages),
      function(x) x$message$hint %||% "",
      character(1)
    )
    expect_true(any(nzchar(hints)))
    expect_match(hints[nzchar(hints)][[1]], "elapsed clock", fixed = TRUE)
  })
})

test_that("a layer over a thousand features earns the several-minutes warning", {
  skip_if_not_installed("shiny")

  big <- tibble::tibble(
    source_id = c("big_polygon", "overlay_point"),
    name = c("Big polygon", "Overlay point"),
    geography_type = c("boundary", "facility"),
    feature_count = c(20465L, 10L)
  )
  testthat::local_mocked_bindings(list_sources = function() big, .package = "ONgeoR")
  app_env <- load_shiny_app_env()

  msg <- app_env$run_hint_text(c("big_polygon", "overlay_point"))
  expect_match(msg, "20,465", fixed = TRUE)
  expect_match(msg, "several minutes", fixed = TRUE)
  expect_match(msg, "coffee", fixed = TRUE)
})
