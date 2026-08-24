# Two dropdown-level asks from the same round of feedback: offer the
# open-source postal codes as a target layer rather than only an uploaded
# list, and let a user attach ON-Marg measures when the source layer is an
# administrative boundary.

test_that("the open-source postal points are offered as a target layer", {
  env <- load_shiny_app_env()
  choices <- unlist(env$target_choices_grouped(), use.names = FALSE)

  expect_true("postal_points" %in% choices)
  # Still a point layer, so it lands in the point-first target picker rather
  # than the source picker.
  expect_identical(env$geom_kind("postal_points"), "point")
  expect_false("postal_points" %in%
    unlist(env$source_choices_grouped(), use.names = FALSE))
})

test_that("the postal layer is windowable, and pre-ticked because of its size", {
  env <- load_shiny_app_env()

  expect_true(env$is_windowable_source("postal_points"))
  expect_true(env$is_windowable_source("census_da_2021"))
  expect_false(env$is_windowable_source("phu_boundaries"))
  expect_false(env$is_windowable_source("postal_upload"))

  # 299,782 points province-wide: the window has to arrive already on.
  expect_true(env$census_window_default("postal_points"))
})

test_that("ON-Marg geographies map to the source ids that can carry them", {
  env <- load_shiny_app_env()

  expect_identical(env$onmarg_geography_for("census_da_2021"), "da")
  expect_identical(env$onmarg_geography_for("phu_boundaries_pre2025"), "phu")

  # The post-2025 PHU vintage has 29 units; ON-Marg keys on the pre-2025 34,
  # so it must NOT be offered there.
  expect_true(is.na(env$onmarg_geography_for("phu_boundaries")))
  expect_true(is.na(env$onmarg_geography_for("municipal_upper")))
  expect_true(is.na(env$onmarg_geography_for(NULL)))
})

test_that("every ON-Marg source id in the mapping is a real, retrievable source", {
  env <- load_shiny_app_env()
  mapped <- ONgeoR::onmarg_geographies()
  mapped <- mapped$source_id[!is.na(mapped$source_id)]

  expect_true(all(mapped %in% ONgeoR::list_sources()$source_id))
  # ...and each one is reachable from the source picker, or the checkbox
  # would appear for a layer the user cannot select.
  expect_true(all(mapped %in%
    unlist(env$source_choices_grouped(), use.names = FALSE)))
})

test_that("the ON-Marg control appears only for a mapped source layer", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  server <- load_shiny_server()

  shiny::testServer(server, {
    session$setInputs(base_layer = "phu_boundaries_pre2025",
      overlay_source = "moh_service_locations")
    expect_identical(onmarg_geography(), "phu")
    expect_match(rendered_html(output$onmarg_ui), "Add ON-Marg measures",
      fixed = TRUE)

    # Unticked is the default, so an unrelated run never pays for the fetch.
    expect_null(requested_onmarg())
    session$setInputs(add_onmarg = TRUE)
    expect_identical(requested_onmarg(), "phu")

    # A source with no ON-Marg equivalent offers nothing...
    session$setInputs(base_layer = "phu_boundaries")
    expect_true(is.na(onmarg_geography()))
    expect_identical(rendered_html(output$onmarg_ui), "")
    # ...and a stale tick cannot smuggle the option back in.
    expect_null(requested_onmarg())
  })
})
