# ONgeoRapp 0.1.0

* Initial release. The Shiny application was extracted from the `ONgeoR`
  package (which retained everything else) so that `ONgeoR` can be submitted
  to CRAN as a data-and-linking package without carrying the application, its
  `shiny`/`bslib`/`DT` dependencies, or the browser smoke test in its tarball.

* `run_app()` moves here unchanged apart from the package it looks in for the
  application directory. The application source under `inst/shiny/` and the
  `testServer` and browser smoke suites move with it.

* The application depends on `ONgeoR` through its exported functions only, so
  the split required no change to any spatial code.
