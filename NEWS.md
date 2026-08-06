# ONgeoRapp (development version)

* The Join sidebar now offers a "Shapes" download (`target.gpkg`): the target
  layer's own geometry as a GeoPackage with the joined attributes merged onto
  it. For intersection and nearest runs the target-level table is joined
  directly; for `build_crosswalk()` runs each target's best pair is kept
  (highest coverage, then lowest match distance, ties by first occurrence);
  for raster runs the sampled values are joined onto the vector target. The
  button stays disabled when the target layer is a raster, which has no
  attribute table to merge onto.

* The sidebar downloads are laid out as a two-column grid with shortened
  labels (Shapes, Map, Table, Pairs, Script); each button's real filename is
  shown as a tooltip.

* "Upload postal codes" is now available as a target layer option in the Shiny
  app. Upload a CSV or Excel file containing postal codes; the app auto-detects
  the postal code column, resolves codes to point coordinates via
  `ONgeoR::resolve_postal_points()`, and uses the resulting sf POINT layer as
  the target for spatial linking. Coverage reporting shows input rows, placed
  codes, and unmatched count, and flags Geonames-sourced points as place-level
  rather than address-level.

* The "PCode2DA" postal-code tab was removed from the Shiny app, along with the
  top-level tab strip that carried it. The app is again a single-purpose spatial
  linking interface filling the page directly. The underlying postal-code
  functions are unaffected and remain available in `ONgeoR`
  (`resolve_postal()`, `normalize_postal_code()`).

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
