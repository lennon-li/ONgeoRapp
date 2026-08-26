# ONgeoRapp (development version)

* The Data tab now shows the joined result rather than the crosswalk. It used
  to render `build_crosswalk()`'s fixed assignment/provenance table, which
  carries neither layer's attributes - so a join of a 5-attribute target to a
  10-attribute source appeared to have lost most of its fields. The table (and
  the Shapes download, which shares the same object) now carries the target's
  own columns, the join provenance, and the matched source feature's columns
  under a `src_` prefix.

* The map layers control names the layers: "Source layer - Ontario Dam
  Inventory" rather than "Source layer".

* The target slot's "Use my own file" works, and accepts point layers only
  (GeoJSON, GeoPackage, KML, or a zipped shapefile). The target drives the
  join direction, so an uploaded polygon target would reintroduce the
  polygon-to-polygon overlay this app avoids; it is rejected with the geometry
  type that was found. Own-file upload and the "Upload postal codes" dropdown
  both replace the target layer, so selecting the latter now clears and hides
  the former instead of leaving both set.

* Ontario's open postal-code points are selectable as a target layer, without
  uploading a list of codes. The layer is 299,782 points province-wide, so the
  "Limit to current map view" window is offered for it and arrives pre-ticked.

* An "Add ON-Marg measures" option appears when the source layer is an
  administrative boundary with a 2021 Ontario Marginalization Index
  equivalent, attaching the factor scores and published quintiles so they
  carry through into the joined result.

* Fixed: the map sometimes drew empty. Leaflet stores the payload and draws
  nothing when its element measures 0x0, and only the widget's own `resize()`
  replays it - which nothing in this app ever triggered, since htmlwidgets
  binds that to `shown.bs.tab` for static widgets only and skips its
  window-resize path when the measured size is unchanged. The app now flushes
  the widget on tab show and after each render.

* Fixed: re-running "Preview on map" after a join left the map without either
  layer. shiny's `reactiveValues` dedupes on `identical()`, and a repeat
  preview writes the same layers back, so nothing the map depends on
  invalidated and no payload was sent at all. Separately, a momentarily `NULL`
  picker - the echo of an `updateSelectInput()` - was treated as a real change
  and discarded the previewed layers.

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

# ONgeoRapp (standalone app source)

* The Shiny application is maintained here as standalone app source while the
  installable R package and its `run_app()` entry point remain in `ONgeoR`.

* The application source and its `testServer` and browser smoke suites are
  synchronized with the copy bundled by `ONgeoR`.

* The application uses `ONgeoR` through its exported functions only; ONgeoR
  bundles the synchronized app source and exposes `ONgeoR::run_app()`.
