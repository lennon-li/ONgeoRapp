# ONgeoRapp

<!-- badges: start -->
[![R-CMD-check](https://github.com/lennon-li/ONgeoRapp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/lennon-li/ONgeoRapp/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The Shiny application companion to
[**ONgeoR**](https://github.com/lennon-li/ONgeoR).

Pick a **Source layer** and a **Target layer** from the Ontario Land
Information Ontario (LIO) registry. The geometry pair alone determines the
linking operation — polygon intersection, nearest point matching, containment,
or raster sampling — so there is no match rule for you to choose. The app draws
both layers on a map, shows the linked table, and exports `target.gpkg` — the
target layer's geometry with the joined attributes merged on, ready to open in
QGIS or ArcGIS — alongside `mapping.csv`, `pairs.csv`, `map.html`, and a
`reproduce.R` script.

Live app: <https://biostats-ongeor.share.connect.posit.cloud/>

## Installation

```r
# ONgeoR is installed automatically via Remotes
pak::pkg_install("github::lennon-li/ONgeoRapp")
```

## Usage

```r
ONgeoRapp::run_app()
```

## Relationship to ONgeoR

`ONgeoR` holds all of the retrieval, linking, and crosswalk logic and is the
package you would use from R or a script. This repository holds only the
application layer, and reaches `ONgeoR` through its exported functions.

They were split so that `ONgeoR` can go to CRAN as a data-and-linking package
without carrying the app, its `shiny`/`bslib`/`DT` dependencies, or the browser
smoke test in its tarball.

## Deployment

The app deploys to Posit Connect Cloud from `inst/shiny/manifest.json`, which
pins the exact `ONgeoR` commit the deployment resolves. The
`manifest-freshness` workflow fails the build when that pin drifts from
`ONgeoR`'s tip. To regenerate it:

```r
pak::pkg_install("github::lennon-li/ONgeoR")
rsconnect::writeManifest(appDir = "inst/shiny", appPrimaryDoc = "app.R")
```

A manifest generated from a *local* source install is rejected — `ONgeoR` must
be installed from GitHub so its `DESCRIPTION` carries `RemoteType`/`RemoteSha`.

## License

MIT © Lennon Li
