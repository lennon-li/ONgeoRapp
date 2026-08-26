# ONgeoRapp

The standalone Shiny application companion to
[**ONgeoR**](https://github.com/lennon-li/ONgeoR). This repository is app
source, not an R package.

Pick a **Source layer** and a **Target layer** from the Ontario Land
Information Ontario (LIO) registry. The geometry pair alone determines the
linking operation — polygon intersection, nearest point matching, containment,
or raster sampling — so there is no match rule for you to choose. The app draws
both layers on a map, shows the linked table, and exports `target.gpkg` — the
target layer's geometry with the joined attributes merged on, ready to open in
QGIS or ArcGIS — alongside `mapping.csv`, `pairs.csv`, `map.html`, and a
`reproduce.R` script.

Live app: <https://biostats-ongeor.share.connect.posit.cloud/>

## Run locally

```r
pak::pkg_install("github::lennon-li/ONgeoR")
shiny::runApp(".")
```

## Relationship to ONgeoR

`ONgeoR` holds all of the retrieval, linking, and crosswalk logic and is the
package you would use from R or a script. This repository holds only the
application layer, and reaches an installed `ONgeoR` through its exported
functions. ONgeoR does not import this repository at runtime: it bundles the
synchronized app source and launches it with `ONgeoR::run_app()`.

The two repositories are kept synchronized during development; ONgeoR is the
installable R package and ONgeoRapp is the standalone app source.

## Deployment

The app deploys to Posit Connect Cloud from `manifest.json`, which
pins the exact `ONgeoR` commit the deployment resolves. The
`manifest-freshness` workflow fails the build when that pin drifts from
`ONgeoR`'s tip. To regenerate it:

```r
pak::pkg_install("github::lennon-li/ONgeoR")
rsconnect::writeManifest(appDir = ".", appPrimaryDoc = "app.R")
```

A manifest generated from a *local* source install is rejected — `ONgeoR` must
be installed from GitHub so its `DESCRIPTION` carries `RemoteType`/`RemoteSha`.

Publish this repository's root directory, with `app.R` as the primary document.
Do not publish the `ONgeoR` package directory itself; it is not a Shiny app:

```r
rsconnect::deployApp(
  appDir = "/path/to/ONgeoRapp",
  appPrimaryDoc = "app.R",
  appName = "biostats-ongeor"
)
```

## License

MIT © Lennon Li
