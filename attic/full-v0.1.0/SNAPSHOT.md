# Snapshot: ONgeoRapp 0.1.0, full pre-MVP feature set

**Taken:** 2026-08-06
**Source:** `ONgeoRapp` `main` @ `7b0c9a0`
**Method:** `git archive HEAD` — tracked files only

## What this is

A verbatim copy of the Shiny app as it stood before the MVP trim. It is
**parked, not retired**: the plan is to cut the app down to a smaller, more
robust core and then pull features back out of here as they earn their way in.

The bulk of what will be trimmed lives in one file,
`inst/shiny/app_impl.R` (~1,700 lines): the five-button download grid
(Shapes / Map / Table / Pairs / Script), the target-geometry merge, the
crosswalk best-match collapse, the raster link aggregation, the async
`ExtendedTask` pair with phase-file progress reporting, and the retrieval
failure classifier.

Note that `inst/shiny/app.R` is a **source-rewriting launcher**: it reads
`app_impl.R`, swaps the future-plan startup block, and injects
`ensure_async_plan()` before each `*_task$invoke(`, hard-failing unless it
finds exactly one of each. Any trim that adds or removes an `invoke(` call
site breaks the launcher. That invariant is easy to forget and worth
re-reading here before cutting.

## What it is not

- Not built, checked, loaded, or tested. `^attic$` is in the package
  `.Rbuildignore`, so `R CMD build` / `R CMD check` / `devtools::load_all()`
  / `testthat` all skip this directory entirely. Nothing in here runs, and the
  225 assertions in `tests/testthat/` are inert copies — the live suite is the
  one at the repo root.
- Not a substitute for git history. `git show 7b0c9a0` is authoritative if the
  two ever disagree. This folder exists so the removed code stays *browsable
  side by side* with the trimmed version while trimming is in progress.

## Companion snapshot

`ONgeoR` was snapshotted at the same time — see
`ONgeoR/attic/full-v0.4.0/SNAPSHOT.md` (`main` @ `d709150`). The app calls into
that library (including `ONgeoR::layer_id_col()`, exported specifically for
this app), so the two are trimmed in lockstep and the snapshots are only
meaningful as a pair.

## Known-unverified at snapshot time

Carried forward so the trim does not silently inherit them as "working":

- The postal upload → resolve → link path has never been driven through a real
  browser; the server tests mock it.
- The Connect manifest still pins an older ONgeoR and needs re-pinning before
  any deploy.

## Removing this folder

Once the MVP has stabilised and nothing more is coming back out, delete the
whole directory and drop `^attic$` from `.Rbuildignore`. The history keeps it.
