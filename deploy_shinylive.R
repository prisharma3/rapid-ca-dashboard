# deploy_shinylive.R

# WHAT THIS FILE DOES
#   Exports shiny_app/ as a static, serverless site using shinylive (R
#   running in the browser via WebAssembly/webR -- no Shiny Server, no
#   hosting cost). Output goes to docs/, which GitHub Pages can serve
#   directly. This never touches respondent-level data: it only bundles
#   whatever is already sitting in data_pipeline_output/ at the time you
#   run this (the small, pre-aggregated, already-suppressed tables that
#   pipeline_run.R produces).
#
# BEFORE RUNNING THIS
#   Run pipeline_run.R first so data_pipeline_output/trend_tables.rds and
#   sample_description.rds actually exist and are current. Deploying
#   without them produces a dashboard that fails to start.
#
# WHY shiny_app/R HOLDS 4 FILE-LEVEL SYMLINKS, NOT ONE FOLDER SYMLINK
#   shinylive::export() only bundles files that live inside the app
#   directory -- see the comment near the top of shiny_app/app.R for the
#   fuller explanation. shiny_app/R used to be a single symlink to the
#   whole top-level R/ folder, which seemed like the simpler "one source
#   of truth" setup -- but shinylive's dependency scanner reads every
#   .R file it finds in the app directory, whether or not app.R actually
#   sources it. R/01_prep.R, R/02_weighting.R, R/03_domains.R, and
#   R/diagnostics.R were along for the ride even though app.R never
#   touches them, and R/02_weighting.R's `library(anesrake)` alone
#   dragged in anesrake's own Depends (Hmisc, weights) and THEIR entire
#   dependency trees (mice, lme4, nlme, jomo, mitml, pan, ordinal,
#   survival, cluster, rpart, and more) -- real weight added to every
#   page load for code the dashboard never runs. shiny_app/R is now a
#   real directory holding 4 individual symlinks, one per file app.R
#   actually sources (00_config.R, 04_summarize.R, 05_excel_export.R,
#   06_reporting.R) -- still a single source of truth (each is a plain
#   symlink to the real file in R/), just without the 4 files that were
#   never part of the dashboard in the first place. If this directory
#   or its symlinks are ever missing (e.g. after a fresh git clone on a
#   platform that doesn't preserve symlinks), recreate them once with:
#     mkdir -p shiny_app/R
#     for f in 00_config 04_summarize 05_excel_export 06_reporting; do
#       ln -s ../../R/$f.R shiny_app/R/$f.R
#     done
#     ln -s ../data_pipeline_output shiny_app/data_pipeline_output
#
# WHY THIS SCRIPT CLEARS docs/ FIRST
#   shinylive::export() reuses whatever's already in destdir rather
#   than always scanning dependencies completely fresh -- confirmed by
#   trimming shiny_app/R above and re-running without clearing docs/
#   first: the exact same 93 MB package bundle came back unchanged,
#   anesrake/Hmisc/weights and all. Deleting the previous export's
#   output before calling export() forces a real fresh dependency scan
#   every time, so a future change that REMOVES a dependency actually
#   shrinks the published bundle instead of silently carrying the old
#   bloat forward forever.
#
# USAGE
#   Rscript deploy_shinylive.R
#   Then either open docs/index.html locally, or commit+push docs/ /
#   let the GitHub Actions workflow (.github/workflows/deploy-shinylive.yml)
#   build and publish it via GitHub Pages.

if (!requireNamespace("shinylive", quietly = TRUE)) {
  install.packages("shinylive", repos = "https://cloud.r-project.org")
}

stopifnot(
  "shiny_app/R is missing -- see the header of this script" = dir.exists("shiny_app/R"),
  "shiny_app/data_pipeline_output is missing -- see the header of this script" =
    file.exists("shiny_app/data_pipeline_output")
)

if (!file.exists(file.path("data_pipeline_output", "trend_tables.rds"))) {
  warning(
    "data_pipeline_output/trend_tables.rds not found -- run pipeline_run.R first. ",
    "Deploying now would produce a dashboard that fails to start."
  )
}

# Keep docs/README.md and docs/FLOW.md (project docs that happen to live
# in the same folder, not part of the exported app) -- delete everything
# else so export() below can't reuse stale dependency-scan output.
docs_keep <- c("README.md", "FLOW.md")
if (dir.exists("docs")) {
  for (entry in setdiff(list.files("docs", all.files = FALSE), docs_keep)) {
    unlink(file.path("docs", entry), recursive = TRUE, force = TRUE)
  }
}

shinylive::export(appdir = "shiny_app", destdir = "docs", subdir = "")

cat("\nExported static shinylive app to docs/. Open docs/index.html to preview locally,\n",
    "or push docs/ (or let CI build it) and enable GitHub Pages for this repo.\n", sep = "")
