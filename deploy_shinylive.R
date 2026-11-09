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
# WHY shiny_app/R AND shiny_app/data_pipeline_output ARE SYMLINKS
#   shinylive::export() only bundles files that live inside the app
#   directory -- see the comment near the top of shiny_app/app.R for the
#   full explanation. If those symlinks are ever missing (e.g. after a
#   fresh git clone, since git does preserve symlinks but some tools/
#   zips don't), recreate them once with:
#     ln -s ../R shiny_app/R
#     ln -s ../data_pipeline_output shiny_app/data_pipeline_output
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
  "shiny_app/R is missing -- see the header of this script" = file.exists("shiny_app/R"),
  "shiny_app/data_pipeline_output is missing -- see the header of this script" =
    file.exists("shiny_app/data_pipeline_output")
)

if (!file.exists(file.path("data_pipeline_output", "trend_tables.rds"))) {
  warning(
    "data_pipeline_output/trend_tables.rds not found -- run pipeline_run.R first. ",
    "Deploying now would produce a dashboard that fails to start."
  )
}

shinylive::export(appdir = "shiny_app", destdir = "docs", subdir = "")

cat("\nExported static shinylive app to docs/. Open docs/index.html to preview locally,\n",
    "or push docs/ (or let CI build it) and enable GitHub Pages for this repo.\n", sep = "")
