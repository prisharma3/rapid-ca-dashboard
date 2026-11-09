# app.R
#
# The RAPID-CA CORE Trends Shiny dashboard. Reads only the small,
# pre-aggregated files pipeline_run.R writes to data_pipeline_output/. It
# never touches respondent-level data and never sources R/01_prep.R,
# R/02_weighting.R, or R/03_domains.R.
#
# Most of the actual logic lives in R/06_reporting.R (querying the
# pre-aggregated data into one figure's panel data, shared with the
# Quarto report) and shiny_app/dashboard_plots.R (turning that into an
# interactive plotly figure). This file is the reactive wiring: UI
# inputs into those functions into outputs.
#
# Four tabs:
#   Trends              the flexible comparison view, with a headline
#                        KPI scorecard above it.
#   Investigate         compare a specific month or custom range against
#                        a reference period's average, as a deviation-bar
#                        chart. Uses monthly_tables.rds (every month,
#                        suppressed or not), not trend_tables.rds.
#   Sample Description  respondent counts by month, filterable by
#                        grouping, downloadable.
#   Methodology Note     one card per domain: the survey question,
#                        answer choices, how the percent is calculated.
#
# The Trends tab's comparison model: every domain has a Grouping axis.
# Parent Distress and Child Behavior also have a Measure axis and a
# Threshold axis; Child Care and Material Hardship have a Measure axis
# only; Food Insecurity has a Threshold axis only. Three controls:
#   Compare by   which axis becomes multiple lines on one plot.
#   Facet by     which axis becomes separate plots, one per value.
#                "None" means no faceting.
#   Series       Monthly or 3-Month Rolling, never both at once.
# Whichever axis is neither Compare by nor Facet by gets pinned to one
# fixed value. R/06_reporting.R's resolve_domain_figure() resolves all of
# this; this file never needs to know the underlying figure titles.
#
# "Overall" (CA-wide) is always the first panel shown, whatever else is
# being compared or faceted.
#
# shiny_app/R and shiny_app/data_pipeline_output are symlinks to the real
# top-level folders. This app deploys two ways: locally via
# shiny::runApp("shiny_app"), and as a static shinylive site. Shinylive's
# export only bundles files inside the app directory, so the symlinks
# keep one source of truth while letting shinylive::export() follow them
# and bundle the real content.
#
# To run locally: shiny::runApp("shiny_app") from the repo root, after
# pipeline_run.R has populated data_pipeline_output/.

library(shiny)
library(dplyr)
library(purrr)
library(plotly)
library(DT)

source("R/00_config.R")
source("R/04_summarize.R")
source("R/05_excel_export.R")
source("R/06_reporting.R")
source("dashboard_helpers.R")
source("dashboard_plots.R")
source("month_range_input.R")

`%||%` <- function(a, b) if (is.null(a)) b else a

trend_tables_path  <- file.path(PIPELINE_OUTPUT_DIR, "trend_tables.rds")
monthly_tables_path <- file.path(PIPELINE_OUTPUT_DIR, "monthly_tables.rds")
sample_desc_path  <- file.path(PIPELINE_OUTPUT_DIR, "sample_description.rds")
fpl_distribution_path <- file.path(PIPELINE_OUTPUT_DIR, "fpl_distribution.rds")

if (!file.exists(trend_tables_path)) {
  stop("data_pipeline_output/trend_tables.rds not found -- run pipeline_run.R first ",
       "(from the repo root, with access to the real masterfile) before starting this app.")
}

trend_tables <- readRDS(trend_tables_path)
sample_description <- if (file.exists(sample_desc_path)) readRDS(sample_desc_path) else NULL
# Powers the Investigate tab. Missing just disables that tab gracefully.
monthly_tables <- if (file.exists(monthly_tables_path)) readRDS(monthly_tables_path) else NULL

# Powers the Sample Description tab's FPL distribution chart. Missing
# just loses that one chart, not the whole app.
fpl_distribution <- if (file.exists(fpl_distribution_path)) readRDS(fpl_distribution_path) else NULL

DOMAIN_CHOICES   <- setNames(names(DOMAIN_FIGURES), vapply(DOMAIN_FIGURES, function(d) d$heading, character(1)))
GROUPING_CHOICES <- setdiff(ALL_GROUPING_LABELS, "Overall")  # Overall is always shown separately, first
FULL_DATE_RANGE  <- range(trend_tables$date, na.rm = TRUE)
FACET_GRID_COLS  <- 2

# The KPI scorecard's "compared to" note names the actual latest and
# prior month, computed at startup so it always reflects the data.
LATEST_MONTH_LABEL <- format(FULL_DATE_RANGE[2], "%B %Y")
PRIOR_MONTH_LABEL  <- format(seq(FULL_DATE_RANGE[2], by = "-1 month", length.out = 2)[2], "%B %Y")

# Sample Description tab's category choices, in the table's own display
# order rather than alphabetical. sample_description is now a list of
# 3 units (overall/by_month/by_survey, see build_sample_description_table())
# -- $overall always has every category present, so it's the one to read
# this from regardless of which unit ends up selected at display time.
SAMPLE_CATEGORY_CHOICES <- if (!is.null(sample_description)) {
  present <- unique(sample_description$overall$grouping)
  ordered <- DEFAULT_SAMPLE_DESCRIPTION_CATEGORY_ORDER[DEFAULT_SAMPLE_DESCRIPTION_CATEGORY_ORDER %in% present]
  c(ordered, sort(setdiff(present, ordered)))
} else {
  character(0)
}

# "By Survey" unit choices for the Sample Description tab's Table
# A/B/C pickers: one entry per SurveyType x SurveyNumber wave, in
# chronological order (wave_start), value-encoded as "<SurveyType>_<SurveyNumber>"
# so the server can parse it straight back into the 2 columns
# summarize_sample_description_unit() filters on.
SAMPLE_SURVEY_CHOICES <- if (!is.null(sample_description)) {
  waves <- sample_description$by_survey %>%
    dplyr::distinct(SurveyType, SurveyNumber, survey_label, wave_start) %>%
    dplyr::arrange(wave_start)
  stats::setNames(paste(waves$SurveyType, waves$SurveyNumber, sep = "_"), waves$survey_label)
} else {
  character(0)
}

#' Parses a "<SurveyType>_<SurveyNumber>" value from SAMPLE_SURVEY_CHOICES
#' back into its 2 numeric parts.
parse_survey_choice <- function(key) {
  parts <- strsplit(key, "_", fixed = TRUE)[[1]]
  list(survey_type = as.numeric(parts[1]), survey_number = as.numeric(parts[2]))
}

#' A small placeholder plotly figure for a suppressed selection, so it
#' shows a clear message instead of a blank panel.
#'
#' @param source_id When this panel's source_id is polled elsewhere
#'   (window_stats_ui()/update_plot_for_window()) even while empty, it
#'   must still register "plotly_relayout" here -- otherwise every poll
#'   against it warns "event ... is not registered" on every reactive
#'   flush, since event_data()'s check runs in a deferred onFlushed()
#'   callback that suppressWarnings() around the poll itself can't reach.
empty_plot_message <- function(msg, source_id = NULL) {
  p <- plotly::plotly_empty(type = "scatter", mode = "markers", source = source_id) %>%
    plotly::layout(
      annotations = list(list(text = msg, showarrow = FALSE, font = list(size = 14)))
    )
  if (!is.null(source_id)) p <- p %>% plotly::event_register("plotly_relayout")
  p
}


# -----------------------------------------------------------------------------
# Methodology Note content, one card per domain.
# -----------------------------------------------------------------------------
METHODOLOGY_CONTENT <- list(
  list(title = "Material Hardship", bullets = c(
    "Survey question: “In the past month, how hard has it been for your family to pay for the very basics like food, housing, medical care, and utilities?” Answer choices: Not very hard, Somewhat hard, Hard, Very hard.",
    "Follow-up question: “Which of these needs have been hard for your family to pay for in the past month?” Respondents can pick any of Food, Housing, Utilities, Healthcare, Child care, or None of the above.",
    "Any Material Hardship is flagged when the household reports any difficulty above Not very hard, or picks at least one need on the follow-up question.",
    "Each source, Food, Housing, Utility, Healthcare, and Child Care, is its own flag, based on whether that specific need was picked.",
    "The percent is the share of respondents whose household had trouble affording at least one basic need that month. Each source's percent covers just that one need."
  )),
  list(title = "Parent Emotional Distress", bullets = c(
    "Depression: two items on losing interest in things and feeling down, each on a 4 point scale from Not at all to Nearly every day.",
    "Anxiety: two items on feeling nervous and being unable to stop worrying, same 4 point scale.",
    "Stress and Loneliness: one item each, on a 5 point scale from Never to Always.",
    sprintf('"%s" is flagged at Several days or more for Depression and Anxiety, or Sometimes or more for Stress and Loneliness.', PARENT_DISTRESS_THRESHOLD_LABELS[["Lower"]]),
    sprintf('"%s" is flagged at More than half the days for Depression and Anxiety, or Usually or more for Stress and Loneliness.', PARENT_DISTRESS_THRESHOLD_LABELS[["Higher"]]),
    "Any measure is flagged when at least one of the four crosses the chosen threshold.",
    "The percent is the share of respondents at or above that threshold, for one measure or combined across all four."
  )),
  list(title = "Child Behavior", bullets = c(
    "Survey question, asked once per child: “Please select the answer which best fits your child's behavior in the last month,” covering Fussy or defiant and Too fearful or anxious.",
    "A household's Fussy score is the highest rating across all children rated. The Fear score works the same way.",
    sprintf('"%s" is flagged at Somewhat or Sometimes true or higher.', CHILD_BEHAVIOR_THRESHOLD_LABELS[["Lower"]]),
    sprintf('"%s" is flagged at Often true or Very true.', CHILD_BEHAVIOR_THRESHOLD_LABELS[["Higher"]]),
    "Any measure is flagged when either Fussy or Fear crosses the chosen threshold.",
    "The percent is the share of households with at least one child at that level, for one measure or combined."
  )),
  list(title = "Child Care", bullets = c(
    "Survey question: “This month, have you used any non-parental care for your child(ren) under the age of 6 years?” Answer choices: Yes, No, Unsure.",
    "Respondents who answer Yes pick every type of care they used. Center-based options include preschool, day care, pre-K, Head Start, faith-based care, public pre-K, and Transitional Kindergarten. Non-center-based options include care from a relative, friend, or neighbor, paid or unpaid, and paid home-based providers.",
    "Parent-Only Care is flagged when the answer is No.",
    "Center-Based Care and Non-Center-Based Care are each flagged when the respondent picks any option in that group. A household can use both in the same month, so the two percents are not mutually exclusive.",
    "The percent is the share of households using each type that month, among those using any non-parental care."
  )),
  list(title = "Healthcare", bullets = c(
    "Survey question: “In the past month, have you missed a well-baby/well-child check-up for any of your children?” Answer choices: Yes, No.",
    "The percent is the share of respondents who missed a check-up that month."
  )),
  list(title = "Food Insecurity", bullets = c(
    "Six survey items ask about running out of food, food not lasting, and skipping meals in the past month. Answer choices vary by item, mostly Never/Sometimes/Often true or Yes/No.",
    "Each item is scored 0 or 1 and added into a raw score from 0 to 6.",
    sprintf('"%s" is a raw score of 2 or higher.', FOOD_INSECURITY_THRESHOLD_LABELS[["Lower"]]),
    sprintf('"%s" is a raw score of 2 to 4.', FOOD_INSECURITY_THRESHOLD_LABELS[["Moderate"]]),
    sprintf('"%s" is a raw score of 5 or 6.', FOOD_INSECURITY_THRESHOLD_LABELS[["Higher"]]),
    "The percent is the share of respondents in that score range.",
    "Asked only of respondents on the Ongoing Assessment (OA) survey track."
  ))
)

methodology_cards <- function() {
  lapply(METHODOLOGY_CONTENT, function(card) {
    tags$div(
      class = "methodology-card",
      h5(card$title),
      tags$ul(lapply(card$bullets, tags$li))
    )
  })
}


# -----------------------------------------------------------------------------
# UI. A function of `request`, not a bare value, so enableBookmarking =
# "url" below can restore state from a shared bookmarked link.
# -----------------------------------------------------------------------------
ui <- function(request) navbarPage(
  # A persistent "data as of" cue, visible on every tab.
  title = tags$span(
    "RAPID-CA CORE Trends",
    tags$span(
      style = sprintf(
        "margin-left:10px; font-size:12px; font-weight:500; vertical-align:middle; background:%s; color:%s; padding:3px 9px; border-radius:10px;",
        RAPID_COLORS$soft_black, RAPID_COLORS$white
      ),
      sprintf("Data through %s", LATEST_MONTH_LABEL)
    )
  ),
  theme = bslib::bs_theme(
    bg = RAPID_COLORS$white, fg = RAPID_COLORS$soft_black,
    primary = RAPID_COLORS$yellow, secondary = RAPID_COLORS$gray,
    success = "#0ca30c", danger = "#d03b3b",
    # A plain font-stack string, not bslib::font_google(): font_google()
    # reaches out to fonts.googleapis.com over the network as part of
    # building the theme object. That's fine in a normal server-hosted
    # Shiny app, but this app also deploys as a static shinylive site,
    # where the whole app -- including building this theme -- runs
    # inside the browser's WebAssembly sandbox. That network call
    # failing/timing out there is fatal: it blocks the entire app from
    # ever finishing startup (10-second timeout, then a dead page with
    # no way to retry). A plain string needs no network call at all,
    # so it can never break startup either way -- Manrope still loads
    # if the browser already has it some other way, and falls back to
    # a safe system sans-serif otherwise.
    base_font = sprintf(
      "'%s', -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif",
      RAPID_FONT_FAMILY
    )
  ),

  # App-wide styling, passed via navbarPage()'s own `header` argument so
  # it's the documented way to add page-level head content here.
  header = tags$head(
    tags$link(rel = "icon", type = "image/svg+xml", href = paste0(
      "data:image/svg+xml,",
      utils::URLencode(sprintf(
        '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 32 32"><circle cx="16" cy="16" r="16" fill="%s"/><text x="16" y="22" font-family="Arial, sans-serif" font-size="17" font-weight="700" fill="%s" text-anchor="middle">R</text></svg>',
        RAPID_COLORS$yellow, RAPID_COLORS$soft_black
      ), reserved = TRUE)
    )),
    tags$style(HTML("
    /* Soft elevation on every card-like panel instead of a flat border. */
    #trends_kpi_area, .trends-plot-card, .methodology-card, .well {
      border: 1px solid rgba(0,0,0,0.06);
      border-radius: 10px;
      box-shadow: 0 1px 2px rgba(0,0,0,0.04), 0 2px 6px rgba(0,0,0,0.06);
    }
    .well { background-color: #fff; }
    .shiny-download-link {
      border: 1.5px solid #D4AF00 !important;
      color: #2E2D29 !important;
      font-weight: 600;
      transition: background-color 0.15s ease;
    }
    .shiny-download-link:hover {
      background-color: #FEE11A !important;
      border-color: #D4AF00 !important;
    }
    /* DT's row-selection highlight auto-picks white text against the
       brand yellow background, which is low contrast. Force dark text. */
    table.dataTable tr.selected,
    table.dataTable tr.selected td {
      color: #1a1a1a !important;
    }
    .rapid-stat-tile {
      border-radius: 8px;
      box-shadow: 0 1px 2px rgba(0,0,0,0.04), 0 2px 6px rgba(0,0,0,0.06);
    }
    /* The brand yellow is too light to read as a highlight on its own. */
    .navbar .nav-link.active {
      border-bottom: 3px solid #D4AF00;
      font-weight: 600;
    }
    .navbar-brand { font-weight: 700; letter-spacing: -0.01em; }
    .container-fluid { padding-top: 14px; }
    h3, h4 { font-weight: 600; letter-spacing: -0.01em; }
  "))),

  tabPanel(
    "Trends",
    # Pure client-side sidebar collapse, no server round-trip.
    tags$head(
      tags$style(HTML("
        #trends_sidebar_col.collapsed { display: none; }
        #trends_main_col.expanded { width: 100%; }
        #trends_kpi_area { padding: 10px 12px; margin-bottom: 10px; background: #fff; }
        #trends_plot_scroll { max-height: 72vh; overflow-y: auto; padding-right: 6px; }
        .trends-plot-card { padding: 12px; margin-bottom: 12px; background: #fff; }
        .trends-toolbar { display: flex; gap: 8px; margin-bottom: 10px; }
        .trends-toolbar .btn {
          font-size: 12px; font-weight: 500; padding: 6px 14px; border-radius: 6px;
          color: #444; background-color: #fff; border: 1px solid #ccc; transition: background-color 0.15s ease;
        }
        .trends-toolbar .btn:hover { background-color: #f5f5f5; }
        #trends_kpi_content.collapsed { display: none; }
        .kpi-toggle-header {
          display: flex; justify-content: space-between; align-items: center;
          cursor: pointer; user-select: none; margin-bottom: 4px;
        }
        .kpi-toggle-label {
          font-size: 12px; font-weight: 600; color: #444; background-color: #fff;
          border: 1px solid #ccc; border-radius: 6px; padding: 4px 12px;
          transition: background-color 0.15s ease;
        }
        .kpi-toggle-header:hover .kpi-toggle-label { background-color: #f5f5f5; }
        #trends_kpi_area .nav-tabs .nav-link,
        #sample_desc_tabs_area .nav-tabs .nav-link { color: #555; border: none; }
        #trends_kpi_area .nav-tabs .nav-link.active,
        #sample_desc_tabs_area .nav-tabs .nav-link.active {
          background-color: #D4AF00; color: #1a1a1a; font-weight: 600; border: none;
        }
      "))
    ),
    tags$script(HTML("
      function toggleTrendsSidebar() {
        var sidebar = document.getElementById('trends_sidebar_col');
        var main = document.getElementById('trends_main_col');
        var label = document.getElementById('filters_toggle_label');
        var nowCollapsed = sidebar.classList.toggle('collapsed');
        main.classList.toggle('expanded');
        label.textContent = nowCollapsed ? 'Show Filters ▸' : 'Hide Filters ▾';
      }
      function toggleKpiArea() {
        var content = document.getElementById('trends_kpi_content');
        var label = document.getElementById('kpi_toggle_label');
        var nowCollapsed = content.classList.toggle('collapsed');
        label.textContent = nowCollapsed ? 'Show ▸' : 'Hide ▾';
      }
    ")),
    tags$div(
      # KPI section: its own Monthly / 3-Month Rolling tabs, independent
      # of the sidebar's Series control, with CA Statewide and LA County
      # side by side.
      tags$div(
        id = "trends_kpi_area",
        tags$div(
          class = "kpi-toggle-header", onclick = "toggleKpiArea()",
          tags$b("Key Metrics"), tags$span(id = "kpi_toggle_label", class = "kpi-toggle-label", "Hide ▾")
        ),
        tags$div(
          id = "trends_kpi_content",
          tabsetPanel(
            tabPanel("Monthly",
              p(style = "font-size:14px; color:#2E2D29; margin:8px 0 6px 0;",
                sprintf("%s vs. %s. Monthly estimates.", LATEST_MONTH_LABEL, PRIOR_MONTH_LABEL)),
              fluidRow(
                column(6, h5("CA Statewide", style = "margin-top:0;"), uiOutput("headline_scorecard_ca_monthly")),
                column(6, h5("LA County", style = "margin-top:0;"), uiOutput("headline_scorecard_la_monthly"))
              )
            ),
            tabPanel("3-Month Rolling",
              p(style = "font-size:14px; color:#2E2D29; margin:8px 0 6px 0;",
                sprintf("%s vs. %s. 3-Month Rolling estimates.", LATEST_MONTH_LABEL, PRIOR_MONTH_LABEL)),
              fluidRow(
                column(6, h5("CA Statewide", style = "margin-top:0;"), uiOutput("headline_scorecard_ca_rolling")),
                column(6, h5("LA County", style = "margin-top:0;"), uiOutput("headline_scorecard_la_rolling"))
              )
            )
          ),
          tags$div(
            style = "font-size:14px; color:#2E2D29; background:#F4F4F4; border-left:4px solid #FEE11A; padding:10px 14px; border-radius:4px; margin:14px 0 4px 0; line-height:1.6;",
            tags$b("Color key: "),
            sprintf(
              "red and green mark a change bigger than %d percentage points vs. the prior period (red = moved in the worse direction for that measure, green = moved in the better direction); yellow marks a smaller, “stable” change (%d points or less, including no change). ",
              KPI_STABLE_THRESHOLD_PTS, KPI_STABLE_THRESHOLD_PTS
            ),
            tags$b("\nGray (Child Care): "),
            "never colored, at any size of change, because there’s no worse or better direction to mark.",
            tags$b("All percentages shown are weighted estimates.")
          )
        )
      ),
      tags$div(
        class = "trends-toolbar",
        tags$button(
          onclick = "toggleTrendsSidebar()", class = "btn",
          tags$span(id = "filters_toggle_label", "Hide Filters ▾")
        ),
        bookmarkButton(label = "Share")
      ),
      p(style = "font-size:13px; color:#464646; margin:0 0 10px 0;",
        "Monthly and 3-Month Rolling are weighted estimates."),
      fluidRow(
        column(3, id = "trends_sidebar_col",
          selectInput("domain", "Domain", choices = DOMAIN_CHOICES),
          hr(),
          tags$div(
            style = "font-size:14px; font-weight:700; color:#0b0b0b; margin-bottom:2px;",
            "What to Show"
          ),
          tags$div(
            style = "font-size:12px; color:#666; margin-bottom:8px; line-height:1.4;",
            "Pick one dimension to split into lines and one to split into separate charts. ",
            "Whatever's left becomes a fixed filter below."
          ),
          selectInput("compare_by", "Split into lines by", choices = "Grouping"),
          selectInput("facet_by", "Split into separate charts by", choices = "None"),
          tags$div(
            id = "trends_pins_box",
            style = "background:#F4F4F4; border-radius:4px; padding:8px 10px 2px 10px; margin-bottom:10px;",
            uiOutput("measure_pin_ui"),
            uiOutput("threshold_pin_ui"),
            uiOutput("compare_grouping_ui"),
            uiOutput("facet_grouping_ui"),
            uiOutput("focus_control_ui")
          ),
          hr(),
          tags$div(
            style = "font-size:14px; font-weight:700; color:#0b0b0b; margin-bottom:8px;",
            "View"
          ),
          radioButtons("series_choice", "Series", choices = c("Monthly", "3-Month Rolling"), selected = "3-Month Rolling"),
          tags$b("Month range"),
          monthRangeInputUI("date_range", FULL_DATE_RANGE[1], FULL_DATE_RANGE[2],
                             default_start = FULL_DATE_RANGE[1], default_end = FULL_DATE_RANGE[2]),
          hr(),
          tags$div(
            style = "font-size:14px; font-weight:700; color:#0b0b0b; margin-bottom:8px;",
            "Download"
          ),
          tags$b("Download range"),
          radioButtons(
            "download_range_preset", NULL,
            choices  = c("Full Survey" = "Overall", "Latest Month" = "Latest Month", "Custom" = "Custom"),
            selected = "Overall"
          ),
          conditionalPanel(
            condition = "input.download_range_preset == 'Custom'",
            monthRangeInputUI("download_date_range", FULL_DATE_RANGE[1], FULL_DATE_RANGE[2],
                               default_start = FULL_DATE_RANGE[1], default_end = FULL_DATE_RANGE[2])
          ),
          downloadButton("download_table", "Download Plot Data (CSV)")
        ),
        column(9, id = "trends_main_col",
          tags$div(
            id = "trends_plot_scroll",
            conditionalPanel(
              condition = "input.facet_by == 'None'",
              fluidRow(
                column(6, h4("Overall")), column(6, h4(textOutput("right_panel_title", inline = TRUE)))
              ),
              fluidRow(
                column(6, div(class = "trends-plot-card", plotly::plotlyOutput("overall_plot", height = "460px"))),
                column(6, div(class = "trends-plot-card", plotly::plotlyOutput("flex_plot", height = "460px")))
              ),
              fluidRow(
                column(6, uiOutput("overall_stats")), column(6, uiOutput("flex_stats"))
              )
            ),
            conditionalPanel(
              condition = "input.facet_by != 'None'",
              uiOutput("facet_grid_ui")
            )
          )
        )
      )
    )
  ),

  tabPanel(
    "Investigate",
    p(
      "Compare every selected demographic level, across one or more groupings, against a reference ",
      "period's average -- pick which groupings and levels to show, what period to investigate (a single ",
      "month, or a custom range), and what to measure it against (the full survey timeline, or a custom ",
      "reference range). A month inside BOTH the reference and investigation periods is always excluded ",
      "from the reference average, so nothing is ever compared against a baseline partly made of itself."
    ),
    tags$head(tags$style(HTML("
      /* Caps the sidebar's own height with an independent, sticky
         scrollbar, so scrolling the filters doesn't scroll the plot
         out of view. */
      .app-sidebar-scroll {
        max-height: calc(100vh - 170px);
        overflow-y: auto;
        padding-right: 6px;
        position: -webkit-sticky;
        position: sticky;
        top: 80px;
      }
      #inv_plot { min-height: 480px; }
    "))),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        tags$div(
          class = "app-sidebar-scroll",
          selectInput("inv_domain", "Domain", choices = DOMAIN_CHOICES),
          uiOutput("inv_measure_pin_ui"),
          uiOutput("inv_threshold_pin_ui"),
          checkboxGroupInput("inv_groupings", "Groupings to include", choices = c("Overall", GROUPING_CHOICES)),
          uiOutput("inv_levels_ui"),
          hr(),
          tags$b("Reference period (the vertical line)"),
          radioButtons("inv_ref_type", NULL, choices = c("Full Timeline", "Custom Range"), selected = "Full Timeline"),
          conditionalPanel(
            condition = "input.inv_ref_type == 'Custom Range'",
            monthRangeInputUI("inv_ref_range", FULL_DATE_RANGE[1], FULL_DATE_RANGE[2],
                               default_start = FULL_DATE_RANGE[1], default_end = FULL_DATE_RANGE[2])
          ),
          radioButtons("inv_ref_avg_type", "Reference average",
                       choices = c("Weighted (bigger months count more)" = "weighted",
                                   "Unweighted (every month counts equally)" = "unweighted"),
                       selected = "weighted"),
          hr(),
          tags$b("Investigation period (the bars)"),
          radioButtons("inv_inv_type", NULL, choices = c("Single Month", "Custom Range"), selected = "Single Month"),
          conditionalPanel(
            condition = "input.inv_inv_type == 'Single Month'",
            monthPickerUI("inv_inv_month", FULL_DATE_RANGE[1], FULL_DATE_RANGE[2], default = FULL_DATE_RANGE[2])
          ),
          conditionalPanel(
            condition = "input.inv_inv_type == 'Custom Range'",
            monthRangeInputUI("inv_inv_range", FULL_DATE_RANGE[1], FULL_DATE_RANGE[2],
                               default_start = FULL_DATE_RANGE[2], default_end = FULL_DATE_RANGE[2])
          ),
          hr(),
          downloadButton("download_inv_table", "Download Table (CSV)")
        )
      ),
      mainPanel(
        width = 9,
        uiOutput("inv_validation_ui"),
        plotly::plotlyOutput("inv_plot", height = "calc(100vh - 340px)"),
        tags$div(style = "margin-top:10px;", uiOutput("inv_reference_ui"))
      )
    )
  ),

  tabPanel(
    "Sample Description",
    sidebarLayout(
      sidebarPanel(
        tags$div(
          class = "app-sidebar-scroll",
        checkboxGroupInput(
          "sample_categories", "Categories to include",
          choices  = SAMPLE_CATEGORY_CHOICES,
          selected = SAMPLE_CATEGORY_CHOICES
        ),
        hr(),
        # Every choice here maps to one of build_sample_description_table()'s
        # 3 EXACT precomputed units (Overall/Month/Survey) -- there's no
        # "Custom" range any more. A custom multi-month range can't get
        # an exact unique-participant count from month-level data alone
        # (it would require summing, which double-counts anyone who
        # responded in more than one of those months), so that option
        # was removed rather than silently showing a response count
        # under a number that now promises "unique participants."
        tags$b("Table A"),
        radioButtons(
          "sample_window_a", NULL,
          choices  = c("Overall (full survey)" = "Overall", "By Month" = "Month", "By Survey" = "Survey"),
          selected = "Overall"
        ),
        conditionalPanel(
          condition = "input.sample_window_a == 'Month'",
          monthPickerUI("sample_month_a", FULL_DATE_RANGE[1], FULL_DATE_RANGE[2], default = FULL_DATE_RANGE[2])
        ),
        conditionalPanel(
          condition = "input.sample_window_a == 'Survey'",
          selectInput("sample_survey_a", NULL, choices = SAMPLE_SURVEY_CHOICES,
                      selected = utils::tail(SAMPLE_SURVEY_CHOICES, 1))
        ),
        checkboxInput("sample_compare_two", "Compare a second time period", value = FALSE),
        conditionalPanel(
          condition = "input.sample_compare_two == true",
          tags$b("Table B"),
          radioButtons(
            "sample_window_b", NULL,
            choices  = c("Overall (full survey)" = "Overall", "By Month" = "Month", "By Survey" = "Survey"),
            selected = "Month"
          ),
          conditionalPanel(
            condition = "input.sample_window_b == 'Month'",
            monthPickerUI("sample_month_b", FULL_DATE_RANGE[1], FULL_DATE_RANGE[2], default = FULL_DATE_RANGE[2])
          ),
          conditionalPanel(
            condition = "input.sample_window_b == 'Survey'",
            selectInput("sample_survey_b", NULL, choices = SAMPLE_SURVEY_CHOICES,
                        selected = utils::tail(SAMPLE_SURVEY_CHOICES, 1))
          )
        ),
        conditionalPanel(
          condition = "input.sample_compare_two == true",
          checkboxInput("sample_compare_three", "Compare a third time period", value = FALSE),
          conditionalPanel(
            condition = "input.sample_compare_two == true && input.sample_compare_three == true",
            tags$b("Table C"),
            radioButtons(
              "sample_window_c", NULL,
              choices  = c("Overall (full survey)" = "Overall", "By Month" = "Month", "By Survey" = "Survey"),
              selected = "Survey"
            ),
            conditionalPanel(
              condition = "input.sample_window_c == 'Month'",
              monthPickerUI("sample_month_c", FULL_DATE_RANGE[1], FULL_DATE_RANGE[2], default = FULL_DATE_RANGE[2])
            ),
            conditionalPanel(
              condition = "input.sample_window_c == 'Survey'",
              selectInput("sample_survey_c", NULL, choices = SAMPLE_SURVEY_CHOICES,
                          selected = SAMPLE_SURVEY_CHOICES[[1]])
            )
          )
        ),
        uiOutput("sample_duplicate_warning"),
        hr(),
        downloadButton("download_sample", "Download Table (CSV)"),
        hr(),
        tags$b("FPL Distribution (Histogram)"),
        radioButtons(
          "fpl_hist_window_type", NULL,
          choices = c("Overall", "By Year", "Custom Date Range", "Single Month"),
          selected = "Overall"
        ),
        conditionalPanel(
          condition = "input.fpl_hist_window_type == 'By Year'",
          selectInput("fpl_hist_year", NULL,
                      choices = seq(as.integer(format(FULL_DATE_RANGE[1], "%Y")),
                                    as.integer(format(FULL_DATE_RANGE[2], "%Y"))))
        ),
        conditionalPanel(
          condition = "input.fpl_hist_window_type == 'Custom Date Range'",
          monthRangeInputUI("fpl_hist_custom_range", FULL_DATE_RANGE[1], FULL_DATE_RANGE[2],
                             default_start = FULL_DATE_RANGE[1], default_end = FULL_DATE_RANGE[2])
        ),
        conditionalPanel(
          condition = "input.fpl_hist_window_type == 'Single Month'",
          monthPickerUI("fpl_hist_month", FULL_DATE_RANGE[1], FULL_DATE_RANGE[2], default = FULL_DATE_RANGE[2])
        ),
        # checkboxGroupInput, not selectInput: a selectize dropdown let a
        # stray backspace clear the selection entirely. Checking more
        # than one bin width shows that many plots at once.
        checkboxGroupInput(
          "fpl_hist_bin_width", "Bin width(s) to show",
          choices = stats::setNames(FPL_HISTOGRAM_BIN_WIDTH_CHOICES,
                                     paste0(FPL_HISTOGRAM_BIN_WIDTH_CHOICES, " percentage points")),
          selected = 50
        ),
        downloadButton("download_fpl_hist", "Download Table (CSV)")
        )  # /app-sidebar-scroll
      ),
      mainPanel(
        tags$div(
          id = "sample_desc_tabs_area",
          tabsetPanel(
            tabPanel(
              "Table",
              DT::dataTableOutput("sample_table"),
              tags$p(tags$small(
                "N (%) is out of each category's own non-missing responses in that time window -- ",
                "missing/not-reported responses are excluded from both the count and the percentage."
              ))
            ),
            tabPanel(
              "FPL Histogram",
              uiOutput("fpl_histogram_plots_ui")
            )
          )
        )
      )
    )
  ),

  tabPanel(
    "Methodology Note",
    tags$head(tags$style(HTML("
      .methodology-grid { display: grid; grid-template-columns: repeat(3, 1fr); gap: 14px; margin-top: 10px; }
      @media (max-width: 900px) { .methodology-grid { grid-template-columns: 1fr; } }
      .methodology-card { padding: 14px 16px; background: #fff; }
      .methodology-card ul { padding-left: 18px; margin-bottom: 6px; }
      .methodology-card li { margin-bottom: 6px; font-size: 13px; }
      .methodology-card h5 { margin-top: 0; margin-bottom: 8px; }
    "))),
    p(
      "This tab explains what each domain measures, the survey question behind it, ",
      "and how the percent shown on the Trends tab is calculated. All percentages are weighted ",
      "using ACS 2020-2024 5 year target estimates (Raking: race, poverty, CA region). ",
      sprintf(
        paste0(
          "Monthly minimum sample size: %d respondents overall, %d within a subgroup. ",
          "3-Month Rolling minimum cumulative sample size (summed across the 3 months): %d respondents overall, %d within a subgroup."
        ),
        N_MIN_OVERALL, N_MIN_SUBGROUP, ROLLING_MIN_OVERALL, ROLLING_MIN_SUBGROUP
      )
    ),
    tags$div(class = "methodology-grid", methodology_cards())
  )
)


# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------
server <- function(input, output, session) {

  # ---- Trends tab: Month range / Download range pickers ----

  date_range_picker <- monthRangeInputServer("date_range")

  download_range_sync <- reactive(resolve_time_window(input$download_range_preset, FULL_DATE_RANGE))
  download_date_range_picker <- monthRangeInputServer("download_date_range", sync_to = download_range_sync)

  download_date_range <- reactive({
    req(input$download_range_preset)
    if (identical(input$download_range_preset, "Custom")) {
      download_date_range_picker()
    } else {
      resolve_time_window(input$download_range_preset, FULL_DATE_RANGE)
    }
  })

  # ---- Trends tab: axis controls ----

  # compare_by/facet_by start with just a placeholder choice in the
  # static UI; their real, domain-specific choices only exist once this
  # observer runs. shiny::restoreInput() is needed here because Shiny
  # restores a bookmarked selectInput by matching it against the
  # <option> tags present at render time, which at first is just the
  # placeholder. restoreInput() returns the bookmarked value while a
  # restore is in progress and `default` otherwise, so it's a no-op for
  # any normal interaction.
  # default = "Grouping" (not input$compare_by): every domain offers
  # "Grouping" as a choice, but "Measure"/"Threshold" are domain-specific
  # internal codes that happen to collide across domains that both have
  # a measure or threshold axis. Defaulting to the PREVIOUS domain's
  # compare_by here meant switching e.g. Child Care (compare_by =
  # "Measure") -> Material Hardship silently kept compare_by == "Measure"
  # there too (since "Measure" is also valid for Material Hardship),
  # drawing one line per hardship type with no obvious indication why --
  # reported as "why am I seeing all the types" even though "Any" was
  # still pinned in the now-hidden measure_pin control.
  observeEvent(input$domain, {
    updateSelectInput(session, "compare_by", choices = get_compare_by_choices(input$domain),
                       selected = shiny::restoreInput(id = "compare_by", default = "Grouping"))
  })

  # Facet by's choices are every other axis this domain has, never the
  # one chosen as Compare by, plus "None". Same bookmark-restore fix.
  # get_compare_by_choices() returns a NAMED vector (names = display
  # text, e.g. "Type" for Material Hardship) -- base::setdiff() drops
  # names on a named vector, so a plain `[` filter is used instead to
  # keep that label intact here too, not just on the Compare by control.
  # Tracks the domain this observe() last ran for, so facet_by only
  # resets to "None" on an actual domain switch -- not on every ordinary
  # within-domain compare_by change, which should keep sticking with
  # whatever facet_by the user already had (if still valid).
  prev_domain_for_facet <- reactiveVal(NULL)
  observe({
    req(input$domain, input$compare_by)
    by_axis_choices <- get_compare_by_choices(input$domain)
    remaining <- by_axis_choices[by_axis_choices != input$compare_by]
    domain_changed <- !identical(prev_domain_for_facet(), input$domain)
    prev_domain_for_facet(input$domain)
    # default = "None" only right after a domain switch -- same reasoning
    # as compare_by above: "None" is always valid, but carrying over the
    # previous domain's facet_by risked an equally confusing silent
    # carryover (e.g. a stale "Grouping" facet from a different domain
    # producing an unexpected faceted layout right after switching).
    facet_default <- if (domain_changed) "None" else input$facet_by
    updateSelectInput(session, "facet_by",
                       choices = c(None = "None", remaining),
                       selected = shiny::restoreInput(id = "facet_by", default = facet_default))
  })

  # The Measure pin: shown when this domain has a measure axis and it's
  # neither the compare axis nor the facet axis.
  output$measure_pin_ui <- renderUI({
    req(input$domain, input$compare_by, input$facet_by)
    choices <- get_measure_choices(input$domain)
    if (is.null(choices) || input$compare_by == "Measure" || input$facet_by == "Measure") return(NULL)
    selectInput("measure_pin", measure_axis_label(input$domain), choices = choices, selected = choices[[1]])
  })

  # The Threshold pin, same idea. "Both" is never offered: pinning it
  # under compare_by == "Grouping" would mix Lower and Higher rows into
  # the same line, and under compare_by == "Measure" there's no matching
  # figure to resolve to at all.
  output$threshold_pin_ui <- renderUI({
    req(input$domain, input$compare_by, input$facet_by)
    choices <- get_threshold_choices(input$domain)
    if (is.null(choices) || input$compare_by == "Threshold" || input$facet_by == "Threshold") return(NULL)
    selectInput("threshold_pin", "Threshold", choices = choices, selected = choices[[1]])
  })

  # NULL (not an error) when that pin's UI isn't currently rendered.
  measure_pin   <- reactive({ if (!is.null(input$measure_pin)   && nzchar(input$measure_pin))   input$measure_pin   else NULL })
  threshold_pin <- reactive({ if (!is.null(input$threshold_pin) && nzchar(input$threshold_pin)) input$threshold_pin else NULL })

  # Which grouping is shown as lines, when Grouping is the compare axis.
  output$compare_grouping_ui <- renderUI({
    req(input$compare_by)
    if (input$compare_by != "Grouping") return(NULL)
    selectInput("compare_grouping", "Grouping (shown as lines)", choices = GROUPING_CHOICES)
  })

  # Which grouping is split into separate plots, when Grouping is the
  # facet axis.
  output$facet_grouping_ui <- renderUI({
    req(input$facet_by)
    if (input$facet_by != "Grouping") return(NULL)
    selectInput("facet_grouping", "Facet grouping (one plot per level)", choices = GROUPING_CHOICES)
  })

  # The Grouping "Focus on" pin: shown only when Grouping is neither the
  # compare axis nor the facet axis, so it needs to be held fixed at one
  # slice while Measure/Threshold do the comparing.
  output$focus_control_ui <- renderUI({
    req(input$compare_by, input$facet_by)
    if (input$compare_by == "Grouping" || input$facet_by == "Grouping") return(NULL)
    tagList(
      # GROUPING_CHOICES, not ALL_GROUPING_LABELS: "Overall" is always
      # shown as its own dedicated panel already (the app's left/first
      # panel, unconditionally), so offering it here too would just
      # let you pin the focus panel to duplicate that same panel.
      selectInput("focus_category", "Focus on", choices = GROUPING_CHOICES, selected = GROUPING_CHOICES[[1]]),
      uiOutput("focus_level_ui")
    )
  })

  # Only a real subgroup needs a second "which level" dropdown.
  output$focus_level_ui <- renderUI({
    req(input$domain, input$focus_category)
    if (input$focus_category %in% c("Overall", "LA County")) return(NULL)
    levels_present <- get_levels_for_grouping(trend_tables, input$domain, input$focus_category)
    selectInput("focus_level", "Focus level", choices = levels_present)
  })

  series_key <- reactive({ if (identical(input$series_choice, "Monthly")) "monthly" else "rolling" })

  # Fixed executive-summary strips: every measure a domain has, for CA
  # statewide and LA County side by side.
  output$headline_scorecard_ca_monthly  <- renderUI(build_headline_scorecard(trend_tables, "Overall",    "monthly"))
  output$headline_scorecard_la_monthly  <- renderUI(build_headline_scorecard(trend_tables, "LA County",  "monthly"))
  output$headline_scorecard_ca_rolling  <- renderUI(build_headline_scorecard(trend_tables, "Overall",    "rolling"))
  output$headline_scorecard_la_rolling  <- renderUI(build_headline_scorecard(trend_tables, "LA County",  "rolling"))
  # These 4 are cheap, static reads off trend_tables, so compute all 4 up
  # front rather than waiting on Shiny's suspend-while-hidden default,
  # which would otherwise cause a visible flash the first time someone
  # switches Monthly/3-Month Rolling tabs.
  outputOptions(output, "headline_scorecard_ca_monthly", suspendWhenHidden = FALSE)
  outputOptions(output, "headline_scorecard_la_monthly", suspendWhenHidden = FALSE)
  outputOptions(output, "headline_scorecard_ca_rolling", suspendWhenHidden = FALSE)
  outputOptions(output, "headline_scorecard_la_rolling", suspendWhenHidden = FALSE)

  figure_heading <- reactive({
    req(input$domain)
    DOMAIN_FIGURES[[input$domain]]$heading
  })

  # ---- Core resolution logic ----

  # For a given facet_value ("Overall", a real facet value, or NULL when
  # Facet by == "None"), which Measure/Threshold pin applies to this
  # panel: overridden by the facet value itself when that's the faceted
  # axis, otherwise the global pin.
  facet_pins <- function(facet_value) {
    is_overall <- identical(facet_value, "Overall")
    list(
      measure   = if (!is.null(facet_value) && !is_overall && input$facet_by == "Measure")   facet_value else measure_pin(),
      threshold = if (!is.null(facet_value) && !is_overall && input$facet_by == "Threshold") facet_value else threshold_pin()
    )
  }

  # A short human label for what's being measured on this panel, used in
  # the plot title.
  panel_measure_description <- function(facet_value) {
    pins <- facet_pins(facet_value)
    # input$threshold_pin can hold a stale value left over from a
    # PREVIOUSLY viewed domain that does have a threshold axis -- Shiny
    # doesn't clear an input just because its selectInput got removed
    # from the UI (threshold_pin_ui returns NULL for a domain with no
    # threshold axis, like Material Hardship). Only resolve a label at
    # all when THIS domain actually has a threshold axis, so a stale
    # pin from a different domain can never leak into this one's title.
    has_threshold_axis <- !is.null(get_threshold_choices(input$domain))
    has_measure_axis   <- !is.null(get_measure_choices(input$domain))
    threshold_label <- if (has_threshold_axis) threshold_code_to_label(input$domain, pins$threshold) else NULL
    # Same staleness risk as threshold_pin, same fix: pins$measure
    # falls back to input$measure_pin whenever this isn't a Measure
    # facet (facet_pins()), and that input keeps whatever value it had
    # from the last domain that DID have a measure axis once
    # measure_pin_ui hides for a domain that doesn't -- e.g. switching
    # to Food Insecurity (no measure axis at all) right after viewing
    # Material Hardship left every panel titled "Any", a measure Food
    # Insecurity doesn't have. Only trust pins$measure when this domain
    # actually has a measure axis to begin with.
    effective_measure <- if (has_measure_axis) pins$measure else NULL

    # input$compare_by has the SAME staleness risk, one level up: right
    # after switching to a domain with no threshold axis at all (e.g.
    # Material Hardship), input$compare_by can still briefly read
    # "Threshold" from whatever domain was selected before --
    # updateSelectInput() refreshing compare_by's own choices doesn't
    # synchronously update input$compare_by's value for this reactive
    # tick. Left unguarded, that produced a real bug: with no real
    # threshold list to show, the branch below built "Any ()" --
    # a measure name with an empty, meaningless parenthetical. Falling
    # back to "Grouping" (always valid for every domain) instead of
    # trusting input$compare_by literally fixes this at the source,
    # rather than patching each place a bad value could leak into text.
    compare_by <- input$compare_by
    if (compare_by == "Threshold" && !has_threshold_axis) compare_by <- "Grouping"
    if (compare_by == "Measure" && !has_measure_axis) compare_by <- "Grouping"

    # Whenever NEITHER compare_by nor facet_by is "Grouping" (the exact
    # condition output$focus_control_ui checks), Grouping is the
    # leftover axis -- held fixed at whatever "Focus on" category/level
    # picked, not shown as lines or facets anywhere. Without naming
    # that pin somewhere, a panel titled e.g. just "All Types" gives no
    # indication it's actually "...for LA County" or "...for White"
    # specifically, not every grouping pooled together.
    is_grouping_focused <- compare_by != "Grouping" && input$facet_by != "Grouping"
    focus_suffix <- function() {
      if (is.null(input$focus_category)) return(NULL)
      if (input$focus_category %in% c("Overall", "LA County")) return(input$focus_category)
      if (is.null(input$focus_level) || !nzchar(input$focus_level)) return(NULL)
      sprintf("%s: %s", input$focus_category, input$focus_level)
    }
    append_focus <- function(base) {
      if (!is_grouping_focused) return(base)
      fs <- focus_suffix()
      if (is.null(fs)) base else paste0(base, " — ", fs)
    }

    if (compare_by == "Threshold") {
      threshold_list <- paste(names(get_threshold_choices(input$domain)), collapse = " vs ")
      measure_part <- effective_measure %||% ""
      base <- if (nzchar(measure_part)) sprintf("%s (%s)", measure_part, threshold_list) else threshold_list
      append_focus(base)
    } else if (compare_by == "Measure") {
      all_label <- paste0("All ", measure_axis_label(input$domain), "s")
      base <- if (!is.null(threshold_label)) sprintf("%s (%s)", all_label, threshold_label) else all_label
      append_focus(base)
    } else {
      parts <- c(effective_measure, threshold_label)
      base <- if (length(parts) == 0) {
        resolve_domain_figure(input$domain, compare_by, effective_measure, pins$threshold)$figure_title
      } else {
        paste(parts, collapse = ", ")
      }
      if (compare_by == "Grouping" && !identical(facet_value, "Overall")) {
        req(input$compare_grouping)
        paste0(base, " by ", input$compare_grouping)
      } else {
        base
      }
    }
  }

  # Doesn't repeat which slice this is (e.g. "White"); that's shown once
  # in a header above the plot, so the in-plot title stays short.
  panel_title <- function(facet_value) {
    paste0(figure_heading(), ": ", panel_measure_description(facet_value))
  }

  # Which grouping label this panel's suppression threshold should use.
  facet_caption_grouping <- function(facet_value) {
    if (identical(facet_value, "Overall")) return("Overall")
    if (input$facet_by == "Grouping" && !is.null(facet_value)) { req(input$facet_grouping); return(input$facet_grouping) }
    if (input$compare_by == "Grouping") { req(input$compare_grouping); return(input$compare_grouping) }
    req(input$focus_category)
    input$focus_category
  }

  # The actual query: get one panel's data, for facet_value ==
  # "Overall", a real facet value, or NULL (Facet by == "None").
  # @param date_range Override for which month range to fetch. NULL uses
  #   the sidebar's Month range control; the CSV download passes
  #   FULL_DATE_RANGE instead so the file always covers the whole survey.
  get_facet_panel_data <- function(facet_value = NULL, date_range = NULL) {
    pins <- facet_pins(facet_value)
    resolved <- resolve_domain_figure(input$domain, input$compare_by, pins$measure, pins$threshold)
    dr <- date_range %||% date_range_picker()

    if (identical(facet_value, "Overall")) {
      return(prepare_single_level_panel_data(
        trend_tables, input$domain, resolved$figure_title, "Overall",
        outcome_label = resolved$outcome_label, date_range = dr
      ))
    }
    if (input$compare_by == "Grouping") {
      req(input$compare_grouping)
      return(prepare_grouping_panel_data(
        trend_tables, input$domain, resolved$figure_title, input$compare_grouping,
        outcome_label = resolved$outcome_label, date_range = dr
      ))
    }
    if (!is.null(facet_value) && input$facet_by == "Grouping") {
      req(input$facet_grouping)
      return(prepare_pinned_level_panel_data(
        trend_tables, input$domain, resolved$figure_title, input$facet_grouping, facet_value,
        outcome_label = resolved$outcome_label, date_range = dr
      ))
    }
    req(input$focus_category)
    if (input$focus_category %in% c("Overall", "LA County")) {
      prepare_single_level_panel_data(
        trend_tables, input$domain, resolved$figure_title, input$focus_category,
        outcome_label = resolved$outcome_label, date_range = dr
      )
    } else {
      req(input$focus_level)
      prepare_pinned_level_panel_data(
        trend_tables, input$domain, resolved$figure_title, input$focus_category, input$focus_level,
        outcome_label = resolved$outcome_label, date_range = dr
      )
    }
  }

  # Every value the facet grid should render a plot for, besides the
  # always-first "Overall" reference. NULL when Facet by == "None".
  facet_values <- reactive({
    req(input$domain, input$facet_by)
    if (input$facet_by == "None") return(NULL)
    extra <- if (input$facet_by == "Grouping") {
      req(input$facet_grouping)
      get_levels_for_grouping(trend_tables, input$domain, input$facet_grouping)
    } else if (input$facet_by == "Measure") {
      get_measure_choices(input$domain)
    } else {
      get_threshold_choices(input$domain)
    }
    c("Overall", extra)
  })

  # Plotly's range-slider fires "plotly_relayout" CONTINUOUSLY while
  # the user is actively dragging it, not just once on release. Without
  # debouncing, every downstream consumer below (this stats panel's
  # render, update_plot_for_window()'s snap-back/title observer) would
  # re-run on every single intermediate event during the drag -- and
  # update_plot_for_window() itself pushes a plotlyProxy relayout each
  # time, which fires yet another event, cascading further. That's
  # exactly what stutters/flickers during a drag. shiny::debounce()
  # only lets a value through once DEBOUNCE_MS has passed with no new
  # event, i.e. once the user has actually paused or released.
  #
  # The debounced reactive must be created ONCE per source id and
  # reused -- a fresh debounce() has no memory of previous event
  # timing, so recreating it on every call would debounce nothing.
  # window_stats_ui() and update_plot_for_window() both read the SAME
  # debounced reactive for a given source id, so they settle in sync
  # rather than polling raw events independently at different paces.
  DEBOUNCE_MS <- 400
  relayout_debouncers <- new.env(parent = emptyenv())
  get_debounced_relayout <- function(source_id) {
    if (is.null(relayout_debouncers[[source_id]])) {
      # suppressWarnings(): plotly warns if a source is polled before
      # its plot has rendered client-side yet; event_data() just
      # returns NULL, which record_visible_range()/window_stats_ui()
      # below already treat as "no event yet".
      raw <- reactive(suppressWarnings(plotly::event_data("plotly_relayout", source = source_id)))
      relayout_debouncers[[source_id]] <- shiny::debounce(raw, DEBOUNCE_MS)
    }
    relayout_debouncers[[source_id]]
  }

  # One entry per source id: the last visible date range actually
  # KNOWN from a relayout event that carried real range information
  # (either a parsed custom range, or an explicit autorange reset).
  # Needed because update_plot_for_window() below pushes a SECOND,
  # separate plotlyProxy relayout purely to update the plot's title
  # text (list("title.text" = ...)) every time it fires -- that also
  # emits its own "plotly_relayout" event on the SAME source id, but
  # with no range keys at all. Without this tracking (a plain
  # pure-function lookup from the raw event, with no memory of what
  # came before, is what this file used to do here), that title-only
  # echo becomes the debounced reactive's newest value, nothing can
  # parse any range out of it, it falls back to the full data range,
  # and the stats panel below the chart briefly (and wrongly) reports
  # "exact, matches full range" even
  # though the plot is still showing the snapped custom window the
  # user actually dragged to. A relayout event that says nothing about
  # the range should leave whatever was last known untouched, not be
  # treated as "range unknown, assume full".
  #
  # ONLY update_plot_for_window()'s observer below ever WRITES here
  # (via record_visible_range()); window_stats_ui() only ever READS
  # it. That split matters, not just tidiness: if window_stats_ui()
  # (a renderUI) both read AND wrote the same reactiveValues entry, the
  # write would invalidate the entry it just subscribed to by reading
  # it, scheduling an extra (and in Shiny, not reliably ordered against
  # the observer's own write) re-render of itself. Observer-writes /
  # render-reads avoids that risk entirely, and also means
  # window_stats_ui() only re-renders once there's an ACTUAL new value
  # to show, never for a tick that carried no range information at all.
  last_visible_range <- reactiveValues()

  # One entry per source id: the last title text update_plot_for_window()
  # actually pushed via plotlyProxy, so it can skip re-pushing an
  # identical one -- see that function's own comment for why.
  last_title_set <- reactiveValues()

  #' Called only from update_plot_for_window()'s observer. See
  #' last_visible_range's own comment above for why a relayout event
  #' with no parseable range info leaves the stored value untouched.
  record_visible_range <- function(source_id, relayout_event, default_range) {
    if (!is.null(relayout_event) && isTRUE(relayout_event[["xaxis.autorange"]])) {
      last_visible_range[[source_id]] <- NULL
      return(default_range)
    }
    parsed <- parse_relayout_range(relayout_event)
    # Same "no overlap with this panel's own data at all" guard the
    # old get_visible_date_range() had, for a stale event that belongs
    # to a previously rendered, different plot.
    if (!is.null(parsed) && !(parsed[2] < default_range[1] || parsed[1] > default_range[2])) {
      clamped <- c(max(parsed[1], default_range[1]), min(parsed[2], default_range[2]))
      snapped <- snap_to_full_months(clamped)
      # Snapping can push slightly past default_range at either end,
      # so clamp back down to what the data can actually support.
      last_visible_range[[source_id]] <- c(max(snapped[1], default_range[1]), min(snapped[2], default_range[2]))
    }
    if (is.null(last_visible_range[[source_id]])) default_range else last_visible_range[[source_id]]
  }

  window_stats_ui <- function(data, series, source_id) {
    if (nrow(data) == 0) return(NULL)
    full_range <- range(data$date, na.rm = TRUE)
    # A pure read -- see last_visible_range's comment above for why
    # this never writes to it.
    visible_range <- last_visible_range[[source_id]] %||% full_range
    stats <- compute_window_stats(data, series = series, date_range = visible_range)
    build_window_stats_ui(stats, visible_range)
  }

  # ---- Visual month-snap-back + live window caption ----
  # After a zoom or range-slider drag, this snaps the plot's own
  # displayed range back to whole calendar months, and bakes the visible
  # window (and, for a single-line panel, its key stats) into the plot's
  # own title, since plotly's camera/download button only rasterizes the
  # plot, not the separate stats table below it. Both proxy calls fire
  # another relayout event; by then the range already matches the
  # snapped range, so the snap step is skipped on that second pass.
  # Reads the SAME debounced reactive window_stats_ui() uses above
  # (get_debounced_relayout()), not a fresh raw event_data() poll --
  # otherwise this fires on every intermediate event during an active
  # drag instead of once it settles, which is what caused the
  # stutter/flicker a continuous drag used to produce.
  # `source_id` (e.g. "overall_plot_src") is only ever used for
  # event_data() polling -- it's the crosstalk/event "source" tag, not a
  # real DOM element id. `output_id` (e.g. "overall_plot") is the actual
  # plotlyOutput() id and is what plotlyProxy() needs: plotlyProxy looks
  # up a real element by that id, so passing it the source tag instead
  # throws "Couldn't find plotly graph with id: <source_id>" in the
  # browser console the moment either proxy call below fires.
  update_plot_for_window <- function(source_id, output_id, get_full_range, get_panel_data, get_title_caption) {
    debounced_relayout <- get_debounced_relayout(source_id)
    observeEvent(debounced_relayout(), {
      relayout <- debounced_relayout()
      raw <- parse_relayout_range(relayout)

      full_range <- get_full_range()
      if (is.null(full_range) || anyNA(full_range)) return(invisible(NULL))
      # record_visible_range(), not a raw get_visible_date_range() poll:
      # the title-only proxy call below echoes back as a relayout event
      # on this same source id with no range keys at all, and this is
      # the one place authorized to update the shared tracked value --
      # see last_visible_range's own comment (above window_stats_ui())
      # for why a range-less event must leave it untouched instead of
      # resetting to full_range.
      visible_range <- record_visible_range(source_id, relayout, full_range)

      if (!is.null(raw) && !identical(raw, visible_range)) {
        plotly::plotlyProxy(output_id, session) %>%
          plotly::plotlyProxyInvoke("relayout", list(
            "xaxis.range[0]" = as.character(visible_range[1]),
            "xaxis.range[1]" = as.character(visible_range[2])
          ))
      }

      data <- get_panel_data()
      if (is.null(data) || nrow(data) == 0) return(invisible(NULL))
      stats <- compute_window_stats(data, series = series_key(), date_range = visible_range)

      window_label <- sprintf("%s–%s", format(visible_range[1], "%b %Y"), format(visible_range[2], "%b %Y"))
      # Participants is only exact for the one window compute_window_stats()
      # flagged as such (the data's full range, matched against a real
      # distinct-CALID count); any other window sums each month's own
      # distinct-respondent count, which can double-count anyone who
      # answered in more than one month of it. build_window_stats_ui()
      # already makes this distinction below the chart -- this title
      # annotation needs the same guard, not just the same numbers.
      stats_label <- if (nrow(stats) == 1 && !is.na(stats$n_weighted_avg)) {
        participants_part <- if (isTRUE(stats$is_exact_participants)) {
          sprintf(", %s participants", format(stats$total_participants, big.mark = ","))
        } else {
          ", participants not exact for this range"
        }
        sprintf(", N-wtd avg %.1f%%, %s responses%s",
                stats$n_weighted_avg * 100,
                format(stats$total_responses, big.mark = ","),
                participants_part)
      } else {
        ""
      }

      tc <- get_title_caption()
      new_title <- paste0(
        plot_title_html(tc$title, tc$caption),
        "<br><span style=\"font-size:12px; color:#888;\">Window: ", window_label, stats_label, "</span>"
      )
      # Only push this if the title actually changed. This proxy call
      # itself echoes back as a "plotly_relayout" event on this same
      # source id (same as the range-snap one above) -- pushing it
      # unconditionally meant that echo would re-trigger this very
      # observer every time, forever, even once nothing is left to
      # change, just to re-set an identical title.
      if (!identical(last_title_set[[source_id]], new_title)) {
        last_title_set[[source_id]] <- new_title
        plotly::plotlyProxy(output_id, session) %>%
          plotly::plotlyProxyInvoke("relayout", list("title.text" = new_title))
      }
    }, ignoreInit = TRUE)
  }

  update_plot_for_window(
    "overall_plot_src", "overall_plot",
    function() {
      data <- get_facet_panel_data("Overall")
      if (nrow(data) == 0) return(NULL)
      range(data$date, na.rm = TRUE)
    },
    function() get_facet_panel_data("Overall"),
    function() list(title = panel_title("Overall"), caption = build_plot_caption(get_facet_panel_data("Overall"), "Overall", series = series_key()))
  )
  update_plot_for_window(
    "flex_plot_src", "flex_plot",
    function() {
      if (!identical(input$facet_by, "None")) return(NULL)
      data <- get_facet_panel_data(NULL)
      if (nrow(data) == 0) return(NULL)
      range(data$date, na.rm = TRUE)
    },
    function() get_facet_panel_data(NULL),
    function() list(
      title = panel_title(NULL),
      caption = build_plot_caption(get_facet_panel_data(NULL), facet_caption_grouping(NULL), series = series_key())
    )
  )

  # Tracks which facet indices already have a snap observer registered,
  # so re-running that block never registers a second observer for the
  # same source id.
  registered_facet_snap_sources <- character(0)

  # ---- Facet by == "None": the classic fixed 2-panel layout ----

  right_panel_heading <- reactive({
    req(input$compare_by)
    if (input$compare_by == "Grouping") { req(input$compare_grouping); input$compare_grouping }
    else if (input$compare_by == "Measure") paste0(measure_axis_label(input$domain), "s")
    else "Threshold"
  })
  output$right_panel_title <- renderText({ right_panel_heading() })

  output$overall_stats <- renderUI(window_stats_ui(get_facet_panel_data("Overall"), series_key(), "overall_plot_src"))
  output$flex_stats    <- renderUI({ req(input$facet_by == "None"); window_stats_ui(get_facet_panel_data(NULL), series_key(), "flex_plot_src") })

  output$overall_plot <- plotly::renderPlotly({
    data <- get_facet_panel_data("Overall")
    if (nrow(data) == 0) return(empty_plot_message("No months met the minimum sample size for this selection.", source_id = "overall_plot_src"))
    build_interactive_trend_plot(
      data, series = series_key(), show_ci = TRUE, title = panel_title("Overall"),
      caption = build_plot_caption(data, "Overall", series = series_key()), source_id = "overall_plot_src"
    )
  })
  output$flex_plot <- plotly::renderPlotly({
    req(input$facet_by == "None")
    data <- get_facet_panel_data(NULL)
    if (nrow(data) == 0) return(empty_plot_message("No months met the minimum sample size for this selection.", source_id = "flex_plot_src"))
    build_interactive_trend_plot(
      data, series = series_key(), show_ci = TRUE, title = panel_title(NULL),
      caption = build_plot_caption(data, facet_caption_grouping(NULL), series = series_key()), source_id = "flex_plot_src"
    )
  })

  # ---- Facet by != "None": the dynamic facet grid ----

  output$facet_grid_ui <- renderUI({
    values <- facet_values()
    if (is.null(values) || length(values) == 0) return(NULL)
    chunks <- split(seq_along(values), ceiling(seq_along(values) / FACET_GRID_COLS))
    rows <- lapply(chunks, function(idxs) {
      fluidRow(lapply(idxs, function(i) {
        column(
          width = 12 %/% FACET_GRID_COLS,
          div(
            class = "trends-plot-card",
            h5(
              if (input$facet_by == "Threshold") threshold_code_to_label(input$domain, values[i]) else values[i],
              style = "text-align:center; margin-top:0;"
            ),
            plotly::plotlyOutput(paste0("facet_plot_", i), height = "460px"),
            uiOutput(paste0("facet_stats_", i))
          )
        )
      }))
    })
    do.call(tagList, rows)
  })

  observe({
    values <- facet_values()
    if (is.null(values)) return(NULL)
    lapply(seq_along(values), function(i) {
      local({
        idx <- i
        fv  <- values[idx]
        output[[paste0("facet_plot_", idx)]] <- plotly::renderPlotly({
          data <- get_facet_panel_data(fv)
          if (nrow(data) == 0) return(empty_plot_message("No months met the minimum sample size for this selection.", source_id = paste0("facet_src_", idx)))
          build_interactive_trend_plot(
            data, series = series_key(), show_ci = TRUE, title = panel_title(fv),
            caption = build_plot_caption(data, facet_caption_grouping(fv), series = series_key()),
            source_id = paste0("facet_src_", idx)
          )
        })
        output[[paste0("facet_stats_", idx)]] <- renderUI({
          window_stats_ui(get_facet_panel_data(fv), series_key(), paste0("facet_src_", idx))
        })

        src_id <- paste0("facet_src_", idx)
        if (!(src_id %in% registered_facet_snap_sources)) {
          registered_facet_snap_sources <<- c(registered_facet_snap_sources, src_id)
          # Re-reads facet_values()/get_facet_panel_data() fresh on every
          # call, not the `fv` captured above, since this observer is
          # only registered once per idx but which facet value occupies
          # that slot can change later.
          current_fv <- function() {
            current_values <- facet_values()
            if (is.null(current_values) || idx > length(current_values)) return(NULL)
            current_values[idx]
          }
          update_plot_for_window(
            src_id, paste0("facet_plot_", idx),
            function() {
              fv_now <- current_fv()
              if (is.null(fv_now)) return(NULL)
              data <- get_facet_panel_data(fv_now)
              if (nrow(data) == 0) return(NULL)
              range(data$date, na.rm = TRUE)
            },
            function() {
              fv_now <- current_fv()
              if (is.null(fv_now)) return(NULL)
              get_facet_panel_data(fv_now)
            },
            function() {
              fv_now <- current_fv()
              list(
                title = panel_title(fv_now),
                caption = build_plot_caption(get_facet_panel_data(fv_now), facet_caption_grouping(fv_now), series = series_key())
              )
            }
          )
        }
      })
    })
  })

  # ---- Plot data CSV download ----
  # Not rendered as a visible table, only downloadable. Its date range is
  # independent of the sidebar's Month range control, driven by its own
  # preset instead.

  # A facet_value for facet_by == "Threshold" is the internal code
  # ("Lower"/"Higher"/"Moderate"), not this domain's own display text --
  # same lookup used for the on-screen facet panel headers, so the
  # downloaded file names panels the same way the dashboard itself does.
  download_panel_label <- function(fv) {
    if (input$facet_by == "Threshold") threshold_code_to_label(input$domain, fv) else fv
  }

  # One named entry per panel the current Compare by/Facet by selection
  # produces, each already filtered to the download date range -- the
  # single source both combined_table_data() (the row-level table) and
  # combined_table_meta() (the per-panel totals) build from, so the two
  # can never drift into describing different data.
  download_panels <- reactive({
    req(input$domain, input$compare_by, input$facet_by)
    dr <- download_date_range()
    if (input$facet_by == "None") {
      stats::setNames(
        list(get_facet_panel_data("Overall", date_range = dr), get_facet_panel_data(NULL, date_range = dr)),
        c("Overall", right_panel_heading())
      )
    } else {
      stats::setNames(
        lapply(facet_values(), function(fv) get_facet_panel_data(fv, date_range = dr)),
        vapply(facet_values(), download_panel_label, character(1))
      )
    }
  })

  # Per-panel totals, using the SAME exact-for-the-full-range logic the
  # on-screen window-stats table uses: an exact distinct-participant
  # count when the download range is this panel's full range, an
  # explicitly-flagged approximation otherwise.
  combined_table_meta <- reactive({
    panels <- download_panels()
    dr <- download_date_range()
    purrr::imap_dfr(panels, function(data, panel_name) {
      stats <- compute_window_stats(data, series = series_key(), date_range = dr)
      if (is.null(stats) || nrow(stats) == 0) return(NULL)
      tibble::tibble(
        Panel = panel_name,
        Level = stats$level,
        `Total Responses` = stats$total_responses,
        `Total Participants` = stats$total_participants,
        `Participants Exact?` = ifelse(stats$is_exact_participants, "Yes",
                                        "No -- approximate, can double-count a respondent across months")
      )
    })
  })

  combined_table_data <- reactive({
    panels <- download_panels()
    base <- purrr::imap_dfr(panels, function(data, panel_name) dplyr::mutate(data, Panel = panel_name))
    base %>%
      dplyr::mutate(
        `Monthly %` = round(weighted_prop * 100, 1),
        `Rolling %` = round(rolling_weighted_prop * 100, 1)
      ) %>%
      dplyr::select(
        Domain = domain_heading, Figure = figure_title, Panel, Level = level,
        Month = month, N = n, Participants = n_participants, `Monthly %`, Reason = suppress_reason,
        `Rolling N` = rolling_n, `Rolling %`, `Rolling Window` = window_months,
        `Rolling Reason` = rolling_suppress_reason
      )
  })

  output$download_table <- downloadHandler(
    filename = function() paste0(input$domain, "_", input$compare_by, "_", input$facet_by, ".csv"),
    content = function(file) {
      utils::write.table(combined_table_meta(), file, sep = ",", row.names = FALSE, col.names = TRUE)
      cat("\n", file = file, append = TRUE)
      utils::write.table(combined_table_data(), file, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)
    }
  )

  # ---- Investigate tab (period-comparison deviation-bar view) ----

  inv_measure_pin   <- reactive({ if (!is.null(input$inv_measure_pin)   && nzchar(input$inv_measure_pin))   input$inv_measure_pin   else NULL })
  inv_threshold_pin <- reactive({ if (!is.null(input$inv_threshold_pin) && nzchar(input$inv_threshold_pin)) input$inv_threshold_pin else NULL })

  output$inv_measure_pin_ui <- renderUI({
    req(input$inv_domain)
    choices <- get_measure_choices(input$inv_domain)
    if (is.null(choices)) return(NULL)
    selectInput("inv_measure_pin", measure_axis_label(input$inv_domain), choices = choices, selected = choices[[1]])
  })

  # Same rule as the Trends tab: "Both" is never offered here either,
  # since this view needs one concrete series to build a single bar per
  # month from.
  output$inv_threshold_pin_ui <- renderUI({
    req(input$inv_domain)
    choices <- get_threshold_choices(input$inv_domain)
    if (is.null(choices)) return(NULL)
    selectInput("inv_threshold_pin", "Threshold", choices = choices, selected = choices[[1]])
  })

  # One checkboxGroupInput per selected grouping, pre-populated with
  # every level that grouping has, all checked by default. Input ids are
  # sanitized since a grouping label can contain characters Shiny input
  # ids can't reliably round-trip.
  inv_grouping_input_id <- function(g) paste0("inv_levels_", make.names(g))

  output$inv_levels_ui <- renderUI({
    req(input$inv_domain)
    if (is.null(input$inv_groupings) || length(input$inv_groupings) == 0) {
      return(tags$p(class = "text-muted", style = "font-size:12px;", "Pick at least one grouping above."))
    }
    tagList(lapply(input$inv_groupings, function(g) {
      levels_present <- get_levels_for_grouping(trend_tables, input$inv_domain, g)
      checkboxGroupInput(inv_grouping_input_id(g), g, choices = levels_present, selected = levels_present)
    }))
  })

  inv_resolved <- reactive({
    req(input$inv_domain)
    resolve_domain_figure(input$inv_domain, "Grouping", inv_measure_pin(), inv_threshold_pin())
  })

  # Every (grouping, level) pair currently checked, as one long data
  # frame, for compute_multi_level_comparison()'s `levels` argument.
  inv_selected_levels <- reactive({
    req(input$inv_groupings)
    rows <- lapply(input$inv_groupings, function(g) {
      lvls <- input[[inv_grouping_input_id(g)]]
      if (is.null(lvls) || length(lvls) == 0) return(NULL)
      tibble::tibble(grouping = g, level = lvls)
    })
    dplyr::bind_rows(rows)
  })

  inv_ref_range_picker   <- monthRangeInputServer("inv_ref_range")
  inv_inv_month_picker   <- monthPickerServer("inv_inv_month")
  inv_inv_range_picker   <- monthRangeInputServer("inv_inv_range")

  inv_reference_range <- reactive({
    if (input$inv_ref_type == "Full Timeline") return(NULL)
    inv_ref_range_picker()
  })

  inv_investigation_range <- reactive({
    if (input$inv_inv_type == "Single Month") {
      rep(inv_inv_month_picker(), 2)
    } else {
      inv_inv_range_picker()
    }
  })

  inv_comparison <- reactive({
    req(monthly_tables, input$inv_domain, input$inv_ref_avg_type)
    levels_df <- inv_selected_levels()
    if (is.null(levels_df) || nrow(levels_df) == 0) {
      return(list(valid = FALSE, error = "Pick at least one grouping and level to investigate.", reference = NULL, levels = NULL))
    }
    resolved <- inv_resolved()
    compute_multi_level_comparison(
      monthly_tables, input$inv_domain, resolved$figure_title, levels_df,
      investigation_range = inv_investigation_range(), reference_range = inv_reference_range(),
      outcome_label = resolved$outcome_label, avg_type = input$inv_ref_avg_type
    )
  })

  output$inv_validation_ui <- renderUI({
    comp <- inv_comparison()
    if (isTRUE(comp$valid)) return(NULL)
    tags$div(
      style = "color:#a33; font-size:13px; border:1px solid #e3b3b3; background:#fdf2f2; padding:8px 12px; border-radius:4px; margin-bottom:10px;",
      tags$b("Can't build this comparison: "), comp$error
    )
  })

  output$inv_plot <- plotly::renderPlotly({
    comp <- inv_comparison()
    req(comp$valid)
    resolved <- inv_resolved()
    title <- paste0(
      DOMAIN_FIGURES[[input$inv_domain]]$heading,
      if (!is.null(resolved$outcome_label)) paste0(": ", resolved$outcome_label) else ""
    )
    build_multi_level_comparison_plot(
      comp, title = title,
      caption = sprintf(
        "Bars show the MONTHLY %% (not the 3-month rolling average shown on the Trends tab). A level with fewer than %d respondents (%d for a subgroup breakdown) in the investigation period shows as 'Suppressed' instead of a bar -- not enough data there to be reliable.",
        N_MIN_OVERALL, N_MIN_SUBGROUP
      )
    )
  })

  output$inv_reference_ui <- renderUI({
    comp <- inv_comparison()
    req(comp$valid)
    build_investigate_notes_ui(comp)
  })

  # Full detail is CSV-only, no on-screen table: the plot plus the notes
  # above already carry everything needed on screen.
  inv_table_data <- reactive({
    comp <- inv_comparison()
    req(comp$valid)
    # comp$levels$weighted_prop already holds whichever avg_type was
    # actually chosen (weighted or unweighted month-averaging) -- the
    # column header needs to say which, instead of always claiming
    # "Weighted %" regardless of what's really in it.
    pct_col <- if (identical(comp$reference$avg_type, "unweighted")) "Unweighted %" else "Weighted %"
    comp$levels %>%
      dplyr::transmute(
        Grouping = grouping,
        Level = level,
        N = n,
        "{pct_col}" := ifelse(suppressed, NA_real_, round(weighted_prop * 100, 1)),
        `Deviation vs. Reference (pts)` = round(deviation_pts, 1),
        Suppressed = ifelse(suppressed, "Yes", "No"),
        Reason = suppress_reason
      )
  })

  # Field/Value metadata so the CSV is self-contained without needing the
  # dashboard open alongside it.
  inv_meta_data <- reactive({
    comp <- inv_comparison()
    req(comp$valid)
    ref <- comp$reference
    resolved <- inv_resolved()

    excluded_text <- if (nrow(ref$excluded) == 0) {
      "None"
    } else {
      paste(sprintf("%s (%s)", ref$excluded$month, ref$excluded$reason), collapse = "; ")
    }

    tibble::tibble(
      Field = c(
        "Domain", "Outcome", "Reference average type used",
        "Reference % (chosen)", "Reference % (weighted avg)", "Reference % (unweighted avg)",
        "Reference months included", "Reference total responses",
        "Reference months excluded", "Investigation period months",
        "Investigation total responses"
      ),
      Value = c(
        DOMAIN_FIGURES[[input$inv_domain]]$heading,
        if (!is.null(resolved$outcome_label)) resolved$outcome_label else NA_character_,
        ref$avg_type,
        sprintf("%.1f%%", ref$chosen_avg * 100),
        sprintf("%.1f%%", ref$n_weighted_avg * 100),
        sprintf("%.1f%%", ref$unweighted_avg * 100),
        paste(ref$months, collapse = "; "),
        format(ref$total_responses, big.mark = ",", trim = TRUE),
        excluded_text,
        paste(comp$investigation_overall$month, collapse = "; "),
        format(comp$investigation_total_responses, big.mark = ",", trim = TRUE)
      )
    )
  })

  output$download_inv_table <- downloadHandler(
    filename = function() paste0("investigate_", input$inv_domain, ".csv"),
    content = function(file) {
      utils::write.table(inv_meta_data(), file, sep = ",", row.names = FALSE, col.names = TRUE)
      cat("\n", file = file, append = TRUE)
      utils::write.table(inv_table_data(), file, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)
    }
  )

  # ---- Sample Description tab ----
  #
  # Table A/B/C each resolve to exactly one of build_sample_description_table()'s
  # 3 precomputed units (Overall/Month/Survey) -- never an arbitrary
  # date range, since only these 3 have an exact unique-participant
  # count available (see that function's header for why).

  sample_month_a_picker <- monthPickerServer("sample_month_a")
  sample_month_b_picker <- monthPickerServer("sample_month_b")
  sample_month_c_picker <- monthPickerServer("sample_month_c")

  #' Resolves one Table's radio selection + its own picker input into a
  #' summarize_sample_description_unit()-ready unit, plus a display label.
  resolve_sample_window <- function(window_choice, month_picker, survey_choice) {
    unit <- switch(window_choice,
      "Overall" = list(type = "overall"),
      "Month"   = { req(month_picker()); list(type = "month", month = month_picker()) },
      "Survey"  = {
        req(survey_choice)
        parsed <- parse_survey_choice(survey_choice)
        label  <- names(SAMPLE_SURVEY_CHOICES)[SAMPLE_SURVEY_CHOICES == survey_choice][[1]]
        list(type = "survey", survey_type = parsed$survey_type, survey_number = parsed$survey_number,
             survey_label = label)
      }
    )
    list(unit = unit, label = describe_sample_unit(unit))
  }

  sample_window_a <- reactive({
    req(input$sample_window_a)
    resolve_sample_window(input$sample_window_a, sample_month_a_picker, input$sample_survey_a)
  })

  sample_window_b <- reactive({
    req(isTRUE(input$sample_compare_two), input$sample_window_b)
    resolve_sample_window(input$sample_window_b, sample_month_b_picker, input$sample_survey_b)
  })

  sample_window_c <- reactive({
    req(isTRUE(input$sample_compare_two), isTRUE(input$sample_compare_three), input$sample_window_c)
    resolve_sample_window(input$sample_window_c, sample_month_c_picker, input$sample_survey_c)
  })

  # Every window currently activated (Table A always; B/C only when
  # their checkbox is on).
  sample_active_windows <- reactive({
    windows <- list(sample_window_a())
    if (isTRUE(input$sample_compare_two)) windows <- c(windows, list(sample_window_b()))
    if (isTRUE(input$sample_compare_two) && isTRUE(input$sample_compare_three)) {
      windows <- c(windows, list(sample_window_c()))
    }
    windows
  })

  #' A canonical string key for one unit, used only to detect 2 Tables
  #' resolving to the exact same unit below.
  sample_unit_key <- function(unit) {
    switch(unit$type,
      "overall" = "overall",
      "month"   = paste0("month_", format(unit$month, "%Y-%m")),
      "survey"  = paste0("survey_", unit$survey_type, "_", unit$survey_number)
    )
  }

  # Two windows "repeat" if they resolve to the exact same unit.
  # Which letters (B, C, ...) duplicate an earlier table.
  sample_duplicate_letters <- reactive({
    windows <- sample_active_windows()
    table_letters <- LETTERS[seq_along(windows)]
    seen <- character(0)
    dup <- character(0)
    for (i in seq_along(windows)) {
      key <- sample_unit_key(windows[[i]]$unit)
      if (key %in% seen) dup <- c(dup, table_letters[i])
      seen <- c(seen, key)
    }
    dup
  })

  # Drops any duplicate window, keeping only its first occurrence.
  sample_deduped_windows <- reactive({
    windows <- sample_active_windows()
    dup_letters <- sample_duplicate_letters()
    if (length(dup_letters) == 0) return(windows)
    windows[!(LETTERS[seq_along(windows)] %in% dup_letters)]
  })

  output$sample_duplicate_warning <- renderUI({
    dup <- sample_duplicate_letters()
    if (length(dup) == 0) return(NULL)
    tags$p(style = "color:#b30000; font-size:12px;",
           "Please select different time points to compare.")
  })

  sample_comparison_table <- reactive({
    req(sample_description, input$sample_categories)
    build_sample_description_comparison(sample_description, sample_deduped_windows(), input$sample_categories)
  })

  output$sample_table <- DT::renderDataTable({
    DT::datatable(
      sample_comparison_table(), style = "bootstrap5", rownames = FALSE,
      options = list(scrollY = "calc(100vh - 320px)", scrollX = TRUE, paging = FALSE)
    )
  })

  output$download_sample <- downloadHandler(
    filename = function() "sample_description.csv",
    content = function(file) write.csv(sample_comparison_table(), file, row.names = FALSE)
  )

  # ---- FPL Distribution chart ----

  fpl_hist_custom_range_picker <- monthRangeInputServer("fpl_hist_custom_range")
  fpl_hist_month_picker        <- monthPickerServer("fpl_hist_month")

  fpl_hist_date_range <- reactive({
    req(input$fpl_hist_window_type)
    switch(input$fpl_hist_window_type,
      "Overall"            = FULL_DATE_RANGE,
      "By Year"            = {
        req(input$fpl_hist_year)
        as.Date(c(sprintf("%s-01-01", input$fpl_hist_year), sprintf("%s-12-31", input$fpl_hist_year)))
      },
      "Custom Date Range"  = fpl_hist_custom_range_picker(),
      "Single Month"       = rep(fpl_hist_month_picker(), 2)
    )
  })

  fpl_hist_title <- reactive({
    label <- switch(input$fpl_hist_window_type,
      "Overall"           = "Overall",
      "By Year"           = input$fpl_hist_year,
      "Custom Date Range" = paste(format(fpl_hist_date_range(), "%b %Y"), collapse = " to "),
      "Single Month"      = format(fpl_hist_date_range()[1], "%B %Y")
    )
    paste0("FPL Distribution -- ", label)
  })

  # Responses are always exact (a plain row count). Participants are
  # exact too for Overall/By Year (compute_fpl_histogram() resolves
  # these from the precomputed totals), but a custom range or single
  # month gets NA back for that -- a single month's response count
  # already equals its participant count (each respondent answers the
  # FPL item at most once per month), so that's noted directly instead
  # of repeating a second, identical-looking number; a genuine custom
  # range gets an explicit note that an exact participant count isn't
  # available, rather than a silent approximation that would look
  # authoritative without being one.
  fpl_hist_caption <- function(hist_data) {
    n_resp <- attr(hist_data, "total_responses")
    n_part <- attr(hist_data, "total_participants")
    resp_str <- sprintf("%s responses", format(n_resp, big.mark = ","))
    if (!is.na(n_part)) {
      sprintf("%s, %s unique participants", resp_str, format(n_part, big.mark = ","))
    } else if (identical(input$fpl_hist_window_type, "Single Month")) {
      sprintf("%s (equal to unique participants for a single month)", resp_str)
    } else {
      sprintf("%s (exact unique participant count isn't available for a custom range)", resp_str)
    }
  }

  # One plotly output per possible bin width, registered up front for all
  # of FPL_HISTOGRAM_BIN_WIDTH_CHOICES. Shiny only evaluates a
  # renderPlotly block when its output has a bound, visible UI element,
  # so an unselected bin width's block never actually runs.
  lapply(FPL_HISTOGRAM_BIN_WIDTH_CHOICES, function(bin_width) {
    local({
      bw <- bin_width
      output[[paste0("fpl_histogram_plot_", bw)]] <- plotly::renderPlotly({
        req(fpl_distribution, fpl_hist_date_range())
        hist_data <- compute_fpl_histogram(
          fpl_distribution, fpl_hist_date_range(), bw,
          window_type = input$fpl_hist_window_type, year = input$fpl_hist_year
        )
        if (nrow(hist_data) == 0) return(empty_plot_message("No FPL data for this time window."))
        build_fpl_histogram_plot(
          hist_data, title = sprintf("%s (%d-point bins)", fpl_hist_title(), bw),
          caption = fpl_hist_caption(hist_data)
        )
      })
    })
  })

  # Metadata block (one row per checked bin width) + the stacked bin
  # table itself, same two-part shape every other tab's CSV download
  # uses -- a self-contained file that doesn't need the dashboard open
  # alongside it to make sense.
  fpl_hist_meta_data <- reactive({
    req(fpl_distribution, fpl_hist_date_range(), input$fpl_hist_bin_width)
    purrr::map_dfr(sort(as.numeric(input$fpl_hist_bin_width)), function(bw) {
      hist_data <- compute_fpl_histogram(
        fpl_distribution, fpl_hist_date_range(), bw,
        window_type = input$fpl_hist_window_type, year = input$fpl_hist_year
      )
      n_part <- attr(hist_data, "total_participants")
      tibble::tibble(
        Window = fpl_hist_title(),
        `Bin Width` = bw,
        `Total Responses` = attr(hist_data, "total_responses"),
        `Total Participants` = ifelse(is.na(n_part), NA_integer_, n_part),
        `Participants Exact?` = dplyr::case_when(
          !is.na(n_part)                                           ~ "Yes",
          identical(input$fpl_hist_window_type, "Single Month")    ~ "Equal to responses for a single month",
          TRUE                                                      ~ "Not available for a custom date range"
        )
      )
    })
  })

  fpl_hist_table_data <- reactive({
    req(fpl_distribution, fpl_hist_date_range(), input$fpl_hist_bin_width)
    purrr::map_dfr(sort(as.numeric(input$fpl_hist_bin_width)), function(bw) {
      hist_data <- compute_fpl_histogram(
        fpl_distribution, fpl_hist_date_range(), bw,
        window_type = input$fpl_hist_window_type, year = input$fpl_hist_year
      )
      if (nrow(hist_data) == 0) return(NULL)
      hist_data %>%
        dplyr::transmute(
          Window = fpl_hist_title(), `Bin Width` = bw, `FPL Bin` = label,
          `N (Responses)` = n, `% of Responses` = round(pct, 1)
        )
    })
  })

  output$download_fpl_hist <- downloadHandler(
    filename = function() "fpl_distribution.csv",
    content = function(file) {
      utils::write.table(fpl_hist_meta_data(), file, sep = ",", row.names = FALSE, col.names = TRUE)
      cat("\n", file = file, append = TRUE)
      utils::write.table(fpl_hist_table_data(), file, sep = ",", row.names = FALSE, col.names = TRUE, append = TRUE)
    }
  )

  output$fpl_histogram_plots_ui <- renderUI({
    selected <- as.numeric(input$fpl_hist_bin_width)
    if (length(selected) == 0) {
      return(tags$p(class = "text-muted", "Check at least one bin width in the sidebar to see the FPL distribution."))
    }
    selected <- sort(selected)
    plots <- lapply(selected, function(bw) {
      tags$div(style = "margin-bottom: 20px;",
               plotly::plotlyOutput(paste0("fpl_histogram_plot_", bw), height = "500px"))
    })
    if (length(selected) > 1) {
      tags$div(style = "max-height: calc(100vh - 260px); overflow-y: auto;", plots)
    } else {
      tags$div(plots)
    }
  })
}

shinyApp(ui, server, enableBookmarking = "url")
