# dashboard_helpers.R
#
# Dashboard-only logic that isn't generic enough to live in R/07_reporting.R:
# Sample Description tab filtering, the Focus level dropdown, and the
# headline KPI scorecard. Plain R, no Shiny dependency, unit tested in
# tests/test_dashboard_helpers.R.
#
# Assumes R/00_config.R and R/07_reporting.R are already sourced.

library(dplyr)


#' Levels of `grouping_label` present in this domain's trend data, for the
#' Focus level dropdown.
get_levels_for_grouping <- function(trend_tables, domain_key, grouping_label) {
  levels_present <- trend_tables %>%
    dplyr::filter(domain == domain_key, grouping == grouping_label) %>%
    dplyr::pull(level) %>%
    unique()
  order_grouping_levels(levels_present, grouping_label)
}

#' Months that exist for one slice of the data, used to constrain the
#' Investigate tab's month pickers. Suppressed months are included since
#' they're still real rows.
get_available_dates_for_slice <- function(monthly_tables, domain_key, figure_title, grouping_label, level_name,
                                            outcome_label = NULL) {
  out <- monthly_tables %>%
    dplyr::filter(domain == domain_key, figure_title == !!figure_title, grouping == grouping_label, level == level_name)
  if (!is.null(outcome_label)) out <- dplyr::filter(out, outcome == outcome_label)
  sort(unique(out$date))
}

#' Convert a "YYYY/MM" month label to a Date (the 1st of that month).
date_from_month <- function(month_label) {
  as.Date(paste0(gsub("/", "-", month_label), "-01"))
}

#' Snap any Date down to the 1st of its own month. A dateRangeInput lets a
#' user pick any day, so without this a start date of Aug 5 would exclude
#' the August row entirely.
floor_to_month <- function(d) as.Date(format(as.Date(d), "%Y-%m-01"))

#' Filter the sample_description table to one grouping and date range.
filter_sample_description <- function(sample_description, grouping_label, date_range) {
  sample_description %>%
    dplyr::filter(
      grouping == grouping_label,
      date_from_month(month) >= date_range[1],
      date_from_month(month) <= date_range[2]
    )
}

#' Bin widths the FPL distribution chart's dropdown offers. Each is an
#' exact multiple of FPL_FINE_BIN_WIDTH so re-bucketing below never needs
#' to approximate.
FPL_HISTOGRAM_BIN_WIDTH_CHOICES <- c(10, 25, 50, 100)

#' Re-bucket fpl_distribution.rds's fine grid into coarser bins for one
#' time window. Lets the dashboard offer an adjustable bin width with no
#' respondent-level data involved.
#' @param fpl_distribution build_fpl_distribution_table()'s output:
#'   list(bins, participants_overall, participants_by_year).
#' @param date_range c(start_date, end_date).
#' @param bin_width Must be an exact multiple of FPL_FINE_BIN_WIDTH.
#' @param window_type Which time-window control picked date_range --
#'   "Overall" or "By Year" get an EXACT participant count (precomputed
#'   once from real respondent-level data at pipeline-build time); any
#'   other value (a custom date range, a single month) only ever gets
#'   the response count, since an exact participant count for a window
#'   the pipeline couldn't have anticipated needs respondent-level data
#'   this dashboard never has, and naively summing each month's own
#'   distinct-participant count would double-count anyone who answered
#'   in more than one month of that window.
#' @param year The selected year, required (and only used) when
#'   window_type == "By Year".
#' @return One row per coarse bin: bin_low, label, n, pct, ordered by
#'   bin_low. Empty if nothing in this window has a usable FPL value.
#'   Two attributes carry the window's totals: "total_responses"
#'   (always exact -- a plain count of real rows, always obtainable)
#'   and "total_participants" (exact for Overall/By Year, NA otherwise
#'   -- NA, not an approximate guess, since an approximation here would
#'   look authoritative without being one).
compute_fpl_histogram <- function(fpl_distribution, date_range, bin_width,
                                   window_type = "Custom Date Range", year = NULL) {
  bins <- fpl_distribution$bins
  filtered <- bins %>%
    dplyr::filter(
      date_from_month(month) >= date_range[1],
      date_from_month(month) <= date_range[2]
    )

  total_responses <- sum(filtered$n)
  total_participants <- if (identical(window_type, "Overall")) {
    fpl_distribution$participants_overall
  } else if (identical(window_type, "By Year") && !is.null(year)) {
    by_year <- fpl_distribution$participants_by_year
    match_row <- by_year$n_participants[by_year$survey_year == as.character(year)]
    if (length(match_row) == 0) NA_integer_ else match_row[[1]]
  } else {
    NA_integer_
  }

  result <- if (nrow(filtered) == 0) {
    filtered[0, c("bin_low", "n")] %>% dplyr::mutate(label = character(0), pct = numeric(0))
  } else {
    filtered %>%
      dplyr::mutate(bin_low = floor(bin_low / bin_width) * bin_width) %>%
      dplyr::group_by(bin_low) %>%
      dplyr::summarise(n = sum(n), .groups = "drop") %>%
      dplyr::mutate(
        label = ifelse(
          bin_low >= FPL_FINE_BIN_MAX,
          sprintf("%d%%+ FPL", FPL_FINE_BIN_MAX),
          sprintf("%d-%d%% FPL", bin_low, bin_low + bin_width)
        ),
        pct = 100 * n / sum(n)
      ) %>%
      dplyr::arrange(bin_low)
  }
  attr(result, "total_responses") <- total_responses
  attr(result, "total_participants") <- total_participants
  result
}

#' Resolve a time-window preset to an actual c(start_date, end_date).
#' @param preset One of "Overall", "Latest Month", "Custom".
#' @param full_range The full survey's c(min_date, max_date).
#' @param custom_range Only used when preset == "Custom".
resolve_time_window <- function(preset, full_range, custom_range = NULL) {
  # A reactive can see input$<preset_id> as NULL or length 0 before the
  # session's first real value arrives, so fall back to the full range
  # rather than erroring on the comparison below.
  if (is.null(preset) || length(preset) == 0) return(full_range)
  if (preset == "Overall") return(full_range)
  if (preset == "Latest Month") return(c(full_range[2], full_range[2]))
  custom_range
}

#' A short label describing a resolved sample-description unit, used as
#' a column header in build_sample_description_comparison()'s output.
#' @param unit list(type = "overall") | list(type = "month", month = Date)
#'   | list(type = "survey", survey_label = ...).
describe_sample_unit <- function(unit) {
  switch(unit$type,
    "overall" = "Overall (full survey)",
    "month"   = sprintf("Month: %s", format(unit$month, "%Y/%m")),
    "survey"  = unit$survey_label
  )
}

#' Table-1-style summary of sample_description for exactly ONE
#' precomputed unit -- the whole study, one calendar month, or one
#' SurveyType x SurveyNumber wave: one row per (Category, Level), with
#' "N (%)" from build_sample_description_table()'s exact
#' n_participants. The percent is of that category's own total in the
#' unit, not the grand total.
#'
#' Never sums n_participants across more than one month/wave -- that
#' would double-count anyone who appears in more than one of them.
#' Each unit's number is already the exact distinct-CALID count
#' build_sample_description_table() computed directly from
#' respondent-level data; this function only selects and formats it.
#' @param sample_description build_sample_description_table()'s output:
#'   list(overall, by_month, by_survey).
#' @param unit list(type = "overall") | list(type = "month", month = Date)
#'   | list(type = "survey", survey_type = ..., survey_number = ...).
#' @param grouping_labels Which categories to include. NULL means all.
summarize_sample_description_unit <- function(sample_description, unit, grouping_labels = NULL) {
  filtered <- switch(unit$type,
    "overall" = sample_description$overall,
    "month"   = sample_description$by_month %>%
      dplyr::filter(month == format(unit$month, "%Y/%m")) %>%
      dplyr::select(-month),
    "survey"  = sample_description$by_survey %>%
      dplyr::filter(SurveyType == unit$survey_type, SurveyNumber == unit$survey_number) %>%
      dplyr::select(grouping, level, n_participants)
  )
  if (!is.null(grouping_labels)) filtered <- dplyr::filter(filtered, grouping %in% grouping_labels)

  filtered %>%
    dplyr::group_by(grouping) %>%
    dplyr::mutate(pct = 100 * n_participants / sum(n_participants)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      Category = grouping,
      Level    = level,
      `N (%)`  = sprintf("%s (%.1f%%)", format(n_participants, big.mark = ",", trim = TRUE), pct)
    )
}

#' The Sample Description tab's full table: Category/Level columns, then
#' one "N (%)" column per requested unit (1-3). A (Category, Level)
#' present in one unit but not another still appears, with NA in
#' whichever column it's missing from.
#' @param windows A list of list(unit = ..., label = ...) -- see
#'   summarize_sample_description_unit()'s `unit` param.
#' @param category_order Display order for the Category column. Anything
#'   present in the data but not listed sorts after the listed ones.
build_sample_description_comparison <- function(sample_description, windows, grouping_labels = NULL,
                                                  category_order = DEFAULT_SAMPLE_DESCRIPTION_CATEGORY_ORDER) {
  tables <- lapply(windows, function(w) {
    summarize_sample_description_unit(sample_description, w$unit, grouping_labels) %>%
      dplyr::rename(!!w$label := `N (%)`)
  })
  combined <- Reduce(function(a, b) dplyr::full_join(a, b, by = c("Category", "Level")), tables)

  present_order <- category_order[category_order %in% unique(combined$Category)]

  # Within each Category, Level follows that grouping's own display
  # order (lowest-to-highest FPL for Income Level, etc.) via the same
  # order_grouping_levels() every other Level column in this pipeline
  # respects -- a plain alphabetical arrange() would put Income Level's
  # bands out of numeric order.
  combined %>%
    dplyr::mutate(Category = factor(Category, levels = present_order)) %>%
    dplyr::group_by(Category) %>%
    dplyr::group_modify(~ {
      .x$Level <- factor(.x$Level, levels = order_grouping_levels(unique(.x$Level), as.character(.y$Category)))
      dplyr::arrange(.x, Level)
    }) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Category = as.character(Category), Level = as.character(Level))
}


# -----------------------------------------------------------------------------
# Headline scorecard (Trends tab): one stat tile per measure a domain has,
# under a domain sub-heading, for a fixed geography (Overall or LA
# County). Always shows every measure, independent of the Trends tab's
# own current filter selections.
# -----------------------------------------------------------------------------

#' Whether a higher value is a worse outcome for this domain, used to
#' color the delta arrow. NA when that's not a meaningful judgment (more
#' center-based child care isn't inherently good or bad).
DOMAIN_HIGHER_IS_WORSE <- c(
  material_hardship = TRUE, parent_distress = TRUE, child_behavior = TRUE,
  healthcare = TRUE, food_insecurity = TRUE, child_care = NA
)

#' Every measure a domain tracks as its own KPI, organized into groups
#' so the scorecard can show one labeled sub-row per threshold rather
#' than a single flat list where the threshold is buried in each tile's
#' label. Reuses the same get_measure_choices()/get_threshold_choices()/
#' resolve_domain_figure() machinery the Trends tab's own pins use, so
#' this can't drift from what the domain actually offers.
#'
#' Domains with BOTH a measure and a threshold axis (Parent Distress,
#' Child Behavior) get one group per threshold -- e.g. "Very frequent"
#' and "At least somewhat frequent" -- each containing one tile per
#' measure (Any, Depression, Anxiety, ...) at that threshold. Both
#' severity cuts are shown this way, rather than collapsing to just the
#' Higher cut and silently dropping the Lower-threshold numbers.
#'
#' Domains with a measure axis only (Material Hardship, Child Care) or a
#' threshold axis only (Food Insecurity) get a single group -- there's
#' only one axis to show, so there's nothing to split into sub-rows.
#'
#' @return A list of list(suffix, specs) groups. `suffix` is NULL for a
#'   single-group domain (the row heading is just the domain name) or
#'   this group's threshold display text otherwise (appended to the
#'   heading as "<Domain> (<suffix>)"). `specs` is a list of
#'   list(label, figure_title, outcome_label) for compute_kpi_tile().
domain_kpi_groups <- function(domain_key) {
  measures <- get_measure_choices(domain_key)
  if (!is.null(measures)) {
    threshold_choices <- get_threshold_choices(domain_key)
    if (is.null(threshold_choices)) {
      specs <- lapply(measures, function(m) {
        resolved <- resolve_domain_figure(domain_key, "Grouping", measure_pin = m)
        list(label = m, figure_title = resolved$figure_title, outcome_label = resolved$outcome_label)
      })
      return(list(list(suffix = NULL, specs = specs)))
    }
    return(lapply(seq_along(threshold_choices), function(i) {
      code       <- threshold_choices[[i]]
      label_text <- names(threshold_choices)[[i]]
      specs <- lapply(measures, function(m) {
        resolved <- resolve_domain_figure(domain_key, "Grouping", measure_pin = m, threshold_pin = code)
        list(label = m, figure_title = resolved$figure_title, outcome_label = resolved$outcome_label)
      })
      list(suffix = label_text, specs = specs)
    }))
  }

  threshold_spec <- DOMAIN_THRESHOLD_ONLY[[domain_key]]
  if (!is.null(threshold_spec)) {
    specs <- lapply(threshold_spec$thresholds, function(code) {
      label <- unname(threshold_spec$threshold_labels[[code]])
      list(label = label, figure_title = threshold_spec$figure_title, outcome_label = label)
    })
    return(list(list(suffix = NULL, specs = specs)))
  }

  fig <- DOMAIN_FIGURES[[domain_key]]$figures[[1]]
  list(list(suffix = NULL, specs = list(list(label = fig$title, figure_title = fig$title, outcome_label = NULL))))
}

#' One measure's headline number for one geography: the latest available
#' month's value and the change versus the prior available month, both
#' from the same series so it matches whatever the Trends tab is showing.
#' @param grouping_label "Overall" or "LA County".
#' @return NULL if no data yet for this geography, otherwise
#'   list(label, value, delta, date, higher_is_worse). delta is NA with
#'   no earlier month to compare against.
compute_kpi_tile <- function(trend_tables, domain_key, spec, grouping_label, series = "monthly") {
  panel <- prepare_single_level_panel_data(
    trend_tables, domain_key, spec$figure_title, grouping_label, outcome_label = spec$outcome_label
  )
  est_col <- if (series == "monthly") "weighted_prop" else "rolling_weighted_prop"
  ordered <- panel %>% dplyr::filter(!is.na(.data[[est_col]])) %>% dplyr::arrange(date)
  if (nrow(ordered) == 0) return(NULL)

  latest <- utils::tail(ordered, 1)
  prior  <- if (nrow(ordered) >= 2) ordered[nrow(ordered) - 1, ] else NULL

  list(
    label           = spec$label,
    value           = latest[[est_col]][[1]],
    date            = latest$date[[1]],
    delta           = if (is.null(prior)) NA_real_ else latest[[est_col]][[1]] - prior[[est_col]][[1]],
    higher_is_worse = unname(DOMAIN_HIGHER_IS_WORSE[domain_key])
  )
}

#' Percentage-point threshold below which a KPI's month-over-month (or
#' rolling-window-over-rolling-window) move is treated as "stable"
#' (yellow) rather than a real improvement/worsening (green/red). A
#' single survey wave's estimate wobbles by a few points on sampling
#' noise alone, so a move this small is colored the same as no move at
#' all rather than implying a real change. Surfaced to the user via the
#' color-key note next to the scorecard (see app.R) -- keep that note's
#' wording in sync if this changes.
KPI_STABLE_THRESHOLD_PTS <- 5

#' Render one compute_kpi_tile() result as a compact stat tile: label,
#' value, and a signed delta with a colored arrow. Color is always paired
#' with the arrow glyph and signed number, never used alone.
#'   - Green/red only when the move exceeds KPI_STABLE_THRESHOLD_PTS
#'     percentage points, with the direction-to-color mapping set by
#'     higher_is_worse.
#'   - Yellow/amber ("stable") for any smaller move, including exactly
#'     zero.
#'   - Gray/neutral when there's no prior period to compare, or when
#'     higher_is_worse is NA -- no color judgment is meaningful for that
#'     domain at all (e.g. Child Care: more or less center-based care
#'     isn't inherently better or worse), so this is never colored
#'     green/red/yellow regardless of the size of the move.
render_stat_tile <- function(tile) {
  value_str <- sprintf("%.1f%%", tile$value * 100)
  delta_pts <- tile$delta * 100

  if (is.na(tile$delta) || is.na(tile$higher_is_worse)) {
    arrow <- if (is.na(tile$delta)) "" else if (delta_pts == 0) "▬" else if (delta_pts > 0) "▲" else "▼"
    delta_color <- "#898781"
    tint_color  <- "#f5f5f4"
  } else if (abs(delta_pts) <= KPI_STABLE_THRESHOLD_PTS) {
    arrow <- if (delta_pts == 0) "▬" else if (delta_pts > 0) "▲" else "▼"
    delta_color <- "#b8860b"
    tint_color  <- "#fdf6d8"
  } else {
    is_up <- delta_pts > 0
    bad <- is_up == tile$higher_is_worse
    arrow <- if (is_up) "▲" else "▼"
    delta_color <- if (bad) "#d03b3b" else "#0ca30c"
    tint_color  <- if (bad) "#fdeeec" else "#eaf7ea"
  }
  delta_str <- if (is.na(tile$delta)) "no prior month" else sprintf("%s %+.1f pts", arrow, delta_pts)

  htmltools::tags$div(
    title = tile$label, class = "rapid-stat-tile",
    style = sprintf(
      "border: 1px solid rgba(0,0,0,0.06); border-left: 4px solid %s; background: %s; padding:8px 12px; min-width:110px;",
      delta_color, tint_color
    ),
    htmltools::tags$div(style = "font-size:13px; color:#52514e; white-space:nowrap; overflow:hidden; text-overflow:ellipsis;", tile$label),
    htmltools::tags$div(style = "font-size:20px; font-weight:600; color:#0b0b0b; line-height:1.2;", value_str),
    htmltools::tags$div(style = paste0("font-size:12px; color:", delta_color, "; font-weight:700;"), delta_str)
  )
}

#' Converts an internal threshold code ("Lower"/"Higher"/"Moderate") to
#' this domain's own display text (e.g. "Very frequent", "Overall").
#' NULL in, NULL out. Falls back to the raw code itself if this domain
#' has no threshold axis or the code isn't one of its real values, so a
#' caller always gets a string back, never an error.
threshold_code_to_label <- function(domain_key, code) {
  if (is.null(code)) return(NULL)
  choices <- get_threshold_choices(domain_key)
  if (is.null(choices)) return(code)
  label <- names(choices)[choices == code]
  if (length(label) == 0) code else label[[1]]
}

#' One domain's KPI tiles, as one or more labeled sub-rows -- one
#' sub-row per domain_kpi_groups() group, e.g. for Child Behavior:
#' "Child Behavior (Often or very true)" with its 3 measure tiles, then
#' "Child Behavior (At least sometimes true)" with its 3 measure tiles,
#' each clearly its own heading rather than one flat row where the
#' threshold is buried inside each tile. NULL if none of the domain's
#' groups have data yet for this geography.
build_domain_kpi_row <- function(trend_tables, domain_key, grouping_label, series = "monthly") {
  groups <- domain_kpi_groups(domain_key)
  base_heading <- DOMAIN_FIGURES[[domain_key]]$heading

  sub_rows <- lapply(groups, function(group) {
    tiles <- lapply(group$specs, function(spec) {
      tile <- compute_kpi_tile(trend_tables, domain_key, spec, grouping_label, series)
      if (is.null(tile)) return(NULL)
      render_stat_tile(tile)
    })
    tiles <- Filter(Negate(is.null), tiles)
    if (length(tiles) == 0) return(NULL)

    heading <- if (is.null(group$suffix)) base_heading else paste0(base_heading, " (", group$suffix, ")")

    htmltools::tags$div(
      style = "margin-bottom:8px;",
      htmltools::tags$div(
        style = "font-size:14px; font-weight:700; color:#0b0b0b; margin-bottom:4px;",
        heading
      ),
      htmltools::tags$div(style = "display:flex; flex-wrap:wrap; gap:10px;", tiles)
    )
  })
  sub_rows <- Filter(Negate(is.null), sub_rows)
  if (length(sub_rows) == 0) return(NULL)

  htmltools::tags$div(sub_rows)
}

#' The full headline scorecard for one geography: one KPI row per domain,
#' in DOMAIN_FIGURES's order, skipping any domain with no data yet.
#' @param grouping_label "Overall" or "LA County".
build_headline_scorecard <- function(trend_tables, grouping_label = "Overall", series = "monthly") {
  rows <- lapply(names(DOMAIN_FIGURES), function(domain_key) {
    build_domain_kpi_row(trend_tables, domain_key, grouping_label, series)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(NULL)
  htmltools::tags$div(rows)
}
