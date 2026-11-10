
# 05_excel_export.R

# Exports every domain's weighted monthly and 3-month-rolling estimates
# to Excel workbooks. One workbook per domain, one tab per grouping cut
# (Overall, then each demographic grouping, then LA County). A styled
# header block per group level, stacked vertically in a tab, or (for
# multi-measure domains) one header block per outcome with the group
# blocks nested inside that.
#
# Each table has Month, N, Monthly %, Rolling N, 3-Month Rolling %.
# Weighted only. Unweighted numbers live in the plots and the
# dashboard's download table instead. Excel also gets 3 extra columns
# the plots don't show: Rolling Window (the 3 calendar months a Rolling
# % was averaged from), Reason (why a month's Monthly % is suppressed),
# and Rolling Reason (same idea for the Rolling %, tracked separately
# since a thin month can still contribute to a reliable 3-month total).
#
# export_all_domains() takes the SAME cuts R/06_reporting.R already
# computed for the dashboard's trend tables, rather than recomputing
# calc_trend()/calc_multi_trend() a second time. The rolling margin of
# error now runs a real bootstrap, so computing it twice for the same
# numbers is expensive for nothing.


library(openxlsx)
library(dplyr)
library(tidyr)
library(purrr)



# 1. Which columns make up each domain's outcome(s)

# Single-outcome domains export one sheet set. Multi-outcome domains get
# one outcome header block per measure inside each grouping tab. Names
# are the display labels used in the outcome header; values are the 0/1
# column names.

MATERIAL_HARDSHIP_TYPES <- c(
  Food        = "fs_food",
  Housing     = "fs_housing",
  Utility     = "fs_utility",
  Healthcare  = "fs_healthcare",
  'Child Care' = "fs_childcare"
)

# Parent Distress and Child Behavior each have two independent axes:
# which measure, and which threshold (Lower/Higher). Everything below
# is derived from these two lists so a column rename only has to happen
# in one place. "Any" is the composite across every individual measure.
PARENT_DISTRESS_MEASURES <- list(
  Any        = c(Lower = "distress_any_lower", Higher = "distress_any_higher"),
  Depression = c(Lower = "depress_lower",      Higher = "depress_higher"),
  Anxiety    = c(Lower = "anxiety_lower",       Higher = "anxiety_higher"),
  Stress     = c(Lower = "stress_lower",        Higher = "stress_higher"),
  Loneliness = c(Lower = "lonely_lower",        Higher = "lonely_higher")
)
CHILD_BEHAVIOR_MEASURES <- list(
  Any   = c(Lower = "child_any_lower", Higher = "child_any_higher"),
  Fussy = c(Lower = "fussy_lower",     Higher = "fussy_higher"),
  Fear  = c(Lower = "fear_lower",      Higher = "fear_higher")
)

# Display text for each domain's thresholds, shown in legends and the
# Threshold dropdown. The internal codes (Lower/Higher/Moderate) never
# change; only this wording does.
PARENT_DISTRESS_THRESHOLD_LABELS <- c(
  Lower  = "At least somewhat frequent",
  Higher = "Very frequent"
)
CHILD_BEHAVIOR_THRESHOLD_LABELS <- c(
  Lower  = "At least sometimes true",
  Higher = "Often or very true"
)
FOOD_INSECURITY_THRESHOLD_LABELS <- c(
  Lower    = "Overall",
  Moderate = "Moderate",
  Higher   = "Severe"
)

# One measure's Lower/Higher pair, relabeled with this domain's own
# display text.
composite_vars_for <- function(measures_list, measure_name, threshold_labels) {
  pair <- measures_list[[measure_name]]
  setNames(
    c(pair[["Lower"]], pair[["Higher"]]),
    unname(threshold_labels[c("Lower", "Higher")])
  )
}
# Every measure except "Any", at one fixed threshold.
measures_at_threshold <- function(measures_list, threshold) {
  individual <- measures_list[setdiff(names(measures_list), "Any")]
  vapply(individual, function(pair) pair[[threshold]], character(1))
}

PARENT_DISTRESS_COMPOSITE   <- composite_vars_for(PARENT_DISTRESS_MEASURES, "Any", PARENT_DISTRESS_THRESHOLD_LABELS)
PARENT_DISTRESS_DEPRESSION  <- composite_vars_for(PARENT_DISTRESS_MEASURES, "Depression", PARENT_DISTRESS_THRESHOLD_LABELS)
PARENT_DISTRESS_ANXIETY     <- composite_vars_for(PARENT_DISTRESS_MEASURES, "Anxiety", PARENT_DISTRESS_THRESHOLD_LABELS)
PARENT_DISTRESS_STRESS      <- composite_vars_for(PARENT_DISTRESS_MEASURES, "Stress", PARENT_DISTRESS_THRESHOLD_LABELS)
PARENT_DISTRESS_LONELINESS  <- composite_vars_for(PARENT_DISTRESS_MEASURES, "Loneliness", PARENT_DISTRESS_THRESHOLD_LABELS)
PARENT_DISTRESS_LOWER       <- measures_at_threshold(PARENT_DISTRESS_MEASURES, "Lower")
PARENT_DISTRESS_HIGHER      <- measures_at_threshold(PARENT_DISTRESS_MEASURES, "Higher")

CHILD_BEHAVIOR_COMPOSITE <- composite_vars_for(CHILD_BEHAVIOR_MEASURES, "Any", CHILD_BEHAVIOR_THRESHOLD_LABELS)
CHILD_BEHAVIOR_FUSSY     <- composite_vars_for(CHILD_BEHAVIOR_MEASURES, "Fussy", CHILD_BEHAVIOR_THRESHOLD_LABELS)
CHILD_BEHAVIOR_FEAR      <- composite_vars_for(CHILD_BEHAVIOR_MEASURES, "Fear", CHILD_BEHAVIOR_THRESHOLD_LABELS)
CHILD_BEHAVIOR_LOWER     <- measures_at_threshold(CHILD_BEHAVIOR_MEASURES, "Lower")
CHILD_BEHAVIOR_HIGHER    <- measures_at_threshold(CHILD_BEHAVIOR_MEASURES, "Higher")

# "Any Non-Parental Care" is left out here on purpose. It's still
# computed as a fallback inside the domain logic, but only these 3
# mutually informative types get reported.
CHILD_CARE_TYPES <- c(
  'Parent-Only Care'       = "parent_only_care",
  'Center-Based Care'      = "center_based_care",
  'Non-Center-Based Care'  = "non_center_based_care"
)

HEALTHCARE_OVERALL       <- c('Missed Well-Baby/Well-Child Check-Up' = "missed_checkup")
FOOD_INSECURITY_OVERALL  <- c('Food Insecure' = "fi_insecure")

# Severity classification. A separate export from FOOD_INSECURITY_OVERALL
# above, not a replacement, same Lower/Moderate/Higher pattern as Parent
# Distress and Child Behavior.
FOOD_INSECURITY_SEVERITY <- setNames(
  c("fi_lower", "fi_moderate", "fi_higher"),
  unname(FOOD_INSECURITY_THRESHOLD_LABELS[c("Lower", "Moderate", "Higher")])
)



# 2. Sheet name safety (Excel forbids / \ ? * [ ] : in a sheet name, and
#    caps names at 31 characters)


sanitize_sheet_name <- function(name) {
  invalid_chars <- c("/", "\\", "?", "*", "[", "]", ":")
  for (ch in invalid_chars) {
    name <- gsub(ch, "-", name, fixed = TRUE)
  }
  substr(name, 1, 31)
}



# 3. Build the 8-column export table for one month x group x outcome cut


# Excel shows every month, including ones suppressed for low sample
# size or with zero valid responses, with a Reason column explaining
# why. Unlike the dashboard (which blanks a suppressed value so it can
# never leak into a plot), Excel always shows the real computed
# %/Rolling % alongside the Reason text. The workbook is meant to be a
# full record of everything calculated, not a publication-safe view.

# @param monthly_all One row per month, with suppressed/suppress_reason.
# @param rolling_all One row per month (same rows as monthly_all) with
#   its own independent rolling_suppressed/rolling_suppress_reason. A
#   month can be individually suppressed while its rolling estimate
#   still shows, or the other way around, so these never assume the
#   monthly suppressed flag.
build_export_table <- function(monthly_all, rolling_all, month_col = "month") {
  rolling_slim <- rolling_all %>%
    dplyr::select(dplyr::all_of(month_col), rolling_n, rolling_weighted_prop,
                  window_months, rolling_suppressed, rolling_suppress_reason)

  monthly_all %>%
    dplyr::arrange(date) %>%
    dplyr::left_join(rolling_slim, by = month_col) %>%
    dplyr::transmute(
      Month                 = .data[[month_col]],
      N                     = n,
      'Monthly %'           = round(weighted_prop * 100, 1),
      'Rolling N'           = rolling_n,
      '3-Month Rolling %'   = round(rolling_weighted_prop * 100, 1),
      'Rolling Window'      = window_months,
      Reason                = suppress_reason,
      'Rolling Reason'      = rolling_suppress_reason
    )
}



# 4. Sheet writer: one tab, with styled header blocks stacked vertically


HEADER_STYLE <- openxlsx::createStyle(
  fontColour = "#FFFFFF", fgFill = "#4472C4", textDecoration = "bold", halign = "center"
)
GROUP_STYLE <- openxlsx::createStyle(
  fgFill = "#D9E2F3", textDecoration = "bold"
)
OUTCOME_STYLE <- openxlsx::createStyle(
  fgFill = "#E2EFDA", textDecoration = "bold"
)
EXPORT_TABLE_NCOL <- 8  # Month, N, Monthly %, Rolling N, 3-Month Rolling %, Rolling Window, Reason, Rolling Reason

# If outcome_var_col is given, blocks are nested outcome first, then
# group second (if group_var is also given), then the table. If only
# group_var is given, one block per group level. If neither is given,
# it's a single plain table (the Overall tab for a single-outcome
# domain).

# @param monthly_all,rolling_all Kept as two parallel data frames
#   (rather than one pre-merged table) so group/outcome filtering only
#   has to happen once, consistently, on both.
write_export_sheet <- function(wb, sheet_name, monthly_all, rolling_all,
                                group_var = NULL, outcome_var_col = NULL,
                                month_col = "month") {
  sheet_name <- sanitize_sheet_name(sheet_name)
  openxlsx::addWorksheet(wb, sheet_name)
  current_row <- 1
  ncol <- EXPORT_TABLE_NCOL

  write_table_block <- function(monthly_sub, rolling_sub, label = NULL, style = NULL) {
    if (!is.null(label)) {
      openxlsx::mergeCells(wb, sheet_name, cols = 1:ncol, rows = current_row)
      openxlsx::writeData(wb, sheet_name, label, startCol = 1, startRow = current_row)
      openxlsx::addStyle(wb, sheet_name, style, rows = current_row, cols = 1:ncol, gridExpand = TRUE)
      current_row <<- current_row + 1
    }
    table <- build_export_table(monthly_sub, rolling_sub, month_col)
    openxlsx::writeData(wb, sheet_name, table, startCol = 1, startRow = current_row,
                         headerStyle = HEADER_STYLE)
    current_row <<- current_row + nrow(table) + 2
  }

  write_group_blocks <- function(monthly_sub, rolling_sub) {
    if (is.null(group_var)) {
      write_table_block(monthly_sub, rolling_sub)
    } else {
      levels_present <- sort(unique(stats::na.omit(monthly_sub[[group_var]])))
      for (lvl in levels_present) {
        write_table_block(
          dplyr::filter(monthly_sub, .data[[group_var]] == lvl),
          dplyr::filter(rolling_sub, .data[[group_var]] == lvl),
          label = lvl, style = GROUP_STYLE
        )
      }
    }
  }

  if (is.null(outcome_var_col)) {
    write_group_blocks(monthly_all, rolling_all)
  } else {
    for (outcome_label in unique(monthly_all[[outcome_var_col]])) {
      openxlsx::mergeCells(wb, sheet_name, cols = 1:ncol, rows = current_row)
      openxlsx::writeData(wb, sheet_name, outcome_label, startCol = 1, startRow = current_row)
      openxlsx::addStyle(wb, sheet_name, OUTCOME_STYLE, rows = current_row, cols = 1:ncol, gridExpand = TRUE)
      current_row <- current_row + 1
      write_group_blocks(
        dplyr::filter(monthly_all, .data[[outcome_var_col]] == outcome_label),
        dplyr::filter(rolling_all, .data[[outcome_var_col]] == outcome_label)
      )
    }
  }

  openxlsx::setColWidths(wb, sheet_name, cols = 1:ncol, widths = c(10, 8, 12, 10, 16, 30, 50, 50))
}



# 5. Top-level export functions, one per domain


# Every grouping tab a domain workbook gets: Overall, then each
# demographic grouping, then Source -- same groupings the dashboard's
# Trends tab offers (ALL_GROUPING_LABELS). LA County is handled
# separately below, as its own geography tab, not through this list --
# it's a geography scope now, not a grouping.
GROUPING_TABS <- c(list(Overall = NULL), GROUPING_VARS, list(Source = "Source"))

# Finds every level's already-computed trend for one (domain, figure,
# grouping) cut inside `cuts` (R/06_reporting.R's
# build_all_trend_and_monthly_tables() output) and reshapes it into
# exactly what calc_trend(df, outcome_var, group_var = group_col_name)
# would have returned directly. No calculation happens here, only
# relabeling and stacking rows that are already computed.
# @param group_col_name NULL for Overall (exactly one level, no group
#   column needed); the real column name otherwise.
combine_cut_levels <- function(cuts, domain_key, figure_title, grouping_label, group_col_name) {
  matching <- Filter(function(cut) {
    !is.null(cut) && identical(cut$domain_key, domain_key) &&
      identical(cut$fig$title, figure_title) && identical(cut$grouping_label, grouping_label)
  }, cuts)

  if (length(matching) == 0) {
    stop("No cuts found for domain '", domain_key, "', figure '", figure_title,
         "', grouping '", grouping_label, "'. figure_title has to match a DOMAIN_FIGURES title exactly.")
  }

  if (is.null(group_col_name)) return(matching[[1]]$trend)

  list(
    monthly_all = dplyr::bind_rows(lapply(matching, function(cut) {
      dplyr::mutate(cut$trend$monthly_all, "{group_col_name}" := cut$level_name)
    })),
    rolling_all = dplyr::bind_rows(lapply(matching, function(cut) {
      dplyr::mutate(cut$trend$rolling_all, "{group_col_name}" := cut$level_name)
    }))
  )
}

# Keeps only the outcome labels this workbook wants, out of everything
# computed for the figure (e.g. the Material Hardship figure covers
# "Any" plus all 5 types in one cut; the two Material Hardship
# workbooks each keep a different subset of that same cut).
filter_trend_outcomes <- function(trend, outcome_var_col, outcome_labels) {
  if (is.null(outcome_var_col) || is.null(outcome_labels)) return(trend)
  list(
    monthly_all = dplyr::filter(trend$monthly_all, .data[[outcome_var_col]] %in% outcome_labels),
    rolling_all = dplyr::filter(trend$rolling_all, .data[[outcome_var_col]] %in% outcome_labels)
  )
}

# Writes every grouping tab for one (domain, figure) cut into an
# already-open workbook, reusing cuts rather than recomputing anything.
# When outcome_labels narrows the cut down to exactly one outcome (e.g.
# Material Hardship's shared cut, kept to just "Any"), no outcome header
# block is drawn, same as a workbook built from a genuinely
# single-outcome figure -- a single remaining label isn't worth a
# section heading of its own.
#
# One geography's worth of cuts in, one geography's worth of tabs out
# -- Overall, then every demographic grouping, then Source, ALL
# resolved within whichever geography `cuts` came from. This gets
# called once per geography (see export_domain_figure() below), not
# once with a combined CA+LA cuts list: each geography needs its own
# full grouping breakdown (not just a single pinned "LA County" line),
# and several grouping tab names (e.g. "Dual/Non-Dual Parent
# Household") already sit right at Excel's 31-character sheet name
# limit, leaving no room for a "CA - "/"LA - " prefix on the same
# sheet -- hence two separate files instead of one workbook with twice
# the tabs.
#' @param cuts One geography's cuts (build_all_trend_and_monthly_tables_by_geo()'s
#'   `cuts` for CA Statewide, or `cuts_la` for LA County).
.export_figure_across_tabs <- function(wb, cuts, domain_key, figure_title,
                                        outcome_var_col = NULL, outcome_labels = NULL) {
  render_outcome_col <- if (!is.null(outcome_var_col) && (is.null(outcome_labels) || length(outcome_labels) != 1)) {
    outcome_var_col
  } else {
    NULL
  }

  for (tab_label in names(GROUPING_TABS)) {
    group_col <- GROUPING_TABS[[tab_label]]
    trend <- combine_cut_levels(cuts, domain_key, figure_title, tab_label, group_col) %>%
      filter_trend_outcomes(outcome_var_col, outcome_labels)
    write_export_sheet(wb, tab_label, trend$monthly_all, trend$rolling_all,
                        group_var = group_col, outcome_var_col = render_outcome_col)
  }
}

# Export one domain figure (e.g. "Material Hardship by Source", filtered
# to just the "Any" outcome) to one standalone workbook, for ONE
# geography's cuts.
export_domain_figure <- function(cuts, domain_key, figure_title, output_path,
                                  outcome_var_col = NULL, outcome_labels = NULL) {
  wb <- openxlsx::createWorkbook()
  .export_figure_across_tabs(wb, cuts, domain_key, figure_title,
                              outcome_var_col = outcome_var_col, outcome_labels = outcome_labels)
  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
}


# Export every domain to its own workbook in output_dir, from cuts
# R/06_reporting.R already computed for the dashboard. Each workbook
# writes its own file, fully independent of the others, so they run in
# parallel. If one fails, the rest still finish; the failure is
# surfaced clearly afterward instead of silently stopping everything
# partway through.
export_all_domains <- function(cuts, cuts_la, output_dir = EXCEL_OUTPUT_DIR) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  path <- function(name) file.path(output_dir, name)

  # Which (domain, figure) this workbook pulls from, and which outcome
  # labels inside that figure's cut it keeps. NULL outcome_labels means
  # keep everything the figure already has.
  jobs_spec <- list(
    "1a_Material_Hardship_Any"     = list(domain_key = "material_hardship", figure_title = "Material Hardship by Source", outcome_var_col = "outcome", outcome_labels = "Any"),
    "1b_Material_Hardship_By_Type" = list(domain_key = "material_hardship", figure_title = "Material Hardship by Source", outcome_var_col = "outcome", outcome_labels = names(MATERIAL_HARDSHIP_TYPES)),
    "2a_Parent_Distress_Composite" = list(domain_key = "parent_distress", figure_title = "Composite: Any Measure (Both Thresholds)", outcome_var_col = "outcome", outcome_labels = NULL),
    "2b_Parent_Distress_Lower"     = list(domain_key = "parent_distress", figure_title = paste0("Four Measures — ", PARENT_DISTRESS_THRESHOLD_LABELS[["Lower"]]), outcome_var_col = "outcome", outcome_labels = NULL),
    "2c_Parent_Distress_Higher"    = list(domain_key = "parent_distress", figure_title = paste0("Four Measures — ", PARENT_DISTRESS_THRESHOLD_LABELS[["Higher"]]), outcome_var_col = "outcome", outcome_labels = NULL),
    "3a_Child_Behavior_Composite"  = list(domain_key = "child_behavior", figure_title = "Composite: Any Measure (Both Thresholds)", outcome_var_col = "outcome", outcome_labels = NULL),
    "3b_Child_Behavior_Lower"      = list(domain_key = "child_behavior", figure_title = paste0("Fear/Fussy — ", CHILD_BEHAVIOR_THRESHOLD_LABELS[["Lower"]]), outcome_var_col = "outcome", outcome_labels = NULL),
    "3c_Child_Behavior_Higher"     = list(domain_key = "child_behavior", figure_title = paste0("Fear/Fussy — ", CHILD_BEHAVIOR_THRESHOLD_LABELS[["Higher"]]), outcome_var_col = "outcome", outcome_labels = NULL),
    "4_Child_Care_Types"           = list(domain_key = "child_care", figure_title = "Child Care by Type", outcome_var_col = "outcome", outcome_labels = NULL),
    "5_Healthcare"                 = list(domain_key = "healthcare", figure_title = "Missed Well-Baby/Well-Child Check-Ups", outcome_var_col = NULL, outcome_labels = NULL),
    # "Overall" inside this severity breakdown is the exact same number
    # as the old standalone fi_insecure measure (byte-identical case_when
    # logic in R/03_domains.R), so a separate plain workbook would only
    # ever duplicate this one's first block -- one file covers all 3
    # levels.
    "6_Food_Insecurity"            = list(domain_key = "food_insecurity", figure_title = "Severity (3 Thresholds)", outcome_var_col = "outcome", outcome_labels = NULL)
  )

  # Every job_spec runs twice -- once against the CA-wide cuts, once
  # against the LA-county-only cuts -- producing two separate files
  # (e.g. "1a_Material_Hardship_Any_CA.xlsx" and
  # "..._LA.xlsx") rather than one combined workbook. Each file gets
  # the SAME full set of grouping tabs (Overall, Race, Income Level,
  # ..., Source), just resolved within that geography -- not a single
  # "LA County" tab bolted onto the CA workbook.
  geo_variants <- list(CA = cuts, LA = cuts_la)

  tasks <- do.call(c, lapply(names(jobs_spec), function(job_name) {
    stats::setNames(
      lapply(names(geo_variants), function(geo_suffix) list(job_name = job_name, geo_suffix = geo_suffix)),
      paste0(job_name, "_", names(geo_variants))
    )
  }))

  cat("  -- exporting", length(jobs_spec), "workbook(s) x 2 geographies =", length(tasks),
      "file(s) across", PIPELINE_CORES, "core(s)...\n")
  results <- parallel::mclapply(tasks, function(task) {
    t0 <- Sys.time()
    spec <- jobs_spec[[task$job_name]]
    export_domain_figure(geo_variants[[task$geo_suffix]], spec$domain_key, spec$figure_title,
                          path(paste0(task$job_name, "_", task$geo_suffix, ".xlsx")),
                          outcome_var_col = spec$outcome_var_col, outcome_labels = spec$outcome_labels)
    cat(sprintf("     [%s_%s] done, %.1f sec\n", task$job_name, task$geo_suffix,
                as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    NULL
  }, mc.cores = PIPELINE_CORES)
  names(results) <- names(tasks)

  # mclapply() returns a try-error object per failed job rather than
  # halting the other jobs.
  failed <- vapply(results, function(x) inherits(x, "try-error"), logical(1))
  if (any(failed)) {
    stop("export_all_domains() failed on: ", paste(names(tasks)[failed], collapse = ", "),
         "\nFirst error: ", attr(results[failed][[1]], "condition")$message)
  }

  invisible(NULL)
}



# 6. Sample Description export: one workbook, 4 sheets (Overall, By
# Year, By Month, By Survey Number). Each sheet has the same grouping
# breakdown as the dashboard's Sample Description tab, plus an
# FPL_CONT_locf distribution for that same time cut.

# Coarse FPL bins for the histogram block.
FPL_EXPORT_BIN_BREAKS <- c(0, 100, 200, 300, 400, Inf)
FPL_EXPORT_BIN_LABELS <- c("0-100% FPL", "101-200% FPL", "201-300% FPL", "301-400% FPL", "Above 400% FPL")

# pivot_wider() only creates a column for a time value if at least one
# row actually has it. A time period with nobody reporting FPL never
# gets a column at all, so add it back filled with fill_value, so every
# sheet always has one column per time_order entry.
ensure_all_time_columns <- function(wide_df, time_order, fill_value = NA_character_) {
  for (col in setdiff(time_order, names(wide_df))) wide_df[[col]] <- fill_value
  wide_df
}

# EXACT unique-participant counts (distinct CALID, never a response/row
# count) by (grouping, level, time), parameterized on the time column
# so one function builds the Year, Month, and Survey Number cuts. Each
# sheet's time_col partitions df into non-overlapping buckets (a
# response belongs to exactly one year, one month, or one survey
# number), so n_distinct(CALID) per (time_col, grouping) cell is always
# exact here -- no cross-bucket summing/approximation needed, unlike
# the dashboard's own Sample Description tab, which has to support an
# arbitrary custom date range on top of its 3 fixed exact units (see
# R/06_reporting.R's sample_description_breakdown()).
sample_description_counts_by_time <- function(df, time_col) {
  count_distinct <- function(data, ...) {
    data %>%
      dplyr::group_by(...) %>%
      dplyr::summarise(n = dplyr::n_distinct(CALID), .groups = "drop")
  }

  overall <- count_distinct(df, .data[[time_col]]) %>%
    dplyr::mutate(grouping = "Overall", level = "Overall")

  by_source <- df %>%
    dplyr::filter(!is.na(Source)) %>%
    count_distinct(.data[[time_col]], Source) %>%
    dplyr::rename(level = Source) %>%
    dplyr::mutate(grouping = "Source", level = as.character(level))

  by_grouping <- purrr::map_dfr(names(GROUPING_VARS), function(label) {
    col <- GROUPING_VARS[[label]]
    df %>%
      dplyr::filter(!is.na(.data[[col]])) %>%
      count_distinct(.data[[time_col]], .data[[col]]) %>%
      dplyr::rename(level = !!col) %>%
      dplyr::mutate(grouping = label, level = as.character(level))
  })

  dplyr::bind_rows(overall, by_source, by_grouping) %>%
    dplyr::rename(time = !!time_col) %>%
    dplyr::mutate(time = as.character(time)) %>%
    dplyr::select(grouping, level, time, n)
}

# Pivot the long counts to one row per (Category, Level), one column
# per time value, "N (%)" formatted as a percent of that category's own
# total within that time period, not the grand total.
# @param time_order Column order, left to right, already character and
#   in the order the columns should appear.
pivot_sample_description_wide <- function(counts_long, time_order,
                                           category_order = DEFAULT_SAMPLE_DESCRIPTION_CATEGORY_ORDER) {
  wide <- counts_long %>%
    dplyr::group_by(grouping, time) %>%
    dplyr::mutate(pct = 100 * n / sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      Category = grouping, Level = level, time = factor(time, levels = time_order),
      `N (%)` = sprintf("%s (%.1f%%)", format(n, big.mark = ",", trim = TRUE), pct)
    ) %>%
    tidyr::pivot_wider(names_from = time, values_from = `N (%)`)

  present_order <- category_order[category_order %in% unique(wide$Category)]

  # Within each Category, Level follows that grouping's own display
  # order (lowest-to-highest FPL for Income Level, etc.) via the same
  # order_grouping_levels() every other Level column in this pipeline
  # respects -- a plain alphabetical arrange() would put Income Level's
  # bands out of numeric order in this workbook too.
  wide %>%
    dplyr::mutate(Category = factor(Category, levels = present_order)) %>%
    dplyr::group_by(Category) %>%
    dplyr::group_modify(~ {
      .x$Level <- factor(.x$Level, levels = order_grouping_levels(unique(.x$Level), as.character(.y$Category)))
      dplyr::arrange(.x, Level)
    }) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Category = as.character(Category), Level = as.character(Level)) %>%
    dplyr::select(Category, Level, dplyr::all_of(time_order))
}

# FPL_CONT_locf summary statistics (N, Mean, Median, SD, Min, P25, P75,
# Max), one column per time value. N here is deliberately a RESPONSE
# count (dplyr::n(), not n_distinct(CALID)) -- a mean/median/SD of a
# per-response value like FPL_CONT_locf needs every response
# contributing to it, not one per participant, unlike the grouping
# breakdown block above it in the same sheet, which is a participant
# headcount. Labeled "N (responses)" rather than bare "N" so the two
# blocks' different N's are never confused for the same thing.
fpl_summary_stats_by_time <- function(df, time_col, time_order) {
  stats_wide <- df %>%
    dplyr::filter(!is.na(FPL_CONT_locf)) %>%
    dplyr::mutate(time = as.character(.data[[time_col]])) %>%
    dplyr::group_by(time) %>%
    dplyr::summarise(
      `N (responses)` = dplyr::n(),
      Mean   = round(mean(FPL_CONT_locf), 1),
      Median = round(stats::median(FPL_CONT_locf), 1),
      SD     = round(stats::sd(FPL_CONT_locf), 1),
      P25    = round(stats::quantile(FPL_CONT_locf, 0.25), 1),
      P75    = round(stats::quantile(FPL_CONT_locf, 0.75), 1),
      Min    = round(min(FPL_CONT_locf), 1),
      Max    = round(max(FPL_CONT_locf), 1),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(-time, names_to = "Statistic", values_to = "value") %>%
    dplyr::mutate(time = factor(time, levels = time_order)) %>%
    tidyr::pivot_wider(names_from = time, values_from = value)

  stat_order <- c("N (responses)", "Mean", "Median", "SD", "Min", "P25", "P75", "Max")
  stats_wide %>%
    ensure_all_time_columns(time_order, fill_value = NA_real_) %>%
    dplyr::mutate(Statistic = factor(Statistic, levels = stat_order)) %>%
    dplyr::arrange(Statistic) %>%
    dplyr::mutate(Statistic = as.character(Statistic)) %>%
    dplyr::select(Statistic, dplyr::all_of(time_order))
}

# FPL_CONT_locf histogram, one row per bin, one column per time value,
# same "N (%)" format as the grouping tables.
fpl_histogram_by_time <- function(df, time_col, time_order) {
  binned <- df %>%
    dplyr::filter(!is.na(FPL_CONT_locf)) %>%
    dplyr::mutate(
      time = as.character(.data[[time_col]]),
      fpl_bin = cut(FPL_CONT_locf, breaks = FPL_EXPORT_BIN_BREAKS, labels = FPL_EXPORT_BIN_LABELS,
                    include.lowest = TRUE, right = TRUE)
    )

  wide <- binned %>%
    dplyr::count(time, fpl_bin, name = "n") %>%
    dplyr::group_by(time) %>%
    dplyr::mutate(pct = 100 * n / sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      `FPL Bin` = factor(fpl_bin, levels = FPL_EXPORT_BIN_LABELS), time = factor(time, levels = time_order),
      `N (%)` = sprintf("%s (%.1f%%)", format(n, big.mark = ",", trim = TRUE), pct)
    ) %>%
    tidyr::pivot_wider(names_from = time, values_from = `N (%)`)

  wide %>%
    ensure_all_time_columns(time_order, fill_value = NA_character_) %>%
    dplyr::arrange(`FPL Bin`) %>%
    dplyr::mutate(`FPL Bin` = as.character(`FPL Bin`)) %>%
    dplyr::select(`FPL Bin`, dplyr::all_of(time_order))
}

# Write one Sample Description sheet: the grouping breakdown block,
# then the FPL summary-stats block, then the FPL histogram block,
# stacked vertically.
write_sample_description_sheet <- function(wb, sheet_name, df, time_col, time_order) {
  openxlsx::addWorksheet(wb, sheet_name)
  current_row <- 1
  ncol <- 1 + length(time_order)

  write_block <- function(label, table) {
    openxlsx::mergeCells(wb, sheet_name, cols = 1:ncol, rows = current_row)
    openxlsx::writeData(wb, sheet_name, label, startCol = 1, startRow = current_row)
    openxlsx::addStyle(wb, sheet_name, OUTCOME_STYLE, rows = current_row, cols = 1:ncol, gridExpand = TRUE)
    current_row <<- current_row + 1
    openxlsx::writeData(wb, sheet_name, table, startCol = 1, startRow = current_row, headerStyle = HEADER_STYLE)
    current_row <<- current_row + nrow(table) + 2
  }

  counts_long <- sample_description_counts_by_time(df, time_col)
  write_block("Respondent Counts by Category", pivot_sample_description_wide(counts_long, time_order))
  write_block("FPL_CONT (% of Poverty Line) -- Summary Statistics (responses, not participants)", fpl_summary_stats_by_time(df, time_col, time_order))
  write_block("FPL_CONT (% of Poverty Line) -- Distribution (% of responses, not participants)", fpl_histogram_by_time(df, time_col, time_order))
}

# Writes all 4 of one geography's sheets (Overall, By Year, By Month,
# By Survey Number), each time cut's own column set recomputed from
# THAT geography's own df -- so e.g. a survey wave that never reached
# LA County doesn't show up as an (empty) column on the LA County
# sheets.
# @param sheet_prefix Prepended to each of the 4 sheet names (e.g.
#   "CA - " / "LA - "); openxlsx sheet names cap at 31 characters, so
#   this has to stay short.
.write_sample_description_sheets_for_geo <- function(wb, master_analysis, sheet_prefix) {
  df <- master_analysis %>% dplyr::mutate(.overall_time = "All Years")

  write_sample_description_sheet(wb, paste0(sheet_prefix, "Overall"), df, ".overall_time", "All Years")
  write_sample_description_sheet(wb, paste0(sheet_prefix, "By Year"), df, "survey_year",
                                  sort(unique(as.character(df$survey_year))))
  write_sample_description_sheet(wb, paste0(sheet_prefix, "By Month"), df, "month",
                                  sort(unique(as.character(df$month))))
  write_sample_description_sheet(wb, paste0(sheet_prefix, "By Survey Number"), df, "SurveyNumber",
                                  as.character(sort(unique(df$SurveyNumber))))
}

# The full Sample Description workbook: Overall (no time split), By
# Year, By Month, By Survey Number -- each duplicated as a CA
# Statewide set of sheets and an LA County (la_county == 1) set, same
# split as the dashboard's Sample Description tab's two geography tabs.
# @param master_analysis master_core after R/01_prep.R (needs Source,
#   every GROUPING_VARS column, survey_year, month, SurveyNumber,
#   la_county, and FPL_CONT_locf).
# @param path Output .xlsx path.
build_sample_description_workbook <- function(master_analysis, path) {
  wb <- openxlsx::createWorkbook()
  .write_sample_description_sheets_for_geo(wb, master_analysis, "CA - ")
  .write_sample_description_sheets_for_geo(wb, dplyr::filter(master_analysis, la_county == 1), "LA - ")

  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(wb)
}
