# 04_summarize.R
# Turns respondent-level rows into monthly and 3-month-rolling summary
# tables, with both a weighted and an unweighted percentage.
#
# calc_prop_summary() makes one row per month (or month x group) with
# the weighted/unweighted proportion, margin of error, n, and a
# suppressed/suppress_reason pair. calc_rolling_avg() takes that output
# and adds the 3-month trailing rolling version as new columns.
#
# compute_trend_averages() / format_trend_subtitle() give two summary
# numbers: an n-weighted average (bigger months count more) and a plain
# unweighted average (every month counts equally).

library(dplyr)
library(tidyr)
library(purrr)

# 1. Precision helpers (MOE, effective sample size)

# Margin of error for a weighted proportion, using the design-effect
# based effective sample size. A month that needed heavy reweighting to
# hit the ACS targets gets a wider MOE here even if its raw n looks fine.
weighted_moe_prop <- function(binary_values, weights, alpha = 0.05) {
  ok <- !is.na(binary_values) & !is.na(weights)
  if (!any(ok)) return(NA_real_)
  x <- binary_values[ok]
  w <- weights[ok]

  p     <- stats::weighted.mean(x, w)
  n_eff <- effective_n(w)
  se    <- sqrt(p * (1 - p) / n_eff)
  stats::qnorm(1 - alpha / 2) * se
}

# Standard margin of error for an unweighted proportion.
unweighted_moe_prop <- function(prop, n, alpha = 0.05) {
  if (is.na(prop) || is.na(n) || n == 0) return(NA_real_)
  stats::qnorm(1 - alpha / 2) * sqrt(prop * (1 - prop) / n)
}

# Effective sample size from raking weights. Equals the raw n when every
# weight is equal, shrinks as weights become more unequal.
effective_n <- function(weights) {
  w <- weights[!is.na(weights)]
  if (length(w) == 0) return(NA_real_)
  sum(w)^2 / sum(w^2)
}

# Weighted mean that returns NA if nothing's valid to average, dropping
# a row if either the outcome or the weight is missing for it.
safe_weighted_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  stats::weighted.mean(x[ok], w[ok])
}

# 2. Monthly summary (weighted and unweighted)

# One row per month (or month x group) with the weighted and unweighted
# percentage for one 0/1 outcome column, plus everything needed to plot
# a confidence band and suppress unreliable months.
#
# @param df Respondent-level data with month, outcome_var, weight_var.
# @param outcome_var Name of the 0/1 outcome column.
# @param group_var Optional grouping column. NULL means overall.
# @param weight_var Name of the raking weight column.
# @param min_n Minimum respondents for a row to count as shown rather
#   than suppressed.
# @param id_col Respondent ID column, used to add n_participants
#   (distinct respondents that month) alongside n (response count). They
#   only differ if someone answers more than once in a single month.
# @return One row per month (x group) that exists in the data at all. A
#   month is never dropped here, even at zero or very low n.
#   suppressed is TRUE if the row shouldn't be shown. suppress_reason
#   distinguishes zero valid responses from below-minimum from included.
calc_prop_summary <- function(df, outcome_var, group_var = NULL,
                               weight_var = RAKE_WEIGHT_COL,
                               month_var = "month",
                               id_col = "CALID",
                               min_n = if (is.null(group_var)) N_MIN_OVERALL else N_MIN_SUBGROUP) {
  group_cols <- c(month_var, group_var)

  # Every (month [x group]) combination
  all_combos <- df %>%
    dplyr::filter(!is.na(.data[[month_var]])) %>%
    dplyr::distinct(dplyr::across(dplyr::all_of(group_cols)))

  answered_stats <- df %>%
    dplyr::filter(!is.na(.data[[outcome_var]])) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
    dplyr::summarise(
      n               = dplyr::n(),
      n_participants  = dplyr::n_distinct(.data[[id_col]]),
      n_eff           = effective_n(.data[[weight_var]]),
      unweighted_prop = mean(.data[[outcome_var]]),
      unweighted_moe  = unweighted_moe_prop(mean(.data[[outcome_var]]), dplyr::n()),
      weighted_prop   = safe_weighted_mean(.data[[outcome_var]], .data[[weight_var]]),
      weighted_moe    = weighted_moe_prop(.data[[outcome_var]], .data[[weight_var]]),
      .groups = "drop"
    )

  all_combos %>%
    dplyr::left_join(answered_stats, by = group_cols) %>%
    dplyr::mutate(
      n = tidyr::replace_na(n, 0L),
      n_participants = tidyr::replace_na(n_participants, 0L),
      unweighted_ci_low  = pmax(0, unweighted_prop - unweighted_moe),
      unweighted_ci_high = pmin(1, unweighted_prop + unweighted_moe),
      weighted_ci_low    = pmax(0, weighted_prop - weighted_moe),
      weighted_ci_high   = pmin(1, weighted_prop + weighted_moe),
      date = as.Date(paste0(gsub("/", "-", .data[[month_var]]), "-01")),
      suppressed = n < min_n,
      suppress_reason = dplyr::case_when(
        n == 0    ~ "No valid responses this month (item may not have been fielded, or nobody answered it)",
        n < min_n ~ sprintf("Suppressed: n = %d is below the required minimum of %d", n, min_n),
        TRUE      ~ "Included"
      )
    ) %>%
    dplyr::arrange(date)
}

# Runs calc_prop_summary() over several outcome columns at once, e.g.
# the 4 Parent Distress measures, stacking the results long with an
# "outcome" label column.
# @param outcome_vars A named character vector, names are display
#   labels, values are column names.
calc_multi_prop_summary <- function(df, outcome_vars, group_var = NULL,
                                     weight_var = RAKE_WEIGHT_COL,
                                     month_var = "month",
                                     min_n = if (is.null(group_var)) N_MIN_OVERALL else N_MIN_SUBGROUP) {
  purrr::imap_dfr(outcome_vars, function(col, label) {
    calc_prop_summary(df, outcome_var = col, group_var = group_var,
                       weight_var = weight_var, month_var = month_var,
                       min_n = min_n) %>%
      dplyr::mutate(outcome = label)
  })
}

# 3. Rolling average (3-month trailing)
# Adds the 3-month trailing rolling average (month T uses T, T-1, T-2)
# as new columns. This is a post-processing smoothing step: the rolling
# weighted % is a mean of the last window months' already-computed
# weighted_prop values, not a fresh weighted.mean() over pooled rows.

# @param monthly_data Output of calc_prop_summary() (or
#   calc_multi_prop_summary() with group_var = "outcome", so each
#   outcome rolls independently). Pass the unfiltered table (every
#   month, suppressed or not); a month suppressed on its own can still
#   contribute toward a reliable 3-month total.
# @param group_var Optional column to roll independently within (a
#   subgroup, or "outcome" for multi-measure data). NULL rolls the whole
#   table as one series.
# @param window Months in the trailing window.
# @param min_months_in_window Minimum months required before a row gets
#   a rolling value at all.
# @param min_n Minimum cumulative n across the window for a row to
#   count as shown rather than suppressed. Deliberately higher than the
#   monthly threshold, since this certifies 3 pooled months, not 1.
# @param raw_df Respondent-level rows, scoped the same way monthly_data
#   was built. When given, the rolling margin of error is computed by
#   resampling whole CALIDs (cluster_bootstrap_rolling_se()), which
#   correctly accounts for the same person showing up in more than one
#   of a window's 3 months. When NULL, falls back to a plain average of
#   the window's monthly MOEs, so this function still works without
#   underlying rows available.
# @param outcome_var The raw outcome column name, when every series
#   rolls the same outcome. Ignored if outcome_var_lookup is given.
# @param outcome_var_lookup Named vector, label to raw column name, for
#   when monthly_data has an "outcome" column covering several outcome
#   variables in one call.
# @param demographic_group_var The real raw-data column name to filter
#   raw_df by subgroup level. Not necessarily the same as this
#   function's own group_var, which can just be a series key. NULL
#   means no subgroup filtering.
# @param weight_var,id_col,date_col,n_boot Passed through to
#   cluster_bootstrap_rolling_se().
# @return monthly_data with rolling_* columns added, including
#   window_months (the exact 3 calendar months this row's window
#   covers, always the fixed trailing span), rolling_suppressed, and
#   rolling_suppress_reason.
calc_rolling_avg <- function(monthly_data, group_var = NULL,
                              window = ROLLING_WINDOW_MONTHS,
                              min_months_in_window = ROLLING_MIN_MONTHS_IN_WINDOW,
                              min_n = ROLLING_MIN_OVERALL,
                              raw_df = NULL, outcome_var = NULL, outcome_var_lookup = NULL,
                              demographic_group_var = NULL, weight_var = RAKE_WEIGHT_COL,
                              id_col = "CALID", date_col = "StartDate",
                              n_boot = ROLLING_BOOTSTRAP_N) {

  compute_rolling_one_series <- function(one_series) {
    one_series <- one_series %>% dplyr::arrange(date)
    n_rows <- nrow(one_series)

    # This series' own raw outcome column and its matching subgroup
    # slice of raw_df, resolved once per series rather than per row,
    # since neither changes across the series' own rows.
    series_outcome_col <- NULL
    series_raw_df <- NULL
    if (!is.null(raw_df)) {
      series_outcome_col <- if (!is.null(outcome_var)) {
        outcome_var
      } else if (!is.null(outcome_var_lookup) && "outcome" %in% names(one_series)) {
        outcome_var_lookup[[one_series$outcome[1]]]
      } else {
        NULL
      }
      if (!is.null(series_outcome_col)) {
        series_raw_df <- raw_df
        if (!is.null(demographic_group_var) && demographic_group_var %in% names(one_series) &&
            demographic_group_var %in% names(raw_df)) {
          lvl <- one_series[[demographic_group_var]][1]
          series_raw_df <- series_raw_df[series_raw_df[[demographic_group_var]] == lvl, ]
        }
      }
    }
    use_bootstrap <- !is.null(series_raw_df) && !is.null(series_outcome_col)

    window_months           <- rep(NA_character_, n_rows)
    n_months_in_window      <- rep(0L, n_rows)
    rolling_n               <- rep(NA_real_, n_rows)
    rolling_weighted_prop   <- rep(NA_real_, n_rows)
    rolling_weighted_moe    <- rep(NA_real_, n_rows)
    rolling_unweighted_prop <- rep(NA_real_, n_rows)
    rolling_unweighted_moe  <- rep(NA_real_, n_rows)

    for (i in seq_len(n_rows)) {
      # The fixed trailing span: this row's calendar month and the
      # window - 1 months immediately before it. Always exactly these
      # dates, whether or not a row exists for all of them.
      window_dates <- seq(one_series$date[i], by = "-1 month", length.out = window)
      idx <- match(window_dates, one_series$date)  # NA for any date with no row at all (a true gap)

      window_months[i]      <- paste(format(sort(window_dates), "%Y/%m"), collapse = ", ")
      n_months_in_window[i] <- sum(!is.na(idx) & !is.na(one_series$weighted_prop[idx]))

      if (n_months_in_window[i] < min_months_in_window) next

      rolling_n[i]               <- sum(one_series$n[idx], na.rm = TRUE)
      rolling_weighted_prop[i]   <- mean(one_series$weighted_prop[idx], na.rm = TRUE)
      rolling_unweighted_prop[i] <- mean(one_series$unweighted_prop[idx], na.rm = TRUE)

      if (use_bootstrap) {
        boot <- cluster_bootstrap_rolling_se(
          series_raw_df, series_outcome_col, weight_var = weight_var,
          id_col = id_col, date_col = date_col,
          window_dates = window_dates, n_boot = n_boot
        )
        rolling_weighted_moe[i]   <- boot$boot_moe
        rolling_unweighted_moe[i] <- boot$unweighted_boot_moe
      } else {
        # No raw_df given, so fall back to a plain average of the
        # window's own per-month MOEs.
        rolling_weighted_moe[i]   <- mean(one_series$weighted_moe[idx], na.rm = TRUE)
        rolling_unweighted_moe[i] <- mean(one_series$unweighted_moe[idx], na.rm = TRUE)
      }
    }

    one_series %>%
      dplyr::mutate(
        n_months_in_window     = n_months_in_window,
        window_months           = window_months,
        rolling_n               = rolling_n,
        rolling_weighted_prop   = rolling_weighted_prop,
        rolling_weighted_moe    = rolling_weighted_moe,
        rolling_unweighted_prop = rolling_unweighted_prop,
        rolling_unweighted_moe  = rolling_unweighted_moe,
        rolling_weighted_ci_low    = pmax(0, rolling_weighted_prop - rolling_weighted_moe),
        rolling_weighted_ci_high   = pmin(1, rolling_weighted_prop + rolling_weighted_moe),
        rolling_unweighted_ci_low  = pmax(0, rolling_unweighted_prop - rolling_unweighted_moe),
        rolling_unweighted_ci_high = pmin(1, rolling_unweighted_prop + rolling_unweighted_moe),
        rolling_suppressed = is.na(rolling_n) | rolling_n < min_n,
        rolling_suppress_reason = dplyr::case_when(
          is.na(rolling_n) ~ "No rolling average yet (no real month in the trailing 3-month window)",
          rolling_n < min_n ~ sprintf(
            "Suppressed: 3-month cumulative n = %d is below the required minimum of %d", rolling_n, min_n
          ),
          TRUE ~ "Included"
        )
      )
  }

  if (!is.null(group_var) && group_var %in% names(monthly_data)) {
    monthly_data %>%
      dplyr::group_by(.data[[group_var]]) %>%
      dplyr::group_modify(~ compute_rolling_one_series(.x)) %>%
      dplyr::ungroup()
  } else {
    compute_rolling_one_series(monthly_data)
  }
}

# 3b. CALID-clustered bootstrap for the 3-month rolling average
#
# The same CALID can appear in more than one of a rolling window's 3
# months, anyone retained in the panel across waves. Those repeated
# responses aren't independent of each other, so a plain average of
# each month's own margin of error treats them as if they were, which
# can misstate the real uncertainty of the rolling estimate. This
# resamples whole CALIDs instead of rows, so a person who appears in 2
# of the 3 months is resampled, or not, as a single unit, keeping
# whatever correlation their repeated answers carry.
#
# rolling_prop_from_rows() reproduces the same point estimate
# calc_rolling_avg() computes from raw rows (the mean of each month's
# own weighted proportion, not one proportion pooled across all rows at
# once), so the bootstrap's SE is a correct replacement for the old
# margin of error, for the same estimator.

#' @param df Respondent-level rows, not yet filtered to the window,
#'   this filters internally so you can pass data already filtered to
#'   whatever single grouping level you care about.
#' @param window_dates The exact 3 calendar month-start Dates this
#'   window covers (e.g. from calc_rolling_avg()'s own
#'   `window_dates <- seq(date[i], by = "-1 month", length.out = window)`).
#' @param weighted TRUE reproduces rolling_weighted_prop (mean of each
#'   month's weighted proportion). FALSE reproduces
#'   rolling_unweighted_prop (mean of each month's plain unweighted
#'   proportion); weight_var is ignored in that case.
#' @return The rolling prop value computed directly from rows, or NA if
#'   no month in the window has any valid (CALID, outcome[, weight]) row.
rolling_prop_from_rows <- function(df, outcome_var, weight_var, date_col, window_dates, weighted = TRUE) {
  month_date <- floor_to_month_date(df[[date_col]])
  monthly_props <- vapply(window_dates, function(d) {
    if (weighted) {
      in_month <- month_date == d & !is.na(df[[outcome_var]]) & !is.na(df[[weight_var]])
      if (!any(in_month, na.rm = TRUE)) return(NA_real_)
      safe_weighted_mean(df[[outcome_var]][in_month], df[[weight_var]][in_month])
    } else {
      in_month <- month_date == d & !is.na(df[[outcome_var]])
      if (!any(in_month, na.rm = TRUE)) return(NA_real_)
      mean(df[[outcome_var]][in_month])
    }
  }, numeric(1))
  if (all(is.na(monthly_props))) return(NA_real_)
  mean(monthly_props, na.rm = TRUE)
}

# Snaps a date down to the 1st of its own month. Respondent-level
# StartDate is an actual survey date, not already floored the way the
# monthly summary table's date column is.
floor_to_month_date <- function(dates) {
  as.Date(format(as.Date(dates), "%Y-%m-01"))
}

#' CALID-clustered bootstrap SE/MOE for one rolling 3-month window, one
#' outcome, already filtered to one grouping level.
#'
#' @param df Respondent-level rows for this one grouping level, not yet
#'   filtered to the window or to non-missing outcome rows, that
#'   happens internally.
#' @param outcome_var,weight_var,id_col,date_col Column names.
#' @param window_dates The exact 3 calendar month-start dates this
#'   window covers.
#' @param n_boot Number of bootstrap replicates. Higher gives a more
#'   stable SE at a roughly linear cost in runtime.
#' @param alpha For the normal-approximation MOE.
#' @return A list with both the weighted and unweighted bootstrap
#'   results for this window, computed from the same resampled
#'   replicates each iteration: point_estimate, boot_se, boot_moe,
#'   boot_ci_low, boot_ci_high, naive_moe, and the unweighted_ versions
#'   of each, plus n_boot and n_clusters. naive_moe is the plain,
#'   clustering-blind margin of error for the same data, included so
#'   boot_moe can be compared against it directly. The boot_* fields are
#'   NA if fewer than 2 CALIDs are in the window.
cluster_bootstrap_rolling_se <- function(df, outcome_var, weight_var = RAKE_WEIGHT_COL,
                                          id_col = "CALID", date_col = "StartDate",
                                          window_dates, n_boot = ROLLING_BOOTSTRAP_N, alpha = 0.05) {
  # Everything below works on plain vectors, not a data frame, since
  # this runs once per month/group/outcome row across the pipeline,
  # each time looping n_boot replicates. month_idx (an integer index
  # into window_dates) replaces a date comparison per replicate with a
  # plain integer comparison, computed once up front.
  month_date <- floor_to_month_date(df[[date_col]])
  keep <- month_date %in% window_dates & !is.na(df[[outcome_var]])
  outcome     <- df[[outcome_var]][keep]
  real_weight <- df[[weight_var]][keep]
  calid_chr   <- as.character(df[[id_col]])[keep]
  month_idx   <- match(month_date[keep], window_dates)
  n_window    <- length(window_dates)

  # The point estimate is called once per window, not per replicate, so
  # it goes through rolling_prop_from_rows() directly, keeping exactly
  # one implementation of this estimator.
  point_estimate            <- rolling_prop_from_rows(df, outcome_var, weight_var, date_col, window_dates, weighted = TRUE)
  unweighted_point_estimate <- rolling_prop_from_rows(df, outcome_var, weight_var, date_col, window_dates, weighted = FALSE)

  # rolling_stat() below is a fast, vector-based version of the same
  # estimator, used only inside the replicate loop further down, where
  # rolling_prop_from_rows()'s per-call date parsing made the bootstrap
  # too slow at real pipeline scale. w = NULL means an unweighted calc.
  # Otherwise w is whatever weight the caller supplies: a replicate
  # weight (how many times a row's CALID was drawn) for a resample,
  # including the unweighted resample, where w = rep_weight alone is
  # what makes a CALID drawn twice count twice. o/idx/w must always be
  # the same length, aligned by position, so a resampled row's outcome
  # lines up with its own month and weight.
  rolling_stat <- function(o, idx, w) {
    monthly <- vapply(seq_len(n_window), function(m) {
      in_month <- idx == m & (if (is.null(w)) TRUE else !is.na(w))
      if (!any(in_month)) return(NA_real_)
      if (is.null(w)) mean(o[in_month]) else stats::weighted.mean(o[in_month], w[in_month])
    }, numeric(1))
    if (all(is.na(monthly))) return(NA_real_)
    mean(monthly, na.rm = TRUE)
  }

  monthly_moes <- vapply(seq_len(n_window), function(m) {
    in_month <- month_idx == m & !is.na(real_weight)
    if (!any(in_month)) return(NA_real_)
    weighted_moe_prop(outcome[in_month], real_weight[in_month], alpha = alpha)
  }, numeric(1))
  naive_moe <- mean(monthly_moes, na.rm = TRUE)

  unweighted_monthly_moes <- vapply(seq_len(n_window), function(m) {
    in_month <- month_idx == m
    if (!any(in_month)) return(NA_real_)
    unweighted_moe_prop(mean(outcome[in_month]), sum(in_month), alpha = alpha)
  }, numeric(1))
  unweighted_naive_moe <- mean(unweighted_monthly_moes, na.rm = TRUE)

  clusters <- unique(calid_chr)
  n_clusters <- length(clusters)

  empty_result <- function() {
    list(point_estimate = point_estimate, boot_se = NA_real_, boot_moe = NA_real_,
         boot_ci_low = NA_real_, boot_ci_high = NA_real_, naive_moe = naive_moe,
         unweighted_point_estimate = unweighted_point_estimate,
         unweighted_boot_se = NA_real_, unweighted_boot_moe = NA_real_,
         unweighted_boot_ci_low = NA_real_, unweighted_boot_ci_high = NA_real_,
         unweighted_naive_moe = unweighted_naive_moe,
         n_boot = 0L, n_clusters = n_clusters)
  }
  if (n_clusters < 2) return(empty_result())

  # Pre-split row indices by CALID once, avoiding re-filtering on every
  # replicate.
  rows_by_calid <- split(seq_along(calid_chr), calid_chr)

  weighted_stats   <- vector("numeric", n_boot)
  unweighted_stats <- vector("numeric", n_boot)
  for (b in seq_len(n_boot)) {
    sampled_calids <- sample(clusters, size = n_clusters, replace = TRUE)
    # How many times each CALID was drawn this replicate, applied as a
    # multiplicative weight instead of literally duplicating rows.
    draw_counts <- table(sampled_calids)
    row_idx  <- unlist(rows_by_calid[names(draw_counts)], use.names = FALSE)
    rep_w    <- as.numeric(draw_counts)[match(calid_chr[row_idx], names(draw_counts))]

    weighted_stats[b]   <- rolling_stat(outcome[row_idx], month_idx[row_idx], real_weight[row_idx] * rep_w)
    unweighted_stats[b] <- rolling_stat(outcome[row_idx], month_idx[row_idx], rep_w)
  }
  weighted_stats   <- weighted_stats[!is.na(weighted_stats)]
  unweighted_stats <- unweighted_stats[!is.na(unweighted_stats)]

  boot_se  <- if (length(weighted_stats) >= 2) stats::sd(weighted_stats) else NA_real_
  boot_moe <- if (is.na(boot_se)) NA_real_ else stats::qnorm(1 - alpha / 2) * boot_se

  unweighted_boot_se  <- if (length(unweighted_stats) >= 2) stats::sd(unweighted_stats) else NA_real_
  unweighted_boot_moe <- if (is.na(unweighted_boot_se)) NA_real_ else stats::qnorm(1 - alpha / 2) * unweighted_boot_se

  list(
    point_estimate = point_estimate,
    boot_se        = boot_se,
    boot_moe       = boot_moe,
    boot_ci_low    = if (is.na(boot_moe)) NA_real_ else pmax(0, point_estimate - boot_moe),
    boot_ci_high   = if (is.na(boot_moe)) NA_real_ else pmin(1, point_estimate + boot_moe),
    naive_moe      = naive_moe,
    unweighted_point_estimate = unweighted_point_estimate,
    unweighted_boot_se        = unweighted_boot_se,
    unweighted_boot_moe       = unweighted_boot_moe,
    unweighted_boot_ci_low    = if (is.na(unweighted_boot_moe)) NA_real_ else pmax(0, unweighted_point_estimate - unweighted_boot_moe),
    unweighted_boot_ci_high   = if (is.na(unweighted_boot_moe)) NA_real_ else pmin(1, unweighted_point_estimate + unweighted_boot_moe),
    unweighted_naive_moe      = unweighted_naive_moe,
    n_boot         = length(weighted_stats),
    n_clusters     = n_clusters
  )
}

# 4. Sample-size totals (post-suppression)

# Total number of responses and total number of unique participants
# (distinct CALIDs) behind a trend.
# @param df The full respondent-level data calc_prop_summary() ran on.
# @param outcome_var Name of the 0/1 outcome column.
# @param monthly_data calc_prop_summary()'s output for this same
#   outcome_var/group_var.
# @param group_var Optional grouping column, returns one row per level
#   instead of one overall row.
# @return A tibble, one row overall or one row per group level, each
#   with total_responses and total_participants.
compute_trend_totals <- function(df, outcome_var, monthly_data, group_var = NULL,
                                  id_col = "CALID", month_var = "month") {
  answered <- df %>% dplyr::filter(!is.na(.data[[outcome_var]]))
  surviving <- dplyr::filter(monthly_data, !suppressed)

  count_totals <- function(data) {
    tibble::tibble(
      total_responses    = nrow(data),
      total_participants = dplyr::n_distinct(data[[id_col]])
    )
  }

  if (is.null(group_var)) {
    kept <- answered %>% dplyr::filter(.data[[month_var]] %in% surviving[[month_var]])
    count_totals(kept)
  } else {
    purrr::map_dfr(sort(unique(stats::na.omit(surviving[[group_var]]))), function(lvl) {
      lvl_months <- surviving[[month_var]][surviving[[group_var]] == lvl]
      kept <- answered %>%
        dplyr::filter(.data[[group_var]] == lvl, .data[[month_var]] %in% lvl_months)
      dplyr::bind_cols(stats::setNames(list(lvl), group_var), count_totals(kept))
    })
  }
}

# 5. Subtitle summary numbers: n-weighted average and unweighted average
# Computed from the monthly (not rolling) weighted %:
#   n_weighted_avg : the n-weighted average of each month's weighted %,
#                     bigger months count more.
#   unweighted_avg : plain mean of the monthly weighted % values, every
#                     month counts equally.

# @param monthly_data Output of calc_prop_summary(), already filtered
#   to non-suppressed rows.
# @param prop_col Which column to average.
compute_trend_averages <- function(monthly_data, prop_col = "weighted_prop", n_col = "n") {
  props <- monthly_data[[prop_col]]
  ns    <- monthly_data[[n_col]]
  ok    <- !is.na(props) & !is.na(ns)

  list(
    n_weighted_avg = sum(props[ok] * ns[ok]) / sum(ns[ok]),
    unweighted_avg = mean(props[ok])
  )
}

# Formats compute_trend_averages() (and optionally compute_trend_totals())
# as the subtitle string every plot uses.
# @param totals Optional single-row output of compute_trend_totals(),
#   if given, adds a second line reporting how much data the trend is
#   based on.
format_trend_subtitle <- function(monthly_data, prop_col = "weighted_prop", n_col = "n", totals = NULL) {
  avgs <- compute_trend_averages(monthly_data, prop_col, n_col)
  base <- sprintf(
    "N-weighted average: %.1f%%  |  Unweighted average: %.1f%%",
    avgs$n_weighted_avg * 100, avgs$unweighted_avg * 100
  )
  if (is.null(totals)) return(base)

  paste0(
    base, "\n",
    sprintf(
      "Based on %s responses from %s unique participants (months below the minimum sample size excluded)",
      format(totals$total_responses, big.mark = ","), format(totals$total_participants, big.mark = ",")
    )
  )
}

# 6. Convenience wrappers: monthly + rolling + totals together, in one call

# Monthly + rolling + totals together, for one outcome.
# @param min_n Monthly suppression threshold.
# @param rolling_min_n Cumulative 3-month suppression threshold,
#   independent of min_n since a month can be individually too thin to
#   show on its own yet still contribute to a reliable pooled estimate.
# @return list(monthly, rolling: only the surviving rows, what plots
#   use; monthly_all, rolling_all: every row including suppressed ones,
#   with why; totals: total_responses/total_participants from the
#   surviving months).
calc_trend <- function(df, outcome_var, group_var = NULL,
                        weight_var = RAKE_WEIGHT_COL, month_var = "month",
                        min_n = if (is.null(group_var)) N_MIN_OVERALL else N_MIN_SUBGROUP,
                        rolling_min_n = if (is.null(group_var)) ROLLING_MIN_OVERALL else ROLLING_MIN_SUBGROUP) {
  monthly_all <- calc_prop_summary(df, outcome_var, group_var = group_var,
                                    weight_var = weight_var, month_var = month_var, min_n = min_n)
  monthly <- dplyr::filter(monthly_all, !suppressed)
  rolling_all <- calc_rolling_avg(monthly_all, group_var = group_var, min_n = rolling_min_n,
                                   raw_df = df, outcome_var = outcome_var,
                                   demographic_group_var = group_var, weight_var = weight_var)
  rolling <- dplyr::filter(rolling_all, !rolling_suppressed)
  totals  <- compute_trend_totals(df, outcome_var, monthly_all, group_var = group_var, month_var = month_var)

  list(monthly = monthly, rolling = rolling, monthly_all = monthly_all, rolling_all = rolling_all, totals = totals)
}

# Monthly + rolling + totals together, for several outcomes at once,
# e.g. the 4 Parent Distress measures. totals has one row per outcome,
# since each measure can have different missingness and suppression.
calc_multi_trend <- function(df, outcome_vars, group_var = NULL,
                              weight_var = RAKE_WEIGHT_COL, month_var = "month",
                              min_n = if (is.null(group_var)) N_MIN_OVERALL else N_MIN_SUBGROUP,
                              rolling_min_n = if (is.null(group_var)) ROLLING_MIN_OVERALL else ROLLING_MIN_SUBGROUP) {
  monthly_all <- calc_multi_prop_summary(df, outcome_vars, group_var = group_var,
                                          weight_var = weight_var, month_var = month_var, min_n = min_n)
  monthly <- dplyr::filter(monthly_all, !suppressed)

  if (is.null(group_var)) {
    # Only "outcome" distinguishes one series from another.
    rolling_all <- calc_rolling_avg(monthly_all, group_var = "outcome", min_n = rolling_min_n,
                                     raw_df = df, outcome_var_lookup = outcome_vars, weight_var = weight_var)
  } else {
    rolling_all <- monthly_all %>%
      dplyr::mutate(.trend_key = paste(outcome, .data[[group_var]])) %>%
      calc_rolling_avg(group_var = ".trend_key", min_n = rolling_min_n,
                        raw_df = df, outcome_var_lookup = outcome_vars,
                        demographic_group_var = group_var, weight_var = weight_var) %>%
      dplyr::select(-.trend_key)
  }
  rolling <- dplyr::filter(rolling_all, !rolling_suppressed)

  totals <- purrr::imap_dfr(outcome_vars, function(col, label) {
    compute_trend_totals(df, col, dplyr::filter(monthly_all, outcome == label),
                          group_var = group_var, month_var = month_var) %>%
      dplyr::mutate(outcome = label, .before = 1)
  })

  list(monthly = monthly, rolling = rolling, monthly_all = monthly_all, rolling_all = rolling_all, totals = totals)
}
