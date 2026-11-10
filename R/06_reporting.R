
# 06_reporting.R

# Shared "which figures does each domain get, and how do we split a
# grouping into per-level pieces" logic, used by the report, the
# pipeline driver, and the Shiny dashboard, so none of them ever risk
# defining this differently.
#
# The outcome-variable constants used below (MATERIAL_HARDSHIP_OVERALL,
# PARENT_DISTRESS_LOWER, etc.) need to already be loaded.


library(dplyr)



# 1. Which figure(s) each domain gets.

DOMAIN_FIGURES <- list(
  material_hardship = list(
    heading = "Material Hardship",
    figures = list(
      # "Any" is the overall composite, alongside each individual
      # source. DOMAIN_MEASURE_ONLY exposes both as Measure choices,
      # all resolving to this one figure.
      list(title = "Material Hardship by Source", outcome_vars = c(Any = "fs_any_a", MATERIAL_HARDSHIP_TYPES))
    )
  ),
  parent_distress = list(
    heading = "Parent Emotional Distress",
    figures = list(
      list(title = "Composite: Any Measure (Both Thresholds)", outcome_vars = PARENT_DISTRESS_COMPOSITE),
      list(title = "Depression (Both Thresholds)",              outcome_vars = PARENT_DISTRESS_DEPRESSION),
      list(title = "Anxiety (Both Thresholds)",                 outcome_vars = PARENT_DISTRESS_ANXIETY),
      list(title = "Stress (Both Thresholds)",                  outcome_vars = PARENT_DISTRESS_STRESS),
      list(title = "Loneliness (Both Thresholds)",               outcome_vars = PARENT_DISTRESS_LONELINESS),
      list(title = paste0("Four Measures — ", PARENT_DISTRESS_THRESHOLD_LABELS[["Lower"]]),  outcome_vars = PARENT_DISTRESS_LOWER),
      list(title = paste0("Four Measures — ", PARENT_DISTRESS_THRESHOLD_LABELS[["Higher"]]), outcome_vars = PARENT_DISTRESS_HIGHER)
    )
  ),
  child_behavior = list(
    heading = "Child Behavior",
    figures = list(
      list(title = "Composite: Any Measure (Both Thresholds)", outcome_vars = CHILD_BEHAVIOR_COMPOSITE),
      list(title = "Fussy (Both Thresholds)",                   outcome_vars = CHILD_BEHAVIOR_FUSSY),
      list(title = "Fear (Both Thresholds)",                    outcome_vars = CHILD_BEHAVIOR_FEAR),
      list(title = paste0("Fear/Fussy — ", CHILD_BEHAVIOR_THRESHOLD_LABELS[["Lower"]]),  outcome_vars = CHILD_BEHAVIOR_LOWER),
      list(title = paste0("Fear/Fussy — ", CHILD_BEHAVIOR_THRESHOLD_LABELS[["Higher"]]), outcome_vars = CHILD_BEHAVIOR_HIGHER)
    )
  ),
  child_care = list(
    heading = "Child Care",
    figures = list(
      list(title = "Child Care by Type", outcome_vars = CHILD_CARE_TYPES)
    )
  ),
  healthcare = list(
    heading = "Healthcare",
    figures = list(
      list(title = "Missed Well-Baby/Well-Child Check-Ups", outcome_vars = HEALTHCARE_OVERALL)
    )
  ),
  food_insecurity = list(
    heading = "Food Insecurity",
    figures = list(
      list(title = "Food Insecurity", outcome_vars = FOOD_INSECURITY_OVERALL),
      list(title = "Severity (3 Thresholds)", outcome_vars = FOOD_INSECURITY_SEVERITY)
    )
  )
)

# Every grouping label a report/dashboard/pipeline consumer should
# offer, in order: Overall, then each demographic grouping, then
# Source. LA County is NOT here -- it's a geography scope (see `geo`
# on every trend/sample-description row), not a grouping breakdown, so
# it no longer competes with Race/Age/etc. as a single pinned level
# that can't itself be broken down further.
ALL_GROUPING_LABELS <- c("Overall", names(GROUPING_VARS), "Source")

# The two geography scopes every trend/monthly/sample-description row
# is tagged with (see the build_*_by_geo() wrappers below). A plain
# top-level toggle, not a grouping -- every grouping breakdown
# (Race, Source, etc.) exists within EACH of these, independently.
GEO_SCOPE_CHOICES <- c("CA Statewide", "LA County")



# 1b. Domain metadata: which domains have a measure axis and/or a
# threshold axis, on top of the grouping axis every domain already has.
# Lets a "Compare by" control ask what it can compare for a given
# domain and resolve straight to a DOMAIN_FIGURES entry, instead of
# pattern-matching figure titles itself.
#
# Parent Distress and Child Behavior have both: every individual
# measure exists at a Lower and a Higher threshold, plus an "Any
# measure" composite. Child Care has a measure axis only, no threshold
# concept. Material Hardship, Healthcare, and Food Insecurity's overall
# figure have neither; comparing by Grouping is the only option there.
#
# both_thresholds_figure(measure) / all_measures_figure(threshold)
# return the exact DOMAIN_FIGURES title for that slice. Kept as
# functions, not a lookup table, so the mapping stays in sync with the
# titles defined above.
DOMAIN_MEASURE_THRESHOLD <- list(
  parent_distress = list(
    measures         = c("Any", "Depression", "Anxiety", "Stress", "Loneliness"),
    thresholds       = c("Lower", "Higher"),
    threshold_labels = PARENT_DISTRESS_THRESHOLD_LABELS,
    both_thresholds_figure = function(measure) {
      if (measure == "Any") "Composite: Any Measure (Both Thresholds)" else paste0(measure, " (Both Thresholds)")
    },
    all_measures_figure = function(threshold) paste0("Four Measures — ", PARENT_DISTRESS_THRESHOLD_LABELS[[threshold]])
  ),
  child_behavior = list(
    measures         = c("Any", "Fussy", "Fear"),
    thresholds       = c("Lower", "Higher"),
    threshold_labels = CHILD_BEHAVIOR_THRESHOLD_LABELS,
    both_thresholds_figure = function(measure) {
      if (measure == "Any") "Composite: Any Measure (Both Thresholds)" else paste0(measure, " (Both Thresholds)")
    },
    all_measures_figure = function(threshold) paste0("Fear/Fussy — ", CHILD_BEHAVIOR_THRESHOLD_LABELS[[threshold]])
  )
)

# axis_label is what the "Compare by"/"Facet by"/pin controls call this
# axis in the UI -- these choices are genuinely TYPES (of care, of
# hardship), not separate measures the way Parent Distress/Child
# Behavior's are, so the control says "Type" rather than "Measure".
# The internal value passed around (input$compare_by == "Measure", etc.)
# never changes; this only relabels what the user sees.
DOMAIN_MEASURE_ONLY <- list(
  child_care = list(
    measures   = c("Parent-Only Care", "Center-Based Care", "Non-Center-Based Care"),
    axis_label = "Type"
  ),
  material_hardship = list(
    measures   = c("Any", names(MATERIAL_HARDSHIP_TYPES)),
    axis_label = "Type"
  )
)

# Threshold axis only, no measure axis. Food Insecurity has one
# underlying score, so nothing to compare on a Measure axis, but it has
# 3 severity thresholds to compare on a Threshold axis. figure_title
# points at the severity breakdown specifically, not the plain overall
# figure.
DOMAIN_THRESHOLD_ONLY <- list(
  food_insecurity = list(
    thresholds       = c("Lower", "Moderate", "Higher"),
    threshold_labels = FOOD_INSECURITY_THRESHOLD_LABELS,
    figure_title     = "Severity (3 Thresholds)"
  )
)

#' Display text for this domain's measure axis -- "Measure" by default,
#' or whatever DOMAIN_MEASURE_ONLY's axis_label says instead (e.g.
#' "Type" for Material Hardship/Child Care). The internal value is
#' always "Measure" regardless of this; only the label shown changes.
measure_axis_label <- function(domain_key) {
  spec <- DOMAIN_MEASURE_ONLY[[domain_key]]
  if (!is.null(spec) && !is.null(spec$axis_label)) spec$axis_label else "Measure"
}

#' Which axes a "Compare by" control should offer for this domain, as a
#' named vector: names are the display text (so a domain-specific
#' measure_axis_label() shows up in the UI), values are the internal
#' codes everything else in this file matches on ("Grouping"/"Measure"/
#' "Threshold"). A plain Shiny selectInput does the right thing with a
#' vector shaped like this on its own: shows the names, stores the values.
get_compare_by_choices <- function(domain_key) {
  if (domain_key %in% names(DOMAIN_MEASURE_THRESHOLD)) {
    return(stats::setNames(c("Grouping", "Measure", "Threshold"), c("Grouping", "Measure", "Threshold")))
  }
  if (domain_key %in% names(DOMAIN_MEASURE_ONLY)) {
    return(stats::setNames(c("Grouping", "Measure"), c("Grouping", measure_axis_label(domain_key))))
  }
  if (domain_key %in% names(DOMAIN_THRESHOLD_ONLY)) {
    return(stats::setNames(c("Grouping", "Threshold"), c("Grouping", "Threshold")))
  }
  stats::setNames("Grouping", "Grouping")
}

#' Measure choices for this domain's Measure selector, or NULL if this
#' domain has no measure axis.
get_measure_choices <- function(domain_key) {
  if (!is.null(DOMAIN_MEASURE_THRESHOLD[[domain_key]])) return(DOMAIN_MEASURE_THRESHOLD[[domain_key]]$measures)
  if (!is.null(DOMAIN_MEASURE_ONLY[[domain_key]]))       return(DOMAIN_MEASURE_ONLY[[domain_key]]$measures)
  NULL
}

#' Threshold choices for this domain's Threshold selector, or NULL if
#' this domain has no threshold axis. Returns a named vector: names are
#' the domain's own display text, values are the internal short codes
#' (Lower/Higher/Moderate) everything else in this file matches on. A
#' plain Shiny selectInput already does the right thing with a vector
#' shaped like this: shows the names, stores the values.
get_threshold_choices <- function(domain_key) {
  spec <- DOMAIN_MEASURE_THRESHOLD[[domain_key]]
  if (is.null(spec)) spec <- DOMAIN_THRESHOLD_ONLY[[domain_key]]
  if (is.null(spec)) return(NULL)
  setNames(spec$thresholds, unname(spec$threshold_labels[spec$thresholds]))
}

#' Resolve a dashboard "Compare by" selection, plus whatever the other
#' two axes are pinned to, down to the (figure_title, outcome_label)
#' pair the panel-data functions below expect.
#'
#' @param compare_by One of "Grouping", "Measure", "Threshold".
#' @param measure_pin,threshold_pin The other axes' fixed values,
#'   required whenever that axis exists for this domain and isn't the
#'   one being compared. threshold_pin == "Both" only makes sense when
#'   compare_by == "Grouping": it pins one measure to the composite
#'   figure and shows both thresholds as its two lines. It is not safe
#'   when compare_by == "Measure", since there's no "all measures, both
#'   thresholds" figure, so that combination would silently match zero
#'   rows. The dashboard's own threshold picker never offers "Both" for
#'   this reason.
#' @return list(figure_title = ..., outcome_label = ...). outcome_label
#'   is NULL when the compare axis is Measure or Threshold, or when
#'   compare_by == "Grouping" and threshold_pin is "Both".
resolve_domain_figure <- function(domain_key, compare_by, measure_pin = NULL, threshold_pin = NULL) {
  mt_spec <- DOMAIN_MEASURE_THRESHOLD[[domain_key]]
  if (!is.null(mt_spec)) {
    if (compare_by == "Threshold") {
      return(list(figure_title = mt_spec$both_thresholds_figure(measure_pin), outcome_label = NULL))
    }
    if (compare_by == "Measure") {
      # threshold_pin is NULL for the "Overall" reference panel while
      # faceting by Threshold -- its own pin control is hidden then, so
      # there's nothing to look up. Also falls back here for a stale
      # value that isn't one of THIS domain's real threshold codes
      # (same staleness risk as the Grouping branch below). Either way,
      # fall back to this domain's first threshold so there's always a
      # real figure to resolve to, instead of indexing a lookup with an
      # unusable value and erroring.
      effective_threshold <- if (is.null(threshold_pin) || !threshold_pin %in% mt_spec$thresholds) {
        mt_spec$thresholds[[1]]
      } else {
        threshold_pin
      }
      return(list(figure_title = mt_spec$all_measures_figure(effective_threshold), outcome_label = NULL))
    }
    # threshold_pin can be stale -- Shiny doesn't synchronously clear an
    # input the instant its selectInput's renderUI output changes (e.g.
    # right after switching domain, or compare_by/facet_by, to a
    # combination with different threshold codes or no threshold axis
    # at all). An unrecognized value is treated exactly like NULL
    # (no pin) rather than indexing threshold_labels and crashing --
    # the next reactive tick resolves to the real current value anyway.
    outcome_label <- if (is.null(threshold_pin) || identical(threshold_pin, "Both") ||
                          !threshold_pin %in% names(mt_spec$threshold_labels)) {
      NULL
    } else {
      unname(mt_spec$threshold_labels[[threshold_pin]])
    }
    return(list(figure_title = mt_spec$both_thresholds_figure(measure_pin), outcome_label = outcome_label))
  }

  m_spec <- DOMAIN_MEASURE_ONLY[[domain_key]]
  if (!is.null(m_spec)) {
    figure_title <- DOMAIN_FIGURES[[domain_key]]$figures[[1]]$title
    outcome_label <- if (compare_by == "Measure") NULL else measure_pin
    return(list(figure_title = figure_title, outcome_label = outcome_label))
  }

  t_spec <- DOMAIN_THRESHOLD_ONLY[[domain_key]]
  if (!is.null(t_spec)) {
    # Same NULL case as above: this domain's threshold pin is hidden
    # while facet_by == "Threshold", so threshold_pin arrives NULL for
    # the "Overall" reference panel.
    # Same staleness guard as the DOMAIN_MEASURE_THRESHOLD branch above.
    outcome_label <- if (compare_by == "Threshold" || is.null(threshold_pin) ||
                          !threshold_pin %in% names(t_spec$threshold_labels)) {
      NULL
    } else {
      unname(t_spec$threshold_labels[[threshold_pin]])
    }
    return(list(figure_title = t_spec$figure_title, outcome_label = outcome_label))
  }

  list(figure_title = DOMAIN_FIGURES[[domain_key]]$figures[[1]]$title, outcome_label = NULL)
}



# 2. Splitting a grouping into per-level pieces


# Split a grouping label into one or more (level_name, data, min_n)
# pieces. Overall and LA County are always exactly one piece each;
# every other grouping produces one piece per level present in the
# data.
#
# Filtering to one level first and calling calc_trend()/
# calc_multi_trend() with no group_var, instead of passing the grouping
# straight through, lets the plot functions stay built for "one series,
# optionally faceted by outcome" without also handling an arbitrary
# grouping variable.

# @param df master_core after every domain outcome has been added
#   (already filtered to one geography's respondents -- this function
#   has no idea which geography it is, see `geo` below).
# @param grouping_label One of ALL_GROUPING_LABELS.
# @param geo "CA Statewide" or "LA County" -- only matters for the
#   "Overall" branch: LA County's Overall has always used the
#   subgroup threshold (30/90), not the true overall one (50/100),
#   matching how LA County was treated as a geography filter even
#   before it became its own geo scope. Every other grouping tab
#   already uses the subgroup threshold regardless of geography, so
#   this parameter changes nothing for them.
# @return A named list; each element is list(data = <filtered df>,
#   min_n = <monthly suppression threshold for this cut>, rolling_min_n
#   = <cumulative 3-month rolling threshold>).
resolve_grouping_levels <- function(df, grouping_label, geo = "CA Statewide") {
  if (grouping_label == "Overall") {
    min_n         <- if (geo == "CA Statewide") N_MIN_OVERALL        else N_MIN_SUBGROUP
    rolling_min_n <- if (geo == "CA Statewide") ROLLING_MIN_OVERALL  else ROLLING_MIN_SUBGROUP
    return(list(Overall = list(data = df, min_n = min_n, rolling_min_n = rolling_min_n)))
  }
  if (grouping_label == "Source") {
    # Source (which survey/panel administration, e.g. "LAEP") isn't in
    # GROUPING_VARS -- it's a plain column directly on df, same one
    # sample_description_breakdown() already reads -- so it needs its
    # own branch here rather than the GROUPING_VARS lookup below. Its
    # own, lower suppression bar (N_MIN_SOURCE/ROLLING_MIN_SOURCE) is
    # in 00_config.R, since some sources have far fewer respondents in
    # any given month than a demographic subgroup typically does.
    # !is.na() alone isn't enough here: a genuinely blank (empty
    # string, not NA) Source value would become a level NAMED "" below
    # -- and list[[""]] always returns NULL in R, even for an element
    # that really is named "" (a longstanding R `[[` quirk, distinct
    # from how a real, non-empty name behaves). That NULL then flows
    # all the way down into calc_multi_trend()'s df argument and blows
    # up with "no applicable method for 'filter' applied to an object
    # of class NULL" several calls later, with no indication it was
    # ever about an empty string to begin with. nzchar() excludes "" the
    # same way is.na() excludes NA, so neither kind of non-value ever
    # becomes a level.
    raw_source <- as.character(df$Source)
    levels_present <- sort(unique(raw_source[!is.na(raw_source) & nzchar(raw_source)]))
    return(stats::setNames(
      lapply(levels_present, function(lvl) {
        list(data = dplyr::filter(df, Source == lvl), min_n = N_MIN_SOURCE, rolling_min_n = ROLLING_MIN_SOURCE)
      }),
      levels_present
    ))
  }

  group_col <- GROUPING_VARS[[grouping_label]]
  levels_present <- order_grouping_levels(unique(stats::na.omit(df[[group_col]])), grouping_label)
  stats::setNames(
    lapply(levels_present, function(lvl) {
      list(data = dplyr::filter(df, .data[[group_col]] == lvl), min_n = N_MIN_SUBGROUP, rolling_min_n = ROLLING_MIN_SUBGROUP)
    }),
    levels_present
  )
}



# 3. Build every (domain, figure, grouping, level) trend table at once


# Computes the trend table for every domain x figure x grouping x level
# combination at once, stacked into one long data frame. This is what
# the pipeline driver saves for the dashboard to read.

# @param master_analysis master_core after weighting and domain
#   outcomes have both run.
# @param domain_keys Which domains to include. Defaults to all 6.
# @param grouping_labels Which groupings to include. Defaults to all of
#   ALL_GROUPING_LABELS.

# Computes one (domain, figure, grouping, level) cut's calc_trend()/
# calc_multi_trend() result, without yet picking which part of it to
# keep, so a caller that wants both the rolling table and the monthly
# table gets both from a single call instead of running the cut twice.
# @param keep_field Which part of the result this cut keeps downstream:
#   "rolling" (only rows that cleared rolling suppression), or
#   "monthly_all" (every month that exists, suppressed or not).
# @param na_suppressed_monthly_prop Only meaningful when keep_field ==
#   "rolling". The rolling table only drops rolling-suppressed rows; it
#   does not touch the monthly weighted_prop/CI columns carried through
#   underneath. A month can be individually suppressed yet still appear
#   here because its 3-month cumulative n cleared the rolling
#   threshold, leaving its real, policy-suppressed monthly value
#   un-blanked. This guards that at the source, once, so every
#   downstream plot gets already-clean data.
# @return One table (possibly 0 rows), or NULL if this cut has no data
#   at all.
compute_one_trend_cut <- function(domain_key, fig, grouping_label, level_name, level_info) {
  trend <- if (length(fig$outcome_vars) == 1) {
    calc_trend(level_info$data, fig$outcome_vars[[1]],
               min_n = level_info$min_n, rolling_min_n = level_info$rolling_min_n)
  } else {
    calc_multi_trend(level_info$data, fig$outcome_vars,
                      min_n = level_info$min_n, rolling_min_n = level_info$rolling_min_n)
  }
  list(domain_key = domain_key, fig = fig, grouping_label = grouping_label,
       level_name = level_name, trend = trend)
}

# Turns one compute_one_trend_cut() result into a finished table for a
# given keep_field: attaches the domain/figure/grouping/level
# identifying columns and totals.
finish_trend_cut <- function(cut, keep_field, na_suppressed_monthly_prop) {
  # Named "domain_info", not "domain": the table below creates a column
  # called "domain" inside mutate(), which would otherwise shadow this
  # variable and break the domain_info$heading reference two lines later.
  domain_info <- DOMAIN_FIGURES[[cut$domain_key]]
  trend <- cut$trend

  kept <- trend[[keep_field]]
  if (nrow(kept) == 0) return(NULL)

  table <- kept %>%
    dplyr::mutate(
      domain         = cut$domain_key,
      domain_heading = domain_info$heading,
      figure_title   = cut$fig$title,
      grouping       = cut$grouping_label,
      level          = cut$level_name
    )
  if (na_suppressed_monthly_prop) {
    table <- table %>%
      dplyr::mutate(
        weighted_prop    = dplyr::if_else(suppressed, NA_real_, weighted_prop),
        weighted_ci_low  = dplyr::if_else(suppressed, NA_real_, weighted_ci_low),
        weighted_ci_high = dplyr::if_else(suppressed, NA_real_, weighted_ci_high)
      )
  }
  if (!"outcome" %in% names(table)) table$outcome <- NA_character_

  # Attach total_responses/total_participants, computed from only the
  # surviving, non-suppressed months so it matches what's plotted. One
  # pair per outcome for multi-outcome figures, one overall pair
  # otherwise.
  if ("outcome" %in% names(trend$totals)) {
    table <- table %>% dplyr::left_join(trend$totals, by = "outcome")
  } else {
    table$total_responses    <- trend$totals$total_responses
    table$total_participants <- trend$totals$total_participants
  }

  table
}

# Every (grouping_label, level_name, domain_key, fig) combination the
# build functions below need to compute, as a flat list.
# resolve_grouping_levels() doesn't depend on domain_key, so it's
# computed once per grouping_label here, not once per domain, which
# would recompute the same level split 6 times over.
build_trend_cut_work_items <- function(master_analysis, domain_keys, grouping_labels, verbose, geo = "CA Statewide") {
  t0 <- Sys.time()
  work_items <- list()
  for (grouping_label in grouping_labels) {
    t_level0 <- Sys.time()
    levels_to_use <- resolve_grouping_levels(master_analysis, grouping_label, geo = geo)
    level_elapsed <- as.numeric(difftime(Sys.time(), t_level0, units = "secs"))
    if (verbose) {
      cat("  [", grouping_label, "]", length(levels_to_use), "level(s)",
          sprintf(" (resolve_grouping_levels: %.1f sec)\n", level_elapsed))
    }

    for (level_name in names(levels_to_use)) {
      level_info <- levels_to_use[[level_name]]
      for (domain_key in domain_keys) {
        for (fig in DOMAIN_FIGURES[[domain_key]]$figures) {
          work_items[[length(work_items) + 1]] <- list(
            domain_key = domain_key, fig = fig, grouping_label = grouping_label,
            level_name = level_name, level_info = level_info
          )
        }
      }
    }
  }
  # If this step alone is slow, it's almost always a data issue in a
  # grouping column (far more distinct values than expected), not the
  # per-cut computation that follows. Timed separately so the two are
  # never confused.
  if (verbose) {
    cat("  -- build_trend_cut_work_items(): ", length(work_items), " total cuts, ",
        sprintf("%.1f", as.numeric(difftime(Sys.time(), t0, units = "secs"))), " sec --\n", sep = "")
  }
  work_items
}

# Runs compute_one_trend_cut() over every work item in parallel, and
# returns the raw list of cuts, not yet reduced to one keep_field.
# @param verbose Prints how many cuts ran and how long the pass took.
#   If this step is the slow one, the problem is inside a specific
#   cut's data, not in resolving grouping levels.
run_trend_cuts_parallel <- function(work_items, caller_name, verbose = FALSE) {
  n_items <- length(work_items)
  if (verbose) {
    cat("  -- running", n_items, "cuts across", PIPELINE_CORES, "core(s)...\n")
  }
  t0 <- Sys.time()
  # Forked workers' cat() output still reaches this console directly, so
  # a progress line every ~5% of items gives a live heartbeat during
  # what would otherwise be total silence until every cut finishes.
  print_every <- max(1L, round(n_items / 20))
  results <- parallel::mclapply(seq_len(n_items), function(i) {
    w <- work_items[[i]]
    if (verbose && (i %% print_every == 0 || i == n_items)) {
      cat(sprintf("     [%s] %d/%d cuts started, %.1f sec elapsed\n",
                  caller_name, i, n_items, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
    }
    # Caught here, with the identifying (domain, grouping, level)
    # attached to the condition, rather than relying on mclapply's own
    # try(silent = TRUE) wrapper: that one just captures whatever
    # condition$message already says, which for an error that
    # originated inside a purrr::map()/imap() call (calc_multi_trend()'s
    # per-outcome loop, used by every multi-measure domain) is often
    # just a terse "In index: N." with the actual cause several lines
    # up in a `cnd_message()`-style chain that a plain $message never
    # unwraps. tryCatch()+rlang::cnd_message() here gets the full
    # chain, and pasting on which (domain, grouping, level) cut it was
    # means a failure is traceable without having to re-run serially
    # just to find out which cut broke.
    tryCatch(
      compute_one_trend_cut(w$domain_key, w$fig, w$grouping_label, w$level_name, w$level_info),
      error = function(e) {
        structure(
          list(
            domain_key = w$domain_key, fig_title = w$fig$title,
            grouping_label = w$grouping_label, level_name = w$level_name,
            message = rlang::cnd_message(e, prefix = TRUE)
          ),
          class = "trend_cut_error"
        )
      }
    )
  }, mc.cores = PIPELINE_CORES)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (verbose) {
    cat(sprintf("  -- run_trend_cuts_parallel(): %.1f sec total, %.2f sec/cut average --\n",
                elapsed, elapsed / max(1, n_items)))
  }

  # mclapply() can ALSO still produce a plain try-error itself (e.g. a
  # forked worker crashing outright, not caught by the tryCatch above
  # at all) -- checked separately from trend_cut_error so either kind
  # of failure gets reported, not just the one this function added.
  is_failure <- function(x) inherits(x, "trend_cut_error") || inherits(x, "try-error")
  failed <- vapply(results, is_failure, logical(1))
  if (any(failed)) {
    descriptions <- vapply(results[failed], function(x) {
      if (inherits(x, "trend_cut_error")) {
        sprintf("  - domain=%s, figure=%s, grouping=%s, level=%s:\n    %s",
                x$domain_key, x$fig_title, x$grouping_label, x$level_name, x$message)
      } else {
        sprintf("  - (cut identity unknown -- mclapply-level failure, not caught by this function's own tryCatch):\n    %s",
                attr(x, "condition")$message)
      }
    }, character(1))
    stop(sprintf("%s() failed on %d of %d cut(s):\n%s",
                 caller_name, sum(failed), n_items, paste(descriptions, collapse = "\n")))
  }

  results
}

# Reduces a run_trend_cuts_parallel() result to one bound table for a
# given keep_field.
bind_finished_cuts <- function(cuts, keep_field, na_suppressed_monthly_prop) {
  dplyr::bind_rows(Filter(Negate(is.null), lapply(cuts, finish_trend_cut,
    keep_field = keep_field, na_suppressed_monthly_prop = na_suppressed_monthly_prop)))
}

# @return One data frame with columns domain, domain_heading,
#   figure_title, grouping, level, plus every column the rolling output
#   already has (n, weighted_prop, unweighted_prop, rolling_weighted_prop,
#   window_months, etc.), and outcome for multi-outcome figures (NA for
#   single-outcome ones). Also total_responses/total_participants, the
#   same value repeated on every row of one cut.
build_all_trend_tables <- function(master_analysis,
                                    domain_keys = names(DOMAIN_FIGURES),
                                    grouping_labels = ALL_GROUPING_LABELS,
                                    verbose = FALSE, geo = "CA Statewide") {
  work_items <- build_trend_cut_work_items(master_analysis, domain_keys, grouping_labels, verbose, geo = geo)
  cuts <- run_trend_cuts_parallel(work_items, caller_name = "build_all_trend_tables", verbose = verbose)
  bind_finished_cuts(cuts, keep_field = "rolling", na_suppressed_monthly_prop = TRUE)
}



# 3b. Every (domain, figure, grouping, level) month at once. The
# monthly twin of build_all_trend_tables(), for anything that needs one
# specific calendar month's own value instead of a rolling estimate.
#
# Can't just reuse build_all_trend_tables()'s output: that table only
# keeps a row if its rolling estimate survived the cumulative
# threshold. A month can have a perfectly fine monthly n on its own and
# still be invisible there if its neighboring 2 months are thin enough
# to drag the 3-month cumulative sum below the limit. This table keeps
# every month that exists at all, suppressed or not, with its own
# suppressed/suppress_reason intact.
# @return Same shape as build_all_trend_tables(), one row per (domain,
#   figure, grouping, level[, outcome], month), including suppressed
#   months.
build_all_monthly_tables <- function(master_analysis,
                                      domain_keys = names(DOMAIN_FIGURES),
                                      grouping_labels = ALL_GROUPING_LABELS,
                                      verbose = FALSE, geo = "CA Statewide") {
  work_items <- build_trend_cut_work_items(master_analysis, domain_keys, grouping_labels, verbose, geo = geo)
  cuts <- run_trend_cuts_parallel(work_items, caller_name = "build_all_monthly_tables", verbose = verbose)
  bind_finished_cuts(cuts, keep_field = "monthly_all", na_suppressed_monthly_prop = FALSE)
}

# Same two tables as build_all_trend_tables()/build_all_monthly_tables(),
# computed in one pass. The pipeline driver always wants both, and a
# single cut computation already produces both pieces internally, so
# calling the two functions separately would run the expensive part
# twice for nothing. Also returns the raw `cuts`, so the Excel export
# (R/05_excel_export.R) can reuse the same already-computed trends
# instead of running calc_trend()/calc_multi_trend() a second time.
# @return list(trend_tables = ..., monthly_tables = ..., cuts = ...).
build_all_trend_and_monthly_tables <- function(master_analysis,
                                                domain_keys = names(DOMAIN_FIGURES),
                                                grouping_labels = ALL_GROUPING_LABELS,
                                                verbose = FALSE, geo = "CA Statewide") {
  work_items <- build_trend_cut_work_items(master_analysis, domain_keys, grouping_labels, verbose, geo = geo)
  cuts <- run_trend_cuts_parallel(work_items, caller_name = "build_all_trend_and_monthly_tables", verbose = verbose)
  list(
    trend_tables   = bind_finished_cuts(cuts, keep_field = "rolling", na_suppressed_monthly_prop = TRUE),
    monthly_tables = bind_finished_cuts(cuts, keep_field = "monthly_all", na_suppressed_monthly_prop = FALSE),
    cuts           = cuts
  )
}

#' build_all_trend_and_monthly_tables(), once for all of CA and once
#' for the LA-county-only subset (la_county == 1 on master_analysis),
#' so every grouping breakdown (Race, Source, etc.) is available WITHIN
#' LA County too -- not just one pinned "LA County" line with no
#' further breakdown, which is how this worked before LA County had its
#' own geography scope. Reuses build_all_trend_and_monthly_tables()
#' completely unchanged, just called twice on two different input
#' slices; the only new work here is tagging each output with `geo`
#' ("CA Statewide"/"LA County") and stacking the two together.
#'
#' `cuts`/`cuts_la` are kept SEPARATE, not combined like the two tables
#' are: R/05_excel_export.R's combine_cut_levels() looks a cut up by
#' (domain_key, figure_title, grouping_label) alone, with no `geo` of
#' its own to filter on, so each geography's Excel export needs that
#' geography's own, separate cut list to look up from.
#' @return list(trend_tables = <geo-tagged, combined>, monthly_tables
#'   = <geo-tagged, combined>, cuts = <CA-only, for Excel export>,
#'   cuts_la = <LA-County-only, for Excel export>).
build_all_trend_and_monthly_tables_by_geo <- function(master_analysis,
                                                        domain_keys = names(DOMAIN_FIGURES),
                                                        grouping_labels = ALL_GROUPING_LABELS,
                                                        verbose = FALSE) {
  ca <- build_all_trend_and_monthly_tables(master_analysis, domain_keys, grouping_labels, verbose, geo = "CA Statewide")
  la <- build_all_trend_and_monthly_tables(
    dplyr::filter(master_analysis, la_county == 1), domain_keys, grouping_labels, verbose, geo = "LA County"
  )
  list(
    trend_tables = dplyr::bind_rows(
      dplyr::mutate(ca$trend_tables, geo = "CA Statewide"),
      dplyr::mutate(la$trend_tables, geo = "LA County")
    ),
    monthly_tables = dplyr::bind_rows(
      dplyr::mutate(ca$monthly_tables, geo = "CA Statewide"),
      dplyr::mutate(la$monthly_tables, geo = "LA County")
    ),
    cuts    = ca$cuts,
    cuts_la = la$cuts
  )
}



# 4. Sample description table (for the dashboard's Sample Description tab)


# EXACT unique-participant counts (distinct CALID), independent of any
# domain's outcome data, by Source and by each demographic grouping.
# Deliberately NOT a response/row count: a respondent who answers in
# more than one month (or completes more than one survey wave in the
# same month) would otherwise be counted once per response instead of
# once. Three units, each computed directly from respondent-level data
# here -- the only place that data exists -- so summing or combining
# them later at display time is never required and never happens:
#   overall   : whole-study distinct participants, no time split.
#   by_month  : distinct participants within each calendar month.
#   by_survey : distinct participants within each SurveyType x
#               SurveyNumber wave -- finer than by_month, since more
#               than one wave can open in the same calendar month and
#               by_month's count alone can't distinguish them.
# @param df Already time-sliced (or not) rows to count within.
# @param time_cols 0+ columns already grouping df (e.g. "month", or
#   c("SurveyType", "SurveyNumber")); character(0) for no time split.
# @return One row per (time_cols..., grouping, level): grouping is
#   "Overall", "Source", or a GROUPING_VARS label; level is the
#   specific value; n_participants is dplyr::n_distinct(CALID) within
#   that cell, never a row count.
sample_description_breakdown <- function(df, time_cols = character(0)) {
  time_syms <- rlang::syms(time_cols)
  n_distinct_calid <- function(data, ...) {
    data %>%
      dplyr::group_by(...) %>%
      dplyr::summarise(n_participants = dplyr::n_distinct(CALID), .groups = "drop")
  }

  overall <- n_distinct_calid(df, !!!time_syms) %>%
    dplyr::mutate(grouping = "Overall", level = "Overall")

  by_source <- df %>%
    dplyr::filter(!is.na(Source)) %>%
    n_distinct_calid(!!!time_syms, Source) %>%
    dplyr::rename(level = Source) %>%
    dplyr::mutate(grouping = "Source", level = as.character(level))

  by_grouping <- purrr::map_dfr(names(GROUPING_VARS), function(label) {
    col <- GROUPING_VARS[[label]]
    df %>%
      dplyr::filter(!is.na(.data[[col]])) %>%
      n_distinct_calid(!!!time_syms, .data[[col]]) %>%
      dplyr::rename(level = !!col) %>%
      dplyr::mutate(grouping = label, level = as.character(level))
  })

  dplyr::bind_rows(overall, by_source, by_grouping) %>%
    dplyr::select(dplyr::all_of(time_cols), grouping, level, n_participants) %>%
    dplyr::arrange(grouping, level)
}

# @param master_analysis master_core (Source and the grouping columns
#   are already present; domain outcomes aren't needed for this table).
# @return list(overall, by_month, by_survey) -- see
#   sample_description_breakdown()'s header for what each unit means.
#   by_survey additionally carries survey_label (human-readable,
#   e.g. "Survey #12 (Type 1, 2024/03)") and wave_start (that wave's
#   earliest StartDate), so the dashboard can show/sort waves
#   chronologically -- SurveyNumber alone doesn't always agree with
#   actual chronological order (see 01_prep.R's "Known issue" note).
build_sample_description_table <- function(master_analysis) {
  overall_tbl  <- sample_description_breakdown(master_analysis)
  by_month_tbl <- sample_description_breakdown(master_analysis, time_cols = "month") %>%
    dplyr::arrange(grouping, level, month)

  wave_info <- master_analysis %>%
    dplyr::group_by(SurveyType, SurveyNumber) %>%
    dplyr::summarise(wave_start = min(StartDate, na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(
      survey_label = sprintf(
        "Survey #%s (Type %s, %s)", SurveyNumber, SurveyType, format(as.Date(wave_start), "%Y/%m")
      )
    )

  by_survey_tbl <- sample_description_breakdown(master_analysis, time_cols = c("SurveyType", "SurveyNumber")) %>%
    dplyr::left_join(wave_info, by = c("SurveyType", "SurveyNumber")) %>%
    dplyr::select(SurveyType, SurveyNumber, survey_label, wave_start, grouping, level, n_participants) %>%
    dplyr::arrange(wave_start, grouping, level)

  list(overall = overall_tbl, by_month = by_month_tbl, by_survey = by_survey_tbl)
}

#' build_sample_description_table(), once for CA, once for LA County,
#' tagged with the same `geo` column as the trend tables -- so the
#' dashboard's Sample Description tab can split into a "CA Statewide"
#' tab and an "LA County" tab the same way the Trends tab splits.
build_sample_description_table_by_geo <- function(master_analysis) {
  ca <- build_sample_description_table(master_analysis)
  la <- build_sample_description_table(dplyr::filter(master_analysis, la_county == 1))
  list(
    overall = dplyr::bind_rows(
      dplyr::mutate(ca$overall, geo = "CA Statewide"), dplyr::mutate(la$overall, geo = "LA County")
    ),
    by_month = dplyr::bind_rows(
      dplyr::mutate(ca$by_month, geo = "CA Statewide"), dplyr::mutate(la$by_month, geo = "LA County")
    ),
    by_survey = dplyr::bind_rows(
      dplyr::mutate(ca$by_survey, geo = "CA Statewide"), dplyr::mutate(la$by_survey, geo = "LA County")
    )
  )
}

# A fine-grained FPL_CONT_locf histogram, one row per (month, bin), at
# a fixed 5-point bin width from 0 to 500% plus one "above 500%"
# catch-all. Pre-aggregated counts within a narrow band, not individual
# values, so it's safe to ship to the dashboard.
#
# Shipping a fixed fine grid rather than the raw FPL_CONT_locf values
# still lets the dashboard offer an adjustable bin width: any width
# that divides evenly into 5 can be built by summing neighboring fine
# bins at display time, with no respondent-level data ever leaving
# this pipeline run.
FPL_FINE_BIN_WIDTH <- 5
FPL_FINE_BIN_MAX   <- 500

# @return list(
#   bins                 : one row per (month, bin), as before -- n is a
#                          RESPONSE count (one row per survey response
#                          with a real FPL_CONT_locf value, not
#                          deduplicated by respondent).
#   participants_overall : exact distinct-CALID count across the whole
#                          study, among respondents with a real FPL
#                          value -- computed once, here, from real
#                          respondent-level data, which is the only way
#                          to get this number right. Summing n_distinct
#                          per month instead would double-count anyone
#                          who answered in more than one month.
#   participants_by_year : same idea, one exact count per calendar year
#                          (survey_year). "Overall" and "By Year" are
#                          the only two dashboard time windows fixed
#                          enough to precompute this for; a user-chosen
#                          custom range or single month isn't known
#                          until the dashboard runs, long after this
#                          pipeline (and the real data) is gone.
build_fpl_distribution_table <- function(master_analysis) {
  breaks <- c(seq(0, FPL_FINE_BIN_MAX, by = FPL_FINE_BIN_WIDTH), Inf)
  bin_starts <- breaks[-length(breaks)]

  valid <- master_analysis %>% dplyr::filter(!is.na(FPL_CONT_locf))

  # right = TRUE (bin edges are "<=") matches the poverty-band
  # convention used elsewhere, so a value sitting exactly on a bin edge
  # classifies the same way everywhere in this pipeline.
  bins <- valid %>%
    dplyr::mutate(
      bin_low = cut(FPL_CONT_locf, breaks = breaks, labels = bin_starts,
                    include.lowest = TRUE, right = TRUE),
      bin_low = as.numeric(as.character(bin_low))
    ) %>%
    dplyr::count(month, bin_low, name = "n") %>%
    dplyr::arrange(month, bin_low)

  list(
    bins                 = bins,
    participants_overall = dplyr::n_distinct(valid$CALID),
    participants_by_year  = valid %>%
      dplyr::group_by(survey_year) %>%
      dplyr::summarise(n_participants = dplyr::n_distinct(CALID), .groups = "drop")
  )
}

#' build_fpl_distribution_table(), once for CA, once for LA County.
#' `bins`/`participants_by_year` get the same `geo` column as the other
#' by-geo tables, since both are already data frames; the two single
#' numbers in `participants_overall` become a geo-named list instead
#' (`list(\`CA Statewide\` = ..., \`LA County\` = ...)`), since there's
#' no row to tag a bare scalar onto.
build_fpl_distribution_table_by_geo <- function(master_analysis) {
  ca <- build_fpl_distribution_table(master_analysis)
  la <- build_fpl_distribution_table(dplyr::filter(master_analysis, la_county == 1))
  list(
    bins = dplyr::bind_rows(
      dplyr::mutate(ca$bins, geo = "CA Statewide"), dplyr::mutate(la$bins, geo = "LA County")
    ),
    participants_overall = list(`CA Statewide` = ca$participants_overall, `LA County` = la$participants_overall),
    participants_by_year = dplyr::bind_rows(
      dplyr::mutate(ca$participants_by_year, geo = "CA Statewide"),
      dplyr::mutate(la$participants_by_year, geo = "LA County")
    )
  )
}



# 5. Querying a build_all_trend_tables()-shaped object into exactly
# what one figure's two plots (Monthly, Rolling) need. Shared by the
# report and the dashboard so the two can never drift into showing this
# differently.


#' Per-level summary stats (n-weighted average, unweighted average,
#' total responses, total participants), recomputed for whatever date
#' window is currently visible on a plot. Lets the dashboard's
#' window-stats panel update live as the user drags a range slider or
#' zooms, instead of always showing one fixed number for the whole
#' trend.
#'
#' total_participants falls back to an approximation only when the
#' visible window is a genuine zoomed-in sub-range: it sums each
#' included month's own distinct-respondent count, which double-counts
#' anyone who answered in more than one month of the window. In
#' practice this makes it come out close to total_responses whenever
#' nobody answers twice within the same month, which can look
#' misleadingly identical to total_responses instead of a smaller
#' unique count. Whenever the visible window is the full trend, this
#' uses the exact respondent-level distinct count instead, already
#' computed once during the pipeline run. Only a genuinely narrowed
#' window falls back to the approximation, since an exact count for an
#' arbitrary sub-range needs respondent-level data this dashboard never
#' has.
#'
#' @param panel_data One row per (level, month).
#' @param series "monthly" or "rolling", which estimate/n columns to use.
#' @param date_range Optional c(min_date, max_date) to restrict to; NULL
#'   uses every row given.
#' @return One row per distinct level present after filtering, with
#'   n_weighted_avg, unweighted_avg, total_responses, total_participants,
#'   n_months. Zero rows if nothing falls in date_range.
compute_window_stats <- function(panel_data, series = c("monthly", "rolling"), date_range = NULL) {
  series <- match.arg(series)
  est_col <- if (series == "monthly") "weighted_prop" else "rolling_weighted_prop"
  n_col   <- if (series == "monthly") "n"             else "rolling_n"

  full_range <- range(panel_data$date, na.rm = TRUE)
  is_full_window <- is.null(date_range) || isTRUE(all.equal(as.numeric(date_range), as.numeric(full_range)))

  df <- panel_data
  if (!is.null(date_range)) {
    df <- dplyr::filter(df, date >= date_range[1], date <= date_range[2])
  }
  if (nrow(df) == 0) return(df[0, ])

  df %>%
    dplyr::group_by(level) %>%
    dplyr::group_modify(~ {
      # total_responses/total_participants/n_months count every row in
      # the window, not just rows with a non-NA est_col, so these don't
      # silently shrink if ROLLING_MIN_MONTHS_IN_WINDOW is ever raised
      # above 1 (today, with the default, this never changes anything).
      # A month's raw response count is real even on a month whose
      # rolling average isn't available yet. The average itself still
      # only uses rows where est_col is actually available.
      avg_rows <- dplyr::filter(.x, !is.na(.data[[est_col]]))
      avgs <- if (nrow(avg_rows) > 0) {
        compute_trend_averages(avg_rows, prop_col = est_col, n_col = n_col)
      } else {
        list(n_weighted_avg = NA_real_, unweighted_avg = NA_real_)
      }

      exact_participants <- if ("total_participants" %in% names(.x)) .x$total_participants[1] else NA_integer_
      is_exact <- is_full_window && !is.na(exact_participants)
      total_participants <- if (is_exact) {
        exact_participants
      } else {
        # Always the raw monthly counts, even when series == "rolling":
        # rolling_n is already a trailing 3-month sum, so summing it
        # again across rows would multiply-count responses.
        sum(.x$n_participants, na.rm = TRUE)
      }

      tibble::tibble(
        n_weighted_avg        = avgs$n_weighted_avg,
        unweighted_avg        = avgs$unweighted_avg,
        total_responses       = sum(.x$n, na.rm = TRUE),
        total_participants    = total_participants,
        is_exact_participants = is_exact,
        n_months              = nrow(.x)
      )
    }) %>%
    dplyr::ungroup()
}

#' Filter a trend-tables object to exactly one domain/figure/grouping/
#' level/geography/date-range selection. `geo` defaults to "CA
#' Statewide" so every call site written before LA County became a
#' geography scope keeps its old (CA-wide) behavior unless it
#' deliberately opts into "LA County".
filter_trend_data <- function(trend_tables, domain_key, figure_title, grouping_label, level_name,
                               date_range = NULL, geo = "CA Statewide") {
  out <- trend_tables %>%
    dplyr::filter(
      domain == domain_key,
      figure_title == !!figure_title,
      grouping == grouping_label,
      level == level_name,
      geo == .env$geo
    )
  if (!is.null(date_range)) {
    out <- out %>% dplyr::filter(date >= date_range[1], date <= date_range[2])
  }
  out %>% dplyr::arrange(date)
}

#' Compare a set of "investigation" months against a "reference"
#' period's average, for one (domain, figure, grouping, level[,
#' outcome]) slice. A shared reference line, with bars showing how far
#' each investigated month differs from it.
#'
#' Overlap handling: any month that falls in both the reference range
#' and the investigation range is excluded from the reference average,
#' so a month is never compared against a baseline partly made of
#' itself. This also naturally covers "investigation == reference"
#' (nothing would be left to average), reported as a validation failure
#' rather than a meaningless all-zero comparison.
#'
#' @param monthly_tables Every month that exists in the data, suppressed
#'   or not.
#' @param reference_range,investigation_range Each c(min_date, max_date).
#'   reference_range = NULL means the full range present in this slice.
#' @param avg_type "weighted" (default) uses the n-weighted average
#'   across the reference months (bigger months count more) as the
#'   value bars are measured against; "unweighted" uses the plain
#'   average instead (every reference month counts equally). Both
#'   flavors are always returned in reference either way; this only
#'   picks which one drives deviation_pts.
#' @return list(
#'   valid         : FALSE if there's nothing usable to compare against;
#'                   reference/investigation are NULL in that case.
#'   reference     : list(months, n_weighted_avg, unweighted_avg,
#'                   chosen_avg, avg_type, total_responses, excluded).
#'                   excluded is a tibble of every reference-range month
#'                   that didn't count toward the average, with why.
#'                   Deliberately has no participant count: summing
#'                   per-month distinct-respondent counts across
#'                   several months can double-count anyone who
#'                   answered more than once, with no reliable way to
#'                   de-duplicate that from pre-aggregated data, so this
#'                   view only ever reports responses.
#'   investigation : one row per investigation month, with n,
#'                   weighted_prop, suppressed, suppress_reason, and
#'                   deviation_pts (percentage points vs chosen_avg, NA
#'                   if suppressed).
#' )
compute_period_comparison <- function(monthly_tables, domain_key, figure_title, grouping_label, level_name,
                                       investigation_range, reference_range = NULL, outcome_label = NULL,
                                       avg_type = c("weighted", "unweighted")) {
  avg_type <- match.arg(avg_type)
  slice <- filter_trend_data(monthly_tables, domain_key, figure_title, grouping_label, level_name)
  if (!is.null(outcome_label)) slice <- dplyr::filter(slice, outcome == outcome_label)

  if (nrow(slice) == 0) {
    return(list(valid = FALSE, error = "No data at all for this selection.", reference = NULL, investigation = NULL))
  }

  ref_range <- if (is.null(reference_range)) range(slice$date, na.rm = TRUE) else reference_range

  investigation <- slice %>% dplyr::filter(date >= investigation_range[1], date <= investigation_range[2])
  if (nrow(investigation) == 0) {
    return(list(
      valid = FALSE,
      error = "No data exists for the investigation period -- check the item was fielded then.",
      reference = NULL, investigation = NULL
    ))
  }

  reference_pool <- slice %>% dplyr::filter(date >= ref_range[1], date <= ref_range[2])
  is_overlap      <- reference_pool$date %in% investigation$date
  overlap_excluded    <- reference_pool[is_overlap, ]
  reference_pool      <- reference_pool[!is_overlap, ]

  if (nrow(reference_pool) == 0) {
    return(list(
      valid = FALSE,
      error = paste(
        "The investigation period covers the entire reference period -- there's nothing left to compare against.",
        "Pick a reference period that extends beyond the investigation period."
      ),
      reference = NULL, investigation = NULL
    ))
  }

  suppressed_excluded <- reference_pool %>% dplyr::filter(suppressed)
  reference_valid     <- reference_pool %>% dplyr::filter(!suppressed)

  if (nrow(reference_valid) == 0) {
    return(list(
      valid = FALSE,
      error = "Every month left in the reference period is suppressed for low sample size -- no reliable reference average can be computed. Try a wider reference period.",
      reference = NULL, investigation = NULL
    ))
  }

  avgs <- compute_trend_averages(reference_valid, prop_col = "weighted_prop", n_col = "n")
  chosen_avg <- if (avg_type == "weighted") avgs$n_weighted_avg else avgs$unweighted_avg

  excluded <- dplyr::bind_rows(
    overlap_excluded %>% dplyr::transmute(month, reason = "Excluded: inside the investigation period"),
    suppressed_excluded %>% dplyr::transmute(month, reason = suppress_reason)
  )

  reference <- list(
    months          = reference_valid$month,
    n_weighted_avg  = avgs$n_weighted_avg,
    unweighted_avg  = avgs$unweighted_avg,
    chosen_avg      = chosen_avg,
    avg_type        = avg_type,
    total_responses = sum(reference_valid$n, na.rm = TRUE),
    excluded        = excluded
  )

  investigation_out <- investigation %>%
    dplyr::transmute(
      month, date, n,
      weighted_prop = dplyr::if_else(suppressed, NA_real_, weighted_prop),
      suppressed, suppress_reason,
      deviation_pts = dplyr::if_else(suppressed, NA_real_, (weighted_prop - chosen_avg) * 100)
    )

  list(valid = TRUE, error = NULL, reference = reference, investigation = investigation_out)
}

#' Compare multiple (grouping, level) slices at once, e.g. every
#' selected Race category and every selected Income Level category
#' together, for one investigation period, against a single shared
#' reference line: the Overall series' average over the reference
#' period, never a per-level reference, so every subgroup bar is
#' measured against the same number. If the investigation period spans
#' more than one month, a level's bar is the n-weighted average of that
#' level's own non-suppressed months within the period, not just its
#' first or last month.
#'
#' @param levels A data frame with columns grouping, level: every
#'   (grouping, level) pair to show as a bar, in display order.
#' @param avg_type See compute_period_comparison(). Applied consistently
#'   to both the shared Overall reference line and each level's own
#'   multi-month investigation average.
#' @return list(
#'   valid               : FALSE only if the Overall reference itself
#'                         can't be computed; error explains, rest NULL.
#'   reference           : same shape as compute_period_comparison()'s.
#'   investigation_overall : the Overall series' own rows for the
#'                         investigation period.
#'   investigation_total_responses : sum of investigation_overall$n.
#'   levels              : one row per requested (grouping, level):
#'                         grouping, level, n, weighted_prop, suppressed,
#'                         suppress_reason, deviation_pts. A level with
#'                         no data, or where every month is suppressed,
#'                         is marked suppressed rather than dropped.
compute_multi_level_comparison <- function(monthly_tables, domain_key, figure_title, levels,
                                            investigation_range, reference_range = NULL, outcome_label = NULL,
                                            avg_type = c("weighted", "unweighted")) {
  avg_type <- match.arg(avg_type)
  overall <- compute_period_comparison(
    monthly_tables, domain_key, figure_title, "Overall", "Overall",
    investigation_range = investigation_range, reference_range = reference_range, outcome_label = outcome_label,
    avg_type = avg_type
  )
  if (!overall$valid) {
    return(list(
      valid = FALSE, error = overall$error, reference = NULL,
      investigation_overall = NULL, investigation_total_responses = NULL, levels = NULL
    ))
  }

  level_rows <- purrr::pmap_dfr(levels, function(grouping, level) {
    slice <- filter_trend_data(monthly_tables, domain_key, figure_title, grouping, level)
    if (!is.null(outcome_label)) slice <- dplyr::filter(slice, outcome == outcome_label)
    inv <- slice %>% dplyr::filter(date >= investigation_range[1], date <= investigation_range[2])

    if (nrow(inv) == 0) {
      return(tibble::tibble(
        grouping = grouping, level = level, n = 0L,
        weighted_prop = NA_real_, suppressed = TRUE,
        suppress_reason = "No data exists for this level in the investigation period.",
        deviation_pts = NA_real_
      ))
    }

    n_total     <- sum(inv$n, na.rm = TRUE)
    valid_rows  <- inv %>% dplyr::filter(!suppressed)

    if (nrow(valid_rows) == 0) {
      reason <- if (nrow(inv) == 1) {
        inv$suppress_reason[1]
      } else {
        sprintf(
          "Suppressed: every month in the investigation period is below the minimum sample size (combined n = %d)",
          n_total
        )
      }
      return(tibble::tibble(
        grouping = grouping, level = level, n = n_total,
        weighted_prop = NA_real_, suppressed = TRUE, suppress_reason = reason, deviation_pts = NA_real_
      ))
    }

    avgs <- compute_trend_averages(valid_rows, prop_col = "weighted_prop", n_col = "n")
    level_avg <- if (avg_type == "weighted") avgs$n_weighted_avg else avgs$unweighted_avg
    tibble::tibble(
      grouping = grouping, level = level, n = n_total,
      weighted_prop = level_avg, suppressed = FALSE, suppress_reason = "Included",
      deviation_pts = (level_avg - overall$reference$chosen_avg) * 100
    )
  })

  inv_overall <- overall$investigation

  list(
    valid = TRUE, error = NULL, reference = overall$reference,
    investigation_overall = inv_overall,
    investigation_total_responses = sum(inv_overall$n, na.rm = TRUE),
    levels = level_rows
  )
}

#' One pinned level of any grouping's panel data, ready to plot. For a
#' multi-outcome figure with no outcome_label given, each measure
#' becomes its own level so it gets its own line, since otherwise every
#' measure's rows would share one level name and blend into a single
#' meaningless line. Passing outcome_label instead pins to exactly that
#' one measure, leaving a single-line panel.
#'
#' This is what lets the dashboard's "Compare by" control focus on one
#' slice (e.g. just Race: White) when the axis being compared is
#' The display order a figure's own outcome values should appear in
#' (e.g. Food Insecurity's Overall/Moderate/Severe, Parent Distress's
#' Lower-then-Higher threshold labels), read from DOMAIN_FIGURES'
#' outcome_vars names -- the one place that order is actually defined.
#' NULL if no matching figure is found (single-outcome figures have no
#' "outcome" column to order in the first place).
figure_outcome_order <- function(domain_key, figure_title) {
  figs <- DOMAIN_FIGURES[[domain_key]]$figures
  for (fig in figs) {
    if (identical(fig$title, figure_title)) return(names(fig$outcome_vars))
  }
  NULL
}

#' Measure or Threshold rather than Grouping.
#'
#' @param grouping_label,level_name The grouping and the one level to
#'   pin to (equal to each other for "Overall", the only grouping
#'   that's always exactly one level).
#' @param geo "CA Statewide" or "LA County" -- see GEO_SCOPE_CHOICES.
prepare_pinned_level_panel_data <- function(trend_tables, domain_key, figure_title, grouping_label, level_name,
                                              outcome_label = NULL, date_range = NULL, geo = "CA Statewide") {
  out <- filter_trend_data(trend_tables, domain_key, figure_title, grouping_label, level_name,
                            date_range = date_range, geo = geo)
  if (!is.null(outcome_label)) {
    out <- dplyr::filter(out, outcome == outcome_label)
  } else if (nrow(out) > 0 && !all(is.na(out$outcome))) {
    outcome_order <- figure_outcome_order(domain_key, figure_title)
    out$level <- if (is.null(outcome_order)) out$outcome else factor(out$outcome, levels = outcome_order)
  }
  out
}

#' "Overall" is the only grouping that's always exactly one level, so
#' grouping_label doubles as the level name here.
prepare_single_level_panel_data <- function(trend_tables, domain_key, figure_title, grouping_label,
                                              outcome_label = NULL, date_range = NULL, geo = "CA Statewide") {
  prepare_pinned_level_panel_data(trend_tables, domain_key, figure_title, grouping_label, grouping_label,
                                    outcome_label = outcome_label, date_range = date_range, geo = geo)
}

#' The Overall panel's data specifically, for callers that only ever
#' want the Overall grouping (within whichever geography), as their
#' fixed reference panel.
prepare_overall_panel_data <- function(trend_tables, domain_key, figure_title, date_range = NULL, geo = "CA Statewide") {
  prepare_single_level_panel_data(trend_tables, domain_key, figure_title, "Overall", date_range = date_range, geo = geo)
}

#' A subgroup-breakdown panel's data: every level of grouping_label
#' together (deliberately not filtered to one level, since showing them
#' side by side is the point). For a multi-outcome figure this must
#' also be filtered to exactly one measure, since every measure times
#' every subgroup level at once would be unreadable.
prepare_grouping_panel_data <- function(trend_tables, domain_key, figure_title, grouping_label,
                                          outcome_label = NULL, date_range = NULL, geo = "CA Statewide") {
  out <- trend_tables %>%
    dplyr::filter(domain == domain_key, figure_title == !!figure_title, grouping == grouping_label, geo == .env$geo)
  if (!is.null(outcome_label)) {
    out <- out %>% dplyr::filter(outcome == outcome_label)
  }
  if (!is.null(date_range)) {
    out <- out %>% dplyr::filter(date >= date_range[1], date <= date_range[2])
  }
  # order_grouping_levels() gives lowest-to-highest for Income Level and
  # any other grouping with a defined display order (00_config.R),
  # alphabetical otherwise -- as an ordered factor so the plot's legend
  # and the table below it both follow the same order, not a plain
  # alphabetical sort of whatever level values happen to be present.
  out$level <- factor(out$level, levels = order_grouping_levels(unique(out$level), grouping_label))
  out %>% dplyr::arrange(level, date)
}

#' The plot caption explaining the suppression rule, plus the total
#' responses/participants when there's exactly one line to report on.
#' Folded into the chart itself instead of a separate metadata block,
#' since it's context that isn't already visible elsewhere on the chart.
#'
#' Deliberately just the suppression-threshold note; total
#' responses/participants have their own spot, the live stats table
#' under the plot, so repeating the same number in two places would
#' invite a "why don't these match" confusion.
#'
#' @param data The panel's already-filtered data. Kept as a parameter
#'   for call-site compatibility even though the caption doesn't read
#'   anything from it.
#' @param grouping_label Which grouping this panel shows (decides the
#'   50-vs-30 minimum).
#' @param series "monthly" or "rolling", which threshold to mention. The
#'   dashboard only shows one series at a time, so the caption only
#'   names that series' own threshold.
build_plot_caption <- function(data, grouping_label, series = c("monthly", "rolling")) {
  series <- match.arg(series)
  is_overall <- grouping_label == "Overall"
  is_source  <- grouping_label == "Source"
  if (series == "monthly") {
    # One month: a respondent answers at most once, so "respondents"
    # and "responses" are the same number here.
    min_n <- if (is_overall) N_MIN_OVERALL else if (is_source) N_MIN_SOURCE else N_MIN_SUBGROUP
    sprintf("Months with fewer than %d respondents are excluded.", min_n)
  } else {
    # 3 months: rolling_n sums each month's own response count
    # (calc_rolling_avg(), R/04_summarize.R), so this total can include
    # the same person more than once if they answered in more than one
    # of the 3 months -- "responses," not "respondents".
    rolling_min_n <- if (is_overall) ROLLING_MIN_OVERALL else if (is_source) ROLLING_MIN_SOURCE else ROLLING_MIN_SUBGROUP
    sprintf("Shown only when the 3-month cumulative total is at least %d responses.", rolling_min_n)
  }
}
