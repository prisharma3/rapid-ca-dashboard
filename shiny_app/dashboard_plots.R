# dashboard_plots.R
#
# The dashboard's interactive (plotly) trend-line figure, with hover
# tooltips, zoom, a range slider, and built-in PNG/SVG export.
#
# One plot, for one series (Monthly or Rolling, never both at once). A
# single level draws one line with a confidence band and no legend. Multiple
# levels draw one line per level in a consistent color, no band, and a
# legend above the plot.
#
# Plots are registered for "plotly_relayout" events (drag, zoom,
# double-click reset) under a caller-supplied source id. Paired with
# build_window_stats_ui() below and app.R's record_visible_range(),
# this lets the dashboard show live averages and response/participant
# counts for whatever window is currently in view. See app.R for how
# the two are wired together.

library(dplyr)
library(plotly)
library(htmltools)


#' Inserts an explicit NA row for any calendar month missing between
#' lvl_data's own first and last date -- a real fielding gap (e.g. a
#' Source not fielded for a few months), as opposed to simply not
#' having started yet or already having ended, which isn't a gap and
#' shouldn't get one manufactured at either edge.
#'
#' plotly's line traces default to connectgaps = FALSE: an explicit
#' NA/null value breaks the line visibly right there. Without a row at
#' all for the missing month (which is what a genuine fielding gap
#' looks like upstream -- no respondents that month means no row, not
#' a suppressed-but-present one), plotly has nothing to break on and
#' instead draws one continuous straight line from the last real point
#' straight to the next one, months later -- visually indistinguishable
#' from an actual gradual trend, even though there's no data there at
#' all.
#' @param lvl_data One level's already date-filtered, date-sorted rows
#'   (level, date, est, low, high, n).
fill_date_gaps <- function(lvl_data) {
  if (nrow(lvl_data) < 2) return(lvl_data)
  full_months <- seq(floor_to_month_start(min(lvl_data$date)), floor_to_month_start(max(lvl_data$date)), by = "1 month")
  missing_months <- full_months[!full_months %in% lvl_data$date]
  if (length(missing_months) == 0) return(lvl_data)

  gap_rows <- tibble::tibble(
    level = lvl_data$level[[1]], date = missing_months,
    est = NA_real_, low = NA_real_, high = NA_real_, n = NA_integer_
  )
  dplyr::bind_rows(lvl_data, gap_rows) %>% dplyr::arrange(date)
}

#' The HTML title+caption markup a plot's title uses. Factored out so
#' app.R's live window-caption updater can push a fresh title into an
#' already-rendered plot with the same markup.
plot_title_html <- function(title, caption) {
  paste0(
    "<b>", title, "</b><br>",
    "<span style=\"font-size:12px; color:#666;\">", caption, "</span>"
  )
}

#' Build one interactive figure for one series (Monthly or Rolling).
#'
#' @param data One row per (level, month): needs level, date, n, and
#'   either weighted_prop/weighted_ci_low/weighted_ci_high (series =
#'   "monthly") or rolling_weighted_prop/rolling_weighted_ci_low/
#'   rolling_weighted_ci_high/rolling_n (series = "rolling").
#' @param series "monthly" or "rolling".
#' @param show_ci Draw the confidence band. Only drawn when data has
#'   exactly one level, since overlapping bands across several subgroup
#'   lines would be unreadable.
#' @param title Plot title.
#' @param caption Small text shown under the title.
#' @param source_id Unique id for plotly::event_data("plotly_relayout",
#'   source = source_id). Give each plot on a page a different id or
#'   their events overwrite each other. NULL disables event registration.
build_interactive_trend_plot <- function(data, series = c("monthly", "rolling"), show_ci = FALSE,
                                           title = "", caption = "", source_id = NULL) {
  series <- match.arg(series)

  # Even with nothing to plot, a source_id must still get registered
  # for "plotly_relayout" -- otherwise any later poll of that source
  # (window_stats_ui()/update_plot_for_window()) warns "event ... is
  # not registered" on every reactive flush, since event_data()'s
  # check runs in a deferred onFlushed() callback a suppressWarnings()
  # around the poll itself can't reach.
  empty_registered_plot <- function() {
    if (is.null(source_id)) return(NULL)
    plotly::plot_ly(source = source_id) %>% plotly::event_register("plotly_relayout")
  }

  if (nrow(data) == 0) return(empty_registered_plot())

  est_col  <- if (series == "monthly") "weighted_prop"          else "rolling_weighted_prop"
  low_col  <- if (series == "monthly") "weighted_ci_low"        else "rolling_weighted_ci_low"
  high_col <- if (series == "monthly") "weighted_ci_high"       else "rolling_weighted_ci_high"
  n_col    <- if (series == "monthly") "n"                      else "rolling_n"
  series_label <- if (series == "monthly") "Monthly" else "Rolling"

  plot_data <- data %>%
    dplyr::transmute(level, date, est = .data[[est_col]], low = .data[[low_col]],
                      high = .data[[high_col]], n = .data[[n_col]]) %>%
    dplyr::filter(!is.na(est))
  if (nrow(plot_data) == 0) return(empty_registered_plot())

  # Respects the caller's own intended order when `level` arrives as an
  # ordered factor (e.g. income bands lowest to highest, or Food
  # Insecurity's Overall/Moderate/Severe) instead of always sorting
  # alphabetically, which would otherwise scramble either of those.
  levels_present <- if (is.factor(plot_data$level)) {
    levels(droplevels(plot_data$level))
  } else {
    sort(unique(plot_data$level))
  }
  single_level   <- length(levels_present) == 1
  draw_ci         <- show_ci && single_level
  color_map <- if (single_level) {
    stats::setNames(unname(TREND_SERIES_COLORS[[series_label]]), levels_present)
  } else {
    build_level_color_map(levels_present)
  }

  p <- if (is.null(source_id)) plotly::plot_ly() else plotly::plot_ly(source = source_id)

  for (lvl in levels_present) {
    lvl_data <- plot_data %>% dplyr::filter(level == lvl) %>% dplyr::arrange(date) %>% fill_date_gaps()
    color <- unname(color_map[lvl])

    if (draw_ci) {
      p <- p %>% plotly::add_ribbons(
        data = lvl_data, x = ~date, ymin = ~low * 100, ymax = ~high * 100,
        line = list(width = 0), fillcolor = color, opacity = 0.15,
        showlegend = FALSE, hoverinfo = "skip", name = paste(lvl, "95% CI")
      )
    }

    # Built as a plain vector here, not a plotly formula, because a
    # formula referencing `lvl` would evaluate lazily and every trace
    # would end up showing the loop's final value instead of its own.
    # NA for a fill_date_gaps()-inserted gap row -- no marker gets drawn
    # there anyway (no y value to plot), but this keeps its hover text
    # from reading "NA%" on the off chance it's ever reachable.
    hover_text <- ifelse(
      is.na(lvl_data$est), NA_character_,
      sprintf("%s<br>%s: %.1f%%<br>n = %d", lvl, format(lvl_data$date, "%B %Y"), lvl_data$est * 100, lvl_data$n)
    )

    p <- p %>% plotly::add_trace(
      data = lvl_data, x = ~date, y = ~est * 100, type = "scatter", mode = "lines+markers",
      name = lvl, showlegend = !single_level, connectgaps = FALSE,
      line = list(color = color, width = 2), marker = list(color = color, size = 6),
      text = hover_text,
      hoverinfo = "text"
    )
  }

  p <- p %>%
    plotly::layout(
      title = list(
        text = plot_title_html(title, caption),
        font = list(size = 14), x = 0, xanchor = "left", y = 0.98, yanchor = "top",
        pad = list(t = 8, b = 8)
      ),
      xaxis = list(title = "", rangeslider = list(visible = TRUE, thickness = 0.08), type = "date"),
      yaxis = list(title = "Percentage (%)", rangemode = "tozero"),
      # Horizontal legend above the plot, not to the right, so the plot
      # keeps its full width. Plotly wraps it onto more rows on its own
      # when there are too many levels for one line.
      #
      # legend.y uses plotly's default frame (relative to the data area,
      # where 1.0 is its top edge), not yref = "container" -- the
      # bundled plotly.js doesn't support legend.yref, so that silently
      # falls back to sitting inside the plot. A y above 1 reliably
      # pushes the legend into the margin instead.
      legend = list(orientation = "h", x = 0, xanchor = "left", y = 1.2, yanchor = "top", font = list(size = 11)),
      hovermode = "closest",
      # Top margin sized for the common case: title + caption, plus a
      # legend that usually wraps to 1-2 rows.
      margin = list(t = if (single_level) 95 else 130, b = 40, r = 40, l = 55),
      autosize = TRUE
    ) %>%
    plotly::config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d"))

  if (!is.null(source_id)) p <- p %>% plotly::event_register("plotly_relayout")
  p
}


#' Round a date down to the 1st of its own calendar month.
floor_to_month_start <- function(d) as.Date(format(d, "%Y-%m-01"))

#' Round a date up to the last day of its own calendar month.
ceiling_to_month_end <- function(d) seq(floor_to_month_start(d), by = "1 month", length.out = 2)[2] - 1

#' Snap a c(min_date, max_date) to whole calendar months. A mouse drag
#' operates at day-level resolution, but every row's date sits on the 1st
#' of its month, so without snapping a drag landing mid-month would
#' silently exclude that month from any downstream calculation.
snap_to_full_months <- function(date_range) {
  c(floor_to_month_start(date_range[1]), ceiling_to_month_end(date_range[2]))
}

#' Extract the raw, unsnapped c(min_date, max_date) a "plotly_relayout"
#' event payload represents. Returns NULL if the payload can't be parsed
#' or represents an autorange reset.
parse_relayout_range <- function(relayout_event) {
  if (is.null(relayout_event)) return(NULL)
  if (isTRUE(relayout_event[["xaxis.autorange"]])) return(NULL)

  lo <- relayout_event[["xaxis.range[0]"]]
  hi <- relayout_event[["xaxis.range[1]"]]
  if (is.null(lo) || is.null(hi)) {
    rng <- relayout_event[["xaxis.range"]]
    if (is.null(rng) || length(rng) < 2) return(NULL)
    lo <- rng[[1]]; hi <- rng[[2]]
  }

  parsed <- suppressWarnings(as.Date(substr(as.character(c(lo, hi)), 1, 10)))
  if (anyNA(parsed) || parsed[1] > parsed[2]) return(NULL)
  parsed
}

#' Render compute_window_stats()'s output as a small HTML stats panel:
#' one row per level, with the currently visible window's averages,
#' responses, and participants, so these update as the user drags the
#' plot's range slider.
#' @param stats compute_window_stats()'s output.
#' @param date_range The window it was computed over.
build_window_stats_ui <- function(stats, date_range) {
  if (is.null(stats) || nrow(stats) == 0) {
    return(htmltools::tags$div(class = "text-muted", style = "font-size:12px;",
                                "No months with data in the selected window."))
  }

  window_label <- sprintf("%s – %s", format(date_range[1], "%b %Y"), format(date_range[2], "%b %Y"))
  n_months <- stats$n_months[1]
  # Every row shares the same window, so this is one value in practice,
  # but read defensively (all()) in case that ever stops being true.
  is_exact <- isTRUE(all(stats$is_exact_participants))

  cell_style <- "border: 1px solid #ddd; padding: 4px 10px;"
  header_cell_style <- paste(cell_style, "background-color: #f5f5f5; text-align:left; color:#555; font-weight:600;")

  rows <- lapply(seq_len(nrow(stats)), function(i) {
    s <- stats[i, ]
    htmltools::tags$tr(
      htmltools::tags$td(s$level, style = paste(cell_style, "font-weight:600;")),
      htmltools::tags$td(sprintf("%.1f%%", s$n_weighted_avg * 100), style = cell_style),
      htmltools::tags$td(sprintf("%.1f%%", s$unweighted_avg * 100), style = cell_style),
      htmltools::tags$td(format(s$total_responses, big.mark = ","), style = cell_style),
      htmltools::tags$td(format(s$total_participants, big.mark = ","), style = cell_style)
    )
  })

  # Only worth a loud warning when it's actually approximate -- an exact
  # count gets a quiet, affirmative note instead of competing for
  # attention with the one case that actually matters to flag.
  participants_note <- if (is_exact) {
    htmltools::tags$div(
      style = "font-size:12px; color:#2E7D32; margin-top:2px; max-width:320px; white-space:normal;",
      "Participants is an exact count for this full range."
    )
  } else {
    htmltools::tags$div(
      style = "font-size:12px; color:#a33; margin-top:2px; max-width:320px; white-space:normal; font-weight:600;",
      "Participants is approximate here: it sums each month's own distinct-respondent count, which can double-count anyone who answered in more than one month of this custom window."
    )
  }

  htmltools::tags$div(
    style = "font-size:12px; max-width:100%; overflow-x:auto;",
    htmltools::tags$div(
      style = "color:#666; margin-bottom:4px;",
      sprintf("Window: %s (%d month%s)", window_label, n_months, if (n_months == 1) "" else "s")
    ),
    htmltools::tags$table(
      style = "border-collapse: collapse; border: 1px solid #ddd; max-width:100%;",
      htmltools::tags$thead(htmltools::tags$tr(
        htmltools::tags$th("", style = header_cell_style),
        htmltools::tags$th("N-wtd avg", style = header_cell_style),
        htmltools::tags$th("Unwtd avg", style = header_cell_style),
        htmltools::tags$th("Responses", style = header_cell_style),
        htmltools::tags$th("Participants", style = header_cell_style)
      )),
      htmltools::tags$tbody(rows)
    ),
    participants_note
  )
}


#' The Investigate tab's deviation-bar plot: a shared reference line with
#' one bar per selected demographic level, showing how far that level's
#' rate differs from the shared reference average. Bars are colored by
#' grouping, not by level or direction, so every level of one grouping
#' reads as one color family. A level with no reliable data in the
#' investigation period draws as a zero-length bar at the reference line,
#' labeled "Suppressed".
#'
#' @param comparison compute_multi_level_comparison()'s output. Call this
#'   only when comparison$valid is TRUE.
build_multi_level_comparison_plot <- function(comparison, title = "", caption = "") {
  lvl <- comparison$levels
  ref <- comparison$reference
  if (is.null(lvl) || nrow(lvl) == 0) return(NULL)

  # "Overall" first, since match() returns NA for it otherwise (it isn't
  # one of GROUPING_VARS/"LA County"), and order() sorts NA last, burying
  # the bar most directly comparable to the reference line. Within a
  # grouping, order by that grouping's own semantic order (e.g. Income
  # Level low-to-high) instead of plain alphabetical, which would bury
  # e.g. "$100,000 or more" before "$25,000-$49,999".
  grouping_order <- c("Overall", names(GROUPING_VARS), "LA County")
  lvl$level_rank <- NA_integer_
  for (g in unique(lvl$grouping)) {
    idx <- lvl$grouping == g
    ordered_levels <- order_grouping_levels(unique(lvl$level[idx]), g)
    lvl$level_rank[idx] <- match(lvl$level[idx], ordered_levels)
  }
  lvl <- lvl[order(match(lvl$grouping, grouping_order), lvl$level_rank), ]
  lvl$axis_label <- sprintf("%s (n=%s)", lvl$level, format(lvl$n, big.mark = ",", trim = TRUE))
  # rev() so plotly's bottom-up categorical axis still reads top-to-bottom.
  lvl$label <- factor(lvl$axis_label, levels = rev(lvl$axis_label))

  ref_pct <- ref$chosen_avg * 100
  axis_title <- if (identical(ref$avg_type, "unweighted")) "Unweighted %" else "Weighted %"
  lvl$pct <- lvl$weighted_prop * 100
  lvl$color <- unname(GROUPING_COLORS[lvl$grouping])
  lvl$color[is.na(lvl$color)] <- "#898781"

  lvl$bar_label <- ifelse(
    lvl$suppressed,
    "Suppressed",
    sprintf("%.1f%% (%+.1f pts)", lvl$pct, lvl$deviation_pts)
  )
  lvl$bar_base  <- ref_pct
  lvl$bar_value <- ifelse(lvl$suppressed, ref_pct, lvl$pct)
  lvl$bar_delta <- lvl$bar_value - lvl$bar_base

  # The reference line stays the horizontal center of the plot. The
  # x-axis range is symmetric around it and adjusts to the data.
  max_dev <- max(abs(c(lvl$bar_value, ref_pct) - ref_pct), na.rm = TRUE)
  if (!is.finite(max_dev) || max_dev == 0) max_dev <- 5
  pad <- max_dev * 1.35
  x_range <- c(ref_pct - pad, ref_pct + pad)

  # One trace per grouping so the legend names each grouping's color.
  # No hover tooltip: the bar label text already shows everything a
  # hover bubble would repeat, and a hover delays showing it until the
  # mouse moves over the bar.
  add_grouping_trace <- function(p, g) {
    sub <- lvl[lvl$grouping == g, ]
    color <- unname(GROUPING_COLORS[g])
    if (is.na(color)) color <- "#898781"
    plotly::add_trace(
      p, data = sub, y = ~label, x = ~bar_delta, base = ~bar_base,
      type = "bar", orientation = "h", name = g,
      marker = list(color = color, line = list(color = RAPID_COLORS$white, width = 1)),
      text = ~bar_label, textposition = "outside", cliponaxis = FALSE,
      textfont = list(size = 13, color = RAPID_COLORS$soft_black, family = RAPID_FONT_FAMILY),
      hoverinfo = "skip"
    )
  }

  groupings_present <- unique(lvl$grouping)
  traces <- plotly::plot_ly()
  for (g in groupings_present) traces <- add_grouping_trace(traces, g)

  traces %>%
    plotly::layout(
      title = list(text = plot_title_html(title, caption), x = 0, xanchor = "left",
                   font = list(family = RAPID_FONT_FAMILY)),
      xaxis = list(title = axis_title, ticksuffix = "%", zeroline = FALSE, range = x_range,
                   tickfont = list(size = 13)),
      yaxis = list(title = "", categoryorder = "array", categoryarray = rev(lvl$axis_label),
                   tickfont = list(size = 13)),
      barmode = "overlay",
      bargap = 0.35,
      shapes = list(list(
        type = "line", x0 = ref_pct, x1 = ref_pct, y0 = -0.5, y1 = nrow(lvl) - 0.5, yref = "y",
        line = list(color = RAPID_COLORS$soft_black, width = 2, dash = "dash")
      )),
      annotations = list(list(
        x = ref_pct, xref = "x", y = 1, yref = "paper", xanchor = "center", yanchor = "bottom",
        showarrow = FALSE, text = sprintf("Reference: %.1f%%", ref_pct),
        font = list(size = 13, color = RAPID_COLORS$white, family = RAPID_FONT_FAMILY),
        bgcolor = RAPID_COLORS$soft_black, bordercolor = RAPID_COLORS$soft_black,
        borderpad = 6, borderwidth = 1
      )),
      margin = list(l = 190, t = 110, b = 50, r = 70),
      font = list(family = RAPID_FONT_FAMILY),
      plot_bgcolor = RAPID_COLORS$white, paper_bgcolor = RAPID_COLORS$white,
      legend = list(orientation = "h", x = 0, xanchor = "left", y = 1.12, yanchor = "bottom",
                    font = list(size = 13))
    ) %>%
    plotly::config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d"))
}


#' Three plain-text notes under the Investigate plot: which reference
#' months were excluded and why, which months counted toward the
#' reference average, and the investigation period itself. Responses
#' only, never participants, since summing per-month distinct-respondent
#' counts across months can double-count anyone who answered more than
#' once, and there's no reliable way to de-duplicate that here.
build_investigate_notes_ui <- function(comparison) {
  if (!isTRUE(comparison$valid)) {
    return(htmltools::tags$div(
      style = "color:#a33; font-size:13px; border:1px solid #e3b3b3; background:#fdf2f2; padding:8px 12px; border-radius:4px;",
      comparison$error
    ))
  }
  ref <- comparison$reference

  excluded_text <- if (nrow(ref$excluded) == 0) {
    "None."
  } else {
    paste(sprintf("%s (%s)", ref$excluded$month, ref$excluded$reason), collapse = "; ")
  }

  htmltools::tags$div(
    style = "font-size:12px; line-height:1.7;",
    htmltools::tags$div(htmltools::tags$b("Months excluded from reference: "), excluded_text),
    htmltools::tags$div(
      htmltools::tags$b("Months included in reference: "),
      sprintf(
        "%d month%s (%s) -- %s responses",
        length(ref$months), if (length(ref$months) == 1) "" else "s", paste(ref$months, collapse = ", "),
        format(ref$total_responses, big.mark = ",")
      )
    ),
    htmltools::tags$div(
      htmltools::tags$b("Investigation period: "),
      sprintf(
        "%d month%s (%s) -- %s responses",
        nrow(comparison$investigation_overall), if (nrow(comparison$investigation_overall) == 1) "" else "s",
        paste(comparison$investigation_overall$month, collapse = ", "),
        format(comparison$investigation_total_responses, big.mark = ",")
      )
    )
  )
}


#' The Sample Description tab's FPL distribution chart: a plain bar chart
#' of compute_fpl_histogram()'s output, one bar per bin in ascending
#' order.
#' @param histogram compute_fpl_histogram()'s output.
#' @param title Plot title.
build_fpl_histogram_plot <- function(histogram, title = "FPL Distribution", caption = "") {
  if (nrow(histogram) == 0) return(NULL)

  histogram <- histogram %>% dplyr::arrange(bin_low)
  histogram$label <- factor(histogram$label, levels = histogram$label)

  plotly::plot_ly(
    histogram, x = ~label, y = ~pct, type = "bar",
    marker = list(color = RAPID_COLORS$yellow, line = list(color = RAPID_COLORS$soft_black, width = 1)),
    text = ~sprintf("%s: %.1f%% (n=%s responses)", label, pct, format(n, big.mark = ",")),
    # text above is for the hover tooltip only (via hovertemplate below);
    # without textposition = "none" plotly also draws it on each bar.
    textposition = "none",
    hovertemplate = "%{text}<extra></extra>"
  ) %>%
    plotly::layout(
      title = list(text = plot_title_html(title, caption), x = 0, xanchor = "left", font = list(family = RAPID_FONT_FAMILY)),
      xaxis = list(title = "", tickangle = -30, categoryorder = "array", categoryarray = levels(histogram$label)),
      yaxis = list(title = "% of responses", ticksuffix = "%"),
      font = list(family = RAPID_FONT_FAMILY),
      plot_bgcolor = RAPID_COLORS$white, paper_bgcolor = RAPID_COLORS$white,
      margin = list(t = if (nzchar(caption)) 80 else 60, b = 90)
    ) %>%
    plotly::config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d"))
}
