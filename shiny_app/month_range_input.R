# month_range_input.R
#
# A Year + Month picker for choosing a month or month range. Every date
# in this app is really just a month, so there's no day to pick here at
# all. That avoids a real bug: a day-level calendar let someone pick a
# non-1st day as a start date and silently drop that whole month.

library(shiny)

# -----------------------------------------------------------------------
# ONE month (Year dropdown + Month dropdown)
# -----------------------------------------------------------------------

#' @param id Namespace id, unique within the UI tree.
#' @param min_date,max_date Full selectable range. Only the year matters.
#' @param default Initially selected month.
#' @param label Optional label shown above the dropdowns.
monthPickerUI <- function(id, min_date, max_date, default, label = NULL) {
  ns <- NS(id)
  years <- seq(as.integer(format(as.Date(min_date), "%Y")), as.integer(format(as.Date(max_date), "%Y")))
  default <- as.Date(default)
  tagList(
    if (!is.null(label)) tags$div(style = "font-size:12px; color:#666; margin-bottom:2px;", label),
    fluidRow(
      column(6, selectInput(ns("year"), "Year", choices = years,
                             selected = as.integer(format(default, "%Y")))),
      column(6, selectInput(ns("month"), "Month", choices = stats::setNames(1:12, month.name),
                             selected = as.integer(format(default, "%m"))))
    )
  )
}

#' Server for monthPickerUI(). Returns a reactive Date, always the 1st
#' of the selected month.
#' @param sync_to Optional reactive Date. When it changes, the picker's
#'   own Year/Month selects update to match, e.g. so a preset like
#'   "Latest Month" can move the picker. Pass NULL for no syncing.
monthPickerServer <- function(id, sync_to = NULL) {
  moduleServer(id, function(input, output, session) {
    if (!is.null(sync_to)) {
      observeEvent(sync_to(), {
        d <- sync_to()
        updateSelectInput(session, "year",  selected = as.integer(format(d, "%Y")))
        updateSelectInput(session, "month", selected = as.integer(format(d, "%m")))
      })
    }
    reactive({
      req(input$year, input$month)
      as.Date(sprintf("%s-%02d-01", input$year, as.integer(input$month)))
    })
  })
}

# -----------------------------------------------------------------------
# A MONTH RANGE (Start Year/Month + End Year/Month)
# -----------------------------------------------------------------------

#' @param default_start,default_end The initially-selected start/end months.
monthRangeInputUI <- function(id, min_date, max_date, default_start, default_end) {
  ns <- NS(id)
  tagList(
    monthPickerUI(ns("start"), min_date, max_date, default_start, label = "Start"),
    monthPickerUI(ns("end"),   min_date, max_date, default_end,   label = "End")
  )
}

#' Server for monthRangeInputUI(). Returns a reactive c(start, end) Date
#' vector, both always the 1st of their month.
#' @param sync_to Optional reactive c(start, end) Date vector, applied
#'   independently to the start and end pickers.
monthRangeInputServer <- function(id, sync_to = NULL) {
  moduleServer(id, function(input, output, session) {
    start_sync <- if (!is.null(sync_to)) reactive(sync_to()[1]) else NULL
    end_sync   <- if (!is.null(sync_to)) reactive(sync_to()[2]) else NULL
    start_date <- monthPickerServer("start", sync_to = start_sync)
    end_date   <- monthPickerServer("end",   sync_to = end_sync)
    reactive(c(start_date(), end_date()))
  })
}
