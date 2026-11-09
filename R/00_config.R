# 00_config.R

# Every path, threshold, and setting this pipeline uses lives here.
# Change a value in this file, not wherever it happens to get read.

# CPU cores for the parallel steps (raking per month, building every
# trend table). Not set to all available cores. Each extra core forks
# the whole R session, and this pipeline's data can get wide enough
# that forking across many cores runs slower than running sequentially
# and can exhaust memory. 2 cores measured faster than both sequential
# and a high core count on a real run. Override with the
# PIPELINE_CORES_OVERRIDE env var if you want to try something higher,
# but time it first on a small sample before trusting it on a full run.
PIPELINE_CORES <- as.integer(Sys.getenv("PIPELINE_CORES_OVERRIDE", unset = "2"))

# 1. INPUT FILE PATHS

# These point to files outside this repository: restricted survey data,
# or reference files too large or sensitive to commit. Set the real
# path as an environment variable so it never ends up in git.

# One restricted .sav file per year. {year} is filled in for each year
# in RAPID_CA_MASTERFILE_YEARS. Each yearly file needs the original raw
# columns plus the imputed _imp columns (RaceGroup_imp, region_imp, the
# income and household-size _imp families). Imputation itself runs
# outside this project.
RAPID_CA_MASTERFILE_PATH_TEMPLATE <- Sys.getenv(
  "RAPID_CA_MASTERFILE_PATH_TEMPLATE",
  unset = "PATH_NOT_SET_CA.MasterFile_groupings_{year}.sav"
)
RAPID_CA_MASTERFILE_YEARS <- 2022:2026

# CountyMerged to region mapping (58 CA counties). Lets this pipeline
# re-derive region on its own and check it against the imputation
# team's numbers, instead of only trusting the delivered region_imp
# column. Not respondent-level data, just a small reference table.
COUNTY_REGION_LOOKUP_PATH <- Sys.getenv(
  "COUNTY_REGION_LOOKUP_PATH",
  unset = "PATH_NOT_SET_county_region_lookup.xlsx"
)

# 2. OUTPUT PATHS

# Where the offline pipeline driver writes the small, already
# aggregated tables the dashboard reads.
PIPELINE_OUTPUT_DIR <- "data_pipeline_output"

EXCEL_OUTPUT_DIR <- file.path(PIPELINE_OUTPUT_DIR, "Excel_Tables")

# Internal-only QC material
INTERNAL_QC_OUTPUT_DIR <- "internal_qc_output"

# Respondent-level snapshots of master_core are the same sensitivity as
# the real masterfile, so point this somewhere outside the repo
# entirely if you plan to keep any. Defaults to INTERNAL_QC_OUTPUT_DIR
# (already gitignored) if MASTER_CORE_SNAPSHOT_DIR isn't set. Each run
# still gets its own timestamped subfolder, so re-running never
# overwrites a previous snapshot.
MASTER_CORE_SNAPSHOT_DIR <- Sys.getenv("MASTER_CORE_SNAPSHOT_DIR", unset = INTERNAL_QC_OUTPUT_DIR)

# 3. MINIMUM SAMPLE SIZE (SUPPRESSION) THRESHOLDS

# A month's estimate only shows or exports if it clears these raw
# respondent counts: N_MIN_OVERALL for the CA-wide number,
# N_MIN_SUBGROUP for any one subgroup breakdown.
N_MIN_OVERALL  <- 50
N_MIN_SUBGROUP <- 30

# The rolling average is always the fixed trailing 3 calendar months.
# It never reaches back further to find 3 real months if one is thin
# or missing. Instead a separate, higher threshold applies to the
# cumulative sample size across those exact 3 months. If that sum
# doesn't clear this bar the rolling estimate is suppressed too, even
# if every individual month passed its own 50/30 threshold.
ROLLING_MIN_OVERALL  <- 100
ROLLING_MIN_SUBGROUP <- 90

# 4. ROLLING AVERAGE WINDOW

# A trailing window: the value shown for month T averages months
# {T, T-1, T-2}.
ROLLING_WINDOW_MONTHS <- 3

# Set to ROLLING_WINDOW_MONTHS instead of 1 if a rolling value should
# only appear once a true 3-month window exists.
ROLLING_MIN_MONTHS_IN_WINDOW <- 1

# Bootstrap replicates behind every rolling margin of error this
# pipeline reports. The same respondent can answer in more than one of
# a rolling window's 3 months, so those rows aren't independent of
# each other. Resampling whole CALIDs instead of rows accounts for
# that correctly, where a plain row-level formula would not. Higher
# n_boot gives a more stable estimate at a roughly linear cost in
# runtime, and this runs once per month/group/outcome row that has a
# real 3-month window, so a wide domain can mean thousands of bootstrap
# calls in one run. 
ROLLING_BOOTSTRAP_N <- 100

# 5. WEIGHTING (RAKING) SETTINGS

# Raking margins are Race, Poverty/income level, and CA Region, fitted
# jointly. No respondent's weight may exceed RAKING_WEIGHT_CAP, so one
# person can't dominate a monthly estimate.
RAKING_WEIGHT_CAP <- 5

# If a month's raking fails to converge, or has too few eligible
# respondents, it pools with prior months (up to a 3-month window)
# before giving up.
RAKING_MIN_N_BEFORE_POOLING <- 50

# The weight column name added to master_core. Update here if the
# scheme ever changes.
RAKE_WEIGHT_COL <- "rake_weight_rpr_cap5"

# California ACS population targets raking corrects toward, for each
# of the 3 margins.If a new ACS vintage comes out, recompute the marginals and
# paste the updated numbers in below.
ACS_TARGETS_RPR <- list(
  race_group_5cat = c(
    White                                   = 0.299971,
    Black                                   = 0.050653,
    Latinx                                  = 0.447785,
    `Asian/Native Hawaiian/Pacific Islander` = 0.153688,
    `Other racial and ethnic groups`        = 0.047902
  ),
  poverty_cat = c(
    `At or Below 100% FPL` = 0.124327,
    `101-200% FPL`         = 0.184157,
    `201-400% FPL`         = 0.285396,
    `Above 400% FPL`       = 0.406120
  ),
  ca_region = c(
    `Urban North` = 0.265641,
    `Urban South` = 0.557428,
    Suburban      = 0.159728,
    Rural         = 0.017203
  )
)

# 6. GROUPING VARIABLES

# The confirmed demographic breakdowns this pipeline reports.
# Race, CA Region, and Income Level come from imputed-and-LOCF'd columns. 
# The other three have no imputed tier and come from LOCF-only columns.
GROUPING_VARS <- list(
  "Race"                             = "race_group_4cat",
  "Income Level"                     = "poverty_cat",
  "CA Region"                        = "ca_region",
  "Household Child Age"              = "hh_child_age_mix",
  "Child Disability"                 = "disability_locf",
  "Dual/Non-Dual Parent Household"   = "single_parent_locf"
)

# Display order for a sample-description table's Category column.
# Overall first, then each grouping above, then Source last.
DEFAULT_SAMPLE_DESCRIPTION_CATEGORY_ORDER <- c("Overall", names(GROUPING_VARS), "Source")

# The 4 possible values of poverty_cat. Kept here, not in 01_prep.R, so
# the dashboard and report can use this order without sourcing prep
# logic they don't need.
POVERTY_CAT_LEVELS <- c(
  "At or Below 100% FPL", "101-200% FPL", "201-400% FPL", "Above 400% FPL"
)

# The 2 possible values of poverty_200_cat. Exactly 200% FPL falls in
# the lower group, matching the 4-level convention above.
POVERTY_200_CAT_LEVELS <- c("At or Below 200% FPL", "Above 200% FPL")

# Display order for grouping levels that aren't naturally alphabetical.
# Income Level's FPL bands sort wrong alphabetically (the lowest band
# ends up last), so its order is spelled out here. Any grouping not
# listed falls back to a plain alphabetical sort.
GROUPING_LEVEL_ORDER <- list(
  "Income Level" = POVERTY_CAT_LEVELS
)

#' Order a grouping's level values for display.
#' @param levels_present The distinct level values actually present.
#' @param grouping_label Which grouping these levels belong to.
#' @return levels_present in GROUPING_LEVEL_ORDER's order if one is
#'   defined, otherwise a plain alphabetical sort.
order_grouping_levels <- function(levels_present, grouping_label) {
  canonical <- GROUPING_LEVEL_ORDER[[grouping_label]]
  if (is.null(canonical)) return(sort(levels_present))
  canonical[canonical %in% levels_present]
}

# 7. RAPID-CA BRANDING CONSTANTS
RAPID_COLORS <- list(
  yellow      = "#FEE11A",  # primary brand color, single-series charts and accents
  light_fog   = "#F4F4F4",
  white       = "#FFFFFF",
  gray        = "#464646",
  soft_black  = "#2E2D29",  # primary text color on light/yellow backgrounds
  warm_red    = "#F4795B",  # secondary, use sparingly
  purple      = "#D7AEFF",  # secondary
  sky_blue    = "#67AFD2"   # secondary
)

RAPID_FONT_FAMILY <- "Manrope"

# 8. SHARED TREND-PLOT COLOR SYSTEM
# Used by both the static report figures and the interactive dashboard

TREND_SERIES_COLORS <- c(Monthly = RAPID_COLORS$soft_black, Rolling = RAPID_COLORS$soft_black)

LEVEL_COLOR_PALETTE <- c(
  RAPID_COLORS$sky_blue, RAPID_COLORS$warm_red,
  RAPID_COLORS$purple, RAPID_COLORS$gray, "#2E7D32", "#8D6E63", "#5C6BC0"
)

# Specific level labels that always get the same color, regardless of
# where alphabetical order would otherwise put them. Each pair/trio
# below is checked for color-vision-deficiency safety before use.
FIXED_LEVEL_COLORS <- c(
  # Parent Distress's two thresholds: higher severity always near-black,
  # lower always mid-gray.
  "Very frequent"              = "#1A1A1A",
  "At least somewhat frequent" = "#999999",
  # Child Behavior's own wording, same color pair.
  "Often or very true"      = "#1A1A1A",
  "At least sometimes true" = "#999999",
  # Food Insecurity's 3 severity levels. "Overall" uses the same mid-gray
  # already validated above for Parent Distress/Child Behavior's lower threshold
  "Overall"  = "#999999",
  "Moderate" = "#808080",
  "Severe"   = "#262626",
  # Child Care's 3 types: grayscale on purpose, light to dark.
  "Parent-Only Care"      = "#CFCFCF",
  "Center-Based Care"     = "#808080",
  "Non-Center-Based Care" = "#262626",
  # Material Hardship's sources.
  "Food"       = RAPID_COLORS$warm_red,
  "Housing"    = RAPID_COLORS$purple,
  "Utility"    = RAPID_COLORS$sky_blue,
  "Healthcare" = "#2E7D32"
)

#' @param levels Character vector of level names to color.
#' @return A named vector, level to hex color. A level with a
#'   FIXED_LEVEL_COLORS entry always gets that color. Every other level
#'   cycles through LEVEL_COLOR_PALETTE by its sorted position among
#'   the remaining levels.
build_level_color_map <- function(levels) {
  sorted_levels <- sort(unique(levels))
  fixed <- FIXED_LEVEL_COLORS[sorted_levels]
  is_fixed <- !is.na(fixed)

  remaining <- sorted_levels[!is_fixed]
  if (length(remaining) > 0) {
    # Skip any palette color already claimed by a fixed level in this
    # set, so a fallback level can't land on the same color as one that
    # was pinned on purpose.
    available_palette <- setdiff(LEVEL_COLOR_PALETTE, unname(fixed[is_fixed]))
    if (length(available_palette) == 0) available_palette <- LEVEL_COLOR_PALETTE
    fixed[!is_fixed] <- available_palette[((seq_along(remaining) - 1) %% length(available_palette)) + 1]
  }
  stats::setNames(unname(fixed), sorted_levels)
}

# 9. THE INVESTIGATE VIEW'S PER-GROUPING COLORS
# A different rule from LEVEL_COLOR_PALETTE above. That one colors a
# chart's lines by level. The Investigate tab's deviation-bar chart
# shows several groupings at once side by side, so color there answers
# "which grouping does this bar belong to," one fixed color per
# grouping.
GROUPING_COLORS <- c(
  "Race"                             = RAPID_COLORS$sky_blue,
  "Income Level"                     = RAPID_COLORS$warm_red,
  "CA Region"                        = RAPID_COLORS$purple,
  "Household Child Age"              = "#2E7D32",
  "Child Disability"                 = "#8D6E63",
  "Dual/Non-Dual Parent Household"   = "#5C6BC0",
  "LA County"                        = RAPID_COLORS$gray
)
