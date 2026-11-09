
# domain_outcome_vars.R

# Which columns make up each domain's outcome(s) -- the outcome_vars
# constants DOMAIN_FIGURES (R/06_reporting.R) resolves every figure
# from. Split out of R/05_excel_export.R on purpose: these are plain
# base-R c()/list()/setNames() constants with zero package
# dependencies, but living inside 05_excel_export.R meant sourcing them
# also ran that file's `library(openxlsx)` -- which the dashboard never
# needs (it never writes an Excel file; only pipeline_run.R does) but
# was paying for anyway, since shinylive's dependency scanner bundles
# whatever every sourced .R file's library() calls name, not just what
# actually gets called at runtime. The dashboard sources this file
# instead of 05_excel_export.R; pipeline_run.R still gets everything
# here too, since 05_excel_export.R itself sources this file first.
#
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
