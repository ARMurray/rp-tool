# =============================================================================
# parameter_summary.R
#
# Purpose: Companion reference dataset describing the full dmr_data table at the
#          parameter level. Answers, for every parameter that appears in the
#          data: how many permits report it, how many records exist, which units
#          are used (and in what proportion), which statistical base types
#          appear, the value range, NODI usage, and whether prior limits are
#          carried. This is the lookup I use to sanity-check the user guide's
#          unit-conversion / coverage claims and, later, the code's branching.
#
# Usage:   Rscript parameter_summary.R
#          (set PR_RP_SQLITE if your DB is not at data/pr_rp.sqlite)
#
# Output:  review_sample/parameter_summary.csv
#             one row per parameter_code (the headline table)
#          review_sample/parameter_unit_breakdown.csv
#             one row per parameter_code x unit (the detail table)
#
# Dependencies: DBI, RSQLite, dplyr, stringr
# =============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(dplyr)
  library(stringr)
})

SQLITE_PATH <- Sys.getenv("PR_RP_SQLITE", unset = "data/pr_rp.sqlite")
OUT_DIR     <- "Analysis/review_sample"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
stopifnot("SQLite DB not found at PR_RP_SQLITE path" = file.exists(SQLITE_PATH))

con <- dbConnect(RSQLite::SQLite(), SQLITE_PATH, flags = RSQLite::SQLITE_RO)
on.exit(dbDisconnect(con), add = TRUE)

raw <- dbGetQuery(con, "
  SELECT permit_id, parameter_code, parameter_desc, dmr_value_nmbr,
         dmr_unit_desc, nodi_code, statistical_base_type_code,
         limit_value_nmbr, limit_unit_desc
  FROM dmr_data
") %>%
  mutate(
    parameter_code = str_pad(as.character(parameter_code), 5, "left", "0"),
    dmr_value_nmbr = suppressWarnings(as.numeric(dmr_value_nmbr)),
    limit_value_nmbr = suppressWarnings(as.numeric(limit_value_nmbr)),
    dmr_unit_desc  = ifelse(is.na(dmr_unit_desc) | dmr_unit_desc == "",
                            "(blank)", dmr_unit_desc)
  )

cat(sprintf("Read %d rows from %s\n", nrow(raw), SQLITE_PATH))

collapse_counts <- function(x) {
  # "mg/L (1203), ug/L (45)" style summary, most common first
  tab <- sort(table(x), decreasing = TRUE)
  paste(sprintf("%s (%d)", names(tab), as.integer(tab)), collapse = ", ")
}

# ---- Headline: one row per parameter ----------------------------------------
param_summary <- raw %>%
  group_by(parameter_code) %>%
  summarise(
    parameter_desc = first(parameter_desc[!is.na(parameter_desc) & parameter_desc != ""]),
    n_records      = n(),
    n_permits      = n_distinct(permit_id),
    n_with_value   = sum(is.finite(dmr_value_nmbr)),
    n_nodi         = sum(!is.na(nodi_code) & nodi_code != ""),
    units_used     = collapse_counts(dmr_unit_desc),
    stat_bases     = collapse_counts(statistical_base_type_code),
    nodi_codes     = {
      nc <- nodi_code[!is.na(nodi_code) & nodi_code != ""]
      if (length(nc)) collapse_counts(nc) else ""
    },
    value_min      = suppressWarnings(min(dmr_value_nmbr, na.rm = TRUE)),
    value_median   = suppressWarnings(median(dmr_value_nmbr, na.rm = TRUE)),
    value_mean     = suppressWarnings(mean(dmr_value_nmbr, na.rm = TRUE)),
    value_max      = suppressWarnings(max(dmr_value_nmbr, na.rm = TRUE)),
    has_limits     = any(is.finite(limit_value_nmbr)),
    limit_units    = {
      lu <- limit_unit_desc[!is.na(limit_unit_desc) & limit_unit_desc != ""]
      if (length(lu)) collapse_counts(lu) else ""
    },
    .groups = "drop"
  ) %>%
  # tidy up the Inf/NaN that min/max/mean produce for all-NA parameters
  mutate(across(c(value_min, value_median, value_mean, value_max),
                ~ ifelse(is.finite(.x), .x, NA_real_))) %>%
  arrange(parameter_code)

# ---- Detail: one row per parameter x unit -----------------------------------
param_unit <- raw %>%
  group_by(parameter_code, parameter_desc, dmr_unit_desc) %>%
  summarise(
    n_records  = n(),
    n_permits  = n_distinct(permit_id),
    value_min  = suppressWarnings(min(dmr_value_nmbr, na.rm = TRUE)),
    value_max  = suppressWarnings(max(dmr_value_nmbr, na.rm = TRUE)),
    .groups    = "drop"
  ) %>%
  mutate(across(c(value_min, value_max), ~ ifelse(is.finite(.x), .x, NA_real_))) %>%
  arrange(parameter_code, desc(n_records))

write.csv(param_summary, file.path(OUT_DIR, "parameter_summary.csv"),
          row.names = FALSE, na = "")
write.csv(param_unit, file.path(OUT_DIR, "parameter_unit_breakdown.csv"),
          row.names = FALSE, na = "")

cat("Wrote:\n")
cat(" ", file.path(OUT_DIR, "parameter_summary.csv"),
    sprintf("(%d parameters)\n", nrow(param_summary)))
cat(" ", file.path(OUT_DIR, "parameter_unit_breakdown.csv"),
    sprintf("(%d parameter x unit rows)\n", nrow(param_unit)))
