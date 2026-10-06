# =============================================================================
# build_dmr_sample.R
#
# Purpose: Produce a COMPACT but REPRESENTATIVE slice of the full dmr_data table
#          for use during code/guide review. The goal is coverage, not volume:
#          every parameter_code, every statistical_base_type_code, and every
#          reported unit that appears in the data is represented, and within
#          each of those groups the value extremes (min / median / max) are kept
#          plus an example of any NODI-coded row and any row that carries a
#          numeric limit value.
#
# Why this shape: the app's unit-conversion, flagging, pH/temperature, and
#          coverage logic all branch on (parameter_code x unit x stat base), so
#          a sample that guarantees one of every combination exercises those
#          branches without shipping the entire dataset.
#
# Usage:   Rscript build_dmr_sample.R
#          (set PR_RP_SQLITE if your DB is not at data/pr_rp.sqlite)
#
# Output:  review_sample/dmr_sample.csv
#          review_sample/dmr_sample_coverage.txt   (quick coverage report)
#
# Dependencies: DBI, RSQLite, dplyr, stringr  (all already used by the app)
# =============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(dplyr)
  library(stringr)
})

# ---- Config -----------------------------------------------------------------
SQLITE_PATH <- Sys.getenv("PR_RP_SQLITE", unset = "data/pr_rp.sqlite")
OUT_DIR     <- "Analysis/review_sample"
# Per (parameter x stat base x unit) group: how many extra mid-range rows to
# keep beyond the min/max/median + nodi + limit examples. Keep small.
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

stopifnot("SQLite DB not found at PR_RP_SQLITE path" = file.exists(SQLITE_PATH))

# ---- Read the full dmr_data table -------------------------------------------
con <- dbConnect(RSQLite::SQLite(), SQLITE_PATH, flags = RSQLite::SQLITE_RO)
on.exit(dbDisconnect(con), add = TRUE)

raw <- dbGetQuery(con, "
  SELECT permit_id, perm_feature_nmbr, monitoring_period_end_date,
         parameter_code, parameter_desc, dmr_value_nmbr,
         dmr_unit_code, dmr_unit_desc, nodi_code,
         statistical_base_type_code, statistical_base_code,
         statistical_base_short_desc,
         limit_value_nmbr, limit_unit_code, limit_unit_desc,
         limit_begin_date, limit_end_date, limit_set_designator
  FROM dmr_data
")

# Standardise the way the app does: pad parameter_code to 5 chars, numeric value
raw <- raw %>%
  mutate(
    parameter_code = str_pad(as.character(parameter_code), 5, "left", "0"),
    dmr_value_nmbr = suppressWarnings(as.numeric(dmr_value_nmbr)),
    dmr_unit_desc  = as.character(dmr_unit_desc),
    .rid           = row_number()
  )

cat(sprintf("Read %d rows from %s\n", nrow(raw), SQLITE_PATH))

# ---- Pick representative row ids within each coverage group -----------------
# Group key mirrors the dimensions the app branches on.
pick_rids <- function(df) {
  v   <- df$dmr_value_nmbr
  fin <- is.finite(v)
  rid <- integer(0)
  
  if (any(fin)) {
    # min, max, and the row nearest the median
    rid <- c(rid,
             df$.rid[which.min(replace(v, !fin, Inf))],
             df$.rid[which.max(replace(v, !fin, -Inf))])
    med <- median(v[fin])
    rid <- c(rid, df$.rid[which.min(abs(v - med))])
  }
  
  # one example of a NODI-coded row (B/Q etc.), if present
  nodi <- which(!is.na(df$nodi_code) & df$nodi_code != "")
  if (length(nodi)) rid <- c(rid, df$.rid[nodi[1]])
  
  # one example carrying a numeric prior limit, if present
  lim <- which(is.finite(suppressWarnings(as.numeric(df$limit_value_nmbr))))
  if (length(lim)) rid <- c(rid, df$.rid[lim[1]])
  
  # fallback: if a group had no finite value, no nodi, no limit, keep first row
  if (!length(rid)) rid <- df$.rid[1]
  
  tibble(.rid = sort(unique(rid)))
}

chosen <- raw %>%
  group_by(parameter_code, statistical_base_type_code, dmr_unit_desc) %>%
  group_modify(~ pick_rids(.x)) %>%
  ungroup()

sample_df <- raw %>%
  filter(.rid %in% chosen$.rid) %>%
  select(-.rid) %>%
  arrange(parameter_code, statistical_base_type_code, dmr_unit_desc,
          dmr_value_nmbr)

# ---- Write sample + coverage report -----------------------------------------
out_csv <- file.path(OUT_DIR, "dmr_sample.csv")
write.csv(sample_df, out_csv, row.names = FALSE, na = "")

cov_lines <- c(
  sprintf("DMR sample coverage report  (%s)", format(Sys.time())),
  sprintf("Source DB: %s", SQLITE_PATH),
  sprintf("Full table rows:        %d", nrow(raw)),
  sprintf("Sample rows:            %d", nrow(sample_df)),
  sprintf("Distinct parameters:    full=%d  sample=%d",
          n_distinct(raw$parameter_code), n_distinct(sample_df$parameter_code)),
  sprintf("Distinct units:         full=%d  sample=%d",
          n_distinct(raw$dmr_unit_desc), n_distinct(sample_df$dmr_unit_desc)),
  sprintf("Distinct stat bases:    full=%d  sample=%d",
          n_distinct(raw$statistical_base_type_code),
          n_distinct(sample_df$statistical_base_type_code)),
  "",
  "Units present in full data (and whether captured in sample):"
)
unit_check <- raw %>%
  distinct(dmr_unit_desc) %>%
  mutate(in_sample = dmr_unit_desc %in% sample_df$dmr_unit_desc) %>%
  arrange(dmr_unit_desc)
cov_lines <- c(cov_lines,
               apply(unit_check, 1, function(r)
                 sprintf("  %-20s %s",
                         ifelse(is.na(r[["dmr_unit_desc"]]) || r[["dmr_unit_desc"]] == "",
                                "(blank)", r[["dmr_unit_desc"]]),
                         ifelse(as.logical(r[["in_sample"]]), "captured", "MISSING"))))

writeLines(cov_lines, file.path(OUT_DIR, "dmr_sample_coverage.txt"))

cat("Wrote:\n")
cat(" ", out_csv, "\n")
cat(" ", file.path(OUT_DIR, "dmr_sample_coverage.txt"), "\n")
cat(sprintf("Sample has %d rows covering %d parameters and %d units.\n",
            nrow(sample_df),
            n_distinct(sample_df$parameter_code),
            n_distinct(sample_df$dmr_unit_desc)))

