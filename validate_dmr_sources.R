# =============================================================================
# validate_dmr_sources.R
#
# Purpose: Compare DMR data for a single permit retrieved from:
#   (A) The local SQLite database built by refresh_db.R
#   (B) The original ECHO API path used by the app
#
# Run this interactively after refresh_db.R has populated the database.
# Results are written to validation_results/ for inspection.
#
# Usage:
#   source("validate_dmr_sources.R")
#   -- or --
#   Rscript validate_dmr_sources.R
# =============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(echor)         # echoGetEffluent
})

# ── Configuration ─────────────────────────────────────────────────────────────

PERMIT_ID   <- "PR0001031"
DATE_START  <- as.Date("2019-01-01")
DATE_END    <- as.Date(Sys.Date())
SQLITE_PATH <- Sys.getenv("PR_RP_SQLITE", unset = "data/pr_rp.sqlite")
OUT_DIR     <- "validation_results"

dir.create(OUT_DIR, showWarnings = FALSE)

start_fmt <- format(DATE_START, "%m/%d/%Y")   # ECHO format
end_fmt   <- format(DATE_END,   "%m/%d/%Y")

cat(sprintf("\n=== DMR Validation: %s ===\n", PERMIT_ID))
cat(sprintf("Date range: %s to %s\n\n", DATE_START, DATE_END))

# =============================================================================
# A. SQLite path
# =============================================================================

cat("--- A. Loading from SQLite ---\n")

sqlite_con <- dbConnect(RSQLite::SQLite(), SQLITE_PATH, flags = RSQLite::SQLITE_RO)

sqlite_raw <- dbGetQuery(sqlite_con, sprintf("
  SELECT
    permit_id,
    perm_feature_nmbr,
    monitoring_period_end_date,
    parameter_code,
    parameter_desc,
    dmr_value_nmbr,
    dmr_unit_desc,
    nodi_code,
    statistical_base_type_code,
    limit_value_nmbr,
    limit_unit_desc
  FROM dmr_data
  WHERE permit_id = '%s'
  AND monitoring_period_end_date >= '%s'
  AND monitoring_period_end_date <= '%s'
  ORDER BY monitoring_period_end_date, parameter_code, statistical_base_type_code
", PERMIT_ID,
                                             format(DATE_START, "%Y-%m-%d"),
                                             format(DATE_END,   "%Y-%m-%d")
))

dbDisconnect(sqlite_con)

sqlite_df <- sqlite_raw %>%
  mutate(
    parameter_code             = str_pad(as.character(parameter_code), 5, "left", "0"),
    dmr_value_nmbr             = as.numeric(dmr_value_nmbr),
    monitoring_period_end_date = as.Date(monitoring_period_end_date),
    source                     = "sqlite"
  ) %>%
  # Apply same filters the app uses
  filter(
    statistical_base_type_code == "MAX" |
      (parameter_code == "00400" & statistical_base_type_code %in% c("MAX", "MIN"))
  )

cat(sprintf("  SQLite rows (pre-dedup): %d\n", nrow(sqlite_df)))
cat(sprintf("  Unique parameters:       %d\n", n_distinct(sqlite_df$parameter_code)))
cat(sprintf("  Unique outfalls:         %d\n", n_distinct(sqlite_df$perm_feature_nmbr)))
cat(sprintf("  Date range in data:      %s to %s\n\n",
            min(sqlite_df$monitoring_period_end_date, na.rm = TRUE),
            max(sqlite_df$monitoring_period_end_date, na.rm = TRUE)))

# =============================================================================
# B. ECHO API path
# =============================================================================

cat("--- B. Loading from ECHO API ---\n")

echo_raw <- tryCatch({
  echor::echoGetEffluent(
    p_id       = PERMIT_ID,
    output     = "df",
    start_date = start_fmt,
    end_date   = end_fmt
  )
}, error = function(e) {
  cat("  ERROR calling ECHO API:", e$message, "\n")
  NULL
})

if (is.null(echo_raw) || nrow(echo_raw) == 0) {
  stop("ECHO API returned no data. Cannot validate.")
}

echo_df <- echo_raw %>%
  mutate(
    parameter_code             = str_pad(as.character(parameter_code), 5, "left", "0"),
    dmr_value_nmbr             = as.numeric(dmr_value_nmbr),
    # ECHO returns dates as MM/DD/YYYY character strings
    monitoring_period_end_date = as.Date(monitoring_period_end_date, format = "%m/%d/%Y"),
    source                     = "echo"
  ) %>%
  filter(
    perm_feature_type_code == "EXO",
    statistical_base_type_code == "MAX" |
      (parameter_code == "00400" & statistical_base_type_code %in% c("MAX", "MIN"))
  ) %>%
  select(
    permit_id                  = npdes_id,
    perm_feature_nmbr,
    monitoring_period_end_date,
    parameter_code,
    parameter_desc,
    dmr_value_nmbr,
    dmr_unit_desc,
    nodi_code,
    statistical_base_type_code,
    limit_value_nmbr,
    limit_unit_desc,
    source
  )

cat(sprintf("  ECHO rows (pre-dedup):   %d\n", nrow(echo_df)))
cat(sprintf("  Unique parameters:       %d\n", n_distinct(echo_df$parameter_code)))
cat(sprintf("  Unique outfalls:         %d\n", n_distinct(echo_df$perm_feature_nmbr)))
cat(sprintf("  Date range in data:      %s to %s\n\n",
            min(echo_df$monitoring_period_end_date, na.rm = TRUE),
            max(echo_df$monitoring_period_end_date, na.rm = TRUE)))

# =============================================================================
# C. Comparison key columns
# =============================================================================

# Normalise to a common key for joining:
#   permit_id + perm_feature_nmbr + monitoring_period_end_date +
#   parameter_code + statistical_base_type_code
# This uniquely identifies a single reported value.

key_cols <- c("perm_feature_nmbr", "monitoring_period_end_date",
              "parameter_code", "statistical_base_type_code")

val_cols <- c("dmr_value_nmbr", "dmr_unit_desc",
              "nodi_code", "limit_value_nmbr", "limit_unit_desc")

sqlite_keyed <- sqlite_df %>%
  select(all_of(c(key_cols, val_cols))) %>%
  mutate(limit_value_nmbr = as.numeric(limit_value_nmbr)) %>%
  distinct()

echo_keyed <- echo_df %>%
  select(all_of(c(key_cols, val_cols))) %>%
  mutate(limit_value_nmbr = as.numeric(limit_value_nmbr)) %>%
  distinct()

cat("--- C. Row-level comparison ---\n")

# C1: Keys in SQLite but not ECHO
only_sqlite <- anti_join(sqlite_keyed, echo_keyed, by = key_cols)
cat(sprintf("  Rows in SQLite only (not in ECHO): %d\n", nrow(only_sqlite)))

# C2: Keys in ECHO but not SQLite
only_echo <- anti_join(echo_keyed, sqlite_keyed, by = key_cols)
cat(sprintf("  Rows in ECHO only (not in SQLite): %d\n", nrow(only_echo)))
if (nrow(only_echo) > 0) {
  cat("  ECHO-only rows (first 10):\n")
  print(head(only_echo %>% arrange(monitoring_period_end_date, parameter_code), 10))
}

# Diagnose duplicates before joining (many-to-many warning means key isn't unique)
dupes_sqlite <- sqlite_keyed %>%
  group_by(across(all_of(key_cols))) %>%
  filter(n() > 1) %>%
  ungroup()
dupes_echo <- echo_keyed %>%
  group_by(across(all_of(key_cols))) %>%
  filter(n() > 1) %>%
  ungroup()
cat(sprintf("  Duplicate keys in SQLite: %d rows\n", nrow(dupes_sqlite)))
cat(sprintf("  Duplicate keys in ECHO:   %d rows\n", nrow(dupes_echo)))
if (nrow(dupes_sqlite) > 0) {
  write.csv(dupes_sqlite, file.path(OUT_DIR, "00_dupes_sqlite.csv"), row.names = FALSE)
  cat("  -> Written to 00_dupes_sqlite.csv\n")
}
if (nrow(dupes_echo) > 0) {
  write.csv(dupes_echo, file.path(OUT_DIR, "00_dupes_echo.csv"), row.names = FALSE)
  cat("  -> Written to 00_dupes_echo.csv\n")
}

# C3: Keys in both — compare values
# Use slice_max on dmr_value_nmbr to deduplicate before joining
# (multiple limit rows per DMR value is the likely cause of many-to-many)
sqlite_dedup <- sqlite_keyed %>%
  group_by(across(all_of(key_cols))) %>%
  slice_max(order_by = dmr_value_nmbr, n = 1, with_ties = FALSE) %>%
  ungroup()

echo_dedup <- echo_keyed %>%
  group_by(across(all_of(key_cols))) %>%
  slice_max(order_by = dmr_value_nmbr, n = 1, with_ties = FALSE) %>%
  ungroup()

both <- inner_join(
  sqlite_dedup %>% rename_with(~ paste0(.x, "_sqlite"), all_of(val_cols)),
  echo_dedup   %>% rename_with(~ paste0(.x, "_echo"),   all_of(val_cols)),
  by = key_cols,
  relationship = "one-to-one"
)
cat(sprintf("  Rows matched on key (deduped):     %d\n\n", nrow(both)))

# Value-level discrepancies on matched rows
both <- both %>%
  mutate(
    val_match   = abs(coalesce(dmr_value_nmbr_sqlite, -9999) -
                        coalesce(dmr_value_nmbr_echo,   -9999)) < 0.0001,
    unit_match  = coalesce(dmr_unit_desc_sqlite, "") ==
      coalesce(dmr_unit_desc_echo,   ""),
    limit_match = abs(coalesce(limit_value_nmbr_sqlite, -9999) -
                        coalesce(limit_value_nmbr_echo,   -9999)) < 0.0001,
    nodi_match  = coalesce(nodi_code_sqlite, "") ==
      coalesce(nodi_code_echo,   ""),
    all_match   = val_match & unit_match & limit_match & nodi_match
  )

n_val_diff   <- sum(!both$val_match,   na.rm = TRUE)
n_unit_diff  <- sum(!both$unit_match,  na.rm = TRUE)
n_limit_diff <- sum(!both$limit_match, na.rm = TRUE)
n_nodi_diff  <- sum(!both$nodi_match,  na.rm = TRUE)
n_all_match  <- sum(both$all_match,    na.rm = TRUE)

cat("--- D. Value-level discrepancies (matched rows) ---\n")
cat(sprintf("  DMR value mismatches:    %d / %d\n",  n_val_diff,   nrow(both)))
cat(sprintf("  Unit desc mismatches:    %d / %d\n",  n_unit_diff,  nrow(both)))
cat(sprintf("  Limit value mismatches:  %d / %d\n",  n_limit_diff, nrow(both)))
cat(sprintf("  NODI code mismatches:    %d / %d\n",  n_nodi_diff,  nrow(both)))
cat(sprintf("  Rows fully matching:     %d / %d\n\n",n_all_match,  nrow(both)))

# =============================================================================
# D. Parameter coverage comparison
# =============================================================================

cat("--- E. Parameter coverage ---\n")

params_sqlite <- sqlite_keyed %>%
  distinct(parameter_code) %>%
  mutate(in_sqlite = TRUE)

params_echo <- echo_keyed %>%
  distinct(parameter_code) %>%
  mutate(in_echo = TRUE)

param_compare <- full_join(params_sqlite, params_echo, by = "parameter_code") %>%
  mutate(
    in_sqlite = coalesce(in_sqlite, FALSE),
    in_echo   = coalesce(in_echo,   FALSE),
    status    = case_when(
      in_sqlite & in_echo   ~ "both",
      in_sqlite & !in_echo  ~ "sqlite only",
      !in_sqlite & in_echo  ~ "echo only"
    )
  ) %>%
  arrange(status, parameter_code)

print(param_compare)

# =============================================================================
# E. Outfall coverage
# =============================================================================

cat("\n--- F. Outfall coverage ---\n")

outfalls_sqlite <- sort(unique(sqlite_keyed$perm_feature_nmbr))
outfalls_echo   <- sort(unique(echo_keyed$perm_feature_nmbr))

cat("  SQLite outfalls:", paste(outfalls_sqlite, collapse = ", "), "\n")
cat("  ECHO outfalls:  ", paste(outfalls_echo,   collapse = ", "), "\n")
cat("  In SQLite only: ", paste(setdiff(outfalls_sqlite, outfalls_echo), collapse = ", "), "\n")
cat("  In ECHO only:   ", paste(setdiff(outfalls_echo,   outfalls_sqlite), collapse = ", "), "\n\n")

# =============================================================================
# F. Monthly record count comparison (time series check)
# =============================================================================

cat("--- G. Monthly record counts ---\n")

monthly_sqlite <- sqlite_keyed %>%
  mutate(ym = format(monitoring_period_end_date, "%Y-%m")) %>%
  count(ym, name = "n_sqlite")

monthly_echo <- echo_keyed %>%
  mutate(ym = format(monitoring_period_end_date, "%Y-%m")) %>%
  count(ym, name = "n_echo")

monthly_compare <- full_join(monthly_sqlite, monthly_echo, by = "ym") %>%
  arrange(ym) %>%
  mutate(
    n_sqlite = coalesce(n_sqlite, 0L),
    n_echo   = coalesce(n_echo,   0L),
    diff     = n_sqlite - n_echo
  )

n_month_diff <- sum(monthly_compare$diff != 0)
cat(sprintf("  Months with count differences: %d / %d\n\n",
            n_month_diff, nrow(monthly_compare)))

if (n_month_diff > 0) {
  cat("  Months with differences:\n")
  print(filter(monthly_compare, diff != 0))
  cat("\n")
}

# =============================================================================
# G. Write outputs
# =============================================================================

write.csv(only_sqlite,     file.path(OUT_DIR, "01_sqlite_only_rows.csv"),    row.names = FALSE)
write.csv(only_echo,       file.path(OUT_DIR, "02_echo_only_rows.csv"),      row.names = FALSE)
write.csv(filter(both, !all_match),
          file.path(OUT_DIR, "03_value_mismatches.csv"),    row.names = FALSE)
write.csv(param_compare,   file.path(OUT_DIR, "04_parameter_coverage.csv"), row.names = FALSE)
write.csv(monthly_compare, file.path(OUT_DIR, "05_monthly_counts.csv"),     row.names = FALSE)

# Write both full datasets side by side for manual inspection
write.csv(sqlite_keyed %>% mutate(source = "sqlite"),
          file.path(OUT_DIR, "06_sqlite_full.csv"), row.names = FALSE)
write.csv(echo_keyed %>% mutate(source = "echo"),
          file.path(OUT_DIR, "06_echo_full.csv"),   row.names = FALSE)

# =============================================================================
# H. Summary verdict
# =============================================================================

cat("=== SUMMARY ===\n")
cat(sprintf("  SQLite rows (filtered):  %d\n", nrow(sqlite_keyed)))
cat(sprintf("  ECHO rows (filtered):    %d\n", nrow(echo_keyed)))
cat(sprintf("  Matched rows:            %d\n", nrow(both)))
cat(sprintf("  SQLite-only rows:        %d\n", nrow(only_sqlite)))
cat(sprintf("  ECHO-only rows:          %d\n", nrow(only_echo)))
cat(sprintf("  Value mismatches:        %d\n", n_val_diff))
cat(sprintf("  Unit mismatches:         %d\n", n_unit_diff))
cat(sprintf("  Limit mismatches:        %d\n", n_limit_diff))

if (nrow(only_sqlite) == 0 && nrow(only_echo) == 0 &&
    n_val_diff == 0 && n_unit_diff == 0) {
  cat("\n  ✓ PASS: SQLite and ECHO data match exactly.\n")
} else {
  cat("\n  ⚠ DIFFERENCES FOUND: Review CSVs in validation_results/\n")
  cat("    Focus on:\n")
  if (nrow(only_echo)   > 0) cat("    - 02_echo_only_rows.csv  (records missing from SQLite)\n")
  if (nrow(only_sqlite) > 0) cat("    - 01_sqlite_only_rows.csv (extra records in SQLite)\n")
  if (n_val_diff        > 0) cat("    - 03_value_mismatches.csv (value differences)\n")
}

cat("\nAll output written to:", OUT_DIR, "\n")

