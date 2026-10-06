# =============================================================================
# refresh_db.R
#
# Purpose: Pull PR/VI NPDES facility and DMR data from ICIS-NPDES Oracle
#          database and write to a local SQLite file for use by the
#          PR RP Calculator Shiny app on Posit Connect.
#
# Schedule: Run weekly via Windows Task Scheduler or Posit Connect scheduler.
#           Output SQLite file should be placed in the app's data/ directory
#           on the Connect server (or synced there via rsconnect::deployApp).
#
# Output:   data/pr_rp.sqlite
#             Tables:
#               facilities    -- one row per permit (name, location, status)
#               dmr_data      -- DMR values with parameter, unit, stat base
#               sync_log      -- timestamp and row counts per run
#
# Usage:
#   Rscript refresh_db.R
#   -- or schedule via Task Scheduler pointing to this file --
#
# Dependencies: DBI, odbc, dplyr, RSQLite, lubridate
# =============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(odbc)
  library(dplyr)
  library(RSQLite)
  library(lubridate)
})

# ── Configuration ─────────────────────────────────────────────────────────────

# Oracle connection settings — set these as environment variables rather than
# hardcoding. In Task Scheduler, set them in the script environment or via
# a .Renviron file in your home directory.
ORACLE_USER <- Sys.getenv("ICIS_USER")
ORACLE_PW <- Sys.getenv("ICIS_PW")
ORACLE_DSN <- Sys.getenv("ICIS_DSN", unset = "ICISCOPY_SID")
ORACLE_DRIVER <- Sys.getenv(
  "ICIS_DRIVER",
  unset = "{Oracle in instantclient_23_0}"
)
TNS_ADMIN <- Sys.getenv(
  "TNS_ADMIN",
  unset = "C:/ODBC/instantclient_23_0/network/admin"
)
INSTANT_CLIENT <- Sys.getenv(
  "INSTANT_CLIENT",
  unset = "C:\\ODBC\\instantclient_23_0"
)

# Output SQLite path — adjust to match your app's data/ directory or a
# staging location you sync to Connect
SQLITE_PATH <- Sys.getenv("PR_RP_SQLITE", unset = "data/pr_rp.sqlite")

# Rolling DMR window — how many years back to pull
DMR_YEARS_BACK <- as.numeric(Sys.getenv("DMR_YEARS_BACK", unset = "10"))

# States to pull (PR = Puerto Rico, VI = US Virgin Islands)
TARGET_STATES <- c("PR", "VI")

# ── Logging ───────────────────────────────────────────────────────────────────

log_msg <- function(...) {
  cat(format(Sys.time(), "[%Y-%m-%d %H:%M:%S]"), ..., "\n")
  flush.console()
}

log_msg("=== PR RP Database Refresh Starting ===")
log_msg("SQLite output:", SQLITE_PATH)
log_msg("DMR window: last", DMR_YEARS_BACK, "years")
log_msg("Target states:", paste(TARGET_STATES, collapse = ", "))

# ── Connect to Oracle ─────────────────────────────────────────────────────────

log_msg("Connecting to ICIS-NPDES Oracle...")

Sys.setenv(TNS_ADMIN = TNS_ADMIN)
Sys.setenv(PATH = paste(INSTANT_CLIENT, Sys.getenv("PATH"), sep = ";"))

ora <- tryCatch(
  {
    dbConnect(
      odbc(),
      .connection_string = sprintf(
        "Driver=%s;Dbq=%s;Uid=%s;Pwd=%s;",
        ORACLE_DRIVER,
        ORACLE_DSN,
        ORACLE_USER,
        ORACLE_PW
      )
    )
  },
  error = function(e) {
    log_msg("FATAL: Could not connect to Oracle:", e$message)
    stop(e)
  }
)

log_msg("Connected to Oracle.")

# ── Helper: execute query with logging ────────────────────────────────────────

run_query <- function(con, sql, label = "") {
  log_msg(sprintf("Querying: %s...", label))
  t0 <- proc.time()["elapsed"]
  result <- tryCatch(
    dbGetQuery(con, sql, stringsAsFactors = FALSE),
    error = function(e) {
      log_msg(sprintf("ERROR in query '%s': %s", label, e$message))
      stop(e)
    }
  )
  elapsed <- round(proc.time()["elapsed"] - t0, 1)
  log_msg(sprintf("  -> %d rows in %.1fs", nrow(result), elapsed))
  result
}

# ── 1. Facilities ─────────────────────────────────────────────────────────────
# One row per active NPDES permit in PR/VI.
# Joins ICIS_PERMIT -> ICIS_PERM_FEATURE -> ICIS_PERM_FEATURE_COORD for coords,
# and XREF_ACTIVITY_FACILITY_INT -> ICIS_FACILITY_INTEREST for facility name.

state_list <- paste0("('", paste(TARGET_STATES, collapse = "','"), "')")
cutoff_date <- format(Sys.Date() - years(DMR_YEARS_BACK), "%Y-%m-%d")

facilities_sql <- sprintf(
  "
SELECT
  p.external_permit_nmbr                          AS permit_id,
  fi.facility_name,
  p.permit_status_code,
  p.effective_date,
  p.major_minor_status_flag,
  MAX(pfc.latitude_measure)                        AS fac_lat,
  MAX(pfc.longitude_measure)                       AS fac_long,
  fi.city,
  fi.state_code,
  fi.zip,
  fi.location_address
FROM icis_permit p
JOIN xref_activity_facility_int xafi
  ON p.activity_id = xafi.activity_id
JOIN icis_facility_interest fi
  ON xafi.icis_facility_interest_id = fi.icis_facility_interest_id
LEFT JOIN icis_perm_feature pf
  ON p.activity_id = pf.activity_id
  AND pf.perm_feature_type_code IN ('EXO', 'INO')
LEFT JOIN icis_perm_feature_coord pfc
  ON pf.perm_feature_id = pfc.perm_feature_id
WHERE SUBSTR(p.external_permit_nmbr, 1, 2) IN %s
AND p.version_nmbr = 0
AND p.permit_status_code IN ('EFF', 'ADC', 'PND')
AND p.permit_type_code = 'NPD'
GROUP BY
  p.external_permit_nmbr,
  fi.facility_name,
  p.permit_status_code,
  p.effective_date,
  p.major_minor_status_flag,
  fi.city,
  fi.state_code,
  fi.zip,
  fi.location_address
ORDER BY p.external_permit_nmbr
",
  state_list
)

facilities_df <- run_query(ora, facilities_sql, "facilities")

# Coerce coordinates to numeric (Oracle returns as character via ODBC)
facilities_df <- facilities_df %>%
  mutate(
    FAC_LAT = as.numeric(FAC_LAT),
    FAC_LONG = as.numeric(FAC_LONG),
    # Build display label for the selectize input (matches old ECHO format)
    display_label = paste0(PERMIT_ID, " - ", FACILITY_NAME)
  )

log_msg(sprintf(
  "Facilities: %d permits, %d with coordinates",
  nrow(facilities_df),
  sum(!is.na(facilities_df$FAC_LAT))
))

# ── 2. Permitted Features (outfalls) ─────────────────────────────────────────
# One row per outfall per permit — used for the outfall selector and map.

features_sql <- sprintf(
  "
SELECT
  p.external_permit_nmbr  AS permit_id,
  pf.perm_feature_nmbr,
  pf.perm_feature_name,
  pf.perm_feature_type_code,
  pf.water_body_name,
  pf.state_water_body_name,
  MAX(pfc.latitude_measure)  AS feat_lat,
  MAX(pfc.longitude_measure) AS feat_long
FROM icis_permit p
JOIN icis_perm_feature pf
  ON p.activity_id = pf.activity_id
LEFT JOIN icis_perm_feature_coord pfc
  ON pf.perm_feature_id = pfc.perm_feature_id
WHERE SUBSTR(p.external_permit_nmbr, 1, 2) IN %s
AND p.version_nmbr = 0
AND p.permit_status_code IN ('EFF', 'ADC', 'PND')
AND p.permit_type_code = 'NPD'
AND pf.perm_feature_type_code IN ('EXO', 'INO')
GROUP BY
  p.external_permit_nmbr,
  pf.perm_feature_nmbr,
  pf.perm_feature_name,
  pf.perm_feature_type_code,
  pf.water_body_name,
  pf.state_water_body_name
ORDER BY p.external_permit_nmbr, pf.perm_feature_nmbr
",
  state_list
)

features_df <- run_query(ora, features_sql, "permitted features")

features_df <- features_df %>%
  mutate(
    FEAT_LAT = as.numeric(FEAT_LAT),
    FEAT_LONG = as.numeric(FEAT_LONG)
  )

# ── 3. DMR Data ───────────────────────────────────────────────────────────────
# Full DMR value chain for PR/VI permits within the rolling window.
# Join chain:
#   ICIS_PERMIT -> ICIS_PERM_FEATURE -> ICIS_LIMIT_SET -> ICIS_DMR_EVENT
#   -> ICIS_DMR_FORM -> ICIS_DMR_FORM_PARAMETER -> ICIS_DMR_PARAMETER
#   -> ICIS_DMR_VALUE + REF_UNIT
#   ICIS_DMR_FORM_PARAMETER -> ICIS_LIMIT -> ICIS_LIMIT_VALUE
#   -> REF_STATISTICAL_BASE -> REF_STATISTICAL_BASE_TYPE
#   REF_PARAMETER for parameter description

# Unit name map: ICIS stores long names, app expects ECHO-style abbreviations
# Applied after fetch via R-side lookup (faster than joining in SQL)
unit_name_map <- c(
  "Milligrams per Liter" = "mg/L",
  "Micrograms per Liter" = "ug/L",
  "Nanograms per Liter" = "ng/L",
  "Standard Units" = "SU",
  "Degrees Centigrade" = "deg C",
  "Degrees Fahrenheit" = "deg F",
  "Kilograms per Day" = "kg/d",
  "Million Gallons per Day" = "MGD",
  "Gallons per Day" = "gal/d",
  "Milliliters per Liter" = "mL/L",
  "Nephelometric Turbidity Units" = "NTU",
  "Color - Platinum Cobalt Unit" = "col unit (pc)",
  "Percent" = "%",
  "Colony Forming Units per 100mL" = "cfu/100mL",
  "Most Probable Number per 100mL" = "MPN/100mL",
  "Pounds per Day" = "lbs/d",
  "Pounds" = "lbs",
  "Kilograms" = "kg",
  "Gallons per Minute" = "gal/min",
  "Cubic Feet per Second" = "cfs",
  "Milligrams" = "mg",
  "Micrograms" = "ug"
)

# Key design change: join ICIS_LIMIT_VALUE through ICIS_DMR_FORM_VALUE so each
# DMR value is matched to its SPECIFIC limit row, not all limits for that parameter.
# This prevents the many-to-many fan-out that caused pH value mismatches.
# v.DMR_FORM_VALUE_ID -> fv.DMR_FORM_VALUE_ID -> fv.LIMIT_VALUE_ID -> lv
#
# perm_feature_type_code IN ('EXO', 'INO'): some PR/VI permits have their
# permitted features mislabeled in ICIS-NPDES as 'Internal Outfall' (INO)
# instead of 'External Outfall' (EXO). Filtering to EXO only silently drops
# all DMR data for those permits. Until ICIS-NPDES data is corrected, both
# types are pulled, and perm_feature_type_code is carried through in the
# SELECT so the app can flag internal outfalls distinctly (label + color)
# rather than treating them identically to true external outfalls.

dmr_sql <- sprintf(
  "
SELECT
  p.external_permit_nmbr                   AS permit_id,
  pf.perm_feature_nmbr,
  pf.perm_feature_type_code,
  ev.monitoring_period_end_date,
  fp.parameter_code,
  rp.parameter_desc,
  v.dmr_value_nmbr,
  v.unit_code                              AS dmr_unit_code,
  ru.unit_desc                             AS dmr_unit_desc,
  v.nodi_code,
  rsbt.statistical_base_type_code,
  rsb.statistical_base_code,
  rsb.statistical_base_short_desc,
  lv.limit_value_nmbr,
  lv.unit_code                             AS limit_unit_code,
  ru_lim.unit_desc                         AS limit_unit_desc,
  l.limit_begin_date,
  l.limit_end_date,
  ls.limit_set_designator,
  ev.dmr_due_date
FROM icis_permit p
JOIN icis_perm_feature pf
  ON p.activity_id = pf.activity_id
  AND pf.perm_feature_type_code IN ('EXO', 'INO')
JOIN icis_limit_set ls
  ON pf.perm_feature_id = ls.perm_feature_id
JOIN icis_dmr_event ev
  ON ls.limit_set_id = ev.limit_set_id
  AND ev.monitoring_period_end_date >= TO_DATE('%s', 'YYYY-MM-DD')
JOIN icis_dmr_form df
  ON ev.dmr_event_id = df.dmr_event_id
JOIN icis_dmr_form_parameter fp
  ON df.dmr_form_id = fp.dmr_form_id
JOIN icis_dmr_parameter dp
  ON fp.dmr_form_parameter_id = dp.dmr_form_parameter_id
JOIN icis_dmr_value v
  ON dp.dmr_parameter_id = v.dmr_parameter_id
-- Join limit value through DMR_FORM_VALUE for exact 1:1 match per reported value
-- This replaces the old ICIS_LIMIT -> ICIS_LIMIT_VALUE fan-out join
LEFT JOIN icis_dmr_form_value fv
  ON v.dmr_form_value_id = fv.dmr_form_value_id
LEFT JOIN icis_limit_value lv
  ON fv.limit_value_id = lv.limit_value_id
LEFT JOIN icis_limit l
  ON lv.limit_id = l.limit_id
LEFT JOIN ref_statistical_base rsb
  ON lv.statistical_base_code = rsb.statistical_base_code
LEFT JOIN ref_statistical_base_type rsbt
  ON rsb.statistical_base_type_code = rsbt.statistical_base_type_code
JOIN ref_parameter rp
  ON fp.parameter_code = rp.parameter_code
LEFT JOIN ref_unit ru
  ON v.unit_code = ru.unit_code
LEFT JOIN ref_unit ru_lim
  ON lv.unit_code = ru_lim.unit_code
WHERE SUBSTR(p.external_permit_nmbr, 1, 2) IN %s
AND p.version_nmbr = 0
AND p.permit_status_code IN ('EFF', 'ADC', 'PND')
AND p.permit_type_code = 'NPD'
AND pf.perm_feature_nmbr IS NOT NULL
ORDER BY
  p.external_permit_nmbr,
  pf.perm_feature_nmbr,
  ev.monitoring_period_end_date,
  fp.parameter_code
",
  cutoff_date,
  state_list
)

dmr_df <- run_query(ora, dmr_sql, "DMR data")

log_msg(sprintf(
  "DMR: %d records across %d permits",
  nrow(dmr_df),
  n_distinct(dmr_df$PERMIT_ID)
))

# Normalize ICIS long unit names to ECHO-style abbreviations
log_msg("Normalizing unit names...")
dmr_df <- dmr_df %>%
  dplyr::mutate(
    DMR_UNIT_DESC = dplyr::recode(
      DMR_UNIT_DESC,
      !!!unit_name_map,
      .default = DMR_UNIT_DESC
    ),
    LIMIT_UNIT_DESC = dplyr::recode(
      LIMIT_UNIT_DESC,
      !!!unit_name_map,
      .default = LIMIT_UNIT_DESC
    )
  )
log_msg("Unit normalization complete.")

# ── 4. Disconnect from Oracle ─────────────────────────────────────────────────

dbDisconnect(ora)
log_msg("Disconnected from Oracle.")

# ── 5. Standardise column names to lowercase ──────────────────────────────────

names(facilities_df) <- tolower(names(facilities_df))
names(features_df) <- tolower(names(features_df))
names(dmr_df) <- tolower(names(dmr_df))

# ── Convert all Date columns to ISO text (YYYY-MM-DD) before writing ──────────
# RSQLite stores R Date/POSIXct as numeric epoch by default, which makes date
# range queries require epoch arithmetic. Storing as text avoids this.
convert_dates_to_text <- function(df) {
  date_cols <- sapply(df, function(x) {
    inherits(x, c("Date", "POSIXct", "POSIXlt"))
  })
  df[date_cols] <- lapply(df[date_cols], function(x) {
    format(as.Date(x), "%Y-%m-%d")
  })
  df
}

facilities_df <- convert_dates_to_text(facilities_df)
features_df <- convert_dates_to_text(features_df)
dmr_df <- convert_dates_to_text(dmr_df)

# ── 6. Write to SQLite ────────────────────────────────────────────────────────

log_msg("Writing to SQLite:", SQLITE_PATH)

# Ensure output directory exists
dir.create(dirname(SQLITE_PATH), showWarnings = FALSE, recursive = TRUE)

# Open connection — overwrite if exists
sqlite <- dbConnect(RSQLite::SQLite(), SQLITE_PATH)

# Write tables — overwrite on each refresh
dbWriteTable(sqlite, "facilities", facilities_df, overwrite = TRUE)
dbWriteTable(sqlite, "permitted_features", features_df, overwrite = TRUE)
dbWriteTable(sqlite, "dmr_data", dmr_df, overwrite = TRUE)

# Indexes for query performance
log_msg("Creating indexes...")
dbExecute(
  sqlite,
  "CREATE INDEX IF NOT EXISTS idx_dmr_permit
                   ON dmr_data (permit_id)"
)
dbExecute(
  sqlite,
  "CREATE INDEX IF NOT EXISTS idx_dmr_period
                   ON dmr_data (permit_id, monitoring_period_end_date)"
)
dbExecute(
  sqlite,
  "CREATE INDEX IF NOT EXISTS idx_dmr_param
                   ON dmr_data (permit_id, parameter_code)"
)
dbExecute(
  sqlite,
  "CREATE INDEX IF NOT EXISTS idx_feat_permit
                   ON permitted_features (permit_id)"
)

# Write sync log
sync_row <- data.frame(
  run_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  n_facilities = nrow(facilities_df),
  n_features = nrow(features_df),
  n_dmr_records = nrow(dmr_df),
  n_permits_dmr = n_distinct(dmr_df$permit_id),
  dmr_window_years = DMR_YEARS_BACK,
  dmr_cutoff_date = cutoff_date,
  status = "SUCCESS",
  stringsAsFactors = FALSE
)

dbWriteTable(sqlite, "sync_log", sync_row, append = TRUE)

dbDisconnect(sqlite)

log_msg("SQLite write complete.")
log_msg(sprintf("  facilities:         %d rows", nrow(facilities_df)))
log_msg(sprintf("  permitted_features: %d rows", nrow(features_df)))
log_msg(sprintf("  dmr_data:           %d rows", nrow(dmr_df)))
log_msg("=== Refresh Complete ===")


dmr <- dbReadTable(sqlite, "dmr_data")

ino <- dmr %>%
  filter(perm_feature_type_code == "INO") %>%
  select(permit_id, perm_feature_nmbr, perm_feature_type_code) %>%
  distinct()

exo <- dmr %>%
  filter(perm_feature_type_code == "EXO") %>%
  select(permit_id, perm_feature_nmbr, perm_feature_type_code) %>%
  distinct()

ino_only <- anti_join(ino, exo, by = "permit_id")

both <- dmr %>%
  select(permit_id) %>%
  filter(permit_id %in% ino$permit_id & permit_id %in% exo$permit_id) %>%
  distinct()
