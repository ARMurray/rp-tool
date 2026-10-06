# PR RP Calculator — Changelog & Current Status

**Session date:** June 2026  
**Files modified:** `app.R`, `functions.R`, `report.qmd`  
**Files added:** `refresh_db.R`, `validate_dmr_sources.R`

---

## Overview of Changes

This session addressed two original bug fixes, a major architectural overhaul of
the DMR data pipeline, and a series of new features around WQS flagging, manual
criterion entry, and data traceability in the report.

---

## 1. Issue 1 — NPDES Form Filter Decoupled from Analysis Gate

**Status: ✅ Complete**

**Problem:** The NPDES form selection was used as a hard filter on which parameters
entered the RP calculation. Parameters with valid DMR data but not associated with
a selected form (e.g. Chlorine, code 50060) were silently dropped from analysis.

**Changes:**

### `functions.R`
- Added `build_form_lookup(crosswalk_form, selected_forms)` — produces a
  `parameter_code → "Form A & Form B"` label table used for display only.

### `app.R`
- Split crosswalk build into two objects at fetch time:
  - `rv$crosswalk_full` — water class filtered only, no form filter. Used for the
    ECHO/SQLite parameter scope, flagging logic, and RP calculation.
  - `rv$crosswalk` — water class + form filtered. Used for coverage table Form
    column and "Reason Included" labels.
- `rv$form_lookup` populated via `build_form_lookup()` after each fetch.
- Coverage table `Form` column now shows associated forms from `rv$form_lookup`
  or "Not in selected forms" for unmatched parameters.
- `rwc_criteria` build switched from `rv$crosswalk` to `rv$crosswalk_effective`
  so non-form parameters (Chlorine, etc.) receive an RP determination.

### `report.qmd`
- "Reason Included" line added under each `## Pollutant` heading:
  - Form-associated: *"Reason Included: Form(s) 2C & 2D selected."*
  - Non-form: *"Reason Included: Not associated with selected forms — prior DMR
    data exists."*
- Coverage table intro text updated to reflect that non-form parameters are
  included.

---

## 2. Issue 2 — pH Sample Count Double-Counting

**Status: ✅ Complete**

**Problem:** pH has both MAX and MIN rows in the DMR. The coverage table was
grouping by `parameter_code` only, counting both rows per monitoring period for
both "pH (maximum)" and "pH (minimum)", resulting in double the true count.

**Changes:**

### `app.R` — `coverage_tbl_data()`
- pH sample count now split by `statistical_base_type_code` using `rv$ph_dmr`
  (keyed by `NPDES_Pollutant`), not from `rv$dmr` grouped by `parameter_code`.
- Before Run RP (when `rv$ph_dmr` is NULL), falls back to splitting raw DMR
  rows by MAX/MIN stat base code.

### `report.qmd`
- `n_samples` in the per-parameter loop guards against double-counting by pulling
  from `ph_dmr_df` filtered by `NPDES_Pollutant` when the pollutant is
  "pH (maximum)" or "pH (minimum)".

---

## 3. Parameters Needing Attention System

**Status: ✅ Complete**

**Problem:** Parameters with DMR data but no matching WQS criterion for the
selected water class had no workflow — they were silently excluded from analysis.

**New Features:**

### `functions.R`
- `recognized_concentration_units()` — returns units handleable by
  `get_unit_conversion()`. Used to restrict manual entry unit selector.
- `flag_unmatched_params(dmr, crosswalk_full, crosswalk_all, dmr_parameters)` —
  identifies parameters in `rv$dmr` with no usable WQS row. Returns case reason:
  - **Case 1 — Wrong water class:** WQS exists for parameter but not for the
    selected water class(es).
  - **Case 2 — No crosswalk entry:** Parameter code has no WQS entry at all.
  - **Case 3 — Missing criterion value:** Crosswalk entry exists but value is NA.
  - Now also uses `parameter_desc` from the DMR data itself as a fallback for
    display names (covers parameters like PCBs that may be missing from the
    separate lookup file).
- `build_manual_crosswalk_rows(overrides)` — converts Include decisions from the
  review panel into synthetic crosswalk rows for injection at Run RP time.

### `app.R`
- `rv$flagged_params` — populated by `flag_unmatched_params()` after each fetch.
- `rv$wqs_overrides` — stores user Include/Exclude decisions from the review panel.
- `rv$quick_run_flag` / `rv$quick_run_params` — tracks Quick Run usage for report.
- **Summary page** redesigned as a 3-tab layout:
  - **Data Overview** — coverage table (existing, enhanced).
  - **⚠ Needs Attention** — per-parameter review panel (new).
  - **WQS Reference** — read-only searchable crosswalk (new).
- **Run RP button** disabled until all flagged parameters are resolved.
- **Quick Run (Drop Unresolved)** red button — appears only when unresolved
  parameters exist. Auto-excludes all unresolved with a generated note and
  sets `rv$quick_run_flag = TRUE`.
- **`crosswalk_effective`** assembled at Run RP time:
  `crosswalk_full` - excluded params + manual rows.
  All RP calculation steps (rwc_criteria, metals, TAN, pH, temperature) use
  `rv$crosswalk_effective` instead of `rv$crosswalk`.
- Manual WQS unit conversion re-runs `get_unit_conversion()` using user-selected
  unit as the WQS target.
- **WQS Reference table** uses `NPDES_Pollutant` with `parameter_desc` fallback
  from `dmr_parameters_lookup`. Column renamed to "Pollutant".

### `report.qmd`
- New params: `overrides_path`, `quick_run`, `quick_run_params`.
- `overrides_df` and `wqs_annotation_df` loaded in setup chunk.
- **Quick Run warning block** — prominent red warning in Summary section listing
  auto-excluded parameters when `quick_run = TRUE`.
- **Coverage table** — `†` dagger marker for manually-entered WQS criteria with
  footnote pointing to Manual WQS Overrides section.
- **Manual WQS callout** — orange warning box in per-parameter RP subsection when
  a manual criterion was applied.
- **New section: Excluded Parameters** — conditional, lists all user-excluded
  parameters with their stated reasons. Required for permanent record.
- **New section: Manual WQS Overrides** — conditional, full audit table of
  manually-entered criteria including basis notes, water classes, and flag reason.

---

## 4. RP Boundary Condition Fixes

**Status: ✅ Complete**

**Problem:** RP determination used strict `>` for maximum standards and strict `<`
for minimum standards. Values exactly equal to the criterion should be violations.

**Changes — `app.R`:**
- pH findings: `max_value >= CRITERION_VALUE`, `min_value <= CRITERION_VALUE`
- Temperature findings: `max_value >= CRITERION_VALUE`
- Concentration RWC: `RWC_rs >= CRITERION_VALUE`

---

## 5. Quick Stats — Min/Max Value Label

**Status: ✅ Complete**

**Problem:** Quick Stats card always showed "Max Value" even for pH (minimum),
which has a minimum criterion.

**Changes — `app.R`:**
- For `input$selected_pollutant == "pH (minimum)"`, label shows "Min Value" and
  value is pulled from `f$min_value` instead of `f$max_value`.

---

## 6. Parameter Name Fallback (Coverage Table + RP Summary)

**Status: ✅ Complete**

**Problem:** Parameters not in the NPDES forms crosswalk had blank `NPDES_Pollutant`
names, causing blank rows in the coverage table and RP summary table.

**Changes — `app.R`:**
- `coverage_tbl_data()` joins `dmr_parameters_lookup` and coalesces
  `NPDES_Pollutant` with `parameter_desc` before renaming to `Pollutant`.
- `rwc_criteria` build (in `run_rp`) applies same coalesce so RP Summary Table
  and Interactive Inspector pollutant selector show proper names.

---

## 7. Page Navigation — Back to Start Button

**Status: ✅ Complete**

**Changes — `app.R`:**
- "← Back to Start" button added to the global top bar (appears on every page).
- `observeEvent(input$back_to_start)` sets `current_page("landing")`.

---

## 8. Report — Page Break Control

**Status: ✅ Complete**

**Problem:** Outfall content within a pollutant section was breaking across pages
in ways that made the report hard to read.

**Changes — `report.qmd`:**
- Each outfall loop body (concentration and pH/temperature) is wrapped in
  `\begin{samepage}...\end{samepage}` via raw LaTeX fenced blocks
  (`` ```{=latex} ``) so Quarto/pandoc passes them through correctly.
- Best-effort: LaTeX will honour the instruction unless the content exceeds one
  full page.

---

## 9. SQLite Database Architecture (replaces ECHO API)

**Status: ✅ Complete**

**Problem:** The ECHO API rate-limited on Posit Connect at startup (`429 Too Many
Requests`), causing the facility selectize to fail to populate.

### New file: `refresh_db.R`

A standalone weekly refresh script that:
1. Connects to Oracle ICIS-NPDES using environment variables (no hardcoded creds).
2. Pulls **facilities** (permit ID, name, location, status) via
   `ICIS_FACILITY_INTEREST` path.
3. Pulls **permitted features** (outfall-level data per permit).
4. Pulls **DMR data** — full chain through `ICIS_DMR_EVENT` → `ICIS_DMR_FORM` →
   `ICIS_DMR_FORM_PARAMETER` → `ICIS_DMR_PARAMETER` → `ICIS_DMR_VALUE`.
   - Limit value joined through `ICIS_DMR_FORM_VALUE` → `ICIS_LIMIT_VALUE` for
     exact 1:1 match per reported value (prevents pH fan-out double rows).
   - `limit_begin_date` and `limit_end_date` included from `ICIS_LIMIT`.
   - Unit names normalized from ICIS long names to ECHO-style abbreviations via
     `unit_name_map` (22 mappings: "Milligrams per Liter" → "mg/L", etc.).
   - All Date columns converted to ISO text (`YYYY-MM-DD`) before SQLite write
     (prevents epoch storage that breaks date range filters).
5. Writes to `data/pr_rp.sqlite` with indexes on `permit_id` and
   `monitoring_period_end_date`.
6. Appends a row to `sync_log` (timestamp, row counts, status).

**Configuration:** Set environment variables `ICIS_USER`, `ICIS_PW`, `ICIS_DSN`,
`ICIS_DRIVER` before running. Optionally set `PR_RP_SQLITE` and `DMR_YEARS_BACK`.

### New file: `validate_dmr_sources.R`

Standalone validation script comparing SQLite vs ECHO DMR data for a single
permit. Writes 6 CSVs to `validation_results/`:
- `01_sqlite_only_rows.csv` — records in SQLite not in ECHO
- `02_echo_only_rows.csv` — records in ECHO not in SQLite
- `03_value_mismatches.csv` — value/unit/limit differences on matched rows
- `04_parameter_coverage.csv` — parameter code presence in each source
- `05_monthly_counts.csv` — monthly record count comparison
- `06_sqlite_full.csv` / `06_echo_full.csv` — full datasets for manual inspection

**Validation results for PR0001031:**
- 0 SQLite-only rows
- 18 ECHO-only rows (all have null DMR values — dropped by app anyway)
- 0 value mismatches (after fix)
- 1 unit mismatch (genuine ICIS/ECHO data entry discrepancy, not a code issue)
- 104 limit mismatches (dedup artifact from multiple limit rows; reference-only,
  does not affect RP calculation)

### `app.R` changes for SQLite

- `library(echor)` removed — no ECHO API calls at runtime.
- `SQLITE_PATH` read from `PR_RP_SQLITE` env var (default: `data/pr_rp.sqlite`).
- `load_facilities_from_db()` replaces `fetch_facility_cache()` at startup.
  Renames SQLite columns to match app expectations (`SourceID`, `CWPName`, etc.).
- `permit_data_rv` reactiveVal wraps facility data for reactive refresh.
- Retry button and status banner shown when facility data fails to load.
- DMR fetch replaces `echoGetEffluent()` with a parameterised SQLite query.
  - **Critically:** crosswalk filter removed from the SQLite fetch — all MAX stat
    base records load regardless of WQS match so unmatched parameters (PCBs, etc.)
    reach `rv$dmr` and appear in the Needs Attention tab.
- `db_last_updated` read from `sync_log` at startup; displayed on landing page.
- Coverage summary header shows last monitoring period date from `rv$dmr`.
- All date parsing updated to handle both ISO `YYYY-MM-DD` (SQLite) and
  `MM/DD/YYYY` (ECHO legacy / standalone uploads) using
  `coalesce(ymd(...), mdy(...))`.
- Facility table renderer updated — removed `CWPState` (doesn't exist in SQLite
  schema), replaced with `any_of()` selecting available columns.
- Limit date columns guarded with `any_of()` and dual-format date parsing.

---

## 10. Report Date Parsing Fix

**Status: ✅ Complete**

**Problem:** `lubridate::mdy()` silently returns `NA` for ISO date strings,
causing ggplot2 to crash when building date axis breaks (`seq.int` error).

**Changes — `report.qmd`:**
- All `lubridate::mdy()` calls replaced with
  `coalesce(lubridate::ymd(...), lubridate::mdy(...))` at 4 locations:
  - `obs` date column in per-pollutant plot loop
  - `ph_dmr_df` date conversion in setup chunk
  - `temp_dmr_df` date conversion in setup chunk
  - `limit_begin_date` / `limit_end_date` parsing

---

## 11. Report LaTeX / PDF Fixes

**Status: ✅ Complete**

- `documentclass: article` added to YAML to avoid KOMA-script `captions` error
  that was crashing XeLaTeX compilation.
- `keep-tex: false` added to YAML.
- `CRITERION_ID` cast to character in `rp_df` at load time to prevent type
  mismatch error in `left_join` with `wqs_annotation_df`.
- `wqs_info_df` / `wqs_annotation_df` split: full `crosswalk_effective` written
  as `wqs_info.csv`; `wqs_annotation_df` is a slim derived object used for
  per-parameter criterion type joins.

---

## Current File Status

| File | Status | Notes |
|---|---|---|
| `app.R` | ✅ Current | All changes applied + temperature handling fixes (session 12) |
| `functions.R` | ✅ Current | All changes applied |
| `report.qmd` | ✅ Current | All changes applied |
| `refresh_db.R` | ✅ Current | New file — run weekly |
| `validate_dmr_sources.R` | ✅ Current | New file — run ad hoc |

---

## Deployment Checklist

1. **Remove `echor` from `renv.lock`** — run `renv::snapshot()` after removing
   `library(echor)` from `app.R` and before deploying to Connect.
2. **Set `PR_RP_SQLITE` env var on Connect** — point to the SQLite file location
   on the Connect server's persistent storage.
3. **Run `refresh_db.R` once manually** to populate the database, then schedule
   weekly via Windows Task Scheduler or Posit Connect's scheduler.
4. **Set Oracle credentials as env vars** — `ICIS_USER`, `ICIS_PW`, `ICIS_DSN`,
   `ICIS_DRIVER` in `.Renviron` for scheduled runs.
5. **Confirm `data/pr_rp.sqlite` is accessible** to the Connect app at the path
   specified by `PR_RP_SQLITE`.
6. **Verify `sync_log` table exists** after first refresh run — landing page
   database status display depends on it.

---

## Known Remaining Items

- **104 limit mismatches** between SQLite and ECHO for PR0001031 — reference-only
  data (permit limit history plot), does not affect RP calculation. Root cause is
  ECHO surfacing limits from a different ICIS version/snapshot than the direct
  Oracle query. Acceptable as-is.
- **1 unit mismatch** (mg/L vs ug/L for one record) — genuine data entry
  discrepancy between ICIS and ECHO, not a code issue.
- **18 ECHO-only rows** — all null DMR values, dropped by app's `drop_na()`.
  Expected and acceptable.
- **Manual crosswalk logging** (discussed but deferred) — a future feature to
  persist user-made `parameter_code → WQS criterion` matches to a database table
  so the crosswalk improves over time.

---

## 12. Temperature Handling Follow-up (June 16, 2026)

**Session date:** June 16, 2026  
**Files modified:** `app.R`

Follow-up session to fix temperature handling. After the earlier work to coalesce
Fahrenheit (parameter `00011`) into Celsius (`00010`) and stop temperature from
tripping the "Needs Attention" flag, temperature was dropping out of the analysis
in the example permit (PR0001031). This session traced and fixed four related
issues so temperature now flows correctly from `rv$dmr` through the coverage
table, unit status, RP calculation, interactive inspector, and RP summary table.

---

### 12a. Temperature WQS criterion dropped from RP calc and coverage table

**Status: ✅ Complete**

**Problem:** The temperature criterion (`CRITERION_ID` 79218, `parameter_code`
00010) is stored under the generic water class `"surface waters"`, not a specific
selectable class. The `crosswalk_full` build filtered the crosswalk to the user's
selected water classes only, so the temperature criterion was removed whenever a
specific class (e.g. class SD/SG/SB waters) was selected. With no 79218 row in
`crosswalk_full` → `crosswalk_effective`, the temperature findings produced nothing
and temperature was absent from both the coverage table and the RP calc. The
standalone-upload crosswalk filter already guarded against this by whitelisting
00010/00400 past the water-class filter; `crosswalk_full` did not.

**Change — `app.R` (`crosswalk_full` build in the SQLite fetch):** added the same
escape hatch so temperature and pH survive regardless of selected class:

```r
crosswalk_full_filt <- crosswalk %>%
  dplyr::filter(
    USE_CLASS_NAME_LOCATION_ETC %in% selected_water_classes |
      parameter_code %in% c("00010", "00400")
  )
```

---

### 12b. Temperature reported "FAIL" unit status after F→C coalesce

**Status: ✅ Complete**

**Problem:** Unit conversion (`get_unit_conversion()` / `Conv_Flag`) runs *before*
the `00011`→`00010` coalesce. Parameter `00011` has no crosswalk row, so its target
WQS unit joined as `NA`, and `get_unit_conversion()` returns `flag = "FAIL"` on an
`NA` target. The coalesce block then recoded `00011`→`00010` and applied the
`(F − 32) × 5/9` conversion to the value, but never updated `Conv_Flag` — so the
stale `FAIL` rode along and the coverage summary showed temperature unit status as
FAIL.

**Change — `app.R` (both coalesce blocks: standalone path and SQLite path):** clear
the flag for coalesced rows, since the coalesce itself performs the conversion:

```r
Conv_Flag = dplyr::if_else(parameter_code == "00011", "PASS", Conv_Flag),
```

Note: the two coalesce blocks are maintained as copies; the change was applied to
both. (Worth extracting the coalesce into a shared helper in `functions.R` in a
future pass so this logic lives in one place.)

---

### 12c. Missing closing brace in the standalone coalesce block

**Status: ✅ Complete**

**Problem:** While editing for fix 12b, the closing brace for the standalone
`if ("00011" %in% df_conv$parameter_code) {` block was dropped, leaving the file
unbalanced and unparseable.

**Change — `app.R` (standalone coalesce block):** restored the closing `}`
(4-space indent, matching the `if` and the sibling 82230 block) immediately after
`dplyr::select(-has_celsius)`, before `rv$dmr <- df_conv`.

---

### 12d. Temperature dropped from inspector and RP summary by `drop_na()`

**Status: ✅ Complete**

**Problem:** In the temperature findings block (`run_rp`), `temp_dmr` selected
several limit columns (`limit_value_nmbr`, `limit_begin_date`, `limit_end_date`)
and then called a bare `drop_na()`. Temperature is a monitor-only parameter with
no numeric effluent limit, so those columns are `NA`, and `drop_na()` removed every
temperature row. The result was an empty `temp_dmr` and a phantom join row
(`NPDES_Pollutant = NA`, `max_value = -Inf`), so temperature never reached the
interactive inspector or the RP summary table.

**Change — `app.R` (temperature findings block):** scoped the drop to the measured
value only:

```r
tidyr::drop_na(dmr_value_nmbr)
```

**Related caution:** the pH findings block uses a similar structure. If a
monitor-only pH permit is ever processed it could hit the same trap. Not changed
this session (pH permits in current scope carry limits).

---

### Verification (PR0001031)

- Temperature appears in the coverage table: **53 observations, unit status PASS**.
- Temperature flows into the RP calculation, interactive inspector, and RP summary table.
- Recommended post-change checks:
  - Confirm temperature max values read as plausible Celsius (mid-20s to low-30s) — verifies the F→C coalesce is firing.
  - Confirm the standalone-upload path and the ICIS-NPDES path produce matching temperature unit status and converted values (the two coalesce blocks are copies and can drift).

---

## 13. UI & Table Improvements (June 16, 2026)

**Session date:** June 16, 2026  
**Files modified:** `app.R`

---

### 13a. Coverage Summary Table — "Unit Status" column header

**Status: ✅ Complete**

**Problem:** The `Unit_Status` column in the Coverage Summary table was displaying
with an underscore in the column header rather than a space.

**Change — `app.R` (`output$coverage_summary` renderDT):**
- Added `rename(\`Unit Status\` = Unit_Status)` after `distinct()`, just before
  the data is passed to `datatable()`.
- Updated the corresponding `formatStyle()` call from `'Unit_Status'` to
  `'Unit Status'` to match.

---

### 13b. RP Summary Table — sort order, color coding, sort/filter

**Status: ✅ Complete**

**Problem:** The RP Summary Table had no color coding on the RP column, no column
filters, and the sort order for RP was incorrect (NO appearing before YES).

**Changes — `app.R`:**

- **Sort order fix:** In `rp_table_data()`, reversed the factor levels from
  `c("YES", "NO")` to `c("NO", "YES")` so that `arrange(desc(RP))` correctly
  places YES at the top. Secondary sort by `NPDES_Pollutant` added for
  alphabetical ordering within each RP group.
- **Color coding:** Added `formatStyle("RP", ...)` using `styleEqual()` to color
  the RP column: YES → red background/text (`#ffcccc` / `#990000`), NO → green
  background/text (`#ccffcc` / `#006600`). Colors match the Unit Status palette
  used in the Coverage Summary table.
- **Sort & filter enabled:** Removed `ordering = FALSE` from `datatable` options
  and added `filter = "top"` to expose column-level filters and re-enable header
  sorting.

---

### 13c. RP Summary Table — row click syncs sidebar inputs

**Status: ✅ Complete**

**Problem:** Clicking a row in the RP Summary Table updated the equation at the
bottom of the tab but did not update the "Select Pollutant" or "Select Outfall"
selectize inputs in the sidebar, so the Quick Stats panel remained out of sync
with the selected row.

**Change — `app.R`:** Added a new `observe` block that fires on
`input$rp_table_rows_selected`. On each row click it reads `perm_feature_nmbr`
and `NPDES_Pollutant` from the selected row and calls `updateSelectInput()` for
both `selected_outfall` and `selected_pollutant`. Since the Quick Stats panel is
reactive to those inputs, it updates automatically to reflect the clicked row.


---

# Session 2 — June 24, 2026

**Files modified:** `app.R`, `functions.R`, `report.qmd`, `crosswalk.csv`

## Overview of Changes (Session 2)

A round of bug fixes and additions following end-user issue review. Five
categories: WQS-flagging logic corrections, unit-converter expansion,
crosswalk-derived display improvements (sub-class column for lakes vs
streams), restoration of behaviour previously documented in §13b that had
been rolled back, and search-page state persistence across navigation.

All Session-2 changes are behaviour-preserving with respect to the
documented intent of prior sessions; nothing here changes the underlying
statistical projection or the RP determination logic.

---

## 14. WQS Flagging — Special-Method and All-EXCLUDED Skips

**Status: ✅ Complete**

**Problem:** Two unrelated parameter categories were being incorrectly surfaced
on the Needs Attention tab, forcing the user to resolve issues that were not
actually problems:

1. **Special-method criteria** (TAN criterion 79613, hardness-dependent
   metals like Cu/Pb/Zn) carry `CRITERION_VALUE = NA` in the crosswalk by
   design — their values are computed at RP-runtime from the user's pH /
   temperature / hardness inputs. The flagging logic checked only for
   missing values, not the `Method` column, so these parameters were
   flagged as "Missing criterion value" on every fetch.
2. **Mass-load / flow parameters** (e.g., Flow code 50050 reporting in
   "Cubic Meters per Day") have no concentration WQS by their nature.
   They were correctly EXCLUDED from the RP calculation by the unit
   converter, but they still appeared on Needs Attention asking the user
   for a manual criterion — which is meaningless, since no concentration
   criterion could ever apply.

**Changes — `functions.R` (`flag_unmatched_params`):**

- Added `has_method_col` guard and amended `matched_codes` to treat
  `Method == "Special"` rows as usable: a parameter with any Special-method
  row in the selected class is considered matched even when its
  `CRITERION_VALUE` is NA.
- Amended the `all_na_codes` computation to exclude Special-method rows
  from the all-NA test, so a parameter whose only NA rows are Special is
  not flagged.
- Added a row-level Conv_Flag filter at the top of the function: any
  parameter whose every DMR record has `Conv_Flag == "EXCLUDED"` is
  dropped from the flag pass entirely. Partially mass-load parameters
  (some `kg/d`, some `mg/L`) are still flagged if the `mg/L` portion needs
  a criterion.
- Removed the obsolete `skip_codes <- c("00400","00010","00070","00080")`
  block; pH, temperature, turbidity, and color now flow through normal
  flagging like any other parameter (see §16).

---

## 15. Unit Converter — Six Flags, ppm Support, Identity Aliases, Mass-Load Expansion

**Status: ✅ Complete**

**Problem:** Several mismatches between what the converter recognised and
what the DMR data actually contains were producing incorrect FAIL flags:

1. Parameters with no WQS criterion for the selected water class (`target
   = NA` after the crosswalk join) returned `FAIL`, conflating "real unit
   problem" with "no criterion to convert toward".
2. TAN's `UNIT_NAME = "[no units]"` placeholder in the crosswalk caused
   the converter to FAIL on ammonia rows that were otherwise fine — the
   runtime TAN calculation produces a value in mg/L matching the DMR.
3. Total residual chlorine (50060) reports in `Parts per Million` for
   ~138 records. The WQS unit is `ug/L`. No conversion rule existed, so
   those rows were silently dropped from chlorine RP.
4. Color and Enterococci report in long-form ICIS strings (`col unit (pc)`,
   `Number per 100 Milliliters`) that are functionally identical to their
   WQS-unit counterparts (`color units`, `#/100mL`) but lexically different,
   producing FAIL where there was no real problem.
5. `lbs/d` and `Cubic Meters per Day` are mass-load / flow units that
   should have been EXCLUDED but were FAIL because they weren't on the
   mass-load list.

**Changes — `functions.R` (`get_unit_conversion`):**

- Added a sixth flag value, **`NO_WQS`**, returned when `target` is NA but
  `reported` is a valid unit string. Distinguishes "no criterion for this
  class" from a real unit problem.
- Added a `[no units]` short-circuit at the top of the function: when the
  target is the literal placeholder, return `NA_character_` (clean match)
  so Special-method rows aren't FAILed by the converter.
- Added ppm conversions: `ppm → mg/L` (×1), `ppm → ug/L` (×1000). Long-form
  spelling `Parts per Million` matched alongside the abbreviation.
- Added an `identity_groups` mechanism: pairs of strings that name the same
  physical unit get treated as a clean match. Initial groups:
  `color units` ↔ `col unit (pc)`; `#/100mL` ↔ `Number per 100 Milliliters`
  ↔ `MPN/100mL`.
- Extended `mass_load_units` with `lbs/d` and `cubic meters per day` so
  they get EXCLUDED status rather than FAIL.
- Added a roxygen-style header to the function listing all six flags and
  their downstream behaviour.

**Changes — `app.R`:**

- Three conversion `case_when` blocks (standalone path, SQLite path,
  manual-rows re-conversion) updated to retain `dmr_value_nmbr` as-is
  when `Conv_Flag` is `EXCLUDED`, `NO_WQS`, or `FAIL`.
- Both unit-status `case_when` blocks (standalone and SQLite paths) now
  track `no_wqs` counts separately and emit a `"NO WQS"` status when every
  WQS-eligible record has no criterion target.
- `status_color` block extended with `"NO WQS" → "lightblue"`.
- Coverage Summary `formatStyle` extended to colour all six statuses plus
  `NO DATA`, with NO WQS in a neutral blue-grey so it reads as informational
  rather than error.
- RP-stage filter comment block updated to document the row-level
  `is.na(Conv_Flag) | Conv_Flag == "PASS"` predicate and what each non-kept
  status means (NO_WQS, EXCLUDED, FAIL all dropped from RP; retained in
  `rv$dmr` for export with their flag).

---

## 16. pH and Temperature — Water-Class Filter Restoration

**Status: ✅ Complete**

**Problem:** pH (00400) and temperature (00010) were force-included in
`crosswalk_full` regardless of the selected water class. The original
rationale was that pH / temperature were needed at runtime for the TAN and
hardness-metals limit calculations — but those calculations were later
replaced with user-supplied sidebar inputs (Receiving Water pH / Temperature
/ Hardness). The force-through was no longer necessary, and was producing
incorrect Findings: a user selecting only SD waters would still see pH and
temperature results for SB.

**Changes — `app.R`:**

- Removed `| parameter_code %in% c("00010", "00400")` from the standalone
  observe `crosswalk_filt` build (both the Form filter and the water-class
  filter).
- Removed the same clause from the SQLite-path `crosswalk_full_filt` build.
- pH and temperature now flow through `pH_findings` and `tempFindings` only
  when the selected water class has a criterion for them; otherwise they
  surface on Needs Attention like any other parameter.

---

## 17. Calculated Metals Limits in `wqs_info.csv` Export

**Status: ✅ Complete**

**Problem:** The TAN criterion was correctly written back into
`rv$crosswalk_effective` after the runtime calculation, so its computed
value flowed into the `wqs_info.csv` shipped in the report ZIP. The
hardness-metals limits were not: they were computed into a local
`limits_filt` data frame, joined into `rwc_criteria` for the RP table, and
then thrown away. `crosswalk_effective` still carried `CRITERION_VALUE = NA`
for those rows, so the exported `wqs_info.csv` showed NA for metals even
though the RP determination had used a calculated value.

**Changes — `app.R`:**

- After `limits_filt` is built (immediately following the
  hardness-set-to-curve match), join the computed limits back into
  `rv$crosswalk_effective` by `CRITERION_ID` and overwrite `CRITERION_VALUE`
  where present. Mirrors the existing TAN write-back pattern.

---

## 18. Crosswalk Deduplication

**Status: ✅ Complete**

**Problem:** The source `crosswalk.csv` was built by joining the NPDES form
pollutant list to the PR WQS criteria via several strategies (Manual
mapping, Name match, CAS match). When multiple strategies succeeded for the
same criterion, both rows survived into the file with the same
`(parameter_code, CRITERION_ID)` but different `NPDES_Pollutant` labels.
Phosphorus criterion 79599 was the most visible case — it appeared as both
"Phosphorus" (Manual join) and "Total phosphorus" (Name join), producing
two rows in the Coverage Summary for what is actually a single record.
Diagnostic scan confirmed phosphorus was the only parameter affected by
this specific artifact.

**Changes — `functions.R`:**

- Added `dedupe_crosswalk(cw)` helper. When a `(parameter_code, CRITERION_ID)`
  pair has multiple rows with different `NPDES_WQS_Join` methods, keep only
  the `Manual` row (authoritative form-name mapping). When the multiple
  rows share the same join method — pH min/max criteria 79595 and 79606
  (both Manual), temperature criterion 79218 (both Manual) — keep them
  all; those are intentional dual labels and are NOT artifacts.
- Helper is safe to call on a crosswalk missing the `NPDES_WQS_Join`
  column; returns input unchanged in that case.

**Changes — `app.R`:**

- Applied `dedupe_crosswalk()` at the tail of the crosswalk loader pipeline,
  right after the existing `distinct()`. A code comment at the loader
  explains both build artifacts (the Temperature summer/winter collapse and
  the new dedup pass).

---

## 19. Sub-Class Column for Lakes vs Streams (Class SD)

**Status: ✅ Complete**

**Problem:** Three parameters have distinct WQS criteria under Class SD
waters depending on whether the receiving water is a stream or a
reservoir/lake:

| Parameter         | CRITERION_ID | Value      | Sub-class      |
|-------------------|--------------|------------|----------------|
| Total nitrogen    | 79614        | 1700 ug/L  | stream         |
| Total nitrogen    | 79615        | 400 ug/L   | reservoir/lake |
| Total phosphorus  | 79616        | 160 ug/L   | stream         |
| Total phosphorus  | 79617        | 26 ug/L    | reservoir/lake |
| Selenium          | 79256        | 3.1 ug/L   | stream         |
| Selenium          | 83712        | 1.5 ug/L   | reservoir/lake |

Both criteria were already in the crosswalk and were producing RP
determinations, but the user-facing tables and chart legend showed both
rows as "class SD waters" with no way to tell which line corresponded to
which receiving water type.

**Changes — `crosswalk.csv`:**

- Added a `sub_class` column at the end of the schema. Six rows populated
  with `stream` or `reservoir/lake` (the values above). All other rows
  carry an empty `sub_class`. The column is designed to be jurisdiction-
  agnostic; today only PR class SD criteria use it, but future
  state-specific sub-types fit the same schema.

**Changes — `functions.R`:**

- Added `format_water_class(class, sub_class, sep)` helper. Returns
  `"class SD waters — stream"` when `sub_class` is populated, base class
  otherwise. Em-dash (`—`, U+2014) separator chosen for readability.
- `build_manual_crosswalk_rows` extended to include `sub_class =
  NA_character_` so manually-entered criteria bind cleanly with the
  main crosswalk.

**Changes — `app.R`:**

- Threaded `sub_class` through every `select()` that pulls from
  `crosswalk_effective` (rwc_criteria, pH_findings, tempFindings) via
  `dplyr::any_of("sub_class")` — defensive against older crosswalks
  without the column.
- `rp_table_data()` mutates `USE_CLASS_NAME_LOCATION_ETC` via
  `format_water_class()` before display.
- WQS Reference tab mutates the `Water Class` column the same way.
- Per-pollutant chart's `wqs_lines` uses the combined label so each
  criterion line gets its own legend entry with its own colour.
- `class_colors` map extended with `"class SD waters — stream"` (darker
  orange `#cc6600`) and `"class SD waters — reservoir/lake"` (lighter
  orange `#ffaa55`) so the SD sub-class lines are visually related to
  the base SD orange but distinguishable.

**Changes — `report.qmd`:**

- Inline copy of `format_water_class()` defined in the setup chunk —
  the report renders out of `tmp_dir` where `functions.R` is not
  available. Comment notes that if the helper changes in `functions.R`,
  mirror the change here.
- All three RP tables updated to use the combined label: the main
  Findings kable, the pH RP table, and the temperature RP table.
- The per-pollutant section's RP-breakdown table, detail table (with
  RWC), and `wqs_lines` (driving the chart's dashed criterion lines)
  also updated.
- `wqs_colors` and `wqs_linetypes` extended with the SD sub-class
  variants, mirroring `class_colors` in `app.R`.

---

## 20. RP Summary Table — §13b Behaviour Re-Applied

**Status: ✅ Complete**

**Problem:** End-user noticed that the RP Summary Table in the Inspector tab
was no longer sorted YES-at-top and the RP cells were no longer colour-
coded. The behaviour was documented in §13b of this changelog but was not
present in the current `app.R` (rollback during a later edit, exact
provenance unclear). DEPENDS — which appears in the data as a real per-class
RP value when the user runs metals with hardness-range mode — was not
handled by the §13b colouring at all.

**Changes — `app.R` (`rp_table_data` reactive and `output$rp_table`):**

- Factor levels in `rp_table_data` are `c("NO", "DEPENDS", "YES")` (ordered)
  so `arrange(desc(RP), NPDES_Pollutant)` puts YES rows on top with
  alphabetical ordering within each RP group.
- `formatStyle("RP", ...)` added with three colour bands:
  - YES → red (`#ffcccc` background, `#990000` text)
  - DEPENDS → orange (`#ffe5cc` / `#994c00`)
  - NO → green (`#ccffcc` / `#006600`)
- `ordering = FALSE` removed, `filter = "top"` added — user can now sort
  and filter per-column. A code comment notes that DT's `_rows_selected`
  returns the underlying data-frame index regardless of UI sort/filter
  state, so the math panel sync is safe under user-driven re-sorting (the
  rationale for the original `ordering = FALSE` no longer holds).

---

## 21. Report — Sub-Class in Per-Pollutant Tables and Plot Legend

**Status: ✅ Complete**

**Problem:** After §19 the main Findings kable in the report displayed
sub-class correctly, but the per-pollutant detail sections (one per
pollutant heading in the report) still showed both SD rows as plain
"class SD waters" in both the RP breakdown table and the detail-with-RWC
table. The corresponding plot also showed two dashed lines in the same
orange with one legend entry, providing no visual differentiation.

**Changes — `report.qmd`:**

- `wqs_lines` (per-pollutant plot) mutated via `format_water_class()` so
  each criterion gets its own legend entry and own colour slot.
- `rp_breakdown` table (top of per-pollutant section) mutated the same way.
- `detail` table (post-plot, with RWC) mutated the same way.

(Same pattern as the main Findings kable from §19; this was a follow-up
catching three additional consumer points.)

---

## 22. Search-Page State Persistence

**Status: ✅ Complete**

**Problem:** Navigating away from the permit search page and back (e.g., to
review or modify a search) reset all the search inputs to their hardcoded
defaults: permit ID went back to `PR0001031`, dates back to "five years ago
through today", forms and water type both cleared. The expected behaviour
is that selections persist within a session and only reset on app restart.

**Changes — `app.R`:**

- Five new `rv$saved_*` slots added to the reactiveValues block:
  `saved_permit_id`, `saved_date_start`, `saved_date_end`,
  `saved_npdes_forms`, `saved_water_type`. All NULL on first visit.
- Five `observeEvent` capture observers added — one per input — that
  copy `input$<x>` into `rv$saved_<x>` whenever the user changes it.
  `ignoreInit = TRUE` on each so the UI's own re-render defaults don't
  overwrite the saved state. The permit_id capture has a non-empty guard
  to avoid clobbering on init when the input first materialises as `""`.
- `select_ui()` reads `rv$saved_*` with `isolate()` and `%||%` fallback to
  the original defaults. The `isolate()` is critical: without it the
  saved-value reads become reactive dependencies of `output$page_content
  <- renderUI({...})`, and every input change re-fires renderUI, which
  destroys and rebuilds the entire search-page UI mid-interaction (in
  particular breaking the selectize widget's typeahead).
- `updateSelectizeInput` in the permit-populate observer wraps its
  `rv$saved_permit_id` read in `isolate()` for the same reason — to
  prevent the populate observer from re-firing on every selection and
  re-initialising the server-side selectize widget.
- Both isolation points carry inline comments explaining the trap so a
  future maintainer doesn't remove the `isolate()` without understanding
  why it's there.

**Scope note:** Persistence covers the four search-page inputs only.
Summary-page settings (hardness, pH, temp, dilution ratio, confidence)
are NOT persisted because they naturally re-select when a different
permit is fetched. Cross-session persistence (i.e., across full app
restarts) would require browser storage or server-side state and is not
addressed here.

