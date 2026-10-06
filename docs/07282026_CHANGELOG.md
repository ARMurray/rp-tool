# PR RP Calculator — Changelog & Current Status

**Session date:** July 28, 2026
**Files modified:** `app.R`, `functions.R`, `report.qmd`, `crosswalk_v2.csv`, `NPDES_Forms_Pollutants_1.csv`, `User_Guide_html.qmd`
**Files added:** `crosswalk_v2.csv`, `statistic_lookup.csv`, `build_crosswalk_v2.py`

---

## Overview of Changes

This session began as a batch of bug reports from two permit writers (Laura and
Siegi) on the Viatris and PREPA San Juan permits, and grew into a structural
change in how the crosswalk describes each criterion. The unifying theme: the
original app treated **every** criterion as a maximum-concentration ceiling to be
projected through the TSD multiplying factor. That assumption holds for most
parameters and fails for exactly the ones that generated the bug reports —
dissolved oxygen (a floor), Enterococci (interval statistics), color/turbidity
(direct comparison), oil & grease (narrative), and pH/temperature (already
special-cased with hardcoded logic).

Rather than add another special case per parameter, the crosswalk was given
**seven new columns that let each row declare its own evaluation semantics**.
This let several hardcoded blocks be deleted rather than extended.

A recurring secondary theme was **synonym parameter codes**: the same analyte
reported under a DMR code different from the crosswalk code, causing silent
non-matches. This appeared four times (ammonia, Enterococci, surfactants,
sulfate) and was fixed in each confirmed case; the remaining candidates were
deliberately routed to Needs Attention because they carry regulatory judgment.

**⚠ Verification status:** Most changes in this session were validated by R
`parse()` (syntax) and structural checks only. The app was run successfully by
the reviewer at several checkpoints, but a number of later changes — the
missing-statistic flag, the manual floor selector, the generic direct-parameter
report block, the log-axis/unit-flag, and the User Guide — have not been
executed end-to-end. See the "Testing status" section at the end.

---

## 1. Crosswalk Semantics Overhaul — Seven New Columns

**Status: ✅ Complete (data + code consuming it)**

**Problem:** The crosswalk's single `Method` column overloaded three unrelated
concepts (comparison direction, criterion form, and a null default), and could
not express a floor criterion, an interval statistic, or which DMR statistic a
criterion should be compared against. Extraction (which DMR records were pulled)
and comparison (which value was tested) were governed by separate hardcoded
logic that could — and did — disagree.

**Solution:** A new crosswalk file, `crosswalk_v2.csv`, adds seven columns.
`Method` is retained unchanged for backward compatibility during migration.

| Column | Values | Purpose |
|---|---|---|
| `rp_method` | `direct` \| `projected` | Selects evaluation path: direct comparison (no MF, no dilution) vs TSD projection |
| `rp_operator` | `>` \| `<` | **Violation-direction**: the operator IS the RP test. `>` = ceiling (RP if value exceeds), `<` = floor (RP if value below). Strict inequality — a value exactly on the criterion is compliant |
| `statistic` | `DAILY_MAX` \| `DAILY_MIN` \| `GEOMEAN` \| `P90` \| `AVG` | Semantic token driving **both** which DMR records are extracted and which value is compared |
| `criterion_form` | `numeric` \| `formula` \| `narrative` \| `binary` \| `none` | What kind of criterion this is; drives Needs Attention routing |
| `criterion_label` | free text | Display-name override; `NPDES_Pollutant` stays the forms-join key |
| `no_exceed_rp` | `NO` \| `DEPENDS` | The determination returned when NO excursion is observed (DO/temp/Enterococci → DEPENDS) |
| `rp_note` | free text | Criterion-specific guidance surfaced in Quick Stats and the report |

**Key design decisions and their reasoning:**

- **`rp_operator` is violation-direction, not compliance-direction.** The
  reviewer's phrasing ("it needs to exceed the limit to trigger RP") is
  self-documenting and makes dropping the `=` fall out naturally. Encoded so
  `RP = get(operator)(value, criterion)` is one expression for every row.
- **Strict inequality (dropping `=`).** PRWQS wording is "shall not exceed" /
  "shall not contain less than" / "outside the range of" — a value exactly on
  the criterion is compliant. This flips some previously-YES pH findings (pH
  values sit on 6.0/9.0 constantly); intended, not a regression.
- **Floating-point tolerance.** With strict `>`, unit-conversion error could
  land a value spuriously on the wrong side of the boundary. A relative
  tolerance (`1e-9`) guards the comparison.
- **`statistic` as a lookup token, not raw ICIS codes.** The messy ICIS
  `statistical_base_short_desc` vocabulary (`GEO MEAN`, `MO GEO`, `30DA GEO` all
  mean geometric mean) lives in a separate `statistic_lookup.csv` mapping tokens
  to `(type_code, short_desc)` pairs, so new spellings are a one-line edit
  rather than a 545-row migration.

### `functions.R` — new helpers
- `tag_statistic(dmr, lookup)` — adds a `statistic` column to a DMR frame by
  matching ICIS codes against the lookup; defaults to `DAILY_MAX` when the frame
  lacks the ICIS stat columns (standalone uploads).
- `rp_exceeds(value, criterion, op, tol)` / `rp_label(...)` — evaluate the RP
  test using `rp_operator`, with the tolerance and a `no_exceed` fallback.
- `resolve_display_name(criterion_label, NPDES_Pollutant, parameter_desc)` —
  coalesces the display label without touching the join key.
- `compute_rwc()` grouping gained `statistic` as a key, so Enterococci's two
  criteria are projected from their own series rather than pooled.

### `app.R`
- Startup loads `crosswalk_v2.csv` and `statistic_lookup.csv`, with a
  `stopifnot()` asserting every crosswalk `statistic` exists in the lookup.
- Fetch filter (was `MAX`-only) replaced with `tag_statistic()` + a
  required-statistic join, so DO minimums and Enterococci geomeans survive.
- The hardcoded exclusion vector `c("00070","00010","00080","00400")` replaced
  with a branch on `rp_method == "direct"`.
- The projected path filters on `rp_method == "projected"` and joins on
  `(parameter_code, statistic)`.
- The pH and temperature findings blocks were **replaced by one generic
  `direct_findings` block** serving every `direct` row, keyed off `rp_operator`
  (observed max for `>`, observed min for `<`).

---

## 2. Dissolved Oxygen — Floor Criterion, No RWC

**Status: ✅ Complete**

**Problem (Laura & Siegi):** DO had ICIS data but none in the RP tool download;
it never produced a result.

**Root cause:** The `MAX`-only fetch filter discarded DO's `MIN` records — and
DO is a floor, so the minimum is the only compliance-relevant value. The one
statistic that mattered was the one being deleted.

**Reasoning for the fix:** Per the reviewer, DO should not run through the RWC
projection at all. A defensible DO analysis needs the Streeter-Phelps equation,
whose inputs (reaeration/deoxygenation rates, travel time, upstream deficit) are
not in ICIS-NPDES. Applying the TSD multiplying factor to DO is wrong in three
ways: it projects an upper bound when DO needs a lower bound, the MF is derived
for an upper percentile, and dilution does not attenuate a DO deficit the way it
attenuates a pollutant.

**Changes:** DO (00300) is `rp_method = direct`, `rp_operator = <`,
`statistic = DAILY_MIN`, `no_exceed_rp = DEPENDS`. It plots the observed minimum
against the 5.0 mg/L line: **YES** if below, **DEPENDS** if not (absence of an
excursion cannot rule out RP without Streeter-Phelps). A note to this effect is
carried in `rp_note`.

---

## 3. Enterococci — Interval Statistics, No RWC

**Status: ✅ Complete**

**Problem (Laura):** Enterococci missing for some outfalls (001/002 present in
ICIS, only 003 in the download).

**Root cause:** The `MAX`-only filter discarded `GEO MEAN` records — but the
PRWQS SB/SD Enterococci criterion IS a geometric mean (35/100 mL over any 90-day
interval), plus a 90th-percentile criterion (130/100 mL). The tool was keeping
the wrong half of the data.

**Reasoning for the fix:** After discussion, the reviewer determined neither
criterion should be projected: the geometric mean and 90th percentile are
frequency- and interval-based statistics, not maximum concentrations, so the MF
projection is not a meaningful operation on them. Both rows are
`rp_method = direct` with `no_exceed_rp = DEPENDS` and a boilerplate note
instructing the permit writer to evaluate the geometric mean and 90th percentile
over the applicable 90-day intervals independently.

**Changes:** Enterococci criteria (35 GEOMEAN, 130 P90) split into distinct rows
per class with distinct `criterion_label`s. Alternate DMR codes **31639** and
**51618** (present in DMR data, absent from crosswalk) were cloned onto the
61211 criteria.

---

## 4. Color and Turbidity — Removed From Hardcoded Exclusion

**Status: ✅ Complete**

**Problem (Laura):** Color and turbidity had standards, DMR data, and appeared
in Data Overview, but never in RP Results.

**Root cause:** Both were in the hardcoded exclusion vector
`c("00070","00010","00080","00400")` that removed pH and temperature (which had
dedicated handlers) — but color (00080) and turbidity (00070) had **no handler
downstream**, so they were excluded from the calc and never re-added. Laura's
guess that they were "accidentally scrapped" was correct; the mechanism was this
line, not the forms filter.

**Changes:** Both are now `rp_method = direct` ceilings (color 15 color units SD;
turbidity 50 NTU SD, 10 NTU SB) and flow through the generic direct block.

---

## 5. Temperature — Direct, But DEPENDS (In-Stream Standard)

**Status: ✅ Complete**

**Problem:** Temperature appeared in the PDF report but not the data summary.

**Root cause (two parts):** (a) The `00011`→`00010` (°F→°C) recode rewrote the
code and unit but left `parameter_desc` as the Fahrenheit description, which
later caused a duplicate-input-ID crash (see §8). (b) Temperature's crosswalk
rows carry the pseudo-class `surface waters`; a class-mapping question (see §12)
affected its visibility.

**Reasoning:** Per Siegi, temperature must stay in a "needs attention" posture
because the standard is "shall not cause the temperature of **any site** to
exceed 30°C" — an in-stream standard, not end-of-pipe. An end-of-pipe comparison
is screening only.

**Changes:** Temperature (00010) is `rp_method = direct`, `rp_operator = >`,
`no_exceed_rp = DEPENDS`, with a note explaining the in-stream basis. YES if the
observed max exceeds 30°C; DEPENDS otherwise.

---

## 6. BOD5 — Correctly Flagged, Not Silently Dropped

**Status: ✅ Complete**

**Problem (Siegi):** BOD5 had data in ICIS but never surfaced.

**Root cause:** The `MAX`-only filter deleted BOD's `AVG` records before the
flagging logic could see them, so it disappeared entirely instead of being
flagged.

**Reasoning:** Confirmed against PRWQS Rule 1303.1.F — BOD has **no numeric
in-stream criterion** (it is set case-by-case to assure compliance with the DO
standard). So BOD correctly belongs in Needs Attention; it just needed to
actually reach it. The statistic-tagging fetch change keeps uncriterioned
parameters in `rv$dmr` so they flag rather than vanish.

---

## 7. Oil & Grease — Binary Detection Criterion

**Status: ✅ Complete**

**Problem:** Oil & grease WQS was never in the crosswalk.

**Reasoning:** PRWQS Rule 1303.1.H is **narrative** — waters shall be
substantially free from oils and greases. There is no numeric value. The
reviewer specified it should be binary: any positive value triggers RP.

**Changes:** Four O&G codes (00550, 00552, 00556, 00560) added as
`criterion_form = binary`, `CRITERION_VALUE = 0`, `rp_operator = >`,
`rp_method = direct`, unit-agnostic. `format_criterion()` renders the criterion
appropriately (the stored `0` is a detection threshold, not a numeric limit of
zero). Each code carries a distinct `criterion_label` (method-qualified) so they
don't collapse to one dropdown entry and confuse the plot's parameter-code
selection.

---

## 8. Duplicate Shiny Input IDs — Crash Fix

**Status: ✅ Complete**

**Problem:** "Duplicate input IDs were found" client error; two identical
temperature cards in Needs Attention.

**Root cause:** `flag_unmatched_params()` joined a per-parameter description
table by `parameter_code`. Temperature had **two** `parameter_desc` values
(from the °F→°C recode leaving the Fahrenheit description in place), so the join
fanned the flagged row into two, and the renderer emitted duplicate
`flag_*_00010` input IDs.

**Changes:** (a) The recode now rewrites `parameter_desc` too. (b) `dmr_desc`
collapses to one row per `parameter_code` via `slice(1)`, so no future
description mismatch can fan out again. A second statistic filter was added
after the 82230/00011 recodes so surplus statistics can't leak onto the recoded
parameter.

---

## 9. Phosphorus / Total Phosphorus Naming

**Status: ✅ Complete**

**Problem (Siegi):** The RP tool labels 00665 "Phosphorus" while the WQC,
permit, and DMR all use "Total Phosphorus."

**Root cause:** A duplicate crosswalk row. `dedupe_crosswalk()` keeps the
`Manual`-join row, which was named "Phosphorus." Worse, the forms file has
"Phosphorus" on form 2A only and "Total phosphorus" on form 2F only, so on a
2C/2F permit phosphorus dropped out of the form-filtered view entirely.

**Changes:** `criterion_label = "Total Phosphorus"` (display only, join key
untouched); the surviving dedupe row renamed to "Total phosphorus" to match form
2F; a "Total phosphorus" → 2A alias added to the forms file so 2A permits still
match.

---

## 10. Ammonia (TAN) — Code Coalescing and Formula Bug

**Status: ✅ Complete**

**Problem (Laura):** Total Ammonia Nitrogen needed calculating; not showing.

**Root causes (two):**
1. The app coalesced only **82230** → 00610, not **00609** — which is the code
   the permit actually reported under. A permit reporting 00609 got no TAN
   evaluation.
2. **A latent regulatory error**: there were two copies of the TAN formula — the
   authoritative `calc_tan_limits()` in `functions.R` and an inline duplicate in
   the Run RP handler in `app.R`. The inline copy (the one that actually runs)
   **dropped the leading `0.8876` coefficient**, inflating every computed TAN
   criterion by ~12.7% and making the standard artificially lenient.

**Changes:** `AMMONIA_ALIAS_CODES <- c("82230","00609")` coalesced at both call
sites (with `parameter_desc` rewritten to avoid the §8 fan-out). The `0.8876`
coefficient restored in `app.R`, with both copies annotated to keep them in
sync. Confirmed against the reg that TAN is **Class SD-only** — no SB criterion
to add. **Any past TAN determinations should be re-run.**

---

## 11. Surfactants (MBAS) — Synonym Code, Not Missing

**Status: ✅ Complete**

**Problem (Laura):** Surfactants as MBAS has a numeric PRWQS standard but was
flagged Needs Attention; not showing in WQS Reference for SD waters.

**Root cause:** The criterion was **never missing** — it existed under code
**47021** ("methylene blue active substances") with correct values (SB 500, SD
100 µg/L) and real registry IDs. The permittee reports under **38260**
("Surfactants [MBAS]"). Same analyte, different code — the join never happened.
This is the third instance of the synonym-code failure mode.

**Changes:** Both 47021 and 38260 aligned to the same criteria (SB 500, SD 100
µg/L numeric; SG "shall not be present" as binary detection). A Class SG
detection row was added (neither code had one).

---

## 12. "Surface Waters" Class Handling — Correction

**Status: ✅ Complete (reverted an incorrect earlier fix)**

**Note on process:** An earlier fix in this session force-appended
`"surface waters"` to the selected classes on the theory that it was not a
user-selectable option. **That theory was wrong** — the reviewer pointed out
`"surface waters"` IS a choice in the Water Type selector. The force-append made
the selector's own option meaningless and pulled the Rule 1303.1 general
standards (temperature, asbestos, radium-226, strontium-90, gross beta, oil &
grease) into scope even when a writer deliberately narrowed to one class.

**Changes:** Reverted at all three class-mapping sites (the reactive helper, the
fetch handler, and the Run RP handler — the third was initially missed). The
general-standard parameters now appear only when "Surface Waters" or "All" is
selected. **Consequence to communicate to users:** temperature and oil & grease
no longer appear unless that class is selected, which may surprise writers who
expect them always.

---

## 13. Sulfate — Fourth Synonym Code

**Status: ✅ Complete**

**Problem:** Sulfate had DMR data but no RP result.

**Root cause:** DMR reports code **51865** (Sulfates, total [as SO4]); the
criterion lives under **00945** (SB 2800, SD 250 mg/L). Same analyte, full class
coverage, plain concentration — the clean synonym pattern.

**Changes:** The 00945 rows cloned onto 51865.

**Deliberately NOT fixed (routed to Needs Attention instead):** copper/nickel
dissolved (hardness-formula rows, dissolved-vs-total fraction is a judgment
call), aluminum, chromium (hexavalent vs total), arsenic, chlorine, cyanide,
TDS, fluoride, total nitrogen. Each carries a regulatory distinction the permit
writer should resolve, so per the reviewer's instruction they flow to Needs
Attention where a criterion can be entered by hand. A green **Hint** was added
to each pointing at the code/value that applies (see §16).

---

## 14. PCB Plotting — Log Axis + Unit-Sanity Flag

**Status: ✅ Complete (untested against live PREPA data)**

**Problem (Siegi):** No graphs for PCBs at internal outfalls.

**Root cause:** PCB observations are ~0–0.5 µg/L against a 6.4e-4 µg/L criterion
(~800× above). On a linear axis autoscaled to the data, the criterion line
collapses onto zero — the plot renders but looks blank. Additionally,
`min_val <- min(vals) - 1` drove the axis negative.

**Reasoning:** The reviewer noted this is likely a reporting-unit issue, and
asked for both a log plot AND a flag: "reported values are orders of magnitude
above limit, double check that units were reported correctly."

**Changes (app plot and report plot):** When observed max / criterion ≥ 100×,
the y-axis switches to `log10` (labeled "log scale"). When ≥ 1000×, a bold
warning is shown (subtitle in the app, caption in the report): *"Reported values
are ~N× the criterion; verify reported units before relying on this result."*
The 1000× threshold catches the common mg/L↔µg/L (exactly 1000×) error. Fires on
any parameter with the signature, not just PCBs.

---

## 15. Missing-Statistic Flag

**Status: ✅ Complete (untested)**

**Problem:** A parameter could match a criterion but report only a statistic the
criterion is not written against (e.g. Enterococci reported only as daily
maximum against a geometric-mean criterion). The join yielded nothing and the
parameter silently disappeared — the exact silent-null failure class behind the
original bug reports.

**Changes:** `flag_unmatched_params()` gained a `statistic_lookup` argument and
detects matched parameters whose DMR data shares no statistic with what the
criterion requires. New Needs Attention reason: **"Reported statistic does not
match criterion."** These are matched codes, so they are appended to the flagged
set rather than filtered from it.

---

## 16. Manual WQS Entry — Floor Criteria and Direction Selector

**Status: ✅ Complete (untested)**

**Problem:** The review panel could only express a ceiling. A hand-entered
DO-style floor standard would be treated as a ceiling and projected.

**Changes:**
- A **"Criterion Direction"** selector (Ceiling `>` / Floor `<`) added to the
  Include branch of each Needs Attention card, saved as `rp_operator`.
- `build_manual_crosswalk_rows()` is now direction-aware: a floor entry becomes
  `rp_method = direct`, `statistic = DAILY_MIN`, `no_exceed_rp = DEPENDS`; a
  ceiling keeps the projected path.
- Known parameters still inherit their profile from the base crosswalk (so a
  direct parameter re-added manually stays direct — this also fixed a crash,
  see below); the selector governs genuinely new parameters.
- Needs Attention `case_labels` gained entries for the narrative and
  missing-statistic reasons (previously rendered as `NA`), plus `alias_hints`
  pointing writers to synonym-code criteria (§13).

**Related crash fix:** When temperature was added via Needs Attention, the
manual-row builder had stamped it `projected`, routing it into `compute_rwc()`,
where `rp_calc_mf()` returned `MF = NA` for a small sample and `if (MF < 1)`
crashed with "missing value where TRUE/FALSE needed." Fixed three ways: an
`is.finite(MF)` guard on the floor operation, the profile-inheritance above, and
a belt-and-suspenders `projected_codes` filter on `cb` so a mis-stamped row can
never reach `compute_rwc()`.

---

## 17. Report (`report.qmd`) Updates

**Status: ✅ Complete (renders; new direct block untested)**

- **Basis column** in the RP Findings table distinguishing "Calculated" (RWC)
  from "Observed max/min" (direct), so an observed DO minimum can't be misread
  as dilution-adjusted.
- **Severity ordering** fixed — `arrange(desc(RP))` on a character column sorted
  YES/NO/DEPENDS and buried the DEPENDS rows; now an ordered factor
  (YES > DEPENDS > NO).
- **Criterion notes** (`rp_note`) rendered as orange callouts per pollutant
  section (DO Streeter-Phelps, Enterococci interval, temperature in-stream, oil
  & grease narrative).
- **Direct-row branch** in the per-pollutant loop: instead of the MF derivation
  (which would print empty equations for direct rows, since `MF = NA`), it
  prints an "Assessed Value" note stating the observed value was compared
  directly.
- **Generic direct-parameter block** added after the pH/temperature branches,
  producing table + plot sections for DO, color, turbidity, Enterococci, and
  oil & grease (previously only pH/temp had report sections). Uses the plumbed-in
  `direct_dmr.csv`. The bespoke pH/temperature blocks were left intact to avoid
  breaking working, unrenderable-here code — folding them in is deferred.
- **Log axis + unit warning** mirrored in the report plot (§14).
- **`Method` column** kept flowing through both selects; the report backfills it
  from `rp_operator` if absent, so it renders against either CSV vintage. (A
  missing `Method` here caused a render crash mid-session — the direct block had
  dropped it from the select.)

**Report formatting fixes (reviewer-reported):**
- The dagger (†) footnote for manually-entered WQS was defined but not rendering
  (auto-print didn't emit the `footnote` under a longtable); wrapped in `cat()`
  with `threeparttable = TRUE`.
- The Manual WQS Overrides table overflowed the page — the root cause was that
  `column_spec(width=)` adds ~0.42 cm of `\tabcolsep` padding **per column** that
  the earlier width sum ignored. Re-sized all eight columns accounting for
  padding, at full font, with Water Class / Criterion Value / Criterion Type
  center-aligned per request.
- The Manual WQS and Excluded Parameters intro paragraphs rendered in monospace
  because their chunks lacked `#| results: asis` — `cat()` output was captured as
  a verbatim code block. Added `asis` and made the kables explicit `print()`.

---

## 18. User Guide (`User_Guide_html.qmd`) — Full Review

**Status: ✅ Complete (structure validated; not rendered — no Quarto available)**

The guide predated this entire session's changes. Corrected: the "what the app
does" flow (statistic-driven, not max-only); the RP result table (added
DEPENDS); the TAN parameter-code table (was 00610/71845/34726 with wrong
conversion factors — corrected to the coalesced 00610/82230/00609); the pH and
temperature sections (said "not implemented" — both now are); the y-axis
description (log-capable). Added: a full **Direct-Comparison Parameters** section
(pH, temp, DO, color, turbidity, Enterococci, oil & grease, surfactants), a
**Needs Attention** workflow section (flag reasons, direction selector, alias
hints, Quick Run), and the unit-sanity flag. Four new figure placeholders added
(`needs_attention`, `direct_do_plot`, `log_axis_flag`, `report_criterion_note`)
for screenshots to be supplied.

---

## Files Changed — Summary

| File | Nature of change |
|---|---|
| `crosswalk_v2.csv` | **New.** 545 → 563 rows. Seven new columns; DO/Enterococci/temp/color/turbidity semantics; oil & grease, surfactant SG, Enterococci alt-code, and sulfate synonym rows |
| `statistic_lookup.csv` | **New.** Semantic statistic token → ICIS code pairs |
| `build_crosswalk_v2.py` | **New.** Reproducible generator for the annotated crosswalk |
| `functions.R` | New semantics helpers; `compute_rwc` grouping; direction-aware + statistic-aware manual builder; missing-statistic detection; MF crash guard |
| `app.R` | Statistic-tagging fetch; direct-findings block; TAN coefficient + 00609 coalescing; direction selector; alias hints; log-axis + unit flag; surface-waters revert; numerous plumbing changes |
| `report.qmd` | Basis column; severity order; criterion notes; direct-row branch; generic direct block; log axis; formatting fixes |
| `NPDES_Forms_Pollutants_1.csv` | Whitespace trim; 2C Table C additions (color, surfactants); Total phosphorus → 2A alias |
| `User_Guide_html.qmd` | Full review against current behavior |

---

## Testing Status

**Confirmed working by reviewer during session:**
- App launches; DMR fetch; report renders end-to-end
- Manual WQS overrides table (after width fixes)
- The crosswalk fixes through the temperature/`compute_rwc` crash

**Syntax-validated only (R `parse()` clean), NOT executed end-to-end:**
- Missing-statistic flag (§15)
- Manual floor direction selector (§16)
- Generic direct-parameter report block (§17)
- Log-axis / unit-sanity flag (§14)
- User Guide (§18) — not rendered (no Quarto in the dev environment)

**Recommended test order before deployment:**
1. Startup — the `stopifnot()` fails fast if `crosswalk_v2.csv` and
   `statistic_lookup.csv` disagree.
2. A permit with DO reported as `INST MIN` — confirms the fetch keeps minimums.
3. A manual **floor** entry end-to-end (add DO via Needs Attention, verify it
   evaluates as a minimum with DEPENDS).
4. A render exercising the new direct-parameter report sections.
5. Re-run any historical **TAN** determinations — the coefficient fix changes
   their values.

---

## Open Items (Deferred)

- **`load_reference.R` / database migration** — designed but not built. The
  crosswalk and reference CSVs should be materialized into the SQLite database
  alongside the ICIS data, keeping the version-controlled CSVs as source of
  truth. Not started.
- **Fold the bespoke pH/temperature report blocks** into the generic direct
  loop. Prerequisite for dropping the legacy `Method` column.
- **Full Rule 1303.2 reconciliation** — the metals/ions synonym candidates left
  to Needs Attention (§13) could be systematically reviewed with the reg open.
- **PCB fix verification** against live PREPA data (§14).
- **`statistic = ANY` token** — if binary detection tests (oil & grease, SG
  surfactants) should consider every reported statistic rather than only
  `DAILY_MAX`.
- **Old saved-session schema drift** — the direction selector added `rp_operator`
  to the overrides tibble; sessions saved before this change lack the column
  (there is a guard, but watch for it on reload).
