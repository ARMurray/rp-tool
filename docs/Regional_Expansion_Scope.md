# Extending the RP Tool Beyond Puerto Rico — Scope and Effort

Prepared 2026-11-17 from a review of the tool as of v1.0.0. The numbers in
sections 1–2 are measured from the repository's data files. **Effort figures are
planning estimates, not measurements**, and assume one developer who already
knows the tool. They are ranges because the biggest unknown (ICIS data volume,
§4) has not been sized.

## Bottom line

- The tool is **Puerto Rico–only today**, not Region 2-wide. `refresh_db.R`
  pulls PR and VI; NJ and NY are not loaded.
- A national starting point already exists. `other/national_wqs.csv` holds 46,235
  state, territory and tribal criteria, and `other/wqs_matched.csv` has already
  matched them to ICIS parameter codes. About 70% matched exactly; the rest
  needs human review.
- The crosswalk is **not** the hardest part. The three bigger items are:
  1. **Equations are missing from the national database.** About 1,300 criteria
     say "See equation" or "See table" instead of giving a number. Hardness
     metals, ammonia, pentachlorophenol and similar criteria must be built by
     hand, as was done for PR.
  2. **Water classes are free text.** There are 1,302 distinct class labels
     across states (median 21 per state; PR has 4). The app's class picker is
     hardcoded to PR.
  3. **PR is hardcoded throughout** `app.R` (dozens of references): class lists, seven
     metal criterion IDs, the TAN criterion ID, default pH/temperature/hardness,
     and PR wording in the report and guide.
- **Rough size.** Foundation work (refactor, mapping review, equation library,
  data pipeline) is about 10–15 weeks. Each additional jurisdiction then costs
  about 2–10 working days depending on tier (§2). Covering only the
  jurisdictions where EPA writes the permits is about 5 weeks after the
  foundation. All 54 is on the order of 9–12 months for one person, less if
  regional staff do the validation.

## 1. State WQS crosswalk to ICIS-NPDES

### What exists

| Item | Count |
|---|---|
| Criteria in `national_wqs.csv` (all entities) | 46,235 |
| …for 50 states + DC + VI, GU, AS + PR | 25,978 (55 jurisdictions; 25,564 excluding PR) |
| …for tribes and other entities | 62 entities (not scoped below) |
| Distinct pollutant names (states) | 914 |
| Already matched to an ICIS parameter code (exact or normalized) | ~70% of rows |
| Fuzzy matches needing review | 737 distinct pollutant names (335 high, 208 medium, 194 low confidence) |
| Rows whose criterion value is not a number | 2,658 (~10%) |
| …of which "See equation / See table" | ~1,300 |

Because a name-to-code mapping is the same in every state, the 737 reviews are
done **once nationally**, not per state.

### Work items

| # | Task | Estimate | Notes |
|---|---|---|---|
| A1 | Review the 737 fuzzy name→parameter-code mappings | 5–8 days | The 194 low-confidence ones need chemistry judgment (isomers, total vs dissolved, speciation). |
| A2 | Decide how to represent water class and criterion type | 3–5 days | Replace free-text class with a state-keyed class plus normalized use (aquatic life acute/chronic, fresh/salt; human health water+organism / organism only). PR's SB/SD/SG fits this as one state's list. |
| A3 | Parse non-numeric values | 3–5 days | pH and temperature ranges ("6.5-9.0") become min/max rows; narrative standards become `criterion_form = narrative`; "7 million" style values need parsing. |
| A4 | Set `rp_method`, operator and statistic per parameter | 5–8 days | Direct-comparison parameters (pH, DO, temperature, bacteria indicators, turbidity, color) vary by state. PR has 31 direct rows to model from. |
| A5 | Hardness-dependent equations library | 8–12 days | PR hardcodes 7 metals and a precomputed table (`www/metal_limits.csv`, hardness 25–400). Replace with an equation table (slope, intercept, conversion factors, hardness floor and cap). Many states copy the federal 304(a) equations, so expect roughly 10–15 distinct equation families. |
| A6 | Ammonia module | 5 days | PR uses one pH/temperature formula (Rule 1303.2(C)(2)(l)). Other states use the 1999 or 2013 EPA criteria (mussels present, early life stages) or their own tables. Freshwater and saltwater both. |
| A7 | Other conditional criteria | tail, defer | Copper BLM, aluminum, pentachlorophenol (pH), sulfate/chloride (hardness or chloride dependent), selenium. Suggest these go to **Needs Attention / manual entry** at first, which the app already supports. |
| A8 | Build script and QA | 5 days | Extend `build_crosswalk_v2.py` into a reproducible national build that writes one crosswalk per state, plus a unit-coverage check against ICIS unit codes. |
| A9 | Verify currency of each state's standards | per state, in §2 | The database is a secondary source (some entries date from 2023). PR was built from the 2025 regulation directly. Spot-check the top ~30 parameters against current state rules. |

**Subtotal, shared crosswalk work: roughly 35–50 days.**

### State complexity tiers

Scored on class count, share of non-numeric criteria, share of non-exact name
matches and total criteria (script output saved with this analysis).

| Tier | Meaning | Per-state effort (after foundation) | Jurisdictions |
|---|---|---|---|
| 1 | Few classes, mostly numeric, clean matches | 2–3 days | 24 |
| 2 | 15–40 classes or 7–30% equation/narrative rows | 4–6 days | 26 (excluding PR) |
| 3 | 40+ classes and heavy equations/conditions | 8–10 days | 4: **AL, FL, MN, CO** |

By region (T1 / T2 / T3):

| Region | Tier 1 | Tier 2 | Tier 3 |
|---|---|---|---|
| 1 | CT, MA, ME, NH, RI, VT | — | — |
| 2 | VI | NJ, NY | — |
| 3 | DC, MD, VA | DE, PA, WV | — |
| 4 | KY, MS, SC, TN | GA, NC | AL, FL |
| 5 | — | IL, IN, MI, OH, WI | MN |
| 6 | — | AR, LA, NM, OK, TX | — |
| 7 | IA, NE | KS, MO | — |
| 8 | ND, UT, WY | MT, SD | CO |
| 9 | CA, GU | AS, AZ, HI, NV | — |
| 10 | AK, ID, WA | OR | — |

Totals: Tier 1 ≈ 48–72 days, Tier 2 ≈ 104–156 days, Tier 3 ≈ 32–40 days, so
about 185–270 person-days for all 54 (≈ 37–54 person-weeks).

Tier is a proxy computed from the database, not a read of each state's
regulation. Expect some states to move up or down once opened.

## 2. Application changes (removing the PR hardcoding)

| # | Task | Estimate |
|---|---|---|
| B1 | State selector driven by the permit's `state_code`; load that state's crosswalk; key everything on (state, criterion) | 3–4 days |
| B2 | Water class UI, filters and colors driven by the state's class list instead of five hardcoded blocks (`app.R` ~657, 827, 1106, 1370, 2468, 3742) | 3–5 days |
| B3 | Replace hardcoded criterion IDs (7 metals in ~6 places, TAN `79613`) with a `formula_id` column read from the crosswalk | 3–4 days |
| B4 | Remove PR-specific general-standards logic ("surface waters" always appended, SD-only panels) | 2 days |
| B5 | Report and user guide: state name, regulation citation and effective date, EPA region from the permit (currently "Region 2" and PRWQSR text in `report.qmd`, `README.md`, `User_Guide.qmd`) | 4–6 days |
| B6 | Acute vs chronic handling (see §3: the tool uses one dilution ratio and one statistic for all concentration criteria) | 3–5 days, if adopted |
| B7 | Regression tests: PR results must not change. The PR0001031 and PR0000698 report packages can serve as golden outputs; there are no automated tests today | 4–5 days |

**Subtotal ≈ 22–30 days.**

## 3. Default values and assumptions to learn from the regions

These are what the tool assumes now. Most are PR choices or TSD defaults that
other regions may handle differently. **This is the main thing to ask the
audience tomorrow.**

| Setting | Current default | Why it may differ |
|---|---|---|
| Dilution ratio | 1 (no mixing zone) | States and regions set dilution from design flows (7Q10, 1Q10, 30Q10, harmonic mean) and allow different percent-of-flow limits for acute, chronic and human health. The tool takes one number and has no flow-based calculator. |
| Confidence level / target percentile | 95% / 95% | The guide describes this as Region 2 practice. The TSD publishes both 95 and 99 tables, and some regions or states use 99/99. |
| CV when n < 10 | 0.6 | TSD default; confirm whether others use a different threshold. |
| Minimum samples | none found in the code | Some programs require a minimum n before a determination. |
| NODI B and Q | treated as 0 | Consistent with the TSD, but some offices exclude them. |
| Non-detects | no DMR qualifier handling found; values ≤ 0 are plotted as non-detects | Practice varies (zero, ½ DL, DL). Confirm what the app does with "<" values before the demo. |
| Background concentration | not used (ambient = 0) | TSD mass balance includes upstream concentration; many regions require it. |
| Hardness | 100 mg/L; equation table covers 25–400 | States specify hardness policy (low-flow, 5th percentile, floors and caps). |
| Ammonia inputs | pH 6.0, temperature 30 °C | Tropical, PR-specific. Elsewhere seasonal, and criteria depend on mussels and early life stages. |
| Default water class | Class SB | State-specific. |
| Effluent statistic | daily maximum for all concentration criteria | Human health criteria usually pair with a long-term average; acute and chronic have different durations. |
| Narrative standards | oil & grease and surfactants run as detection tests | These are PR operationalizations of narrative criteria. Each region may want its own rule. |
| Data window | UI default 5 years; SQLite holds 10 | Permit cycle length varies. |

Suggested approach: a one-page questionnaire handed out at the demo, covering
each row above, with "who decides — region, state or permit writer" for each.
Then add a `defaults` table keyed by region and state, shown in the app and
stamped on the report as it is now for CL and percentile.

Estimate: 2–3 days to build the configurable defaults table; the survey and
follow-up conversations are the real time cost.

## 4. ICIS-NPDES data pipeline

| # | Task | Estimate |
|---|---|---|
| C1 | Size the snapshot: row counts per state for DMR, 10 years | 1 day (needs Oracle access) |
| C2 | Decide architecture: one SQLite per region, one national SQLite, or live queries | decision |
| C3 | Extend `refresh_db.R` (`TARGET_STATES`), add region and permit-authority fields, handle tribal permits | 2–3 days |
| C4 | Re-run the SQLite vs ECHO validation (`validation_results/`) for each new region's pilot states | 1–2 days per pilot |
| C5 | Check Posit Connect limits (bundle size, memory, refresh job) | 1–2 days |

Estimate: 5–10 days, but **this is the biggest unknown.** A national DMR
snapshot is very large, and `pr_rp.sqlite` is loaded at app startup. Options:

- **Limit the snapshot to EPA-issued permits.** ICIS records the issuing
  authority. EPA is the permitting authority in a small subset of jurisdictions
  (for example MA, NH, NM, DC, the territories, and Indian country — **confirm
  against ICIS before relying on this list**). That would shrink the data by
  orders of magnitude and also shrinks the crosswalk work (§5).
- Keep one snapshot file per region and let the app open the region's file.
- Query Oracle live per permit. Avoid this unless the data team supports it:
  the app moved to a snapshot to avoid external-service rate limits and
  dependencies.

## 5. Recommended phasing

| Phase | Content | Estimate |
|---|---|---|
| 0 | Run the defaults survey at the demo; size ICIS volume; confirm which jurisdictions EPA issues permits in | 1 week |
| 1 | Foundation: A1–A6, A8, B1–B4, B7, C2–C3 | 10–15 weeks |
| 2 | Pilot: VI (Region 2, already in the refresh, Tier 1) plus one or two others from another region | 1–2 weeks |
| 3 | EPA-issued jurisdictions: VI, MA, NH, DC, GU (all Tier 1), NM, AS (Tier 2) | ~4 weeks (≈ 22 days) |
| 4 | Remaining states by demand; regional staff validate their own states | 2–10 days each |

Phase 3 depends on the list of EPA-issued jurisdictions being right. If the
regions want delegated states too (so permit writers can review state-issued
permits), go straight to Phase 4 for those.

## 6. Risks and decisions

- **Correctness at scale.** Each state's crosswalk is a legal-standards
  interpretation. Assign a named regional reviewer per state; version each
  crosswalk and print its version and effective date on the report.
- **Standards change.** Triennial reviews mean crosswalks go stale. Decide
  who owns updates and how often.
- **Tool reach.** The tool is an aid, not a substitute for permit writer
  judgment (README). More states means more users who did not build it; the
  user guide and the Needs Attention workflow matter more.
- **Credentials in the repository.** `refresh_db.R` and
  `archive/archive_07012026_refresh_db.R` set a default Oracle username and
  password in code, contradicting the README guidance. Before sharing the
  repository with other regions, remove those defaults, rotate the password,
  and consider that the value stays in git history.
- **Method choices before code.** Acute vs chronic dilution, background
  concentration, and the minimum-sample rule change results, not just
  presentation. Settle them with the regions before building B6.
