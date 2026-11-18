# RP Tool demo: talking points by page

Demo permit: **PR0001031** (class SB). Simpler backup: **PR0024210**.
Suggested running time: about 20 minutes, 25 with questions. Minutes per page are in brackets.

Numbers below come from the ECHO DMR exports for the app's default window (2021-11-18 onward, data through early 2026). The app reads its own database snapshot, so counts on screen can differ a little. Anything marked **(check live)** should be confirmed in the app before the talk.

---

## Before you start

- [ ] Open the app, confirm the Home page shows a recent "Data last updated" line.
- [ ] Search page: set **Water Type to Class SB and Surface Waters** (both). The app defaults to Class SB only. Temperature and oil & grease have their standards under "surface waters", so with SB alone they land on Needs Attention instead of Findings. Showing that is optional (see page 4).
- [ ] Pick the NPDES form(s). The list starts empty and the Fetch button needs at least one.
- [ ] Leave the dates at the default five years.
- [ ] Run PR0001031 once beforehand with the default dates and check the Findings table looks like the one below.
- [ ] Confirm the permit's status on the Search page (Effective or Administratively Continued) and that you are comfortable showing the facility.
- [ ] Have the graphics ready: `docs/graphics/06_backend_data_fixes.png` for page 3, and `01_overall_solved` or `02_by_region` for the close.

---

## 1. Home page [1 min]

**Show:** the two workflow buttons, the "Data last updated" line, the User Guide button.

- Purpose in one sentence: helps permit writers decide whether **reasonable potential** exists for a discharge to exceed a water quality standard, using the TSD (1991) method.
- Two ways in: **Connect to ICIS-NPDES** (you have a permit ID) or **Standalone Calculator** (no ID, you bring your own data).
- Point at the data line: the tool reads a local snapshot of ICIS-NPDES DMR data. No live ECHO calls, so no rate limits and no waiting.
- It is built and tested on Puerto Rico. The pieces that are specific to a jurisdiction (water classes, standards, defaults) are data, not code. That is what the rest of the Regions would plug in.

---

## 2. Search page [2 min]

**Show:** permit search box, map, facility details, date range, forms, water type, Fetch.

- Search by permit ID or facility name. PR0001031 is preselected.
- Facility panel: status, effective date, link to the ECHO report. Use it to say "this is the real ICIS record".
- The map has a satellite toggle. Useful for outfall context, not for the calculation.
- Date range defaults to the last five years. The tool uses whatever is in that window.
- **Water Type is where the state standards come in.** Class SB, SD, SG and "surface waters" are Puerto Rico's use classes. For another Region this list becomes that state's designated uses.
- Choose the NPDES forms, then click **Fetch DMR Data**.

**If asked why Class SB:** it is the PRWQS class for coastal and estuarine waters, and it has criteria for about 150 parameter codes in the crosswalk (Class SD is similar, Class SG about 115). It was chosen for this demo because the permit discharges to SB waters. Confirm the class wording against the PRWQS before saying more.

---

## 3. Data Overview (first tab after Fetch) [4 min]

**Show:** the Coverage Summary table and the colored **Unit_Status** column. Then put up `06_backend_data_fixes.png`.

This is the page that sells the tool. Everything on it happened automatically in the seconds after Fetch.

- One row per parameter with its ICIS code, sample count and a **Unit_Status** color: green MATCH, yellow PASS (converted), grey EXCLUDED, blue-grey NO WQS, red FAIL.
- **Mixed units, converted per record.** Total residual chlorine (about 90 records) and zinc (about 65) are reported in mg/L; the standards are in ug/L. Both show PASS. Point at one and say: nobody opened a spreadsheet.
- **Temperature.** PR0001031 reports it twice, as °C under code 00010 and as °F under code 00011. The tool converts the °F series to °C, merges the codes and drops the duplicate, so temperature shows about 44 samples, not the roughly 140 you would get by counting both series. (check live)
- **Flows and loads** (kg/d, gal/d, MGD) are greyed out as EXCLUDED. Not an error: they cannot be compared to a concentration standard. They stay in the download.
- **Non-detects.** About 365 records carry a "below detection" code with no value and no unit. The tool counts them as zero and fills the unit, so they are not silently lost.
- Say it plainly: this is the manual work the tool removes. Without it, someone converts units, reconciles codes and filters statistics by hand for every parameter at every outfall.
- Left panel: dilution ratio (1), confidence level (0.95), target percentile (0.95). These are the TSD defaults. Other Regions may want different ones, and that is a question for the room.
- Also here: **Download Raw DMR**, and **Append DMR Data** (download a template, add local data, upload) for monitoring that is not in ICIS.

**Watch out:** the non-detect treatment (set to zero) is a simplification. If someone challenges it, say it is the current default and that you would like the Regions' view. The TSD allows other treatments.

---

## 4. Needs Attention tab [3 min]

**Show:** the orange parameter cards, the Run RP button (disabled), Quick Run.

- With Class SB plus Surface Waters, expect roughly **eight parameters** with data but no standard in these classes: total suspended solids, settleable solids, color, BOD, COD, chromium, free chlorine and total nitrogen. (check live)
- The tool does not drop them and does not guess. Each card says **why**: no entry anywhere, wrong water class, missing value, narrative only, or the reported statistic does not match the criterion.
- Some cards carry a **hint**: free chlorine (50064) points to the criterion on 50060; chromium and total nitrogen get similar notes. The writer decides whether the translation is appropriate.
- Each card lets the writer **include** with a manual criterion (value, unit, class, ceiling or floor, note) or **exclude** with a note.
- **Run RP stays disabled until every flagged parameter is resolved.** That is the guard against silent gaps.
- **Quick Run** drops everything unresolved. Use it live to keep moving, and say so out loud.
- Manual entries and exclusions are recorded in the report, so the decisions are auditable.

**Optional 30-second variant:** reset the Water Type to Class SB only and re-fetch. Temperature and oil & grease now appear here as "wrong water class". It shows that the class choice drives what gets evaluated.

---

## 5. WQS Reference tab [1 min]

**Show:** the searchable table.

- The full crosswalk: criterion ID, value, unit, water class, and the ICIS code each one is tied to.
- Use it before making a manual entry on Needs Attention.
- This table is what scales. Puerto Rico's is hand-built and reviewed. The national build now ties **84% of all 46,235 criteria** to an ICIS code, including the states, territories and tribes (`02_by_region`), and what is left is a resolver tool away. Mention it only if the audience asks about expansion; the closing slide covers it.

---

## 6. Findings page [6 min]

**Show:** the RP table on the left, the plot on the right, the banner along the bottom.

**Orientation (30 seconds)**
- Table columns: outfall (E = external, I = internal, shown in red), pollutant, WQS, water class, RWC, RP. Sorted **YES, then DEPENDS, then NO**, with filters on every column.
- Click a row and the plot updates: observed samples, the WQS line, the RWC, and the prior permit limit if there is one. The prior limit is converted to the WQS unit so the lines are comparable.
- Plot subtitle: sample count, maximum, RWC, and the RP verdict.
- Banner: "Exceedances Detected" or "No Exceedances Found".

**Expected mix for PR0001031 (about 22 YES, 11 NO, 1 DEPENDS across 4 outfalls)**

| Click this row | What to say |
|---|---|
| **Chlorine, total residual, outfall 001** (about 44 samples, RWC about 225 vs 7.5 ug/L) | Reported in mg/L, plotted in ug/L, so the unit conversion is what makes this comparison possible. A clear YES. |
| **Copper, outfall 001** (about 84 samples, 15.6 vs 3.7 ug/L) | A metal with a numeric standard, plenty of data. Show the plot and the RWC above the line. |
| **Nickel, outfall 001** (3 samples) | Small sample. With fewer than 10 results the tool uses the TSD default CV of 0.6, which widens the multiplying factor. This is why a thin dataset still produces a protective answer. |
| **Silver or thallium, outfall 001** (RWC about 1.15 vs 2.24; about 0.45 vs 0.47 ug/L) | A **NO**. Thallium is close, and it is worth saying that NO means the projection stays below the standard, not that it is far below it. |
| **Cyanide, outfall 001** (about 1.007 vs 1.0 ug/L) | A YES by a hair, which is a good moment to say the tool applies a strict comparison with a tiny rounding tolerance, so conversion error cannot flip a result. |
| **Temperature, outfall 001** (observed maximum about 37.4 °C vs 30) | A **direct comparison**: no multiplying factor, no dilution. The subtitle says "Observed maximum ... no RWC calculated". The °F series was converted on the way in. |
| **Dissolved oxygen, outfall 001** (observed minimum 4.4 vs 5 mg/L) | A **floor** criterion, so the tool compares the observed minimum. |
| **Dissolved oxygen, outfall 002** (1 sample) | The one **DEPENDS**. Open the note under the plot: a defensible DO analysis needs a Streeter-Phelps model, which ICIS data cannot supply, so "no excursion seen" is not enough to say no RP. |
| **pH, outfall 01A** (min 6.4 vs 7.3, max 8.9 vs 8.5) | Two-sided criterion handled as a minimum row and a maximum row. |

Pick four or five of these, not all. A good arc is: chlorine (units), copper (a clean YES), thallium (a close NO), temperature (direct comparison), DO outfall 002 (DEPENDS).

**Points to hit**
- **YES, NO and DEPENDS are all real determinations.** DEPENDS is not "unknown".
- Two methods in one table: projected (TSD multiplying factor from the data) and direct comparison (pH, temperature, DO, turbidity, oil & grease).
- Internal outfalls are flagged in red because some PR permits label real discharges as internal in ICIS.
- The method choice, operator and statistic come from the crosswalk, so a new state is data, not code.

**Watch out**
- Outfall 002 oil & grease has one record of 1.4 ug/L that looks like a unit entry error. The tool converts it to 0.0014 mg/L and still returns YES (the "standard" is a detection test). Skip that row unless asked.
- Several outfalls carry the same pollutant. If someone asks why chlorine appears three times, the answer is that it is evaluated per outfall.

---

## 7. Report download [1 min]

**Show:** the Download Report button and the "Include Data in Download" checkbox.

- One click produces a PDF: coverage table, the basis for including each pollutant, each finding with its plot, and flags for any manual entries or user-added data.
- Tick **Include Data** and you get a ZIP with the underlying data and the criteria table used, so a reviewer can reproduce the numbers.
- This is the record that goes into the fact sheet file.

---

## 8. Standalone Calculator [optional, 1 min]

- For facilities with no NPDES ID, or data that is not in ICIS. Download the DMR template, fill it in, upload, and the same cleaning and RP steps run.
- Mention it for pre-permit work, or any facility whose data is not in ICIS.

---

## 9. Close: what it takes to go regional [2 min]

**Show:** `docs/graphics/01_overall_solved` and `02_by_region`, then the one-page summary (`docs/Regional_Expansion_Summary.html`).

- What is region-specific is **data**: the state standards tied to ICIS codes, and the defaults.
- The national crosswalk is built: **84%** of all 46,235 criteria are already tied to an ICIS code (states and DC 84%, territories 88%, tribes 83%). By Region, states run 82% to 87% and tribes 65% (Region 2, one tribe) to 89%. The rest is a resolver tool away.
- Tribes are in the numbers on purpose: those are the permits Regions are most likely to be reviewing themselves. `04b_by_tribe` shows each one. The lowest are Colville (56%), Bishop Paiute (59%) and Saint Regis Mohawk (65%, in Region 2).
- Ask the room two questions:
  1. Which **defaults** do your states use (dilution ratio, CV, confidence level, percentile, non-detect treatment)?
  2. Which **jurisdictions** should go first?

---

## Optional segment: total ammonia nitrogen (TAN), Class SD [3 min]

TAN exists only for **Class SD** waters, so this needs a second permit and a different Water Type. Permit: **PR0025283** (one outfall, 001; about 1,330 records; monthly data through Jan 2026).

**Setup:** on the Search page set Water Type to **Class SD** only. Search PR0025283, pick a form, Fetch. On the settings page a **Receiving Water Inputs (Class SD)** block appears: hardness (default 100 mg/L), receiving-water pH (default 6) and temperature (default 30 °C).

**Say:**
- The ammonia standard is not a number. It is a formula of pH and temperature (PRWQS 1303.2(C)(2)(l)), so the tool calculates it at run time from the receiving-water values you enter.
- At the defaults (pH 6, 30 °C) the criterion is about **1.16 mg/L N**. At pH 8.0 and 25 °C it is about 0.56. At pH 8.5 and 30 °C it is about 0.18. The criterion on the plot and in the table moves with the inputs.
- Same idea for **copper and lead**: the limit depends on hardness. Tick "Show hardness range analysis" and the Findings page offers a Hardness vs Limit view.
- Same permit also shows the **stream vs reservoir/lake** split for total phosphorus (160 vs 26 ug/L) and mg/L to ug/L conversion for chlorine and phosphorus.

**Expected on Findings (about, check live):** TAN n = 50, maximum 45.5 mg/L, RWC about 48 mg/L. That is far above the criterion, so it is a **YES at every pH and temperature** in the plausible range (the criterion never exceeds about 8 mg/L). The inputs change the line, not the verdict. Copper (RWC about 77 vs 9.3 ug/L at hardness 100) and lead (about 15 vs 3.2) are also YES.

**Watch out**
- This permit is a poor fit for "mix of YES and NO". Almost everything exceeds. Use it for the formulas, not the mix.
- pH minimum includes five reported values of 0 (no discharge entered as zero), which drives a YES on pH. Skip that row or explain it.
- **Do not substitute another permit without testing.** 146 other PR permits report ammonia as code 82230. The app recodes it to 00610, but the records keep a NO_WQS unit flag and are dropped before the RP math, so no TAN result appears. PR0025283 works because it also reports 00610 directly. See the note in the hand-off.

---

## Quick answers for likely questions

| Question | Answer |
|---|---|
| Why is the CV 0.6? | The TSD default when there are fewer than 10 results. With 10 or more, the tool computes the CV from the data. |
| Why a dilution ratio of 1? | A conservative default: no mixing credit. It is a setting, so a writer can enter the facility's value. |
| How are non-detects handled? | Counted as zero, with the unit filled in. A known simplification; open to the Regions' preference. |
| What if units cannot be converted? | The record is flagged FAIL, kept in the download and left out of the math. Flows and loads are EXCLUDED the same way. |
| What about hardness-dependent metals and ammonia? | Class SD only. The limit is calculated at run time from hardness, or from pH and temperature for ammonia. Not part of the SB demo. |
| Is it live ICIS data? | A local snapshot refreshed on a schedule. The Home page shows the date. |
| Can I add my own data? | Yes: Append DMR Data on the settings page, or the Standalone Calculator. |
| Does it replace the permit writer's judgment? | No. It does the arithmetic and the data cleanup. The writer still decides what to include, exclude and how to translate a standard. |

---

## Before you commit to this permit

The numbers above come from my reconstruction of the app's logic on the ECHO exports, not from the app itself. Run the permit with the default dates before the talk and check:

1. The Findings table has roughly the YES, NO and DEPENDS mix above.
2. Chlorine and zinc show PASS, and temperature shows PASS.
3. Needs Attention lists the parameters you expect.

If anything looks off, switch to **PR0024210**: one outfall, about 15 parameters, phosphorus reported in both ug/L and mg/L.
