# Changelog

All notable changes to the Puerto Rico RP Calculator. The version lives in
`VERSION` and follows *major.minor.patch*: patch = error fixes that don't change
correct results, minor = new features, major = changes to how determinations
are made. The user guide (Appendix D) carries the user-facing version of these
notes.

Dated session logs from before versioning are in `06242026_CHANGELOG.md` and
`docs/07282026_CHANGELOG.md`.

## 1.0.0 — 2026-10-06

First versioned release.

### Report
- New layout: title page (EPA logo, Region 2, permit/facility, monitoring
  period, forms, generation date, tool version); running header and footer
  with page X of Y; every top-level section on a new page; EPA colours chosen
  to stay readable in black-and-white print.
- Direct-comparison parameters appear only in Non-Concentration Based
  Measures; both measure sections are ordered parameter → outfall and share
  one layout: note, one summary table per parameter and outfall (WQS ID,
  water class, samples, units converted, criterion, RWC or observed value,
  RP), then a framed, titled plot. Tables and plots never split across pages.
- Water classes listed in the summary; facility-wide sample count removed from
  Included Pollutants; Section 5 renamed "Parameters from Applications without
  DMR Data"; Excluded Parameters columns no longer cut off.
- "Check data" flag for temperatures below 0 °C (Celsius values reported under
  the °F code); negative values are no longer hidden by the plot axis or drawn
  as non-detects.
- Hardness setting reported correctly (was always "Range").

### App
- Version shown in the title bar and landing page, and stamped on reports,
  data-package READMEs and the user-guide download.
- Manually entered criteria inherit their method from the full crosswalk:
  a temperature criterion entered for SB/SD was projected through the RWC
  multiplying factor instead of compared directly.
- "Units converted" no longer lost for °F temperatures with a manual criterion.
- Append upload no longer fails when optional columns (including the outfall)
  are missing; uploads accept `YYYY-MM-DD` as well as `MM/DD/YYYY` dates
  (ISO dates were silently dropped); the DMR template has an outfall column.
- Hardness vs Limit view on the Findings page renders again.
- Standalone upload clears Needs Attention state from an earlier NPDES fetch.

### User guide
- Single source (`User_Guide.qmd`, built with `build_user_guide.R`); reviewed
  against the app and rewritten where it had drifted — Findings page, report
  contents, hardness range analysis, TAN inputs, Coverage Summary, uploads,
  standalone workflow. Version history added as Appendix D.

### Tooling
- `testing/render_example.R` re-renders the report from a downloaded report
  package (zip or folder) without running the app.
