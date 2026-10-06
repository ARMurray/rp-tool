# Puerto Rico Reasonable Potential (RP) Calculator

**Current version: 1.0.0** · [Changelog](CHANGELOG.md) · [User Guide](User_Guide.qmd)

An R Shiny application that helps EPA Region 2 permit writers determine whether
an NPDES-permitted discharge in Puerto Rico has reasonable potential (RP) to
cause or contribute to an exceedance of the Puerto Rico Water Quality Standards
Regulation (PRWQSR, as amended 2025). It retrieves effluent monitoring (DMR) data
from a weekly snapshot of ICIS-NPDES, matches each parameter to the applicable
criteria, applies the EPA TSD (1991) statistical projection or a direct
comparison as the criterion requires, and produces a documented PDF report.

The app is an analytical aid, not a substitute for permit writer judgment. See the
user guide (in the app, or `www/RPA_User_Guide.pdf`) for the methodology and its
limitations.

---

## What's new in 1.0.0

Version 1.0.0 (2026-10-06) is the first versioned release. Full details are in
[CHANGELOG.md](CHANGELOG.md) and in Appendix D of the user guide.

### Updates

- **Versioning.** The version (from the `VERSION` file) is shown in the app's
  title bar and landing page, and is stamped on every report, data-package
  README, and the user guide.
- **Redesigned PDF report.**
  - A title page with the EPA logo, Region 2, permit and facility, monitoring
    period, forms, date, and tool version.
  - A running header and footer with page *X of Y* on every page, and every
    section on a new page.
  - EPA colours chosen to stay readable when printed in black and white.
  - Direct-comparison parameters (pH, temperature, DO, Enterococci, color,
    turbidity, oil & grease) appear only in *Non-Concentration Based Measures*.
  - Both measure sections are ordered parameter → outfall and share one layout:
    a summary table per parameter and outfall, then a framed, titled plot that
    is never split across pages.
  - A *Check data* flag for temperatures below 0 °C.
- **User guide** rewritten from a single source (`User_Guide.qmd`) and checked
  against the app, with a version history appendix.

### Bug fixes

- A temperature criterion entered manually for Class SB/SD was run through
  the RWC multiplying factor instead of being compared directly.
- *Units Converted* showed "No" for °F temperatures that had been converted.
- Appending a CSV without the outfall column — including the app's own
  template — failed.
- Dates written as `YYYY-MM-DD` in uploaded CSVs were silently dropped.
- Reports always said *Hardness setting: Range*, and the Findings page's
  Hardness vs Limit view never appeared.
- On the Standalone page, Run RP could be blocked by Needs Attention items left
  over from an earlier NPDES fetch.
- Plots hid negative values and drew them as non-detects.
- The *Excluded Parameters* table was cut off at the right margin.

---

## Repository layout

| Path | Contents |
|---|---|
| `app.R` | The Shiny app (UI, data loading, RP calculation, downloads) |
| `R/functions.R` | Shared helpers: unit conversion, RWC, statistic tagging, manual criteria |
| `report.qmd` | Quarto template for the PDF report |
| `User_Guide.qmd` | User guide source (HTML and PDF) |
| `build_user_guide.R` | Builds the user guide into `www/` |
| `www/` | Files served by the app: logo, rendered user guide, metal limit table |
| `data/` | Crosswalk and lookup tables, and the ICIS-NPDES SQLite snapshot |
| `refresh_db.R` | Weekly job that rebuilds `data/pr_rp.sqlite` from ICIS-NPDES |
| `testing/render_example.R` | Re-renders a report from a downloaded report package |
| `VERSION` | The current version number |
| `CHANGELOG.md` | Release notes |
| `archive/`, `docs/` | Earlier versions and session notes |

## Running locally

**Requirements**

- R 4.4 or later with these packages: shiny, bslib, shinyjs, DT, leaflet,
  plotly, dplyr, tidyr, purrr, readr, stringr, lubridate, ggplot2, ggtext,
  ggforce, ggnewscale, knitr, kableExtra, writexl, readxl, DBI, RSQLite,
  quarto
- [Quarto](https://quarto.org) and a LaTeX installation for the PDF report
  (`quarto install tinytex`). The report uses the LaTeX packages `fancyhdr`,
  `lastpage`, `sectsty`, and `needspace`; TinyTeX installs them on first use.
- These files in `data/`. They are not all committed to the repository, so copy
  them from the deployed app or the shared drive:
  - `crosswalk_v2.csv`
  - `NPDES_Forms_Pollutants_1.csv`
  - `dmr_parameters.csv`
  - `statistic_lookup.csv`
  - `pr_rp.sqlite` (built by `refresh_db.R`)

**Run**

```r
shiny::runApp()
```

Set `PR_RP_SQLITE` to point the app at a database file outside `data/`.

## Common tasks

**Rebuild the user guide** after editing `User_Guide.qmd`:

```bash
Rscript build_user_guide.R
```

This renders the HTML and PDF with the version from `VERSION` and copies them to
`www/RPA_User_Guide.html` and `www/RPA_User_Guide.pdf`.

**Check a report change without running the app.** Download a report with
*Include Data in Download* ticked, then:

```bash
Rscript testing/render_example.R path/to/RP_Report_<permit>_<date>.zip out.pdf
```

**Refresh the ICIS-NPDES snapshot.** `refresh_db.R` runs weekly. It reads its
Oracle connection settings from environment variables (`ICIS_USER`, `ICIS_PW`,
`ICIS_DSN`, …), set in `.Renviron` or the scheduler. Do not put credentials in
the script.

**Deploy** to Posit Connect with Posit Publisher. The files to publish are listed
in `.posit/publish/PR_RP_Final-JRR9.toml`. When you add a file the app needs at
run time, add it there too.

## Releasing a new version

1. Update `VERSION`. Use *major.minor.patch*: patch for fixes, minor for new
   features, major for changes to how determinations are made.
2. Add an entry to `CHANGELOG.md`, and to Appendix D of `User_Guide.qmd` when
   users will notice the change.
3. Update the version and *What's new* section of this README.
4. Rebuild the user guide: `Rscript build_user_guide.R`.
5. Commit, tag, and push:

   ```bash
   git tag -a V1.1.0 -m "Version 1.1.0"
   git push origin main V1.1.0
   ```

6. Republish to Posit Connect.
