# Re-render report.qmd from a downloaded report package (RP_Report_*.zip).
#
# The zip's data.xlsx carries rp_concentration, dmr, coverage and the
# effective crosswalk (WQS_Info). The pH / temperature / direct-comparison
# observation files are rebuilt here the same way app.R builds them. WQS
# overrides are not in the package, so the Excluded Parameters and Manual
# WQS Overrides sections will be empty in the rebuilt report.
#
# Usage (from the repo root):
#   Rscript testing/render_example.R <zip or folder> [out.pdf] [facility] [forms]

suppressPackageStartupMessages(library(dplyr))

args <- commandArgs(trailingOnly = TRUE)
zip_path <- normalizePath(args[1], mustWork = TRUE)
out_pdf  <- if (length(args) >= 2) args[2] else sub("\\.zip$", ".pdf", basename(zip_path))
facility <- if (length(args) >= 3) args[3] else ""
forms    <- if (length(args) >= 4) args[4] else ""

if (!nzchar(Sys.getenv("QUARTO_PATH"))) {
  positron_quarto <- file.path(
    Sys.getenv("LOCALAPPDATA"),
    "Programs/Positron/resources/app/quarto/bin/quarto.exe"
  )
  if (file.exists(positron_quarto)) Sys.setenv(QUARTO_PATH = positron_quarto)
}

tmp_dir <- tempfile("rp_render_")
dir.create(tmp_dir)
# Accept the zip as downloaded or an already-extracted package folder.
if (dir.exists(zip_path)) {
  file.copy(list.files(zip_path, full.names = TRUE), tmp_dir)
} else {
  utils::unzip(zip_path, exdir = tmp_dir)
}
xlsx <- file.path(tmp_dir, "data.xlsx")

# Read every sheet as text so parameter codes keep their leading zeros, then
# write CSV and let read_csv in the report re-guess the types as it does for
# the app's own files.
sheet <- function(name) {
  readxl::read_excel(xlsx, name, col_types = "text") %>%
    # Text mode returns Excel date serials ("43373"); restore ISO dates.
    mutate(across(
      matches("_date$") & where(~ all(grepl("^\\d+$", .x[!is.na(.x)]))),
      ~ format(as.Date(as.numeric(.x), origin = "1899-12-30"))
    ))
}
write <- function(df, name) readr::write_csv(df, file.path(tmp_dir, name), na = "NA")

rp   <- sheet("RP_Concentration")
dmr  <- sheet("DMR")
cov  <- sheet("Coverage")
wqs  <- sheet("WQS_Info")

write(rp,  "rp_concentration.csv")
write(dmr, "dmr.csv")
write(cov %>% select(-any_of("User Data")), "coverage.csv")
write(wqs, "wqs_info.csv")

# Mirrors the direct-findings block in app.R
direct_cw <- wqs %>% filter(rp_method == "direct")
direct_dmr <- dmr %>%
  filter(parameter_code %in% direct_cw$parameter_code) %>%
  select(
    parameter_code, statistic, perm_feature_nmbr,
    any_of("perm_feature_type_code"), dmr_value_nmbr, dmr_unit_desc,
    limit_value_nmbr, statistical_base_type_code, monitoring_period_end_date,
    any_of(c("limit_begin_date", "limit_end_date"))
  ) %>%
  filter(!is.na(dmr_value_nmbr))

ph_dmr <- direct_dmr %>%
  filter(parameter_code == "00400") %>%
  left_join(
    direct_cw %>% distinct(parameter_code, statistic, criterion_label, NPDES_Pollutant),
    by = c("parameter_code", "statistic")
  ) %>%
  mutate(NPDES_Pollutant = coalesce(na_if(criterion_label, ""), NPDES_Pollutant))

temp_dmr <- direct_dmr %>% filter(parameter_code == "00010")

first_num <- function(x) as.numeric(x[!is.na(x)][1])

path_if <- function(df, name) if (nrow(df) > 0) { write(df, name); name } else ""

readme <- readLines(file.path(tmp_dir, "README.txt"), warn = FALSE)
permit <- sub(".*:\\s*", "", grep("Permit ID", readme, value = TRUE)[1])
dates  <- regmatches(readme, regexpr("\\d{4}-\\d{2}-\\d{2} to \\d{4}-\\d{2}-\\d{2}", readme))
dates  <- strsplit(dates, " to ")[[1]]
hardness <- suppressWarnings(as.numeric(sub(".*:\\s*", "", grep("Hardness", readme, value = TRUE)[1])))
# Blank in range-mode packages; the report only reads it in "set" mode, but
# Quarto rejects an NA parameter.
if (is.na(hardness)) hardness <- 100

file.copy("report.qmd", file.path(tmp_dir, "report.qmd"), overwrite = TRUE)
file.copy("www/epa_logo.png", file.path(tmp_dir, "epa_logo.png"), overwrite = TRUE)

quarto::quarto_render(
  input = file.path(tmp_dir, "report.qmd"),
  output_format = "pdf",
  output_file = "report_rendered.pdf",
  execute_params = list(
    permit_id = permit,
    facility_name = facility,
    date_start = dates[1],
    date_end = dates[2],
    forms = forms,
    dilution_ratio = first_num(rp$dilution_ratio),
    confidence_level = first_num(rp$confidence_level),
    target_percentile = first_num(rp$target_percentile),
    hardness_mode = "range",
    hardness_value = hardness,
    rp_path = "rp_concentration.csv",
    coverage_path = "coverage.csv",
    dmr_path = "dmr.csv",
    ph_dmr_path = path_if(ph_dmr, "ph_dmr.csv"),
    temp_dmr_path = path_if(temp_dmr, "temp_dmr.csv"),
    direct_dmr_path = path_if(direct_dmr, "direct_dmr.csv"),
    wqs_info_path = "wqs_info.csv",
    overrides_path = "",
    quick_run = FALSE,
    quick_run_params = "",
    app_version = trimws(readLines("VERSION", warn = FALSE)[1])
  ),
  quiet = FALSE
)

file.copy(file.path(tmp_dir, "report_rendered.pdf"), out_pdf, overwrite = TRUE)
cat("Wrote", normalizePath(out_pdf), "\n")
