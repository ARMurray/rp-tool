# Build the user guide and put it where the app serves it.
#
# User_Guide.qmd is the single source for both formats. The app shows
# www/RPA_User_Guide.html in its User Guide modal and offers
# www/RPA_User_Guide.pdf from the landing page's download button.
# The version printed in the guide comes from the VERSION file.
#
# Usage (from the repo root):  Rscript build_user_guide.R
# The PDF needs a TeX install:  quarto install tinytex

if (!nzchar(Sys.getenv("QUARTO_PATH"))) {
  positron_quarto <- file.path(
    Sys.getenv("LOCALAPPDATA"),
    "Programs/Positron/resources/app/quarto/bin/quarto.exe"
  )
  if (file.exists(positron_quarto)) Sys.setenv(QUARTO_PATH = positron_quarto)
}

version <- trimws(readLines("VERSION", warn = FALSE)[1])

# Inline R is not evaluated in the YAML header, so the versioned subtitle is
# passed as metadata here.
quarto::quarto_render(
  "User_Guide.qmd",
  output_format = "all",
  quarto_args = c("--metadata", paste0("subtitle:User Guide — Version ", version))
)

stopifnot(
  file.copy("User_Guide.html", "www/RPA_User_Guide.html", overwrite = TRUE),
  file.copy("User_Guide.pdf", "www/RPA_User_Guide.pdf", overwrite = TRUE)
)
# The root copies are build output; www/ holds the published ones.
unlink(c("User_Guide.html", "User_Guide.pdf"))

cat("User guide", trimws(readLines("VERSION", warn = FALSE)[1]), "written to www/\n")
