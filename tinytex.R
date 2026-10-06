# 1) Load and verify TinyTeX is installed
library(tinytex)
tinytex::is_tinytex()

tinytex::tinytex_root()
bin <- file.path(tinytex::tinytex_root(), "bin", "windows")



# 2) Add TinyTeX's bin dir to your PATH persistently (Windows)
tinytex::tlmgr(c("path", "add"))

# 3) Confirm pdflatex is now visible
Sys.which("pdflatex")


# Get TinyTeX root and bin dir
tinytex::tinytex_root()
bin <- file.path(tinytex::tinytex_root(), "bin", "windows")

# Add to PATH for this R session
Sys.setenv(PATH = paste(bin, Sys.getenv("PATH"), sep = ";"))

# Confirm
Sys.which("pdflatex")
