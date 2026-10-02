# Rebuild the package vignette into inst/doc.
#
# This used to build a PDF and shrink it with ghostscript, via
# devtools::build_vignettes() and tools::compactPDF(). The vignette is now
# HTML, so neither LaTeX nor ghostscript is involved and the old recipe no
# longer applies. R CMD build produces inst/doc/Intro.html itself when it
# builds the tarball, which is what CRAN and `R CMD INSTALL` consume.
#
# This script is therefore only needed to preview the rendered vignette
# locally, without a full tarball build.

library(ungroup)

out <- file.path(tempdir(), "doc")
dir.create(out, showWarnings = FALSE, recursive = TRUE)

rmarkdown::render(
  input       = "vignettes/Intro.Rmd",
  output_dir  = out,
  quiet       = TRUE
)

message("Rendered vignette to ", file.path(out, "Intro.html"))

# Fri Oct 02 2026 ------------------------------
# Marius Pascariu
