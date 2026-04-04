# aaa.R

# Next line suppresses package check warning about "density"
# that is used in quick_pdf_plot
utils::globalVariables(c("density"))

# Suppress messages about masked conflicts with either
# options(conflicts.policy = list(warn = FALSE))
#  or
# library(pcurveMix, warn.conflicts = FALSE)

.onAttach <- function(libname, pkgname) {
  s <- utils::packageVersion(pkgname)
  s <- paste("Package",pkgname,"version",s)
  packageStartupMessage(s)
  packageStartupMessage("use `browseVignettes(",pkgname,")` to see vignettes.")
  packageStartupMessage("Check the vignette `intro` for a basic introduction.")
} # .onAttach

