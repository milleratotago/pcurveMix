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
  packageStartupMessage("Get help with these RStudio console commands:")
  packageStartupMessage(' ?',pkgname,'    # shows a summary of the package.')
  packageStartupMessage(' vignette("Intro", package = ',pkgname,')   # shows a basic introductory vignette illustrating the package and its shiny app.')
  packageStartupMessage(' browseVignettes(',pkgname,')    # shows a catalog of all vignettes.')
  packageStartupMessage(' help(package = "',pkgname,'")   # shows a manual of all functions exported from the package.')
} # .onAttach

