# aaa.R

# Suppress messages about masked conflicts with either
# options(conflicts.policy = list(warn = FALSE))
#  or
# library(pcurveMix, warn.conflicts = FALSE)

.onAttach <- function(libname, pkgname) {
  s <- packageVersion(pkgname)
  s <- paste("Package",pkgname,"version",s)
  packageStartupMessage(s)
} # .onAttach

