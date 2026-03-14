# pcurveMix
R package for computations with *p*-curves under Ulrich &amp; Miller (2026) mixture model.
The package functions allow you to estimate the model's parameters from vectors of
observed *p* values and to compute the predicted pdfs and cdfs from the fitted model.

To install the package from inside RStudio:
- `> install.packages("remotes")`  # if you don't already have this package.
- `> remotes::install_github("milleratotag/pcurveMix", build_vignettes = TRUE)`
- `> library(pcurveMix)

You should then be able to see the vignettes with `> browseVignettes(package = "pcurveMix")`

