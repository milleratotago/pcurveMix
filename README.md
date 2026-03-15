# pcurveMix

This is an R package for computations with *p*-curves under the Ulrich &amp; Miller (2026) mixture model.
According to the model, an observed set of *p* values comes from a population of studies within which
the proportion $\pi$ test for effects that are actually present (i.e., H1 is true)
and the proportion $1-\pi$ test for effects that are actually absent (i.e., H0 is true).
Among studies testing for effects that are present, the noncentrality parameters vary
normally with mean $\mu$ and standard deviation $\sigma$.

This package fits the model by maximum likelihood to a set of observed *p* values, which can come
either from a complete set of studies (i.e., 0<*p*<1) or from a set of studies
selected to have significant results (i.e., $p<\alpha$).
The fitting results include estimates of the model's parameters and standard errors of those estimates.

Functions are also provided to compute the model's predicted PDFs of *p*, CDFs of *p*, quantiles, and random *p* values
under any specified set of parameter values.

Both one- and two-tailed *p* values are supported.

## ONCE THE PACKAGE IS PUBLIC:

To install the package from inside RStudio:
- `> install.packages("remotes")`  # if you don't already have this package.
- `> remotes::install_github("milleratotag/pcurveMix", build_vignettes = TRUE)`
- `> library(pcurveMix)

You should then be able to see the vignettes with `> browseVignettes(package = "pcurveMix")`

