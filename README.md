# pcurveMix

This is an R package for computations and model-fitting with *p*-curves under the Ulrich &amp; Miller (2026, in preparation) mixture model.

According to the model, an observed set of *p* values comes from a population of studies within which
the proportion $\pi$ test for effects that are actually present (i.e., H1 is true)
and the proportion $1-\pi$ test for effects that are actually absent (i.e., H0 is true).
Among studies testing for effects that are present, the noncentrality parameters vary
normally with mean $\mu$ and standard deviation $\sigma$.

This package fits the model by maximum likelihood to a set of observed *p* values, which can come
either from a complete set of studies (i.e., 0<*p*<1) or from a set of studies
selected to have significant results (i.e., $p<\alpha$).
The fitted results include estimates of the model's parameters and standard errors of those estimates.

Functions are also provided to compute the model's predicted PDFs of *p*, CDFs of *p*, and quantiles,
as well as to generate random *p* values,
under any specified set of parameter values.

Both one- and two-tailed *p* values are supported.

## Installation

Install package **pcurveMix** directly from source via GitHub using the command in RStudio: 

```r
remotes::install_github("milleratotago/pcurveMix", build_vignettes = TRUE)
```

You can ignore the warnings about masked objects.

The package files are installed into R's standard package directory, wherever that is on your computer, just as if you were installing from CRAN.

The `remotes::install_github` command requires that the package `remotes` was already installed on your computer.
If that is not the case, you must first run the command `install.packages("remotes")`.
(This command only needs to be executed once per computer.)

#### Confirm Install

Here are a few brief commands to check that the installation has been successful.

```r
# Make sure that RStudio can load the library successfully.
library(pcurveMix)
# Don't worry about "object masked" messages.
```

```r
# Call a pcurveMix function with test values. This method generates random
# p-curves from a random-effects model:
ps <- pcurveMix::random(n = 20, mu = 2, sigma = 1)
print(ps)
# This should print 20 random p values.
```

## Getting started

The best way to start is by starting the shiny app and looking at the vignette 'Intro'.
The commands for that are:

```r
run_shiny_app()
vignette("Intro", package = "pcurveMix")
```

The first command opens the shiny app in one window, and the second shows the `Intro` vignette in the RStudio Help window.
You should be able to run the shiny app and make sense of its results by going back and
forth between the shiny app and the `Intro` vignette.
The first section of the vignette ("Option 1") explains how to set the various options in the shiny app,
start the model fitting process, and download the results (if desired).
The second section of the vignette ("Option 2") shows how to do the model-fitting with an `R`
script instead of the shiny app, and this section explains step-by-step what the fitting results outputs mean.

It is intended that you would upload a CSV file of p-values to which you would like to fit the model. 
Some example input CSV files are included with the package, and the path to them is shown under the shiny app's `Browse` box.

## Further help

For the RStudio overview of the package, type
```r
?pcurveMix
```

To generate a complete list of all exported functions in the RStudio Help pane, use the command:

```r
help(package = "pcurveMix")
```

Then click on the name of an exported function for its description and arguments.

In addition to the `Intro` vignette, there are several other vignettes illustrating
various model fitting scenarios and model-based computations and simulations.
You can see a full list of the vignettes with the RStudio command:

```r
browseVignettes(package = "pcurveMix")
```

This will open a browser window with links to the vignettes, and you can
click on a link to see the indicated version of the vignette.