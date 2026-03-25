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

Functions are also provided to compute the model's predicted PDFs of *p*, CDFs of *p*, and quantiles, as well as to generate random *p* values
under any specified set of parameter values.

Both one- and two-tailed *p* values are supported.

## Installation

Install package **pcurveMix** directly from source via GitHub using the command in RStudio: 

```r
> remotes::install_github("milleratotago/pcurveMix", build_vignettes = TRUE)
```

The package files are installed into R's standard package directory, wherever that is on your computer, just as if you were installing from CRAN.

The previous command requires the package `remotes` to be installed on your machine.
If package `remotes` is not already installed, first run the command `install.packages("remotes")`.
(This command only needs to be executed once per machine.)

## Confirm Install

Check that the installation has been successful.

```r
> # Call a pcurveMix function with test values. This method generates random
> # p-curves from a random-effects model
> ps <- pcurveMix::random(n = 20, mu = 2, sigma = 1)
> print(ps)
> # This should give 20 random p values.
```

```r
> # Make sure that RStudio can load the library successfully.
> library(pcurveMix)
```

```r
> # Display the source code of one of pcurveMix's functions
> pcurveMix::bootstrap
```

## Demos

After installation, you should be able to see the demonstration vignettes with

```r
> browseVignettes(package = "pcurveMix")
```

These vignettes illustrate different computations and model fitting scenarios.

## Launch the Companion RShiny Application

With this GUI tool, you can upload a csv file of p-values and quickly fit the model. 
Here is how to do start the shiny app from inside RStudio:

```r
> # Find the path to the RShiny app.R file as installed on your machine
> shiny_path <- system.file("shiny", "app.R", package = "pcurveMix")

> # Launch the shiny application:
> shiny::runApp(shiny_path)
```

A simple example input file is included with the package install. Its path can
be found by running this statement in the RStudio console:

`system.file("shiny", "sample_ps.csv", package = "pcurveMix")`

