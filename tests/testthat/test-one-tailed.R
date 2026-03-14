test_that("one-tailed pdf & cdf", {
  ps <- seq(0.1,0.9,0.1)
  # Testing under H0, alpha = 1
  mu <- 0
  sigma <- 0
  for (p in ps) {
    cdf1 <- cdf(p, mu, sigma, tails = 1)
    pdf1 <- pdf(p, mu, sigma, tails = 1)
    expect_equal( round(pdf1,4), 1 )
    expect_equal( round(cdf1,4), p )
  }
  # Testing under H0, alpha = 0.05
  alpha <- 0.05
  ps2 <- alpha * ps
  for (p in ps2) {
    cdf1 <- cdf(p, mu, sigma, tails = 1, alpha = alpha)
    pdf1 <- pdf(p, mu, sigma, tails = 1, alpha = alpha)
    expect_equal( round(pdf1,4), round(1/alpha,4) )
    expect_equal( round(cdf1,4), round(p/alpha,4) )
  }

  # Testing under H1 with the following 2 original functions from
  # Rolf's toolbox_onesided.R

  f_alt_pdf <- function(p, mu, sigma) {
    # p: one-sided p-values (upper tail), p = 1 - Phi(Z)
    p <- pmin(pmax(as.numeric(p), 1e-12), 1 - 1e-12)
    z <- qnorm(1 - p)                # Z = Phi^{-1}(1 - p)
    s1 <- sqrt(1 + sigma^2)

    # f_P(p) = (1/s1) * phi((z-mu)/s1) / phi(z)
    # log f = -log s1 - 0.5 * [((z-mu)^2 / s1^2) - z^2]
    log_f <- -log(s1) - 0.5 * (((z - mu)^2 / (s1^2)) - z^2)
    exp(log_f)
  }

  F_alt_cdf <- function(p, mu, sigma) {
    # CDF von p unter H1: F_P(p) = P(p_val <= p)
    # p = 1 - Phi(Z), p klein <-> Z groß (rechter Rand)
    p <- pmin(pmax(as.numeric(p), 1e-12), 1 - 1e-12)
    z_thr <- qnorm(1 - p)                # Schwelle in Z-Skala
    s1 <- sqrt(1 + sigma^2)
    # P(Z >= z_thr) = 1 - Phi((z_thr - mu)/s1)
    1 - pnorm((z_thr - mu) / s1)
  }

  mu_h1 <- 1
  sigma_h1 <- 0.5
  for (p in ps) {
    pdf1a <- pdf(p, mu_h1, sigma_h1, tails = 1)
    pdf1b <- f_alt_pdf(p, mu_h1, sigma_h1)
    expect_equal( round(pdf1a,4), round(pdf1b,4) )
    cdf1a <- cdf(p, mu_h1, sigma_h1, tails = 1)
    cdf1b <- F_alt_cdf(p, mu_h1, sigma_h1)
    expect_equal( round(cdf1a,4), round(cdf1b,4) )
  }
})
