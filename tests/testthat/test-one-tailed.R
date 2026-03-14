test_that("one-tailed pdf & cdf", {
  ps <- seq(0.1,0.9,1)
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
})
