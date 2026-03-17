test_that("Reproduce values in fig 3 caption", {
  sigma <- c(0, 0.125, 0.25, 0.5, 1.0, 2.0)
  n_sigmas <- length(sigma)
  mu <- 1.5
  pi <- 0.5
  alpha <- 0.05
  pwr_a <- numeric(n_sigmas)
  pwr_b <- numeric(n_sigmas)
  for (i in 1:n_sigmas) {
    pwr_a[i] <- cdf(alpha, mu = mu, sigma = sigma[i])
    pwr_b[i] <- cdf(alpha, mu = mu, sigma = sigma[i], pi = 0.5)
  }
  expect_equal( round(pwr_a,2), c(0.32, 0.32, 0.33, 0.34, 0.38, 0.48) )
  expect_equal( round(pwr_b,2), c(0.19, 0.19, 0.19, 0.20, 0.21, 0.26) )
})
