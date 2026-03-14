test_that("Reproduce values in fig 2 caption", {
  sigma <- c(0, 0.25, 0.5, 1.0, 2.0)
  n_sigmas <- length(sigma)
  mu <- 3
  pi <- 0.5
  alpha <- 0.05
  pwr_a <- numeric(n_sigmas)
  pwr_b <- numeric(n_sigmas)
  for (i in 1:n_sigmas) {
   pwr_a[i] <- cdf(alpha, mu = mu, sigma = sigma[i])
   pwr_b[i] <- cdf(alpha, mu = mu, sigma = sigma[i], pi = 0.5)
  }
  expect_equal( round(pwr_a,2), c(0.85, 0.84, 0.82, 0.77, 0.69) )
  expect_equal( round(pwr_b,2), c(0.45, 0.45, 0.44, 0.41, 0.37) )
})
