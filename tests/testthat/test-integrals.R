test_that("Check that integrals of PDFs equal differences of CDFs", {
  prop_low <- 0.02
  prop_hi <- 0.22
  mu <- 1.4
  sigma <- 0.7
  for (pi in c(seq(0.1,0.9,0.2), 1)) {
    for (alpha in c(0.05, 0.5, 1)) {
      for (tails in 1:2) {
        p_low <- prop_low * alpha
        p_hi <- prop_hi * alpha
        result <- integrate(pdf, lower = p_low, upper = p_hi, mu = mu, sigma = sigma, pi = pi, alpha = alpha, tails = tails)
        int4 = round(result$value,4)
        dif4 = round(cdf(p_hi,  mu = mu, sigma = sigma, pi = pi, alpha = alpha, tails = tails) -
                     cdf(p_low, mu = mu, sigma = sigma, pi = pi, alpha = alpha, tails = tails), 4)
        expect_equal(int4, dif4)
        # Check that full-range integral of pdf = 1 (to 4 decimal places)
        full_integral <- integrate(pdf, lower = 0, upper = alpha, mu = mu, sigma = sigma, pi = pi, alpha = alpha, tails = tails)
        expect_equal(round(full_integral$value,4),1)
      } # for tails
    } # for alpha
  } # for pi
})
