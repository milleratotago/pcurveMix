test_that("Check that random number generators give p values in range 0-alpha_cutoff and match predicted distributions.", {
  set.seed(1243)  # min p about 0.067 with this seed
  mu <- 1.4
  sigma <- 0.7
  n_random <- 2000
  for (pi in c(seq(0.1,0.9,0.2), 1)) {
    for (alpha_cutoff in c(0.05, 0.5, 1)) {
      for (tails in 1:2) {
        pvals <- random(n_random, mu = mu, sigma = sigma, pi = pi, alpha = alpha_cutoff, tails = tails)
        ok <- pvals >= 0 & pvals <= alpha_cutoff
        expect_equal(sum(ok),n_random)
        cdf_fit <- function(x) cdf(x, mu = mu, sigma = sigma, pi = pi, alpha = alpha_cutoff, tails = tails)
        ks <- ks_with_cdf(pvals, cdf_fit)
        # print(ks$p.value)
        expect_equal(ks$p.value>0.05, TRUE)
      } # for tails
    } # for alpha
  } # for pi
})
