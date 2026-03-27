# Simulate significant two-sided p-values 

n        <- 5000   # number of observed p-values
pi       <- 0.5    # base rate of true effects
mu_c     <- 1.0    # mean of random effects
sigma_c  <- 2.0    # SD of random effects
alpha    <- 0.05   # significance threshold (two-sided)
tiny     <- 1e-15  # replacement for zeros

# ----------------------------
# Simulate p-values
# ----------------------------

# Modell 1:
# With probability pi:
#   mu_i ~ N(mu_c, sigma_c^2)
#   z    ~ N(mu_i, 1)
# With probability 1 - pi:
#   z    ~ N(0, 1)
# Only significant p-values are observed:
#   p = 2 * (1 - Phi(|z|)) <= alpha


Modell.1 <- function(pi, mu_c, sigma_c, alpha = 0.05, tiny = 1e-15) {
  repeat {
    if (runif(1) < pi) {
      mu_i <- rnorm(1, mean = mu_c, sd = sigma_c)
      z    <- rnorm(1, mean = mu_i, sd = 1)
    } else {
      z <- rnorm(1, mean = 0, sd = 1)
    }
    
    p <- 2 * (1 - pnorm(abs(z)))
    
    if (p <= alpha) {
      p <- max(p, tiny)
      return(p)
    }
  }
}

# ----------------------------
# Simulate n p-values
# ----------------------------
p_values <- replicate(n, Modell.1(pi = pi, mu_c = mu_c, sigma_c = sigma_c,
                                  alpha = alpha, tiny = tiny))

#-------------------------------------
# Compare empirical and theoretical CDF
#--------------------------------------
plot(ecdf(p_values),main="Empirical vs. theoretical CDF")
p_seq <- seq(0.0001,0.05,0.0002)
pred_cdf <- pcurveMix::cdf(p_seq, mu = mu_c, sigma = sigma_c, pi = pi, alpha = 0.05, tails = 2)
lines(p_seq, pred_cdf, lty = "dashed",col="red")

