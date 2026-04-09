# Compare two models for significant two-sided p-values
# Model 1: Draw from the full mixture and keep only significant p-values
# Model 2: Draw directly from a mixture of already truncated components

# ----------------------------
# Parameters
# ----------------------------
n        <- 5000   # Number of observed p-values per model
pi       <- 0.40   # Base rate / mixture weight
mu_c     <- 2.0    # Mean of random effects
sigma_c  <- 2.0    # SD of random effects
alpha    <- 0.05   # Two-sided significance threshold 
tiny     <- 1e-15  # Replacement for exact zeros

# ----------------------------
# Model 1
# ----------------------------
# With probability pi:
#   mu_i ~ N(mu_c, sigma_c^2)
#   z    ~ N(mu_i, 1)
# With probability 1 - pi:
#   z    ~ N(0, 1)
# Only significant p-values are observed:
#   p = 2 * (1 - Phi(|z|)) <= alpha

Model.1 <- function(pi, mu_c, sigma_c, alpha = 0.05, tiny = 1e-15) {
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

Model.1.n <- function(n, pi, mu_c, sigma_c, alpha = 0.05, tiny = 1e-15) {
  replicate(n, Model.1(pi = pi, mu_c = mu_c, sigma_c = sigma_c,
                       alpha = alpha, tiny = tiny))
}

# ----------------------------
# Model 2
# ----------------------------
# Draw one z-value from N(mu, 1), truncated to the two tails
# z <= -c_alpha or z >= c_alpha, where c_alpha is the two-sided
# critical value corresponding to alpha.

r_trunc_two_tails_1 <- function(mu, alpha = 0.05) {
  c_alpha <- qnorm(1 - alpha / 2)

  p_left  <- pnorm(-c_alpha, mean = mu, sd = 1)
  p_right <- 1 - pnorm(c_alpha, mean = mu, sd = 1)
  p_total <- p_left + p_right

  if (p_total <= 0) {
    stop("The truncation region has probability 0.")
  }

  if (runif(1) < p_left / p_total) {
    u <- runif(1, min = 0, max = p_left)
    z <- qnorm(u, mean = mu, sd = 1)
  } else {
    u <- runif(1, min = pnorm(c_alpha, mean = mu, sd = 1), max = 1)
    z <- qnorm(u, mean = mu, sd = 1)
  }

  z
}

# ----------------------------
# Model 2
# ----------------------------
# With probability pi:
#   mu_i ~ N(mu_c, sigma_c^2)
#   z    ~ N(mu_i, 1), already truncated to significance
# With probability 1 - pi:
#   z    ~ N(0, 1), already truncated to significance
# This is a mixture of already truncated components.

Model.2 <- function(pi, mu_c, sigma_c, alpha = 0.05, tiny = 1e-15) {
  if (runif(1) < pi) {
    mu_i <- rnorm(1, mean = mu_c, sd = sigma_c)
    z    <- r_trunc_two_tails_1(mu = mu_i, alpha = alpha)
  } else {
    z <- r_trunc_two_tails_1(mu = 0, alpha = alpha)
  }

  p <- 2 * (1 - pnorm(abs(z)))
  p <- max(p, tiny)

  return(p)
}

Model.2.n <- function(n, pi, mu_c, sigma_c, alpha = 0.05, tiny = 1e-15) {
  replicate(n, Model.2(pi = pi, mu_c = mu_c, sigma_c = sigma_c,
                       alpha = alpha, tiny = tiny))
}

# ----------------------------
# Simulate both models
# ----------------------------
set.seed(123)

p_values_1 <- Model.1.n(n = n, pi = pi, mu_c = mu_c, sigma_c = sigma_c,
                        alpha = alpha, tiny = tiny)

p_values_2 <- Model.2.n(n = n, pi = pi, mu_c = mu_c, sigma_c = sigma_c,
                        alpha = alpha, tiny = tiny)

# ----------------------------
# Empirical CDFs
# ----------------------------
ecdf_1 <- ecdf(p_values_1)
ecdf_2 <- ecdf(p_values_2)
grid   <- seq(0, alpha, length.out = 500)


plot(grid, ecdf_1(grid), type = "l", lwd = 2,
     xlab = "p", ylab = "Empirical CDF",
     main = "Empirical CDFs of Two Different Models",
     ylim = c(0, 1))

lines(grid, ecdf_2(grid), lwd = 2, lty = 2)

legend("bottomright",
       legend = c("Model 1: full mixture, then selection",
                  "Model 2: mixture of truncated components"),
       lwd = 2, lty = c(1, 2), bty = "n")


