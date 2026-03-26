#This simulates one two-sided p-value form truncated normals.

r_p_mix_sig_1 <- function(pi, mu_c, sigma_c, alpha = 0.05) {
  r_trunc_two_tails_1 <- function(mu, sigma = 1, alpha) {
    c <- qnorm(1 - alpha / 2)
    
    p_left  <- pnorm(-c, mean = mu, sd = sigma)
    p_right <- 1 - pnorm(c, mean = mu, sd = sigma)
    p_total <- p_left + p_right
    
    if (p_total <= 0) {
      stop("The truncation region has probability 0.")
    }
    
    if (runif(1) < p_left / p_total) {
      u <- runif(1, min = 0, max = p_left)
      z <- qnorm(u, mean = mu, sd = sigma)
    } else {
      u <- runif(1, min = pnorm(c, mean = mu, sd = sigma), max = 1)
      z <- qnorm(u, mean = mu, sd = sigma)
    }
    
    z
  }
  
  mu_i <- rnorm(1, mean = mu_c, sd = sigma_c)
  
  if (runif(1) < pi) {
    z <- r_trunc_two_tails_1(mu = mu_i, sigma = 1, alpha = alpha)
  } else {
    z <- r_trunc_two_tails_1(mu = 0, sigma = 1, alpha = alpha)
  }
  
  p <- 2 * (1 - pnorm(abs(z)))
  p
}

set.seed(88)
p_values <- replicate(1500, r_p_mix_sig_1(pi = 0.4, mu_c = 2, sigma_c = 2, alpha = 0.05))
p_values


plot(ecdf(p_values))
