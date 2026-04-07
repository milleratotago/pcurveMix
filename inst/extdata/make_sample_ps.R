# Make two files with sample p values:

library(pcurveMix)

set.seed(1234)  # for reproducability

output_path <- "C:/R/Projects/pcurveMix/pkg/inst/extdata/"
# Some arbitrary parameter values for examples.
true_mu <- 2
true_sigma <- 2
true_pi <- 0.3
n_ps <- 2000

# Sample 1: Full range of p values, 2-tailed
alpha_cutoff <- 1 # allow full range of p's, not selected for p<alpha significance
tails <- 2
sample_ps <- pcurveMix::random(n_ps, mu = true_mu, sigma = true_sigma, pi = true_pi, alpha = alpha_cutoff, tails = tails)
df <- data.frame(p = sample_ps)
file_name <- "sample_ps.csv"
write.csv(df, file = paste0(output_path,file_name), row.names = FALSE)

return()

# Sample 2: Significant p values, 2-tailed
alpha_cutoff <- 0.05 # selected for significance
tails <- 2
sample_ps <- pcurveMix::random(n_ps, mu = true_mu, sigma = true_sigma, pi = true_pi, alpha = alpha_cutoff, tails = tails)
df <- data.frame(p = sample_ps)
file_name <- "Sig_ps_2-tailed.csv"
write.csv(df, file = paste0(output_path,file_name), row.names = FALSE)

# Sample 3: Full range of p values, 1-tailed
alpha_cutoff <- 1 # allow full range of p's, not selected for p<alpha significance
tails <- 1
sample_ps <- pcurveMix::random(n_ps, mu = true_mu, sigma = true_sigma, pi = true_pi, alpha = alpha_cutoff, tails = tails)
df <- data.frame(p = sample_ps)
file_name <- "All_ps_1-tailed.csv"
write.csv(df, file = paste0(output_path,file_name), row.names = FALSE)

# Sample 2: Significant p values, 1-tailed
alpha_cutoff <- 0.05 # selected for significance
tails <- 1
sample_ps <- pcurveMix::random(n_ps, mu = true_mu, sigma = true_sigma, pi = true_pi, alpha = alpha_cutoff, tails = tails)
df <- data.frame(p = sample_ps)
file_name <- "Sig_ps_1-tailed.csv"
write.csv(df, file = paste0(output_path,file_name), row.names = FALSE)


