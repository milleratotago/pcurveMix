# dev_jack.R
# Debugging wild jackknife values with sample file

devtools::load_all("C:/R/Projects/pcurveMix_pkg/pkg")

sample_csv <- read.csv("C:/R/Projects/pcurveMix_pkg/pkg/inst/extdata/sample_ps.csv")
ps <- sample_csv$p

fit_list <- fit_p_curve(ps)
ps_mat <- generate_jackknife_subsamples(ps)
progressr::handlers(global = TRUE)
# Jackknife estimates vary widely when using default start_parms,
# They are much more consistent when starting at the mle_ests,
# but they consistently converge to a point _very different_
# from the mle_ests!
# mle_ests <- list(mu = fit_list$mu, sigma = fit_list$sigma, pi = fit_list$pi)
# ests_tbl <- fits_for_matrix(ps_mat, start_parms = mle_ests)
ests_tbl <- fits_for_matrix(ps_mat)
summaries <- get_parm_summaries(ests_tbl)
full_sample_n <- length(ps)
jack_tbl <- jackknife_computations(fit_list, summaries, full_sample_n)

fit_list2 <- fit_p_curve(ps, start_parms = mle_ests)
fit_list3 <- fit_p_curve(ps, start_parms = make_optim_starting_parms_df() )
