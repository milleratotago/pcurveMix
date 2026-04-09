#Test file

# # Load latest package
# remove.packages("pcurveMix")` and then reinstall with
# remotes::install_github("milleratotago/pcurveMix", build_vignettes = TRUE)`
# packageVersion("pcurveMix")



# Test fitting
set.seed(8888)

ps <- pcurveMix::random(n = 10000, mu = 2, sigma = 2, pi = 0.5)

ps[ps <= 0 | ps >= 1] # check if values are outside 0-1

ps[ps==0] <- 1e-40    # no zeros

ps_out <- format(ps, scientific = FALSE, trim = TRUE, decimal.mark = ".")

write.table(data.frame(p = ps_out),
            file = "pvalues.csv",
            sep = ",",
            row.names = FALSE,
            col.names = TRUE,
            quote = FALSE)

alpha_cuff <- 1

fit<-pcurveMix::fit_p_curve(ps_out,alpha=alpha_cuff)

pcurveMix::fit_to_estimates_tbl(fit)

sum(ps_out < alpha_cuff, na.rm=TRUE)

#JEFF: How can I use pcurveMIX to compare plots of cdfs: data vs. predicted?
# NEWROLF: This was illustrated in the last section of demo_fit_and_report.Rmd,
# but now I have made new functions to do this:

quick_cdf_plot(ps, fit)

quick_pdf_plot(ps, fit)

# # The following is the necessary code, adapted to this example.
# # I do not want to encapsulate this because I think users will want
# # to make plots to their own specifications.
# library(ggplot2)
# p <- seq(0.001, alpha_cuff, 0.002)
# pred_cdfs <- cdf(p, mu = fit$mu, sigma = fit$sigma, pi = fit$pi,
#                  alpha = alpha_cuff)  # compute predicted
# cdf_df <- data.frame(p = ps_out)
# cdf_plot <- ggplot() +
#   stat_ecdf(data = cdf_df, ggplot2::aes(x = p), geom = "step") +  # empirical cdf
#   geom_line(aes(x = p, y = pred_cdfs), color = "red") +  # predicted cdf
#   labs(title = "Observed (black) vs predicted (red) CDFs",
#        x = "p value",
#        y = "cumulative proportion")
# print(cdf_plot)
