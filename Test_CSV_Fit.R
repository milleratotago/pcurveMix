#Test file 

set.seed(88)

alpha_cutoff <- 0.05

ps <- pcurveMix::random(n = 500, mu = 2, sigma = 2, pi = 0.5)

ps[ps <= 0 | ps >= 1] # check if values are outside 0-1

ps_out <- format(ps, scientific = FALSE, trim = TRUE, decimal.mark = ".")

write.table(data.frame(p = ps_out),
            file = "C:/Users/siaul01/GitHub/pcurveMix/pvalues.csv",
            sep = ",",
            row.names = FALSE,
            col.names = TRUE,
            quote = FALSE)

fit<-pcurveMix::fit_p_curve(ps_out)

pcurveMix::fit_to_estimates_tbl(fit) # Estimates? 

