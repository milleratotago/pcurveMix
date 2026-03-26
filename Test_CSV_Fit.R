#Test file 

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

packageVersion("pcurveMix")

