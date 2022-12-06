#This script loads in the stanfit object and outputs some initial summary info
load(stanfit.RData)
dir.create("Outputs")

#Our parameters and priors:
#beta0 ~ normal(0.0, 1.0);
#betas ~ normal(0.0, 1.0); #covariate coefficients
#zetas ~ gamma(0.05, 1.0); #spatial kernel  1-306
#theta[,i] ~ normal(0.0, 1.0); #noise

#Output summaries of the mixed chains for all parameters except theta
main_summaries <- summary(stanfit, pars = c('beta0', 'betas', 'zetas', 'lp__'))
write.csv(main_summaries$summary,"Outputs/main_summaries.csv", row.names = TRUE)
#Theta summaries are big, but we output anyway
theta_summaries <- summary(stanfit, pars = c('theta'))
write.csv(theta_summaries$summary,"Outputs/theta_summaries.csv", row.names = FALSE)

#Save a plot of the beta trajectories
beta_trajectories <- traceplot(stanfit, pars=c('beta0', 'betas'))
png(file="Outputs\\beta_trajectories.png",
    width=1440, height=1080, res = 150)
plot(beta_trajectories)
dev.off()

#We'll save the zeta plots, though unlikely to be that interested...
dir.create("Outputs\\zeta_plots")
for(i in 1:19){
  zeta_trajectories <- traceplot(stanfit, pars =sprintf('zetas[%s]',((i-1)*16 + 1):(16*i)))
  png(file=sprintf("Outputs\\zeta_plots\\zeta_trajectories_%s.png", i),
      width=1440, height=1080, res = 150)
  plot(zeta_trajectories)
  dev.off()
}

zeta_trajectories <- traceplot(stanfit, pars =sprintf('zetas[%s]',304:306))
png(file=sprintf("Outputs\\zeta_plots\\zeta_trajectories_%s.png", 20),
    width=1440, height=1080, res = 150)
plot(zeta_trajectories)
dev.off()

#Next we give some sense of how our thetas are.
#Theta_means <- get_posterior_mean(stanfit, pars = 'theta')

#Reminder of what all the betas correspond to:
#1 - Pop_per_km2
#2 - cumVaccPercentage_FirstDose
#3 - cumVaccPercentage_SecondDose
#4 - cumVaccPercentage_ThirdDose
#5 - prop_white_british
#6 - IMD_Average_score
#7 - mean_age
#8 - Median_annual_income
#9 - workplaces_percent_change_from_baseline
#10 - residential_percent_change_from_baseline
#11 - Alpha_proportion
#12 - Delta_proportion
#13 - Delta_AY_4_2_proportion
#14 - Omicron_BA_1_proportion
#15 - Omicron_BA_2_proportion
#16 - Omicron_BA_4_proportion
#17 - Omicron_BA_5_proportion
#18 - Other_proportion

beta_summaries <- main_summaries$summary
#Next we have a look at how good our fit is
betas_1_10 <- plot(stanfit, pars = sprintf('betas[%s]',1:10))
#ci_level: 0.8 (80% intervals)
#outer_level: 0.95 (95% intervals)
betas_1_10 <- betas_1_10 + scale_y_continuous(breaks = c(10:1),
                                labels = c("1) Pop_per_km2", "2) CumVacc_1dose", 
                                         "3) CumVacc_2dose", "4) CumVacc_3dose",
                                         "5) prop_white_british", "6) IMD_Average_score",
                                         "7) mean_age", "8) Median_annual_income",
                                         "9) workplaces_movement",
                                         "10) residential_movement")) +
  geom_vline(xintercept = 0, color = "skyblue", lty = 5, size = 1) +ggtitle("Covariate coefficients")

png(file="Outputs\\betas_1_10.png",
    width=1440, height=1080, res = 150)
plot(betas_1_10)
dev.off()


betas_11_18 <- plot(stanfit, pars = sprintf('betas[%s]',11:18))
#ci_level: 0.8 (80% intervals)
#outer_level: 0.95 (95% intervals)
betas_11_18 <- betas_11_18 + scale_y_continuous(breaks = c(8:1),
                                              labels = c("11) Alpha", "12) Delta", 
                                                         "13) Delta_AY_4_2", "14) Omicron_BA_1",
                                                         "15) Omicron_BA_2", "16) Omicron_BA_4",
                                                         "17) Omicron_BA_5", "18) Other variants")) +
  geom_vline(xintercept = 0, color = "skyblue", lty = 5, size = 1) +ggtitle("Variant proportion coefficients")

png(file="Outputs\\betas_11_18.png",
    width=1440, height=1080, res = 150)
plot(betas_11_18)
dev.off()

# MCMCplot(stanfit, 
#          params = 'betas[1]',
#          rank = TRUE,
#          xlab = 'ESTIMATE')

# log_pp <- stan_diag(stanfit)
# png(file="Outputs\\lpp_acceptance.png",
#     width=1440, height=1080, res = 150)
# plot(log_pp)
# dev.off()

#Other stuff
sampler_params <- get_sampler_params(stanfit, inc_warmup = FALSE)
#To do things like calculate the average value of accept_stat__ for each chain 
#(or the maximum value of treedepth__ for each chain if using the NUTS algorithm, etc.) 
#the sapply function is useful as it will apply the same function to each component 
#of sampler_params:
mean_accept_stat_by_chain <- sapply(sampler_params, function(x) mean(x[, "accept_stat__"]))
print(mean_accept_stat_by_chain)


#Investigate how good the model fit is against real data
load(model_data.RData)
#y is the REAL "next week cases". 103 (time) x 306 (LTLAs)



test <- get_posterior_mean(stanfit, pars = 'betas[2]')
#Compare means to the data:
#Mus <- rstan::extract(stanfit, pars = 'mu')
Mus <- get_posterior_mean(stanfit, pars = 'mu')
plot(log(y), log(Mus[,4]))

Phis <- get_posterior_mean(stanfit, pars = 'phi')
traceplot(stanfit, pars=c('theta[1,2]'))



stan_diag(stanfit)
stan_rhat(stanfit)
stan_mcse(stanfit)
