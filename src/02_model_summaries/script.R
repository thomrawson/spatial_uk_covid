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
write.csv(main_summaries$summary,"Outputs\main_summaries.csv", row.names = FALSE)
#Theta summaries are big, but we output anyway
theta_summaries <- summary(stanfit, pars = c('theta'))
write.csv(theta_summaries$summary,"Outputs\theta_summaries.csv", row.names = FALSE)

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
Theta_means <- get_posterior_mean(stanfit, pars = 'theta')


#Next we have a look at how good our fit is


#Other stuff
sampler_params <- get_sampler_params(stanfit, inc_warmup = FALSE)
#To do things like calculate the average value of accept_stat__ for each chain 
#(or the maximum value of treedepth__ for each chain if using the NUTS algorithm, etc.) 
#the sapply function is useful as it will apply the same function to each component 
#of sampler_params:
mean_accept_stat_by_chain <- sapply(sampler_params, function(x) mean(x[, "accept_stat__"]))
print(mean_accept_stat_by_chain)

test <- get_posterior_mean(stanfit, pars = 'betas[2]')
#Compare means to the data:
#Mus <- rstan::extract(stanfit, pars = 'mu')
Mus <- get_posterior_mean(stanfit, pars = 'mu')
plot(log(y), log(Mus[,4]))

Phis <- get_posterior_mean(stanfit, pars = 'phi')
traceplot(stanfit, pars=c('theta[1,2]'))
