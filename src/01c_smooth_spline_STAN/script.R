## Script to employ the method's presented in Sophie Lee's paper;
## "A Bayesian modelling framework to quantify multiple sources of spatial variation for disease mapping"
## https://doi.org/10.1098/rsif.2022.0440

##########################################################################

#Load the Cases data
load("Cases_Data.RData")

#Load the Boundaries data
load("Boundaries_Data.RData")

#if you are using rstan locally on a multicore machine and have plenty of RAM to
#estimate your model in parallel, at this point execute
options(mc.cores = parallel::detectCores())
#and
rstan_options(auto_write = TRUE)
#which allows you to automatically save a bare version of a compiled Stan program 
#to the hard disk so that it does not need to be recompiled (unless you change it). 
#You will need to run these commands each time you load the rstan library.

### Scale coordinates to lie between [0, 1] 
Case_Rates_Data <- Case_Rates_Data %>% 
  mutate(y_scale = (centroid_y - min(centroid_y))/(max(centroid_y) - min(centroid_y)),
         x_scale = (centroid_x - min(centroid_x))/(max(centroid_x) - min(centroid_x)))


#First, we are going to look at the spatial variation for just one Week. 
#Let's say Week 50; (very low cases) Week 85 for very high.
#We will loop over all weeks
#Goes from 2 - 129
#Weeks_to_assess <- unique(Case_Rates_Data$Week)
Weeks_to_assess <- c(50,85)

#Data files I'll fill up as we go:
sp_k_10_smoothing_parameters <- data.frame(Week = rep(NA,length(Weeks_to_assess)), 
                                           sp = rep(NA,length(Weeks_to_assess)))
sp_k_20_smoothing_parameters <- data.frame(Week = rep(NA,length(Weeks_to_assess)), 
                                           sp = rep(NA,length(Weeks_to_assess)))
sp_k_40_smoothing_parameters <- data.frame(Week = rep(NA,length(Weeks_to_assess)), 
                                           sp = rep(NA,length(Weeks_to_assess)))

GR_diag_over_1_point_1 <- data.frame(Week = rep(NA,length(Weeks_to_assess)), 
                                     GR_over_point1 = rep(NA,length(Weeks_to_assess)))
Intercept_estimate <- data.frame(Week = rep(NA,length(Weeks_to_assess)), 
                                     intercept_b1 = rep(NA,length(Weeks_to_assess)))
Mixing_estimate <- data.frame(Week = rep(NA,length(Weeks_to_assess)), 
                                 phi_est = rep(NA,length(Weeks_to_assess)))
MAE_estimate <- data.frame(Week = rep(NA,length(Weeks_to_assess)), 
                   mae = rep(NA,length(Weeks_to_assess)))

dir.create("outputs")
dir.create("outputs/plots")
dir.create("outputs/plots/diagnostic_plots")
dir.create("outputs/plots/Cases")
dir.create("outputs/plots/Cases_by_population")
dir.create("outputs/plots/Cases_by_population_via_smooth_spline")
dir.create("outputs/posterior_plots")
dir.create("outputs/fitted_spatial_components")
dir.create("outputs/model_fits")
dir.create("outputs/plots/Combined")

for(i in 1:length(Weeks_to_assess)){

  Week_isolated <- Weeks_to_assess[i]  

Reduced_Data <- filter(Case_Rates_Data, Week == Week_isolated)


#############################
#NOW FOR ACTUAL MODELLING
###########################

#### Step 1: Fit the spatial smooth model ####
## Fit GAM to obtain sensible priors for lambdas (sp)
#Start with k = 10

sp_check_10 <- gam(Week_Cases ~  s(y_scale, x_scale, k = 10, bs = "tp"), offset = log(Population),
             family = "poisson", data = Reduced_Data, method = "REML")

#summary(sp_check_10)

#sp_check_10$sp
#~ 0.48, which is quite high, but not weird or anything.
sp_k_10_smoothing_parameters$Week[i] <- Week_isolated
sp_k_10_smoothing_parameters$sp <- sp_check_10$sp

#Let's just plot the cases first:
plot_data <- Reduced_Data[,c(1,4,8)]
colnames(plot_data) <- c("CODE", "Population", "Week_Cases")

Boundaries %>% 
  inner_join(plot_data, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = Week_Cases), lwd =  .05) +
  scale_fill_viridis_c(name = sprintf("Week %s Cases", Week_isolated)) +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> Cases_plot

ggsave(Cases_plot, 
       filename = sprintf("outputs/plots/Cases/Week_%s_cases.png", Week_isolated))

#And plot divided by population
Boundaries %>% 
  inner_join(plot_data, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = Week_Cases/Population), lwd =  .05) +
  scale_fill_viridis_c(name = sprintf("Week %s Cases/Population", Week_isolated)) +
  theme_void() +
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> Cases_by_pop_plot

ggsave(Cases_by_pop_plot, 
       filename = sprintf("outputs/plots/Cases_by_population/Week_%s_cases_by_pop.png", Week_isolated))


#Now plot the fit out of the GAM
plot_data$GAM_fit <- sp_check_10$fitted.values

Boundaries %>% 
  inner_join(plot_data, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = GAM_fit/Population), lwd =  .05) +
  scale_fill_viridis_c(name = sprintf("Week %s GAM k=10 Cases/Population",Week_isolated)) +
  theme_void() +
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> Gam_k10_plot

ggsave(Gam_k10_plot, 
       filename = sprintf("outputs/plots/Cases_by_population_via_smooth_spline/Week_%s_k_10.png", Week_isolated))



#What if I give the smoothing spline more basis functions to work with?
sp_check_20 <- gam(Week_Cases ~  s(y_scale, x_scale, k = 20, bs = "tp"), offset = log(Population),
                   family = "poisson", data = Reduced_Data, method = "REML")

#summary(sp_check_20)
#Significant...
#sp_check_20$sp
sp_k_20_smoothing_parameters$Week[i] <- Week_isolated
sp_k_20_smoothing_parameters$sp <- sp_check_20$sp
#~ 0.235, which makes sense, twice the penalty, with double the basis functions
plot_data$GAM_fit_20 <- sp_check_20$fitted.values

Boundaries %>% 
  inner_join(plot_data, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = GAM_fit_20/Population), lwd =  .05) +
  scale_fill_viridis_c(name = sprintf("Week %s GAM k=20 Cases/Population",Week_isolated)) +
  theme_void() +
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> Gam_k20_plot

ggsave(Gam_k20_plot, 
       filename = sprintf("outputs/plots/Cases_by_population_via_smooth_spline/Week_%s_k_20.png", Week_isolated))



#This DOES look better, but is ofc a bit... rougher
#We can do an ANOVA to have a look at if this 20 version is significantly better:

#anova(sp_check, sp_check_20, test = "Chisq")

#HUGELY so. What about 40 basis functions?

sp_check_40 <- gam(Week_Cases ~  s(y_scale, x_scale, k = 40, bs = "tp"), offset = log(Population),
                   family = "poisson", data = Reduced_Data, method = "REML")

#summary(sp_check_40)
#Significant...
#sp_check_40$sp
#~ 0.235, which makes sense, twice the penalty, with double the basis functions
sp_k_40_smoothing_parameters$Week[i] <- Week_isolated
sp_k_40_smoothing_parameters$sp <- sp_check_40$sp


plot_data$GAM_fit_40 <- sp_check_40$fitted.values

Boundaries %>% 
  inner_join(plot_data, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = GAM_fit_40/Population), lwd =  .05) +
  scale_fill_viridis_c(name = sprintf("Week %s GAM k=40 Cases/Population",Week_isolated)) +
  theme_void() +
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> Gam_k40_plot

ggsave(Gam_k40_plot, 
       filename = sprintf("outputs/plots/Cases_by_population_via_smooth_spline/Week_%s_k_40.png", Week_isolated))



#This actually doesn't look too different, just a little tighter maybe
#anova(sp_check_20, sp_check_40, test = "Chisq") #Still hugely better apparently.
#Warning message:
#using F test with a 'poisson' family is inappropriate 
#See: https://katrienantonio.github.io/Risk-modelling-in-insurance/glms.html
#instead:
#anova(sp_check_20, sp_check_40, test = "Chisq")
#Still way more significant


#NOTE WE ONLY DO THIS TO obtain sensible priors for lambdas (sp)
#But also of course to get an idea of how well the smoothing spline will perform.

###################################################################################
# NOW FOR THE BAYESIAN MODELLING
# Start with just distance-based, and heterogeneous noise.

### Fit a model with a smooth spatial random effect and an IID random
## effect in NIMBLE to data 

# Use mgcv to extract basis functions for the smooth term
jd_smooth <- jagam(Week_Cases ~  s(y_scale, x_scale, k = 40, bs = "tp"),
                   offset = log(Population),
                   # Save JAGS code to create model
                   file = "jagam_ten.txt", 
                   # Set prior for smoothing function (in this case, the spatial smooth)
                   sp.prior = "gamma", 
                   family = poisson,
                   # If T, smooths are re-parameterised to have an iid Gaussian prior (not appropriate here)
                   # diagonalize cannot be used with >1 dimension
                   diagonalize = F, 
                   data = Reduced_Data)

# Extract basis functions to use as linear predictors in the model
X <- jd_smooth$jags.data$X


### Write model formula
Model <- nimbleCode({ 
  
  # u = spatial smooth term (using basis funtions)
  u[1:n] <- X[1:n, 2:m] %*% b[2:m] #b is the coefficients, beta
  
  for (i in 1:n) { 
    # y = number of cases
    y[i] ~ dpois(mu[i]) 
    
    log(mu[i]) <- b[1] + u[i] + v[i] + log(e[i])
    
    # v = iid random effect
    v[i] ~ dnorm(0, sd = sig_re)
  } 
  
  # Priors
  # Random effect SD
  sig_re ~ dexp(.1)
  
  # Intercept
  b[1] ~ dnorm(0, sd = 5) 
  
  ## prior for sd(smooth function)
  K1[1:(m-1), 1:(m-1)] <- S1[1:(m-1), 1:(m-1)] * lambda[1] + 
    S1[1:(m-1), m:(2*(m-1))] * lambda[2]
  
  # Prior for smooth coefficients
  b[2:m] ~ dmnorm(zero[2:m], K1[1:(m-1), 1:(m-1)]) 
  
  ## smoothing parameter priors 
  for (i in 1:2) {
    # truncate lambdas to avoid simulations getting stuck
    lambda[i] ~ T(dgamma(.05, .005), 0, 5)
  }
} )

Stan_model_string = "
data {
  int<lower=0> N; // Number of areas
  int<lower=0> M; // Number of basis functions


  int<lower=0> y[N];              // count outcomes (current week's cases)
  //int<lower=1> K;                 // num covariates 
  int<lower=0> e[N];              // offset (LTLA population)
  
  matrix[N, M] X;                 // basis functions matrix
  matrix[M-1, 2*(M-1)] S1;        // Penalty matrices
  vector[M] zero;


}
transformed data {
  //matrix[N,T] log_E = log(E + E_neighbours); #kept as formatting reminder
}
parameters {
  vector[M] b;       // basis covariates
  vector<lower = 0, upper = 5>[2] lambda;       //penalty parameters
  real sig_re;        // hierarchical sd parameter for the iid noise (v)
  vector[N] v;        // iid noise
  
}
transformed parameters {
matrix[M-1, M-1] K1 = (S1[1:(M-1), 1:(M-1)] * lambda[1]) + (S1[1:(M-1), M:(2*(M-1))] * lambda[2]);
vector[N] u = X[1:N, 2:M] * b[2:M];
}
model {
for(i in 1:N){
  y[i] ~ poisson_log(b[1] + u[i] + v[i] + log(e[i]));  // 
  v[i] ~ normal(0, sig_re);
}
  
  //PRIORS
  sig_re ~ exponential(0.1);
  // Intercept
  b[1] ~ normal(0, 5);
  
  // Prior for smooth coefficients
  b[2:M] ~ multi_normal_prec(zero[2:M], K1);

  // smoothing parameter priors 
  for (i in 1:2) {
    // truncate lambdas to avoid simulations getting stuck
    lambda[i] ~ gamma(.05, .005);
  }
  
  
  
}
generated quantities {
  //matrix[N,T] eta = log(E[,i] + zetas .*(Cases_Nbors[,i]./N_Nbors)) + beta0 + x[i] * betas + theta[,i]
  vector[N] mu;
  
  for(i in 1:N){
  mu[i] = exp(b[1] + u[i] + v[i] + log(e[i]));
  }
}
"
# Convert data into data suitable for stan
modelData <- list(N = length(Reduced_Data$Week_Cases), M = ncol(X),
                  y = Reduced_Data$Week_Cases, X = X, zero = jd_smooth$jags.data$zero, 
                   S1 = jd_smooth$jags.data$S1, e = Reduced_Data$Population)

# Set initial values for MCMC
#inits <- list(b = rnorm(ncol(X),sd = 1), lambda = c(3, 3), 
#              sig_re = runif(1), v = rnorm(nrow(Reduced_Data), 1))
#Maybe try starting from the b of the gam fit...
inits <- list(b = colMeans(X), lambda = c(3, 3), 
              sig_re = runif(1), v = rnorm(nrow(Reduced_Data), 1))

stanfit = stan(model_code = Stan_model_string,
               data=modelData,
               algorithm = "NUTS",
               chains = 4,
               #warmup=2500, 
               iter=2000
               #thin = 100,
               #init = inits,
               #control = list(max_treedepth = 10)
               )

#9mins:20secs to run iter = 2000 (others default)

main_summaries <- summary(stanfit, pars = c("b", "lambda", "sig_re", "u", "v", "mu", "lp__"))
write.csv(main_summaries$summary,"outputs/main_summaries.csv", row.names = TRUE)

## Check convergence using Stan's Rhat
#
#The Rhat function produces R-hat convergence diagnostic, 
#which compares the between- and within-chain estimates for model parameters 
#and other univariate quantities of interest. If chains have not mixed well 
#(ie, the between- and within-chain estimates don't agree), R-hat is larger than 1. 
#We recommend running at least four chains by default and only using the sample 
#if R-hat is less than #1.05#. Stan reports R-hat which is the maximum of rank 
#normalized split-R-hat and rank normalized folded-split-R-hat, which works for 
#thick tailed distributions and is sensitive also to differences in scale.

GR_diag_over_1_point_1$Week[i] <- Week_isolated
GR_diag_over_1_point_1$GR_over_point1[i] <-  sum(main_summaries$summary[, "Rhat"] > 1.05)

#Let's export key traceplots at some plots
# Open a pdf file
png(sprintf("outputs/posterior_plots/b2_Week_%s.png",Week_isolated)) 
# 2. Create a plot
traceplot(stanfit, pars= sprintf('b[%s]',15:26))
# Close the png file
dev.off()

png(sprintf("outputs/posterior_plots/b1_Week_%s.png",Week_isolated)) 
# 2. Create a plot
traceplot(stanfit, pars= sprintf('b[%s]',1:14))
# Close the png file
dev.off()
png(sprintf("outputs/posterior_plots/b3_Week_%s.png",Week_isolated)) 
traceplot(stanfit, pars= sprintf('b[%s]',27:40))
dev.off()

png(sprintf("outputs/posterior_plots/lambda_Week_%s.png",Week_isolated)) 
traceplot(stanfit, pars= sprintf('lambda[%s]',1:2))
dev.off()

png(sprintf("outputs/posterior_plots/sig_re_%s.png",Week_isolated)) 
traceplot(stanfit, pars= c("sig_re")) 
dev.off() 

png(sprintf("outputs/posterior_plots/u1_Week_%s.png",Week_isolated)) 
traceplot(stanfit, pars= sprintf('u[%s]',1:14))
dev.off() 
png(sprintf("outputs/posterior_plots/u2_Week_%s.png",Week_isolated)) 
traceplot(stanfit, pars= sprintf('u[%s]',15:26))
dev.off() 

png(sprintf("outputs/posterior_plots/v1_Week_%s.png",Week_isolated)) 
traceplot(stanfit, pars= sprintf('v[%s]',1:14))
dev.off() 
png(sprintf("outputs/posterior_plots/v2_Week_%s.png",Week_isolated)) 
traceplot(stanfit, pars= sprintf('v[%s]',15:26))
dev.off() 


# Save the model object
write_rds(stanfit, 
          file = sprintf("outputs/model_fits/Week_%s_stanfit.rds", Week_isolated))


#### Extract predicted intercept from models ####
## Spatial smooth model

## Extract simulations of the intercept
b_sim <- do.call(rbind, results$samples)[ , "b[1]"]
b_sim <- summary(stanfit, pars = c('b[1]'))
b_sim <- b_sim$summary
# Return mean and 95% credible interval & format 
b_est <- data.table(b_est = b_sim[,"mean"], 
                    b_lq = b_sim[,"2.5%"],
                    b_uq = b_sim[,"97.5%"],
                    b_format = paste0(round(b_sim[,"mean"], 3), " (",
                                      round(b_sim[,"2.5%"], 3), 
                                      ", ", round(b_sim[,"97.5%"], 3), ")"))
Intercept_estimate$Week[i] <- Week_isolated
Intercept_estimate$intercept_b1[i] <- b_est$b_format

#### Extract estimates for phi/mixing parameters ####
n <- nrow(Reduced_Data)

## Spatial smooth model
matrix_of_draws <- as.matrix(stanfit)
# Return column numbers with structured random effect simulations
umin <- which(colnames(matrix_of_draws) == "u[1]")
umax <- which(colnames(matrix_of_draws) == paste0("u[", n, "]"))

# Return column numbers with unstructured random effect simulations
vmin <- which(colnames(matrix_of_draws) == "v[1]")
vmax <- which(colnames(matrix_of_draws) == paste0("v[", n, "]"))


# Extract simulations of structured random effect
# A 28,500 x 356 matrix
u_mat <- matrix_of_draws[, umin:umax]
# Estimate the variance of each simulation (OVER ROWS)
u_var <- apply(u_mat, 1, var)

# Extract simulations of unstructured random effect
v_mat <- matrix_of_draws[,vmin:vmax]
# Estimate the variance of each simulation
v_var <- apply(v_mat, 1, var)

# Calculate the proportion of variance explained by the structured term
propn_spat <- u_var/(u_var + v_var)

re_vars <- data.table(spat_var = u_var, 
                      iid_var = v_var,
                      propn_var = propn_spat)

## Combine the estimated phi from each model and calculate mean and 95% CI
smooth_phi_est <- re_vars %>% 
  summarise(phi_est = mean(propn_var),
            phi_lq = quantile(propn_var, .025),
            phi_uq = quantile(propn_var, .975))

#Cool, so estimated that ~ 42.5% of the "noise" is spatial!
Mixing_estimate$Week[i] <- Week_isolated
Mixing_estimate$phi_est[i] <- paste0(round(mean(smooth_phi_est$phi_est), 3), " (",
                                     round(smooth_phi_est$phi_lq, 3), 
                                     ", ", round(smooth_phi_est$phi_uq, 3), ")")


#### Calculate model diagnostic statistics
diag_plot <- stan_diag(stanfit)
ggsave(diag_plot, 
       filename = sprintf("outputs/plots/diagnostic_plots/Week_%s_stan_diag.png", Week_isolated))

rhat_plot <- stan_rhat(stanfit)
ggsave(rhat_plot, 
       filename = sprintf("outputs/plots/diagnostic_plots/Week_%s_stan_rhat.png", Week_isolated))

mcse_plot <- stan_mcse(stanfit)
ggsave(mcse_plot, 
       filename = sprintf("outputs/plots/diagnostic_plots/Week_%s_stan_mcse.png", Week_isolated))




## Mean absolute error
## Smooth model
# Extract predicted values from smooth model

# Return column numbers with lambda simulations
lammin <- which(colnames(matrix_of_draws) == "mu[1]")
lammax <- which(colnames(matrix_of_draws) == paste0("mu[", n, "]"))

## # Extract simulations of lambda
lam_mat <- matrix_of_draws[ , lammin:lammax]
# Estimate mean from each simulation
lam_mean <- apply(lam_mat, 2, mean)

mu_est <- lam_mean

mae_smooth <- mean(abs(Reduced_Data$Week_Cases - mu_est))
MAE_estimate$Week[i] <- Week_isolated
MAE_estimate$mae[i] <- mae_smooth
#That's.... slightly outrageously good.
#plot(log(Reduced_Data$Week_Cases), log(mu_est))

#Let's plot the spatial bit:

# Estimate mean from each simulation
u_mean <- apply(u_mat, 2, mean)
v_mean <- apply(v_mat, 2, mean)
exp_u_mean <- exp(u_mean)
exp_v_mean <- exp(v_mean)
plot_data <- Reduced_Data[,c(1,4,8)]
plot_data$u_mean <- exp_u_mean
plot_data$v_mean <- exp_v_mean
colnames(plot_data) <- c("CODE", "Population", "Week_Cases", "u_mean", "v_mean")

Boundaries %>% 
  inner_join(plot_data, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = u_mean), lwd =  .05) +
  scale_fill_viridis_c(name = "u_mean") +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> uplot

ggsave(uplot, 
       filename = sprintf("outputs/fitted_spatial_components/Week_%s_distance_based_u.png", Week_isolated))

Boundaries %>% 
  inner_join(plot_data, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = v_mean), lwd =  .05) +
  scale_fill_viridis_c(name = "v_mean") +
  theme_void() +
  theme(plot.background = element_rect(fill = 'white', color = "white"))-> vplot

ggsave(vplot, 
       filename = sprintf("outputs/fitted_spatial_components/Week_%s_iid_random_v.png", Week_isolated))

plot_grid(Cases_by_pop_plot + theme(legend.title= element_blank()),
          Gam_k40_plot + theme(legend.title= element_blank()), uplot, vplot,
          nrow = 2, labels = c(sprintf("Real cases/pop - Week %s", Week_isolated), "GAM k40 fit",
                               sprintf("Bayesian u - %s percent", Mixing_estimate$phi_est[i]),
                               sprintf("Bayesian v - iid noise - %s bad GRs", GR_diag_over_1_point_1$GR_over_point1[i])),
          scale = 0.9) -> combined_plot


ggsave(combined_plot, 
       filename = sprintf("outputs/plots/Combined/Week_%s.png", Week_isolated),
       width = 10.82, height = 9.52, units = c("in"))

}

#Finally, save all the generated data_files
write.csv(sp_k_10_smoothing_parameters, "outputs/sp_k_10_smoothing_parameters.csv", row.names=FALSE)
saveRDS(sp_k_10_smoothing_parameters, file = "outputs/sp_k_10_smoothing_parameters.rds")
write.csv(sp_k_20_smoothing_parameters, "outputs/sp_k_20_smoothing_parameters.csv", row.names=FALSE)
saveRDS(sp_k_20_smoothing_parameters, file = "outputs/sp_k_20_smoothing_parameters.rds")
write.csv(sp_k_40_smoothing_parameters, "outputs/sp_k_40_smoothing_parameters.csv", row.names=FALSE)
saveRDS(sp_k_40_smoothing_parameters, file = "outputs/sp_k_40_smoothing_parameters.rds")

write.csv(GR_diag_over_1_point_1, "outputs/Rhat_diag_over_1_point_05.csv", row.names=FALSE)
saveRDS(GR_diag_over_1_point_1, file = "outputs/Rhat_diag_over_1_point_05.rds")
write.csv(Intercept_estimate, "outputs/Intercept_estimate.csv", row.names=FALSE)
saveRDS(Intercept_estimate, file = "outputs/Intercept_estimate.rds")
write.csv(Mixing_estimate, "outputs/Mixing_estimate.csv", row.names=FALSE)
saveRDS(Mixing_estimate, file = "outputs/Mixing_estimate.rds")
write.csv(MAE_estimate, "outputs/MAE_estimate.csv", row.names=FALSE)
saveRDS(MAE_estimate, file = "outputs/MAE_estimate.rds")

dir.create("outputs/estimates_plots_over_all_weeks")

ggplot(data = GR_diag_over_1_point_1) +
  geom_point(aes(x= Week, y = GR_over_point1)) -> GR_plot
ggsave(GR_plot, 
       filename = "outputs/estimates_plots_over_all_weeks/Gelman_Rubin.png")
ggplot(data = Intercept_estimate) +
  geom_point(aes(x= Week, y = intercept_b1)) -> intercept_plot
ggsave(intercept_plot, 
       filename = "outputs/estimates_plots_over_all_weeks/b1_intercept.png")
ggplot(data = Mixing_estimate) +
  geom_point(aes(x= Week, y = phi_est)) -> percent_distance_plot
ggsave(percent_distance_plot, 
       filename = "outputs/estimates_plots_over_all_weeks/Percent_distance_based_spatial.png")
ggplot(data = MAE_estimate) +
  geom_point(aes(x= Week, y = mae)) -> MAE_plot
ggsave(MAE_plot, 
       filename = "outputs/estimates_plots_over_all_weeks/MAE_estimate.png")

ggplot(data = sp_k_10_smoothing_parameters) +
  geom_point(aes(x= Week, y = sp)) -> sp_k_10_plot
ggsave(sp_k_10_plot, 
       filename = "outputs/estimates_plots_over_all_weeks/sp_k_10_plot.png")
ggplot(data = sp_k_20_smoothing_parameters) +
  geom_point(aes(x= Week, y = sp)) -> sp_k_20_plot
ggsave(sp_k_20_plot, 
       filename = "outputs/estimates_plots_over_all_weeks/sp_k_20_plot.png")
ggplot(data = sp_k_40_smoothing_parameters) +
  geom_point(aes(x= Week, y = sp)) -> sp_k_40_plot
ggsave(sp_k_40_plot, 
       filename = "outputs/estimates_plots_over_all_weeks/sp_k_40_plot.png")

