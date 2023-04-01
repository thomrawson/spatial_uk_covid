## Script to employ the method's presented in Sophie Lee's paper;
## "A Bayesian modelling framework to quantify multiple sources of spatial variation for disease mapping"
## https://doi.org/10.1098/rsif.2022.0440

#The key difference between this and 01c, is that we fit all weeks simultaneously,
#and include the covariates of interest as model parameters.

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

#First, we need to do all the steps to reduce down to the covariates we want:
###############################################################################
#Cut down to the only covariates we're interested in:
Case_Rates_Data <- Case_Rates_Data[,-c(5,6,15,16,18,19,22,24,25,28,30,31,32,33,
                                       43, #Cut Omicron BQ1
                                       46,49,50,51,60,61,62:76)] 


#Weeks go from 2:130
#Note, the government stopped providing free LFTs on April 1st 2022
#This will have, in turn, affected the case data, so let's chop off everything
#after week 104 (w/b 25/04/22)
#TODO: Could include still but keep this as a variable
final_week <- 104
Case_Rates_Data <- filter(Case_Rates_Data, Week < final_week+1)

#Let's investigate NAs. First of all, we sadly have incomplete mobility/vaccine
#data in devolved nations, so we trim down to just England:
#sum(is.na(Case_Rates_Data$prop_white_british))/36668
#8% NA
#sum(is.na(Case_Rates_Data$IMD_Average_score))/36668
#14% NA
#sum(is.na(Case_Rates_Data$mean_age))/36668
#8% NA
#sum(is.na(Case_Rates_Data$residential_percent_change_from_baseline))/36668
#0.3% NA
#sum(is.na(Case_Rates_Data$Alpha_proportion))/36668
#14% NA
test <- Case_Rates_Data[which(is.na(Case_Rates_Data$Alpha_proportion)),]
unique(test$areaCode)

#Missing Scotland and Wales in the variant proportions, so start by filtering out those:
Case_Rates_Data <- Case_Rates_Data[which(!is.na(Case_Rates_Data$Alpha_proportion)),]

#After this:
#sum(is.na(Case_Rates_Data$prop_white_british))/36668
#0% NA
#sum(is.na(Case_Rates_Data$IMD_Average_score))/36668
#0% NA
#sum(is.na(Case_Rates_Data$mean_age))/36668
#0% NA
#sum(is.na(Case_Rates_Data$residential_percent_change_from_baseline))/36668
#0.06% NA
#sum(is.na(Case_Rates_Data$Alpha_proportion))/36668
#0% NA

missing_data <- Case_Rates_Data[which(is.na(Case_Rates_Data$residential_percent_change_from_baseline)),]
unique(missing_data$areaCode)


#We need to quickly clean up an issue with the Residential & transit mobility
#TODO: Move this to the 00 task.
#There are 24 occasions where the residential mobility is NA,
#19 of these are Rutland

#For Rutland, we're going to take the average of it's neighbors mobility score for these days:
#Rutland is index 97
#Has four neighboring regions, 117, 189, 190, 195
Rutland_neighbors <- c(117, 189, 190, 195)
for(i in 1:length(missing_data$areaCode)){
  if(missing_data$areaName[i] == "Rutland"){
    Week_hold <- missing_data$Week[i]
    Neighbor_hold <- filter(Case_Rates_Data, Week == Week_hold)
    Neighbor_hold <- filter(Neighbor_hold, INDEX %in% Rutland_neighbors)
    residential_mean_hold <- mean(Neighbor_hold$residential_percent_change_from_baseline)
    
    Case_Rates_Data$residential_percent_change_from_baseline[which((Case_Rates_Data$areaName == "Rutland")&(Case_Rates_Data$Week == Week_hold))] <- residential_mean_hold
    
  }
}


#For these remaining 5, all week 2 and 3, I'm going to use the mobility scores for
#week 4
#These dates are within the first lockdown so I don't envision much difference
for( i in 1:length(missing_data$areaCode)){
  Week_hold <- missing_data$Week[i]
  areaCode_hold <- missing_data$areaCode[i]
  
  Neighbor_hold <- filter(Case_Rates_Data, areaCode == areaCode_hold)
  Neighbor_hold <- filter(Neighbor_hold, Week == 4)
  residential_mean_hold <- Neighbor_hold$residential_percent_change_from_baseline[1]
  
  Case_Rates_Data$residential_percent_change_from_baseline[which((Case_Rates_Data$areaCode == areaCode_hold)&(Case_Rates_Data$Week == Week_hold))] <- residential_mean_hold
  
}

missing_data <- Case_Rates_Data[which(is.na(Case_Rates_Data$residential_percent_change_from_baseline)),]
#All NAs are now removed!
rm(test, missing_data, Neighbor_hold, Week_hold, areaCode_hold, residential_mean_hold)

#Rutland is missing basically all of it's transit mobility data, so we'll take the average of it's neighbors for all of these

missing_data <- Case_Rates_Data[which(is.na(Case_Rates_Data$transit_stations_percent_change_from_baseline)),]
for(i in 1:length(missing_data$areaCode)){
  Week_hold <- missing_data$Week[i]
  Neighbor_hold <- filter(Case_Rates_Data, Week == Week_hold)
  Neighbor_hold <- filter(Neighbor_hold, INDEX %in% Rutland_neighbors)
  transit_mean_hold <- mean(Neighbor_hold$transit_stations_percent_change_from_baseline)
  
  Case_Rates_Data$transit_stations_percent_change_from_baseline[which((Case_Rates_Data$areaName == "Rutland")&(Case_Rates_Data$Week == Week_hold))] <- transit_mean_hold
  
}
any(is.na(Case_Rates_Data))
#No more missing data!
################################################################################


#First, we are going to look at the spatial variation for just one Week. 
#Let's say Week 50; (very low cases) Week 85 for very high.
#We will loop over all weeks
#Goes from 2 - 104
Weeks_to_assess <- unique(Case_Rates_Data$Week)
#Weeks_to_assess <- c(2,11,12)

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
                                     intercept_b1 = rep(NA,length(Weeks_to_assess)),
                                 b1_mean = rep(NA,length(Weeks_to_assess)),
                                 b1_lq = rep(NA,length(Weeks_to_assess)),
                                 b1_uq = rep(NA,length(Weeks_to_assess)))
Mixing_estimate <- data.frame(Week = rep(NA,length(Weeks_to_assess)), 
                                 phi_est = rep(NA,length(Weeks_to_assess)),
                              phi_mean = rep(NA,length(Weeks_to_assess)),
                              phi_lq = rep(NA,length(Weeks_to_assess)),
                              phi_uq = rep(NA,length(Weeks_to_assess)))
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

#Make an array to hold our basis function covariates
X_basis <- array(0, dim = c(length(Weeks_to_assess),306, 40))

S1_data <- array(0, dim = c(length(Weeks_to_assess), 39,78))

for(i in 1:length(Weeks_to_assess)){

  Week_isolated <- Weeks_to_assess[i]  

Reduced_Data <- filter(Case_Rates_Data, Week == Week_isolated)


#############################
#NOW FOR ACTUAL MODELLING
###########################

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
X_hold <- jd_smooth$jags.data$X
X_basis[i,,] <- X_hold

S1_hold <- jd_smooth$jags.data$S1
S1_data[i,,] <- S1_hold

}
#End the Loop here. 

### Write model formula


Stan_model_string = "
data {
  int<lower=0> N; // Number of areas
  int<lower=0> M; // Number of basis functions
  int<lower=0> T; // Number of time points


  int<lower=0> y[N, T];              // count outcomes (current week's cases)
  //int<lower=1> K;                 // num covariates 
  int<lower=0> e[N];              // offset (LTLA population)
  
  matrix[N, M] X[T];                 // basis functions matrix
  matrix[M-1, 2*(M-1)] S1[T];        // Penalty matrices
  vector[M] zero;


}
transformed data {
  //matrix[N,T] log_E = log(E + E_neighbours); #kept as formatting reminder
}
parameters {
  //matrix[M,T] b;       // basis covariates
  vector[M] b[T];       // basis covariates
  vector<lower = 0, upper = 5>[2] lambda[T];       //penalty parameters
  real<lower = 0> sig_re[T];        // hierarchical sd parameter for the iid noise (v)
  vector[N] v[T];        // iid noise
  
}
transformed parameters {
matrix[M-1, M-1] K1[T];
vector[N] u[T];

for(t in 1:T){
K1[t,,] = (S1[t,1:(M-1), 1:(M-1)] * lambda[t,1]) + (S1[t,1:(M-1), M:(2*(M-1))] * lambda[t,2]);
u[t] = X[t,1:N, 2:M] * b[t,2:M];
}
}
model {
for(t in 1:T){
for(i in 1:N){
  y[i,t] ~ poisson_log(b[t,1] + u[t][i] + v[t][i] + log(e[i]));  // 
  v[t][i] ~ normal(0, sig_re[t]);
}
}
  
  //PRIORS
  for(t in 1:T){
  sig_re[t] ~ exponential(0.1);
  // Intercept
  b[t,1] ~ normal(0, 5);
  
  // Prior for smooth coefficients
  b[t,2:M] ~ multi_normal_prec(zero[2:M], K1[t]);

  // smoothing parameter priors 
  for (i in 1:2) {
    // truncate lambdas to avoid simulations getting stuck
    lambda[t][i] ~ gamma(.05, .005);
  }
  
  }
  
  
}
generated quantities {
  //matrix[N,T] eta = log(E[,i] + zetas .*(Cases_Nbors[,i]./N_Nbors)) + beta0 + x[i] * betas + theta[,i]
  matrix[T,N] mu;
  
  for(t in 1:T){
  for(i in 1:N){
  mu[t,i] = exp(b[t,1] + u[t][i] + v[t][i] + log(e[i]));
  }
  }
}
"
# Convert data into data suitable for stan
#y[N, T]
y_data <- array(0, dim = c(length(unique(Case_Rates_Data$areaCode)), length(Weeks_to_assess)))
for(i in 1:length(Weeks_to_assess)){
  Reduced_Data <- filter(Case_Rates_Data, Week == Weeks_to_assess[i])
  y_data[,i] <- Reduced_Data$Week_Cases
}



modelData <- list(N = length(unique(Case_Rates_Data$areaCode)), 
                  M = ncol(X_basis[1,,]),
                  T = length(Weeks_to_assess),
                  y = y_data, X = X_basis, zero = jd_smooth$jags.data$zero, 
                   S1 = S1_data, e = Reduced_Data$Population)


stanfit = rstan::stan(model_code = Stan_model_string,
               data=modelData,
               algorithm = "NUTS",
               chains = 4,
               #warmup=2500, 
               iter=50
               #iter = 2000
               #thin = 100,
               #control = list(max_treedepth = 10)
               )

#############################

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

#To extract the weekly GRs, we need to reduce the summaries down to just the week in question.
for(t in 1:length(Weeks_to_assess)){
  data_hold <- main_summaries$summary
  
  Week_isolated <- Weeks_to_assess[t]
  strings_to_keep <- c(sprintf("b\\[%s,", t),
                       sprintf("lambda\\[%s,", t),
                       sprintf("sig_re\\[%s\\]", t),
                       sprintf("u\\[%s,", t),
                       sprintf("v\\[%s,", t),
                       sprintf("mu\\[%s,", t)
                       )

  data_hold2 <- data_hold[grep(paste(strings_to_keep,collapse="|"), rownames(data_hold)),]
  
  GR_diag_over_1_point_1$Week[t] <- Week_isolated
  GR_diag_over_1_point_1$GR_over_point1[t] <-  sum(data_hold2[, "Rhat"] > 1.05)
  
  
}



#Let's export key traceplots at some plots
for(t in 1:length(Weeks_to_assess)){
  Week_isolated <- Weeks_to_assess[t]
b2_plot <- rstan::traceplot(stanfit, pars= sprintf('b[%s,%s]',t,15:26))
ggsave(b2_plot,
       filename = sprintf("outputs/posterior_plots/b2_Week_%s.png",Week_isolated))

b1_plot <- rstan::traceplot(stanfit, pars= sprintf('b[%s,%s]',t,1:14))
ggsave(b1_plot,
       filename = sprintf("outputs/posterior_plots/b1_Week_%s.png",Week_isolated))

b3_plot <- rstan::traceplot(stanfit, pars= sprintf('b[%s,%s]',t,27:40))
ggsave(b3_plot,
       filename = sprintf("outputs/posterior_plots/b3_Week_%s.png",Week_isolated))

lambda_plot <- rstan::traceplot(stanfit, pars= sprintf('lambda[%s,%s]',t,1:2))
ggsave(lambda_plot,
       filename = sprintf("outputs/posterior_plots/lambda_Week_%s.png",Week_isolated))

sig_plot <- rstan::traceplot(stanfit, pars= sprintf("sig_re[%s]",t))
ggsave(sig_plot,
       filename = sprintf("outputs/posterior_plots/sig_re_%s.png",Week_isolated))

u1_plot <- rstan::traceplot(stanfit, pars= sprintf('u[%s,%s]',t,1:14))
ggsave(u1_plot,
       filename = sprintf("outputs/posterior_plots/u1_Week_%s.png",Week_isolated))

u2_plot <- rstan::traceplot(stanfit, pars= sprintf('u[%s,%s]',t,15:26))
ggsave(u2_plot,
       filename = sprintf("outputs/posterior_plots/u2_Week_%s.png",Week_isolated))

u3_plot <- rstan::traceplot(stanfit, pars= sprintf('u[%s,%s]',t,27:40))
ggsave(u3_plot,
       filename = sprintf("outputs/posterior_plots/u3_Week_%s.png",Week_isolated))



v1_plot <- rstan::traceplot(stanfit, pars= sprintf('v[%s,%s]',t,1:14))
ggsave(v1_plot,
       filename = sprintf("outputs/posterior_plots/v1_Week_%s.png",Week_isolated))

v2_plot <- rstan::traceplot(stanfit, pars= sprintf('v[%s,%s]',t,15:26))
ggsave(v2_plot,
       filename = sprintf("outputs/posterior_plots/v2_Week_%s.png",Week_isolated))

v3_plot <- rstan::traceplot(stanfit, pars= sprintf('v[%s,%s]',t,27:40))
ggsave(v3_plot,
       filename = sprintf("outputs/posterior_plots/u3_Week_%s.png",Week_isolated))

}

# Save the model object
 write_rds(stanfit,
           file = "outputs/model_fits/stanfit.rds")

#### Extract predicted intercept from models ####
## Spatial smooth model

 for(t in 1:length(Weeks_to_assess)){
## Extract simulations of the intercept
#b_sim <- do.call(rbind, results$samples)[ , "b[1]"]
b_sim <- summary(stanfit, pars = sprintf('b[%s,1]',t))
b_sim <- b_sim$summary
# Return mean and 95% credible interval & format
b_est <- data.table(b_est = b_sim[,"mean"],
                    b_lq = b_sim[,"2.5%"],
                    b_uq = b_sim[,"97.5%"],
                    b_format = paste0(round(b_sim[,"mean"], 3), " (",
                                      round(b_sim[,"2.5%"], 3),
                                      ", ", round(b_sim[,"97.5%"], 3), ")"))
Intercept_estimate$Week[t] <- Week_isolated
Intercept_estimate$intercept_b1[t] <- b_est$b_format
Intercept_estimate$b1_mean[t] <- b_est$b_est
Intercept_estimate$b1_lq[t] <- b_est$b_lq
Intercept_estimate$b1_uq[t] <- b_est$b_uq

}
#### Extract estimates for phi/mixing parameters ####
n <- nrow(Reduced_Data)
 matrix_of_draws <- as.matrix(stanfit)
 
 for(t in 1:length(Weeks_to_assess)){
   Week_isolated <- Weeks_to_assess[t]
   Reduced_Data <- filter(Case_Rates_Data, Week == Week_isolated)
## Spatial smooth model

# Return column numbers with structured random effect simulations
u_cols <- which(colnames(matrix_of_draws) %in% sprintf("u[%s,%s]",t,1:306))

# Return column numbers with unstructured random effect simulations
v_cols <- which(colnames(matrix_of_draws) %in% sprintf("v[%s,%s]",t,1:306))

# Extract simulations of structured random effect
# A 28,500 x 356 matrix
u_mat <- matrix_of_draws[, u_cols]
# Estimate the variance of each simulation (OVER ROWS)
u_var <- apply(u_mat, 1, var)

# Extract simulations of unstructured random effect
v_mat <- matrix_of_draws[,v_cols]
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
Mixing_estimate$Week[t] <- Week_isolated
Mixing_estimate$phi_est[t] <- paste0(round(mean(smooth_phi_est$phi_est), 3), " (",
                                     round(smooth_phi_est$phi_lq, 3),
                                     ", ", round(smooth_phi_est$phi_uq, 3), ")")
Mixing_estimate$phi_mean[t] <- smooth_phi_est$phi_est
Mixing_estimate$phi_lq[t] <- smooth_phi_est$phi_lq
Mixing_estimate$phi_uq[t] <- smooth_phi_est$phi_uq

#Let's plot the spatial bit:

###############################################
#HERE's THE SPLINE PLOTS THAT WERE ORIGINALLY DONE PRE-FIT:

#### Step 1: Fit the spatial smooth model ####
## Fit GAM to obtain sensible priors for lambdas (sp)
#Start with k = 10

sp_check_10 <- gam(Week_Cases ~  s(y_scale, x_scale, k = 10, bs = "tp"), offset = log(Population),
                   family = "poisson", data = Reduced_Data, method = "REML")

#summary(sp_check_10)

#sp_check_10$sp
#~ 0.48, which is quite high, but not weird or anything.
sp_k_10_smoothing_parameters$Week[t] <- Week_isolated
sp_k_10_smoothing_parameters$sp[t] <- sp_check_10$sp

#Let's just plot the cases first:
plot_data <- Reduced_Data[,c(1,4,6)]
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
sp_k_20_smoothing_parameters$Week[t] <- Week_isolated
sp_k_20_smoothing_parameters$sp[t] <- sp_check_20$sp
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
sp_k_40_smoothing_parameters$Week[t] <- Week_isolated
sp_k_40_smoothing_parameters$sp[t] <- sp_check_40$sp


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

################################################

# Estimate mean from each simulation
u_mean <- apply(u_mat, 2, mean)
v_mean <- apply(v_mat, 2, mean)
exp_u_mean <- exp(u_mean)
exp_v_mean <- exp(v_mean)
plot_data <- Reduced_Data[,c(1,4,6)]
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
                               sprintf("Bayesian u - %s percent", Mixing_estimate$phi_est[t]),
                               sprintf("Bayesian v - iid noise - %s bad GRs", GR_diag_over_1_point_1$GR_over_point1[i])),
          scale = 0.9) -> combined_plot


ggsave(combined_plot,
       filename = sprintf("outputs/plots/Combined/Week_%s.png", Week_isolated),
       width = 10.82, height = 9.52, units = c("in"))


}

#### Calculate model diagnostic statistics

png("outputs/plots/diagnostic_plots/stan_diag.png")
stan_diag(stanfit)
dev.off()

rhat_plot <- stan_rhat(stanfit)
ggsave(rhat_plot,
       filename = "outputs/plots/diagnostic_plots/stan_rhat.png")
rm(rhat_plot)

mcse_plot <- stan_mcse(stanfit)
ggsave(mcse_plot,
       filename = "outputs/plots/diagnostic_plots/stan_mcse.png")
rm(mcse_plot)



## Mean absolute error
## Smooth model
# Extract predicted values from smooth model
for(t in 1:length(Weeks_to_assess)){
  Week_isolated <- Weeks_to_assess[t]
  Reduced_Data <- filter(Case_Rates_Data, Week == Week_isolated)
# Return column numbers with lambda simulations
lam_cols <- which(colnames(matrix_of_draws) %in% sprintf("mu[%s,%s]",t,1:306))

## # Extract simulations of lambda
lam_mat <- matrix_of_draws[ , lam_cols]
# Estimate mean from each simulation
lam_mean <- apply(lam_mat, 2, mean)

mu_est <- lam_mean

mae_smooth <- mean(abs(Reduced_Data$Week_Cases - mu_est))
MAE_estimate$Week[t] <- Week_isolated
MAE_estimate$mae[t] <- mae_smooth
#That's.... slightly outrageously good.
#plot(log(Reduced_Data$Week_Cases), log(mu_est))
}

#Remove stanfit to aid memory
rm(stanfit)

############################################



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
       filename = "outputs/estimates_plots_over_all_weeks/Rhat.png")
ggplot(data = Intercept_estimate) +
  geom_point(aes(x= Week, y = b1_mean)) +
  geom_errorbar(aes(x=Week, ymin = b1_lq, ymax = b1_uq)) -> intercept_plot
ggsave(intercept_plot, 
       filename = "outputs/estimates_plots_over_all_weeks/b1_intercept.png")
ggplot(data = Mixing_estimate) +
  geom_point(aes(x= Week, y = phi_mean)) +
  geom_errorbar(aes(x = Week, ymin = phi_lq, ymax = phi_uq)) -> percent_distance_plot
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

