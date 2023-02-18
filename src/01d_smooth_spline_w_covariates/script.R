## Script to employ the method's presented in Sophie Lee's paper;
## "A Bayesian modelling framework to quantify multiple sources of spatial variation for disease mapping"
## https://doi.org/10.1098/rsif.2022.0440

##########################################################################

#Load the Cases data
load("Cases_Data.RData")

#Load the Boundaries data
load("Boundaries_Data.RData")
#Load the adjacency matrix
load("W.RData")

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
which(W[97,] == 1)
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
#Goes from 2 - 129
Weeks_to_assess <- unique(Case_Rates_Data$Week)
#Weeks_to_assess <- c(2, 85,86,87)

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
WAIC <- data.frame(Week = rep(NA,length(Weeks_to_assess)), 
                   WAIC = rep(NA,length(Weeks_to_assess)))
MAE_estimate <- data.frame(Week = rep(NA,length(Weeks_to_assess)), 
                   mae = rep(NA,length(Weeks_to_assess)))

#The beta covariate estimates
beta_estimates <- data.table(beta_est = rep(NA,26*length(Weeks_to_assess)), 
                             beta_lq = rep(NA,26*length(Weeks_to_assess)),
                             beta_uq = rep(NA,26*length(Weeks_to_assess)),
                             beta_format = rep(NA,26*length(Weeks_to_assess)),
                             Week = rep(Weeks_to_assess, each = 26),
                             param = rep(1:26, length(Weeks_to_assess))
)

dir.create("outputs")
dir.create("outputs/plots")
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
  theme_void() +
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

# Set constants for model (number obs & number of coefficients)
Consts <- list(n = length(Reduced_Data$Week_Cases), m = ncol(X))

### Write model formula
Model <- nimbleCode({ 
  # u = spatial smooth term (using basis funtions)
  u[1:n] <- X[1:n, 2:m] %*% b[2:m] #b is the coefficients, beta
  covariate_addition[1:n] <- betas_covariates[1:26] %*% covar_x[1:26,1:n]
  
  for (I in 1:n) { 
    # y = number of cases
    y[I] ~ dpois(mu[I]) 
    
    log(mu[I]) <- b[1] + u[I] + v[I] + covariate_addition[I] + log(e[I])
    #betas_covar = 1x26
    #covar_x = 26x306
    # v = iid random effect
    v[I] ~ dnorm(0, sd = sig_re)
  } 
  
  #for(i in 1:306){
  #  covariate_addition[i] <- inprod(betas_covariates[1:26]%*%covar_x[,i])
  #}
  
  # Priors
  # Random effect SD
  sig_re ~ dexp(.1)
  
  # Intercept
  b[1] ~ dnorm(0, sd = 5) 
  for(i in 1:26){
    betas_covariates[i] ~ dnorm(0, sd = 5) 
  }
  ## prior for sd(smooth function)
  K1[1:(m-1), 1:(m-1)] <- S1[1:(m-1), 1:(m-1)] * lambda[1] + 
    S1[1:(m-1), m:(2*(m-1))] * lambda[2]
  
  # Prior for smooth coefficients
  b[2:m] ~ dmnorm(zero[2:m], K1[1:(m-1), 1:(m-1)]) 
  
  ## smoothing parameter priors 
  for (I in 1:2) {
    # truncate lambdas to avoid simulations getting stuck
    lambda[I] ~ T(dgamma(.05, .005), 0, 5)
  }
} )

###############################################
# Here we make the 26x306 covariates matrix:
covar_x <- matrix(0, nrow = 26, ncol = 306)

covar_x[1,] <- Reduced_Data$cumVaccPercentage_FirstDose/100
covar_x[2,] <- Reduced_Data$cumVaccPercentage_SecondDose/100
covar_x[3,] <- Reduced_Data$cumVaccPercentage_ThirdDose/100
covar_x[4,] <- scale(Reduced_Data$prop_white_british)
covar_x[5,] <- scale(Reduced_Data$prop_asian)
covar_x[6,] <- scale(Reduced_Data$prop_black_afr_car)
covar_x[7,] <- scale(Reduced_Data$IMD_Average_score)   
covar_x[8,] <- scale(Reduced_Data$prop_o65) 
covar_x[9,] <- scale(Reduced_Data$Median_annual_income) 
covar_x[10,] <- scale(Reduced_Data$workplaces_percent_change_from_baseline)
covar_x[11,] <- scale(Reduced_Data$residential_percent_change_from_baseline)
covar_x[12,] <- scale(Reduced_Data$transit_stations_percent_change_from_baseline)
covar_x[13,] <- Reduced_Data$Alpha_proportion/100
covar_x[14,] <- Reduced_Data$Delta_proportion/100
covar_x[15,] <- Reduced_Data$Delta_AY_4_2_proportion/100
covar_x[16,] <- Reduced_Data$Omicron_BA_1_proportion/100
covar_x[17,] <- Reduced_Data$Omicron_BA_2_proportion/100
covar_x[18,] <- Reduced_Data$Omicron_BA_4_proportion/100
covar_x[19,] <- Reduced_Data$Omicron_BA_5_proportion/100
covar_x[20,] <- Reduced_Data$Other_proportion/100
covar_x[21,] <- scale(Reduced_Data$Core_services_funding_by_weighted)
covar_x[22,] <- scale(Reduced_Data$Primary_care_funding_by_weighted)
covar_x[23,] <- scale(Reduced_Data$Specialised_services_by_weighted)
covar_x[24,] <- scale(Reduced_Data$unringfenced/Reduced_Data$Population)
covar_x[25,] <- scale(Reduced_Data$contain_outbreak_management/Reduced_Data$Population)
covar_x[26,] <- scale(Reduced_Data$ASC_infection_control_fund/Reduced_Data$Population)

###############################################

# Convert jagam data into data suitable for nimble
nimbleData <- list(y = Reduced_Data$Week_Cases, X = X, zero = jd_smooth$jags.data$zero, 
                   S1 = jd_smooth$jags.data$S1, e = Reduced_Data$Population,
                   covar_x = covar_x)


# Set initial values for MCMC
#inits <- list(b = rnorm(ncol(X),sd = 1), lambda = c(3, 3), 
#              sig_re = runif(1), v = rnorm(nrow(Reduced_Data), 1))
#Maybe try starting from the b of the gam fit...
 inits <- list(b = colMeans(X), lambda = c(3, 3), 
               sig_re = runif(1), v = rnorm(nrow(Reduced_Data), 1),
               betas_covariates = rnorm(26,sd=1))
##Doing this did help a LOT actually


# Sets up model in nimble code
nimbleModel <- nimbleModel(code = Model, name = 'nimbleModel', 
                           constants = Consts, data = nimbleData, 
                           inits = inits)

# Tell model which parameter to estimate and return
MCMCconfig <- configureMCMC(nimbleModel,
                            monitors=c("b","lambda", "u", "v",
                                       "sig_re", "mu", "betas_covariates"),
                            # Return WAIC to compare models
                            enableWAIC = TRUE)


# Build the model
modelMCMC <- buildMCMC(MCMCconfig)

compiled_model <- compileNimble(nimbleModel)

compiled_model_MCMC <- compileNimble(modelMCMC, project = nimbleModel)

results <- runMCMC(compiled_model_MCMC, thin = 100, 
                   niter = 5000000, nburnin = 500000, 
                   nchains = 3, inits=inits, progressBar = T, 
                   samplesAsCodaMCMC = T, WAIC = TRUE)

#1h:21mins to run thin = 100, 
#niter = 1000000, nburnin = 100000,


## Check convergence using Gelman-Rubin diagnostics with coda 
GR.diag <- gelman.diag(results$samples, multivariate = F)

GR_diag_over_1_point_1$Week[i] <- Week_isolated
GR_diag_over_1_point_1$GR_over_point1[i] <-  sum(GR.diag$psrf[, "Point est."] > 1.1)

# Our u definitely did NOT converge! or b, or lambda. Anything but mu basically.
#This was improved significantly by doing 10* the samples. Only 50 over 1.1 now, compared to 750
#Let's look at some plots
# Open a pdf file
png(sprintf("outputs/posterior_plots/b_lambda_Week_%s.png",Week_isolated)) 
# 2. Create a plot
plot(results$samples[ , c("b[2]","lambda[2]")]) 
# Close the png file
dev.off() 

dir.create("outputs/posterior_plots/beta_covariates")

for(j in c(1,3,5,7,9,11,13,15,17,19,21,23,25)){
png(sprintf("outputs/posterior_plots/beta_covariates/beta_covariates%s_Week_%s.png",j, Week_isolated)) 
# 2. Create a plot
plot(results$samples[ , c(sprintf("betas_covariates[%s]",i),sprintf("betas_covariates[%s]",(j+1)))]) 
# Close the png file
dev.off() 
}

png(sprintf("outputs/posterior_plots/sig_re_lambda_Week_%s.png",Week_isolated)) 
plot(results$samples[ , c("sig_re","lambda[1]")]) 
dev.off() 

png(sprintf("outputs/posterior_plots/u_v_Week_%s.png",Week_isolated)) 
plot(results$samples[ , c("u[2]","v[2]")]) 
dev.off() 


# Save the model object
write_rds(results, 
          file = sprintf("outputs/model_fits/Week_%s_results.rds", Week_isolated))


#### Extract predicted intercept from models ####
## Spatial smooth model

## Extract simulations of the intercept
b_sim <- do.call(rbind, results$samples)[ , "b[1]"]

# Return mean and 95% credible interval & format 
b_est <- data.table(b_est = mean(b_sim), 
                    b_lq = quantile(b_sim, .025),
                    b_uq = quantile(b_sim, .975),
                    b_format = paste0(round(mean(b_sim), 3), " (",
                                      round(quantile(b_sim, .025), 3), 
                                      ", ", round(quantile(b_sim, .975), 3), ")"))
Intercept_estimate$Week[i] <- Week_isolated
Intercept_estimate$intercept_b1[i] <- b_est$b_format

#### Extract predicted beta covariates from models ####

## Extract simulations of the intercept

for(J in 1:26){
beta_sim <- do.call(rbind, results$samples)[ , sprintf("betas_covariates[%s]",J)]
j <- ((i-1)*26)+ J
# Return mean and 95% credible interval & format 
beta_estimates$beta_est[j] = mean(beta_sim)
beta_estimates$beta_lq[j] = quantile(beta_sim, .025)
beta_estimates$beta_uq[j] = quantile(beta_sim, .975)
beta_estimates$beta_format[j] = paste0(round(mean(beta_sim), 3), " (",
                                      round(quantile(beta_sim, .025), 3), 
                                      ", ", round(quantile(beta_sim, .975), 3), ")")

}
#### Extract estimates for phi/mixing parameters ####
n <- nrow(Reduced_Data)

## Spatial smooth model
# Return column numbers with structured random effect simulations
umin <- which(colnames(results$samples[[1]]) == "u[1]")
umax <- which(colnames(results$samples[[1]]) == paste0("u[", n, "]"))

# Return column numbers with unstructured random effect simulations
vmin <- which(colnames(results$samples[[1]]) == "v[1]")
vmax <- which(colnames(results$samples[[1]]) == paste0("v[", n, "]"))


# Extract simulations of structured random effect
# A 28,500 x 356 matrix
u_mat <- do.call(rbind, results$samples)[, umin:umax]
# Estimate the variance of each simulation (OVER ROWS)
u_var <- apply(u_mat, 1, var)

# Extract simulations of unstructured random effect
v_mat <- do.call(rbind, results$samples)[,vmin:vmax]
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


#### Calculate model comparison statistics ####
## WAIC
## Smooth model
WAIC$Week[i] <- Week_isolated
WAIC$WAIC[i] <- results$WAIC$WAIC



## Mean absolute error
## Smooth model
# Extract predicted values from smooth model

# Return column numbers with lambda simulations
lammin <- which(colnames(results$samples[[1]]) == "mu[1]")
lammax <- which(colnames(results$samples[[1]]) == paste0("mu[", n, "]"))

## # Extract simulations of lambda
lam_mat <- do.call(rbind, results$samples)[ , lammin:lammax]
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
  theme_void() +
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> uplot

ggsave(uplot, 
       filename = sprintf("outputs/fitted_spatial_components/Week_%s_distance_based_u.png", Week_isolated))

Boundaries %>% 
  inner_join(plot_data, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = v_mean), lwd =  .05) +
  scale_fill_viridis_c(name = "v_mean") +
  theme_void() +
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> vplot

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

write.csv(GR_diag_over_1_point_1, "outputs/GR_diag_over_1_point_1.csv", row.names=FALSE)
saveRDS(GR_diag_over_1_point_1, file = "outputs/GR_diag_over_1_point_1.rds")
write.csv(Intercept_estimate, "outputs/Intercept_estimate.csv", row.names=FALSE)
saveRDS(Intercept_estimate, file = "outputs/Intercept_estimate.rds")
write.csv(Mixing_estimate, "outputs/Mixing_estimate.csv", row.names=FALSE)
saveRDS(Mixing_estimate, file = "outputs/Mixing_estimate.rds")
write.csv(WAIC, "outputs/WAIC.csv", row.names=FALSE)
saveRDS(WAIC, file = "outputs/WAIC.rds")
write.csv(MAE_estimate, "outputs/MAE_estimate.csv", row.names=FALSE)
saveRDS(MAE_estimate, file = "outputs/MAE_estimate.rds")

write.csv(beta_estimates, "outputs/covar_beta_estimates.csv", row.names=FALSE)
saveRDS(beta_estimates, file = "outputs/covar_beta_estimates.rds")


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
ggplot(data = WAIC) +
  geom_point(aes(x= Week, y = WAIC)) -> WAIC_plot
ggsave(WAIC_plot, 
       filename = "outputs/estimates_plots_over_all_weeks/WAIC_plot.png")
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

titles <- c("cumVaccPercentage_FirstDose",
            "cumVaccPercentage_SecondDose",
            "cumVaccPercentage_ThirdDose",
            "prop_white_british",
            "prop_asian",
            "prop_black_afr_car",
            "IMD_Average_score",
            "prop_o65",
            "Median_annual_income",
            "workplaces_percent_change_from_baseline",
            "residential_percent_change_from_baseline",
            "transit_stations_percent_change_from_baseline",
            "Alpha_proportion",
            "Delta_proportion",
            "Delta_AY_4_2_proportion",
            "Omicron_BA_1_proportion",
            "Omicron_BA_2_proportion",
            "Omicron_BA_4_proportion",
            "Omicron_BA_5_proportion",
            "Other_proportion",
            "Core_services_funding_by_weighted",
            "Primary_care_funding_by_weighted",
            "Specialised_services_by_weighted",
            "unringfenced_by_Population",
            "contain_outbreak_management_by_Population",
            "ASC_infection_control_fund_by_Population"
)
dir.create("outputs/estimates_plots_over_all_weeks/beta_covariates")
for(i in 1:26){
  reduced_b_est <- filter(beta_estimates, param == i)
  ggplot(data = reduced_b_est) +
    geom_point(aes(x = Week, y = beta_est)) +
    geom_errorbar(aes(x = Week, ymin=beta_lq,ymax=beta_uq)) +
    geom_hline(yintercept = 0, color = "red", lty = "dashed") +
    ylab(sprintf("beta_%s",i)) +
    ggtitle(titles[i]) -> plot
  ggsave(plot, 
         filename = sprintf("outputs/estimates_plots_over_all_weeks/beta_covariates/beta_%s_plot.png",i))
  
}
