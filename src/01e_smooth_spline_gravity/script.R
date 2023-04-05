## Script to employ the method's presented in Sophie Lee's paper;
## "A Bayesian modelling framework to quantify multiple sources of spatial variation for disease mapping"
## https://doi.org/10.1098/rsif.2022.0440

#This model uses both distance based, and gravity model linkage
#No covariates added in this version, and each time point is fit 
#individually

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

#################################################################################
#Build the distance matrix
#scaled also by population of location
#Pick a random week (50) just to pluck the population/distance info from
Week_50_data <- filter(Case_Rates_Data, Week == 50)
Week_50_data <- Week_50_data[,c(3,4,7,77,78,80,81)]
#Keep this but I'm not currently ordering by INDEX
#Week_50_data <- Week_50_data[order(Week_50_data$INDEX),] 
N <- nrow(Week_50_data)
Distance_matrix <- array(0, dim = c(N,N))
for(i in 1:N){
  for(j in 1:N){
    if(i == j){
      
    }else{
      location_i <- Week_50_data[i,]
      location_j <- Week_50_data[j,]
      euclid_distance <- sqrt((location_i$centroid_x - location_j$centroid_x)^2 + (location_i$centroid_y - location_j$centroid_y)^2)
      Distance_matrix[i,j] <- ((location_i$Population/1000)*(location_j$Population/1000))/(euclid_distance)
    }
  }
}

#Now, this matrix conveys gravitational strength, but we want the matrix to instead
# capture dissimilarity, i.e. low gravity = high dissimilarity, so we invert the values
#test <- 1/Distance_matrix
for(i in 1:N){
  for(j in 1:N){
    if(i == j){
      
    }else{
      location_i <- Week_50_data[i,]
      location_j <- Week_50_data[j,]
      euclid_distance <- sqrt((location_i$centroid_x - location_j$centroid_x)^2 + (location_i$centroid_y - location_j$centroid_y)^2)
      Distance_matrix[i,j] <- 1/(((location_i$Population/1000)*(location_j$Population/1000))/(euclid_distance))
    }
  }
}

msd_scale <- cmdscale(Distance_matrix, eig = T, k = 2)
# Plot to view coordinate system
#plot(msd_scale$points[,1], msd_scale$points[,2])

# Convert into a df with 'connectivity coordinates' per city
connect_coords <-  as.data.frame(msd_scale$points)
names(connect_coords) <- c("connect_coord1", "connect_coord2")
connect_coords$areaName <- Week_50_data$areaName

connect_coords <- connect_coords %>% 
  mutate(connect_coord1_scale = (connect_coord1 - min(connect_coord1))/(max(connect_coord1) - min(connect_coord1)),
         connect_coord2_scale = (connect_coord2 - min(connect_coord2))/(max(connect_coord2) - min(connect_coord2)))


#################################################################################



#First, we are going to look at the spatial variation for just one Week. 
#Let's say Week 50; (very low cases) Week 85 for very high.
#We will loop over all weeks
#Goes from 2 - 129
Weeks_to_assess <- unique(Case_Rates_Data$Week)
#Weeks_to_assess <- c(2,11,12)

#Data files I'll fill up as we go:
sp_k_10_smoothing_parameters <- data.frame(Week = rep(NA,length(Weeks_to_assess)), 
                                           sp_distance = rep(NA,length(Weeks_to_assess)),
                                           sp_connected = rep(NA,length(Weeks_to_assess)))
sp_k_20_smoothing_parameters <- data.frame(Week = rep(NA,length(Weeks_to_assess)), 
                                           sp_distance = rep(NA,length(Weeks_to_assess)),
                                           sp_connected = rep(NA,length(Weeks_to_assess)))
sp_k_40_smoothing_parameters <- data.frame(Week = rep(NA,length(Weeks_to_assess)), 
                                           sp_distance = rep(NA,length(Weeks_to_assess)),
                                           sp_connected = rep(NA,length(Weeks_to_assess)))

GR_diag_over_1_point_1 <- data.frame(Week = rep(NA,length(Weeks_to_assess)), 
                                     GR_over_point1 = rep(NA,length(Weeks_to_assess)))
Intercept_estimate <- data.frame(Week = rep(NA,length(Weeks_to_assess)), 
                                 intercept_b1 = rep(NA,length(Weeks_to_assess)),
                                 b1_mean = rep(NA,length(Weeks_to_assess)),
                                 b1_lq = rep(NA,length(Weeks_to_assess)),
                                 b1_uq = rep(NA,length(Weeks_to_assess)))


Mixing_estimate <- data.frame(Week = rep(NA,length(Weeks_to_assess)), 
                              phi_dist_est = rep(NA,length(Weeks_to_assess)),
                              phi_dist_mean = rep(NA,length(Weeks_to_assess)),
                              phi_dist_lq = rep(NA,length(Weeks_to_assess)),
                              phi_dist_uq = rep(NA,length(Weeks_to_assess)), 
                              
                              phi_human_est = rep(NA,length(Weeks_to_assess)),
                              phi_human_mean = rep(NA,length(Weeks_to_assess)),
                              phi_human_lq = rep(NA,length(Weeks_to_assess)),
                              phi_human_uq = rep(NA,length(Weeks_to_assess)), 
                              
                              phi_iid_est = rep(NA,length(Weeks_to_assess)),
                              phi_iid_mean = rep(NA,length(Weeks_to_assess)),
                              phi_iid_lq = rep(NA,length(Weeks_to_assess)),
                              phi_iid_uq = rep(NA,length(Weeks_to_assess)))


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
dir.create("outputs/main_summaries")

for(i in 1:length(Weeks_to_assess)){
  
  Week_isolated <- Weeks_to_assess[i]  
  
  Reduced_Data <- filter(Case_Rates_Data, Week == Week_isolated)
  
  #ADD IN THE GRAVITY COORDINATES
  Reduced_Data$connect_coord1_scale <- connect_coords$connect_coord1_scale
  Reduced_Data$connect_coord2_scale <- connect_coords$connect_coord2_scale
  
  #############################
  #NOW FOR ACTUAL MODELLING
  ###########################
  
  #### Step 1: Fit the spatial smooth model ####
  ## Fit GAM to obtain sensible priors for lambdas (sp)
  #Start with k = 10
  
  sp_check_10 <- gam(Week_Cases ~  s(y_scale, x_scale, k = 10, bs = "tp")
                     + s(connect_coord1_scale, connect_coord2_scale, k = 10, bs = "tp"),
                     offset = log(Population),
                     family = "poisson", data = Reduced_Data, method = "REML")
  
  #summary(sp_check_10)
  
  #sp_check_10$sp
  #~ 0.48, which is quite high, but not weird or anything.
  sp_k_10_smoothing_parameters$Week[i] <- Week_isolated
  sp_k_10_smoothing_parameters$sp_distance[i] <- sp_check_10$sp[1]
  sp_k_10_smoothing_parameters$sp_connected[i] <- sp_check_10$sp[2]
  
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
  sp_check_20 <- gam(Week_Cases ~  s(y_scale, x_scale, k = 20, bs = "tp")
                     + s(connect_coord1_scale, connect_coord2_scale, k = 20, bs = "tp"), 
                     offset = log(Population),
                     family = "poisson", data = Reduced_Data, method = "REML")
  
  #summary(sp_check_20)
  #Significant...
  #sp_check_20$sp
  sp_k_20_smoothing_parameters$Week[i] <- Week_isolated
  sp_k_20_smoothing_parameters$sp_distance[i] <- sp_check_20$sp[1]
  sp_k_20_smoothing_parameters$sp_connected[i] <- sp_check_20$sp[2]
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
  
  sp_check_40 <- gam(Week_Cases ~  s(y_scale, x_scale, k = 40, bs = "tp")
                     + s(connect_coord1_scale, connect_coord2_scale, k = 40, bs = "tp"),
                     offset = log(Population),
                     family = "poisson", data = Reduced_Data, method = "REML")
  
  #summary(sp_check_40)
  #Significant...
  #sp_check_40$sp
  #~ 0.235, which makes sense, twice the penalty, with double the basis functions
  sp_k_40_smoothing_parameters$Week[i] <- Week_isolated
  sp_k_40_smoothing_parameters$sp_distance[i] <- sp_check_40$sp[1]
  sp_k_40_smoothing_parameters$sp_connected[i] <- sp_check_40$sp[2]
  
  
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
  jd_smooth <- jagam(Week_Cases ~  s(y_scale, x_scale, k = 40, bs = "tp")
                     + s(connect_coord1_scale, connect_coord2_scale, k = 40, bs = "tp"),
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
  X_dist <- jd_smooth$jags.data$X[,2:40]
  X_human <- jd_smooth$jags.data$X[,41:79]
  
  m_dist <- ncol(X_dist)
  m_human <- ncol(X_human)
  m <- m_dist + m_human
  
  ### Write model formula
  Model <- nimbleCode({ 
    
    # u = spatial smooth term (using basis funtions)
    u[1:n] <- X[1:n, 2:m] %*% b[2:m] #b is the coefficients, beta
    
    # u_dist = spatial smooth term based on distance
    u_dist[1:n] <- X_dist[1:n, 1:m_dist] %*% b[2:(m_dist + 1)] 
    
    # u_human = spatial smooth term based on human movement
    u_human[1:n] <- X_human[1:n, 1:m_human] %*% b[(m_dist + 2):(m + 1)] 
    
    
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
  int<lower=0> M_dist; // Number of distance-based basis functions
  int<lower=0> M_human; // Number of connectivity-based basis functions


  int<lower=0> y[N];              // count outcomes (current week's cases)
  //int<lower=1> K;                 // num covariates 
  int<lower=0> e[N];              // offset (LTLA population)
  
  matrix[N, M_dist] X_dist;                 // basis functions matrix
  matrix[N, M_human] X_human;                 // basis functions matrix
  matrix[M_dist, 2*(M_dist)] S1;        // Penalty matrices
  matrix[M_human, 2*(M_human)] S2;        // Penalty matrices
  vector[M+1] zero;


}
transformed data {
  //matrix[N,T] log_E = log(E + E_neighbours); #kept as formatting reminder
}
parameters {
  vector[M+1] b;       // basis covariates
  vector<lower = 0, upper = 5>[4] lambda;       //penalty parameters
  real<lower = 0> sig_re;        // hierarchical sd parameter for the iid noise (v)
  vector[N] v;        // iid noise
  
}
transformed parameters {
matrix[M_dist, M_dist] K1 = (S1[1:M_dist, 1:M_dist] * lambda[1]) + (S1[1:M_dist, (M_dist+1):(2*(M_dist))] * lambda[2]);
matrix[M_human, M_human] K2 = (S2[1:M_human, 1:M_human] * lambda[3]) + (S2[1:M_human, (M_human+1):(2*(M_human))] * lambda[4]);
vector[N] u_dist = X_dist[1:N, 1:M_dist] * b[2:(M_dist+1)];
vector[N] u_human = X_human[1:N, 1:M_human] * b[(M_dist+2):(M+1)];
}
model {
for(i in 1:N){
  y[i] ~ poisson_log(b[1] + u_dist[i] + u_human[i] + v[i] + log(e[i]));  // 
  v[i] ~ normal(0, sig_re);
}
  
  //PRIORS
  sig_re ~ exponential(0.1);
  // Intercept
  b[1] ~ normal(0, 5);
  
  // Prior for smooth coefficients
  b[2:(M_dist+1)] ~ multi_normal_prec(zero[2:(M_dist+1)], K1);
  b[(M_dist+2):(M+1)] ~ multi_normal_prec(zero[2:(M_human+1)], K2);

  // smoothing parameter priors 
  for (i in 1:4) {
    // truncate lambdas to avoid simulations getting stuck
    lambda[i] ~ gamma(.05, .005);
  }
  
  
  
}
generated quantities {
  //matrix[N,T] eta = log(E[,i] + zetas .*(Cases_Nbors[,i]./N_Nbors)) + beta0 + x[i] * betas + theta[,i]
  vector[N] mu;
  
  for(i in 1:N){
  mu[i] = exp(b[1] + u_dist[i] + u_human[i] + v[i] + log(e[i]));
  }
}
"
# Convert data into data suitable for stan
modelData <- list(N = length(Reduced_Data$Week_Cases), M = (ncol(X_dist)+ncol(X_human)),
                  M_dist = ncol(X_dist), M_human = ncol(X_human),
                  y = Reduced_Data$Week_Cases, X_dist = X_dist, X_human = X_human,
                  zero = jd_smooth$jags.data$zero, 
                  S1 = jd_smooth$jags.data$S1, S2 = jd_smooth$jags.data$S2,
                  e = Reduced_Data$Population)

# Set initial values for MCMC
#inits <- list(b = rnorm(ncol(X),sd = 1), lambda = c(3, 3), 
#              sig_re = runif(1), v = rnorm(nrow(Reduced_Data), 1))
#Maybe try starting from the b of the gam fit...
#inits <- list(b = colMeans(X), lambda = c(3, 3), 
#              sig_re = runif(1), v = rnorm(nrow(Reduced_Data), 1))

stanfit = rstan::stan(model_code = Stan_model_string,
               data=modelData,
               algorithm = "NUTS",
               chains = 4,
               #warmup=2500, 
               iter=2000
               #thin = 100,
               #init = inits,
               #control = list(max_treedepth = 12)
               )


#############################

#9mins:20secs to run iter = 2000 (others default)

main_summaries <- summary(stanfit, pars = c("b", "lambda", "sig_re", "u_dist", "u_human", "v", "mu", "lp__"))
write.csv(main_summaries$summary,sprintf("outputs/main_summaries/main_summaries_%s.csv", Week_isolated), row.names = TRUE)

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

b2_plot <- rstan::traceplot(stanfit, pars= sprintf('b[%s]',15:26))
ggsave(b2_plot,
       filename = sprintf("outputs/posterior_plots/b2_Week_%s.png",Week_isolated))

b1_plot <- rstan::traceplot(stanfit, pars= sprintf('b[%s]',1:14))
ggsave(b1_plot,
       filename = sprintf("outputs/posterior_plots/b1_Week_%s.png",Week_isolated))

b3_plot <- rstan::traceplot(stanfit, pars= sprintf('b[%s]',27:40))
ggsave(b3_plot,
       filename = sprintf("outputs/posterior_plots/b3_Week_%s.png",Week_isolated))

b4_plot <- rstan::traceplot(stanfit, pars= sprintf('b[%s]',41:54))
ggsave(b4_plot,
       filename = sprintf("outputs/posterior_plots/b4_Week_%s.png",Week_isolated))

b5_plot <- rstan::traceplot(stanfit, pars= sprintf('b[%s]',55:68))
ggsave(b5_plot,
       filename = sprintf("outputs/posterior_plots/b5_Week_%s.png",Week_isolated))

b6_plot <- rstan::traceplot(stanfit, pars= sprintf('b[%s]',69:79))
ggsave(b6_plot,
       filename = sprintf("outputs/posterior_plots/b6_Week_%s.png",Week_isolated))

lambda_plot <- rstan::traceplot(stanfit, pars= sprintf('lambda[%s]',1:4))
ggsave(lambda_plot,
       filename = sprintf("outputs/posterior_plots/lambda_Week_%s.png",Week_isolated))

sig_plot <- rstan::traceplot(stanfit, pars= c("sig_re"))
ggsave(sig_plot,
       filename = sprintf("outputs/posterior_plots/sig_re_%s.png",Week_isolated))

u_dist1_plot <- rstan::traceplot(stanfit, pars= sprintf('u_dist[%s]',1:14))
ggsave(u_dist1_plot,
       filename = sprintf("outputs/posterior_plots/u_dist1_Week_%s.png",Week_isolated))

u_dist2_plot <- rstan::traceplot(stanfit, pars= sprintf('u_dist[%s]',15:26))
ggsave(u_dist2_plot,
       filename = sprintf("outputs/posterior_plots/u_dist2_Week_%s.png",Week_isolated))

u_dist3_plot <- rstan::traceplot(stanfit, pars= sprintf('u_dist[%s]',27:40))
ggsave(u_dist3_plot,
       filename = sprintf("outputs/posterior_plots/u_dist3_Week_%s.png",Week_isolated))

u_human1_plot <- rstan::traceplot(stanfit, pars= sprintf('u_human[%s]',1:14))
ggsave(u_human1_plot,
       filename = sprintf("outputs/posterior_plots/u_human1_Week_%s.png",Week_isolated))

u_human2_plot <- rstan::traceplot(stanfit, pars= sprintf('u_human[%s]',15:26))
ggsave(u_human2_plot,
       filename = sprintf("outputs/posterior_plots/u_human2_Week_%s.png",Week_isolated))

u_human3_plot <- rstan::traceplot(stanfit, pars= sprintf('u_human[%s]',27:40))
ggsave(u_human3_plot,
       filename = sprintf("outputs/posterior_plots/u_human3_Week_%s.png",Week_isolated))



v1_plot <- rstan::traceplot(stanfit, pars= sprintf('v[%s]',1:14))
ggsave(v1_plot,
       filename = sprintf("outputs/posterior_plots/v1_Week_%s.png",Week_isolated))

v2_plot <- rstan::traceplot(stanfit, pars= sprintf('v[%s]',15:26))
ggsave(v2_plot,
       filename = sprintf("outputs/posterior_plots/v2_Week_%s.png",Week_isolated))

v3_plot <- rstan::traceplot(stanfit, pars= sprintf('v[%s]',27:40))
ggsave(v3_plot,
       filename = sprintf("outputs/posterior_plots/u3_Week_%s.png",Week_isolated))


# Save the model object
write_rds(stanfit,
          file = sprintf("outputs/model_fits/Week_%s_stanfit.rds", Week_isolated))

#### Extract predicted intercept from models ####
## Spatial smooth model

## Extract simulations of the intercept
#b_sim <- do.call(rbind, results$samples)[ , "b[1]"]
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
Intercept_estimate$b1_mean[i] <- b_est$b_est
Intercept_estimate$b1_lq[i] <- b_est$b_lq
Intercept_estimate$b1_uq[i] <- b_est$b_uq

#### Extract estimates for phi/mixing parameters ####
n <- nrow(Reduced_Data)

## Spatial smooth model
matrix_of_draws <- as.matrix(stanfit)

# Return column numbers with distance-based random effect simulations
u_dist_min <- which(colnames(matrix_of_draws) == "u_dist[1]")
u_dist_max <- which(colnames(matrix_of_draws) == paste0("u_dist[", n, "]"))

# Return column numbers with human movement-based random effect simulations
u_human_min <- which(colnames(matrix_of_draws) == "u_human[1]")
u_human_max <- which(colnames(matrix_of_draws) == paste0("u_human[", n, "]"))


# Return column numbers with unstructured random effect simulations
vmin <- which(colnames(matrix_of_draws) == "v[1]")
vmax <- which(colnames(matrix_of_draws) == paste0("v[", n, "]"))


# Extract simulations of structured random effect
# A 28,500 x 356 matrix
u_dist_mat <- matrix_of_draws[, u_dist_min:u_dist_max]
# Estimate the variance of each simulation (OVER ROWS)
u_dist_var <- apply(u_dist_mat, 1, var)

u_human_mat <- matrix_of_draws[, u_human_min:u_human_max]
# Estimate the variance of each simulation (OVER ROWS)
u_human_var <- apply(u_human_mat, 1, var)

# Extract simulations of unstructured random effect
v_mat <- matrix_of_draws[,vmin:vmax]
# Estimate the variance of each simulation
v_var <- apply(v_mat, 1, var)

# Calculate the proportion of variance explained by the structured term
# Calculate the proportion of variance explained by each term
propn_spat_dist <- u_dist_var/(u_dist_var +  u_human_var + v_var)
propn_spat_human <- u_human_var/(u_dist_var +  u_human_var + v_var)
propn_spat_iid <- v_var/(u_dist_var +  u_human_var + v_var)

re_vars <- data.table(spat_dist_var = u_dist_var,
                      spat_human_var = u_human_var,
                      iid_var = v_var,
                      propn_var_dist = propn_spat_dist,
                      propn_var_human = propn_spat_human,
                      propn_var_iid = propn_spat_iid)

## Combine the estimated phi from each model and calculate mean and 95% CI
smooth_phi_est <- re_vars %>% 
  summarise(phi_dist_est = mean(propn_var_dist),
            phi_dist_lq = quantile(propn_var_dist, .025),
            phi_dist_uq = quantile(propn_var_dist, .975),
            phi_human_est = mean(propn_var_human),
            phi_human_lq = quantile(propn_var_human, .025),
            phi_human_uq = quantile(propn_var_human, .975),
            phi_iid_est = mean(propn_var_iid),
            phi_iid_lq = quantile(propn_var_iid, .025),
            phi_iid_uq = quantile(propn_var_iid, .975))


#Cool, so estimated that ~ 42.5% of the "noise" is spatial!
Mixing_estimate$Week[i] <- Week_isolated
Mixing_estimate$phi_human_est[i] <- paste0(round(mean(smooth_phi_est$phi_human_est), 3), " (",
                                           round(smooth_phi_est$phi_human_lq, 3),
                                           ", ", round(smooth_phi_est$phi_human_uq, 3), ")")
Mixing_estimate$phi_human_mean[i] <- smooth_phi_est$phi_human_est
Mixing_estimate$phi_human_lq[i] <- smooth_phi_est$phi_human_lq
Mixing_estimate$phi_human_uq[i] <- smooth_phi_est$phi_human_uq

Mixing_estimate$phi_dist_est[i] <- paste0(round(mean(smooth_phi_est$phi_dist_est), 3), " (",
                                          round(smooth_phi_est$phi_dist_lq, 3),
                                          ", ", round(smooth_phi_est$phi_dist_uq, 3), ")")
Mixing_estimate$phi_dist_mean[i] <- smooth_phi_est$phi_dist_est
Mixing_estimate$phi_dist_lq[i] <- smooth_phi_est$phi_dist_lq
Mixing_estimate$phi_dist_uq[i] <- smooth_phi_est$phi_dist_uq

Mixing_estimate$phi_iid_est[i] <- paste0(round(mean(smooth_phi_est$phi_iid_est), 3), " (",
                                         round(smooth_phi_est$phi_iid_lq, 3),
                                         ", ", round(smooth_phi_est$phi_iid_uq, 3), ")")
Mixing_estimate$phi_iid_mean[i] <- smooth_phi_est$phi_iid_est
Mixing_estimate$phi_iid_lq[i] <- smooth_phi_est$phi_iid_lq
Mixing_estimate$phi_iid_uq[i] <- smooth_phi_est$phi_iid_uq


#### Calculate model diagnostic statistics

png(sprintf("outputs/plots/diagnostic_plots/Week_%s_stan_diag.png", Week_isolated))
stan_diag(stanfit)
dev.off()

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
u_dist_mean <- apply(u_dist_mat, 2, mean)
u_human_mean <- apply(u_human_mat, 2, mean)
v_mean <- apply(v_mat, 2, mean)
exp_u_dist_mean <- exp(u_dist_mean)
exp_u_human_mean <- exp(u_human_mean)
exp_v_mean <- exp(v_mean)
plot_data <- Reduced_Data[,c(1,4,8)]
plot_data$u_dist_mean <- exp_u_dist_mean
plot_data$u_human_mean <- exp_u_human_mean
plot_data$v_mean <- exp_v_mean
colnames(plot_data) <- c("CODE", "Population", "Week_Cases", "u_dist_mean", "u_human_mean", "v_mean")

Boundaries %>%
  inner_join(plot_data, by = "CODE") %>%
  ggplot( ) +
  geom_sf(aes(fill = u_dist_mean), lwd =  .05) +
  scale_fill_viridis_c(name = "u_dist_mean") +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> u_dist_plot

ggsave(u_dist_plot,
       filename = sprintf("outputs/fitted_spatial_components/Week_%s_distance_based_u.png", Week_isolated))

Boundaries %>%
  inner_join(plot_data, by = "CODE") %>%
  ggplot( ) +
  geom_sf(aes(fill = u_human_mean), lwd =  .05) +
  scale_fill_viridis_c(name = "u_human_mean") +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> u_human_plot

ggsave(u_human_plot,
       filename = sprintf("outputs/fitted_spatial_components/Week_%s_human_movement_based_u.png", Week_isolated))

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
          u_dist_plot, u_human_plot, vplot,
          nrow = 2, labels = c(sprintf("Real cases/pop - Week %s", Week_isolated),
                               sprintf("Bayesian u_dist - %s percent", Mixing_estimate$phi_dist_est[i]),
                               sprintf("Bayesian u_human - %s percent", Mixing_estimate$phi_human_est[i]),
                               sprintf("Bayesian v - iid noise - %s bad GRs", GR_diag_over_1_point_1$GR_over_point1[i])),
          scale = 0.9) -> combined_plot


ggsave(combined_plot,
       filename = sprintf("outputs/plots/Combined/Week_%s.png", Week_isolated),
       width = 10.82, height = 9.52, units = c("in"))

#Remove stanfit to aid memory
rm(stanfit)

############################################

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
       filename = "outputs/estimates_plots_over_all_weeks/Rhat.png")
ggplot(data = Intercept_estimate) +
  geom_point(aes(x= Week, y = b1_mean)) +
  geom_errorbar(aes(x=Week, ymin = b1_lq, ymax = b1_uq)) -> intercept_plot
ggsave(intercept_plot, 
       filename = "outputs/estimates_plots_over_all_weeks/b1_intercept.png")

ggplot(data = Mixing_estimate) +
  geom_point(aes(x= Week, y = phi_dist_mean)) +
  geom_errorbar(aes(x = Week, ymin = phi_dist_lq, ymax = phi_dist_uq)) -> percent_distance_plot
ggsave(percent_distance_plot, 
       filename = "outputs/estimates_plots_over_all_weeks/u_dist_percent.png")

ggplot(data = Mixing_estimate) +
  geom_point(aes(x= Week, y = phi_human_mean)) +
  geom_errorbar(aes(x = Week, ymin = phi_human_lq, ymax = phi_human_uq)) -> percent_human_plot
ggsave(percent_human_plot, 
       filename = "outputs/estimates_plots_over_all_weeks/u_human_percent.png")

ggplot(data = Mixing_estimate) +
  geom_point(aes(x= Week, y = phi_iid_mean)) +
  geom_errorbar(aes(x = Week, ymin = phi_iid_lq, ymax = phi_iid_uq)) -> percent_iid_plot
ggsave(percent_iid_plot, 
       filename = "outputs/estimates_plots_over_all_weeks/v_iid_percent.png")

#Now plot all three on the same scale
Mixing_long <- data.frame(Week = rep(0,3*length(Weeks_to_assess)),
                          type = rep(0,3*length(Weeks_to_assess)),
                          mean = rep(0,3*length(Weeks_to_assess)),
                          lq = rep(0,3*length(Weeks_to_assess)),
                          uq = rep(0,3*length(Weeks_to_assess)))

for(i in 1:length(Weeks_to_assess)){
  Week_isolated <- Weeks_to_assess[i]
  index_range <- ((3*i)-2):(3*i)
  Mixing_long$Week[index_range] <- Week_isolated
  Mixing_long$type[index_range] <- c("u_dist", "u_human", "v")
  extracted_mixing <- filter(Mixing_estimate, Week == Week_isolated)
  Mixing_long$mean[index_range] <- c(extracted_mixing$phi_dist_mean, extracted_mixing$phi_human_mean, extracted_mixing$phi_iid_mean)
  Mixing_long$lq[index_range] <- c(extracted_mixing$phi_dist_lq, extracted_mixing$phi_human_lq, extracted_mixing$phi_iid_lq)
  Mixing_long$uq[index_range] <- c(extracted_mixing$phi_dist_uq, extracted_mixing$phi_human_uq, extracted_mixing$phi_iid_uq)
  
}

ggplot(data = Mixing_long) +
  geom_point(aes(x= Week, y = mean, color = type)) +
  geom_errorbar(aes(x = Week, ymin = lq, ymax = uq, color = type)) -> percent_joined_plot
ggsave(percent_joined_plot, 
       filename = "outputs/estimates_plots_over_all_weeks/All_phi_combined.png")



ggplot(data = MAE_estimate) +
  geom_point(aes(x= Week, y = mae)) -> MAE_plot
ggsave(MAE_plot, 
       filename = "outputs/estimates_plots_over_all_weeks/MAE_estimate.png")

ggplot(data = sp_k_10_smoothing_parameters) +
  geom_point(aes(x= Week, y = sp_distance)) -> sp_k_10_plot
ggsave(sp_k_10_plot, 
       filename = "outputs/estimates_plots_over_all_weeks/sp_distance_k_10_plot.png")
ggplot(data = sp_k_20_smoothing_parameters) +
  geom_point(aes(x= Week, y = sp_distance)) -> sp_k_20_plot
ggsave(sp_k_20_plot, 
       filename = "outputs/estimates_plots_over_all_weeks/sp_distance_k_20_plot.png")
ggplot(data = sp_k_40_smoothing_parameters) +
  geom_point(aes(x= Week, y = sp_distance)) -> sp_k_40_plot
ggsave(sp_k_40_plot,
       filename = "outputs/estimates_plots_over_all_weeks/sp_distance_k_40_plot.png")


ggplot(data = sp_k_10_smoothing_parameters) +
  geom_point(aes(x= Week, y = sp_connected)) -> sp_k_10_plot
ggsave(sp_k_10_plot, 
       filename = "outputs/estimates_plots_over_all_weeks/sp_connected_k_10_plot.png")
ggplot(data = sp_k_20_smoothing_parameters) +
  geom_point(aes(x= Week, y = sp_connected)) -> sp_k_20_plot
ggsave(sp_k_20_plot, 
       filename = "outputs/estimates_plots_over_all_weeks/sp_connected_k_20_plot.png")
ggplot(data = sp_k_40_smoothing_parameters) +
  geom_point(aes(x= Week, y = sp_connected)) -> sp_k_40_plot
ggsave(sp_k_40_plot, 
       filename = "outputs/estimates_plots_over_all_weeks/sp_connected_k_40_plot.png")

