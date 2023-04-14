## Script to employ the method's presented in Sophie Lee's paper;
## "A Bayesian modelling framework to quantify multiple sources of spatial variation for disease mapping"
## https://doi.org/10.1098/rsif.2022.0440

#This combines the changes made in 01d and 01e, human-connectedness AND covariates included together

#AND, instead off-setting with population, we offset with LAST WEEK's cases!
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
#final_week <- 15
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
rm(Neighbor_hold, missing_data, Week_hold, transit_mean_hold, Rutland_neighbors)
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
dir.create("outputs/plots/betas_plots")

#Make an array to hold our basis function covariates
X_dist <- array(0, dim = c(length(Weeks_to_assess),306, 39))
X_human <- array(0, dim = c(length(Weeks_to_assess),306, 39))

S1_data <- array(0, dim = c(length(Weeks_to_assess), 39,78))
S2_data <- array(0, dim = c(length(Weeks_to_assess), 39,78))

#################################################################################
#Build the distance matrix
#scaled also by population of location
# pluck the population/distance info from random week (10)
Reduced_Data <- filter(Case_Rates_Data, Week == 10)
gravity_hold_data <- Reduced_Data[,which(colnames(Reduced_Data) %in% c("areaName", "Population", "INDEX", "centroid_x", "centroid_y", "y_scale", "x_scale"))]
#Keep this but I'm not currently ordering by INDEX
#gravity_hold_data <- gravity_hold_data[order(gravity_hold_data$INDEX),] 
N <- nrow(gravity_hold_data)
Distance_matrix <- array(0, dim = c(N,N))
for(I in 1:N){
  for(J in 1:N){
    if(I == J){
      
    }else{
      location_i <- gravity_hold_data[I,]
      location_j <- gravity_hold_data[J,]
      euclid_distance <- sqrt((location_i$centroid_x - location_j$centroid_x)^2 + (location_i$centroid_y - location_j$centroid_y)^2)
      Distance_matrix[I,J] <- ((location_i$Population/1000)*(location_j$Population/1000))/(euclid_distance)
    }
  }
}

#Now, this matrix conveys gravitational strength, but we want the matrix to instead
# capture dissimilarity, i.e. low gravity = high dissimilarity, so we invert the values
#test <- 1/Distance_matrix
for(I in 1:N){
  for(J in 1:N){
    if(I == J){
      
    }else{
      location_i <- gravity_hold_data[I,]
      location_j <- gravity_hold_data[J,]
      euclid_distance <- sqrt((location_i$centroid_x - location_j$centroid_x)^2 + (location_i$centroid_y - location_j$centroid_y)^2)
      Distance_matrix[I,J] <- 1/(((location_i$Population/1000)*(location_j$Population/1000))/(euclid_distance))
    }
  }
}

msd_scale <- cmdscale(Distance_matrix, eig = T, k = 2)
# Plot to view coordinate system
#plot(msd_scale$points[,1], msd_scale$points[,2])

# Convert into a df with 'connectivity coordinates' per city
connect_coords <-  as.data.frame(msd_scale$points)
names(connect_coords) <- c("connect_coord1", "connect_coord2")
connect_coords$areaName <- gravity_hold_data$areaName

connect_coords <- connect_coords %>% 
  mutate(connect_coord1_scale = (connect_coord1 - min(connect_coord1))/(max(connect_coord1) - min(connect_coord1)),
         connect_coord2_scale = (connect_coord2 - min(connect_coord2))/(max(connect_coord2) - min(connect_coord2)))


#################################################################################

for(i in 1:length(Weeks_to_assess)){

  Week_isolated <- Weeks_to_assess[i]  

Reduced_Data <- filter(Case_Rates_Data, Week == Week_isolated)


#ADD IN THE GRAVITY COORDINATES
Reduced_Data$connect_coord1_scale <- connect_coords$connect_coord1_scale
Reduced_Data$connect_coord2_scale <- connect_coords$connect_coord2_scale

#############################
#NOW FOR ACTUAL MODELLING
###########################

###################################################################################
# NOW FOR THE BAYESIAN MODELLING
# Start with just distance-based, and heterogeneous noise.

### Fit a model with a smooth spatial random effect and an IID random
## effect in NIMBLE to data 

# Use mgcv to extract basis functions for the smooth term
jd_smooth <- jagam(Week_Cases ~  s(y_scale, x_scale, k = 40, bs = "tp")
                   + s(connect_coord1_scale, connect_coord2_scale, k = 40, bs = "tp"),
                   offset = log(previous_week_cases),
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
X_hold <- jd_smooth$jags.data$X[,2:40]
X_dist[i,,] <- X_hold
m_dist <- ncol(X_hold)

X_hold <- jd_smooth$jags.data$X[,41:79]
X_human[i,,] <- X_hold
m_human <- ncol(X_hold)

m <- m_dist + m_human

S1_hold <- jd_smooth$jags.data$S1
S1_data[i,,] <- S1_hold

S2_hold <- jd_smooth$jags.data$S2
S2_data[i,,] <- S2_hold

}
#End the Loop here. 

#Make an array to hold our covariate data!

K <- 26 #Number of covariates

X_covariates <- array(0, dim = c(length(Weeks_to_assess), length(unique(Case_Rates_Data$areaCode)), K))


for(i in 2:max(Weeks_to_assess)){
  j <- i-1
  
  Reduced_Data <- filter(Case_Rates_Data, Week == i)
  #Don't use this line to re-order, but can be handy to remember how.
  #Reduced_Data <- Reduced_Data[order(Reduced_Data$INDEX),]
  #scale() will fail if all variables are the same value (i.e. if sd = 0)
  
  #For now I will not scale the percentages, I'll just give them from 0 to 1
  
  X_covariates[j,,1] <- Reduced_Data$cumVaccPercentage_FirstDose/100
  X_covariates[j,,2] <- Reduced_Data$cumVaccPercentage_SecondDose/100
  X_covariates[j,,3] <- Reduced_Data$cumVaccPercentage_ThirdDose/100
  X_covariates[j,,4] <- scale(Reduced_Data$prop_white_british)
  X_covariates[j,,5] <- scale(Reduced_Data$prop_asian)
  X_covariates[j,,6] <- scale(Reduced_Data$prop_black_afr_car)
  X_covariates[j,,7] <- scale(Reduced_Data$IMD_Average_score)   
  X_covariates[j,,8] <- scale(Reduced_Data$prop_o65) 
  X_covariates[j,,9] <- scale(Reduced_Data$Median_annual_income) 
  X_covariates[j,,10] <- scale(Reduced_Data$workplaces_percent_change_from_baseline)
  X_covariates[j,,11] <- scale(Reduced_Data$residential_percent_change_from_baseline)
  X_covariates[j,,12] <- scale(Reduced_Data$transit_stations_percent_change_from_baseline)
  X_covariates[j,,13] <- Reduced_Data$Alpha_proportion/100
  X_covariates[j,,14] <- Reduced_Data$Delta_proportion/100
  X_covariates[j,,15] <- Reduced_Data$Delta_AY_4_2_proportion/100
  X_covariates[j,,16] <- Reduced_Data$Omicron_BA_1_proportion/100
  X_covariates[j,,17] <- Reduced_Data$Omicron_BA_2_proportion/100
  X_covariates[j,,18] <- Reduced_Data$Omicron_BA_4_proportion/100
  X_covariates[j,,19] <- Reduced_Data$Omicron_BA_5_proportion/100
  X_covariates[j,,20] <- Reduced_Data$Other_proportion/100
  X_covariates[j,,21] <- scale(Reduced_Data$Core_services_funding_by_weighted)
  X_covariates[j,,22] <- scale(Reduced_Data$Primary_care_funding_by_weighted)
  X_covariates[j,,23] <- scale(Reduced_Data$Specialised_services_by_weighted)
  X_covariates[j,,24] <- scale(Reduced_Data$unringfenced/Reduced_Data$Population)
  X_covariates[j,,25] <- scale(Reduced_Data$contain_outbreak_management/Reduced_Data$Population)
  X_covariates[j,,26] <- scale(Reduced_Data$ASC_infection_control_fund/Reduced_Data$Population)
  
}





### Write model formula


Stan_model_string = "
data {
  int<lower=0> N; // Number of areas
  int<lower=0> M; // Number of basis functions
  int<lower=0> M_dist; // Number of distance-based basis functions
  int<lower=0> M_human; // Number of connectivity-based basis functions
  
  int<lower=0> T; // Number of time points


  int<lower=0> y[N, T];              // count outcomes (current week's cases)
  int<lower=1> K;                 // num covariates 
  int<lower=0> e[N, T];              // offset (previous weeks cases)
  
  matrix[N, M_dist] X_dist[T];                 // basis functions matrix
  matrix[N, M_human] X_human[T];                 // basis functions matrix
  
  matrix[M_dist, 2*(M_dist)] S1[T];        // Penalty matrices
  matrix[M_human, 2*(M_human)] S2[T];        // Penalty matrices
  vector[M+1] zero;
  
  matrix[N, K] X_covariates[T];  //Covariates matrix


}
transformed data {
  //matrix[N,T] log_E = log(E + E_neighbours); #kept as formatting reminder
}
parameters {
  //matrix[M,T] b;       // basis coefficients
  vector[M+1] b[T];       // basis coefficients
  vector<lower = 0, upper = 5>[4] lambda[T];       //penalty parameters
  real<lower = 0> sig_re[T];        // hierarchical sd parameter for the iid noise (v)
  vector[N] v[T];        // iid noise
  
  vector[K] betas;       // covariates coefficients
  
}
transformed parameters {
matrix[M_dist, M_dist] K1[T];
matrix[M_human, M_human] K2[T];
vector[N] u_dist[T];
vector[N] u_human[T];

for(t in 1:T){
K1[t,,] = (S1[t,1:(M_dist), 1:(M_dist)] * lambda[t,1]) + (S1[t,1:(M_dist), (M_dist+1):(2*(M_dist))] * lambda[t,2]);
K2[t,,] = (S2[t,1:(M_human), 1:(M_human)] * lambda[t,3]) + (S2[t,1:(M_human), (M_human+1):(2*(M_human))] * lambda[t,4]);

u_dist[t] = X_dist[t,1:N, 1:M_dist] * b[t,2:(M_dist+1)];
u_human[t] = X_human[t,1:N, 1:M_human] * b[t,(M_dist+2):(M+1)];
}
}
model {
for(t in 1:T){
for(i in 1:N){
  y[i,t] ~ poisson_log(b[t,1] + u_dist[t][i] + u_human[t][i] + v[t][i] + (X_covariates[t,i,] * betas) + log(e[i,t]));  // 
  v[t][i] ~ normal(0, sig_re[t]);
}
}
  
  //PRIORS
  for(t in 1:T){
  sig_re[t] ~ exponential(0.1);
  // Intercept
  b[t,1] ~ normal(0, 5);
  
  // Prior for smooth coefficients
  b[t,2:(M_dist+1)] ~ multi_normal_prec(zero[2:(M_dist+1)], K1[t]);
  b[t,(M_dist+2):(M+1)] ~ multi_normal_prec(zero[2:(M_human+1)], K2[t]);

  // smoothing parameter priors 
  for (i in 1:4) {
    // truncate lambdas to avoid simulations getting stuck
    lambda[t][i] ~ gamma(.05, .005);
  }
  
  //Covariate coefficients
  betas ~ normal(0.0, 1.0);
  
  }
  
  
}
generated quantities {
  //matrix[N,T] eta = log(E[,i] + zetas .*(Cases_Nbors[,i]./N_Nbors)) + beta0 + x[i] * betas + theta[,i]
  matrix[T,N] mu;
  
  for(t in 1:T){
  for(i in 1:N){
  mu[t,i] = exp(b[t,1] + u_dist[t][i] + u_human[t][i] + v[t][i] + (X_covariates[t,i,] * betas) + log(e[i,t]));
  }
  }
}
"
# Convert data into data suitable for stan
#y[N, T]
y_data <- array(0, dim = c(length(unique(Case_Rates_Data$areaCode)), length(Weeks_to_assess)))
prev_week_data <- array(0, dim = c(length(unique(Case_Rates_Data$areaCode)), length(Weeks_to_assess)))
for(i in 1:length(Weeks_to_assess)){
  Reduced_Data <- filter(Case_Rates_Data, Week == Weeks_to_assess[i])
  y_data[,i] <- Reduced_Data$Week_Cases
  prev_week_data[,i] <- Reduced_Data$previous_week_cases
}



modelData <- list(N = length(unique(Case_Rates_Data$areaCode)), 
                  M = (m_dist+m_human),
                  M_dist = m_dist, M_human =m_human,
                  T = length(Weeks_to_assess),
                  y = y_data, K = K, 
                  X_dist = X_dist, X_human = X_human,
                  zero = jd_smooth$jags.data$zero, 
                   S1 = S1_data, S2 = S2_data,
                  e = prev_week_data,
                  X_covariates = X_covariates)


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

main_summaries <- summary(stanfit, pars = c("b", "lambda", "sig_re", "u_dist", "u_human", "v", "mu", "betas", "lp__"))
write.csv(main_summaries$summary,"outputs/main_summaries.csv", row.names = TRUE)

betas_summaries <- summary(stanfit, pars = c("betas"))
write.csv(betas_summaries$summary,"outputs/betas_summaries.csv", row.names = TRUE)

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
                       sprintf("u_dist\\[%s,", t),
                       sprintf("u_human\\[%s,", t),
                       sprintf("v\\[%s,", t),
                       sprintf("mu\\[%s,", t)
                       )

  data_hold2 <- data_hold[grep(paste(strings_to_keep,collapse="|"), rownames(data_hold)),]
  
  GR_diag_over_1_point_1$Week[t] <- Week_isolated
  GR_diag_over_1_point_1$GR_over_point1[t] <-  sum(data_hold2[, "Rhat"] > 1.05)
  
  
}

betas_GR_over_1_point_05 <-  data_hold[grep("betas", rownames(data_hold)),]
betas_GR_over_1_point_05 <-  sum(betas_GR_over_1_point_05[, "Rhat"] > 1.05)
GR_diag_over_1_point_1$GR_over_point1 <- GR_diag_over_1_point_1$GR_over_point1 + betas_GR_over_1_point_05



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

b4_plot <- rstan::traceplot(stanfit, pars= sprintf('b[%s,%s]',t,41:54))
ggsave(b4_plot,
       filename = sprintf("outputs/posterior_plots/b4_Week_%s.png",Week_isolated))

b5_plot <- rstan::traceplot(stanfit, pars= sprintf('b[%s,%s]',t,55:68))
ggsave(b5_plot,
       filename = sprintf("outputs/posterior_plots/b5_Week_%s.png",Week_isolated))

b6_plot <- rstan::traceplot(stanfit, pars= sprintf('b[%s,%s]',t,69:79))
ggsave(b6_plot,
       filename = sprintf("outputs/posterior_plots/b6_Week_%s.png",Week_isolated))

lambda_plot <- rstan::traceplot(stanfit, pars= sprintf('lambda[%s,%s]',t,1:4))
ggsave(lambda_plot,
       filename = sprintf("outputs/posterior_plots/lambda_Week_%s.png",Week_isolated))

sig_plot <- rstan::traceplot(stanfit, pars= sprintf("sig_re[%s]",t))
ggsave(sig_plot,
       filename = sprintf("outputs/posterior_plots/sig_re_%s.png",Week_isolated))

u1_plot <- rstan::traceplot(stanfit, pars= sprintf('u_dist[%s,%s]',t,1:14))
ggsave(u1_plot,
       filename = sprintf("outputs/posterior_plots/u_dist1_Week_%s.png",Week_isolated))

u2_plot <- rstan::traceplot(stanfit, pars= sprintf('u_dist[%s,%s]',t,15:26))
ggsave(u2_plot,
       filename = sprintf("outputs/posterior_plots/u_dist2_Week_%s.png",Week_isolated))

u3_plot <- rstan::traceplot(stanfit, pars= sprintf('u_dist[%s,%s]',t,27:40))
ggsave(u3_plot,
       filename = sprintf("outputs/posterior_plots/u_dist3_Week_%s.png",Week_isolated))

u1_plot <- rstan::traceplot(stanfit, pars= sprintf('u_human[%s,%s]',t,1:14))
ggsave(u1_plot,
       filename = sprintf("outputs/posterior_plots/u_human1_Week_%s.png",Week_isolated))

u2_plot <- rstan::traceplot(stanfit, pars= sprintf('u_human[%s,%s]',t,15:26))
ggsave(u2_plot,
       filename = sprintf("outputs/posterior_plots/u_human2_Week_%s.png",Week_isolated))

u3_plot <- rstan::traceplot(stanfit, pars= sprintf('u_human[%s,%s]',t,27:40))
ggsave(u3_plot,
       filename = sprintf("outputs/posterior_plots/u_human3_Week_%s.png",Week_isolated))



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

#betas traceplots
betas_plot <- rstan::traceplot(stanfit, pars= sprintf('betas[%s]',1:26))
ggsave(betas_plot,
       filename = "outputs/plots/betas_plots/betas_traceplot.png")

#Also want to plot the posterior distribution for each betas param
########################################################################
#Next we have a look at how good our fit is
betas_1_12 <- plot(stanfit, pars = sprintf('betas[%s]',1:12))
#ci_level: 0.8 (80% intervals)
#outer_level: 0.95 (95% intervals)
betas_1_12 <- betas_1_12 + scale_y_continuous(breaks = c(12:1),
                                              labels = c("1) CumVacc_1dose", 
                                                         "2) CumVacc_2dose", "3) CumVacc_3dose",
                                                         "4) prop_white_british", "5) prop_asian",
                                                         "6) prop_black_afr", "7) IMD_Average_score",
                                                         "8) prop_o65", "9) Median_annual_income",
                                                         "10) workplaces_movement",
                                                         "11) residential_movement", "12) transit_movement")) +
  geom_vline(xintercept = 0, color = "skyblue", lty = 5, size = 1) +ggtitle("Covariate coefficients")

png(file="outputs/plots/betas_plots/betas_1_12.png",
    width=1440, height=1080, res = 150)
plot(betas_1_12)
dev.off()


betas_13_20 <- plot(stanfit, pars = sprintf('betas[%s]',13:20))
#ci_level: 0.8 (80% intervals)
#outer_level: 0.95 (95% intervals)
betas_13_20 <- betas_13_20 + scale_y_continuous(breaks = c(8:1),
                                                labels = c("13) Alpha", "14) Delta", 
                                                           "15) Delta_AY_4_2", "16) Omicron_BA_1",
                                                           "17) Omicron_BA_2", "18) Omicron_BA_4",
                                                           "19) Omicron_BA_5", "20) Other variants")) +
  geom_vline(xintercept = 0, color = "skyblue", lty = 5, size = 1) +ggtitle("Variant proportion coefficients")

png(file="outputs/plots/betas_plots/betas_13_20.png",
    width=1440, height=1080, res = 150)
plot(betas_13_20)
dev.off()

betas_21_26 <- plot(stanfit, pars = sprintf('betas[%s]',21:26))
#ci_level: 0.8 (80% intervals)
#outer_level: 0.95 (95% intervals)
betas_21_26 <- betas_21_26 + scale_y_continuous(breaks = c(6:1),
                                                labels = c("21) Core Services", "22) Primary Care", 
                                                           "23) Specialised Services", "24) Unringfenced",
                                                           "25) Outbreak Management", "26) ASC infection control")) +
  geom_vline(xintercept = 0, color = "skyblue", lty = 5, size = 1) +ggtitle("Funding coefficients")

png(file="outputs/plots/betas_plots/betas_21_26.png",
    width=1440, height=1080, res = 150)
plot(betas_21_26)
dev.off()

### We also plot the spatial plot of all the covariates themselves
#X_covariates[j,,1] <- Reduced_Data$cumVaccPercentage_FirstDose/100
#X_covariates[j,,2] <- Reduced_Data$cumVaccPercentage_SecondDose/100
#X_covariates[j,,3] <- Reduced_Data$cumVaccPercentage_ThirdDose/100

dir.create("outputs/plots/covariate_plots")
dir.create("outputs/plots/covariate_plots/FirstDose")
dir.create("outputs/plots/covariate_plots/SecondDose")
dir.create("outputs/plots/covariate_plots/ThirdDose")

dir.create("outputs/plots/covariate_plots/Alpha")
dir.create("outputs/plots/covariate_plots/Delta")
dir.create("outputs/plots/covariate_plots/Delta_AY_4_2")
dir.create("outputs/plots/covariate_plots/Omicron_BA_1")
dir.create("outputs/plots/covariate_plots/Omicron_BA_2")
dir.create("outputs/plots/covariate_plots/Omicron_BA_4")
dir.create("outputs/plots/covariate_plots/Omicron_BA_5")
dir.create("outputs/plots/covariate_plots/Other_variant")
dir.create("outputs/plots/covariate_plots/workplace")
dir.create("outputs/plots/covariate_plots/residential")
dir.create("outputs/plots/covariate_plots/transit")

for(i in 1:length(Weeks_to_assess)){
  
  Week_isolated <- Weeks_to_assess[i]  
  Reduced_Data <- filter(Case_Rates_Data, Week == Week_isolated)
  
  
  
plot_data <- Reduced_Data[,c(1,10,11,12)]
colnames(plot_data) <- c("CODE", "First_dose", "Second_dose", "Third_dose")

Boundaries %>% 
  inner_join(plot_data, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = First_dose), lwd =  .05) +
  scale_fill_viridis_c(name = sprintf("Week %s First Dose uptake", Week_isolated)) +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> firstdose_plot

ggsave(firstdose_plot, 
       filename = sprintf("outputs/plots/covariate_plots/FirstDose/Week_%s_First_Dose.png", Week_isolated))

##################
Boundaries %>% 
  inner_join(plot_data, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = Second_dose), lwd =  .05) +
  scale_fill_viridis_c(name = sprintf("Week %s Second Dose uptake", Week_isolated)) +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> seconddose_plot

ggsave(seconddose_plot, 
       filename = sprintf("outputs/plots/covariate_plots/SecondDose/Week_%s_Second_Dose.png", Week_isolated))

##################
Boundaries %>% 
  inner_join(plot_data, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = Third_dose), lwd =  .05) +
  scale_fill_viridis_c(name = sprintf("Week %s Third Dose uptake", Week_isolated)) +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> thirddose_plot

ggsave(thirddose_plot, 
       filename = sprintf("outputs/plots/covariate_plots/ThirdDose/Week_%s_Third_Dose.png", Week_isolated))

##############################

plot_data <- Reduced_Data[,c(1,23,24,25,26,27,28,29,30)]
names(plot_data)[names(plot_data) == 'areaCode'] <- 'CODE'

Boundaries %>% 
  inner_join(plot_data, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = Alpha_proportion), lwd =  .05) +
  scale_fill_viridis_c(name = sprintf("Week %s Alpha Proportion", Week_isolated)) +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> alpha_plot

ggsave(alpha_plot, 
       filename = sprintf("outputs/plots/covariate_plots/Alpha/Week_%s_Alpha.png", Week_isolated))

##################

Boundaries %>% 
  inner_join(plot_data, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = Delta_proportion), lwd =  .05) +
  scale_fill_viridis_c(name = sprintf("Week %s Delta Proportion", Week_isolated)) +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> delta_plot

ggsave(delta_plot, 
       filename = sprintf("outputs/plots/covariate_plots/Delta/Week_%s_Delta.png", Week_isolated))

##################

Boundaries %>% 
  inner_join(plot_data, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = Delta_AY_4_2_proportion), lwd =  .05) +
  scale_fill_viridis_c(name = sprintf("Week %s Delta AY 4 2 Proportion", Week_isolated)) +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> delta_plot

ggsave(delta_plot, 
       filename = sprintf("outputs/plots/covariate_plots/Delta_AY_4_2/Week_%s_Delta_AY42.png", Week_isolated))

##################

Boundaries %>% 
  inner_join(plot_data, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = Omicron_BA_1_proportion), lwd =  .05) +
  scale_fill_viridis_c(name = sprintf("Week %s Omicron BA 1 Proportion", Week_isolated)) +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> omicron_plot

ggsave(omicron_plot, 
       filename = sprintf("outputs/plots/covariate_plots/Omicron_BA_1/Week_%s_Omicron_BA1.png", Week_isolated))

##################

Boundaries %>% 
  inner_join(plot_data, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = Omicron_BA_1_proportion), lwd =  .05) +
  scale_fill_viridis_c(name = sprintf("Week %s Omicron BA 1 Proportion", Week_isolated)) +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> omicron_plot

ggsave(omicron_plot, 
       filename = sprintf("outputs/plots/covariate_plots/Omicron_BA_1/Week_%s_Omicron_BA1.png", Week_isolated))

##################

Boundaries %>% 
  inner_join(plot_data, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = Omicron_BA_2_proportion), lwd =  .05) +
  scale_fill_viridis_c(name = sprintf("Week %s Omicron BA 2 Proportion", Week_isolated)) +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> omicron_plot

ggsave(omicron_plot, 
       filename = sprintf("outputs/plots/covariate_plots/Omicron_BA_2/Week_%s_Omicron_BA2.png", Week_isolated))

##################

Boundaries %>% 
  inner_join(plot_data, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = Omicron_BA_4_proportion), lwd =  .05) +
  scale_fill_viridis_c(name = sprintf("Week %s Omicron BA 4 Proportion", Week_isolated)) +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> omicron_plot

ggsave(omicron_plot, 
       filename = sprintf("outputs/plots/covariate_plots/Omicron_BA_4/Week_%s_Omicron_BA4.png", Week_isolated))

##################

Boundaries %>% 
  inner_join(plot_data, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = Omicron_BA_5_proportion), lwd =  .05) +
  scale_fill_viridis_c(name = sprintf("Week %s Omicron BA 5 Proportion", Week_isolated)) +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> omicron_plot

ggsave(omicron_plot, 
       filename = sprintf("outputs/plots/covariate_plots/Omicron_BA_5/Week_%s_Omicron_BA5.png", Week_isolated))

##################

Boundaries %>% 
  inner_join(plot_data, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = Other_proportion), lwd =  .05) +
  scale_fill_viridis_c(name = sprintf("Week %s Other Variant Proportion", Week_isolated)) +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> other_plot

ggsave(other_plot, 
       filename = sprintf("outputs/plots/covariate_plots/Other_variant/Week_%s_Other.png", Week_isolated))

##################
plot_data <- Reduced_Data[,c(1,20, 21, 22)]
names(plot_data)[names(plot_data) == 'areaCode'] <- 'CODE'

Boundaries %>% 
  inner_join(plot_data, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = workplaces_percent_change_from_baseline), lwd =  .05) +
  scale_fill_viridis_c(name = sprintf("Week %s Workplace percent", Week_isolated)) +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> other_plot

ggsave(other_plot, 
       filename = sprintf("outputs/plots/covariate_plots/workplace/Week_%s_workplace.png", Week_isolated))

##################

Boundaries %>% 
  inner_join(plot_data, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = residential_percent_change_from_baseline), lwd =  .05) +
  scale_fill_viridis_c(name = sprintf("Week %s residential percent", Week_isolated)) +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> other_plot

ggsave(other_plot, 
       filename = sprintf("outputs/plots/covariate_plots/residential/Week_%s_residential.png", Week_isolated))

##################

Boundaries %>% 
  inner_join(plot_data, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = transit_stations_percent_change_from_baseline), lwd =  .05) +
  scale_fill_viridis_c(name = sprintf("Week %s transit percent", Week_isolated)) +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> other_plot

ggsave(other_plot, 
       filename = sprintf("outputs/plots/covariate_plots/transit/Week_%s_transit.png", Week_isolated))

##################

}


plot_data <- Case_Rates_Data[,c(1,13:19)]
plot_data2 <- Reduced_Data[,c(1,13:19)]
names(plot_data)[names(plot_data) == 'areaCode'] <- 'CODE'
names(plot_data2)[names(plot_data2) == 'areaCode'] <- 'CODE'

Boundaries %>% 
  inner_join(plot_data2, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = prop_white_british), lwd =  .05) +
  scale_fill_viridis_c(name = "Proportion White British") +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> prop_plot

ggsave(prop_plot, 
       filename = "outputs/plots/covariate_plots/Prop_White_British.png")

##################

Boundaries %>% 
  inner_join(plot_data2, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = prop_asian), lwd =  .05) +
  scale_fill_viridis_c(name = "Proportion Asian") +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> prop_plot

ggsave(prop_plot, 
       filename = "outputs/plots/covariate_plots/Prop_Asian.png")

##################

Boundaries %>% 
  inner_join(plot_data2, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = prop_black_afr_car), lwd =  .05) +
  scale_fill_viridis_c(name = "Proportion Black Afr Car") +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> prop_plot

ggsave(prop_plot, 
       filename = "outputs/plots/covariate_plots/Prop_Black_Afr_Car.png")

##################

Boundaries %>% 
  inner_join(plot_data2, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = prop_o65), lwd =  .05) +
  scale_fill_viridis_c(name = "Proportion Over 65") +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> prop_plot

ggsave(prop_plot, 
       filename = "outputs/plots/covariate_plots/Prop_White_British.png")

##################

Boundaries %>% 
  inner_join(plot_data2, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = IMD_Average_score), lwd =  .05) +
  scale_fill_viridis_c(name = "IMD Average score") +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> prop_plot

ggsave(prop_plot, 
       filename = "outputs/plots/covariate_plots/IMD_average_score.png")

##################
plot_data <- Case_Rates_Data[,c(1,2,19)]
names(plot_data)[names(plot_data) == 'areaCode'] <- 'CODE'
plot_data$tax_year <- plot_data$Week > 49

plot_data2 <- filter(plot_data, !tax_year)
plot_data3 <- filter(plot_data2, Week == 5)

Boundaries %>% 
  inner_join(plot_data3, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = Median_annual_income), lwd =  .05) +
  scale_fill_viridis_c(name = "Median income 20/21") +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> prop_plot

ggsave(prop_plot, 
       filename = "outputs/plots/covariate_plots/Median_income_1.png")

##################

plot_data2 <- filter(plot_data, tax_year)
plot_data3 <- filter(plot_data2, Week == 60)

Boundaries %>% 
  inner_join(plot_data3, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = Median_annual_income), lwd =  .05) +
  scale_fill_viridis_c(name = "Median income 21/22") +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> prop_plot

ggsave(prop_plot, 
       filename = "outputs/plots/covariate_plots/Median_income_2.png")

##################
plot_data <- Case_Rates_Data[,c(1,2,4,33:38)]
names(plot_data)[names(plot_data) == 'areaCode'] <- 'CODE'
plot_data$tax_year <- plot_data$Week > 49

plot_data2 <- filter(plot_data, !tax_year)
plot_data3 <- filter(plot_data2, Week == 5)

Boundaries %>% 
  inner_join(plot_data3, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = Core_services_funding_by_weighted), lwd =  .05) +
  scale_fill_viridis_c(name = "Core services (weighted) 20/21") +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> prop_plot

ggsave(prop_plot, 
       filename = "outputs/plots/covariate_plots/core_services_1.png")

##################

Boundaries %>% 
  inner_join(plot_data3, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = Primary_care_funding_by_weighted), lwd =  .05) +
  scale_fill_viridis_c(name = "Primary Care (weighted) 20/21") +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> prop_plot

ggsave(prop_plot, 
       filename = "outputs/plots/covariate_plots/primary_care_1.png")

##################

Boundaries %>% 
  inner_join(plot_data3, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = Specialised_services_by_weighted), lwd =  .05) +
  scale_fill_viridis_c(name = "Specialised services \n (weighted) 20/21") +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> prop_plot

ggsave(prop_plot, 
       filename = "outputs/plots/covariate_plots/specialised_services_1.png")

##################

Boundaries %>% 
  inner_join(plot_data3, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = unringfenced/Population), lwd =  .05) +
  scale_fill_viridis_c(name = "Unringfenced \n (weighted) 20/21") +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> prop_plot

ggsave(prop_plot, 
       filename = "outputs/plots/covariate_plots/unringfenced_1.png")

##################

Boundaries %>% 
  inner_join(plot_data3, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = contain_outbreak_management/Population), lwd =  .05) +
  scale_fill_viridis_c(name = "Contain outbreak \n (weighted) 20/21") +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> prop_plot

ggsave(prop_plot, 
       filename = "outputs/plots/covariate_plots/contain_outbreak_1.png")

##################

Boundaries %>% 
  inner_join(plot_data3, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = ASC_infection_control_fund/Population), lwd =  .05) +
  scale_fill_viridis_c(name = "infection control \n (weighted) 20/21") +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> prop_plot

ggsave(prop_plot, 
       filename = "outputs/plots/covariate_plots/infection_control_1.png")

##################
plot_data2 <- filter(plot_data, tax_year)
plot_data3 <- filter(plot_data2, Week == 60)

Boundaries %>% 
  inner_join(plot_data3, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = Core_services_funding_by_weighted), lwd =  .05) +
  scale_fill_viridis_c(name = "Core services (weighted) 21/22") +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> prop_plot

ggsave(prop_plot, 
       filename = "outputs/plots/covariate_plots/core_services_2.png")

##################

Boundaries %>% 
  inner_join(plot_data3, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = Primary_care_funding_by_weighted), lwd =  .05) +
  scale_fill_viridis_c(name = "Primary Care (weighted) 21/22") +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> prop_plot

ggsave(prop_plot, 
       filename = "outputs/plots/covariate_plots/primary_care_2.png")

##################

Boundaries %>% 
  inner_join(plot_data3, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = Specialised_services_by_weighted), lwd =  .05) +
  scale_fill_viridis_c(name = "Specialised services \n (weighted) 21/22") +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> prop_plot

ggsave(prop_plot, 
       filename = "outputs/plots/covariate_plots/specialised_services_2.png")

##################

Boundaries %>% 
  inner_join(plot_data3, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = unringfenced/Population), lwd =  .05) +
  scale_fill_viridis_c(name = "Unringfenced \n (weighted) 21/22") +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> prop_plot

ggsave(prop_plot, 
       filename = "outputs/plots/covariate_plots/unringfenced_2.png")

##################

Boundaries %>% 
  inner_join(plot_data3, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = contain_outbreak_management/Population), lwd =  .05) +
  scale_fill_viridis_c(name = "Contain outbreak \n (weighted) 21/22") +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> prop_plot

ggsave(prop_plot, 
       filename = "outputs/plots/covariate_plots/contain_outbreak_2.png")

##################

Boundaries %>% 
  inner_join(plot_data3, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = ASC_infection_control_fund/Population), lwd =  .05) +
  scale_fill_viridis_c(name = "infection control \n (weighted) 21/22") +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> prop_plot

ggsave(prop_plot, 
       filename = "outputs/plots/covariate_plots/infection_control_2.png")

##################
########################################################################

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
   
   #ADD IN THE GRAVITY COORDINATES
   Reduced_Data$connect_coord1_scale <- connect_coords$connect_coord1_scale
   Reduced_Data$connect_coord2_scale <- connect_coords$connect_coord2_scale
## Spatial smooth model

# Return column numbers with structured random effect simulations
u_dist_cols <- which(colnames(matrix_of_draws) %in% sprintf("u_dist[%s,%s]",t,1:306))
u_human_cols <- which(colnames(matrix_of_draws) %in% sprintf("u_human[%s,%s]",t,1:306))

# Return column numbers with unstructured random effect simulations
v_cols <- which(colnames(matrix_of_draws) %in% sprintf("v[%s,%s]",t,1:306))

# Extract simulations of structured random effect
# A 28,500 x 356 matrix
u_dist_mat <- matrix_of_draws[, u_dist_cols]
u_human_mat <- matrix_of_draws[, u_human_cols]
# Estimate the variance of each simulation (OVER ROWS)
u_dist_var <- apply(u_dist_mat, 1, var)
u_human_var <- apply(u_human_mat, 1, var)

# Extract simulations of unstructured random effect
v_mat <- matrix_of_draws[,v_cols]
# Estimate the variance of each simulation
v_var <- apply(v_mat, 1, var)

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
Mixing_estimate$Week[t] <- Week_isolated
Mixing_estimate$phi_human_est[t] <- paste0(round(mean(smooth_phi_est$phi_human_est), 3), " (",
                                           round(smooth_phi_est$phi_human_lq, 3),
                                           ", ", round(smooth_phi_est$phi_human_uq, 3), ")")
Mixing_estimate$phi_human_mean[t] <- smooth_phi_est$phi_human_est
Mixing_estimate$phi_human_lq[t] <- smooth_phi_est$phi_human_lq
Mixing_estimate$phi_human_uq[t] <- smooth_phi_est$phi_human_uq

Mixing_estimate$phi_dist_est[t] <- paste0(round(mean(smooth_phi_est$phi_dist_est), 3), " (",
                                          round(smooth_phi_est$phi_dist_lq, 3),
                                          ", ", round(smooth_phi_est$phi_dist_uq, 3), ")")
Mixing_estimate$phi_dist_mean[t] <- smooth_phi_est$phi_dist_est
Mixing_estimate$phi_dist_lq[t] <- smooth_phi_est$phi_dist_lq
Mixing_estimate$phi_dist_uq[t] <- smooth_phi_est$phi_dist_uq

Mixing_estimate$phi_iid_est[t] <- paste0(round(mean(smooth_phi_est$phi_iid_est), 3), " (",
                                         round(smooth_phi_est$phi_iid_lq, 3),
                                         ", ", round(smooth_phi_est$phi_iid_uq, 3), ")")
Mixing_estimate$phi_iid_mean[t] <- smooth_phi_est$phi_iid_est
Mixing_estimate$phi_iid_lq[t] <- smooth_phi_est$phi_iid_lq
Mixing_estimate$phi_iid_uq[t] <- smooth_phi_est$phi_iid_uq

#Let's plot the spatial bit:

###############################################
#HERE's THE SPLINE PLOTS THAT WERE ORIGINALLY DONE PRE-FIT:

#### Step 1: Fit the spatial smooth model ####
## Fit GAM to obtain sensible priors for lambdas (sp)
#Start with k = 10

sp_check_10 <- gam(Week_Cases ~  s(y_scale, x_scale, k = 10, bs = "tp")
                   + s(connect_coord1_scale, connect_coord2_scale, k = 10, bs = "tp"), offset = log(Population),
                   family = "poisson", data = Reduced_Data, method = "REML")

#summary(sp_check_10)

#sp_check_10$sp
#~ 0.48, which is quite high, but not weird or anything.
sp_k_10_smoothing_parameters$Week[t] <- Week_isolated
sp_k_10_smoothing_parameters$sp_distance[t] <- sp_check_10$sp[1]
sp_k_10_smoothing_parameters$sp_connected[t] <- sp_check_10$sp[2]

#Let's just plot the cases first:
plot_data <- Reduced_Data[,c(1,4,6,8)]
colnames(plot_data) <- c("CODE", "Population", "Week_Cases", "Previous_Week_Cases")

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

#And plot divided by previous weeks cases
Boundaries %>% 
  inner_join(plot_data, by = "CODE") %>% 
  ggplot( ) +
  geom_sf(aes(fill = Week_Cases/Previous_Week_Cases), lwd =  .05) +
  scale_fill_viridis_c(name = sprintf("Week %s R estimate", Week_isolated)) +
  theme_void() +
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> R_est_plot

ggsave(R_est_plot, 
       filename = sprintf("outputs/plots/Cases_by_population/Week_%s_R_est_plot.png", Week_isolated))




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
sp_k_20_smoothing_parameters$Week[t] <- Week_isolated
sp_k_20_smoothing_parameters$sp_distance[t] <- sp_check_20$sp[1]
sp_k_20_smoothing_parameters$sp_connected[t] <- sp_check_20$sp[2]
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
sp_k_40_smoothing_parameters$Week[t] <- Week_isolated
sp_k_40_smoothing_parameters$sp_distance[t] <- sp_check_40$sp[1]
sp_k_40_smoothing_parameters$sp_connected[t] <- sp_check_40$sp[2]


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
u_dist_mean <- apply(u_dist_mat, 2, mean)
u_human_mean <- apply(u_human_mat, 2, mean)
v_mean <- apply(v_mat, 2, mean)
exp_u_dist_mean <- exp(u_dist_mean)
exp_u_human_mean <- exp(u_human_mean)
exp_v_mean <- exp(v_mean)
plot_data <- Reduced_Data[,c(1,4,6)]
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
       filename = sprintf("outputs/fitted_spatial_components/Week_%s_u_dist.png", Week_isolated))

Boundaries %>%
  inner_join(plot_data, by = "CODE") %>%
  ggplot( ) +
  geom_sf(aes(fill = u_human_mean), lwd =  .05) +
  scale_fill_viridis_c(name = "u_human_mean") +
  theme_void()+
  theme(plot.background = element_rect(fill = 'white', color = "white")) -> u_human_plot

ggsave(u_human_plot,
       filename = sprintf("outputs/fitted_spatial_components/Week_%s_u_human.png", Week_isolated))


Boundaries %>%
  inner_join(plot_data, by = "CODE") %>%
  ggplot( ) +
  geom_sf(aes(fill = v_mean), lwd =  .05) +
  scale_fill_viridis_c(name = "v_mean") +
  theme_void() +
  theme(plot.background = element_rect(fill = 'white', color = "white"))-> vplot

ggsave(vplot,
       filename = sprintf("outputs/fitted_spatial_components/Week_%s_iid_random_v.png", Week_isolated))

plot_grid(R_est_plot + theme(legend.title= element_blank()),
          u_dist_plot, u_human_plot, vplot,
          nrow = 2, labels = c(sprintf("R estimate - Week %s", Week_isolated),
                               sprintf("Bayesian u_dist - %s percent", Mixing_estimate$phi_dist_est[t]),
                               sprintf("Bayesian u_human - %s percent", Mixing_estimate$phi_human_est[t]),
                               sprintf("Bayesian v - iid noise - %s bad GRs", GR_diag_over_1_point_1$GR_over_point1[t])),
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
  geom_point(aes(x= Week, y = phi_dist_mean)) +
  geom_errorbar(aes(x = Week, ymin = phi_dist_lq, ymax = phi_dist_uq)) -> percent_distance_plot
ggsave(percent_distance_plot, 
       filename = "outputs/estimates_plots_over_all_weeks/u_dist_based_spatial.png")

ggplot(data = Mixing_estimate) +
  geom_point(aes(x= Week, y = phi_human_mean)) +
  geom_errorbar(aes(x = Week, ymin = phi_human_lq, ymax = phi_human_uq)) -> percent_human_plot
ggsave(percent_human_plot, 
       filename = "outputs/estimates_plots_over_all_weeks/u_human_based_spatial.png")

ggplot(data = Mixing_estimate) +
  geom_point(aes(x= Week, y = phi_iid_mean)) +
  geom_errorbar(aes(x = Week, ymin = phi_iid_lq, ymax = phi_iid_uq)) -> percent_iid_plot
ggsave(percent_iid_plot, 
       filename = "outputs/estimates_plots_over_all_weeks/v_iid_based_spatial.png")

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



