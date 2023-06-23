#This script reads in the Case data as provided in task 00, runs a stan model,
#and outputs the stanfit object for future use.

###########################################################################
#Load the cleaned and prepared case/covariate data
load('Cases_Data.RData')
#Load the neigbors matrix
load('W.RData')


###################
################################################################################
#When running this code originally with just the cumulative vaccination numbers,
#there was an odd trend where it would punish the uptick of second jabs. I think
#this is because it allows for an additive impact of respective jabs which doesn't quite make sense

#Instead we change our cumulative vaccination, to PROPORTION vaccination,
#i.e. if you sum prop_no_dose, prop_1_dose, prop_2_dose, prop_3_dose for every i,j, it'll equal 1.
#Quickly added in this switch to deal with that:
PROP_vacc <- TRUE

#Additionally, I have the option to choose between two options for variant data
#
use_SGTF_data <- TRUE
#if TRUE, data is unique for each LTLA, but is based on yes/no SGTF data, which
#is built from less information and so is arguably less reliable
#if FALSE, data is from the dashboard at NHS region level (with a bit of VAM)
#although I have lumped together the Alpha, Delta, and Omicron variants
################################################################################

#if you are using rstan locally on a multicore machine and have plenty of RAM to
#estimate your model in parallel, at this point execute
options(mc.cores = parallel::detectCores())
#and
rstan_options(auto_write = TRUE)
#which allows you to automatically save a bare version of a compiled Stan program 
#to the hard disk so that it does not need to be recompiled (unless you change it). 
#You will need to run these commands each time you load the rstan library.

###################
#Prepare Data
  
  #Cut down to the only covariates we're interested in:
  Case_Rates_Data <- Case_Rates_Data[,-c(5,6,15,16,18,19,22,24,25,28,30,31,32,33,
                                         43, #Cut Omicron BQ1
                                         50,53,54,55,64,65,66:80)] #Added 4 to all these indices
  
  
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

##############################################################################
if(PROP_vacc){
  Case_Rates_Data$prop_one_dose <- Case_Rates_Data$cumVaccPercentage_FirstDose - Case_Rates_Data$cumVaccPercentage_SecondDose
  Case_Rates_Data$prop_two_dose <- Case_Rates_Data$cumVaccPercentage_SecondDose - Case_Rates_Data$cumVaccPercentage_ThirdDose
  Case_Rates_Data$prop_three_dose <- Case_Rates_Data$cumVaccPercentage_ThirdDose
}

###################

###################
#Begin STAN fit



#Define model

Stan_model_string_gravity = "
data {
  int<lower=0> N; // Number of areas
  int<lower=0> T; // Number of timepoints


  int<lower=0> y[N,T];              // count outcomes (next week's cases)
  matrix<lower=0>[N,T] E;           // exposure (current week's cases)
  matrix<lower=0>[N,T] E_neighbours; // exposure (mean current week's cases for neighbours)
  int<lower=1> K;                 // num covariates 
  matrix[N, K] x[T];                 // design matrix
  
  matrix<lower=0,upper=1>[N,T] susceptible_proxy; //Factor in a rough metric of how many susceptibles in the population
  
  
  //Distance between LTLAs
  matrix<lower=0>[N,N] Distance_matrix;
  
    //Populations of LTLAs (scaled between 0 and 1 as proportion of total population)
  real<lower=0> Populations[N];

}
transformed data {
}
parameters {
  real beta0;            // intercept
  vector[K] betas;       // covariates
  vector[T] beta_random_walk; //We add in a random walk error term

  vector[N] theta;       // heterogeneous effects
  real theta_mu; //hierarchical hyperparameter for drawing theta
  real<lower=0> theta_sd; //hierarchical hyperparameter for drawing theta
  
  
  real<lower=0> distance_alpha;
  real<lower=0> distance_gamma;
  
}
transformed parameters {
matrix<lower=0>[N,N] smoothed_distance_matrix;
matrix<lower=0>[N,N] scaled_distance_matrix;
  for(i in 1:N){
  for(j in 1:N){
  smoothed_distance_matrix[i,j] = (Populations[i]*Populations[j])/((1 + (Distance_matrix[i,j]/distance_alpha))^distance_gamma);
  }
  }
  
//Lastly, we scale this matrix such that it is a valid probability, by scaling all COLUMNS to sum to 1
  
  for(i in 1:N){
    scaled_distance_matrix[,i] = smoothed_distance_matrix[,i]/sum(smoothed_distance_matrix[,i]);
  }

}
model {
y[,1] ~ poisson_log(log(susceptible_proxy[,1].*((scaled_distance_matrix*E[,1]))) + beta0 + (x[1] * betas) + (beta_random_walk[1]) + theta);
for(i in 2:T){
  y[,i] ~ poisson_log(log(susceptible_proxy[,i].*((scaled_distance_matrix*E[,i]))) + beta0 + (x[i] * betas) + (beta_random_walk[i] - beta_random_walk[i-1]) + theta);  // extra noise removed removed: + theta[,i]
}


  beta0 ~ normal(0.0, 1.0);
  betas ~ normal(0.0, 1.0);
  //zetas ~ normal(0.05, 1.0);
  beta_random_walk ~ normal(0.0, 1.0);
  
  
  theta ~ normal(theta_mu, theta_sd);
  theta_mu ~ normal(0.0,1.0);
  theta_sd ~ uniform(0.0,20.0);
  
  
  distance_alpha ~ uniform(0,50);
  distance_gamma ~ normal(1.0, 1.0);

  
}
generated quantities {
}
"



######################################################################################################################


T <- final_week-1;
#Note, 306 is after cutting the 21 welsh, and 29 scottish. JUST ENGLISH CODES
N <- 306;
y <- array(0, dim = c(T,N))
E <- array(0, dim = c(T,N))
#K is current number of covariates
K <- 21;
x <- array(0, dim = c(T, N,K))

scale_by_recent_cases <- array(0, dim = c(T,N))

for(i in 2:final_week){
  j <- i-1
  
  Reduced_Data <- filter(Case_Rates_Data, Week == i)
  Reduced_Data <- Reduced_Data[order(Reduced_Data$INDEX),]
  
  y[j,] = Reduced_Data$next_week_cases;
  E[j,] = Reduced_Data$Week_Cases;
  scale_by_recent_cases[j,] = Reduced_Data$total_cases/Reduced_Data$Population
  
  #scale() will fail if all variables are the same value (i.e. if sd = 0)
  
  #For now I will not scale the percentages, I'll just give them from 0 to 1
  
  if(PROP_vacc){
    x[j,,1] <- Reduced_Data$prop_one_dose/100
    x[j,,2] <- Reduced_Data$prop_two_dose/100
    x[j,,3] <- Reduced_Data$prop_three_dose/100
  }else{
    x[j,,1] <- Reduced_Data$cumVaccPercentage_FirstDose/100
    x[j,,2] <- Reduced_Data$cumVaccPercentage_SecondDose/100
    x[j,,3] <- Reduced_Data$cumVaccPercentage_ThirdDose/100
  }
  x[j,,4] <- scale(Reduced_Data$prop_white_british)
  x[j,,5] <- scale(Reduced_Data$prop_asian)
  x[j,,6] <- scale(Reduced_Data$prop_black_afr_car)
  x[j,,7] <- scale(Reduced_Data$IMD_Average_score)   
  x[j,,8] <- scale(Reduced_Data$prop_o65) 
  x[j,,9] <- scale(Reduced_Data$Median_annual_income) 
  x[j,,10] <- scale(Reduced_Data$workplaces_percent_change_from_baseline)
  x[j,,11] <- scale(Reduced_Data$residential_percent_change_from_baseline)
  x[j,,12] <- scale(Reduced_Data$transit_stations_percent_change_from_baseline)
  if(use_SGTF_data){
    x[j,,13] <- Reduced_Data$s_Alpha_prop
    x[j,,14] <- Reduced_Data$s_Delta_prop
    x[j,,15] <- Reduced_Data$s_Omicron_prop
    
  } else{
    x[j,,13] <- Reduced_Data$Alpha_proportion/100
    x[j,,14] <- (Reduced_Data$Delta_proportion + Reduced_Data$Delta_AY_4_2_proportion)/100
    x[j,,15] <- (Reduced_Data$Omicron_BA_1_proportion + Reduced_Data$Omicron_BA_2_proportion + Reduced_Data$Omicron_BA_4_proportion + Reduced_Data$Omicron_BA_5_proportion)/100
  }
  
  x[j,,16] <- scale(Reduced_Data$Core_services_funding_by_weighted)
  x[j,,17] <- scale(Reduced_Data$Primary_care_funding_by_weighted)
  x[j,,18] <- scale(Reduced_Data$Specialised_services_by_weighted)
  x[j,,19] <- scale(Reduced_Data$unringfenced/Reduced_Data$Population)
  x[j,,20] <- scale(Reduced_Data$contain_outbreak_management/Reduced_Data$Population)
  x[j,,21] <- scale(Reduced_Data$ASC_infection_control_fund/Reduced_Data$Population)
  
}



W_reduced <- W[Reduced_Data$INDEX, Reduced_Data$INDEX]

#Calculate E_neighbours
E_neighbours <- W_reduced%*%t(E)
if(scale_by_number_of_neighbours == TRUE){
Nbors <- as.numeric(rowSums(W_reduced))
} else{
  Nbors <- 1
}
E_neighbours_scaled <- E_neighbours/Nbors
susceptible_proportion_estimate <- 1 - scale_by_recent_cases #This is currently #of cases / population, so 1 - this is a rough proxy of S/N

if(scale_by_susceptible_pool){
  susceptible_proxy <- t(susceptible_proportion_estimate)
}else if(!scale_by_susceptible_pool){
  susceptible_proxy <- t(array(1, dim = c(T,N)))
}else{
  stop("Incompatible scale_by_susceptible_pool. Expected TRUE or FALSE")
}

#################################################################################
#Build the distance matrix
#Pick a random week to extract distance/population data from
Week_50_data <- filter(Case_Rates_Data, Week == 50)
Week_50_data <- Week_50_data[,c(4,5,45,46)]
Week_50_data <- Week_50_data[order(Week_50_data$INDEX),]

Week_50_data <- Week_50_data %>% 
  mutate(y_scale = (centroid_y - min(centroid_y))/(max(centroid_y) - min(centroid_y)),
         x_scale = (centroid_x - min(centroid_x))/(max(centroid_x) - min(centroid_x)))


Distance_matrix <- array(0, dim = c(N,N))
Populations <- rep(0, length(Week_50_data$Population))
for(i in 1:306){
  Populations[i] <- Week_50_data$Population[i] /sum(Week_50_data$Population)
  for(j in 1:306){
    location_i <- Week_50_data[i,]
    location_j <- Week_50_data[j,]
    euclid_distance <- sqrt((location_i$x_scale - location_j$x_scale)^2 + (location_i$y_scale - location_j$y_scale)^2)
    Distance_matrix[i,j] <- (euclid_distance)
  }
}

#Populations is a proportion, so sums to 1

#################################################################################


stanfit = stan(model_code = Stan_model_string_gravity,
               data=list(N=N,T=T,
                         y=t(y),
                         x=x, K=K,
                         E=t(E),
                         E_neighbours = E_neighbours_scaled,
                         Distance_matrix = Distance_matrix,
                         susceptible_proxy = susceptible_proxy,
                         Populations = Populations),
               #warmup=warmup_iterations, iter=total_iterations,
               control = list(max_treedepth = tree_depth));

#Make folder for outputs
dir.create("Outputs")

save(stanfit, file = 'Outputs/stanfit.RData')
save(N,T,y,x,K,E,E_neighbours_scaled,
       Distance_matrix, susceptible_proxy, Populations,
       Case_Rates_Data, file = "Outputs/model_data.RData")

