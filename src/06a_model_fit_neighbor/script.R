#This script reads in the Case data as provided in task 00, runs a stan model,
#and outputs the stanfit object for future use.

###########################################################################
#Load the cleaned and prepared case/covariate data
load('Cases_Data.RData')
#Load the neigbors matrix
load('W.RData')

#if you are using rstan locally on a multicore machine and have plenty of RAM to
#estimate your model in parallel, at this point execute
options(mc.cores = parallel::detectCores())
#and
rstan_options(auto_write = TRUE)
#which allows you to automatically save a bare version of a compiled Stan program 
#to the hard disk so that it does not need to be recompiled (unless you change it). 
#You will need to run these commands each time you load the rstan library.

###################
#Let's print an output .txt of the parameters used:
param_string <- sprintf("tree_depth: %s \n
  scale_by_number_of_neighbours: %s \n
  scale_by_susceptible_pool: %s \n
  cases_type: %s \n
  use_prop_vacc: %s \n
  use_SGTF_data: %s \n
  final_week: %s \n
  theta_ON: %s \n
  random_walk_prior_scale: %s \n
  rw_penalty: %s ", tree_depth, scale_by_number_of_neighbours, 
                        scale_by_susceptible_pool, cases_type,
                        use_prop_vacc, use_SGTF_data, final_week, theta_ON,
                        random_walk_prior_scale, rw_penalty)

fileConn<-file("parameters_used.txt")
writeLines(param_string, fileConn)
close(fileConn)
################################################################################
#When running this code originally with just the cumulative vaccination numbers,
#there was an odd trend where it would punish the uptick of second jabs. I think
#this is because it allows for an additive impact of respective jabs which doesn't quite make sense

#Instead we change our cumulative vaccination, to PROPORTION vaccination,
#i.e. if you sum prop_no_dose, prop_1_dose, prop_2_dose, prop_3_dose for every i,j, it'll equal 1.
#Quickly added in this switch to deal with that:
PROP_vacc <- use_prop_vacc

#Additionally, I have the option to choose between two options for variant data
#

#use_SGTF_data <- TRUE

#if TRUE, data is unique for each LTLA, but is based on yes/no SGTF data, which
#is built from less information and so is arguably less reliable
#if FALSE, data is from the dashboard at NHS region level (with a bit of VAM)
#although I have lumped together the Alpha, Delta, and Omicron variants
################################################################################

#Prepare Data
  
  #Cut down to the only covariates we're interested in:
  #test <- Case_Rates_Data[,-c(5,6,15,16,18,19,22,24,25,28,30,31,32,33,
  #                                       43, #Cut Omicron BQ1
  #                                       50,53,54,55,64,65,66:80)] #Added 4 to all these indices
  
  
Case_Rates_Data <- Case_Rates_Data[,c("areaCode", "Week", "areaName",
                                      "Population", "INDEX",
                                      "date_begin"                                   ,
                                      "Week_Cases"                                   ,
                                      "previous_week_cases"                          ,
                                      "next_week_cases"                              ,
                                      "Linelist_P2_PCR_Week_Cases",
                                      "Linelist_P2_PCR_Previous_Week_Cases",
                                      "Linelist_P2_PCR_Next_Week_Cases",
                                      "First_Episodes_Total",
                                      "cumVaccPercentage_FirstDose"                  ,
                                      "cumVaccPercentage_SecondDose"                 ,
                                      "cumVaccPercentage_ThirdDose"                  ,
                                      "prop_white_british"                           ,
                                      "prop_asian"                                   ,
                                      "prop_black_afr_car"                           ,
                                      "IMD_Average_score"                            ,
                                      "mean_age"                                     ,
                                      "prop_o65"                                     ,
                                      "Median_annual_income"                         ,
                                      "transit_stations_percent_change_from_baseline",
                                      "workplaces_percent_change_from_baseline"      ,
                                      "residential_percent_change_from_baseline"     ,
                                      "Alpha_proportion"                             ,
                                      "Delta_proportion"                             ,
                                      "Delta_AY_4_2_proportion"                      ,
                                      "Omicron_BA_1_proportion"                      ,
                                      "Omicron_BA_2_proportion"                      ,
                                      "Other_proportion"                             ,
                                      "Omicron_BA_4_proportion"                      ,
                                      "Omicron_BA_5_proportion"                      ,
                                      "s_Wild_prop"                                  ,
                                      "s_Alpha_prop"                                 ,
                                      "s_Delta_prop"                                 ,
                                      "s_Omicron_prop"                               ,
                                      "CCG_2019_Name"                                ,
                                      "NHS_registered_population"                    ,
                                      "Core_services_funding_by_weighted"            ,
                                      "Primary_care_funding_by_weighted"             ,
                                      "Specialised_services_by_weighted"             ,
                                      "unringfenced"                                 ,
                                      "contain_outbreak_management"                  ,
                                      "ASC_infection_control_fund"                   ,
                                      "ASC_workforce_capacity"                       ,
                                      "ASC_rapid_testing"                            ,
                                      "centroid_x"                                   ,
                                      "centroid_y")] 


  #Weeks go from 2:130
  #Note, the government stopped providing free LFTs on April 1st 2022
  #This will have, in turn, affected the case data, so let's chop off everything
  #after week 104 (w/b 25/04/22)
  #TODO: Could include still but keep this as a variable

#This is now a orderly parameter.  
#final_week <- 104
 
#KEY WEEKS:
#104 - (w/b 25/04/22) the default, before testing gets weird
#96 - (w/b 27/02/22) Neil thought this might be a smarter place to end
#71 - (w/b 5/09/21) This is the last week with NO s_Omicron_prop
  
 
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
#Begin STAN fit



#Define model
if(theta_ON){
#Annoyingly, we need to have two cases of the script, for if we scale or not
if(scale_by_susceptible_pool){
Stan_model_string_neighbours = "
data {
  int<lower=0> N; // Number of areas
  int<lower=0> T; // Number of timepoints


  int<lower=0> y[N,T];              // count outcomes (next week's cases)
  matrix<lower=0>[N,T] E;           // exposure (current week's cases)
  matrix<lower=0>[N,T] E_neighbours; // exposure (mean current week's cases for neighbours)
  int<lower=1> K;                 // num covariates 
  matrix[N, K] x[T];                 // design matrix
  
  matrix<lower=0,upper=1>[N,T] susceptible_proxy; //Factor in a rough metric of how many susceptibles in the population

  real random_walk_prior; //What prior value to use in the prior distribution for the random walk sd
  real penalty_term; // Penalty applied to the random_walk magnitude

}
transformed data {
}
parameters {
  vector[K] betas;       // covariates
  vector<lower = 0, upper = 1>[N] zetas;       //spatial kernel, but this can't be less than 0
  
  vector[T] beta_random_walk_steps; //We add in a random walk error term
  real<lower=0> sqrtQ; //Standard deviation of random walk

  vector[N] theta;       // heterogeneous effects
  real theta_mu; //hierarchical hyperparameter for drawing theta
  real<lower=0, upper = 20> theta_sd; //hierarchical hyperparameter for drawing theta
  
  real<lower  = 0, upper = 1> susc_scaling; //parameter for scaling the number of first episodes so far for aqcquired immunity
}
transformed parameters {
vector[T] beta_random_walk  = cumulative_sum(beta_random_walk_steps);
}
model {

y[,1] ~ poisson_log(log((susc_scaling*susceptible_proxy[,1]).*(E[,1] + (zetas .*E_neighbours[,1]))) + x[1] * betas + (beta_random_walk[1]) + theta);  // extra noise removed removed: + theta[,i]
target += -penalty_term*fabs(beta_random_walk[1]);
for(i in 2:T){
  y[,i] ~ poisson_log(log((susc_scaling*susceptible_proxy[,i]).*(E[,i] + (zetas .*E_neighbours[,i]))) + x[i] * betas + (beta_random_walk[i]) + theta);  // extra noise removed removed: + theta[,i]
  target += -penalty_term*fabs(beta_random_walk[i]);
}

  betas ~ normal(0.0, 1.0);
  zetas ~ normal(0.05, 1.0);
  
  sqrtQ ~ gamma(1,random_walk_prior);
  susc_scaling ~ beta(1,1);

  for(i in 1:T){
  beta_random_walk_steps[i] ~ normal(0, sqrtQ);
  }
  
  theta ~ normal(theta_mu, theta_sd);
  theta_mu ~ normal(0.0,1.0);
  theta_sd ~ uniform(0.0,20.0);

}
generated quantities {
}
"
} else {
  Stan_model_string_neighbours = "
data {
  int<lower=0> N; // Number of areas
  int<lower=0> T; // Number of timepoints


  int<lower=0> y[N,T];              // count outcomes (next week's cases)
  matrix<lower=0>[N,T] E;           // exposure (current week's cases)
  matrix<lower=0>[N,T] E_neighbours; // exposure (mean current week's cases for neighbours)
  int<lower=1> K;                 // num covariates 
  matrix[N, K] x[T];                 // design matrix
  
  matrix<lower=0,upper=1>[N,T] susceptible_proxy; //Factor in a rough metric of how many susceptibles in the population

  real random_walk_prior; //What prior value to use in the prior distribution for the random walk sd
  real penalty_term; // Penalty applied to the random_walk magnitude
}
transformed data {
}
parameters {
  vector[K] betas;       // covariates
  vector<lower = 0, upper = 1>[N] zetas;       //spatial kernel, but this can't be less than 0
  
  vector[T] beta_random_walk_steps; //We add in a random walk error term
  real<lower=0> sqrtQ; //Standard deviation of random walk

  vector[N] theta;       // heterogeneous effects
  real theta_mu; //hierarchical hyperparameter for drawing theta
  real<lower=0, upper = 20> theta_sd; //hierarchical hyperparameter for drawing theta
  
  real<lower  = 0, upper = 1> susc_scaling; //parameter for scaling the number of first episodes so far for aqcquired immunity
}
transformed parameters {
vector[T] beta_random_walk  = cumulative_sum(beta_random_walk_steps);
}
model {

y[,1] ~ poisson_log(log((susceptible_proxy[,1]).*(E[,1] + (zetas .*E_neighbours[,1]))) + x[1] * betas + (beta_random_walk[1]) + theta);  // extra noise removed removed: + theta[,i]
target += -penalty_term*fabs(beta_random_walk[1]);
for(i in 2:T){
  y[,i] ~ poisson_log(log((susceptible_proxy[,i]).*(E[,i] + (zetas .*E_neighbours[,i]))) + x[i] * betas + (beta_random_walk[i]) + theta);  // extra noise removed removed: + theta[,i]
  target += -penalty_term*fabs(beta_random_walk[i]);
}

  betas ~ normal(0.0, 1.0);
  zetas ~ normal(0.05, 1.0);
  
  sqrtQ ~ gamma(1,random_walk_prior);

  for(i in 1:T){
  beta_random_walk_steps[i] ~ normal(0, sqrtQ);
  }
  
  theta ~ normal(theta_mu, theta_sd);
  theta_mu ~ normal(0.0,1.0);
  theta_sd ~ uniform(0.0,20.0);

}
generated quantities {
}
"
}

}else{
  if(scale_by_susceptible_pool){
    Stan_model_string_neighbours = "
data {
  int<lower=0> N; // Number of areas
  int<lower=0> T; // Number of timepoints


  int<lower=0> y[N,T];              // count outcomes (next week's cases)
  matrix<lower=0>[N,T] E;           // exposure (current week's cases)
  matrix<lower=0>[N,T] E_neighbours; // exposure (mean current week's cases for neighbours)
  int<lower=1> K;                 // num covariates 
  matrix[N, K] x[T];                 // design matrix
  
  matrix<lower=0,upper=1>[N,T] susceptible_proxy; //Factor in a rough metric of how many susceptibles in the population

  real random_walk_prior; //What prior value to use in the prior distribution for the random walk sd
  real penalty_term; // Penalty applied to the random_walk magnitude
}
transformed data {
}
parameters {
  vector[K] betas;       // covariates
  vector<lower = 0, upper = 1>[N] zetas;       //spatial kernel, but this can't be less than 0
  
  vector[T] beta_random_walk_steps; //We add in a random walk error term
  real<lower=0> sqrtQ; //Standard deviation of random walk
  
  real<lower  = 0, upper = 1> susc_scaling; //parameter for scaling the number of first episodes so far for aqcquired immunity
}
transformed parameters {
vector[T] beta_random_walk  = cumulative_sum(beta_random_walk_steps);
}
model {

y[,1] ~ poisson_log(log((susc_scaling*susceptible_proxy[,1]).*(E[,1] + (zetas .*E_neighbours[,1]))) + x[1] * betas + (beta_random_walk[1]) );  // extra noise removed removed: + theta[,i]
target += -penalty_term*fabs(beta_random_walk[1]);
for(i in 2:T){
  y[,i] ~ poisson_log(log((susc_scaling*susceptible_proxy[,i]).*(E[,i] + (zetas .*E_neighbours[,i]))) + x[i] * betas + (beta_random_walk[i]) );  // extra noise removed removed: + theta[,i]
  target += -penalty_term*fabs(beta_random_walk[i]);
}

  betas ~ normal(0.0, 1.0);
  zetas ~ normal(0.05, 1.0);
  
  sqrtQ ~ gamma(1,random_walk_prior);
  susc_scaling ~ beta(1,1);

  for(i in 1:T){
  beta_random_walk_steps[i] ~ normal(0, sqrtQ);
  }

}
generated quantities {
}
"
  } else {
    Stan_model_string_neighbours = "
data {
  int<lower=0> N; // Number of areas
  int<lower=0> T; // Number of timepoints


  int<lower=0> y[N,T];              // count outcomes (next week's cases)
  matrix<lower=0>[N,T] E;           // exposure (current week's cases)
  matrix<lower=0>[N,T] E_neighbours; // exposure (mean current week's cases for neighbours)
  int<lower=1> K;                 // num covariates 
  matrix[N, K] x[T];                 // design matrix
  
  matrix<lower=0,upper=1>[N,T] susceptible_proxy; //Factor in a rough metric of how many susceptibles in the population

  real random_walk_prior; //What prior value to use in the prior distribution for the random walk sd
  real penalty_term; // Penalty applied to the random_walk magnitude
}
transformed data {
}
parameters {
  vector[K] betas;       // covariates
  vector<lower = 0, upper = 1>[N] zetas;       //spatial kernel, but this can't be less than 0
  
  vector[T] beta_random_walk_steps; //We add in a random walk error term
  real<lower=0> sqrtQ; //Standard deviation of random walk
  
  real<lower  = 0, upper = 1> susc_scaling; //parameter for scaling the number of first episodes so far for aqcquired immunity
}
transformed parameters {
vector[T] beta_random_walk  = cumulative_sum(beta_random_walk_steps);
}
model {

y[,1] ~ poisson_log(log((susceptible_proxy[,1]).*(E[,1] + (zetas .*E_neighbours[,1]))) + x[1] * betas + (beta_random_walk[1]) );  // extra noise removed removed: + theta[,i]
target += -penalty_term*fabs(beta_random_walk[1]);
for(i in 2:T){
  y[,i] ~ poisson_log(log((susceptible_proxy[,i]).*(E[,i] + (zetas .*E_neighbours[,i]))) + x[i] * betas + (beta_random_walk[i]) );  // extra noise removed removed: + theta[,i]
  target += -penalty_term*fabs(beta_random_walk[i]);
}

  betas ~ normal(0.0, 1.0);
  zetas ~ normal(0.05, 1.0);
  
  sqrtQ ~ gamma(1,random_walk_prior);

  for(i in 1:T){
  beta_random_walk_steps[i] ~ normal(0, sqrtQ);
  }

}
generated quantities {
}
"
  }
}
#######################################################################################################################
######################################################################################################################


T <- final_week-1;
#Note, 306 is after cutting the 21 welsh, and 29 scottish. JUST ENGLISH CODES
N <- 306;
y <- array(0, dim = c(T,N))
E <- array(0, dim = c(T,N))
#K is current number of covariates
#K <- 26; Was 26 but changed to 21 for sgtf variants
K <- 21
x <- array(0, dim = c(T, N,K))

scale_by_recent_cases <- array(0, dim = c(T,N))

for(i in 2:final_week){
  j <- i-1
  
  Reduced_Data <- filter(Case_Rates_Data, Week == i)
  Reduced_Data <- Reduced_Data[order(Reduced_Data$INDEX),]
  
  if(cases_type == "Dashboard"){
    y[j,] = Reduced_Data$next_week_cases;
    E[j,] = pmax(Reduced_Data$Week_Cases, 0.000001);
  } else if(cases_type == "Linelist"){
    y[j,] = Reduced_Data$Linelist_P2_PCR_Next_Week_Cases;
    E[j,] = pmax(Reduced_Data$Linelist_P2_PCR_Week_Cases, 0.000001);
  } else{
    stop("Unrecognised cases data type, must be Dashboard or Linelist")
  }
  
  scale_by_recent_cases[j,] = Reduced_Data$First_Episodes_Total/Reduced_Data$Population
  
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

#################################################################################


stanfit = stan(model_code = Stan_model_string_neighbours,
               data=list(N=N,T=T,
                         y=t(y),
                         x=x, K=K,
                         E=t(E),
                         E_neighbours = E_neighbours_scaled,
                         susceptible_proxy = susceptible_proxy,
                         random_walk_prior = random_walk_prior_scale,
                         penalty_term = rw_penalty),
               warmup=warmup_iterations, iter=total_iterations,
               control = list(max_treedepth = tree_depth));

#Make folder for outputs
dir.create("Outputs")

save(stanfit, file = 'Outputs/stanfit.RData')
save(N,T,y,x,K,E,E_neighbours_scaled, susceptible_proxy, 
       Case_Rates_Data, file = "Outputs/model_data.RData")

