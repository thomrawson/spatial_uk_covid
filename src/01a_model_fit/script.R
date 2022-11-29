#This script reads in the Case data as provided in task 00, runs a stan model,
#and outputs the stanfit object for future use.

###########################################################################
#Load the cleaned and prepared case/covariate data
load('Cases_Data.RData')
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
#Prepare Data

if(covariates == "default"){
  
  #Cut down to the only covariates we're interested in:
  Case_Rates_Data <- Case_Rates_Data[,-c(4,6,15,18,19,21,22,24,25,26,27,28,
                                         37)] #Cut Omicron BQ1
  
  
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
  
  
#We need to quickly clean up an issue with the Residential mobility
#TODO: Move this to the 00 task.
  #There are 24 occassions where the residential mobility is NA,
  #19 of these are Rutland

#For Rutland, we're going to take the average of it's neighbors mobility score for these days:
#Rutland is index 97
which(W[97,] == 1)
#Has four neighboring regions, 117, 189, 190, 195
Rutland_neighbors <- c(117, 189, 190, 195)
for(i in 1:length(missing_data$areaCode)){
  if(missing_data$areaName[i] == "Rutland"){
    Week_hold <- missing_data$Week[i]
  }
}


###################
#Begin STAN fit



#Define model

Stan_model_string = "
data {
  int<lower=0> N; // Number of areas
  int<lower=0> T; // Number of timepoints
  //int<lower=0> N_edges; //Number of edges
  //int<lower=1, upper=N> node1[N_edges];  // node1[i] adjacent to node2[i]
  //int<lower=1, upper=N> node2[N_edges];  // and node1[i] < node2[i]

  int<lower=0> y[N,T];              // count outcomes
  matrix<lower=0>[N,T] E;           // exposure (previous days cases)
  matrix<lower=0>[N,T] E_neighbours; // exposure (mean previous days cases of neighbours)
  int<lower=1> K;                 // num covariates 
  //matrix[N, K] x;                 // design matrix
  //matrix[7, 2] mu[15, 12];  declares a 15 by 12 array of  7Ã—2 matrices.
  matrix[N, K] x[T];                 // design matrix

}
transformed data {
  //matrix[N,T] log_E = log(E + E_neighbours);
}
parameters {
  real beta0;            // intercept
  vector[K] betas;       // covariates
  vector<lower = 0>[N] zetas;       //spatial kernel, but this can't be less than 0

  //matrix[N,T] theta;       // heterogeneous effects
  //real theta;       // heterogeneous effects
  vector[N] theta;       // heterogeneous effects
}
transformed parameters {

}
model {
for(i in 1:T){
  y[,i] ~ poisson_log(log(E[,i] + zetas .*E_neighbours[,i]) + beta0 + x[i] * betas + theta);  // extra noise removed removed: + theta[,i]
}

  beta0 ~ normal(0.0, 1.0);
  betas ~ normal(0.0, 1.0);
  //zetas ~ normal(0.05, 1.0);
  zetas ~ gamma(0.05, 1.0);
  
  //Unsure just HOW MANY noise terms we should incorporate. Let's try with just one to start with
  //for(i in 1:T){
  //theta[,i] ~ normal(0.0, 1.0);
  //}
  theta ~ normal(0.0, 1.0);
  
}
generated quantities {
  //matrix[N,T] eta = log(E[,i] + zetas .*(Cases_Nbors[,i]./N_Nbors)) + beta0 + x[i] * betas + theta[,i]
  
  //matrix[N,T] mu = exp(eta);
  
}
"


#Weeks go from 2:130
#Note, the government stopped providing free LFTs on April 1st 2022
#This will have, in turn, affected the case data, so let's chop off everything
#after week 104 (w/b 25/04/22)
#TODO: Could include still but keep this as a variable
final_week <- 104
Case_Rates_Data <- filter(Case_Rates_Data, Week < final_week+1)

T <- final_week-1;
#Note, 306 is after cutting the 21 welsh, and 29 scottish. JUST ENGLISH CODES
N <- 306;
y <- array(0, dim = c(T,N))
E <- array(0, dim = c(T,N))
#K is current number of covariates
K <- 10;
x <- array(0, dim = c(T, N,K))


#Now we smooth out the mobility means for when there's NaN
#TODO: Think of a better way to do this tbh...
for(i in 2:final_week){
  j <- i-1
  
  Reduced_Data <- filter(Case_Rates_Data, Week == i)
  Reduced_Data <- Reduced_Data[order(Reduced_Data$INDEX),]
  
  #Note that Rutland is often NaN
  if(sum(is.na(Reduced_Data$Residential_Mobility))>0){
    #Then set it to the average 
    #set <- 1:356
    #set <- set[-97]
    dropped_data <- drop_na(Reduced_Data)
    Reduced_Data$Residential_Mobility[is.na(Reduced_Data$Residential_Mobility)] <- mean(dropped_data$Residential_Mobility)
  }
  
  
  #There are 21 Welsh codes
  #There are 29 Scottish codes
  #Dropping these 50 leaves us with 306 English rows
  Reduced_Data <- drop_na(Reduced_Data)
  
  
  y[j,] = Reduced_Data$Week_Cases;
  E[j,] = Reduced_Data$previous_week_cases;
  
  x[j,,1] <- Reduced_Data$cumVaccPercentage_FirstDose/100
  x[j,,2] <- Reduced_Data$cumVaccPercentage_SecondDose/100
  x[j,,3] <- Reduced_Data$cumVaccPercentage_ThirdDose/100
  x[j,,4] <- Reduced_Data$prop_white_british
  x[j,,5] <- Reduced_Data$IMD_Average_score/100   #(Highest value is 45.039, should probably scale to this really, should check what this measure means literally)
  x[j,,6] <- Reduced_Data$resident_earnings/1000 #(highest 912.9) #TODO: Change to MEDIAN
  x[j,,7] <- Reduced_Data$mean_age/100 #(highest is 47.50081)
  #x[j,,7] <- Reduced_Data$prop_o65
  x[j,,8] <- Reduced_Data$mean_popden/10000   #(highest is 16426.98)
  x[j,,9] <- Reduced_Data$Residential_Mobility/100  #(highest is 31.2)
  x[j,,10] <- Reduced_Data$Delta_proportion/100   #(highest is 99.99)
  
}

W_reduced <- W[Reduced_Data$INDEX, Reduced_Data$INDEX]
#Calculate E_neighbours
E_neighbours <- W_reduced%*%t(E)
Nbors <- as.numeric(rowSums(W_reduced))
E_neighbours_scaled <- E_neighbours/Nbors

stanfit = stan(model_code = Stan_model_string,
               data=list(N=N,T=T,
                         y=t(y),
                         x=x, K=K,
                         E=t(E),
                         E_neighbours = E_neighbours_scaled),
               warmup=warmup_iterations, iter=total_iterations,
               control = list(max_treedepth = tree_depth));


#Make folder for outputs
dir.create("Outputs")

save(stanfit, file = 'Outputs/stanfit.RData')

