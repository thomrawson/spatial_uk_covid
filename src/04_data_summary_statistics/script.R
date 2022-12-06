#Run some summary statistics and output some summary plots of the informing dataset

#Load the data
load("Cases_Data.RData")
load("W.RData")

#Clean-up in the same way done in the 01a_model_fit task
covariates <- "default"
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
      Neighbor_hold <- filter(Case_Rates_Data, Week == Week_hold)
      Neighbor_hold <- filter(Neighbor_hold, INDEX %in% Rutland_neighbors)
      residential_mean_hold <- mean(Neighbor_hold$residential_percent_change_from_baseline)
      
      Case_Rates_Data$residential_percent_change_from_baseline[which((Case_Rates_Data$areaName == "Rutland")&(Case_Rates_Data$Week == Week_hold))] <- residential_mean_hold
      
    }
  }
  
  missing_data <- Case_Rates_Data[which(is.na(Case_Rates_Data$residential_percent_change_from_baseline)),]
  unique(missing_data$areaCode)
  
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
}


#start by generating some comparison and correlation plots:
#We trim off the columns we don't need
Case_Rates_Data %>%
  select(-areaCode, -date_begin,
         -INDEX, -previous_week_cases,
         -next_week_cases, 
         #-resident_earnings, -no_jobs, -prop_travelling_to_work,
         #-retail_and_recreation_percent_change_from_baseline,
         #-grocery_and_pharmacy_percent_change_from_baseline,
         #-parks_percent_change_from_baseline,
         #-mean_popden, 
         -Delta_AY_4_2_proportion,
         -Omicron_BA_1_proportion, -Omicron_BA_2_proportion,
         #-Omicron_BQ_1_proportion, 
         -Omicron_BA_4_proportion,
         -Omicron_BA_5_proportion
         ) -> Reduced_Data

area_names <- unique(Case_Rates_Data$areaName)

#It's too large to plot all at once, let's do one area at a time
Reduced_Data %>%
  filter(areaName == area_names[2]) %>%
  select(-areaName) %>%
  GGally::ggpairs()
