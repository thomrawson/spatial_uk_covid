#orderly::orderly_develop_start("13_acquired_immunity_estimate", use_draft = "newer", parameters = list(tree_depth = 15, total_iterations = 4000, n_chains = 16))

#This script loads in the Case Rates data object and tries to build an estimate for acquired immunity
#For use in the counterfactuals
load("model_data.RData")

#I want to try fitting a logistic growth with offset I think for the first episodes
Case_Rates_Data %>%
  select(areaName, areaCode, Population, Week, Week_Cases, First_Episodes_Total) -> logistic_data

logistic_data$Cases_Total <- 0

LTLA_names <- unique(logistic_data$areaName)
AllWeeks <- sort(unique(logistic_data$Week))

for(i in 1:length(LTLA_names)){
  areaName_hold <- LTLA_names[i] 
  
  hold_data <- logistic_data[which(logistic_data$areaName == areaName_hold),]
  first_episodes_hold <- min(hold_data$First_Episodes_Total)
  hold_data$Week_Cases[which(hold_data$Week == 2)] -> first_cases_hold
  
  logistic_data$First_Episodes_Total[which(logistic_data$areaName == areaName_hold)] <- logistic_data$First_Episodes_Total[which(logistic_data$areaName == areaName_hold)] - first_episodes_hold + first_cases_hold
  
  for(j in 1:length(AllWeeks)){
    
    weekHold <- AllWeeks[j]
    if(j == 1){
      logistic_data$Cases_Total[which(logistic_data$areaName == areaName_hold & logistic_data$Week == weekHold)] <- logistic_data$Week_Cases[which(logistic_data$areaName == areaName_hold & logistic_data$Week == weekHold)]
    }else{
      logistic_data$Cases_Total[which(logistic_data$areaName == areaName_hold & logistic_data$Week == weekHold)] <- (logistic_data$Cases_Total[which(logistic_data$areaName == areaName_hold & logistic_data$Week == (weekHold-1))] + logistic_data$Week_Cases[which(logistic_data$areaName == areaName_hold & logistic_data$Week == weekHold)] )
    }
    
  }
  
}


#The above gives us now a running tally of total cases, and a running tally of first_episodes
#Let's also calculate the weekly new first episodes
logistic_data$Week_First_Episodes <- NA
for( i in 1:length(logistic_data$areaCode)){
  areaName_hold <- logistic_data$areaName[i]
  weekHold <- logistic_data$Week[i]
  
  if(weekHold == 2){
    logistic_data$Week_First_Episodes[i] <- logistic_data$First_Episodes_Total[i]
  }else{
    reduced_data <- filter(filter(logistic_data, areaName == areaName_hold), Week == (weekHold-1))
    logistic_data$Week_First_Episodes[i] <- logistic_data$First_Episodes_Total[i] - reduced_data$First_Episodes_Total
  }
  
}


#Now, my idea is that, for each new week there is a probability of a case being a first episode, or a new case,
# first_episodes[t] =  (new_cases[t] * (first_episodes_total[t-1]/Population))*FACTOR

#We just want to find that factor.

FACTOR_data <- data.frame(FACTOR_value = seq(0.5,1.5, length.out = 1000),
                          error_value = rep(0,1000))

for(i in 1:length(FACTOR_data$FACTOR_value)){
  error_hold <- sum(abs(logistic_data$Week_First_Episodes - (logistic_data$Week_Cases*min((1-(logistic_data$First_Episodes_Total/logistic_data$Population))*FACTOR_data$FACTOR_value[i],1))))
  FACTOR_data$error_value[i] <- error_hold
}
plot(FACTOR_data$FACTOR_value, FACTOR_data$error_value)
FACTOR_data[which(FACTOR_data$error_value == min(FACTOR_data$error_value)),]
#The lowest error seems to be factor = 1.46
#Let's quickly plot some of that to see how it looks.


FACTOR_data <- data.frame(FACTOR_value = seq(0.5,1.5, length.out = 1000),
                          error_value = rep(0,1000))

for(k in 1:length(FACTOR_data$FACTOR_value)){


for(i in 1:length(LTLA_names)){
  areaName_hold <- LTLA_names[i]
  
  Reduced_Data <- filter(logistic_data, areaName == areaName_hold)
  
  Reduced_Data$Fit_First_Episodes <- NA
  
  for(j in 1:length(AllWeeks)){
    weekHold <- AllWeeks[j]
    
    if(weekHold == 2){
      Reduced_Data$Fit_First_Episodes[which(Reduced_Data$Week == weekHold)] <- Reduced_Data$First_Episodes_Total[which(Reduced_Data$Week == weekHold)]
    }
    else{
      Reduced_Data$Fit_First_Episodes[which(Reduced_Data$Week == weekHold)] <- Reduced_Data$Week_Cases[which(Reduced_Data$Week == weekHold)]*min(FACTOR_data$FACTOR_value[k]*(1-(Reduced_Data$First_Episodes_Total[which(Reduced_Data$Week == weekHold)]/Reduced_Data$Population[1])),1)
    }
    
    
    
  }
  
  if(i == 1){
    Plot_data <- Reduced_Data
  }else{
    Plot_data <- rbind(Plot_data, Reduced_Data)
  }
  
  
}
  #print(k)
  FACTOR_data$error_value[k]  <- sum(abs(Plot_data$Fit_First_Episodes - Plot_data$Week_First_Episodes)/Plot_data$Population)
}

FACTOR_data[which(FACTOR_data$error_value == min(FACTOR_data$error_value)),]

best_factor <- FACTOR_data$FACTOR_value[which(FACTOR_data$error_value == min(FACTOR_data$error_value))]

plot(FACTOR_data$FACTOR_value, FACTOR_data$error_value)
#1.1667 looks best if not scaled by population
#If scaled by population (i.e. don't just make birmingham fit best: 1.168

for(i in 1:length(LTLA_names)){
  areaName_hold <- LTLA_names[i]
  
  Reduced_Data <- filter(logistic_data, areaName == areaName_hold)
  
  Reduced_Data$Fit_First_Episodes <- NA
  
  for(j in 1:length(AllWeeks)){
    weekHold <- AllWeeks[j]
    
    if(weekHold == 2){
      Reduced_Data$Fit_First_Episodes[which(Reduced_Data$Week == weekHold)] <- Reduced_Data$First_Episodes_Total[which(Reduced_Data$Week == weekHold)]
    }
    else{
      Reduced_Data$Fit_First_Episodes[which(Reduced_Data$Week == weekHold)] <- Reduced_Data$Week_Cases[which(Reduced_Data$Week == weekHold)]*min(best_factor*(1-(Reduced_Data$First_Episodes_Total[which(Reduced_Data$Week == weekHold)]/Reduced_Data$Population[1])),1)
    }
    
    
    
  }
  
  if(i == 1){
    Plot_data <- Reduced_Data
  }else{
    Plot_data <- rbind(Plot_data, Reduced_Data)
  }
  
  
}

Plot_data2 <- select(Plot_data, areaName, Week, Week_First_Episodes, Fit_First_Episodes)
#elongate
Plot_data2 <- Plot_data2 %>% 
  pivot_longer(
    cols = 3:4, 
    names_to = "Scenario",
    values_to = "First_Episodes"
  )

#Now we want to plot how good the fit is
dir.create("First_Episodes_Total_fits")

for(i in 1:length(LTLA_names)){
  areaName_hold <- LTLA_names[i]
  
  Reduced_Data <- filter(Plot_data2, areaName == areaName_hold)
  
  ggplot() +
    geom_line(data = Reduced_Data, aes(x = Week, y = First_Episodes, color = Scenario), alpha = 0.9, size = 1) +
    ggtitle(sprintf("First Episodes in %s - factor of %s", areaName_hold, best_factor)) -> plot_hold
  
  png(file=sprintf("First_Episodes_Total_fits\\model_fit_%s.png", areaName_hold),
      width=1440, height=1080, res = 150)
  plot(plot_hold)
  dev.off()
  
}
