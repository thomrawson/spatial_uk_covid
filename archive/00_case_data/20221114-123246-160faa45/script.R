
#Make folder for outputs
dir.create("Outputs")

#Switch to say if we want to record the previous day's cases or not
record_previous_day_cases <- FALSE

#Start by loading information on UK LTLA region boundaries from shape file
Boundaries <- st_read("Data/district_borough_unitary_region.shp")



#Now load daily new COVID cases data at LTLA level
#Originally this was pulled from the government portal API:
#Case_Rates_Data<- read.csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDateRollingSum&format=csv')

#But in the interest of re-producibility, we use the hard-saved pull from this API
#Current source pulled November 10th 2022
Case_Rates_Data <- read.csv('Data/ltla_cases_10_11_2022.csv')

#Ensure date is being read as a date
Case_Rates_Data$date <- as.Date(Case_Rates_Data$date)

#Load vaccine uptake proportions
#These are the live API links used
#First_Vaccine_Data <- read.csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=cumVaccinationFirstDoseUptakeByVaccinationDatePercentage&format=csv')
#Second_Vaccine_Data <- read.csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=cumVaccinationSecondDoseUptakeByVaccinationDatePercentage&format=csv')
#Third_Vaccine_Data <- read.csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=cumVaccinationThirdInjectionUptakeByVaccinationDatePercentage&format=csv')

#Similarly to above though, we use the data accessed on November 10th 2022
First_Vaccine_Data <- read.csv('Data/ltla_cum_1dose_vaccine_percentage_10_11_2022.csv')
Second_Vaccine_Data <- read.csv('Data/ltla_cum_2dose_vaccine_percentage_10_11_2022.csv')
Third_Vaccine_Data <- read.csv('Data/ltla_cum_3dose_vaccine_percentage_10_11_2022.csv')

#Assign a sensible variable name
names(First_Vaccine_Data) <- c('areaCode', 'areaName', 'areaType', 'date', 'cumVaccPercentage_FirstDose')
names(Second_Vaccine_Data) <- c('areaCode', 'areaName', 'areaType', 'date', 'cumVaccPercentage_SecondDose')
names(Third_Vaccine_Data) <- c('areaCode', 'areaName', 'areaType', 'date', 'cumVaccPercentage_ThirdDose')

#Ensure date is date-type
First_Vaccine_Data$date <- as.Date(First_Vaccine_Data$date)
Second_Vaccine_Data$date <- as.Date(Second_Vaccine_Data$date)
Third_Vaccine_Data$date <- as.Date(Third_Vaccine_Data$date)

#Merge the Case rates data with the vaccinations data
Case_Rates_Data <- merge(Case_Rates_Data, First_Vaccine_Data, by = c('areaCode','date'), all.x = TRUE)
Case_Rates_Data <- Case_Rates_Data[,-c(6,7)]

Case_Rates_Data <- merge(Case_Rates_Data, Second_Vaccine_Data, by = c('areaCode','date'), all.x = TRUE)
Case_Rates_Data <- Case_Rates_Data[,-c(7,8)]

Case_Rates_Data <- merge(Case_Rates_Data, Third_Vaccine_Data, by = c('areaCode','date'), all.x = TRUE)
Case_Rates_Data <- Case_Rates_Data[,-c(8,9)]

#Remove the Northern Ireland data:
Case_Rates_Data <- filter(Case_Rates_Data, !grepl('N09', areaCode))

#Now we need to deal with some discrepancies between the areaCodes in our case data, and those in our Boundaries data

#All area codes in data: 369
Case_Data_Codes <- unique(Case_Rates_Data$areaCode)
#All polygon codes: 363
Boundary_Codes <- unique(Boundaries$CODE)

sum(Boundary_Codes %in% Case_Data_Codes)
#5 of the boundary codes are not in Case data - Let's list which:

index <- 1
Not_in_Case_Rates1 <- rep(NA,5)
Not_in_Case_Rates2 <- rep(NA,5)
for( i in 1:length(Boundary_Codes)){
  if(Boundary_Codes[i] %in% Case_Data_Codes){
    
  } else {
    Not_in_Case_Rates1[index] <- Boundaries$NAME[i]
    Not_in_Case_Rates2[index] <- Boundary_Codes[i]
    index <- index + 1
  }
}

Not_in_Case_Rates1
Not_in_Case_Rates2
#Case rates does not have;
#West Northamptonshire
#Isles of Scilly
#North Northamptonshire
#Buckinghamshire
#City and County of the City of London

sum(Case_Data_Codes %in% Boundary_Codes)
#And 11 in cases codes are not in boundary codes

index <- 1
Not_in_Boundaries1 <- rep(NA,11)
Not_in_Boundaries2 <- rep(NA,11)
for( i in 1:length(Case_Data_Codes)){
  if(Case_Data_Codes[i] %in% Boundary_Codes){
    
  } else {
    #Not_in_Boundaries1[index] <- Boundaries$NAME[i]
    Not_in_Boundaries2[index] <- Case_Data_Codes[i]
    index <- index + 1
  }
}

for(i in 1:length(Not_in_Boundaries2)){
  Not_in_Boundaries1[i] <- Case_Rates_Data$areaName[which(Case_Rates_Data$areaCode == Not_in_Boundaries2[i])[1]]
}

Not_in_Boundaries1
Not_in_Boundaries2

#We first combine Hackney and City of London boundaries, as is done for case data


#This will change the polygon for Hackney to be a polygon for hackney AND city of london
Boundaries$geometry[320] <- st_cast(
  st_union(Boundaries[c(which(grepl('Hackney', Boundaries$NAME)), which(grepl('City of London', Boundaries$NAME))),]) 
  , "MULTIPOLYGON")

#Now let's remove City of London from Boundaries
Boundaries <- Boundaries[-324,]


#Now we do this for Scilly too
#We want to add Scilly boundaries to the Cornwall boundaries.
Boundaries$geometry[120] <- st_cast(
  st_union(Boundaries[c(which(grepl('Cornwall', Boundaries$NAME)), which(grepl('Scilly', Boundaries$NAME))),]) 
  , "MULTIPOLYGON")

#Now remove Isles of Scilly 
Boundaries <- Boundaries[-121,]

#Let's see what's left
#All area codes in data: 369
Case_Data_Codes <- unique(Case_Rates_Data$areaCode)
#All polygon codes: 361
Boundary_Codes <- unique(Boundaries$CODE)

sum(Boundary_Codes %in% Case_Data_Codes)

index <- 1
Not_in_Case_Rates1 <- rep(NA,3)
Not_in_Case_Rates2 <- rep(NA,3)
for( i in 1:length(Boundary_Codes)){
  if(Boundary_Codes[i] %in% Case_Data_Codes){
    
  } else {
    Not_in_Case_Rates1[index] <- Boundaries$NAME[i]
    Not_in_Case_Rates2[index] <- Boundary_Codes[i]
    index <- index + 1
  }
}

Not_in_Case_Rates1
Not_in_Case_Rates2

Not_in_Boundaries1
Not_in_Boundaries2
#So, there are eleven regions which the Case rates data list, which are actually made up of
#three regions in the Boundaries data:
#Buckinghamshire = South Bucks, Aylesbury Vale, Chiltern, Wycombe, 
#West Northamptonshire = Northampton , South Northamptonshire, Daventry
#North Northamptonshire = East Northamptonshire (weirdly, I think), Wellingborough, Corby, Kettering

#So we collapse  those case locations into the broader boundaries.

#First save these areas in case wanted later:
Old_Cases <- filter(Case_Rates_Data, areaCode %in% Not_in_Boundaries2)

areaChanges <- data.frame(from_code = Not_in_Boundaries2, from_name = Not_in_Boundaries1,
                          to_name = c('Buckinghamshire', 'Buckinghamshire', 'Buckinghamshire',
                                      "Buckinghamshire", "North Northamptonshire", "West Northamptonshire",
                                      "North Northamptonshire", "North Northamptonshire", "West Northamptonshire",
                                      'West Northamptonshire', "North Northamptonshire"),
                          to_code = c("E06000060", "E06000060", "E06000060",
                                      "E06000060", "E06000061", "E06000062",
                                      "E06000061", "E06000061", "E06000062",
                                      "E06000062", "E06000061"))

#Now change the areaCode and areaName:
for(i in 1:length(Case_Rates_Data[,1])){
  if(Case_Rates_Data$areaCode[i] %in% Not_in_Boundaries2){
    index <- which(areaChanges$from_code == Case_Rates_Data$areaCode[i])
    Case_Rates_Data$areaCode[i] <- areaChanges$to_code[index]
    Case_Rates_Data$areaName[i] <- areaChanges$to_name[index]
  }
}

New_Cases <- filter(Case_Rates_Data, areaCode %in% Not_in_Case_Rates2)
Case_Rates_Data <- Case_Rates_Data[,-c(3,4)]

#Now we aggregate these by date and code:
Agg_Case_Rates_Data <- aggregate(Case_Rates_Data$newCasesBySpecimenDate,
                                 by=list(date=Case_Rates_Data$date, 
                                         areaCode = Case_Rates_Data$areaCode, 
                                         areaName = Case_Rates_Data$areaName),
                                 FUN=sum)

#We need to calculate the "mean" vaccination rate across these three agglomerated regions
#TODO: NEED TO DO THE MEAN VACC RELATIVE TO POPULATION.
First_Vacc_Mean_Data <- aggregate(Case_Rates_Data$cumVaccPercentage_FirstDose,
                            by=list(date=Case_Rates_Data$date,
                                    areaCode = Case_Rates_Data$areaCode,
                                    areaName = Case_Rates_Data$areaName),
                            FUN=mean)

Second_Vacc_Mean_Data <- aggregate(Case_Rates_Data$cumVaccPercentage_SecondDose,
                                  by=list(date=Case_Rates_Data$date,
                                          areaCode = Case_Rates_Data$areaCode,
                                          areaName = Case_Rates_Data$areaName),
                                  FUN=mean)

Third_Vacc_Mean_Data <- aggregate(Case_Rates_Data$cumVaccPercentage_ThirdDose,
                                  by=list(date=Case_Rates_Data$date,
                                          areaCode = Case_Rates_Data$areaCode,
                                          areaName = Case_Rates_Data$areaName),
                                  FUN=mean)

New_Cases2 <- filter(Agg_Case_Rates_Data, areaCode %in% Not_in_Case_Rates2)

#Now we no longer have any overlap

rm(areaChanges, New_Cases, New_Cases2, Old_Cases,
   Not_in_Boundaries1, Not_in_Boundaries2,
   Not_in_Case_Rates1, Not_in_Case_Rates2)

Agg_Case_Rates_Data$previous_day_cases <- NA
colnames(Agg_Case_Rates_Data) <- c('date', 'areaCode', 'areaName','Cases', 'previous_day_cases')
colnames(First_Vacc_Mean_Data) <- c('date', 'areaCode', 'areaName','cumVaccPercentage_FirstDose')
colnames(Second_Vacc_Mean_Data) <- c('date', 'areaCode', 'areaName','cumVaccPercentage_SecondDose')
colnames(Third_Vacc_Mean_Data) <- c('date', 'areaCode', 'areaName','cumVaccPercentage_ThirdDose')

Case_Rates_Data <- merge(Agg_Case_Rates_Data, First_Vacc_Mean_Data, by = c('areaCode', 'areaName', 'date'))
Case_Rates_Data <- merge(Case_Rates_Data, Second_Vacc_Mean_Data, by = c('areaCode', 'areaName', 'date'))
Case_Rates_Data <- merge(Case_Rates_Data, Third_Vacc_Mean_Data, by = c('areaCode', 'areaName', 'date'))

Case_Rates_Data$date <- as.Date(Case_Rates_Data$date)

#Now we record, for each LTLA, the case count from the previous day in the row too.
#This takes a long time, there's probably a...
#TODO: Make this way more efficient!

if(record_previous_day_cases){

for(i in 1: length(Case_Rates_Data$date)){
  areaName_hold <- Case_Rates_Data$areaName[i]
  date_hold <- Case_Rates_Data$date[i]
  
  #Obtain the previous day's case_Rate
  data_hold <- filter(Case_Rates_Data, areaName == areaName_hold)
  data_hold <- filter(data_hold, date == date_hold - 1)
  
  if(length(data_hold$areaCode) == 0){
    
  } else if (length(data_hold$areaCode) == 1){
    Case_Rates_Data$previous_day_cases[i] <- data_hold$Cases
  } else {
    print(paste('Error: length neither 0 nor 1 at i =', i, sep = " "))
  }
  
}
  
}

#save(Case_Rates_Data, file = 'Case_Date_26_10.RData')

#We choose to cut everything before April 30th 2020, as case data was still pretty
#flimsy before then (not sufficient testing), can always change this though...
minimum_date <- "2020-04-30"
Case_Rates_Data$date <- as.Date(Case_Rates_Data$date)
Case_Rates_Data <- filter(Case_Rates_Data, date > as.Date(minimum_date))

#We also need the population of each place.
LTLA_Demog <- read.csv('Data/LTLA_demog.csv')
#This contains, for each LTLA, the population, area, pop. density, and median age

#Attach this demographic data to the case_rates dataframe
#TODO: Make this more efficient!
Case_Rates_Data$Population <- NA
Case_Rates_Data$Pop_per_km2 <- NA
Case_Rates_Data$Median_age <- NA

for(i in 1:length(Case_Rates_Data$date)){
  hold_code <- Case_Rates_Data$areaCode[i]
  data_hold <- filter(LTLA_Demog, areaCode == hold_code)
  Case_Rates_Data$Population[i] <- data_hold$Population
  Case_Rates_Data$Pop_per_km2[i] <- data_hold$Pop_per_km2
  Case_Rates_Data$Median_age[i] <- data_hold$Median_age
}

#Remove the unnecessary vaccine dataframes
rm(Agg_Case_Rates_Data, First_Vacc_Mean_Data, First_Vaccine_Data,
   Second_Vacc_Mean_Data, Second_Vaccine_Data, 
   Third_Vacc_Mean_Data, Third_Vaccine_Data)

#There's a modeling problem in cases when places have no boundaries
#TODO: This *could* be fixed by defining custom boundaries, but for now we remove.
#issues at:
#102 - isle of wight
#103 - isle of anglesey
#112 - Orkney islands
#119 - Shetland Islands
#124 - Na h-Eileanan an Iar
Error_indices <- c(102, 103, 112, 119, 124)
Error_codes <- Boundaries$CODE[Error_indices]

#Remove from Boundaries and from case data
Boundaries <- filter(Boundaries, !(CODE %in% Error_codes))
Case_Rates_Data <- filter(Case_Rates_Data, !(areaCode %in% Error_codes))

#Define a column that will say which element of the spatial matrix each row relates to
Case_Rates_Data$INDEX <- NA
for(i in 1:length(Case_Rates_Data$date)){
  hold_code <- Case_Rates_Data$areaCode[i]
  Case_Rates_Data$INDEX[i] <- which(Boundaries$CODE == hold_code)
}

#Build weight matrix - defining if two LTLAs are connected or not

col_sp <- as(Boundaries, "Spatial")
col_nb <- poly2nb(col_sp) # queen neighborhood
col_listw <- nb2listw(col_nb, style = "B", zero.policy = TRUE) # listw version of the neighborhood
W <- nb2mat(col_nb, style = "B", zero.policy = TRUE) # binary structure

W <- W * 1L
#Define D as the total number of neighbours for each LTLA
D <- diag(rowSums(W))

#######################
#That's all we need, now we export:
#Case_Rates_Data
#Boundaries
#W
#D
#save(Case_Rates_Data, Boundaries, W, D, file = 'Outputs/00_output_12_01_22.RData')
#######################


##################################################################
##      WEEKLY
##################################################################
#We want to convert to using the weekly sum of cases

#Start by summing together the weekly cases
#Start by adding a new variable, week number:

#FIRST DATE: Monday 4th May 2020
Case_Rates_Data$Week <- plyr::round_any(as.numeric(Case_Rates_Data$date - as.Date('2020-05-04') + 0.5), 7, f = ceiling)/7

#Filter out the week 0 cases
Case_Rates_Data <- filter(Case_Rates_Data, Week > 0)

#Now sum by, everything, but not vaccination yet
Case_Rates_Data_hold <- aggregate(Case_Rates_Data$Cases, by=list(Week=Case_Rates_Data$Week, areaCode = Case_Rates_Data$areaCode,
                                                                 areaName = Case_Rates_Data$areaName, Population = Case_Rates_Data$Population,
                                                                 Pop_per_km2 = Case_Rates_Data$Pop_per_km2,
                                                                 Median_age = Case_Rates_Data$Median_age,
                                                                 INDEX = Case_Rates_Data$INDEX), FUN=sum)

#Now need to add a "previous week cases" column, and a "date_begin" column:
colnames(Case_Rates_Data_hold) <- c('Week', 'areaCode', 'areaName', 'Population',
                                    'Pop_per_km2', 'Median_age', 'INDEX', 'Week_Cases')

Case_Rates_Data_hold$date_begin <- as.Date('2020-05-04') + ((Case_Rates_Data_hold$Week - 1)*7) 

#Previous weeks column
Case_Rates_Data_hold$previous_week_cases <- NA


for(i in 1: length(Case_Rates_Data_hold$Week)){
  areaName_hold <- Case_Rates_Data_hold$areaName[i]
  date_hold <- Case_Rates_Data_hold$Week[i]
  
  #Obtain the previous day's case_Rate
  data_hold <- filter(Case_Rates_Data_hold, areaName == areaName_hold)
  data_hold <- filter(data_hold, Week == date_hold - 1)
  
  if(length(data_hold$areaCode) == 0){
    
  } else if (length(data_hold$areaCode) == 1){
    Case_Rates_Data_hold$previous_week_cases[i] <- data_hold$Week_Cases
  } else {
    print(paste('Error: length neither 0 nor 1 at i =', i, sep = " "))
  }
  
}

#Drop "week one", as we don't have the previous week cases.
Case_Rates_Data_hold <- drop_na(Case_Rates_Data_hold)

rm(data_hold, areaName_hold, date_hold, i)

#Now we need to add back the vaccination states for that day too. 
#We currently use the percentage on the date of week beginning but could feasibly...
#TODO: SET THIS TO THE WEEKLY MEAN VACC INSTEAD
Case_Rates_Data_hold$cumVaccPercentage_FirstDose <- NA
Case_Rates_Data_hold$cumVaccPercentage_SecondDose <- NA
Case_Rates_Data_hold$cumVaccPercentage_ThirdDose <- NA
Case_Rates_Data_hold$date_begin <- as.Date(Case_Rates_Data_hold$date_begin)

for(i in 1:length(Case_Rates_Data_hold$Week)){
  date_hold <- Case_Rates_Data_hold$date_begin[i]
  areaCode_hold <- Case_Rates_Data_hold$areaCode[i]
  
  data_filter <- filter(Case_Rates_Data, date == date_hold)
  data_filter <- filter(data_filter, areaCode == areaCode_hold)
  
  Case_Rates_Data_hold$cumVaccPercentage_FirstDose[i] <- data_filter$cumVaccPercentage_FirstDose[1]
  Case_Rates_Data_hold$cumVaccPercentage_SecondDose[i] <- data_filter$cumVaccPercentage_SecondDose[1]
  Case_Rates_Data_hold$cumVaccPercentage_ThirdDose[i] <- data_filter$cumVaccPercentage_ThirdDose[1]
}

Case_Rates_Data <- Case_Rates_Data_hold
rm(data_filter, date_hold, areaCode_hold, Case_Rates_Data_hold)


#save(Case_Rates_Data, file = 'Outputs/Weekly_Case_Date_12_01_22.RData')



#We need to investigate the NA's for vaccination:
NA_vacc_data <- filter(Case_Rates_Data, is.na(Case_Rates_Data$cumVaccPercentage_FirstDose))

#All at zero up to at least week 35,
#after that just scotland and wales missing,
#after about week 50 it's just wales missing.

#VACCINATION ASSUMPTIONs
#Step 1
#If NA and not in Wales, then set to 0

for(i in 1:length(Case_Rates_Data$Week)){
  if(!(grepl('W', Case_Rates_Data$areaCode[i]))){
    if(is.na(Case_Rates_Data$cumVaccPercentage_FirstDose[i])){
      Case_Rates_Data$cumVaccPercentage_FirstDose[i] <- 0
    }
    if(is.na(Case_Rates_Data$cumVaccPercentage_SecondDose[i])){
      Case_Rates_Data$cumVaccPercentage_SecondDose[i] <- 0
    }
    if(is.na(Case_Rates_Data$cumVaccPercentage_ThirdDose[i])){
      Case_Rates_Data$cumVaccPercentage_ThirdDose[i] <- 0
    }
  }
}

#Step 2
#Set all Welsh figures equal to the Wales national average for that day
#We use the data downloaded from API on November 11th 2022
#Wales_First_Vaccination_Data <- read.csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=W92000004&metric=cumVaccinationFirstDoseUptakeByPublishDatePercentage&format=csv')
Wales_First_Vaccination_Data <- read.csv('Data/Wales_cum_first_vacc.csv')

#Wales_Second_Vaccination_Data <- read.csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=W92000004&metric=cumVaccinationSecondDoseUptakeByPublishDatePercentage&format=csv')
Wales_Second_Vaccination_Data <- read.csv('Data/Wales_cum_second_vacc.csv')

#Wales_Third_Vaccination_Data <- read.csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=W92000004&metric=cumVaccinationThirdInjectionUptakeByPublishDatePercentage&format=csv')
Wales_Third_Vaccination_Data <- read.csv('Data/Wales_cum_third_vacc.csv')

for(i in 1:length(Case_Rates_Data$Week)){
  if((grepl('W', Case_Rates_Data$areaCode[i]))){
    if(is.na(Case_Rates_Data$cumVaccPercentage_FirstDose[i])){
        vacc_hold <- filter(Wales_First_Vaccination_Data, Wales_First_Vaccination_Data$date == Case_Rates_Data$date_begin[i])
        if(length(vacc_hold)>0){
        Case_Rates_Data$cumVaccPercentage_FirstDose[i] <- vacc_hold$cumVaccinationFirstDoseUptakeByPublishDatePercentage[1]
        }
    }
    
    if(is.na(Case_Rates_Data$cumVaccPercentage_SecondDose[i])){
      vacc_hold <- filter(Wales_Second_Vaccination_Data, Wales_Second_Vaccination_Data$date == Case_Rates_Data$date_begin[i])
      if(length(vacc_hold)>0){
        Case_Rates_Data$cumVaccPercentage_SecondDose[i] <- vacc_hold$cumVaccinationSecondDoseUptakeByPublishDatePercentage[1]
      }
    }
    
    if(is.na(Case_Rates_Data$cumVaccPercentage_ThirdDose[i])){
      vacc_hold <- filter(Wales_Third_Vaccination_Data, Wales_Third_Vaccination_Data$date == Case_Rates_Data$date_begin[i])
      if(length(vacc_hold)>0){
        Case_Rates_Data$cumVaccPercentage_ThirdDose[i] <- vacc_hold$cumVaccinationThirdInjectionUptakeByPublishDatePercentage[1]
      }
    }
    
  }
}

#Step 3 
#Everything left as NA is pre-vacc Wales, so set to 0
for(i in 1:length(Case_Rates_Data$Week)){
  if(is.na(Case_Rates_Data$cumVaccPercentage_FirstDose[i])){
    Case_Rates_Data$cumVaccPercentage_FirstDose[i] <- 0
  } 
  if(is.na(Case_Rates_Data$cumVaccPercentage_SecondDose[i])){
    Case_Rates_Data$cumVaccPercentage_SecondDose[i] <- 0
  } 
  if(is.na(Case_Rates_Data$cumVaccPercentage_ThirdDose[i])){
    Case_Rates_Data$cumVaccPercentage_ThirdDose[i] <- 0
  } 
}

rm(NA_vacc_data, vacc_hold, Wales_First_Vaccination_Data,
   Wales_Second_Vaccination_Data, Wales_Third_Vaccination_Data)

#######################
#Order the data by index
Case_Rates_Data <- Case_Rates_Data[order(Case_Rates_Data$INDEX),]

###################################################
# Affix more covariates
###################################################

Covar_Data <- read.csv('Data/combined_covariates.csv')

#For now we're picking out just these covariates
Covars_keep <- c('LTLA19CD',
                 'prop_white_british',
                 'IMD_Average_score',
                 'IMD_Rank_of_average_score',
                 'prop_travelling_to_work',
                 'resident_earnings',
                 'mean_age',
                 'mean_popden',
                 'prop_o65')

#Filter down just to the above columns
Covar_Data <- Covar_Data[ , which(names(Covar_Data) %in% Covars_keep)]

#Rename areaCode
colnames(Covar_Data)[colnames(Covar_Data) == 'LTLA19CD'] <- 'areaCode'

#(remove City of London because it's data is skewhiff)
Covar_Data <- filter(Covar_Data, areaCode != 'E09000001')
Covar_Data$resident_earnings <- as.numeric(Covar_Data$resident_earnings)


#ISSUES
#######
#We have NO covariates for Scotland currently
#We don't have IMD data for Wales

#We are missing info for: 
#E06000060 - Buckinghamshire
#E06000061 - North Northamptonshire
#E06000062 - West Northamptonshire

#This is because, to match the case counts we had to do some sticking together:
#Buckinghamshire = South Bucks ("E07000006"), Aylesbury Vale ("E07000004"), Chiltern ("E07000005"), Wycombe ("E07000007"), 
#West Northamptonshire = Northampton ("E07000154") , South Northamptonshire ("E07000155"), Daventry ("E07000151")
#North Northamptonshire = East Northamptonshire ("E07000152"), Wellingborough ("E07000156"), Corby ("E07000150"), Kettering ("E07000153")

#Easiest thing will just be to do this by hand
Buck_codes <- c("E07000004", "E07000005", "E07000006", "E07000007")
North_North_codes <- c("E07000153", "E07000150", "E07000156", "E07000152")
West_North_codes <- c("E07000151", "E07000155", "E07000154")

Covar_Data_full <- read.csv('Data/combined_covariates.csv')
colnames(Covar_Data_full)[colnames(Covar_Data_full) == 'LTLA19CD'] <- 'areaCode'
Covar_Data_full <- filter(Covar_Data_full, areaCode != 'E09000001')
Covar_Data_full$resident_earnings <-as.numeric(Covar_Data_full$resident_earnings)

Buck_data <- filter(Covar_Data_full, areaCode %in% Buck_codes)
North_North_data <- filter(Covar_Data_full, areaCode %in% North_North_codes)
West_North_data <- filter(Covar_Data_full, areaCode %in% West_North_codes)


Covar_Extra <- data.frame(areaCode = c("E06000060", "E06000061", "E06000062"),
                          prop_travelling_to_work = c((sum(Buck_data$nomis_total_people*Buck_data$prop_travelling_to_work)/sum(Buck_data$nomis_total_people)),
                                                      (sum(North_North_data$nomis_total_people*North_North_data$prop_travelling_to_work)/sum(North_North_data$nomis_total_people)),
                                                      (sum(West_North_data$nomis_total_people*West_North_data$prop_travelling_to_work)/sum(West_North_data$nomis_total_people))),
                          
                          prop_white_british = c((sum(Buck_data$total_people_eth*Buck_data$prop_white_british)/sum(Buck_data$total_people_eth)),
                                                 (sum(North_North_data$total_people_eth*North_North_data$prop_white_british)/sum(North_North_data$total_people_eth)),
                                                 (sum(West_North_data$total_people_eth*West_North_data$prop_white_british)/sum(West_North_data$total_people_eth))),
                          
                          IMD_Average_score = c(sum(Buck_data$IMD_Average_score*Buck_data$total_people_eth)/sum(Buck_data$total_people_eth),
                                                sum(North_North_data$IMD_Average_score*North_North_data$total_people_eth)/sum(North_North_data$total_people_eth),
                                                sum(West_North_data$IMD_Average_score*West_North_data$total_people_eth)/sum(West_North_data$total_people_eth)),
                          #Obviously a rough way of doing this
                          #TODO: Improve this.
                          IMD_Rank_of_average_score = c(sum(Buck_data$IMD_Rank_of_average_score*Buck_data$total_people_eth)/sum(Buck_data$total_people_eth),
                                                        sum(North_North_data$IMD_Rank_of_average_score*North_North_data$total_people_eth)/sum(North_North_data$total_people_eth),
                                                        sum(West_North_data$IMD_Rank_of_average_score*West_North_data$total_people_eth)/sum(West_North_data$total_people_eth)),
                          
                          resident_earnings = c(sum(Buck_data$resident_earnings*Buck_data$total_people_eth)/sum(Buck_data$total_people_eth),
                                                sum(North_North_data$resident_earnings*North_North_data$total_people_eth)/sum(North_North_data$total_people_eth),
                                                sum(West_North_data$resident_earnings*West_North_data$total_people_eth)/sum(West_North_data$total_people_eth)),
                          
                          mean_age = c(sum(Buck_data$mean_age*Buck_data$total_people_eth)/sum(Buck_data$total_people_eth),
                                       sum(North_North_data$mean_age*North_North_data$total_people_eth)/sum(North_North_data$total_people_eth),
                                       sum(West_North_data$mean_age*West_North_data$total_people_eth)/sum(West_North_data$total_people_eth)),
                          
                          prop_o65 = c(sum(Buck_data$prop_o65*Buck_data$total_people_eth)/sum(Buck_data$total_people_eth),
                                       sum(North_North_data$prop_o65*North_North_data$total_people_eth)/sum(North_North_data$total_people_eth),
                                       sum(West_North_data$prop_o65*West_North_data$total_people_eth)/sum(West_North_data$total_people_eth)),
                          #Estimating this just from the population 2019 and sqrt data in Wes_horror
                          mean_popden = c(543973/1563,
                                          (94527+79707+72218+101776)/(510+163+80+233),
                                          (753278+94490+85950)/(2364+634+663))
)

#stick the two together
Covar_Data <- rbind(Covar_Data, Covar_Extra)

rm(Covar_Data_full, Buck_data, North_North_data, West_North_data, Buck_codes, North_North_codes, West_North_codes, Covar_Extra, i, Covars_keep)

#Now merge with Case_Rates_Data
Case_Rates_Data <- merge(Case_Rates_Data, Covar_Data, by = 'areaCode', all.x = TRUE)

################
#Add mobility data
################

#Pull out specific areaCodes for the google mobility data
#Load data from https://www.google.com/covid19/mobility/

#The data shows how visitors to (or time spent in) categorized places change compared to 
#our baseline days. A baseline day represents a normal value for that day of the week. 
#The baseline day is the median value from the 5???week period Jan 3 ??? Feb 6, 2020.

#For each region-category, the baseline isn???t a single value???it???s 7 individual values.
#The same number of visitors on 2 different days of the week, result in different
#percentage changes. So, we recommend the following:
#Don???t infer that larger changes mean more visitors or smaller changes mean less visitors.
#Avoid comparing day-to-day changes. Especially weekends with weekdays.

#The data shows how visits to places, such as corner shops and parks, are changing 
#in each geographic region.

#The Community Mobility Reports are no longer being updated as of 2022-10-15. 
#All historical data will remain publicly available for research purposes.
Google_2020_Data <- read.csv('Data/2020_GB_Region_Mobility_Report.csv')
Google_2021_Data <- read.csv('Data/2021_GB_Region_Mobility_Report.csv')
Google_2022_Data <- read.csv('Data/2022_GB_Region_Mobility_Report.csv')
#combine them:
Google_Total_Data <- rbind(Google_2020_Data, Google_2021_Data, Google_2022_Data)
rm(Google_2020_Data, Google_2021_Data, Google_2022_Data)

#We filter to one day out to observe different areas:
One_day_google <- filter(Google_Total_Data, date == "2021-03-30")

One_code_google <- Google_Total_Data[!duplicated(Google_Total_Data$place_id),]

#Go through reduced data:
Reduced_Data <- filter(Case_Rates_Data, Week == 100)
#read areaName

#look in region 2 for it. if only one, then copy areacode

#if not found, search region 1 for it

One_code_google$areaCode <- NA

for(i in 1:length(Reduced_Data$areaCode)){
  search_string <- Reduced_Data$areaName[i]
  
  if( sum( grepl( search_string, One_code_google$sub_region_2 , fixed = TRUE)   ) > 0  ){
    #search string was in region 2
    
    Reduced_google <- filter(One_code_google, grepl( search_string, One_code_google$sub_region_2 , fixed = TRUE) )
    
    if ( nrow(Reduced_google) == 1){
      #Just one match with the area name:
      insert_index <- which(grepl( search_string, One_code_google$sub_region_2 , fixed = TRUE))
      
      One_code_google$areaCode[insert_index] <- Reduced_Data$areaCode[i]
      
    }else{
      print(sprintf('Multiple region 2 locations found for %s', search_string))
      print(Reduced_Data$areaCode[i])
      print(i)
    }
    
  } else{
    #Couldn't find search string in region 2. So look for a unique region 1
    
    #filter out everything with a region 2 specifier
    Reduced_google <- filter(One_code_google, sub_region_2 == "")
    
    #Now see if the search string exists in region 1:
    if( sum( grepl( search_string, Reduced_google$sub_region_1 , fixed = TRUE)   ) > 0  ){
      #search string was in region 1
      Reduced_google <- filter(Reduced_google, grepl( search_string, Reduced_google$sub_region_1 , fixed = TRUE) )
      
      if ( nrow(Reduced_google) == 1){
        #Just one match with the area name:
        Google_code_match <- Reduced_google$place_id[1]
        insert_index <- which(One_code_google$place_id == Google_code_match)
        
        One_code_google$areaCode[insert_index] <- Reduced_Data$areaCode[i]
        
      }else{
        print(sprintf('Multiple region 1 locations found for %s', search_string))
        print(Reduced_Data$areaCode[i])
        print(i)
      }
      
      
    } else {
      print(sprintf('Name %s not found in region 1 or 2', search_string))
      print(Reduced_Data$areaCode[i])
      print(i)
    }
    
  }
  
  
  
}

Google_conversion <- One_code_google[,-c(6,7,9,10,11,12,13,14,15)]

Google_conversion <- filter(Google_conversion, !is.na(areaCode))

#Now stick an area code to everything in Google_Total_Data

Google_Total_Data <- merge(x = Google_Total_Data, y = Google_conversion[ , c("place_id", "areaCode")], by = "place_id", all.x=TRUE)

Google_Total_Data <- filter(Google_Total_Data, !is.na(areaCode))

Google_Total_Data$date <- as.Date(Google_Total_Data$date)

#Now just need to average together some of the troublesome areas
Trouble_Codes <- c('E06000061', #North Northamptonshire
                   'E06000062', #West Northamptonshire
                   'E09000012', #Hackney and City of London     #NOTE, CITY OF LONDON HAS NO RESIDENTIAL DATA, so I've removed it from the code thingy for now
                   'E07000244', #East Suffolk
                   'E07000246' #Taunton and West Somerset
)


#We average the columns by areaCode AND date

Google_Total_Data <- aggregate(Google_Total_Data$residential_percent_change_from_baseline,
                               by=list(areaCode=Google_Total_Data$areaCode, date = Google_Total_Data$date), FUN=mean)

colnames(Google_Total_Data) <- c('areaCode', 'date', 'Residential_mobility_percentage_change')

Google_Mobility_Data <- Google_Total_Data

write.csv(Google_Mobility_Data, file = 'Outputs/Google_Mobility_Data.csv')


#For now, we just affix the residential data to the case_rates data
#We use the mean of the week
Case_Rates_Data$Residential_Mobility <- NA

for(i in 1:nrow(Case_Rates_Data)){
  areaCode_hold <- Case_Rates_Data$areaCode[i]
  date_hold <- Case_Rates_Data$date_begin[i] + 0:6
  
  Reduced_Mobility <- filter(Google_Mobility_Data, areaCode == areaCode_hold)
  Reduced_Mobility <- filter(Reduced_Mobility, date %in% date_hold)
  mobility_mean <- mean(Reduced_Mobility$Residential_mobility_percentage_change, na.rm = TRUE)
  
  Case_Rates_Data$Residential_Mobility[i] <- mobility_mean
  
}




#We want to set the minimum case numbers possible to be 1, to get around issues with log(0)
#There are 73 occurences of this!
for(i in 1:nrow(Case_Rates_Data)){
  if(Case_Rates_Data$Week_Cases[i] == 0){
    Case_Rates_Data$Week_Cases[i] <- 1
  }
  
  if(Case_Rates_Data$previous_week_cases[i] == 0){
    Case_Rates_Data$previous_week_cases[i] <- 1
  }
}


#We also want to import in the mean Delta proportion data
#We only have this for the regions of England, but that will do
#TODO: Add in Alpha proportion data! And Omicron too!
#TODO: #MAJOR TODO: We need to also record the "switch off" of each variant
#Load the LTLA to region conversion table
LTLA_to_region <- read.csv('Data/LTLA_to_Region.csv')
#Load the Delta prop data
Delta_proportion_data <- read.csv('Data/Delta_Proportion_Data.csv')
Delta_proportion_data$dates <- as.Date(Delta_proportion_data$dates, format = '%Y-%m-%d')

Case_Rates_Data$Delta_proportion <- NA
Available_codes <- unique(LTLA_to_region$LAD21CD)

for(i in 1:nrow(Case_Rates_Data)){
  #read date
  date_hold <- Case_Rates_Data$date_begin[i]
  LTLA_hold <- Case_Rates_Data$areaCode[i]
  #Check we have data available
  #The delta prop data covers march 8th 2021 to july 31st 2021
  
  
  if(!(LTLA_hold %in% Available_codes)){
    
  }else if(date_hold < as.Date('2021-03-08')){
    Case_Rates_Data$Delta_proportion[i] <- 0
  }else if(date_hold > as.Date('2021-07-25')){
    Case_Rates_Data$Delta_proportion[i] <- 99.99
  }else{
    region_hold <- LTLA_to_region$RGN21NM[which(LTLA_to_region$LAD21CD == LTLA_hold)]
    
    delta_reduced <- filter(Delta_proportion_data, region == region_hold)
    
    Case_Rates_Data$Delta_proportion[i] <- mean(delta_reduced$pos_mean[which(delta_reduced$dates == date_hold)+0:6])
    
  }
  
}

#Done for now, export the data

write.csv(Case_Rates_Data, file = 'Outputs/Cases_Data.csv')
save(Case_Rates_Data, file = 'Outputs/Cases_Data.RData')
save(W, file = 'Outputs/W.RData')

