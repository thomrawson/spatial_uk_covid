
#Make folder for outputs
dir.create("Outputs")

#Switch to say if we want to record the previous day's cases or not
record_previous_day_cases <- FALSE

#Start by loading information on UK LTLA region boundaries from shape file
Boundaries <- st_read("Data/district_borough_unitary_region.shp")



#Now load daily new COVID cases data at LTLA level
#Originally this was pulled from the government portal API:
#Case_Rates_Data<- read.csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=newCasesBySpecimenDate&format=csv')

#But in the interest of re-producibility, we use the hard-saved pull from this API
#Current source pulled June 22nd 2023
Case_Rates_Data <- read.csv('Data/ltla_cases_2023_06_22.csv')

#Ensure date is being read as a date
Case_Rates_Data$date <- as.Date(Case_Rates_Data$date)

#To speed things up, we start by clipping off everything after Nov 10th 2022
Case_Rates_Data <- filter(Case_Rates_Data, date < as.Date("2022-11-10"))

#############################################################################
#############################################################################
#We now want to also attach multiple other metrics of case counting, starting 
#with others from the online dashboard:

#First, is a measure of cumulative first episodes:
FirstEpisodes_Data <- read.csv("Data/ltla_first_episodes_2023_06_22.csv")
FirstEpisodes_Data$date <- as.Date(FirstEpisodes_Data$date)
FirstEpisodes_Data <- filter(FirstEpisodes_Data, date < as.Date("2022-11-10"))

#Are all the LTLAs as recorded the same in both datasets?
new_LTLA_codes <- unique(FirstEpisodes_Data$areaCode)
CaseRates_LTLA_codes <- unique(Case_Rates_Data$areaCode)
CaseRates_LTLA_names <- unique(Case_Rates_Data$areaName)

#sum(grepl("E", CaseRates_LTLA_codes))
#This line shows us that, of the 380 codes, 65 of them are Scotland / Wales

#These lines show us that there is no mismatch between LTLA codes
#CaseRates_LTLA_names[which(!(CaseRates_LTLA_codes %in% new_LTLA_codes))]
#CaseRates_LTLA_codes[which(!(CaseRates_LTLA_codes %in% new_LTLA_codes))]

#This is a very easy stick together:
Case_Rates_Data <- full_join(Case_Rates_Data, FirstEpisodes_Data, by=c("areaCode", "areaName", "areaType", "date"))

#############################################################################
#############################################################################
#Next, we're going to stick on a different measure of cases:
#Only the pillar 2, and PCR_only cases from the direct England linelist
Linelist_Data <- readRDS("Data/reduced_linelist.rds")
Linelist_Data <- filter(Linelist_Data, specimen_date < as.Date("2022-11-10"))

new_LTLA_codes <- unique(Linelist_Data$ltla_code)
new_LTLA_names <- unique(Linelist_Data$ltla_name)
CaseRates_LTLA_names[which(!(CaseRates_LTLA_codes %in% new_LTLA_codes))]
CaseRates_LTLA_codes[which(!(CaseRates_LTLA_codes %in% new_LTLA_codes))]

#The linelist does not have data for 4 of the English LTLAs in the dashboard
#(It also doesn't have any of the 65 devolved nations LTLAs info):
#E07000006 - South Bucks
#E07000004 - Aylesbury Vale
#E07000005 - Chiltern
#E07000007 - Wycombe

new_LTLA_names[which(!(new_LTLA_codes %in% CaseRates_LTLA_codes))]
new_LTLA_codes[which(!(new_LTLA_codes %in% CaseRates_LTLA_codes))]

#Likewise, the dashboard data does not have info on three of the linelist LTLAs:
#E06000053 - Isles of Scilly
#E06000060 - Buckinghamshire
#E09000001 - City of London

#Firstly, in the dashboard data, it combines Hackney AND City of London under (E09000012)
#Let's go ahead and combine these in the linelist data now:
Linelist_Data$ltla_code[which(Linelist_Data$ltla_code == "E09000001")] <- "E09000012"
Linelist_Data$ltla_name[which(Linelist_Data$ltla_code == "E09000012")] <- "Hackney and City of London"

#Similarly, in the dashboard data, Isles of Scilly are rolled in with Cornwall under E06000052 (just Cornwall for linelist)
Linelist_Data$ltla_code[which(Linelist_Data$ltla_code == "E06000053")] <- "E06000052"
Linelist_Data$ltla_name[which(Linelist_Data$ltla_code == "E06000052")] <- "Cornwall and Isles of Scilly"

#Now we aggregate these by date and code:
Linelist_Data <- aggregate(Linelist_Data$cases,
                                 by=list(date=Linelist_Data$specimen_date, 
                                         areaCode = Linelist_Data$ltla_code, 
                                         areaName = Linelist_Data$ltla_name),
                                 FUN=sum)

#Lastly,
##Buckinghamshire = South Bucks, Aylesbury Vale, Chiltern, Wycombe, 
#Later on, we're going to collapse these 4 regions into for the Case Rates Data, 
#but we first need to attach the vaccine data we have for the existing LTLAs as
#presented in the dashboard data. We do the following in order:
#1. Attach the vaccine data to the dashboard data
#2. Collapse the Buckinghamshire LTLAs in the dashboard data, and the other two sets
#3. Collapse the remaining two re-drawn boundaries for the linelist data
#4. Combine the two datasets.

#############################################################################
#############################################################################



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
Case_Rates_Data <- Case_Rates_Data[,-c(7,8)]

Case_Rates_Data <- merge(Case_Rates_Data, Second_Vaccine_Data, by = c('areaCode','date'), all.x = TRUE)
Case_Rates_Data <- Case_Rates_Data[,-c(8,9)]

Case_Rates_Data <- merge(Case_Rates_Data, Third_Vaccine_Data, by = c('areaCode','date'), all.x = TRUE)
Case_Rates_Data <- Case_Rates_Data[,-c(9,10)]

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

Agg_First_Episodes_Data <- aggregate(Case_Rates_Data$cumFirstEpisodesBySpecimenDate,
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



Agg_Case_Rates_Data$previous_day_cases <- NA
colnames(Agg_Case_Rates_Data) <- c('date', 'areaCode', 'areaName','Cases', 'previous_day_cases')
colnames(Agg_First_Episodes_Data) <- c('date', 'areaCode', 'areaName','First_Episodes_Total')
colnames(First_Vacc_Mean_Data) <- c('date', 'areaCode', 'areaName','cumVaccPercentage_FirstDose')
colnames(Second_Vacc_Mean_Data) <- c('date', 'areaCode', 'areaName','cumVaccPercentage_SecondDose')
colnames(Third_Vacc_Mean_Data) <- c('date', 'areaCode', 'areaName','cumVaccPercentage_ThirdDose')

Case_Rates_Data <- merge(Agg_Case_Rates_Data, Agg_First_Episodes_Data, by = c('areaCode', 'areaName', 'date'))
Case_Rates_Data <- merge(Case_Rates_Data, First_Vacc_Mean_Data, by = c('areaCode', 'areaName', 'date'))
Case_Rates_Data <- merge(Case_Rates_Data, Second_Vacc_Mean_Data, by = c('areaCode', 'areaName', 'date'))
Case_Rates_Data <- merge(Case_Rates_Data, Third_Vacc_Mean_Data, by = c('areaCode', 'areaName', 'date'))

Case_Rates_Data$date <- as.Date(Case_Rates_Data$date)

#########################
#Now we repeat all these steps with the linelist data, collapsing two of the region sets
#Now change the areaCode and areaName:
for(i in 1:length(Linelist_Data[,1])){
  if(Linelist_Data$areaCode[i] %in% Not_in_Boundaries2){
    index <- which(areaChanges$from_code == Linelist_Data$areaCode[i])
    Linelist_Data$areaCode[i] <- areaChanges$to_code[index]
    Linelist_Data$areaName[i] <- areaChanges$to_name[index]
  }
}

New_Cases <- filter(Linelist_Data, areaCode %in% Not_in_Case_Rates2)

colnames(Linelist_Data) <- c("date", "areaCode", "areaName", "Linelist_P2_PCR_cases")
#Now we aggregate these by date and code:
Agg_Linelist_Data <- aggregate(Linelist_Data$Linelist_P2_PCR_cases,
                                 by=list(date=Linelist_Data$date, 
                                         areaCode = Linelist_Data$areaCode, 
                                         areaName = Linelist_Data$areaName),
                                 FUN=sum)

colnames(Agg_Linelist_Data) <- c("date", "areaCode", "areaName", "Linelist_P2_PCR_cases")

#We should now have no mismatch in LTLAs between our two case sets
Case_Data_Codes <- unique(Case_Rates_Data$areaCode)
Linelist_Codes <- unique(Agg_Linelist_Data$areaCode)

#sum(!(Linelist_Codes %in% Case_Data_Codes))
#Case_Data_Codes[which(!(Case_Data_Codes %in% Linelist_Codes))]
#Correct, it's just the devolved nations we don't have!

#I just want to quickly check how well the names match so there's no nasty surprises
Case_Data_Codes <- unique(Case_Rates_Data[,c(1,2)])
Linelist_Codes <- unique(Agg_Linelist_Data[,c(2,3)])
Compare_Names <- merge(Case_Data_Codes, Linelist_Codes, by = c("areaCode"))

#A perfect match
Agg_Linelist_Data <- Agg_Linelist_Data[,-3]
#########################
#Now merge our two data sets
Case_Rates_Data <- merge(Case_Rates_Data, Agg_Linelist_Data, all = TRUE, by = c('areaCode', 'date'))

#Replace the NAs in the Linelist cases column with 0s:
Case_Rates_Data$Linelist_P2_PCR_cases[is.na(Case_Rates_Data$Linelist_P2_PCR_cases)] <- 0

#Cut the junk
rm(areaChanges, New_Cases, New_Cases2, Old_Cases,
   Not_in_Boundaries1, Not_in_Boundaries2,
   Not_in_Case_Rates1, Not_in_Case_Rates2, Agg_First_Episodes_Data,
   Agg_Linelist_Data, Case_Data_Codes, Compare_Names,
   Linelist_Codes, Linelist_Data, FirstEpisodes_Data)
#Now we record, for each LTLA, the case count from the previous day in the row too.
#This takes a long time, there's probably a...
#TODO: Make this more efficient!

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

Case_Rates_Data <- merge(Case_Rates_Data, LTLA_Demog[,c(1,5,6,7)], by = 'areaCode', all.x = TRUE)

#Remove the unnecessary vaccine dataframes
rm(Agg_Case_Rates_Data, First_Vacc_Mean_Data, First_Vaccine_Data,
   Second_Vacc_Mean_Data, Second_Vaccine_Data, 
   Third_Vacc_Mean_Data, Third_Vaccine_Data)

#And remove some extra rubbish
rm(Boundary_Codes, CaseRates_LTLA_codes, CaseRates_LTLA_names,
   i, index, new_LTLA_codes, new_LTLA_names)

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
#TODO: Can definitely do this without needing a loop. Make more efficient.
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
#If desired, could export daily data here:
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

#FIRST DATE: Sunday 3rd May 2020
Case_Rates_Data$Week <- plyr::round_any(as.numeric(Case_Rates_Data$date - as.Date('2020-05-03') + 0.5), 7, f = ceiling)/7

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

Case_Rates_Data_hold$date_begin <- as.Date('2020-05-03') + ((Case_Rates_Data_hold$Week - 1)*7) 

#Previous weeks column
Case_Rates_Data_hold$previous_week_cases <- NA
Case_Rates_Data_hold$next_week_cases <- NA


for(i in 1: length(Case_Rates_Data_hold$Week)){
  areaName_hold <- Case_Rates_Data_hold$areaName[i]
  date_hold <- Case_Rates_Data_hold$Week[i]
  
  #Obtain the previous day's case_Rate
  data_hold <- filter(Case_Rates_Data_hold, areaName == areaName_hold)
  data_hold <- filter(data_hold, Week == date_hold - 1)
  
  #Obtain the next day's case_Rate
  data_hold2 <- filter(Case_Rates_Data_hold, areaName == areaName_hold)
  data_hold2 <- filter(data_hold2, Week == date_hold + 1)
  
  if(length(data_hold$areaCode) == 0){
    
  } else if (length(data_hold$areaCode) == 1){
    Case_Rates_Data_hold$previous_week_cases[i] <- data_hold$Week_Cases
  } else {
    print(paste('Error: length neither 0 nor 1 at prev_week i =', i, sep = " "))
  }
  
  if(length(data_hold2$areaCode) == 0){
    
  } else if (length(data_hold2$areaCode) == 1){
    Case_Rates_Data_hold$next_week_cases[i] <- data_hold2$Week_Cases
  } else {
    print(paste('Error: length neither 0 nor 1 at next_week i =', i, sep = " "))
  }
  
}

#Drop "week one", as we don't have the previous week cases.
Case_Rates_Data_hold <- drop_na(Case_Rates_Data_hold)

rm(data_hold, areaName_hold, date_hold, i)

#Now we need to add back the vaccination states, and first episodes for that day too. 
#We currently use the percentage on the date of week ENDING but could feasibly...
#TODO: SET THIS TO THE WEEKLY MEAN VACC INSTEAD
Case_Rates_Data_hold$First_Episodes_Total <- NA
Case_Rates_Data_hold$cumVaccPercentage_FirstDose <- NA
Case_Rates_Data_hold$cumVaccPercentage_SecondDose <- NA
Case_Rates_Data_hold$cumVaccPercentage_ThirdDose <- NA
Case_Rates_Data_hold$date_begin <- as.Date(Case_Rates_Data_hold$date_begin)

for(i in 1:length(Case_Rates_Data_hold$Week)){
  date_hold <- Case_Rates_Data_hold$date_begin[i] + 6
  areaCode_hold <- Case_Rates_Data_hold$areaCode[i]
  
  data_filter <- filter(Case_Rates_Data, date == date_hold)
  data_filter <- filter(data_filter, areaCode == areaCode_hold)
  
  Case_Rates_Data_hold$First_Episodes_Total[i] <- data_filter$First_Episodes_Total[1]
  
  Case_Rates_Data_hold$cumVaccPercentage_FirstDose[i] <- data_filter$cumVaccPercentage_FirstDose[1]
  Case_Rates_Data_hold$cumVaccPercentage_SecondDose[i] <- data_filter$cumVaccPercentage_SecondDose[1]
  Case_Rates_Data_hold$cumVaccPercentage_ThirdDose[i] <- data_filter$cumVaccPercentage_ThirdDose[1]
}

#Case_Rates_Data <- Case_Rates_Data_hold

rm(data_filter, date_hold, areaCode_hold)

#save(Case_Rates_Data, file = 'Outputs/Weekly_Case_Date_12_01_22.RData')

#But, first episodes doesn't seem to always line up correctly with the new number of cases...
#Of course, we wouldn't expect it too! First episodes will always be a bit less than
#new cases, as it doesn't count the new repeat cases.

#A quick sanity check though is always good
#first_episode_differences <- rep(0, 45924)
#all_dates <- unique(Case_Rates_Data$Week)
#for(i in 3:131){
#  current_week <- filter(Case_Rates_Data, Week == i)
#  previous_week <- filter(Case_Rates_Data, Week == (i-1))
#  first_eps <- current_week$First_Episodes_Total - previous_week$First_Episodes_Total
#  cases_diff <- current_week$Week_Cases
#  first_episode_differences[((356*(i-3))+1):(356*(i-2))] <- cases_diff - first_eps
#  }
#Confirms that it's always less first episodes than cases, and is mostly zeros in early weeks

#GOT TO HERE: NOW NEED TO DO THE SAME BUT FOR THE LINELIST CASES AND RE-ATTACH
##############################################################

#Now sum by, everything, but not vaccination yet
Linelist_Data_hold <- aggregate(Case_Rates_Data$Linelist_P2_PCR_cases, by=list(Week=Case_Rates_Data$Week, areaCode = Case_Rates_Data$areaCode), FUN=sum)

#Now need to add a "previous week cases" column, and a "date_begin" column:
colnames(Linelist_Data_hold) <- c('Week', 'areaCode', 'Linelist_P2_PCR_Week_Cases')

#Previous weeks column
Linelist_Data_hold$Linelist_P2_PCR_Previous_Week_Cases <- NA
Linelist_Data_hold$Linelist_P2_PCR_Next_Week_Cases <- NA


for(i in 1:length(Linelist_Data_hold$Week)){
  areaCode_hold <- Linelist_Data_hold$areaCode[i]
  date_hold <- Linelist_Data_hold$Week[i]
  
  #Obtain the previous day's case_Rate
  data_hold <- filter(Linelist_Data_hold, areaCode == areaCode_hold)
  data_hold <- filter(data_hold, Week == date_hold - 1)
  
  #Obtain the next day's case_Rate
  data_hold2 <- filter(Linelist_Data_hold, areaCode == areaCode_hold)
  data_hold2 <- filter(data_hold2, Week == date_hold + 1)
  
  if(length(data_hold$areaCode) == 0){
    
  } else if (length(data_hold$areaCode) == 1){
    Linelist_Data_hold$Linelist_P2_PCR_Previous_Week_Cases[i] <- data_hold$Linelist_P2_PCR_Week_Cases
  } else {
    print(paste('Error: length neither 0 nor 1 at prev_week i =', i, sep = " "))
  }
  
  if(length(data_hold2$areaCode) == 0){
    
  } else if (length(data_hold2$areaCode) == 1){
    Linelist_Data_hold$Linelist_P2_PCR_Next_Week_Cases[i] <- data_hold2$Linelist_P2_PCR_Week_Cases
  } else {
    print(paste('Error: length neither 0 nor 1 at next_week i =', i, sep = " "))
  }
  
}

#Drop "week one", as we don't have the previous week cases.
Linelist_Data_hold <- drop_na(Linelist_Data_hold)

rm(data_hold, areaCode_hold, date_hold, i, data_hold2)

#Now re-join the data
Case_Rates_Data <- merge(Case_Rates_Data_hold, Linelist_Data_hold, by = c("areaCode", "Week"))

rm(Case_Rates_Data_hold, Linelist_Data_hold)
##############################################################

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
                 'prop_all_other_white',
                 'prop_mixed_multiple',
                 'prop_asian',
                 'prop_black_afr_car',
                 'prop_other',
                 'IMD_Average_score',
                 'IMD_Rank_of_average_score',
                 'prop_travelling_to_work',
                 'resident_earnings',
                 'mean_age',
                 'mean_popden',
                 'prop_o65',
                 'prop_not_in_work')

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
                          
                          prop_not_in_work = c((sum(Buck_data$nomis_total_people*Buck_data$prop_not_in_work)/sum(Buck_data$nomis_total_people)),
                                               (sum(North_North_data$nomis_total_people*North_North_data$prop_not_in_work)/sum(North_North_data$nomis_total_people)),
                                               (sum(West_North_data$nomis_total_people*West_North_data$prop_not_in_work)/sum(West_North_data$nomis_total_people))),
                          
                          prop_white_british = c((sum(Buck_data$total_people_eth*Buck_data$prop_white_british)/sum(Buck_data$total_people_eth)),
                                                 (sum(North_North_data$total_people_eth*North_North_data$prop_white_british)/sum(North_North_data$total_people_eth)),
                                                 (sum(West_North_data$total_people_eth*West_North_data$prop_white_british)/sum(West_North_data$total_people_eth))),
                          
                          prop_all_other_white = c((sum(Buck_data$total_people_eth*Buck_data$prop_all_other_white)/sum(Buck_data$total_people_eth)),
                                                   (sum(North_North_data$total_people_eth*North_North_data$prop_all_other_white)/sum(North_North_data$total_people_eth)),
                                                   (sum(West_North_data$total_people_eth*West_North_data$prop_all_other_white)/sum(West_North_data$total_people_eth))),
                          
                          prop_mixed_multiple = c((sum(Buck_data$total_people_eth*Buck_data$prop_mixed_multiple)/sum(Buck_data$total_people_eth)),
                                                  (sum(North_North_data$total_people_eth*North_North_data$prop_mixed_multiple)/sum(North_North_data$total_people_eth)),
                                                  (sum(West_North_data$total_people_eth*West_North_data$prop_mixed_multiple)/sum(West_North_data$total_people_eth))),
                          
                          prop_asian = c((sum(Buck_data$total_people_eth*Buck_data$prop_asian)/sum(Buck_data$total_people_eth)),
                                         (sum(North_North_data$total_people_eth*North_North_data$prop_asian)/sum(North_North_data$total_people_eth)),
                                         (sum(West_North_data$total_people_eth*West_North_data$prop_asian)/sum(West_North_data$total_people_eth))),
                          
                          prop_black_afr_car = c((sum(Buck_data$total_people_eth*Buck_data$prop_black_afr_car)/sum(Buck_data$total_people_eth)),
                                                 (sum(North_North_data$total_people_eth*North_North_data$prop_black_afr_car)/sum(North_North_data$total_people_eth)),
                                                 (sum(West_North_data$total_people_eth*West_North_data$prop_black_afr_car)/sum(West_North_data$total_people_eth))),
                          
                          prop_other = c((sum(Buck_data$total_people_eth*Buck_data$prop_other)/sum(Buck_data$total_people_eth)),
                                         (sum(North_North_data$total_people_eth*North_North_data$prop_other)/sum(North_North_data$total_people_eth)),
                                         (sum(West_North_data$total_people_eth*West_North_data$prop_other)/sum(West_North_data$total_people_eth))),
                          
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
#Add earnings data
################
#This is data taken from here: https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/placeofresidencebylocalauthorityashetable8

Earnings_Data <- read.csv('Data/Annual_median_income.csv')



#Fortunately, all Earnings areaCodes are already in the case rate data codes
#We have two "years" split by the tax year change point of 6th April 2021
#"Annual estimates are provided for the tax year that ended on 5th April in the reference year"
Earnings_Data_2021 <- filter(Earnings_Data, year == 2021)
Earnings_Data_2022 <- filter(Earnings_Data, year == 2022)

#Note, we have some "NA" in the earnings data which is marked with an "x"
#This is a pain, the first thing we'll do is, if it's missing, we'll use the value from the other year
for(i in 1:length(Earnings_Data_2021$areaCode)){
  year_hold <- Earnings_Data_2021$year[i]
  code_hold <- Earnings_Data_2021$areaCode[i]
  earnings_hold <- Earnings_Data_2021$Median_annual_income[i]
  jobs_hold <- Earnings_Data_2021$no_jobs[i]
  
  if(earnings_hold == "x"){
    pull_earnings <- filter(Earnings_Data_2022, areaCode == code_hold)
      Earnings_Data_2021$Median_annual_income[i] <- pull_earnings$Median_annual_income[1]
  }
  
  if(jobs_hold == "x"){
    pull_jobs <- filter(Earnings_Data_2022, areaCode == code_hold)
    Earnings_Data_2021$no_jobs[i] <- pull_earnings$no_jobs[1]
  }
}

for(i in 1:length(Earnings_Data_2022$areaCode)){
  year_hold <- Earnings_Data_2022$year[i]
  code_hold <- Earnings_Data_2022$areaCode[i]
  earnings_hold <- Earnings_Data_2022$Median_annual_income[i]
  jobs_hold <- Earnings_Data_2022$no_jobs[i]
  
  if(earnings_hold == "x"){
    pull_earnings <- filter(Earnings_Data_2021, areaCode == code_hold)
    Earnings_Data_2022$Median_annual_income[i] <- pull_earnings$Median_annual_income[1]
  }
  
  if(jobs_hold == "x"){
    pull_jobs <- filter(Earnings_Data_2021, areaCode == code_hold)
    Earnings_Data_2022$no_jobs[i] <- pull_earnings$no_jobs[1]
  }
}




#Make new cols
Case_Rates_Data$Median_annual_income <- NA
Case_Rates_Data$no_jobs <- NA

#TODO: make this more efficient: would need to add a tax_year column to case_rates data.
for(i in 1:length(Case_Rates_Data$areaCode)){
  date_hold <- Case_Rates_Data$date_begin[i]
  area_hold <- Case_Rates_Data$areaCode[i]
  
  if(date_hold > as.Date("2021-04-06")){
    earnings_hold <- filter(Earnings_Data_2022, areaCode == area_hold )
    Case_Rates_Data$Median_annual_income[i] <- earnings_hold$Median_annual_income[1]
    Case_Rates_Data$no_jobs[i] <- earnings_hold$no_jobs[1]
  } else if(date_hold <= as.Date("2021-04-06")){
    earnings_hold <- filter(Earnings_Data_2021, areaCode == area_hold )
    Case_Rates_Data$Median_annual_income[i] <- earnings_hold$Median_annual_income[1]
    Case_Rates_Data$no_jobs[i] <- earnings_hold$no_jobs[1]
  } else{
    print("Incompatible date for earnings data")
  }
  
}

#remove held data
rm(earnings_hold, area_hold, date_hold, Earnings_Data_2021,
   Earnings_Data_2022, Earnings_Data, year_hold, code_hold, jobs_hold,
   pull_earnings, pull_jobs)


#Need to convert these earnings/jobs data to numeric
Case_Rates_Data$Median_annual_income <- as.numeric(Case_Rates_Data$Median_annual_income)
Case_Rates_Data$no_jobs <- as.numeric(Case_Rates_Data$no_jobs)

################
#Add mobility data
################

#Pull out specific areaCodes for the google mobility data
#Load data from https://www.google.com/covid19/mobility/

#The data shows how visitors to (or time spent in) categorized places change compared to 
#our baseline days. A baseline day represents a normal value for that day of the week. 
#The baseline day is the median value from the 5‑week period Jan 3 – Feb 6, 2020.

#For each region-category, the baseline isn’t a single value—it’s 7 individual values.
#The same number of visitors on 2 different days of the week, result in different
#percentage changes. So, we recommend the following:
#Don’t infer that larger changes mean more visitors or smaller changes mean less visitors.
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

#SOLUTION HERE IS TO USE THE GOOGLE_TO_LTLA_CODES THAT I'VE ALREADY MADE
Google_conversion <- One_code_google[,-c(6,7,9,10,11,12,13,14,15)]
#26 LTLA codes not assigned:
# "E06000010" "E06000014" "E06000015" "E06000016" "E06000018"
# "E06000019" "E06000023" "E06000052" "E06000055" "E06000061"
# "E06000062" "E07000008" "E07000064" "E07000112" "E07000128"
# "E07000178" "E07000197" "E07000222" "E07000240" "E07000244"
# "E07000246" "E08000013" "E09000005" "E09000012" "S12000036"
# "S12000038"

#I'm just going to fix these last few manually. 
# "E06000010" - Kingston upon Hull, City of
Google_conversion$areaCode[which(Google_conversion$sub_region_1 == "Kingston upon Hull")] <- "E06000010"
#"E06000014" - York
Google_conversion$areaCode[which(Google_conversion$sub_region_1 == "York")] <- "E06000014"
#"E06000015" - Derby
Google_conversion$areaCode[which(Google_conversion$sub_region_1 == "Derby")] <- "E06000015"
#"E06000016" - Leicester
Google_conversion$areaCode[which(Google_conversion$sub_region_1 == "Leicester")] <- "E06000016"
#"E06000018" - Nottingham
Google_conversion$areaCode[which(Google_conversion$sub_region_1 == "Nottingham")] <- "E06000018"
#"E06000019" - Herefordshire
Google_conversion$areaCode[which(Google_conversion$sub_region_1 == "Herefordshire")] <- "E06000019"
#"E06000023" - Bristol City
Google_conversion$areaCode[which(Google_conversion$sub_region_1 == "Bristol City")] <- "E06000023"
#"E06000052" - Cornwall (two listed, one is Penryn which we ignore)
Google_conversion$areaCode[which(Google_conversion$sub_region_1 == "Cornwall")[1]] <- "E06000052"
#"E06000055" - Bedford
Google_conversion$areaCode[which(Google_conversion$sub_region_1 == "Bedford")] <- "E06000055"
#"E06000061" - North Northamptonshire
#This is one of the fiendish ones, recall that:
#Buckinghamshire = South Bucks ("E07000006"), Aylesbury Vale ("E07000004"), Chiltern ("E07000005"), Wycombe ("E07000007"), 
#West Northamptonshire = Northampton ("E07000154") , South Northamptonshire ("E07000155"), Daventry ("E07000151")
#North Northamptonshire = East Northamptonshire ("E07000152"), Wellingborough ("E07000156"), Corby ("E07000150"), Kettering ("E07000153")
#so we assign this code to 4 different areas:
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "East Northamptonshire")] <- "E06000061"
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Borough of Wellingborough")] <- "E06000061"
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Corby District")] <- "E06000061"
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Borough of Kettering")] <- "E06000061"
#"E06000062" - West Northamptonshire
#we assign this code to 3 different areas
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Northampton District")] <- "E06000062"
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "South Northamptonshire District")] <- "E06000062"
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Daventry District")] <- "E06000062"
#"E07000008" - Cambridge
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Cambridge")] <- "E07000008"
#"E07000064" - Rother District
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Rother District")] <- "E07000064"
#"E07000112" - Folkestone & Hythe District
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Folkestone & Hythe District")] <- "E07000112"
#"E07000128" - Wyre District
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Wyre District")] <- "E07000128"
#"E07000178" - Oxford
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Oxford")] <- "E07000178"
#"E07000197" - Stafford District
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Stafford District")] <- "E07000197"
#"E07000222" - Warwick
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Warwick")] <- "E07000222"
#"E07000240" - Saint Albans District
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Saint Albans District")] <- "E07000240"
#"E07000244" - East Suffolk
#This is actually made up of two areas: Suffolk Coastal District, Waveney District
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Suffolk Coastal District")] <- "E07000244"
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Waveney District")] <- "E07000244"
#"E07000246" - Somerset West and Taunton
#Also made up of two: West Somerset District, Taunton Deane
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "West Somerset District")] <- "E07000246"
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Taunton Deane")] <- "E07000246"
#"E08000013" - Metropolitan Borough of St Helens
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "Metropolitan Borough of St Helens")] <- "E08000013"
#"E09000005" - London Borough of Brent
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "London Borough of Brent")] <- "E09000005"
#"E09000012" - London Borough of Hackney
Google_conversion$areaCode[which(Google_conversion$sub_region_2 == "London Borough of Hackney")] <- "E09000012"
#"S12000036" - Edinburgh
Google_conversion$areaCode[which(Google_conversion$sub_region_1 == "Edinburgh")] <- "S12000036"
#"S12000038" - Renfrewshire
Google_conversion$areaCode[which(Google_conversion$sub_region_1 == "Renfrewshire")] <- "S12000038"

#All areaCodes from the case rates data are now in this "Google_conversion" sheet.
#We save this for reference
write.csv(Google_conversion, file = 'Outputs/Google_codes_LTLA_codes.csv')


Google_conversion <- filter(Google_conversion, !is.na(areaCode))

#Now stick an area code to everything in Google_Total_Data

Google_Total_Data <- merge(x = Google_Total_Data, y = Google_conversion[ , c("place_id", "areaCode")], by = "place_id", all.x=TRUE)

Google_Total_Data <- filter(Google_Total_Data, !is.na(areaCode))

Google_Total_Data$date <- as.Date(Google_Total_Data$date)

#Now just need to average together some of the troublesome areas which have multiple locations under the code:
Trouble_Codes <- c('E06000061', #North Northamptonshire
                   'E06000062', #West Northamptonshire
                   'E09000012', #Hackney and City of London     #NOTE, CITY OF LONDON HAS NO RESIDENTIAL DATA, so I've not affixed the code to the Google entries for "City of London"
                   'E07000244', #East Suffolk
                   'E07000246' #Taunton and West Somerset
)


#We average the columns by areaCode AND date
#We can use the formula method of aggregate. 
#The variables on the 'rhs' of ~ are the grouping variables while 
#the . represents all other variables in the 'df1' 
#(from the example, we assume that we need the mean for all the 
#columns except the grouping)
#aggregate(.~id1+id2, df1, mean)

Google_Total_Data <- stats::aggregate(cbind(retail_and_recreation_percent_change_from_baseline,
                                  grocery_and_pharmacy_percent_change_from_baseline,
                                  parks_percent_change_from_baseline,
                                  transit_stations_percent_change_from_baseline,
                                  workplaces_percent_change_from_baseline,
                                  residential_percent_change_from_baseline
                                )~areaCode+date, Google_Total_Data, FUN=mean, na.rm =TRUE,  na.action=na.pass)

#The NAs have changed to NaN, so change them back
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

Google_Total_Data[is.nan(Google_Total_Data)] <- NA


Google_Mobility_Data <- Google_Total_Data

write.csv(Google_Mobility_Data, file = 'Outputs/Google_Mobility_Data.csv')


#affix the mobility data to the case_rates data
#We use the mean of the week
Case_Rates_Data$retail_and_recreation_percent_change_from_baseline <- NA
Case_Rates_Data$grocery_and_pharmacy_percent_change_from_baseline <- NA
Case_Rates_Data$parks_percent_change_from_baseline <- NA
Case_Rates_Data$transit_stations_percent_change_from_baseline <- NA
Case_Rates_Data$workplaces_percent_change_from_baseline <- NA
Case_Rates_Data$residential_percent_change_from_baseline <- NA

#TODO: make this more efficient
for(i in 1:nrow(Case_Rates_Data)){
  areaCode_hold <- Case_Rates_Data$areaCode[i]
  date_hold <- Case_Rates_Data$date_begin[i] + 0:6
  
  Reduced_Mobility <- filter(Google_Mobility_Data, areaCode == areaCode_hold)
  Reduced_Mobility <- filter(Reduced_Mobility, date %in% date_hold)
  mobility_mean <- mean(Reduced_Mobility$Residential_mobility_percentage_change, na.rm = TRUE)
  
  Case_Rates_Data$retail_and_recreation_percent_change_from_baseline[i] <- ifelse(is.nan(mean(Reduced_Mobility$retail_and_recreation_percent_change_from_baseline, na.rm = TRUE)),NA,mean(Reduced_Mobility$retail_and_recreation_percent_change_from_baseline, na.rm = TRUE))
  Case_Rates_Data$grocery_and_pharmacy_percent_change_from_baseline[i] <- ifelse(is.nan(mean(Reduced_Mobility$grocery_and_pharmacy_percent_change_from_baseline, na.rm = TRUE)),NA,mean(Reduced_Mobility$grocery_and_pharmacy_percent_change_from_baseline, na.rm = TRUE))
  Case_Rates_Data$parks_percent_change_from_baseline[i] <- ifelse(is.nan(mean(Reduced_Mobility$parks_percent_change_from_baseline, na.rm = TRUE)),NA,mean(Reduced_Mobility$parks_percent_change_from_baseline, na.rm = TRUE))
  Case_Rates_Data$transit_stations_percent_change_from_baseline[i] <- ifelse(is.nan(mean(Reduced_Mobility$transit_stations_percent_change_from_baseline, na.rm = TRUE)),NA,mean(Reduced_Mobility$transit_stations_percent_change_from_baseline, na.rm = TRUE))
  Case_Rates_Data$workplaces_percent_change_from_baseline[i] <- ifelse(is.nan(mean(Reduced_Mobility$workplaces_percent_change_from_baseline, na.rm = TRUE)),NA,mean(Reduced_Mobility$workplaces_percent_change_from_baseline, na.rm = TRUE))
  Case_Rates_Data$residential_percent_change_from_baseline[i] <- ifelse(is.nan(mean(Reduced_Mobility$residential_percent_change_from_baseline, na.rm = TRUE)),NA,mean(Reduced_Mobility$residential_percent_change_from_baseline, na.rm = TRUE))
  
}

#A few NA issues
#NA percentage:
sum(is.na(Case_Rates_Data$retail_and_recreation_percent_change_from_baseline))/length(Case_Rates_Data$areaCode)
#Retail: 1.9% NA
sum(is.na(Case_Rates_Data$grocery_and_pharmacy_percent_change_from_baseline))/length(Case_Rates_Data$areaCode)
#grocery and pharmacy: 2.1% NA
sum(is.na(Case_Rates_Data$parks_percent_change_from_baseline))/length(Case_Rates_Data$areaCode)
#parks: 10.5% NA
sum(is.na(Case_Rates_Data$transit_stations_percent_change_from_baseline))/length(Case_Rates_Data$areaCode)
#transit: 2.4% NA
sum(is.na(Case_Rates_Data$workplaces_percent_change_from_baseline))/length(Case_Rates_Data$areaCode)
#workplace: 0.9% NA
sum(is.na(Case_Rates_Data$residential_percent_change_from_baseline))/length(Case_Rates_Data$areaCode)
#residential: 1.2% NA

#To get around this, we'll use the average value between weeks surrounding an NA to see if that helps:
for(i in 1:nrow(Case_Rates_Data)){
  areaCode_hold <- Case_Rates_Data$areaCode[i]
  week_hold <- Case_Rates_Data$Week[i]
  week_hold <- week_hold + c(-1,0,1)
  
  Reduced_Mobility <- filter(Case_Rates_Data, areaCode == areaCode_hold)
  Reduced_Mobility <- filter(Reduced_Mobility, Week %in% week_hold )
  
  Case_Rates_Data$retail_and_recreation_percent_change_from_baseline[i] <- ifelse(is.nan(mean(Reduced_Mobility$retail_and_recreation_percent_change_from_baseline, na.rm = TRUE)),NA,mean(Reduced_Mobility$retail_and_recreation_percent_change_from_baseline, na.rm = TRUE))
  Case_Rates_Data$grocery_and_pharmacy_percent_change_from_baseline[i] <- ifelse(is.nan(mean(Reduced_Mobility$grocery_and_pharmacy_percent_change_from_baseline, na.rm = TRUE)),NA,mean(Reduced_Mobility$grocery_and_pharmacy_percent_change_from_baseline, na.rm = TRUE))
  Case_Rates_Data$parks_percent_change_from_baseline[i] <- ifelse(is.nan(mean(Reduced_Mobility$parks_percent_change_from_baseline, na.rm = TRUE)),NA,mean(Reduced_Mobility$parks_percent_change_from_baseline, na.rm = TRUE))
  Case_Rates_Data$transit_stations_percent_change_from_baseline[i] <- ifelse(is.nan(mean(Reduced_Mobility$transit_stations_percent_change_from_baseline, na.rm = TRUE)),NA,mean(Reduced_Mobility$transit_stations_percent_change_from_baseline, na.rm = TRUE))
  Case_Rates_Data$workplaces_percent_change_from_baseline[i] <- ifelse(is.nan(mean(Reduced_Mobility$workplaces_percent_change_from_baseline, na.rm = TRUE)),NA,mean(Reduced_Mobility$workplaces_percent_change_from_baseline, na.rm = TRUE))
  Case_Rates_Data$residential_percent_change_from_baseline[i] <- ifelse(is.nan(mean(Reduced_Mobility$residential_percent_change_from_baseline, na.rm = TRUE)),NA,mean(Reduced_Mobility$residential_percent_change_from_baseline, na.rm = TRUE))

}



##################################################################
#MINIMUM CASE NUMBERS

#We want to set the minimum case numbers possible to be 1, to get around issues with log(0)
#There are only 73 occurrences of this!
#for(i in 1:nrow(Case_Rates_Data)){
#  if(Case_Rates_Data$Week_Cases[i] == 0){
#    Case_Rates_Data$Week_Cases[i] <- 1
#  }
#  
#  if(Case_Rates_Data$previous_week_cases[i] == 0){
#    Case_Rates_Data$previous_week_cases[i] <- 1
#  }
#  if(Case_Rates_Data$next_week_cases[i] == 0){
#    Case_Rates_Data$next_week_cases[i] <- 1
#  }
#}

#What about for the Linelist data though?
sum(Case_Rates_Data$Linelist_P2_PCR_Week_Cases == 0)
#There are 7960 instances of it. That's a lot! 17% of all the data. 
#Keep in mind most of these are well into 2022 though, maybe beyond our region of interest
#Hence why I've switched this chunk off for now.

########################################################
#Variant Proportion

#Here we record the proportion of cases by variant as a covariate
#This is too sparse at LTLA level, so we provide aggregates at
#NHS region instead

#We use this file to help us link LTLAs to regions
LTLA_to_region <- read.csv('Data/LTLA_to_Region.csv')

#We have two sources of data
#The first is the one taken from the UK COVID portal, there is a live API:
#Portal_Variant_Data <- read.csv('https://api.coronavirus.data.gov.uk/v2/data?areaType=region&metric=variants&format=csv')
#But for consistency we use the hard-saved version downloaded on Nov 28th 2022:
Portal_Variant_Data <- read.csv("Data/Portal_variant_data.csv")
Portal_Variant_Data <- Portal_Variant_Data[,-c(1,3)]
Portal_Variant_Data$date <- as.Date(Portal_Variant_Data$date)

#We rename the regions to the sircovid format
Portal_Variant_Data %>%
  mutate(across('areaName', str_replace, 'North East', 'north_east_and_yorkshire')) %>%
  mutate(across('areaName', str_replace, 'North West', 'north_west')) %>%
  mutate(across('areaName', str_replace, 'Yorkshire and The Humber', 'north_east_and_yorkshire')) %>%
  mutate(across('areaName', str_replace, 'East Midlands', 'midlands')) %>%
  mutate(across('areaName', str_replace, 'West Midlands', 'midlands')) %>%
  mutate(across('areaName', str_replace, 'East of England', 'east_of_england')) %>%
  mutate(across('areaName', str_replace, 'London', 'london')) %>%
  mutate(across('areaName', str_replace, 'South East', 'south_east')) %>%
  mutate(across('areaName', str_replace, 'South West', 'south_west')) -> Portal_Variant_Data

#Need to multiply out the number of cases for Portal_Variant_Data
#To then aggregate back into a 7-region format
Portal_Variant_Data$cases <- round(Portal_Variant_Data$cumWeeklySequenced*(Portal_Variant_Data$newWeeklyPercentage/100))
Portal_Variant_Data <- Portal_Variant_Data[,-c(4,5)]

#Aggregate
Portal_Variant_Data <- aggregate(Portal_Variant_Data$cases,
                              by=list(date=Portal_Variant_Data$date, 
                                      nhs_region = Portal_Variant_Data$areaName, 
                                      variant = Portal_Variant_Data$variant),
                              FUN=sum)

colnames(Portal_Variant_Data) <- c("date", "nhs_region",
                                   "variant", "cases")


group_by(Portal_Variant_Data, date, nhs_region) %>% 
  mutate(newWeeklyPercentage = 100*(cases/sum(cases))) -> Portal_Variant_Data


#We also have another source, aggregated from the VAM linelist.
#This has the benefit of covering the Alpha window. The above
#data only covers Feb-21 onwards.
VAM_Variant_Data <- read.csv("Data/VAM_variant_data.csv")

VAM_Variant_Data %>%
  mutate(across('nhs_region', str_replace, 'North East and Yorkshire', 'north_east_and_yorkshire')) %>%
  mutate(across('nhs_region', str_replace, 'North West', 'north_west')) %>%
  mutate(across('nhs_region', str_replace, 'Midlands', 'midlands')) %>%
  mutate(across('nhs_region', str_replace, 'East of England', 'east_of_england')) %>%
  mutate(across('nhs_region', str_replace, 'London', 'london')) %>%
  mutate(across('nhs_region', str_replace, 'South East', 'south_east')) %>%
  mutate(across('nhs_region', str_replace, 'South West', 'south_west')) -> VAM_Variant_Data

VAM_Variant_Data$date <- as.Date(VAM_Variant_Data$date)

#Now make sure they have the same variant names
#A reminder that in the VAM data, undetermined means we don't know what it is
#Unclassified means we do, but it's not major.
#Filter out all the undetermined to start with:
VAM_Variant_Data <- filter(VAM_Variant_Data, variant != "Undetermined")

VAM_Variant_Data %>%
  mutate(across('variant', str_replace, 'Unclassified', 'Other')) %>%
  mutate(across('variant', str_replace, 'VUI-22JAN-01', 'Omicron_BA_2')) %>%
  mutate(across('variant', str_replace, 'VOC-20DEC-01', 'Alpha')) %>%
  mutate(across('variant', str_replace, 'VOC-21APR-02', 'Delta')) %>%
  mutate(across('variant', str_replace, 'VOC-21NOV-01', 'Omicron_BA_1')) %>%
  mutate(across('variant', str_replace, 'VUI-21OCT-01', 'Delta_AY_4_2')) %>%
  mutate(across('variant', str_replace, 'V-22APR-04', 'Omicron_BA_5')) %>%
  mutate(across('variant', str_replace, 'V-22APR-03', 'Omicron_BA_4')) %>%
  mutate(across('variant', str_replace, 'V-22OCT-01', 'Omicron_BQ_1')) -> VAM_Variant_Data

Portal_Variant_Data$variant <- str_replace_all(Portal_Variant_Data$variant, " ", "_")
Portal_Variant_Data$variant <- str_replace_all(Portal_Variant_Data$variant, "\\.", "_")
Portal_Variant_Data$variant <- str_replace_all(Portal_Variant_Data$variant, "\\(", "")
Portal_Variant_Data$variant <- str_replace_all(Portal_Variant_Data$variant, "\\)", "")


Portal_Variant_Data %>%
  mutate(across('variant', str_replace, 'VOC-22JAN-01_Omicron_BA_2', 'Omicron_BA_2')) %>%
  mutate(across('variant', str_replace, 'V-20DEC-01_Alpha', 'Alpha')) %>%
  mutate(across('variant', str_replace, 'V-21APR-02_Delta_B_1_617_2', 'Delta')) %>%
  mutate(across('variant', str_replace, 'VOC-21NOV-01_Omicron_BA_1', 'Omicron_BA_1')) %>%
  mutate(across('variant', str_replace, 'V-21OCT-01_Delta_AY_4_2', 'Delta_AY_4_2')) %>%
  mutate(across('variant', str_replace, 'VOC-22APR-04_Omicron_BA_5', 'Omicron_BA_5')) %>%
  mutate(across('variant', str_replace, 'VOC-22APR-03_Omicron_BA_4', 'Omicron_BA_4')) %>%
  mutate(across('variant', str_replace, 'V-22OCT-01_Omicron_BQ_1', 'Omicron_BQ_1')) -> Portal_Variant_Data
  
#Cut the earlier dates
VAM_Variant_Data <- filter(VAM_Variant_Data, date > minimum_date)

#Aggregate the VAM data into weeks
VAM_Variant_Data$date <- cut.Date(VAM_Variant_Data$date, 
                                  breaks = "week", start.on.monday = FALSE)

VAM_Variant_Data <- aggregate(VAM_Variant_Data$cases,
                              by=list(date=VAM_Variant_Data$date, 
                                      nhs_region = VAM_Variant_Data$nhs_region, 
                                      variant = VAM_Variant_Data$variant),
                              FUN=sum)


colnames(VAM_Variant_Data) <- c("date", "nhs_region", "variant", "cases")

VAM_Variant_Data$date <- as.Date(VAM_Variant_Data$date)
#Calculate the percentages for the VAM data:
group_by(VAM_Variant_Data, date, nhs_region) %>% 
  mutate(newWeeklyPercentage = 100*(cases/sum(cases))) -> VAM_Variant_Data

#We quickly plot the two datasets together to see how they vary 

VAM_Variant_Data$data_source <- "VAM"
Portal_Variant_Data$data_source <- "Portal"

plot_test <- rbind(VAM_Variant_Data, Portal_Variant_Data)
# 
# nhs_list <- unique(plot_test$nhs_region)
# ggplot(plot_test) +
#   geom_line(aes(x = date, y = newWeeklyPercentage, color = variant, 
#                 lty = data_source)) + 
#   facet_wrap(~ nhs_region)

#Good to see the datasets align near perfectly, save for some disagreement
#during the rise in Omicron BA.4
#We will use the Portal data but append the earlier VAM data to the list.

#So cut the later VAM data:
VAM_Variant_Data <- filter(VAM_Variant_Data, date < as.Date("2021-02-14"))

#There's technically a few Deltas in the VAM early dates, which is odd. 
#Will leave it there for now

#Stitch the two together
Variant_Data <- rbind(VAM_Variant_Data, Portal_Variant_Data)

ggplot(Variant_Data) +
  geom_line(aes(x = date, y = newWeeklyPercentage, color = variant,
                lty = data_source)) +
  facet_wrap(~ nhs_region) -> variant_plot

#ggsave("Outputs/Variants.png", width = 35, height = 20, units = "cm")
ggsave(filename = "Variants.png",
       path = 'Outputs/', plot = variant_plot,
       dpi=300, 
       height=20, width=35, units="cm")


write.csv(Variant_Data, file = "Outputs/Variants_data.csv")


#Now add this to Case_Rates_Data
#We want a separate column for each variant
Variant_Data <- Variant_Data[,-c(4,6)]

Variant_Data %>%
  pivot_wider(names_from = variant, 
              values_from = newWeeklyPercentage) -> Variant_Data

#Set all the NAs to 0s
Variant_Data[is.na(Variant_Data)] <- 0

Case_Rates_Data$Alpha_proportion <- NA
Case_Rates_Data$Delta_proportion <- NA
Case_Rates_Data$Delta_AY_4_2_proportion <- NA
Case_Rates_Data$Omicron_BA_1_proportion <- NA
Case_Rates_Data$Omicron_BA_2_proportion <- NA
Case_Rates_Data$Other_proportion <- NA
Case_Rates_Data$Omicron_BQ_1_proportion <- NA
Case_Rates_Data$Omicron_BA_4_proportion <- NA
Case_Rates_Data$Omicron_BA_5_proportion <- NA

Available_codes <- unique(LTLA_to_region$LAD21CD)

for(i in 1:nrow(Case_Rates_Data)){
  #read date
  date_hold <- Case_Rates_Data$date_begin[i]
  LTLA_hold <- Case_Rates_Data$areaCode[i]
  #Check we have data available
  
  
  if(!(LTLA_hold %in% Available_codes)){
    print(sprintf("Warning, unrecognised LTLA code in row %s", i)) #Note, this will happen for Scotland, Wales, NI
  }else{
    region_hold <- LTLA_to_region$RGN21NM[which(LTLA_to_region$LAD21CD == LTLA_hold)]
    
    variant_reduced <- filter(Variant_Data, nhs_region == region_hold)
    variant_reduced <- filter(variant_reduced, date == date_hold)
    
    Case_Rates_Data$Alpha_proportion[i] <- variant_reduced$Alpha[1]
    Case_Rates_Data$Delta_proportion[i] <- variant_reduced$Delta[1]
    Case_Rates_Data$Delta_AY_4_2_proportion[i] <- variant_reduced$Delta_AY_4_2[1]
    Case_Rates_Data$Omicron_BA_1_proportion[i] <- variant_reduced$Omicron_BA_1[1]
    Case_Rates_Data$Omicron_BA_2_proportion[i] <- variant_reduced$Omicron_BA_2[1]
    Case_Rates_Data$Other_proportion[i] <- variant_reduced$Other[1]
    Case_Rates_Data$Omicron_BQ_1_proportion[i] <- variant_reduced$Omicron_BQ_1[1]
    Case_Rates_Data$Omicron_BA_4_proportion[i] <- variant_reduced$Omicron_BA_4[1]
    Case_Rates_Data$Omicron_BA_5_proportion[i] <- variant_reduced$Omicron_BA_5[1]
    
  }
  
}

##################
# The downside to the VAM data is that it's only at the NHS region level
# the s-gene target failure (SGTF) data meanwhile is at the LTLA level, and can be used
# as a proxy to signal variant, if we pick certain dates at which to interpret s_pos
# and s_neg as particular variants. We build these columns below:

#Load in the SGTF data
SGTF_Data <- read.csv("Data/s_by_ltla_pillar2_pcr.csv")

#First, let's collapse this to be weekly sums as opposed to daily.
SGTF_Data$specimen_date <- as.Date(SGTF_Data$specimen_date)

SGTF_Data$date <- cut.Date(SGTF_Data$specimen_date, 
                                  breaks = "week", start.on.monday = FALSE)

#We're going to alter the troublesome LTLA codes now before we aggregate;
#Three england LTLAs not covered: "E06000060" "E06000061" "E06000062"
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

SGTF_Data$ltla_code[which(SGTF_Data$ltla_code %in% Buck_codes)] <- "E06000060"
SGTF_Data$ltla_code[which(SGTF_Data$ltla_code %in% North_North_codes)] <- "E06000061"
SGTF_Data$ltla_code[which(SGTF_Data$ltla_code %in% West_North_codes)] <- "E06000062"

SGTF_Data <- aggregate(cbind(SGTF_Data$s_positive, SGTF_Data$s_negative),
                              by=list(date=SGTF_Data$date, 
                                      ltla_code = SGTF_Data$ltla_code),
                              FUN=sum)


colnames(SGTF_Data) <- c("date", "ltla_code", "s_positive", "s_negative")

SGTF_Data$date <- as.Date(SGTF_Data$date)

#Second, we double check that all LTLAs in the data exist in the Case_Rates_Data and vice versa
Case_Data_areaCodes <- unique(Case_Rates_Data$areaCode)
SGTF_areaCodes <- unique(SGTF_Data$ltla_code)
#Note that we don't have SGTF for the devolved nations
Case_Data_areaCodes[which(!(Case_Data_areaCodes %in% SGTF_areaCodes))]
SGTF_areaCodes[which(!(SGTF_areaCodes %in% Case_Data_areaCodes))]
#Only ones we don't use are Isle of Wight, Isles of Scilly, and City of London, which is what we expected


#Next, we need to define how we interpret s_pos and s_neg at certain time-points
#Key Dates:
#START - 2021-03-21 : For these weeks, s_positive is wild type, and s-negative is Alpha
#2021-03-28 - 2021-09-05 : For these weeks, s_positive is Delta, and s-negative is Alpha
#2021-09-12 - 2021-12-26: For these weeks, s_positive is Delta, and s-negative is Omicron 
#2022-01-02 - From this point on, we just say it's all Omicron, as BA.2 is s-positive, hard to capture the blur out of delta
#Obviously this is earlier than likely for Omicron, but it doesn't matter, we just need to be confident Alpha has burned out.

#Note, SGTF data ends at May 29th 2022, after which we call it all Omicron
Case_Rates_Data$s_Wild_prop <- NA
Case_Rates_Data$s_Alpha_prop <- NA
Case_Rates_Data$s_Delta_prop <- NA
Case_Rates_Data$s_Omicron_prop <- NA

for(i in 1:length(Case_Rates_Data$Week)){
  
  date_hold <- Case_Rates_Data$date_begin[i]
  areaCode_hold <- Case_Rates_Data$areaCode[i]
  
  s_pos_hold <- SGTF_Data$s_positive[which((SGTF_Data$date == date_hold)&(SGTF_Data$ltla_code == areaCode_hold))]
  s_neg_hold <- SGTF_Data$s_negative[which((SGTF_Data$date == date_hold)&(SGTF_Data$ltla_code == areaCode_hold))]
  s_total_hold <- s_pos_hold + s_neg_hold
  if(length(s_total_hold) == 0){
    s_total_hold <- 0
  }

  if(date_hold < as.Date("2021-03-28")){
    Case_Rates_Data$s_Delta_prop[i] <- 0
    Case_Rates_Data$s_Omicron_prop[i] <- 0
    if(s_total_hold == 0){
      Case_Rates_Data$s_Wild_prop[i] <- 1
      Case_Rates_Data$s_Alpha_prop[i] <- 0
    } else{
      Case_Rates_Data$s_Wild_prop[i] <- s_pos_hold/s_total_hold
      Case_Rates_Data$s_Alpha_prop[i] <- s_neg_hold/s_total_hold
    }
  }else if(date_hold < as.Date("2021-09-12")){
    Case_Rates_Data$s_Omicron_prop[i] <- 0
    Case_Rates_Data$s_Wild_prop[i] <- 0
    if(s_total_hold == 0){
      Case_Rates_Data$s_Delta_prop[i] <- 1
      Case_Rates_Data$s_Alpha_prop[i] <- 0
    }else{
      Case_Rates_Data$s_Delta_prop[i] <- s_pos_hold/s_total_hold
      Case_Rates_Data$s_Alpha_prop[i] <- s_neg_hold/s_total_hold
    }
  }else if(date_hold < as.Date("2022-01-12")){
    Case_Rates_Data$s_Wild_prop[i] <- 0
    Case_Rates_Data$s_Alpha_prop[i] <- 0
    if(s_total_hold == 0){
      Case_Rates_Data$s_Delta_prop[i] <- 0
      Case_Rates_Data$s_Omicron_prop[i] <- 1
    } else{
      Case_Rates_Data$s_Delta_prop[i] <- s_pos_hold/s_total_hold
      Case_Rates_Data$s_Omicron_prop[i] <- s_neg_hold/s_total_hold
    }
      
  } else{
    Case_Rates_Data$s_Wild_prop[i] <- 0
    Case_Rates_Data$s_Alpha_prop[i] <- 0
      Case_Rates_Data$s_Delta_prop[i] <- 0
      Case_Rates_Data$s_Omicron_prop[i] <- 1
  }
  
  }

#Now add funding data.
#Start with NHS Funding data, as taken from the allocation doc found here: 
#https://www.england.nhs.uk/allocations/previous/allocations-for-2019-20-to-2023-24/
#Note, this seems to miss some reported extra COVID surge funding which I'm struggling to find
#info on how it was allocated.
CCG_LTLA_lookup <- read.csv("Data/LTLA_to_CCG_codes.csv")
NHS_funding_data <- read.csv("Data/NHS_funding_allocations.csv")

Case_Data_Codes <- unique(Case_Rates_Data$areaCode)
Lookup_codes <- unique(CCG_LTLA_lookup[,1])

cols_hold <- colnames(CCG_LTLA_lookup)
cols_hold[1] <- "areaCode"
colnames(CCG_LTLA_lookup) <- cols_hold

CCG_LTLA_lookup <- CCG_LTLA_lookup[,c(1,5,6)]

Case_Rates_Data <- left_join(Case_Rates_Data, CCG_LTLA_lookup, by = 'areaCode')

#Now add the funding data
cols_hold <- colnames(NHS_funding_data)
cols_hold[1] <- "CCG_code"
colnames(NHS_funding_data) <- cols_hold

CCG_Case_codes <- unique(Case_Rates_Data$CCG_2019_Code)
Funding_CCG_codes <- unique(NHS_funding_data$CCG_code)
CCG_Case_codes %in% Funding_CCG_codes
#For some reason, the NHS Funding data stream here is using two different CCGs for Devon (99P and 99Q)
#This hasn't been correctly changed to 15N, so change all appearances of 15N in Case rates to "QJK" which is the DEvon STP
Case_Rates_Data$CCG_2019_Code[which(Case_Rates_Data$CCG_2019_Code == "15N")] <- "QJK"

Case_Rates_Data$NHS_registered_population <- NA

Case_Rates_Data$Core_services_funding <- NA
Case_Rates_Data$Primary_care_funding <- NA
Case_Rates_Data$Specialised_services <- NA

#The allocated funds are designed via a algorithm that calculates a weighted population need
#This column is the above total funding but divided by that weighted population
Case_Rates_Data$Core_services_funding_by_weighted <- NA
Case_Rates_Data$Primary_care_funding_by_weighted <- NA
Case_Rates_Data$Specialised_services_by_weighted <- NA

#TODO: Make more efficient
for(i in 1:length(Case_Rates_Data$areaCode)){
  if(Case_Rates_Data$CCG_2019_Code[i] != ""){
date_hold <- Case_Rates_Data$date_begin[i]
CCG_hold <- Case_Rates_Data$CCG_2019_Code[i]

if(date_hold < as.Date("2021-04-06")){
  funding_hold <- filter(NHS_funding_data, tax_year_start == 2020)
  funding_hold <- filter(funding_hold, CCG_code == CCG_hold)
  funding_hold_core <- filter(funding_hold, funding_type == "core_services")
  funding_hold_primary <- filter(funding_hold, funding_type == "primary_care")
  funding_hold_specialised <- filter(funding_hold, funding_type == "specialised_services")
  
  Case_Rates_Data$NHS_registered_population[i] <- funding_hold$registered_population[1]
  
  Case_Rates_Data$Core_services_funding[i] <- funding_hold_core$funding_allocation[1]
  Case_Rates_Data$Primary_care_funding[i] <- funding_hold_primary$funding_allocation[1]
  Case_Rates_Data$Specialised_services[i] <- funding_hold_specialised$funding_allocation[1]
  
  Case_Rates_Data$Core_services_funding_by_weighted[i] <- funding_hold_core$funding_allocation[1] / funding_hold_core$weighted_population[1]
  Case_Rates_Data$Primary_care_funding_by_weighted[i] <- funding_hold_primary$funding_allocation[1] / funding_hold_primary$weighted_population[1]
  Case_Rates_Data$Specialised_services_by_weighted[i] <- funding_hold_specialised$funding_allocation[1] / funding_hold_specialised$weighted_population[1]
  
} else if(date_hold < as.Date("2022-04-06")){
  funding_hold <- filter(NHS_funding_data, tax_year_start == 2021)
  funding_hold <- filter(funding_hold, CCG_code == CCG_hold)
  funding_hold_core <- filter(funding_hold, funding_type == "core_services")
  funding_hold_primary <- filter(funding_hold, funding_type == "primary_care")
  funding_hold_specialised <- filter(funding_hold, funding_type == "specialised_services")
  
  Case_Rates_Data$NHS_registered_population[i] <- funding_hold$registered_population[1]
  
  Case_Rates_Data$Core_services_funding[i] <- funding_hold_core$funding_allocation[1]
  Case_Rates_Data$Primary_care_funding[i] <- funding_hold_primary$funding_allocation[1]
  Case_Rates_Data$Specialised_services[i] <- funding_hold_specialised$funding_allocation[1]
  
  Case_Rates_Data$Core_services_funding_by_weighted[i] <- funding_hold_core$funding_allocation[1] / funding_hold_core$weighted_population[1]
  Case_Rates_Data$Primary_care_funding_by_weighted[i] <- funding_hold_primary$funding_allocation[1] / funding_hold_primary$weighted_population[1]
  Case_Rates_Data$Specialised_services_by_weighted[i] <- funding_hold_specialised$funding_allocation[1] / funding_hold_specialised$weighted_population[1]
  
} else{
  funding_hold <- filter(NHS_funding_data, tax_year_start == 2022)
  funding_hold <- filter(funding_hold, CCG_code == CCG_hold)
  funding_hold_core <- filter(funding_hold, funding_type == "core_services")
  funding_hold_primary <- filter(funding_hold, funding_type == "primary_care")
  funding_hold_specialised <- filter(funding_hold, funding_type == "specialised_services")
  
  Case_Rates_Data$NHS_registered_population[i] <- funding_hold$registered_population[1]
  
  Case_Rates_Data$Core_services_funding[i] <- funding_hold_core$funding_allocation[1]
  Case_Rates_Data$Primary_care_funding[i] <- funding_hold_primary$funding_allocation[1]
  Case_Rates_Data$Specialised_services[i] <- funding_hold_specialised$funding_allocation[1]
  
  Case_Rates_Data$Core_services_funding_by_weighted[i] <- funding_hold_core$funding_allocation[1] / funding_hold_core$weighted_population[1]
  Case_Rates_Data$Primary_care_funding_by_weighted[i] <- funding_hold_primary$funding_allocation[1] / funding_hold_primary$weighted_population[1]
  Case_Rates_Data$Specialised_services_by_weighted[i] <- funding_hold_specialised$funding_allocation[1] / funding_hold_specialised$weighted_population[1]
  
}

}
}


#Now add specific COVID support funds provided, as taken from here:
#https://www.gov.uk/government/publications/covid-19-emergency-funding-for-local-government
#ASC is adult social care, and was basically for paying peoples wages when they had covid.
#Outbreak management details here:
#https://www.gov.uk/government/publications/contain-outbreak-management-fund-2021-to-2022/contain-outbreak-management-fund-guidance-financial-year-2021-to-2022
  
COVID_funding <- read.csv("Data/LTLA_Covid_Extra_Funding.csv")
funding_types <- unique(COVID_funding$fund)
ONS_codes <- unique(COVID_funding$ONS_code)
#All the England codes are captured in this funding block
#Turn funding type into a variable
COVID_funding <- pivot_wider(COVID_funding, names_from = fund, values_from = value)

Case_Rates_Data[funding_types] <- NA

#Now, the first problem, is that some funding was given to LTLAs (included), while 
#other funding was given to UTLAs. So we need to roughly divide up this UTLA funding amongst the LTLAs
#We'll do this by LTLA population, seems reasonable.

#First, split the data into those that we currently track (LTLAs) and those we don't
COVID_funding_LTLA <- filter(COVID_funding, ONS_code %in% Case_Data_Codes)
COVID_funding_other <- filter(COVID_funding, !(ONS_code %in% Case_Data_Codes))

#We have 30 fire services. I'm going to remove these
COVID_funding_other <- filter(COVID_funding_other, !grepl('E31', ONS_code))

LTLA_to_UTLA <- read.csv("Data/LTLA_to_UTLA_codes.csv")
UTLA_codes <- unique(LTLA_to_UTLA$UTLA21CD)
sum(UTLA_codes %in% COVID_funding_other$ONS_code) #There's 27 codes we can divide up further
LTLA_Populations <- data.frame(areaCode = Case_Rates_Data$areaCode,
                               Population = Case_Rates_Data$Population)
LTLA_Populations <- distinct(LTLA_Populations)

COVID_funding_UTLA <- filter(COVID_funding_other, ONS_code %in% UTLA_codes)
COVID_funding_other <- filter(COVID_funding_other, !(ONS_code %in% UTLA_codes))

for(i in 1:length(COVID_funding_UTLA$ONS_code)){
  UTLA_code_hold <- COVID_funding_UTLA$ONS_code[i]
  tax_year_hold <- COVID_funding_UTLA$tax_year_start[i]
  
  #extract which LTLAs this will be divided by:
  LTLAs_hold <- filter(LTLA_to_UTLA, UTLA21CD == UTLA_code_hold)
  LTLA_codes_hold <- unique(LTLAs_hold$LTLA21CD)
  Populations_hold <- filter(LTLA_Populations, areaCode %in% LTLA_codes_hold)
  Population_sum <- sum(Populations_hold$Population)
  
  for(j in 1:length(LTLAs_hold$LTLA21CD)){
    LTLA_code_hold <- LTLAs_hold$LTLA21CD[j]
    
    #There are three specialist cases: Isle of Wight, Isles of Scilly, City of London
    if(LTLA_code_hold == "E06000046"){} else #Isle of Wight
      if(LTLA_code_hold == "E06000053"){
        LTLA_code_hold <- "E06000052" #If Scilly, change to Cornwall
        funding_vector <- COVID_funding_UTLA[i,4:25]

        #Now add this vector to the LTLA version
        if(!(LTLA_code_hold %in% unique(COVID_funding_LTLA$ONS_code))){
          sprintf("Error: We don't have an LTLA code (%s) to give this to.",LTLA_code_hold)
        }
        COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == tax_year_hold)&(COVID_funding_LTLA$ONS_code == LTLA_code_hold)), 4:25] <- COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == tax_year_hold)&(COVID_funding_LTLA$ONS_code == LTLA_code_hold)), 4:25] + funding_vector
        
      } else 
      if(LTLA_code_hold == "E09000001"){
        LTLA_code_hold <- "E09000012" #If City of London, change to Hackney
        funding_vector <- COVID_funding_UTLA[i,4:25]
        
        #Now add this vector to the LTLA version
        if(!(LTLA_code_hold %in% unique(COVID_funding_LTLA$ONS_code))){
          sprintf("Error: We don't have an LTLA code (%s) to give this to.",LTLA_code_hold)
        }
        COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == tax_year_hold)&(COVID_funding_LTLA$ONS_code == LTLA_code_hold)), 4:25] <- COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == tax_year_hold)&(COVID_funding_LTLA$ONS_code == LTLA_code_hold)), 4:25] + funding_vector
        
      } else{
    
    specific_population_hold <- Populations_hold$Population[which(Populations_hold$areaCode == LTLA_code_hold)]
    funding_vector <- COVID_funding_UTLA[i,4:25]
    funding_vector <- funding_vector*(specific_population_hold/Population_sum)
    
    #Now add this vector to the LTLA version
    if(!(LTLA_code_hold %in% unique(COVID_funding_LTLA$ONS_code))){
      sprintf("Error: We don't have an LTLA code (%s) to give this to.",LTLA_code_hold)
    }
    COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == tax_year_hold)&(COVID_funding_LTLA$ONS_code == LTLA_code_hold)), 4:25] <- COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == tax_year_hold)&(COVID_funding_LTLA$ONS_code == LTLA_code_hold)), 4:25] + funding_vector
    
    
  }
  }
}


#Next, we need to divide up this Greater London extra funding across the 32 london districts we include
COVID_funding_london <- filter(COVID_funding_other, ONS_code == "-")
COVID_funding_other <- filter(COVID_funding_other, !(ONS_code == "-"))
London_populations <- filter(LTLA_Populations, grepl('E09', areaCode))
London_sum_populations <- sum(London_populations$Population)
funding_vector <- COVID_funding_london[1,4:25]
for(i in length(London_populations$areaCode)){
  LTLA_code_hold <- London_populations$areaCode[i]
  funding_vector <- funding_vector*(London_populations$Population[i]/London_sum_populations)
  COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == 2020)&(COVID_funding_LTLA$ONS_code == LTLA_code_hold)), 4:25] <- COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == 2020)&(COVID_funding_LTLA$ONS_code == LTLA_code_hold)), 4:25] + funding_vector
}
funding_vector <- COVID_funding_london[2,4:25]
for(i in length(London_populations$areaCode)){
  LTLA_code_hold <- London_populations$areaCode[i]
  funding_vector <- funding_vector*(London_populations$Population[i]/London_sum_populations)
  COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == 2021)&(COVID_funding_LTLA$ONS_code == LTLA_code_hold)), 4:25] <- COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == 2020)&(COVID_funding_LTLA$ONS_code == LTLA_code_hold)), 4:25] + funding_vector
}

#Lastly, we need to sort out the northamptonshire issue that's been coming up constantly
#We need to divide the northamptonshire funding across the seven sub-regions, then split that into the separate two councils
northamptonshire_codes <- unique(COVID_funding_other$ONS_code)
northamptonshire_codes <- northamptonshire_codes[-6]

west_north_fund_2020 <- filter(COVID_funding_other, tax_year_start == 2020)
west_north_fund_2020 <- filter(west_north_fund_2020, 
                               ONS_code %in% c("E07000154", "E07000155", "E07000151"))
west_north_fund_2020 %>%
  adorn_totals("row") -> west_north_fund_2020
west_north_fund_2020$ONS_code[4] <- "E06000062"
west_north_fund_2020$ONS_name[4] <- "West Northamptonshire"
west_north_fund_2020 <- west_north_fund_2020[4,]
west_north_fund_2020$tax_year_start <- 2020
west_north_fund_2020[1,4:25] <- west_north_fund_2020[1,4:25] + (COVID_funding_other[which((COVID_funding_other$tax_year_start == 2020)&(COVID_funding_other$ONS_code == "E10000021")),4:25]*(406733/(406733+350448)))

west_north_fund_2021 <- filter(COVID_funding_other, tax_year_start == 2021)
west_north_fund_2021 <- filter(west_north_fund_2021, 
                               ONS_code %in% c("E07000154", "E07000155", "E07000151"))
west_north_fund_2021 %>%
  adorn_totals("row") -> west_north_fund_2021
west_north_fund_2021$ONS_code[4] <- "E06000062"
west_north_fund_2021$ONS_name[4] <- "West Northamptonshire"
west_north_fund_2021 <- west_north_fund_2021[4,]
west_north_fund_2021$tax_year_start <- 2021
west_north_fund_2021[1,4:25] <- west_north_fund_2021[1,4:25] + (COVID_funding_other[which((COVID_funding_other$tax_year_start == 2021)&(COVID_funding_other$ONS_code == "E10000021")),4:25]*(406733/(406733+350448)))


north_north_fund_2020 <- filter(COVID_funding_other, tax_year_start == 2020)
north_north_fund_2020 <- filter(north_north_fund_2020, 
                                ONS_code %in% c("E07000152", "E07000156", "E07000150", "E07000153"))
north_north_fund_2020 %>%
  adorn_totals("row") -> north_north_fund_2020
north_north_fund_2020$ONS_code[5] <- "E06000061"
north_north_fund_2020$ONS_name[5] <- "North Northamptonshire"
north_north_fund_2020 <- north_north_fund_2020[5,]
north_north_fund_2020$tax_year_start <- 2020
north_north_fund_2020[1,4:25] <- north_north_fund_2020[1,4:25] + (COVID_funding_other[which((COVID_funding_other$tax_year_start == 2020)&(COVID_funding_other$ONS_code == "E10000021")),4:25]*(350448/(406733+350448)))


north_north_fund_2021 <- filter(COVID_funding_other, tax_year_start == 2021)
north_north_fund_2021 <- filter(north_north_fund_2021, 
                                ONS_code %in% c("E07000152", "E07000156", "E07000150", "E07000153"))
north_north_fund_2021 %>%
  adorn_totals("row") -> north_north_fund_2021
north_north_fund_2021$ONS_code[5] <- "E06000061"
north_north_fund_2021$ONS_name[5] <- "North Northamptonshire"
north_north_fund_2021 <- north_north_fund_2021[5,]
north_north_fund_2021$tax_year_start <- 2021
north_north_fund_2021[1,4:25] <- north_north_fund_2021[1,4:25] + (COVID_funding_other[which((COVID_funding_other$tax_year_start == 2021)&(COVID_funding_other$ONS_code == "E10000021")),4:25]*(350448/(406733+350448)))

COVID_funding_Northamptonshire <- rbind(west_north_fund_2020, west_north_fund_2021, north_north_fund_2020, north_north_fund_2021)

COVID_funding_LTLA <- rbind(COVID_funding_LTLA, COVID_funding_Northamptonshire)

#There are some existing payments to North northamptonshire, add the two together
COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == 2020)&(COVID_funding_LTLA$ONS_name =="North Northamptonshire" )),4:25] <- COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == 2020)&(COVID_funding_LTLA$ONS_name =="North Northamptonshire" )),4:25] + COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == 2020)&(COVID_funding_LTLA$ONS_name =="North Northamptonshire4" )),4:25]
COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == 2021)&(COVID_funding_LTLA$ONS_name =="North Northamptonshire" )),4:25] <- COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == 2021)&(COVID_funding_LTLA$ONS_name =="North Northamptonshire" )),4:25] + COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == 2021)&(COVID_funding_LTLA$ONS_name =="North Northamptonshire4" )),4:25]

COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == 2020)&(COVID_funding_LTLA$ONS_name =="West Northamptonshire" )),4:25] <- COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == 2020)&(COVID_funding_LTLA$ONS_name =="West Northamptonshire" )),4:25] + COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == 2020)&(COVID_funding_LTLA$ONS_name =="West Northamptonshire5" )),4:25]
COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == 2021)&(COVID_funding_LTLA$ONS_name =="West Northamptonshire" )),4:25] <- COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == 2021)&(COVID_funding_LTLA$ONS_name =="West Northamptonshire" )),4:25] + COVID_funding_LTLA[which((COVID_funding_LTLA$tax_year_start == 2021)&(COVID_funding_LTLA$ONS_name =="West Northamptonshire5" )),4:25]

#Remove the old ones
COVID_funding_LTLA <- filter(COVID_funding_LTLA, !(ONS_name %in% c("North Northamptonshire4", "West Northamptonshire5") ))

#Now, assign those LTLA funding values to the Case Rates dataframe
#TODO: make more efficient (set a tax year column and "merge" the dataframes)

case_rates_cols <- ncol(Case_Rates_Data)

for(i in 1:length(Case_Rates_Data$areaCode)){
  date_hold <- Case_Rates_Data$date_begin[i]
  areaCode_hold <- Case_Rates_Data$areaCode[i]
  
  if(areaCode_hold %in% ONS_codes){
    
    if(date_hold < as.Date("2021-04-06")){
      
      extra_funding_hold <- filter(COVID_funding_LTLA, tax_year_start == 2020)
      extra_funding_hold <- filter(extra_funding_hold, ONS_code == areaCode_hold)
      
      Case_Rates_Data[i,(case_rates_cols-21):case_rates_cols] <- extra_funding_hold[4:25]
      
    } else{
      extra_funding_hold <- filter(COVID_funding_LTLA, tax_year_start == 2021)
      extra_funding_hold <- filter(extra_funding_hold, ONS_code == areaCode_hold)
      
      Case_Rates_Data[i,(case_rates_cols-21):case_rates_cols] <- extra_funding_hold[4:25]
    }
    
  }
}

#Lastly, we want to export some midpoint coordinates for more spatially-specific spatial kernels
sf_cent <- st_centroid(Boundaries)
sf_cent_geom <- sf_cent$geometry
#plot(sf_cent_geom)

LTLA_centroids <- data.frame(areaCode = sf_cent$CODE, centroid_x = NA, centroid_y = NA)
for(i in 1:356){
  LTLA_centroids$centroid_x[i] <- as.numeric(sf_cent$geometry[[i]])[1]
  LTLA_centroids$centroid_y[i] <- as.numeric(sf_cent$geometry[[i]])[2]
}

Case_Rates_Data <- merge(Case_Rates_Data, LTLA_centroids, by = c('areaCode'), all.x = TRUE)


############################################ 
# Don't need this block anymore, we attached first episodes at the top!
# ###############################################################################
# #We want to keep a tally of the total cumulative number of cases in the region to date... 
# 
# #We do this using the cumulative number of "first episodes" from the dashboard. API link:
# #https://api.coronavirus.data.gov.uk/v2/data?areaType=ltla&metric=cumFirstEpisodesBySpecimenDate&format=csv
# 
# #But we saved a hard copy:
# first_episodes_data <- read.csv("Data/ltla_first_episodes_2023_06_22.csv")
# first_episodes_data$date <- as.Date(first_episodes_data$date)
# #BUT, unfortunately we need to do all the annoying LTLA boundary re-processing again... so,
# #for now, we just sum up after May 10th
# ###########
# #TODO: Use the official sum numbers
# #Note that, on the one hand, we're missing the first few months this way, but we're then also double-counting the first episodes
# ############
# Case_Rates_Data$total_cases <- NA
# for(i in 1:nrow(Case_Rates_Data)){
#   Week_hold <- Case_Rates_Data$Week[i]
#   #Week_start_hold <- max((Week_hold - 5),2)
#   Week_start_hold <- 2
#   areahold <- Case_Rates_Data$areaCode[i]
#   weeks_to_keep <- seq(Week_start_hold, Week_hold)
#   
#   Case_Rates_Data %>%
#     filter(areaCode == areahold) %>%
#     filter(Week %in% weeks_to_keep) -> data_hold
#   
#   cumCases <- sum(data_hold$Week_Cases)
#   Case_Rates_Data$total_cases[i] <- cumCases
#     
# }
# 
# sum(which(Case_Rates_Data$total_cases > Case_Rates_Data$Population))
# 
# 
# ############################################

#Done for now, export the data

write.csv(Case_Rates_Data, file = 'Outputs/Cases_Data.csv')
save(Case_Rates_Data, file = 'Outputs/Cases_Data.RData')
save(W, file = 'Outputs/W.RData')

#We also export hospitalisations by NHS region data, for use in coupled model.
#This is taken from dashboard, api link: https://api.coronavirus.data.gov.uk/v2/data?areaType=nhsRegion&metric=newAdmissions&format=csv

#But we use a hard-saved version downloaded on 16/12/2022
hospital_admissions <- read.csv("Data/hospital_admissions_nhsRegion_2022_12_15.csv")
hospital_admissions$date <- as.Date(hospital_admissions$date)
hospital_admissions <- hospital_admissions[-3]

#To aid with modelling, we also want to export a 306x7 matrix that denotes with 0/1 which LTLAs are in which region
#assign a region index
nhs_regions <- unique(hospital_admissions$areaName)
hospital_admissions$region_index <- lapply(hospital_admissions$areaName, function(x) which(nhs_regions == x))
hospital_admissions$region_index <- as.numeric(hospital_admissions$region_index)

LTLA_indexs <- Case_Rates_Data[,c(1,3,7)]
LTLA_indexs <- distinct(LTLA_indexs)

LTLA_to_region_matrix <- matrix(0, nrow = 356, ncol = 7)
lookup_regions <- c("london", "south_east", "south_west", "east_of_england", 
                    "midlands", "north_east_and_yorkshire", "north_west")

for(i in 1:length(LTLA_indexs$areaCode)){
  LTLA_coord <- LTLA_indexs$INDEX[i]
  LTLA_code_hold <- LTLA_indexs$areaCode[i]
  
  region_hold <- LTLA_to_region$RGN21NM[which(LTLA_to_region$LAD21CD == LTLA_code_hold)]
  region_coord <- which(lookup_regions == region_hold)
  
  LTLA_to_region_matrix[LTLA_coord, region_coord] <- 1
  
}
#Convert this to WEEKLY hospitalisations
#FIRST DATE: Sunday 3rd May 2020
hospital_admissions$Week <- plyr::round_any(as.numeric(hospital_admissions$date - as.Date('2020-05-03') + 0.5), 7, f = ceiling)/7

#Filter out the week 0 cases
hospital_admissions <- filter(hospital_admissions, Week > 0)

#Now sum by, everything, but not vaccination yet
hospital_admissions_hold <- aggregate(hospital_admissions$newAdmissions, by=list(Week=hospital_admissions$Week, areaCode = hospital_admissions$areaCode,
                                                                                 areaName = hospital_admissions$areaName,
                                                                 region_index = hospital_admissions$region_index), FUN=sum)

#Now need to add a "previous week cases" column, and a "date_begin" column:
colnames(hospital_admissions_hold) <- c('Week', 'areaCode', 'areaName', 'region_index', 'Week_Hospitalisations')

hospital_admissions_hold$date_begin <- as.Date('2020-05-03') + ((hospital_admissions_hold$Week - 1)*7) 

hospital_admissions <- hospital_admissions_hold
rm(hospital_admissions_hold)

#Now we add most of the covariate data to the hospital admissions dataframe too
#Population
LTLA_Populations <- distinct(Case_Rates_Data[,c(1,4)])
colnames(LTLA_to_region) <- c("row", "areaCode", "areaName", "regionCode", "regionName")
LTLA_Populations <- merge(LTLA_Populations, LTLA_to_region[,c(2,5)], by = c("areaCode"))
Region_populations <- aggregate(LTLA_Populations$Population, by = list(regionName = LTLA_Populations$regionName),
                                FUN = sum)
colnames(Region_populations) <- c("areaName", "Population")

hospital_admissions %>%
  mutate(across('areaName', str_replace, 'London', 'london')) %>%
  mutate(across('areaName', str_replace, 'South East', 'south_east')) %>%
  mutate(across('areaName', str_replace, 'South West', 'south_west')) %>%
  mutate(across('areaName', str_replace, 'East of England', 'east_of_england')) %>%
  mutate(across('areaName', str_replace, 'Midlands', 'midlands')) %>%
  mutate(across('areaName', str_replace, 'North East and Yorkshire', 'north_east_and_yorkshire')) %>%
  mutate(across('areaName', str_replace, 'North West', 'north_west'))  -> hospital_admissions

hospital_admissions <- merge(hospital_admissions, Region_populations, by = "areaName")

#Median age? Too largescale to be useful here I think

#VACCINATION. 
#We need to multiply the percentages by the population and then add them all together and divide back down.
LTLA_Vaccinations <- Case_Rates_Data[,c("areaCode","Week","Population","cumVaccPercentage_FirstDose",                       
                                        "cumVaccPercentage_SecondDose",                      
                                        "cumVaccPercentage_ThirdDose" )]

LTLA_Vaccinations$no_vaccs_one <- LTLA_Vaccinations$Population*LTLA_Vaccinations$cumVaccPercentage_FirstDose/100
LTLA_Vaccinations$no_vaccs_two <- LTLA_Vaccinations$Population*LTLA_Vaccinations$cumVaccPercentage_SecondDose/100
LTLA_Vaccinations$no_vaccs_three <- LTLA_Vaccinations$Population*LTLA_Vaccinations$cumVaccPercentage_ThirdDose/100

#Now total those three new columns by region
LTLA_Vaccinations <- merge(LTLA_Vaccinations, LTLA_to_region[,c(2,5)], by = c("areaCode"))
#first dose
region_one_dose <- aggregate(LTLA_Vaccinations$no_vaccs_one, by = list(regionName = LTLA_Vaccinations$regionName,
                                                                       Week = LTLA_Vaccinations$Week),
                                FUN = sum)
region_one_dose$x <- round(region_one_dose$x)
colnames(region_one_dose) <- c("areaName", "Week", "vacc_one_dose_population")
hospital_admissions <- merge(hospital_admissions, region_one_dose, by = c("areaName","Week"))
#second dose
region_two_dose <- aggregate(LTLA_Vaccinations$no_vaccs_two, by = list(regionName = LTLA_Vaccinations$regionName,
                                                                       Week = LTLA_Vaccinations$Week),
                             FUN = sum)
region_two_dose$x <- round(region_two_dose$x)
colnames(region_two_dose) <- c("areaName", "Week", "vacc_two_dose_population")
hospital_admissions <- merge(hospital_admissions, region_two_dose, by = c("areaName","Week"))
#third dose
region_three_dose <- aggregate(LTLA_Vaccinations$no_vaccs_three, by = list(regionName = LTLA_Vaccinations$regionName,
                                                                       Week = LTLA_Vaccinations$Week),
                             FUN = sum)
region_three_dose$x <- round(region_three_dose$x)
colnames(region_three_dose) <- c("areaName", "Week", "vacc_three_dose_population")
hospital_admissions <- merge(hospital_admissions, region_three_dose, by = c("areaName","Week"))
####
hospital_admissions$cumVaccPercentage_FirstDose <- (hospital_admissions$vacc_one_dose_population/hospital_admissions$Population)*100
hospital_admissions$cumVaccPercentage_SecondDose <- (hospital_admissions$vacc_two_dose_population/hospital_admissions$Population)*100
hospital_admissions$cumVaccPercentage_ThirdDose <- (hospital_admissions$vacc_three_dose_population/hospital_admissions$Population)*100

#remove all the chaff
hospital_admissions <- hospital_admissions[,-c(8,9,10)]
rm(region_one_dose, region_two_dose, region_three_dose, LTLA_Vaccinations)


#IMD average score
#Again, multiply by population, aggregate, then divide down
LTLA_IMD <- distinct(Case_Rates_Data[,c("areaCode","Population","IMD_Average_score")])
LTLA_IMD$total_IMD <- LTLA_IMD$Population*LTLA_IMD$IMD_Average_score

LTLA_IMD <- merge(LTLA_IMD, LTLA_to_region[,c(2,5)], by = c("areaCode"))
#first dose
Region_IMD <- aggregate(LTLA_IMD$total_IMD, by = list(regionName = LTLA_IMD$regionName),
                             FUN = sum)

colnames(Region_IMD) <- c("areaName","Total_IMD")
hospital_admissions <- merge(hospital_admissions, Region_IMD, by = c("areaName"))
hospital_admissions$IMD_Average_score <- hospital_admissions$Total_IMD/hospital_admissions$Population
hospital_admissions <- hospital_admissions[,-11]
rm(LTLA_IMD, Region_IMD)

#transit_stations_percent_change_from_baseline
#Again, multiply by population, aggregate, then divide down
LTLA_transit <- distinct(Case_Rates_Data[,c("areaCode","Week","Population","transit_stations_percent_change_from_baseline")])
LTLA_transit$total_transit <- LTLA_transit$Population*LTLA_transit$transit_stations_percent_change_from_baseline/100

LTLA_transit <- merge(LTLA_transit, LTLA_to_region[,c(2,5)], by = c("areaCode"))
Region_transit <- aggregate(LTLA_transit$total_transit, by = list(regionName = LTLA_transit$regionName,
                                                                  Week = LTLA_transit$Week),
                        FUN = sum, na.rm = TRUE)

colnames(Region_transit) <- c("areaName","Week", "Total_transit")
hospital_admissions <- merge(hospital_admissions, Region_transit, by = c("areaName", "Week"))
hospital_admissions$transit_stations_percent_change_from_baseline <- hospital_admissions$Total_transit/hospital_admissions$Population*100
hospital_admissions <- hospital_admissions[,-12]
rm(LTLA_transit, Region_transit)

#residential_percent_change_from_baseline
#Again, multiply by population, aggregate, then divide down
LTLA_resident <- distinct(Case_Rates_Data[,c("areaCode","Week","Population","residential_percent_change_from_baseline")])
LTLA_resident$total_resident <- LTLA_resident$Population*LTLA_resident$residential_percent_change_from_baseline/100

LTLA_resident <- merge(LTLA_resident, LTLA_to_region[,c(2,5)], by = c("areaCode"))
Region_resident <- aggregate(LTLA_resident$total_resident, by = list(regionName = LTLA_resident$regionName,
                                                                  Week = LTLA_resident$Week),
                            FUN = sum, na.rm = TRUE)

colnames(Region_resident) <- c("areaName","Week", "Total_resident")
hospital_admissions <- merge(hospital_admissions, Region_resident, by = c("areaName", "Week"))
hospital_admissions$residential_percent_change_from_baseline <- hospital_admissions$Total_resident/hospital_admissions$Population*100
hospital_admissions <- hospital_admissions[,-13]
rm(LTLA_resident, Region_resident)


#VARIANT PROPORTIONS
#ALPHA
LTLA_variant <- distinct(Case_Rates_Data[,c("areaCode","Week","Population","Alpha_proportion")])
LTLA_variant$total_variant <- LTLA_variant$Population*LTLA_variant$Alpha_proportion/100

LTLA_variant <- merge(LTLA_variant, LTLA_to_region[,c(2,5)], by = c("areaCode"))
Region_variant <- aggregate(LTLA_variant$total_variant, by = list(regionName = LTLA_variant$regionName,
                                                                     Week = LTLA_variant$Week),
                             FUN = sum, na.rm = TRUE)

colnames(Region_variant) <- c("areaName","Week", "Total_variant")
hospital_admissions <- merge(hospital_admissions, Region_variant, by = c("areaName", "Week"))
hospital_admissions$Alpha_proportion <- hospital_admissions$Total_variant/hospital_admissions$Population*100
hospital_admissions <- hospital_admissions[,-14]
rm(LTLA_variant, Region_variant)

#Delta_proportion
LTLA_variant <- distinct(Case_Rates_Data[,c("areaCode","Week","Population","Delta_proportion")])
LTLA_variant$total_variant <- LTLA_variant$Population*LTLA_variant$Delta_proportion/100

LTLA_variant <- merge(LTLA_variant, LTLA_to_region[,c(2,5)], by = c("areaCode"))
Region_variant <- aggregate(LTLA_variant$total_variant, by = list(regionName = LTLA_variant$regionName,
                                                                  Week = LTLA_variant$Week),
                            FUN = sum, na.rm = TRUE)

colnames(Region_variant) <- c("areaName","Week", "Total_variant")
hospital_admissions <- merge(hospital_admissions, Region_variant, by = c("areaName", "Week"))
hospital_admissions$Delta_proportion <- hospital_admissions$Total_variant/hospital_admissions$Population*100
hospital_admissions <- hospital_admissions[,-15]
rm(LTLA_variant, Region_variant)


#Delta_AY_4_2_proportion
LTLA_variant <- distinct(Case_Rates_Data[,c("areaCode","Week","Population","Delta_AY_4_2_proportion")])
LTLA_variant$total_variant <- LTLA_variant$Population*LTLA_variant$Delta_AY_4_2_proportion/100

LTLA_variant <- merge(LTLA_variant, LTLA_to_region[,c(2,5)], by = c("areaCode"))
Region_variant <- aggregate(LTLA_variant$total_variant, by = list(regionName = LTLA_variant$regionName,
                                                                  Week = LTLA_variant$Week),
                            FUN = sum, na.rm = TRUE)

colnames(Region_variant) <- c("areaName","Week", "Total_variant")
hospital_admissions <- merge(hospital_admissions, Region_variant, by = c("areaName", "Week"))
hospital_admissions$Delta_AY_4_2_proportion <- hospital_admissions$Total_variant/hospital_admissions$Population*100
hospital_admissions <- hospital_admissions[,-16]
rm(LTLA_variant, Region_variant)


#Omicron_BA_1_proportion
LTLA_variant <- distinct(Case_Rates_Data[,c("areaCode","Week","Population","Omicron_BA_1_proportion")])
LTLA_variant$total_variant <- LTLA_variant$Population*LTLA_variant$Omicron_BA_1_proportion/100

LTLA_variant <- merge(LTLA_variant, LTLA_to_region[,c(2,5)], by = c("areaCode"))
Region_variant <- aggregate(LTLA_variant$total_variant, by = list(regionName = LTLA_variant$regionName,
                                                                  Week = LTLA_variant$Week),
                            FUN = sum, na.rm = TRUE)

colnames(Region_variant) <- c("areaName","Week", "Total_variant")
hospital_admissions <- merge(hospital_admissions, Region_variant, by = c("areaName", "Week"))
hospital_admissions$Omicron_BA_1_proportion <- hospital_admissions$Total_variant/hospital_admissions$Population*100
hospital_admissions <- hospital_admissions[,-17]
rm(LTLA_variant, Region_variant)

#Omicron_BA_2_proportion
LTLA_variant <- distinct(Case_Rates_Data[,c("areaCode","Week","Population","Omicron_BA_2_proportion")])
LTLA_variant$total_variant <- LTLA_variant$Population*LTLA_variant$Omicron_BA_2_proportion/100

LTLA_variant <- merge(LTLA_variant, LTLA_to_region[,c(2,5)], by = c("areaCode"))
Region_variant <- aggregate(LTLA_variant$total_variant, by = list(regionName = LTLA_variant$regionName,
                                                                  Week = LTLA_variant$Week),
                            FUN = sum, na.rm = TRUE)

colnames(Region_variant) <- c("areaName","Week", "Total_variant")
hospital_admissions <- merge(hospital_admissions, Region_variant, by = c("areaName", "Week"))
hospital_admissions$Omicron_BA_2_proportion <- hospital_admissions$Total_variant/hospital_admissions$Population*100
hospital_admissions <- hospital_admissions[,-18]
rm(LTLA_variant, Region_variant)

#Other_proportion
LTLA_variant <- distinct(Case_Rates_Data[,c("areaCode","Week","Population","Other_proportion")])
LTLA_variant$total_variant <- LTLA_variant$Population*LTLA_variant$Other_proportion/100

LTLA_variant <- merge(LTLA_variant, LTLA_to_region[,c(2,5)], by = c("areaCode"))
Region_variant <- aggregate(LTLA_variant$total_variant, by = list(regionName = LTLA_variant$regionName,
                                                                  Week = LTLA_variant$Week),
                            FUN = sum, na.rm = TRUE)

colnames(Region_variant) <- c("areaName","Week", "Total_variant")
hospital_admissions <- merge(hospital_admissions, Region_variant, by = c("areaName", "Week"))
hospital_admissions$Other_proportion <- hospital_admissions$Total_variant/hospital_admissions$Population*100
hospital_admissions <- hospital_admissions[,-19]
rm(LTLA_variant, Region_variant)

#Omicron_BQ_1_proportion
LTLA_variant <- distinct(Case_Rates_Data[,c("areaCode","Week","Population","Omicron_BQ_1_proportion")])
LTLA_variant$total_variant <- LTLA_variant$Population*LTLA_variant$Omicron_BQ_1_proportion/100

LTLA_variant <- merge(LTLA_variant, LTLA_to_region[,c(2,5)], by = c("areaCode"))
Region_variant <- aggregate(LTLA_variant$total_variant, by = list(regionName = LTLA_variant$regionName,
                                                                  Week = LTLA_variant$Week),
                            FUN = sum, na.rm = TRUE)

colnames(Region_variant) <- c("areaName","Week", "Total_variant")
hospital_admissions <- merge(hospital_admissions, Region_variant, by = c("areaName", "Week"))
hospital_admissions$Omicron_BQ_1_proportion <- hospital_admissions$Total_variant/hospital_admissions$Population*100
hospital_admissions <- hospital_admissions[,-20]
rm(LTLA_variant, Region_variant)

#Omicron_BA_4_proportion
LTLA_variant <- distinct(Case_Rates_Data[,c("areaCode","Week","Population","Omicron_BA_4_proportion")])
LTLA_variant$total_variant <- LTLA_variant$Population*LTLA_variant$Omicron_BA_4_proportion/100

LTLA_variant <- merge(LTLA_variant, LTLA_to_region[,c(2,5)], by = c("areaCode"))
Region_variant <- aggregate(LTLA_variant$total_variant, by = list(regionName = LTLA_variant$regionName,
                                                                  Week = LTLA_variant$Week),
                            FUN = sum, na.rm = TRUE)

colnames(Region_variant) <- c("areaName","Week", "Total_variant")
hospital_admissions <- merge(hospital_admissions, Region_variant, by = c("areaName", "Week"))
hospital_admissions$Omicron_BA_4_proportion <- hospital_admissions$Total_variant/hospital_admissions$Population*100
hospital_admissions <- hospital_admissions[,-21]
rm(LTLA_variant, Region_variant)

#Omicron_BA_5_proportion
LTLA_variant <- distinct(Case_Rates_Data[,c("areaCode","Week","Population","Omicron_BA_5_proportion")])
LTLA_variant$total_variant <- LTLA_variant$Population*LTLA_variant$Omicron_BA_5_proportion/100

LTLA_variant <- merge(LTLA_variant, LTLA_to_region[,c(2,5)], by = c("areaCode"))
Region_variant <- aggregate(LTLA_variant$total_variant, by = list(regionName = LTLA_variant$regionName,
                                                                  Week = LTLA_variant$Week),
                            FUN = sum, na.rm = TRUE)

colnames(Region_variant) <- c("areaName","Week", "Total_variant")
hospital_admissions <- merge(hospital_admissions, Region_variant, by = c("areaName", "Week"))
hospital_admissions$Omicron_BA_5_proportion <- hospital_admissions$Total_variant/hospital_admissions$Population*100
hospital_admissions <- hospital_admissions[,-22]
rm(LTLA_variant, Region_variant)


#Weighted NHS funding
#"Core_services_funding_by_weighted"                 
LTLA_funding <- distinct(Case_Rates_Data[,c(1,2,4,which(colnames(Case_Rates_Data) == "Core_services_funding_by_weighted"))])
LTLA_funding$total_funding <- LTLA_funding$Population*LTLA_funding$Core_services_funding_by_weighted

LTLA_funding <- merge(LTLA_funding, LTLA_to_region[,c(2,5)], by = c("areaCode"))
Region_funding <- aggregate(LTLA_funding$total_funding, by = list(regionName = LTLA_funding$regionName,
                                                                  Week = LTLA_funding$Week),
                            FUN = sum, na.rm = TRUE)

colnames(Region_funding) <- c("areaName","Week", "Total_funding")
hospital_admissions <- merge(hospital_admissions, Region_funding, by = c("areaName", "Week"))
hospital_admissions$Core_services_funding_by_weighted <- hospital_admissions$Total_funding/hospital_admissions$Population
hospital_admissions <- hospital_admissions[,-23]
rm(LTLA_funding, Region_funding)

#"Primary_care_funding_by_weighted"
LTLA_funding <- distinct(Case_Rates_Data[,c(1,2,4,which(colnames(Case_Rates_Data) == "Primary_care_funding_by_weighted"))])
LTLA_funding$total_funding <- LTLA_funding$Population*LTLA_funding$Primary_care_funding_by_weighted

LTLA_funding <- merge(LTLA_funding, LTLA_to_region[,c(2,5)], by = c("areaCode"))
Region_funding <- aggregate(LTLA_funding$total_funding, by = list(regionName = LTLA_funding$regionName,
                                                                  Week = LTLA_funding$Week),
                            FUN = sum, na.rm = TRUE)

colnames(Region_funding) <- c("areaName","Week", "Total_funding")
hospital_admissions <- merge(hospital_admissions, Region_funding, by = c("areaName", "Week"))
hospital_admissions$Primary_care_funding_by_weighted <- hospital_admissions$Total_funding/hospital_admissions$Population
hospital_admissions <- hospital_admissions[,-24]
rm(LTLA_funding, Region_funding)

#"Specialised_services_by_weighted"  
LTLA_funding <- distinct(Case_Rates_Data[,c(1,2,4,which(colnames(Case_Rates_Data) == "Specialised_services_by_weighted"))])
LTLA_funding$total_funding <- LTLA_funding$Population*LTLA_funding$Specialised_services_by_weighted

LTLA_funding <- merge(LTLA_funding, LTLA_to_region[,c(2,5)], by = c("areaCode"))
Region_funding <- aggregate(LTLA_funding$total_funding, by = list(regionName = LTLA_funding$regionName,
                                                                  Week = LTLA_funding$Week),
                            FUN = sum, na.rm = TRUE)

colnames(Region_funding) <- c("areaName","Week", "Total_funding")
hospital_admissions <- merge(hospital_admissions, Region_funding, by = c("areaName", "Week"))
hospital_admissions$Specialised_services_by_weighted <- hospital_admissions$Total_funding/hospital_admissions$Population
hospital_admissions <- hospital_admissions[,-25]
rm(LTLA_funding, Region_funding)


save(hospital_admissions, file = 'Outputs/Hospitalisations_Data.RData')
save(LTLA_to_region_matrix, file = 'Outputs/LTLA_to_region_matrix.RData')
write.csv(hospital_admissions, file = 'Outputs/Hospitalisations_Data.csv')
save(Boundaries, file = 'Outputs/Boundaries_Data.RData')

