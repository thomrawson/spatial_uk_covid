#Data to give Raf

Case_Rates_Data <- Case_Rates_Data[,c(2,4,6,9,10,11,12,13,14,15,16,18,19,20,
                                       21,22,23,24,25,26,27,28,29,30,33,34,35,36,37,38)]


Case_Rates_Data2 <- Case_Rates_Data[,c(4,1,2,3,5:30)]
Case_Rates_Data2$cumVaccPercentage_FirstDose <- Case_Rates_Data2$cumVaccPercentage_FirstDose/100
Case_Rates_Data2$cumVaccPercentage_SecondDose <- Case_Rates_Data2$cumVaccPercentage_SecondDose/100
Case_Rates_Data2$cumVaccPercentage_ThirdDose <- Case_Rates_Data2$cumVaccPercentage_ThirdDose/100
Case_Rates_Data2$unringfenced <- Case_Rates_Data2$unringfenced / Case_Rates_Data2$Population
Case_Rates_Data2$contain_outbreak_management <- Case_Rates_Data2$contain_outbreak_management / Case_Rates_Data2$Population
Case_Rates_Data2$ASC_infection_control_fund <- Case_Rates_Data2$ASC_infection_control_fund / Case_Rates_Data2$Population

Case_Rates_Data_new <- Case_Rates_Data2[,-3]

write.csv(Case_Rates_Data_new,"simple_data_for_raf.csv", row.names = FALSE)
