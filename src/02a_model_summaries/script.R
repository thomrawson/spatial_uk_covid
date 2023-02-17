#This script loads in the stanfit object and outputs some initial summary info
load("stanfit.RData")
dir.create("Case_Outputs")
dir.create("Hospital_Outputs")

#Our parameters and priors:
#beta0 ~ normal(0.0, 1.0);
#betas ~ normal(0.0, 1.0); #covariate coefficients
#zetas ~ gamma(0.05, 1.0); #spatial kernel  1-306
#theta[,i] ~ normal(0.0, 1.0); #noise

#Output summaries of the mixed chains for all parameters except theta
if(spatial_kernel == "neighbours"){
main_summaries <- summary(stanfit, pars = c('beta0', 'betas', 'zetas', 'lp__'))
write.csv(main_summaries$summary,"Case_Outputs/main_summaries.csv", row.names = TRUE)
#Theta summaries are big, but we output anyway
theta_summaries <- summary(stanfit, pars = c('theta_mu', 'theta_sd','theta'))
write.csv(theta_summaries$summary,"Case_Outputs/theta_summaries.csv", row.names = TRUE)
#Main hospitalisation summaries
main_summaries_hosp <- summary(stanfit, pars = c('beta0_hosp', 'betas_hosp', 'lp__'))
write.csv(main_summaries_hosp$summary,"Hospital_Outputs/main_summaries_hosp.csv", row.names = TRUE)
theta_summaries_hosp <- summary(stanfit, pars = c('theta_hosp_mu', 'theta_hosp_sd','theta_hosp'))
write.csv(theta_summaries_hosp$summary,"Hospital_Outputs/theta_summaries_hosp.csv", row.names = TRUE)
} else if(spatial_kernel == "gravity"){
  main_summaries <- summary(stanfit, pars = c('beta0', 'betas', 'distance_alpha', 'distance_gamma', 'lp__'))
  write.csv(main_summaries$summary,"Case_Outputs/main_summaries.csv", row.names = TRUE)
  #Theta summaries are big, but we output anyway
  theta_summaries <- summary(stanfit, pars = c('theta_mu', 'theta_sd','theta'))
  write.csv(theta_summaries$summary,"Case_Outputs/theta_summaries.csv", row.names = TRUE)
  #Main hospitalisation summaries
  main_summaries_hosp <- summary(stanfit, pars = c('beta0_hosp', 'betas_hosp', 'lp__'))
  write.csv(main_summaries_hosp$summary,"Hospital_Outputs/main_summaries_hosp.csv", row.names = TRUE)
  theta_summaries_hosp <- summary(stanfit, pars = c('theta_hosp_mu', 'theta_hosp_sd','theta_hosp'))
  write.csv(theta_summaries_hosp$summary,"Hospital_Outputs/theta_summaries_hosp.csv", row.names = TRUE)
}else{
  stop("Unrecognised spatial_kernel parameter.")
}



#Save a plot of the beta trajectories
beta_trajectories1 <- traceplot(stanfit, pars=c('beta0', sprintf('betas[%s]',1:14)), nrow = 3)
png(file="Case_Outputs\\beta_trajectories1.png",
    width=1440, height=1080, res = 150)
plot(beta_trajectories1)
dev.off()

beta_trajectories2 <- traceplot(stanfit, pars= sprintf('betas[%s]',15:26))
png(file="Case_Outputs\\beta_trajectories2.png",
    width=1440, height=1080, res = 150)
plot(beta_trajectories2)
dev.off()

#We'll save the zeta plots, though unlikely to be that interested...
if(spatial_kernel == "neighbours"){
dir.create("Case_Outputs\\zeta_plots")
for(i in 1:19){
  zeta_trajectories <- traceplot(stanfit, pars =sprintf('zetas[%s]',((i-1)*16 + 1):(16*i)))
  png(file=sprintf("Case_Outputs\\zeta_plots\\zeta_trajectories_%s.png", i),
      width=1440, height=1080, res = 150)
  plot(zeta_trajectories)
  dev.off()
}

zeta_trajectories <- traceplot(stanfit, pars =sprintf('zetas[%s]',304:306))
png(file=sprintf("Case_Outputs\\zeta_plots\\zeta_trajectories_%s.png", 20),
    width=1440, height=1080, res = 150)
plot(zeta_trajectories)
dev.off()
} else if(spatial_kernel == "gravity"){
  distance_trajectories <- traceplot(stanfit, pars =c("distance_alpha", "distance_gamma"))
  png(file="Case_Outputs\\distance_trajectories.png",
      width=1440, height=1080, res = 150)
  plot(distance_trajectories)
  dev.off()

}else{
  
}

##We export a map of the UK LTLAs and shaded with the value of their zetas
if(spatial_kernel == "neighbours"){
load("Boundaries_Data.RData")
load("model_data.RData")
dir.create("Case_Outputs\\Zeta_maps")

#Remove to just the indices I've modelled:
areaCodes_used <- unique(Case_Rates_Data$areaCode)
Boundaries_reduced <- filter(Boundaries, CODE %in% areaCodes_used)

#CAN DO THIS BETTER:
zetas_mean <- get_posterior_mean(stanfit, pars = 'zetas')
zetas_mean <- as.data.frame(zetas_mean[,5])

Boundaries_reduced$zetas <- as.numeric(zetas_mean[,1]) 

gb_cities <- read.csv("gb_cities.csv")
#Manually trim off the ones we don't want.
gb_cities <- gb_cities[1:30,]
gb_cities <- gb_cities[-c(6,7,9,16,17,18,19,21,22,24,25,27,29,30),]

gb_cities %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) -> gb_cities

#PLOT standard
zetas_map <- ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = zetas)) +
  scale_fill_gradient2() +
  ggtitle("LTLAs, nearest neighbor model")

London_zetas <- ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = zetas)) +
  scale_fill_gradient2()

combined_plot <- grid.arrange(zetas_map, London_zetas, nrow = 1, widths = c(2,1))

png(file="Case_Outputs\\Zeta_maps\\zetas.png",
    width=1440, height=1080, res = 150)
plot(combined_plot)
dev.off()

zetas_map <- zetas_map + geom_sf_label(data = gb_cities[1:30,], aes( label = city), alpha = 0.25, size = 3) +
  xlab("") + ylab("")

combined_plot <- grid.arrange(zetas_map, London_zetas, nrow = 1, widths = c(2,1))

png(file="Case_Outputs\\Zeta_maps\\zetas_w_cities.png",
    width=1440, height=1080, res = 150)
plot(combined_plot)
dev.off()

#PLOT standard capped
zetas_map <- ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = zetas)) +
  scale_fill_gradient2(limits = c(0,1), midpoint = 0.05) +
  ggtitle("LTLAs, nearest neighbor model") 

London_zetas <- ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = zetas)) +
  scale_fill_gradient2(limits = c(0,1), midpoint = 0.05)

combined_plot <- grid.arrange(zetas_map, London_zetas, nrow = 1, widths = c(2,1))

png(file="Case_Outputs\\Zeta_maps\\zetas_capped.png",
    width=1440, height=1080, res = 150)
plot(combined_plot)
dev.off()

zetas_map <- zetas_map + geom_sf_label(data = gb_cities[1:30,], aes( label = city), alpha = 0.25, size = 3) +
  xlab("") + ylab("")

combined_plot <- grid.arrange(zetas_map, London_zetas, nrow = 1, widths = c(2,1))

png(file="Case_Outputs\\Zeta_maps\\zetas_capped_w_cities.png",
    width=1440, height=1080, res = 150)
plot(combined_plot)
dev.off()

#PLOT log
zetas_map <- ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = log(zetas))) +
  scale_fill_gradient2() +
  ggtitle("LTLAs, nearest neighbor model")

London_zetas <- ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = log(zetas))) +
  scale_fill_gradient2()

combined_plot <- grid.arrange(zetas_map, London_zetas, nrow = 1, widths = c(2,1))

png(file="Case_Outputs\\Zeta_maps\\zetas_log.png",
    width=1440, height=1080, res = 150)
plot(combined_plot)
dev.off()

zetas_map <- zetas_map + geom_sf_label(data = gb_cities[1:30,], aes( label = city), alpha = 0.25, size = 3) +
  xlab("") + ylab("")

combined_plot <- grid.arrange(zetas_map, London_zetas, nrow = 1, widths = c(2,1))

png(file="Case_Outputs\\Zeta_maps\\zetas_log_w_cities.png",
    width=1440, height=1080, res = 150)
plot(combined_plot)
dev.off()


}

#Save a plot of the hospital beta trajectories
beta_trajectories1 <- traceplot(stanfit, pars=c('beta0_hosp', sprintf('betas_hosp[%s]',1:8)), nrow = 3)
png(file="Hospital_Outputs\\beta_trajectories1.png",
    width=1440, height=1080, res = 150)
plot(beta_trajectories1)
dev.off()

beta_trajectories2 <- traceplot(stanfit, pars= sprintf('betas_hosp[%s]',9:17), nrow = 3)
png(file="Hospital_Outputs\\beta_trajectories2.png",
    width=1440, height=1080, res = 150)
plot(beta_trajectories2)
dev.off()


#Save a plot of the theta hyperparameters
theta_trajectories <- traceplot(stanfit, pars=c("theta_mu", "theta_sd", "theta_hosp_mu", "theta_hosp_sd"), nrow = 2)
png(file="Hospital_Outputs\\theta_trajectories.png",
    width=1440, height=1080, res = 150)
plot(theta_trajectories)
dev.off()
png(file="Case_Outputs\\theta_trajectories.png",
    width=1440, height=1080, res = 150)
plot(theta_trajectories)
dev.off()


#Next we give some sense of how our thetas are.
#Theta_means <- get_posterior_mean(stanfit, pars = 'theta')

#Reminder of what all the betas correspond to:

#1 - cumVaccPercentage_FirstDose
#2 - cumVaccPercentage_SecondDose
#3 - cumVaccPercentage_ThirdDose
#4 - prop_white_british
#5 - prop_asian
#6 - prop_black_afr_car
#7 - IMD_Average_score
#8 - prop_o65
#9 - Median_annual_income
#10 - workplaces_percent_change_from_baseline
#11 - residential_percent_change_from_baseline
#12 - transit_stations_percent_change_from_baseline
#13 - Alpha_proportion
#14 - Delta_proportion
#15 - Delta_AY_4_2_proportion
#16 - Omicron_BA_1_proportion
#17 - Omicron_BA_2_proportion
#18 - Omicron_BA_4_proportion
#19 - Omicron_BA_5_proportion
#20 - Other_proportion
#21 - Core_services_funding_by_weighted
#22 - Primary_care_funding_by_weighted
#23 - Specialised_services_by_weighted
#24 - unringfenced
#25 - contain_outbreak_management
#26 - ASC_infection_control_fund

beta_summaries <- main_summaries$summary
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

png(file="Case_Outputs\\betas_1_12.png",
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

png(file="Case_Outputs\\betas_13_20.png",
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

png(file="Case_Outputs\\betas_21_26.png",
    width=1440, height=1080, res = 150)
plot(betas_21_26)
dev.off()

#Hospital betas
#1 - cumVaccPercentage_FirstDose
#2 - cumVaccPercentage_SecondDose
#3 - cumVaccPercentage_ThirdDose
#4 - IMD_Average_score
#5 - residential_percent_change_from_baseline
#6 - transit_stations_percent_change_from_baseline
#7 - Alpha_proportion
#8 - Delta_proportion
#9 - Delta_AY_4_2_proportion
#10 - Omicron_BA_1_proportion
#11 - Omicron_BA_2_proportion
#12 - Omicron_BA_4_proportion
#13 - Omicron_BA_5_proportion
#14 - Other_proportion
#15 - Core_services_funding_by_weighted
#16 - Primary_care_funding_by_weighted
#17 - Specialised_services_by_weighted

betas_hosp_covar <- plot(stanfit, pars = sprintf('betas_hosp[%s]',c(1,2,3,4,5,6,15,16,17)))
#ci_level: 0.8 (80% intervals)
#outer_level: 0.95 (95% intervals)
betas_hosp_covar <- betas_hosp_covar + scale_y_continuous(breaks = c(9:1),
                                              labels = c("1) CumVacc_1dose", 
                                                         "2) CumVacc_2dose", "3) CumVacc_3dose",
                                                         "4) IMD_Average_score",
                                                         "5) residential_movement", "6) transit_movement",
                                                         "15) Core Services", "16) Primary Care", 
                                                         "17) Specialised Services")) +
  geom_vline(xintercept = 0, color = "skyblue", lty = 5, size = 1) +ggtitle("Hospital Covariate coefficients")

png(file="Hospital_Outputs\\betas_other.png",
    width=1440, height=1080, res = 150)
plot(betas_hosp_covar)
dev.off()

betas_hosp_variant <- plot(stanfit, pars = sprintf('betas_hosp[%s]',7:14))
#ci_level: 0.8 (80% intervals)
#outer_level: 0.95 (95% intervals)
betas_hosp_variant <- betas_hosp_variant + scale_y_continuous(breaks = c(8:1),
                                                          labels = c("7) Alpha", "8) Delta", 
                                                                     "9) Delta_AY_4_2", "10) Omicron_BA_1",
                                                                     "11) Omicron_BA_2", "12) Omicron_BA_4",
                                                                     "13) Omicron_BA_5", "14) Other variants")) +
  geom_vline(xintercept = 0, color = "skyblue", lty = 5, size = 1) +ggtitle("Hospital Covariate coefficients")

png(file="Hospital_Outputs\\betas_variants.png",
    width=1440, height=1080, res = 150)
plot(betas_hosp_variant)
dev.off()

# MCMCplot(stanfit, 
#          params = 'betas[1]',
#          rank = TRUE,
#          xlab = 'ESTIMATE')

# log_pp <- stan_diag(stanfit)
# png(file="Outputs\\lpp_acceptance.png",
#     width=1440, height=1080, res = 150)
# plot(log_pp)
# dev.off()

#Other stuff
sampler_params <- get_sampler_params(stanfit, inc_warmup = FALSE)
#To do things like calculate the average value of accept_stat__ for each chain 
#(or the maximum value of treedepth__ for each chain if using the NUTS algorithm, etc.) 
#the sapply function is useful as it will apply the same function to each component 
#of sampler_params:
mean_accept_stat_by_chain <- sapply(sampler_params, function(x) mean(x[, "accept_stat__"]))
write.table(as.character(mean_accept_stat_by_chain), file = "mean_accept_stat_by_chain.txt", sep = "\t",
            row.names = FALSE)

#Investigate how good the model fit is against real data
load("model_data.RData")
#y is the REAL "next week cases". 103 (time) x 306 (LTLAs)
#E is the "current" weeks cases.  i.e. E[2] = y[1]


if(spatial_kernel == "neighbours"){
 
  dir.create("Case_Outputs//Goodness_of_fit")
  dir.create("Case_Outputs//Goodness_of_fit//trajectories")
  
  #Script model definition:
  #y[,i] ~ poisson_log(log(susceptible_proxy[,i].*(E[,i] + (zetas .*E_neighbours[,i]))) + beta0 + x[i] * betas + theta);  // extra noise removed removed: + theta[,i]
  #y_hosp[,i+average_hosp_lag] ~ poisson_log(log(LTLA_to_region*y_as_matrix[,i]) + beta0_hosp + x_hosp[i]*betas_hosp + theta_hosp);
  
  model_betas <- as.numeric(get_posterior_mean(stanfit, pars = 'betas')[,5])
  model_beta0 <- as.numeric(get_posterior_mean(stanfit, pars = 'beta0')[,5])
  model_zetas <- as.numeric(get_posterior_mean(stanfit, pars = 'zetas')[,5])
  model_theta <- as.numeric(get_posterior_mean(stanfit, pars = 'theta')[,5])
  model_theta_mu <- as.numeric(get_posterior_mean(stanfit, pars = 'theta_mu')[,5])
  model_theta_sd <- as.numeric(get_posterior_mean(stanfit, pars = 'theta_sd')[,5])
  
  model_approx_y <- array(0, dim = c(103,306)) #
  for(i in 1:103){
  model_approx_y[i,] <- as.numeric(log(susceptible_proxy[,i]*(E[i,] + (model_zetas *E_neighbours_scaled[,i])))) + model_beta0 + x[i,,]%*%model_betas + model_theta
  }
  model_approx_y <- exp(model_approx_y)
  
  REAL_week_difference <- abs(y - E)
  MODEL_week_difference <- abs(model_approx_y - E)
  
  #Is the model better than just assuming the same rate of increase from the week previous?
  basic_approx_y <- array(0, dim = c(103,306))
  for(i in 3:103){
    for(j in 1:306){
      basic_approx_y[i,j] <- y[i-1,j]*(y[i-1,j]/y[i-2,j])
    }
  }
  
  #First thing we'll make, is a plot for each of the 306 LTLAs showing the real data against the model approx data.
  #In 34 3x3 plots
  LTLAs_by_Index <- unique(Case_Rates_Data[,c(1,3,5)])
  LTLAs_by_Index <- LTLAs_by_Index[order(LTLAs_by_Index$INDEX),]
  AllDates <- unique(Case_Rates_Data$date_begin)
  for(i in 1:(306/9)){
    Indices_to_plot <- seq(((9*i)-8),(9*i)) 
    myplots <- list()
    for(j in Indices_to_plot){
      k <- j - ((i-1)*9)
      areaCode_plot <- LTLAs_by_Index$areaCode[j]
      areaName_plot <- LTLAs_by_Index$areaName[j]
      
      dataHold <- data.frame(Date = rep(AllDates[2:103],3),
                             areaCode = rep(areaCode_plot, 306),
                             areaName = rep(areaName_plot, 306),
                             Source = c(rep("Real Data", 102), rep("Model Approx.",102), rep("Same R assumed",102)),
                             Cases = c(y[1:102,j],model_approx_y[1:102,j],basic_approx_y[1:102,j]),
                             LineType = c(rep("1",204), rep("2",102))
                             )
      dataHold$date <- as.Date(dataHold$Date)
    
      ggplot(dataHold) +
        geom_line(aes(x = Date, y = log(Cases), color = Source, linetype = LineType), size = 1, alpha = 0.6) +
        theme_classic() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        scale_x_date(date_breaks = "2 month", date_labels = "%b %y") + 
        ggtitle(paste(areaName_plot, " ", areaCode_plot)) +
        scale_linetype(guide = "none") -> plotHold
    
      if(k %in% c(1,4)){
        myplots[[k]] <- plotHold + xlab(NULL)
      } else if(k ==7){
        myplots[[k]] <- plotHold
      } else if(k %in% c(8,9)){
        myplots[[k]] <- plotHold + ylab(NULL)
      } else{
        myplots[[k]] <- plotHold + ylab(NULL) + xlab(NULL)
      }
      
      
    }
    
    plot_combined <- plot_grid(myplots[[1]] + theme(legend.position="none"),
                               myplots[[2]] + theme(legend.position="none"),
                               myplots[[3]] + theme(legend.position="none"),
                               myplots[[4]] + theme(legend.position="none"),
                               myplots[[5]] + theme(legend.position="none"),
                               myplots[[6]] + theme(legend.position="none"),
                               myplots[[7]] + theme(legend.position="none"),
                               myplots[[8]] + theme(legend.position="none"),
                               myplots[[9]] + theme(legend.position="none"),
                               align = "vh",
                               nrow = 3)
    
    legend <- get_legend(
      # create some space to the left of the legend
      myplots[[1]] + theme(legend.box.margin = margin(0, 0, 0, 12))
    )
  
    plot_combined <- plot_grid(plot_combined, legend, rel_widths = c(3, .4))
      
    
    png(file=sprintf("Case_Outputs//Goodness_of_fit//trajectories//trajectories_%s.png", i),
        width=1440, height=1080, res = 150)
    plot(plot_combined)
    dev.off()
    
  }
  
  
  plot(REAL_week_difference, MODEL_week_difference)
  #What if we just do y ~ poisson
  plot(y[,2], model_approx_y[,2])
  
  

    
  our_model_error <- abs(y - model_approx_y)
  basic_error <- abs(y - basic_approx_y)
  
  plot(log(our_model_error[,20]), log(basic_error[,20]))
  abline(a=0, b=1, col = "red")      
}


#test <- get_posterior_mean(stanfit, pars = 'betas[2]')
#Compare means to the data:
#Mus <- rstan::extract(stanfit, pars = 'mu')
#Mus <- get_posterior_mean(stanfit, pars = 'mu')
#plot(log(y), log(Mus[,4]))

#Phis <- get_posterior_mean(stanfit, pars = 'phi')
#traceplot(stanfit, pars=c('theta[1,2]'))



#stan_diag(stanfit)
#stan_rhat(stanfit)
#stan_mcse(stanfit)