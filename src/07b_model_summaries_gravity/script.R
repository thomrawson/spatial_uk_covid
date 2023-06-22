#This script loads in the stanfit object and outputs some initial summary info
load("stanfit.RData")
dir.create("Case_Outputs")

################################################################################
#When running this code originally with just the cumulative vaccination numbers,
#there was an odd trend where it would punish the uptick of second jabs. I think
#this is because it allows for an additive impact of respective jabs which doesn't quite make sense

#Instead we change our cumulative vaccination, to PROPORTION vaccination,
#i.e. if you sum prop_no_dose, prop_1_dose, prop_2_dose, prop_3_dose for every i,j, it'll equal 1.
#Quickly added in this switch to deal with that:
PROP_vacc <- TRUE
################################################################################

#Our parameters and priors:
#beta0 ~ normal(0.0, 1.0);
#betas ~ normal(0.0, 1.0); #covariate coefficients
#zetas ~ gamma(0.05, 1.0); #spatial kernel  1-306
#theta[,i] ~ normal(0.0, 1.0); #noise

#Output summaries of the mixed chains for all parameters except theta
main_summaries <- summary(stanfit, pars = c('beta0', 'betas', 'beta_random_walk', 'distance_alpha', 'distance_gamma', 'lp__'))
write.csv(main_summaries$summary,"Case_Outputs/main_summaries.csv", row.names = TRUE)
#Theta summaries are big, but we output anyway
theta_summaries <- summary(stanfit, pars = c('theta_mu', 'theta_sd','theta'))
write.csv(theta_summaries$summary,"Case_Outputs/theta_summaries.csv", row.names = TRUE)



#Save a plot of the beta trajectories
beta_trajectories1 <- rstan::traceplot(stanfit, pars=c('beta0', sprintf('betas[%s]',1:11)), nrow = 3)
png(file="Case_Outputs\\beta_trajectories1.png",
    width=1440, height=1080, res = 150)
plot(beta_trajectories1)
dev.off()

beta_trajectories2 <- rstan::traceplot(stanfit, pars= sprintf('betas[%s]',12:21))
png(file="Case_Outputs\\beta_trajectories2.png",
    width=1440, height=1080, res = 150)
plot(beta_trajectories2)
dev.off()

#Let's also save the random walk plots
rw_trajectories1 <- rstan::traceplot(stanfit, pars=c(sprintf('beta_random_walk[%s]',1:25)), nrow = 5)
png(file="Case_Outputs\\rw_trajectories1.png",
    width=1440, height=1080, res = 150)
plot(rw_trajectories1)
dev.off()

rw_trajectories2 <- rstan::traceplot(stanfit, pars=c(sprintf('beta_random_walk[%s]',26:50)), nrow = 5)
png(file="Case_Outputs\\rw_trajectories2.png",
    width=1440, height=1080, res = 150)
plot(rw_trajectories2)
dev.off()

rw_trajectories3 <- rstan::traceplot(stanfit, pars=c(sprintf('beta_random_walk[%s]',51:75)), nrow = 5)
png(file="Case_Outputs\\rw_trajectories3.png",
    width=1440, height=1080, res = 150)
plot(rw_trajectories3)
dev.off()

rw_trajectories4 <- rstan::traceplot(stanfit, pars=c(sprintf('beta_random_walk[%s]',76:103)), nrow = 5)
png(file="Case_Outputs\\rw_trajectories4.png",
    width=1440, height=1080, res = 150)
plot(rw_trajectories4)
dev.off()

#We'll save the zeta plots, though unlikely to be that interested...

distance_trajectories <- rstan::traceplot(stanfit, pars = c('distance_gamma', 'distance_alpha'))
png(file="Case_Outputs\\distance_trajectories.png",
    width=1440, height=1080, res = 150)
plot(distance_trajectories)
dev.off()


##We export a map of the UK LTLAs and shaded with the value of their zetas
load("Boundaries_Data.RData")
load("model_data.RData")

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
#15 - Omicron_proportion

#16 - Core_services_funding_by_weighted
#17 - Primary_care_funding_by_weighted
#18 - Specialised_services_by_weighted
#19 - unringfenced
#20 - contain_outbreak_management
#21 - ASC_infection_control_fund

#######################################
if(PROP_vacc){
  labels_hold <- c("1) Prop_1dose", 
                   "2) Prop_2dose", "3) Prop_3dose",
                   "4) prop_white_british", "5) prop_asian",
                   "6) prop_black_afr", "7) IMD_Average_score",
                   "8) prop_o65", "9) Median_annual_income",
                   "10) workplaces_movement",
                   "11) residential_movement", "12) transit_movement")
}else{
  labels_hold <- c("1) CumVacc_1dose", 
    "2) CumVacc_2dose", "3) CumVacc_3dose",
    "4) prop_white_british", "5) prop_asian",
    "6) prop_black_afr", "7) IMD_Average_score",
    "8) prop_o65", "9) Median_annual_income",
    "10) workplaces_movement",
    "11) residential_movement", "12) transit_movement")
}

beta_summaries <- main_summaries$summary
#Next we have a look at how good our fit is
betas_1_12 <- plot(stanfit, pars = sprintf('betas[%s]',1:12))
#ci_level: 0.8 (80% intervals)
#outer_level: 0.95 (95% intervals)
betas_1_12 <- betas_1_12 + scale_y_continuous(breaks = c(12:1),
                                labels = labels_hold) +
  geom_vline(xintercept = 0, color = "skyblue", lty = 5, size = 1) +ggtitle("Covariate coefficients")

png(file="Case_Outputs\\betas_1_12.png",
    width=1440, height=1080, res = 150)
plot(betas_1_12)
dev.off()


betas_13_15 <- plot(stanfit, pars = sprintf('betas[%s]',13:15))
#ci_level: 0.8 (80% intervals)
#outer_level: 0.95 (95% intervals)
betas_13_15 <- betas_13_15 + scale_y_continuous(breaks = c(3:1),
                                              labels = c("13) Alpha", "14) Delta", 
                                                         "15) Omicron")) +
  geom_vline(xintercept = 0, color = "skyblue", lty = 5, size = 1) +ggtitle("Variant proportion coefficients")

png(file="Case_Outputs\\betas_13_15.png",
    width=1440, height=1080, res = 150)
plot(betas_13_15)
dev.off()

betas_16_21 <- plot(stanfit, pars = sprintf('betas[%s]',16:21))
#ci_level: 0.8 (80% intervals)
#outer_level: 0.95 (95% intervals)
betas_16_21 <- betas_16_21 + scale_y_continuous(breaks = c(6:1),
                                                labels = c("16) Core Services", "17) Primary Care", 
                                                           "18) Specialised Services", "19) Unringfenced",
                                                           "20) Outbreak Management", "21) ASC infection control")) +
  geom_vline(xintercept = 0, color = "skyblue", lty = 5, size = 1) +ggtitle("Funding coefficients")

png(file="Case_Outputs\\betas_16_21.png",
    width=1440, height=1080, res = 150)
plot(betas_16_21)
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

rhat_plot <- stan_rhat(stanfit)+ ggtitle(sprintf("%s parameters - %s have Rhat over 1.1", nrow(main_summaries$summary), sum(main_summaries$summary[, "Rhat"] > 1.1)))
ggsave(rhat_plot,
       filename = "stan_rhat.png") 
rm(rhat_plot)

#sum(main_summaries$summary[, "Rhat"] > 1.1)

#Investigate how good the model fit is against real data
load("model_data.RData")
#y is the REAL "next week cases". 103 (time) x 306 (LTLAs)
#E is the "current" weeks cases.  i.e. E[2] = y[1]

 
  dir.create("Case_Outputs//Goodness_of_fit")
  dir.create("Case_Outputs//Goodness_of_fit//basic_comparison_trajectories")
  dir.create("Case_Outputs//Goodness_of_fit//log_basic_comparison_trajectories")
  dir.create("Case_Outputs//Goodness_of_fit//basic_comparison_trajectories_grid")
  dir.create("Case_Outputs//Goodness_of_fit//week_to_week_trajectories_grid")
  dir.create("Case_Outputs//Goodness_of_fit//week_to_week_trajectories")
  dir.create("Case_Outputs//Goodness_of_fit//week_to_week_basic_trajectories")
  dir.create("Case_Outputs//Goodness_of_fit//week_to_week_basic_trajectories_grid")
  dir.create("Case_Outputs//Goodness_of_fit//log_week_to_week_trajectories")
  dir.create("Case_Outputs//Goodness_of_fit//log_week_to_week_basic_trajectories")
  dir.create("Case_Outputs//Goodness_of_fit//log_week_to_week_basic_trajectories_grid")
  dir.create("Case_Outputs//Goodness_of_fit//trajectories")
  dir.create("Case_Outputs//Goodness_of_fit//log_trajectories")
  dir.create("Case_Outputs//Goodness_of_fit//errors")
  
  #Script model definition:
  #y[,i] ~ poisson_log(log(susceptible_proxy[,i].*(E[,i] + (zetas .*E_neighbours[,i]))) + beta0 + x[i] * betas + theta);  // extra noise removed removed: + theta[,i]
  #y_hosp[,i+average_hosp_lag] ~ poisson_log(log(LTLA_to_region*y_as_matrix[,i]) + beta0_hosp + x_hosp[i]*betas_hosp + theta_hosp);
  
  model_betas <- as.numeric(get_posterior_mean(stanfit, pars = 'betas')[,5])
  model_beta0 <- as.numeric(get_posterior_mean(stanfit, pars = 'beta0')[,5])
  model_beta_random_walk <- as.numeric(get_posterior_mean(stanfit, pars = 'beta_random_walk')[,5])
  model_distance_gamma <- as.numeric(get_posterior_mean(stanfit, pars = 'distance_gamma')[,5])
  model_distance_alpha <- as.numeric(get_posterior_mean(stanfit, pars = 'distance_alpha')[,5])
  model_theta <- as.numeric(get_posterior_mean(stanfit, pars = 'theta')[,5])
  model_theta_mu <- as.numeric(get_posterior_mean(stanfit, pars = 'theta_mu')[,5])
  model_theta_sd <- as.numeric(get_posterior_mean(stanfit, pars = 'theta_sd')[,5])
  
  ###########
  #I can remove this once I've re-run the task
  Populations <- unique(Case_Rates_Data[,c(1,3,4,5)])
  Populations <- Populations[order(Populations$INDEX),]
  Populations$Population <- Populations$Population/sum(Populations$Population)
  Populations <- Populations$Population
  ###########
  
  #smoothed_distance_matrix[i,j] = (Populations[i]*Populations[j])/((1 + (Distance_matrix[i,j]/distance_alpha))^distance_gamma);
  smoothed_distance_matrix <- matrix(0, nrow = N, ncol = N)
  scaled_distance_matrix <- matrix(0, nrow = N, ncol = N)
  for(i in 1:N){
    for(j in 1:N){
      smoothed_distance_matrix[i,j] = (Populations[i]*Populations[j])/((1 + (Distance_matrix[i,j]/model_distance_alpha))^model_distance_gamma);
    }
  }
  
  #Then scale it
  #scaled_distance_matrix[i,] = smoothed_distance_matrix[i,]/sum(smoothed_distance_matrix[i,]);
  for(i in 1:N){
    scaled_distance_matrix[i,] = smoothed_distance_matrix[i,]/sum(smoothed_distance_matrix[i,])
  }
  #Interesting, Birmingham has the highest 1-1 travel mapping, and Rutland the lowest
  
  #Let's actually plot that
  Names_Index <- unique(Case_Rates_Data[,c(3,5)])
  Names_Index <- Names_Index[order(Names_Index$INDEX),]
  Names_Index$diagonal <- diag(scaled_distance_matrix)
  Names_Index <- arrange(Names_Index, diagonal)
  
  dir.create("Case_Outputs\\diagonal_movement_plots")
  
  ggplot(data = Names_Index[251:306,]) +
    geom_point(aes(x = diagonal, y = reorder(areaName, diagonal))) +
    ylab("LTLA") + xlab("Probability of case staying in LTLA") -> diagonal_movement_plot1
  
  png(file="Case_Outputs\\diagonal_movement_plots\\diagonal_movement_plot_1.png",
      width=1440, height=1080, res = 150)
  plot(diagonal_movement_plot1)
  dev.off()
  #
  ggplot(data = Names_Index[201:250,]) +
    geom_point(aes(x = diagonal, y = reorder(areaName, diagonal))) +
    ylab("LTLA") + xlab("Probability of case staying in LTLA") -> diagonal_movement_plot2
  
  png(file="Case_Outputs\\diagonal_movement_plots\\diagonal_movement_plot_2.png",
      width=1440, height=1080, res = 150)
  plot(diagonal_movement_plot2)
  dev.off()
  #
  ggplot(data = Names_Index[151:200,]) +
    geom_point(aes(x = diagonal, y = reorder(areaName, diagonal))) +
    ylab("LTLA") + xlab("Probability of case staying in LTLA") -> diagonal_movement_plot3
  
  png(file="Case_Outputs\\diagonal_movement_plots\\diagonal_movement_plot_3.png",
      width=1440, height=1080, res = 150)
  plot(diagonal_movement_plot3)
  dev.off()
  #
  ggplot(data = Names_Index[101:150,]) +
    geom_point(aes(x = diagonal, y = reorder(areaName, diagonal))) +
    ylab("LTLA") + xlab("Probability of case staying in LTLA") -> diagonal_movement_plot4
  
  png(file="Case_Outputs\\diagonal_movement_plots\\diagonal_movement_plot_4.png",
      width=1440, height=1080, res = 150)
  plot(diagonal_movement_plot4)
  dev.off()
  #
  ggplot(data = Names_Index[51:100,]) +
    geom_point(aes(x = diagonal, y = reorder(areaName, diagonal))) +
    ylab("LTLA") + xlab("Probability of case staying in LTLA") -> diagonal_movement_plot5
  
  png(file="Case_Outputs\\diagonal_movement_plots\\diagonal_movement_plot_5.png",
      width=1440, height=1080, res = 150)
  plot(diagonal_movement_plot5)
  dev.off()
  #
  ggplot(data = Names_Index[1:50,]) +
    geom_point(aes(x = diagonal, y = reorder(areaName, diagonal))) +
    ylab("LTLA") + xlab("Probability of case staying in LTLA") -> diagonal_movement_plot6
  
  png(file="Case_Outputs\\diagonal_movement_plots\\diagonal_movement_plot_7.png",
      width=1440, height=1080, res = 150)
  plot(diagonal_movement_plot6)
  dev.off()
  
  #######################################################################################
  #I'll also output the probability spread for each LTLA
  dir.create("Case_Outputs\\gravity_probabilities")
  
  Names_Index <- unique(Case_Rates_Data[,c(1,3,5)])
  Names_Index <- Names_Index[order(Names_Index$INDEX),]
  
  areaCodes_used <- unique(Case_Rates_Data$areaCode)
  Boundaries_reduced <- filter(Boundaries, CODE %in% areaCodes_used)
  
  for(i in 1:length(Names_Index$areaName)){
    plot_data <- Names_Index
    LTLA_hold <- Names_Index$areaName[i]
    code_hold <- Names_Index$areaCode[i]
    
    Names_Index$probabilities <- scaled_distance_matrix[i,]
    Names_Index$probabilities[i] <- NA
    
    Boundaries_reduced$probabilities <- Names_Index$probabilities
  
    ggplot(Boundaries_reduced) +
      geom_sf(aes(fill = probabilities)) +
      scale_fill_gradient2() +
      ggtitle(sprintf("%s - Probability of case travel", LTLA_hold)) -> plot_hold
    
    #London_plots <- ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
    #  geom_sf(aes(fill = probabilities)) +
    #  scale_fill_gradient2()
    
    png(file=sprintf("Case_Outputs\\gravity_probabilities\\%s_%s.png", i, LTLA_hold),
        width=1440, height=1080, res = 150)
    plot(plot_hold)
    dev.off()
      
  }
  
  #######################################################################################
  
  model_approx_y <- array(0, dim = c(103,306)) #
  #y[,1] ~ poisson_log(log(susceptible_proxy[,1].*((scaled_distance_matrix*E[,1]))) + beta0 + (x[1] * betas) + (beta_random_walk[1]) + theta);

  model_approx_y[1,] <- as.numeric(log(susceptible_proxy[,1]*(scaled_distance_matrix%*%E[1,] ))) + model_beta0 + x[1,,]%*%model_betas + (model_beta_random_walk[1]) + model_theta
  for(i in 2:103){
  model_approx_y[i,] <- as.numeric(log(susceptible_proxy[,i]*(scaled_distance_matrix%*%E[i,] ))) + model_beta0 + x[i,,]%*%model_betas + (model_beta_random_walk[i] - model_beta_random_walk[i-1]) + model_theta
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
  BASIC_week_difference <- abs(basic_approx_y - E)
  
  ##
  #Let's quickly plot the random walks too.
  dates_hold <- unique(Case_Rates_Data[,c(2,7)])
  random_walk_plot <- data.frame(date = as.Date(dates_hold$date_begin), random_walk_value = model_beta_random_walk)
  
  ggplot(data = random_walk_plot) +
    geom_line(aes(x = date, y = random_walk_value)) ->random_walk_p1
  
  png(file="Case_Outputs//random_walk_values.png",
      width=1440, height=1080, res = 150)
  plot(random_walk_p1)
  dev.off()
  
  #And also plot the differences
  random_walk_differences <- random_walk_plot[-1,]
  random_walk_differences$random_walk_value <- model_beta_random_walk[2:103] - model_beta_random_walk[1:102]
  
  ggplot(data = random_walk_differences) +
    geom_line(aes(x = date, y = random_walk_value)) +
    ylab("Random walk difference terms (rw[i] - rw[i-1])") -> random_walk_p2
  
  png(file="Case_Outputs//random_walk_differences.png",
      width=1440, height=1080, res = 150)
  plot(random_walk_p2)
  dev.off()
  ##
  
  #First thing we'll make, is a plot for each of the 306 LTLAs showing the real data against the model approx data.
  #In 34 3x3 plots
  LTLAs_by_Index <- unique(Case_Rates_Data[,c(1,3,4,5)])
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
      
    
    png(file=sprintf("Case_Outputs//Goodness_of_fit//basic_comparison_trajectories_grid//trajectories_%s.png", i),
        width=1440, height=1080, res = 150)
    plot(plot_combined)
    dev.off()
    
  }
  
  
  plot(REAL_week_difference, MODEL_week_difference)
  #What if we just do y ~ poisson
  plot(y[,2], model_approx_y[,2])
  ############################################################################
  
  for(i in 1:(306)){

      j <- i
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
      
      
      
  
    png(file=sprintf("Case_Outputs//Goodness_of_fit//log_basic_comparison_trajectories//trajectories_%s.png", i),
        width=1440, height=1080, res = 150)
    plot(plotHold)
    dev.off()
    
  }
  #############################################################################
  ############################################################################
  
  for(i in 1:(306)){
    
    j <- i
    areaCode_plot <- LTLAs_by_Index$areaCode[j]
    areaName_plot <- LTLAs_by_Index$areaName[j]
    
    dataHold <- data.frame(Date = rep(AllDates[2:103],2),
                           areaCode = rep(areaCode_plot, 204),
                           areaName = rep(areaName_plot, 204),
                           Source = c(rep("Real Data", 102), rep("Model Approx.",102)),
                           Cases = c(y[1:102,j],model_approx_y[1:102,j]),
                           LineType = c(rep("1",204))
    )
    dataHold$date <- as.Date(dataHold$Date)
    
    ggplot(dataHold) +
      geom_line(aes(x = Date, y = Cases, color = Source, linetype = LineType), size = 1, alpha = 0.6) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      scale_x_date(date_breaks = "2 month", date_labels = "%b %y") + 
      ggtitle(paste(areaName_plot, " ", areaCode_plot)) +
      scale_linetype(guide = "none") -> plotHold
    
    
    
    
    png(file=sprintf("Case_Outputs//Goodness_of_fit//trajectories//trajectories_%s.png", i),
        width=1440, height=1080, res = 150)
    plot(plotHold)
    dev.off()
    
  }
  #############################################################################
  ############################################################################
  
  for(i in 1:(306)){
    
    j <- i
    areaCode_plot <- LTLAs_by_Index$areaCode[j]
    areaName_plot <- LTLAs_by_Index$areaName[j]
    
    dataHold <- data.frame(Date = rep(AllDates[2:103],2),
                           areaCode = rep(areaCode_plot, 204),
                           areaName = rep(areaName_plot, 204),
                           Source = c(rep("Real Data", 102), rep("Model Approx.",102)),
                           Cases = c(y[1:102,j],model_approx_y[1:102,j]),
                           LineType = c(rep("1",204))
    )
    dataHold$date <- as.Date(dataHold$Date)
    
    ggplot(dataHold) +
      geom_line(aes(x = Date, y = log(Cases), color = Source, linetype = LineType), size = 1, alpha = 0.6) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      scale_x_date(date_breaks = "2 month", date_labels = "%b %y") + 
      ggtitle(paste(areaName_plot, " ", areaCode_plot)) +
      scale_linetype(guide = "none") -> plotHold
    
    
    
    
    png(file=sprintf("Case_Outputs//Goodness_of_fit//log_trajectories//trajectories_%s.png", i),
        width=1440, height=1080, res = 150)
    plot(plotHold)
    dev.off()
    
  }
  #############################################################################
  
  for(i in 1:(306)){
    
    j <- i
    areaCode_plot <- LTLAs_by_Index$areaCode[j]
    areaName_plot <- LTLAs_by_Index$areaName[j]
    
    dataHold <- data.frame(Date = rep(AllDates[2:103],3),
                           areaCode = rep(areaCode_plot, 306),
                           areaName = rep(areaName_plot, 306),
                           Source = c(rep("Real Data", 102), rep("Model Approx.",102), rep("Same R assumed",102)),
                           Week_to_Week_Difference = c(REAL_week_difference[1:102,j],MODEL_week_difference[1:102,j],BASIC_week_difference[1:102,j]),
                           LineType = c(rep("1",204), rep("2",102))
    )
    dataHold$date <- as.Date(dataHold$Date)
    
    ggplot(dataHold) +
      geom_line(aes(x = Date, y = log(Week_to_Week_Difference), color = Source, linetype = LineType), size = 1, alpha = 0.6) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      scale_x_date(date_breaks = "2 month", date_labels = "%b %y") + 
      ggtitle(paste(areaName_plot, " ", areaCode_plot)) +
      scale_linetype(guide = "none") -> plotHold
    
    
    
    
    png(file=sprintf("Case_Outputs//Goodness_of_fit//log_week_to_week_basic_trajectories//trajectories_%s.png", i),
        width=1440, height=1080, res = 150)
    plot(plotHold)
    dev.off()
    
  }
  
  for(i in 1:(306)){
    
    j <- i
    areaCode_plot <- LTLAs_by_Index$areaCode[j]
    areaName_plot <- LTLAs_by_Index$areaName[j]
    
    dataHold <- data.frame(Date = rep(AllDates[2:103],3),
                           areaCode = rep(areaCode_plot, 306),
                           areaName = rep(areaName_plot, 306),
                           Source = c(rep("Real Data", 102), rep("Model Approx.",102), rep("Same R assumed",102)),
                           Week_to_Week_Difference = c(REAL_week_difference[1:102,j],MODEL_week_difference[1:102,j],BASIC_week_difference[1:102,j]),
                           LineType = c(rep("1",204), rep("2",102))
    )
    dataHold$date <- as.Date(dataHold$Date)
    
    ggplot(dataHold) +
      geom_line(aes(x = Date, y = Week_to_Week_Difference, color = Source, linetype = LineType), size = 1, alpha = 0.6) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      scale_x_date(date_breaks = "2 month", date_labels = "%b %y") + 
      ggtitle(paste(areaName_plot, " ", areaCode_plot)) +
      scale_linetype(guide = "none") -> plotHold
    
    
    
    
    png(file=sprintf("Case_Outputs//Goodness_of_fit//week_to_week_basic_trajectories//trajectories_%s.png", i),
        width=1440, height=1080, res = 150)
    plot(plotHold)
    dev.off()
    
  }
  #############################################################################
  for(i in 1:(306)){
    
    j <- i
    areaCode_plot <- LTLAs_by_Index$areaCode[j]
    areaName_plot <- LTLAs_by_Index$areaName[j]
    
    dataHold <- data.frame(Date = rep(AllDates[2:103],2),
                           areaCode = rep(areaCode_plot, 204),
                           areaName = rep(areaName_plot, 204),
                           Source = c(rep("Real Data", 102), rep("Model Approx.",102)),
                           Week_to_Week_Difference = c(REAL_week_difference[1:102,j],MODEL_week_difference[1:102,j]),
                           LineType = c(rep("1",204))
    )
    dataHold$date <- as.Date(dataHold$Date)
    
    ggplot(dataHold) +
      geom_line(aes(x = Date, y = Week_to_Week_Difference, color = Source, linetype = LineType), size = 1, alpha = 0.6) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      scale_x_date(date_breaks = "2 month", date_labels = "%b %y") + 
      ggtitle(paste(areaName_plot, " ", areaCode_plot)) +
      scale_linetype(guide = "none") -> plotHold
    
    
    
    
    png(file=sprintf("Case_Outputs//Goodness_of_fit//week_to_week_trajectories//trajectories_%s.png", i),
        width=1440, height=1080, res = 150)
    plot(plotHold)
    dev.off()
    
  }
  
  #############################################################################
  #############################################################################
  for(i in 1:(306)){
    
    j <- i
    areaCode_plot <- LTLAs_by_Index$areaCode[j]
    areaName_plot <- LTLAs_by_Index$areaName[j]
    
    dataHold <- data.frame(Date = rep(AllDates[2:103],2),
                           areaCode = rep(areaCode_plot, 204),
                           areaName = rep(areaName_plot, 204),
                           Source = c(rep("Real Data", 102), rep("Model Approx.",102)),
                           Week_to_Week_Difference = c(REAL_week_difference[1:102,j],MODEL_week_difference[1:102,j]),
                           LineType = c(rep("1",204))
    )
    dataHold$date <- as.Date(dataHold$Date)
    
    ggplot(dataHold) +
      geom_line(aes(x = Date, y = log(Week_to_Week_Difference), color = Source, linetype = LineType), size = 1, alpha = 0.6) +
      theme_classic() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      scale_x_date(date_breaks = "2 month", date_labels = "%b %y") + 
      ggtitle(paste(areaName_plot, " ", areaCode_plot)) +
      scale_linetype(guide = "none") -> plotHold
    
    
    
    
    png(file=sprintf("Case_Outputs//Goodness_of_fit//log_week_to_week_trajectories//trajectories_%s.png", i),
        width=1440, height=1080, res = 150)
    plot(plotHold)
    dev.off()
    
  }
  
  #############################################################################
  #Plot the week to week difference
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
                             Week_to_Week_Difference = c(REAL_week_difference[1:102,j],MODEL_week_difference[1:102,j],BASIC_week_difference[1:102,j]),
                             LineType = c(rep("1",204), rep("2",102))
      )
      dataHold$date <- as.Date(dataHold$Date)
      
      ggplot(dataHold) +
        geom_line(aes(x = Date, y = Week_to_Week_Difference, color = Source, linetype = LineType), size = 1, alpha = 0.6) +
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
    
    
    png(file=sprintf("Case_Outputs//Goodness_of_fit//week_to_week_basic_trajectories_grid//trajectories_%s.png", i),
        width=1440, height=1080, res = 150)
    plot(plot_combined)
    dev.off()
    
  }
  #############################################################################
  png(file="Case_Outputs//Goodness_of_fit//log_week_to_week_difference.png",
      width=1440, height=1080, res = 150)
  plot(log(REAL_week_difference), log(MODEL_week_difference))
  abline(a=0, b=1, col = "red") 
  dev.off()
  
  png(file="Case_Outputs//Goodness_of_fit//week_to_week_difference.png",
      width=1440, height=1080, res = 150)
  plot(REAL_week_difference, MODEL_week_difference)
  abline(a=0, b=1, col = "red") 
  dev.off()
  #############################################################################  
  our_model_error <- abs(y - model_approx_y)
  basic_error <- abs(y - basic_approx_y)

  #Plot all times per LTLA  
  
  plot(log(our_model_error[,50]), log(basic_error[,50]))
  abline(a=0, b=1, col = "red")      
  
  #Plot all LTLAs per week
  plot(log(our_model_error[50,]), log(basic_error[50,]))
  abline(a=0, b=1, col = "red") 
  
  #Make a plot of boxplots for each week, showing the population adjusted error for each LTLA (so boxplot shows 306 LTLAs)
  #Make the data
  Error_data <- data.frame(Time = rep(as.Date("01/01/1999"),31518),
                           Pop_Adj_Error = rep(0,31518))
  for(i in 1:103){
    Error_data$Time[(((i-1)*306)+1):((i)*306)] <- AllDates[i]
    Error_data$Pop_Adj_Error[(((i-1)*306)+1):((i)*306)] <- our_model_error[i,]/LTLAs_by_Index$Population
  }

  total_error <- sum(Error_data$Pop_Adj_Error)
  
  ggplot(Error_data, aes(x=Time, y=Pop_Adj_Error, group = Time)) + 
    geom_boxplot(alpha = 0.2) + ylab("Population Adjusted Error across all LTLAs") +
    ylim(c(0,0.1)) + ggtitle(sprintf("Total Error: %s",total_error)) + theme_bw() -> error_box
    
  png(file="Case_Outputs//Goodness_of_fit/Boxplot_error.png",
      width=1440, height=1080, res = 150)
  plot(error_box)
  dev.off()
  
  Mean_Weekly_Pop_error <- aggregate(Error_data, list(Error_data$Time), mean)

  ggplot(Mean_Weekly_Pop_error) +
    geom_line(aes(x = Time, y = Pop_Adj_Error), size = 1) + theme_minimal() +
    ylab("Weekly Mean Population Adjusted Error") -> Mean_error_plot

  png(file="Case_Outputs//Goodness_of_fit/Mean_Weekly_Error.png",
      width=1440, height=1080, res = 150)
  plot(Mean_error_plot)
  #dev.off()
  while (!is.null(dev.list()))  dev.off()
      
##############################################################################



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



