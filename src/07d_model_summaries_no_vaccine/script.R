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
################################################################################
T <- final_week - 1

#Let's print an output .txt of the parameters used:
param_string <- sprintf("tree_depth: %s \n
  scale_by_number_of_neighbours: %s \n
  scale_by_susceptible_pool: %s \n
  cases_type: %s \n
  use_SGTF_data: %s \n
  final_week: %s \n
  theta_ON: %s \n
  random_walk_prior_scale: %s \n
  rw_penalty: %s \n
  print_extra_gof:  %s ", tree_depth, scale_by_number_of_neighbours, 
                        scale_by_susceptible_pool, cases_type,
                         use_SGTF_data, final_week,
                        theta_ON,
                        random_walk_prior_scale, rw_penalty, print_extra_gof)

fileConn<-file("parameters_used.txt")
writeLines(param_string, fileConn)
close(fileConn)

#Our parameters and priors:
#beta0 ~ normal(0.0, 1.0);
#betas ~ normal(0.0, 1.0); #covariate coefficients
#zetas ~ gamma(0.05, 1.0); #spatial kernel  1-306
#theta[,i] ~ normal(0.0, 1.0); #noise

#Output summaries of the mixed chains for all parameters except theta
if(scale_by_susceptible_pool){
  main_summaries <- summary(stanfit, pars = c('sqrtQ', 'susc_scaling', 'betas', 'beta_random_walk', 'zetas', 'lp__'))
  write.csv(main_summaries$summary,"Case_Outputs/main_summaries.csv", row.names = TRUE)
  
}else{
main_summaries <- summary(stanfit, pars = c('sqrtQ', 'betas', 'beta_random_walk', 'zetas', 'lp__'))
write.csv(main_summaries$summary,"Case_Outputs/main_summaries.csv", row.names = TRUE)
}

if(theta_ON){
#Theta summaries are big, but we output anyway
theta_summaries <- summary(stanfit, pars = c('theta_mu', 'theta_sd','theta'))
write.csv(theta_summaries$summary,"Case_Outputs/theta_summaries.csv", row.names = TRUE)
} 


#Save a plot of the beta trajectories
beta_trajectories1 <- rstan::traceplot(stanfit, pars=c('sqrtQ', sprintf('betas[%s]',1:11)), nrow = 3)
png(file="Case_Outputs\\beta_trajectories1.png",
    width=1440, height=1080, res = 150)
plot(beta_trajectories1)
dev.off()

beta_trajectories2 <- rstan::traceplot(stanfit, pars= sprintf('betas[%s]',12:18))
png(file="Case_Outputs\\beta_trajectories2.png",
    width=1440, height=1080, res = 150)
plot(beta_trajectories2)
dev.off()

if(scale_by_susceptible_pool){
  susc_trajectories <- rstan::traceplot(stanfit, pars=c('susc_scaling'))
  png(file="Case_Outputs\\susc_scaling_trajectories.png",
      width=1440, height=1080, res = 150)
  plot(susc_trajectories)
  dev.off()
}

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

if(T > 75){
rw_trajectories3 <- rstan::traceplot(stanfit, pars=c(sprintf('beta_random_walk[%s]',51:75)), nrow = 5)
png(file="Case_Outputs\\rw_trajectories3.png",
    width=1440, height=1080, res = 150)
plot(rw_trajectories3)
dev.off()

rw_trajectories4 <- rstan::traceplot(stanfit, pars=c(sprintf('beta_random_walk[%s]',76:T)), nrow = 5)
png(file="Case_Outputs\\rw_trajectories4.png",
    width=1440, height=1080, res = 150)
plot(rw_trajectories4)
dev.off()
}else{
  rw_trajectories3 <- rstan::traceplot(stanfit, pars=c(sprintf('beta_random_walk[%s]',51:T)), nrow = 5)
  png(file="Case_Outputs\\rw_trajectories3.png",
      width=1440, height=1080, res = 150)
  plot(rw_trajectories3)
  dev.off()
}
#And just the steps of the RW too:
######################
rw_trajectories1 <- rstan::traceplot(stanfit, pars=c(sprintf('beta_random_walk_steps[%s]',1:25)), nrow = 5)
png(file="Case_Outputs\\rw_steps_trajectories1.png",
    width=1440, height=1080, res = 150)
plot(rw_trajectories1)
dev.off()

rw_trajectories2 <- rstan::traceplot(stanfit, pars=c(sprintf('beta_random_walk_steps[%s]',26:50)), nrow = 5)
png(file="Case_Outputs\\rw_steps_trajectories2.png",
    width=1440, height=1080, res = 150)
plot(rw_trajectories2)
dev.off()

if(T > 75){
rw_trajectories3 <- rstan::traceplot(stanfit, pars=c(sprintf('beta_random_walk_steps[%s]',51:75)), nrow = 5)
png(file="Case_Outputs\\rw_steps_trajectories3.png",
    width=1440, height=1080, res = 150)
plot(rw_trajectories3)
dev.off()

rw_trajectories4 <- rstan::traceplot(stanfit, pars=c(sprintf('beta_random_walk_steps[%s]',76:T)), nrow = 5)
png(file="Case_Outputs\\rw_steps_trajectories4.png",
    width=1440, height=1080, res = 150)
plot(rw_trajectories4)
dev.off()
}else{
  rw_trajectories3 <- rstan::traceplot(stanfit, pars=c(sprintf('beta_random_walk_steps[%s]',51:T)), nrow = 5)
  png(file="Case_Outputs\\rw_steps_trajectories3.png",
      width=1440, height=1080, res = 150)
  plot(rw_trajectories3)
  dev.off()
}
######################


#We'll save the zeta plots, though unlikely to be that interested...
dir.create("Case_Outputs\\zeta_plots")
for(i in 1:19){
  zeta_trajectories <- rstan::traceplot(stanfit, pars =sprintf('zetas[%s]',((i-1)*16 + 1):(16*i)))
  png(file=sprintf("Case_Outputs\\zeta_plots\\zeta_trajectories_%s.png", i),
      width=1440, height=1080, res = 150)
  plot(zeta_trajectories)
  dev.off()
}

zeta_trajectories <- rstan::traceplot(stanfit, pars =sprintf('zetas[%s]',304:306))
png(file=sprintf("Case_Outputs\\zeta_plots\\zeta_trajectories_%s.png", 20),
    width=1440, height=1080, res = 150)
plot(zeta_trajectories)
dev.off()


##We export a map of the UK LTLAs and shaded with the value of their zetas
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
  scale_fill_viridis_c() +
  ggtitle("LTLAs, nearest neighbor model")

London_zetas <- ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = zetas)) +
  scale_fill_viridis_c()

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

  labels_hold <- c(
    "1) prop_white_british", "2) prop_asian",
    "3) prop_black_afr", "4) IMD_Average_score",
    "5) prop_o65", "6) Median_annual_income",
    "7) workplaces_movement",
    "8) residential_movement", "9) transit_movement")


beta_summaries <- main_summaries$summary
#Next we have a look at how good our fit is
betas_1_9 <- plot(stanfit, pars = sprintf('betas[%s]',1:9))
#ci_level: 0.8 (80% intervals)
#outer_level: 0.95 (95% intervals)
betas_1_9 <- betas_1_9 + scale_y_continuous(breaks = c(9:1),
                                labels = labels_hold) +
  geom_vline(xintercept = 0, color = "skyblue", lty = 5, size = 1) +ggtitle("Covariate coefficients")

png(file="Case_Outputs\\betas_1_9.png",
    width=1440, height=1080, res = 150)
plot(betas_1_9)
dev.off()


betas_13_15 <- plot(stanfit, pars = sprintf('betas[%s]',10:12))
#ci_level: 0.8 (80% intervals)
#outer_level: 0.95 (95% intervals)
betas_13_15 <- betas_13_15 + scale_y_continuous(breaks = c(3:1),
                                              labels = c("10) Alpha", "11) Delta", 
                                                         "12) Omicron")) +
  geom_vline(xintercept = 0, color = "skyblue", lty = 5, size = 1) +ggtitle("Variant proportion coefficients")

png(file="Case_Outputs\\betas_10_12.png",
    width=1440, height=1080, res = 150)
plot(betas_13_15)
dev.off()

betas_16_21 <- plot(stanfit, pars = sprintf('betas[%s]',13:18))
#ci_level: 0.8 (80% intervals)
#outer_level: 0.95 (95% intervals)
betas_16_21 <- betas_16_21 + scale_y_continuous(breaks = c(6:1),
                                                labels = c("13) Core Services", "14) Primary Care", 
                                                           "15) Specialised Services", "16) Unringfenced",
                                                           "17) Outbreak Management", "18) ASC infection control")) +
  geom_vline(xintercept = 0, color = "skyblue", lty = 5, size = 1) +ggtitle("Funding coefficients")

png(file="Case_Outputs\\betas_13_18.png",
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
#y is the REAL "next week cases". T (time) x 306 (LTLAs)
#E is the "current" weeks cases.  i.e. E[2] = y[1]

 
  dir.create("Case_Outputs//Goodness_of_fit")
  dir.create("Case_Outputs//Goodness_of_fit//week_to_week_trajectories")
  dir.create("Case_Outputs//Goodness_of_fit//trajectories")
  dir.create("Case_Outputs//Goodness_of_fit//basic_comparison_trajectories_grid")
  
  
  
  if(print_extra_gof){
    dir.create("Case_Outputs//Goodness_of_fit//log_trajectories")
    dir.create("Case_Outputs//Goodness_of_fit//log_basic_comparison_trajectories")
    dir.create("Case_Outputs//Goodness_of_fit//week_to_week_basic_trajectories")
    dir.create("Case_Outputs//Goodness_of_fit//week_to_week_basic_trajectories_grid")
    dir.create("Case_Outputs//Goodness_of_fit//log_week_to_week_trajectories")
    dir.create("Case_Outputs//Goodness_of_fit//log_week_to_week_basic_trajectories")
    
  }
  
  #Script model definition:
  #y[,i] ~ poisson_log(log(susceptible_proxy[,i].*(E[,i] + (zetas .*E_neighbours[,i]))) + beta0 + x[i] * betas + theta);  // extra noise removed removed: + theta[,i]
  #y_hosp[,i+average_hosp_lag] ~ poisson_log(log(LTLA_to_region*y_as_matrix[,i]) + beta0_hosp + x_hosp[i]*betas_hosp + theta_hosp);
  
  model_betas <- as.numeric(get_posterior_mean(stanfit, pars = 'betas')[,5])
  #model_beta0 <- as.numeric(get_posterior_mean(stanfit, pars = 'beta0')[,5])
  model_beta_random_walk <- as.numeric(get_posterior_mean(stanfit, pars = 'beta_random_walk')[,5])
  model_beta_random_walk_steps <- as.numeric(get_posterior_mean(stanfit, pars = 'beta_random_walk_steps')[,5])
  model_zetas <- as.numeric(get_posterior_mean(stanfit, pars = 'zetas')[,5])
  
  if(theta_ON){
  model_theta <- as.numeric(get_posterior_mean(stanfit, pars = 'theta')[,5])
  model_theta_mu <- as.numeric(get_posterior_mean(stanfit, pars = 'theta_mu')[,5])
  model_theta_sd <- as.numeric(get_posterior_mean(stanfit, pars = 'theta_sd')[,5])
  }
  
  if(scale_by_susceptible_pool){
    model_susc_scale <- as.numeric(get_posterior_mean(stanfit, pars = 'susc_scaling')[,5])
  }
  
  model_approx_y <- array(0, dim = c(T,306)) #
  
  
  if(theta_ON){
  for(i in 1:T){
    if(scale_by_susceptible_pool){
      model_approx_y[i,] <- as.numeric(log((model_susc_scale*susceptible_proxy[,i])*(E[i,] + (model_zetas *E_neighbours_scaled[,i])))) + x[i,,]%*%model_betas + (model_beta_random_walk[i]) + model_theta
      
    }else{
  model_approx_y[i,] <- as.numeric(log(susceptible_proxy[,i]*(E[i,] + (model_zetas *E_neighbours_scaled[,i])))) + x[i,,]%*%model_betas + (model_beta_random_walk[i]) + model_theta
    }
  }
  }else{
    
    for(i in 1:T){
      if(scale_by_susceptible_pool){
        model_approx_y[i,] <- as.numeric(log((model_susc_scale*susceptible_proxy[,i])*(E[i,] + (model_zetas *E_neighbours_scaled[,i])))) + x[i,,]%*%model_betas + (model_beta_random_walk[i])
        
      }else{
        model_approx_y[i,] <- as.numeric(log(susceptible_proxy[,i]*(E[i,] + (model_zetas *E_neighbours_scaled[,i])))) + x[i,,]%*%model_betas + (model_beta_random_walk[i])
      }
    }
    
  }
  
  
  
  model_approx_y <- exp(model_approx_y)
  
  REAL_week_difference <- abs(y - E)
  MODEL_week_difference <- abs(model_approx_y - E)
  
  #Is the model better than just assuming the same rate of increase from the week previous?
  basic_approx_y <- array(0, dim = c(T,306))
  for(i in 3:T){
    for(j in 1:306){
      basic_approx_y[i,j] <- y[i-1,j]*(y[i-1,j]/y[i-2,j])
    }
  }
  BASIC_week_difference <- abs(basic_approx_y - E)
  
  ##
  #Let's quickly plot the random walks too.
  dates_hold <- unique(Case_Rates_Data[,c("Week","date_begin")])
  #Order it by week
  dates_hold <- dates_hold[order(dates_hold$Week,decreasing=FALSE),]
  random_walk_plot <- data.frame(date = as.Date(dates_hold$date_begin), random_walk_value = model_beta_random_walk)
  random_walk_steps_plot <- data.frame(date = as.Date(dates_hold$date_begin), random_walk_steps_value = model_beta_random_walk_steps)
  
  ggplot(data = random_walk_plot) +
    geom_line(aes(x = date, y = random_walk_value)) ->random_walk_p1
  
  png(file="Case_Outputs//random_walk_values.png",
      width=1440, height=1080, res = 150)
  plot(random_walk_p1)
  dev.off()
  
  ggplot(data = random_walk_steps_plot) +
    geom_line(aes(x = date, y = random_walk_steps_value)) ->random_walk_steps_p1
  
  png(file="Case_Outputs//random_walk_steps_values.png",
      width=1440, height=1080, res = 150)
  plot(random_walk_steps_p1)
  dev.off()
  
  #And also plot the differences
  random_walk_differences <- random_walk_plot[-1,]
  random_walk_differences$random_walk_value <- model_beta_random_walk[2:T] - model_beta_random_walk[1:(T-1)]
  
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
      
      dataHold <- data.frame(Date = rep(AllDates[2:T],3),
                             areaCode = rep(areaCode_plot, (T-1)*3),
                             areaName = rep(areaName_plot, (T-1)*3),
                             Source = c(rep("Real Data", T-1), rep("Model Approx.",T-1), rep("Same R assumed",T-1)),
                             Cases = c(y[1:T-1,j],model_approx_y[1:(T-1),j],basic_approx_y[1:(T-1),j]),
                             LineType = c(rep("1",(T-1)*2), rep("2",T-1))
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
  
  
  #plot(REAL_week_difference, MODEL_week_difference)
  #What if we just do y ~ poisson
  #plot(y[,2], model_approx_y[,2])
###########################################################
  #########################################################
  for(i in 1:(306)){
    
    j <- i
    areaCode_plot <- LTLAs_by_Index$areaCode[j]
    areaName_plot <- LTLAs_by_Index$areaName[j]
    
    dataHold <- data.frame(Date = rep(AllDates[2:T],2),
                           areaCode = rep(areaCode_plot, (T-1)*2),
                           areaName = rep(areaName_plot, (T-1)*2),
                           Source = c(rep("Real Data", (T-1)), rep("Model Approx.",(T-1))),
                           Cases = c(y[1:(T-1),j],model_approx_y[1:(T-1),j]),
                           LineType = c(rep("1",(T-1)*2))
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
  for(i in 1:(306)){
    
    j <- i
    areaCode_plot <- LTLAs_by_Index$areaCode[j]
    areaName_plot <- LTLAs_by_Index$areaName[j]
    
    dataHold <- data.frame(Date = rep(AllDates[2:T],2),
                           areaCode = rep(areaCode_plot, (T-1)*2),
                           areaName = rep(areaName_plot, (T-1)*2),
                           Source = c(rep("Real Data", (T-1)), rep("Model Approx.",(T-1))),
                           Week_to_Week_Difference = c(REAL_week_difference[1:(T-1),j],MODEL_week_difference[1:(T-1),j]),
                           LineType = c(rep("1",(T-1)*2))
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
  if(print_extra_gof){
  ############################################################################
  
  for(i in 1:(306)){
    
    j <- i
    areaCode_plot <- LTLAs_by_Index$areaCode[j]
    areaName_plot <- LTLAs_by_Index$areaName[j]
    
    dataHold <- data.frame(Date = rep(AllDates[2:T],2),
                           areaCode = rep(areaCode_plot, (T-1)*2),
                           areaName = rep(areaName_plot, (T-1)*2),
                           Source = c(rep("Real Data", (T-1)), rep("Model Approx.",(T-1))),
                           Cases = c(y[1:(T-1),j],model_approx_y[1:(T-1),j]),
                           LineType = c(rep("1",(T-1)*2))
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
  
  ############################################################################
  
  for(i in 1:(306)){
    
    j <- i
    areaCode_plot <- LTLAs_by_Index$areaCode[j]
    areaName_plot <- LTLAs_by_Index$areaName[j]
    
    dataHold <- data.frame(Date = rep(AllDates[2:T],3),
                           areaCode = rep(areaCode_plot, (T-1)*3),
                           areaName = rep(areaName_plot, (T-1)*3),
                           Source = c(rep("Real Data", (T-1)), rep("Model Approx.",(T-1)), rep("Same R assumed",(T-1))),
                           Cases = c(y[1:(T-1),j],model_approx_y[1:(T-1),j],basic_approx_y[1:(T-1),j]),
                           LineType = c(rep("1",(T-1)*2), rep("2",(T-1)))
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
  
  
  #############################################################################
  
  for(i in 1:(306)){
    
    j <- i
    areaCode_plot <- LTLAs_by_Index$areaCode[j]
    areaName_plot <- LTLAs_by_Index$areaName[j]
    
    dataHold <- data.frame(Date = rep(AllDates[2:T],3),
                           areaCode = rep(areaCode_plot, (T-1)*3),
                           areaName = rep(areaName_plot, (T-1)*3),
                           Source = c(rep("Real Data", (T-1)), rep("Model Approx.",(T-1)), rep("Same R assumed",(T-1))),
                           Week_to_Week_Difference = c(REAL_week_difference[1:(T-1),j],MODEL_week_difference[1:(T-1),j],BASIC_week_difference[1:(T-1),j]),
                           LineType = c(rep("1",(T-1)*2), rep("2",(T-1)))
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
    
    dataHold <- data.frame(Date = rep(AllDates[2:T],3),
                           areaCode = rep(areaCode_plot, (T-1)*3),
                           areaName = rep(areaName_plot, (T-1)*3),
                           Source = c(rep("Real Data", (T-1)), rep("Model Approx.",(T-1)), rep("Same R assumed",(T-1))),
                           Week_to_Week_Difference = c(REAL_week_difference[1:(T-1),j],MODEL_week_difference[1:(T-1),j],BASIC_week_difference[1:(T-1),j]),
                           LineType = c(rep("1",(T-1)*2), rep("2",(T-1)))
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
  #############################################################################
  for(i in 1:(306)){
    
    j <- i
    areaCode_plot <- LTLAs_by_Index$areaCode[j]
    areaName_plot <- LTLAs_by_Index$areaName[j]
    
    dataHold <- data.frame(Date = rep(AllDates[2:T],2),
                           areaCode = rep(areaCode_plot, (T-1)*2),
                           areaName = rep(areaName_plot, (T-1)*2),
                           Source = c(rep("Real Data", (T-1)), rep("Model Approx.",(T-1))),
                           Week_to_Week_Difference = c(REAL_week_difference[1:(T-1),j],MODEL_week_difference[1:(T-1),j]),
                           LineType = c(rep("1",(T-1)*2))
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
      
      dataHold <- data.frame(Date = rep(AllDates[2:T],3),
                             areaCode = rep(areaCode_plot, (T-1)*3),
                             areaName = rep(areaName_plot, (T-1)*3),
                             Source = c(rep("Real Data", (T-1)), rep("Model Approx.",(T-1)), rep("Same R assumed",(T-1))),
                             Week_to_Week_Difference = c(REAL_week_difference[1:(T-1),j],MODEL_week_difference[1:(T-1),j],BASIC_week_difference[1:(T-1),j]),
                             LineType = c(rep("1",(T-1)*2), rep("2",(T-1)))
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
  Error_data <- data.frame(Time = rep(as.Date("01/01/1999"),(306*T)),
                           Pop_Adj_Error = rep(0,(306*T)))
  for(i in 1:T){
    Error_data$Time[(((i-1)*306)+1):((i)*306)] <- AllDates[i]
    Error_data$Pop_Adj_Error[(((i-1)*306)+1):((i)*306)] <- our_model_error[i,]/LTLAs_by_Index$Population
  }

  total_error <- sum(Error_data$Pop_Adj_Error)
  
  ggplot(Error_data, aes(x=Time, y=Pop_Adj_Error, group = Time)) + 
    geom_boxplot(alpha = 0.2) + ylab("Population Adjusted Error across all LTLAs") +
    ylim(c(0,plyr::round_any(max(Error_data$Pop_Adj_Error), 0.02, f=ceiling))) + ggtitle(sprintf("Total Error: %s",total_error)) + theme_bw() -> error_box
    
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



