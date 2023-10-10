#orderly::orderly_develop_start("11_figure_demos", use_draft = "newer", parameters = list(tree_depth = 15, total_iterations = 4000, n_chains = 16))

#This script loads in the stanfit object and outputs some demos of paper figures to play around with
load("stanfit.RData")
load("model_data.RData")
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
n_chains: %s \n
total_iterations: %s \n
  scale_by_susceptible_pool: %s \n
  cases_type: %s \n
  use_SGTF_data: %s \n
  final_week: %s \n
  random_walk_prior_scale: %s \n
  rw_penalty: %s \n
  print_extra_gof:  %s ", tree_depth, n_chains, total_iterations,
                        scale_by_susceptible_pool, cases_type,
                        use_SGTF_data, final_week,
                        random_walk_prior_scale, rw_penalty, print_extra_gof)

fileConn<-file("parameters_used.txt")
writeLines(param_string, fileConn)
close(fileConn)

#####################################
#Start by extracting all the values from all the chains.
###########
#Here's some switches to possibly save time:

#If TRUE then we don't run all the long code, we just get the final data frames at the end
Preload_data <- FALSE
#Do we want to then factor in the Poisson uncertainty?
include_NegBin <- TRUE

if(Preload_data){
  load("CI_data_outputs.RData")
} else{
  
  
  
  
  list_of_draws <- rstan::extract(stanfit)
  print(names(list_of_draws))
  n_draws <- length(list_of_draws$sqrtQ)
  #We have 32000 draws, that's because we had 2000 iterations (after a 2000 warm-up) and 16 chains. 16*2000
  
  #We now want to have our 32,000 associated draws for y_approx
  y_approx <- array(data = NA, dim = c((final_week-1),306,n_draws))
  
  for(j in 1:n_draws){
    
    if(scale_by_susceptible_pool){
      model_susc_scale <- list_of_draws$susc_scaling[j]
    }
    
    for(i in 1:T){
      if(scale_by_susceptible_pool){
        y_approx[i,,j] <- as.numeric(((model_susc_scale*susceptible_proxy[,i])*(E[i,] + (list_of_draws$zetas[j,] *E_neighbours[,i]))))*exp((list_of_draws$beta_random_walk[j,i]) + list_of_draws$theta[j,])
        
      }else{
        y_approx[i,,j] <- as.numeric((susceptible_proxy[,i]*(E[i,] + (list_of_draws$zetas[j,] *E_neighbours[,i]))))*exp((list_of_draws$beta_random_walk[j,i]) + list_of_draws$theta[j,])
      }
    }
    
  }
  
  model_phi <- list_of_draws$phi
  mean_phi <- mean(model_phi)
  
  if(include_NegBin){
    mean_y <- array(dim=c(95,306))
    median_y <- array(dim=c(95,306))
    CI_y <- array(dim=c(2,95,306))
    
    for(i in 1:95){
      for(j in 1:306){
        print(i)
        hold_values <- rep(NA, n_draws*1000)
        for(k in 1:n_draws){
          hold_values[(((k-1)*1000)+1):(((k-1)*1000)+1000)] <- rnbinom(1000, size = model_phi[k], mu = y_approx[i,j,k])
        }
        mean_y[i,j] <- mean(hold_values)
        median_y[i,j] <- median(hold_values)
        CI_y[,i,j] <- quantile(hold_values, c(0.025, 0.975))
      }
    }
    
  } else{
    
    #and lastly, we want to then collapse this down to the mean, median, and 95% CIs
    
    # Calculate the mean/median along the 3rd dimension
    mean_y <- apply(y_approx, c(1, 2), mean)
    median_y <- apply(y_approx, c(1, 2), median)
    
    # Calculate 95% credible interval
    CI_y <- apply(y_approx, c(1, 2), function(x) quantile(x, c(0.025, 0.975)))
    
    
  }
#Next, we need a dataframe that has the LTLA, date, week_cases, and then the mean, median and lower upper in it as well

LTLAs_by_Index <- unique(Case_Rates_Data[,c(1,3,4,5)])
LTLAs_by_Index <- LTLAs_by_Index[order(LTLAs_by_Index$INDEX),]
#Major error spotted here! The cases are in the wrong place! The dates aren't the right order!
#Remember, our main default is using the P2 Case linelist data. y = Linelist_P2_PCR_Next_Week_Cases
AllDates <- sort(unique(Case_Rates_Data$date_begin))
#Also remember, because the y data here are technically speaking the "next week" values, that
#we want to use +7 to the dates
AllDates <- AllDates + 7

Model_fit_data <- data.frame(areaCode = rep(NA, 95*306),
                             areaName = rep(NA, 95*306),
                             date = as.Date(rep("2020-07-07", 95*306)),
                             Population = rep(NA, 95*306),
                             Real_Cases = rep(NA, 95*306),
                             Model_mean = rep(NA, 95*306),
                             Model_median = rep(NA, 95*306),
                             Model_lower = rep(NA, 95*306),
                             Model_upper = rep(NA, 95*306))

for(i in 1:95){
  for( j in 1:306){
    data_index <- ((i-1)*306) + j
    Model_fit_data$areaCode[data_index] <- LTLAs_by_Index$areaCode[j]
    Model_fit_data$areaName[data_index] <- LTLAs_by_Index$areaName[j]
    Model_fit_data$date[data_index] <- as.Date(AllDates[i])
    Model_fit_data$Population[data_index] <- LTLAs_by_Index$Population[j]
    Model_fit_data$Real_Cases[data_index] <- y[i,j]
    Model_fit_data$Model_mean[data_index] <- mean_y[i,j]
    Model_fit_data$Model_median[data_index] <- median_y[i,j]
    Model_fit_data$Model_lower[data_index] <- CI_y[1,i,j]
    Model_fit_data$Model_upper[data_index] <- CI_y[2,i,j]
  }
}
#Something's wrong, y doesn't seem to match the actual Case_Rates data...
#Yes it does, just remember we're using the "next week" P2 PCR cases
test <- filter(Model_fit_data, areaName == "Bolton")
test2 <- filter(Case_Rates_Data, areaName == "Bolton")

#I also want to do the total sum for the whole country:
england_daily_total <- Model_fit_data %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(Real_Cases = sum(Real_Cases),
            Model_mean = sum(Model_mean),
            Model_median = sum(Model_median),
            Model_lower = sum(Model_lower),
            Model_upper = sum(Model_upper))

}
# Let's plot the england total to start with

#Save the data.frames
save(Model_fit_data, england_daily_total, file = "CI_data_outputs.RData")
#save(Model_fit_data, england_daily_total, file = "poisson_y_approx.RData")


grey_lines <- c(
  "2021-01-05", ## 16. Lockdown 3 starts
  "2021-03-08", ## 17. Step 1 of roadmap: schools reopen
  "2021-04-19", ## 19. Step 2 of roadmap: outdoors hospitality (04-12) 
  ##     and schools return (04-19)
  "2021-05-17", ## 20. Step 3 of roadmap: indoors hospitality
  "2021-07-19") ## 24. Step 4

ggplot(data = england_daily_total, aes(x= date, y = Model_mean )) +
  geom_line(size = 1, color = "#9BC362") + 
  geom_ribbon(aes(ymin = Model_lower, ymax = Model_upper), alpha = 0.3, fill = "#9BC362", show.legend = FALSE) + 
  theme_classic() + ylab('Weekly Cases') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  coord_cartesian(xlim = c(as.Date('2020-05-01'),as.Date('2022-04-01'))) +
  #labs(color = "Strategy") +
  geom_vline(xintercept = as.Date('2021-03-31'), alpha = 0.9, color = 'black') +
  geom_vline(xintercept = as.Date(grey_lines), alpha = 0.7, color = 'gray23', lty = 'dashed') +
  xlab(NULL) +
  theme(axis.text=element_text(size=rel(1.2)),
        axis.title=element_text(size=rel(1.3)),
        legend.text = element_text(size=rel(1.2)),
        legend.title = element_text(size=rel(1.3))) +
  ggtitle("Weekly cases reported in England") +
  geom_point(data = england_daily_total, aes(x = date, y = Real_Cases), alpha = 0.7, shape = 18,
             color = 'black')  -> england_plot

png(file="england_fit.png",
    width=1440, height=1080, res = 150)
plot(england_plot)
dev.off()

LTLA_names <- unique(Model_fit_data$areaName)

for(i in 1:length(LTLA_names)){
  
  areaName_hold <- LTLA_names[i]

#Let's just do a one off, for example, of Bolton
  ggplot(data = filter(Model_fit_data, areaName == areaName_hold), aes(x= date, y = Model_mean )) +
    geom_line(size = 1, color = "#9BC362") + 
    geom_ribbon(aes(ymin = Model_lower, ymax = Model_upper), alpha = 0.3, fill = "#9BC362", show.legend = FALSE) + 
    theme_classic() + ylab('Weekly Cases') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
    coord_cartesian(xlim = c(as.Date('2020-05-01'),as.Date('2022-04-01'))) +
    #labs(color = "Strategy") +
    geom_vline(xintercept = as.Date('2021-03-31'), alpha = 0.9, color = 'black') +
    geom_vline(xintercept = as.Date(grey_lines), alpha = 0.7, color = 'gray23', lty = 'dashed') +
    xlab(NULL) +
    theme(axis.text=element_text(size=rel(1.2)),
          axis.title=element_text(size=rel(1.3)),
          legend.text = element_text(size=rel(1.2)),
          legend.title = element_text(size=rel(1.3))) +
    ggtitle(sprintf("Weekly cases reported in %s", areaName_hold)) +
  geom_point(data = filter(Model_fit_data, areaName == areaName_hold), aes(x = date, y = Real_Cases), alpha = 0.7, shape = 18,
             color = 'black') -> plot_hold
  
  png(file=sprintf("Case_Outputs\\model_fit_%s_%s.png", i, areaName_hold),
                   width=1440, height=1080, res = 150)
  plot(plot_hold)
  dev.off()
}


