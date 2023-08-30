#orderly::orderly_develop_start("11_figure_demos", use_draft = "newer", parameters = list(tree_depth = 15, total_iterations = 4000, n_chains = 16))

#This script loads in the stanfit object and outputs some demos of paper figures to play around with
load("stanfit.RData")
load("model_data.RData")
dir.create("Case_Outputs")
dir.create("Case_Outputs\\LTLA_model_fits")
dir.create("Case_Outputs\\fig_demos")

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
include_Poisson <- TRUE

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
    y_approx[i,,j] <- as.numeric(log((model_susc_scale*susceptible_proxy[,i])*(E[i,] + (list_of_draws$zetas[j,] *E_neighbours[,i])))) + x[i,,]%*%list_of_draws$betas[j,] + (list_of_draws$beta_random_walk[j,i]) + list_of_draws$theta[j,]
    
  }else{
    y_approx[i,,j] <- as.numeric(log(susceptible_proxy[,i]*(E[i,] + (list_of_draws$zetas[j,] *E_neighbours[,i])))) + x[i,,]%*%list_of_draws$betas[j,] + (list_of_draws$beta_random_walk[j,i]) + list_of_draws$theta[j,]
  }
}
  
}



if(include_Poisson){
  mean_y <- array(dim=c(95,306))
  median_y <- array(dim=c(95,306))
  CI_y <- array(dim=c(2,95,306))
  
  for(i in 1:95){
    for(j in 1:306){
      print(i)
      hold_values <- rep(NA, n_draws*1000)
      for(k in 1:n_draws){
        hold_values[(((k-1)*1000)+1):(((k-1)*1000)+1000)] <- rpois(1000, exp(y_approx[i,j,k]))
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

#I may as well take the exponential here
mean_y <- exp(mean_y)
median_y <- exp(median_y)
CI_y <- exp(CI_y)

}
#Next, we need a dataframe that has the LTLA, date, week_cases, and then the mean, median and lower upper in it as well

LTLAs_by_Index <- unique(Case_Rates_Data[,c(1,3,4,5)])
LTLAs_by_Index <- LTLAs_by_Index[order(LTLAs_by_Index$INDEX),]
#Major error spotted here! The cases are in the wrong place! The dates aren't the right order!
#Remember, our main default is using the P2 Case linelist data. y = Linelist_P2_PCR_Next_Week_Cases
#Only the pillar 2, and PCR_only cases from the direct England linelist

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

# Let's plot the england total to start with

}

##################################


#grey_lines <- c(
#  "2021-01-05", ## 16. Lockdown 3 starts
  #"2021-03-08", ## 17. Step 1 of roadmap: schools reopen
  #"2021-04-19", ## 19. Step 2 of roadmap: outdoors hospitality (04-12) 
  ##     and schools return (04-19)
  #"2021-05-17", ## 20. Step 3 of roadmap: indoors hospitality
  #"2021-07-19") ## 24. Step 4
#)

grey_lines <- c(
  "2020-06-01", ## End of first stay-at-home-order
  "2020-11-05", ## Start of second stay at home order
  "2020-12-02", ## End of second stay-at-home order 
  "2021-01-06", ## Start of third stay-at-home order
  "2021-03-08", ## End of third stay-at-home order (step 1)
  "2021-07-19" ## End of steps, no more legal restrictions
)

variant_dates <- c( #Dates that variants first appear in our dataset
  "2020-07-26", #ALPHA
  "2021-03-28", #DELTA
  "2021-09-12"  #OMICRON
)

lockdown_shades <- data.frame(date_start = as.Date(c("2020-04-01", grey_lines)),
                              date_end = as.Date(c(grey_lines, '2022-04-01' )),
                              rect_bottom = rep(775, 7),
                              rect_top = rep(800, 7),
                              rect_col = c("red", "orange", "red", "orange", "red", "orange", "green"))

######################################

ggplot() +
  geom_line(data = england_daily_total, aes(x = date, y = Model_mean / 1000,
                                            color = "Model Fit"), size = 1) +
  geom_ribbon(data = england_daily_total,
              aes(x = date, ymin = Model_lower / 1000, ymax = Model_upper / 1000,
                  fill = "Model Fit"), alpha = 0.3, show.legend = FALSE) +
  theme_classic() + ylab('Weekly New Cases (thousands)') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y") +
  coord_cartesian(xlim = c(as.Date('2020-05-01'), as.Date('2022-04-01'))) +
  geom_rect(data = lockdown_shades, aes(xmin = as.Date(date_start), xmax = as.Date(date_end),
                                        ymin = 775, ymax = 800),
            fill = c('#FFB6B6', "#FFD6A5", '#FFB6B6', "#FFD6A5", '#FFB6B6', "#FFD6A5", "#C8E7C1"),
            show.legend = FALSE) +
  
  geom_vline(xintercept = as.Date(variant_dates), alpha = 0.9, color = 'black') +
  geom_vline(xintercept = as.Date(grey_lines), alpha = 0.7, color = 'gray23', lty = 'dashed') +
  xlab("Date") +
  annotate('text', x = as.Date("2020-07-26") - 13, label = "Alpha emergence", y = 500, colour = "black", size = 4, angle = 90) +
  annotate('text', x = as.Date("2021-03-28") - 13, label = "Delta emergence", y = 500, colour = "black", size = 4, angle = 90) +
  annotate('text', x = as.Date("2021-09-12") - 13, label = "Omicron emergence", y = 500, colour = "black", size = 4, angle = 90) +
  
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.2)),
        legend.title = element_text(size = rel(1.3))) +
  ggtitle("Weekly total Pillar 2 and PCR cases reported in England ") +
  geom_point(data = england_daily_total, aes(x = date, y = Real_Cases / 1000,
                                             color = "Data"),
             alpha = 0.7, shape = 18) +
  scale_color_manual(values = c("Data" = "black", "Model Fit" = "#9BC362")) +
  scale_fill_manual(values = c("Model Fit" = "#9BC362")) +  # Adjust fill for the ribbon legend
  
  # Modify legend labels
  guides(color = guide_legend(override.aes = list(shape = c(18, NA), linetype = c(NA, 1)))) +
  labs(color = "")  -> england_plot

######################################


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
  
  png(file=sprintf("Case_Outputs\\LTLA_model_fits\\model_fit_%s_%s.png", i, areaName_hold),
      width=1440, height=1080, res = 150)
  plot(plot_hold)
  dev.off()
}

dir.create("Case_Outputs\\LTLA_infection_rates")

for(i in 1:length(LTLA_names)){
  
  areaName_hold <- LTLA_names[i]
  
  #Let's just do a one off, for example, of Bolton
  ggplot(data = filter(Model_fit_data, areaName == areaName_hold), aes(x= date, y = Model_mean/Population )) +
    geom_line(size = 1, color = "#9BC362") + 
    geom_ribbon(aes(ymin = Model_lower/Population, ymax = Model_upper/Population), alpha = 0.3, fill = "#9BC362", show.legend = FALSE) + 
    theme_classic() + ylab('Weekly Case rates') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
    coord_cartesian(xlim = c(as.Date('2020-05-01'),as.Date('2022-04-01'))) +
    geom_vline(xintercept = as.Date(grey_lines), alpha = 0.7, color = 'gray23', lty = 'dashed') +
    xlab(NULL) +
    theme(axis.text=element_text(size=rel(1.2)),
          axis.title=element_text(size=rel(1.3)),
          legend.text = element_text(size=rel(1.2)),
          legend.title = element_text(size=rel(1.3))) +
    ggtitle(sprintf("Weekly case rates reported in %s", areaName_hold)) -> plot_hold
  
  png(file=sprintf("Case_Outputs\\LTLA_infection_rates\\case_rate_fit_%s_%s.png", i, areaName_hold),
      width=1440, height=1080, res = 150)
  plot(plot_hold)
  dev.off()
}


####################################################
#I want to output plots for LTLA_A, and LTLA_B to compare
LTLA_A_index <- 267
LTLA_B_index <- 67
LTLA_A_name <- LTLA_names[LTLA_A_index]
LTLA_B_name <- LTLA_names[LTLA_B_index]
LTLA_A_data <- filter(Model_fit_data, areaName == LTLA_A_name)
LTLA_B_data <- filter(Model_fit_data, areaName == LTLA_B_name)

ggplot() +
  geom_rect(data = lockdown_shades, aes(xmin = as.Date(date_start), xmax = as.Date(date_end),
                                        ymin = 0.019, ymax = 0.02),
            fill = c('#FFB6B6', "#FFD6A5", '#FFB6B6', "#FFD6A5", '#FFB6B6', "#FFD6A5", "#C8E7C1"),
            show.legend = FALSE) +
  geom_line(data = LTLA_A_data, aes(x = date, y = Model_mean / Population,
                                            color = "Model Fit"), size = 1, show.legend = FALSE) +
  geom_ribbon(data = LTLA_A_data,
              aes(x = date, ymin = Model_lower / Population, ymax = Model_upper / Population,
                  fill = "Model Fit"), alpha = 0.3, show.legend = FALSE) +
  theme_classic() + ylab('Weekly Case Rates') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") +
  coord_cartesian(xlim = c(as.Date('2020-05-01'), as.Date('2022-04-01'))) +
  ylim(c(0,0.02)) +
  #geom_vline(xintercept = as.Date(variant_dates), alpha = 0.9, color = 'black') +
  geom_vline(xintercept = as.Date(grey_lines), alpha = 0.7, color = 'gray23', lty = 'dashed') +
  xlab("Date") +
  #annotate('text', x = as.Date("2020-07-26") - 13, label = "Alpha emergence", y = 500, colour = "black", size = 4, angle = 90) +
  #annotate('text', x = as.Date("2021-03-28") - 13, label = "Delta emergence", y = 500, colour = "black", size = 4, angle = 90) +
  #annotate('text', x = as.Date("2021-09-12") - 13, label = "Omicron emergence", y = 500, colour = "black", size = 4, angle = 90) +
  
  theme(axis.text.y = element_text(size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.2)),
        legend.title = element_text(size = rel(1.3)),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  ggtitle(sprintf("%s ", LTLA_A_name)) +
  geom_point(data = LTLA_A_data, aes(x = date, y = Real_Cases / Population,
                                             color = "Real Data"),
             alpha = 0.7, shape = 18, show.legend = FALSE) +
  scale_color_manual(values = c("Real Data" = "black", "Model Fit" = "#9BC362")) +
  scale_fill_manual(values = c("Model Fit" = "#9BC362")) +  # Adjust fill for the ribbon legend
  # Modify legend labels
  guides(color = guide_legend(override.aes = list(shape = c(18, NA), linetype = c(NA, 1)))) +
  labs(color = "Legend")  -> LTLA_A_plot

ggplot() +
  geom_rect(data = lockdown_shades, aes(xmin = as.Date(date_start), xmax = as.Date(date_end),
                                        ymin = 0.019, ymax = 0.02),
            fill = c('#FFB6B6', "#FFD6A5", '#FFB6B6', "#FFD6A5", '#FFB6B6', "#FFD6A5", "#C8E7C1"),
            show.legend = FALSE) +
  geom_line(data = LTLA_B_data, aes(x = date, y = Model_mean / Population,
                                    color = "Model Fit"), size = 1, show.legend = FALSE) +
  geom_ribbon(data = LTLA_B_data,
              aes(x = date, ymin = Model_lower / Population, ymax = Model_upper / Population,
                  fill = "Model Fit"), alpha = 0.3, show.legend = FALSE) +
  theme_classic() + ylab('') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") +
  coord_cartesian(xlim = c(as.Date('2020-05-01'), as.Date('2022-04-01'))) +
  ylim(c(0,0.02)) +
  #geom_vline(xintercept = as.Date(variant_dates), alpha = 0.9, color = 'black') +
  geom_vline(xintercept = as.Date(grey_lines), alpha = 0.7, color = 'gray23', lty = 'dashed') +
  xlab("Date") +
  #annotate('text', x = as.Date("2020-07-26") - 13, label = "Alpha emergence", y = 500, colour = "black", size = 4, angle = 90) +
  #annotate('text', x = as.Date("2021-03-28") - 13, label = "Delta emergence", y = 500, colour = "black", size = 4, angle = 90) +
  #annotate('text', x = as.Date("2021-09-12") - 13, label = "Omicron emergence", y = 500, colour = "black", size = 4, angle = 90) +
  
  theme(axis.text.y = element_text(size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.2)),
        legend.title = element_text(size = rel(1.3)),
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) +
  ggtitle(sprintf("%s ", LTLA_B_name)) +
  geom_point(data = LTLA_B_data, aes(x = date, y = Real_Cases / Population,
                                     color = "Real Data"),
             alpha = 0.7, shape = 18, show.legend = FALSE) +
  scale_color_manual(values = c("Real Data" = "black", "Model Fit" = "#9BC362")) +
  scale_fill_manual(values = c("Model Fit" = "#9BC362")) +  # Adjust fill for the ribbon legend
  # Modify legend labels
  guides(color = guide_legend(override.aes = list(shape = c(18, NA), linetype = c(NA, 1)))) +
  labs(color = "Legend")  -> LTLA_B_plot


##########################

#Now stick it all together in a cowplot.


LTLAs_plot <- plot_grid(LTLA_A_plot, LTLA_B_plot, labels = c('B', 'C'))

Fig1_plot <- plot_grid(england_plot, LTLAs_plot, nrow = 2, labels = c("A", ""))

#Export the plot
ggsave(filename = "f1_trajectories.png",
       path = 'Case_Outputs\\fig_demos', plot = Fig1_plot,
       dpi=300, height=11, width=11, units="in")

ggsave(filename = "f1_trajectories.tiff",
       path = 'Case_Outputs\\fig_demos', plot = Fig1_plot,
       dpi=300, height=11, width=11, units="in")

ggsave(filename = "f1_trajectories.pdf",
       path = 'Case_Outputs\\fig_demos', plot = Fig1_plot,
       dpi=300, height=11, width=11, units="in")

############################################################################
#FIG 2
############################################################################
#This figure will show regional variation in certain covariates
#IMD, and, 


load("Boundaries_Data.RData")
#Remove to just the indices I've modelled:
areaCodes_used <- unique(Case_Rates_Data$areaCode)
Boundaries_reduced <- filter(Boundaries, CODE %in% areaCodes_used)

#I'm fairly sure that the Boundaries file is the INDEX order, yes look:
# for(i in 1:length(Case_Rates_Data$date)){
#   hold_code <- Case_Rates_Data$areaCode[i]
#   Case_Rates_Data$INDEX[i] <- which(Boundaries$CODE == hold_code)
# }

#IMD average score is the same for every date
Case_Rates_Data %>%
  filter(Week == 10) %>% #Any week will do
  select(areaCode, areaName, INDEX, IMD_Average_score, prop_white_british) -> fig2_data

fig2_data <- fig2_data[order(fig2_data$INDEX),]

Boundaries_reduced$IMD_Average_score <- as.numeric(fig2_data$IMD_Average_score) 
Boundaries_reduced$prop_white_british <- as.numeric(fig2_data$prop_white_british) 

# gb_cities <- read.csv("gb_cities.csv")
# #Manually trim off the ones we don't want.
# gb_cities <- gb_cities[1:30,]
# gb_cities <- gb_cities[-c(6,7,9,16,17,18,19,21,22,24,25,27,29,30),]
# 
# gb_cities %>%
#   st_as_sf(coords = c("lng", "lat"), crs = 4326) -> gb_cities

#PLOT standard
  ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = IMD_Average_score)) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  c("#9BC362", "white", "#e54339"),  # Blue, white, and red colors
    values = scales::rescale(c(min(Boundaries_reduced$IMD_Average_score), median(Boundaries_reduced$IMD_Average_score), max(Boundaries_reduced$IMD_Average_score)))
  ) +
  ggtitle("Average Index of Multiple Deprivation (IMD) by LTLA") +
    labs(fill = "Average \nIMD Score") +
  theme_void() +
    theme(plot.margin = unit(c(0, 0, 0, 0.5), "cm")) +
    # Highlight the two specific regions
  geom_sf(data = Boundaries_reduced %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
          color = "blue", fill = NA, size = 0.8) -> england_IMD
  
  ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
    geom_sf(aes(fill = IMD_Average_score), show.legend = FALSE) +
    #scale_fill_viridis_c() +
    scale_fill_gradientn(
      colours =  c("#9BC362", "white", "#e54339"),  # Blue, white, and red colors
      values = scales::rescale(c(min(Boundaries_reduced$IMD_Average_score), median(Boundaries_reduced$IMD_Average_score), max(Boundaries_reduced$IMD_Average_score)))
    ) +
    #ggtitle("Average Index of Multiple Deprivation (IMD) by LTLA") +
    labs(fill = "Average \nIMD Score") +
    theme_void() +
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) +
    # Highlight the two specific regions
    geom_sf(data = Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),] %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
            color = "blue", fill = NA, size = 0.8) -> london_IMD
  


  ggplot(Boundaries_reduced) +
    geom_sf(aes(fill = prop_white_british)) +
    #scale_fill_viridis_c() +
    scale_fill_gradientn(
      colours =  c("#9BC362", "white", "#e54339"),  # Blue, white, and red colors
      values = scales::rescale(c(min(Boundaries_reduced$prop_white_british), median(Boundaries_reduced$prop_white_british), max(Boundaries_reduced$prop_white_british)))
    ) +
    ggtitle("White British Proportion of LTLA Population") +
    labs(fill = "Proportion \nWhite British") +
    theme_void() +
    theme(plot.margin = unit(c(0, 0.5, 0, 0), "cm")) +
    # Highlight the two specific regions
    geom_sf(data = Boundaries_reduced %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
            color = "blue", fill = NA, size = 0.8) -> england_prop_white
  
  
Fig2_plot <- plot_grid(england_IMD, england_prop_white, 
                       nrow = 1, labels = c("A", "B"), align = "v", axis = "bt") +
  theme(plot.background = element_rect(fill = "white"))

ggsave(filename = "f2_covariates.png",
       path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
       dpi=300, height=6.31, width=11, units="in")

ggsave(filename = "f2_covariates.tiff",
       path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
       dpi=300, height=6.31, width=11, units="in")

ggsave(filename = "f2_covariates.pdf",
       path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
       dpi=300, height=6.31, width=11, units="in")

#########################################
#FIGURE 3

#This figure will be my covariate CrI. I want them all on one plot, 
#but we can have them separated by sections

#I say we just use the stanfit default and alter it slightly to better suit our wants

labels_hold <- c(
  "1) Proportion Asian", "2) Proportion Black Afr/Car",
  "3) Proportion Other Ethnicity", "4) IMD Average Score",
  "5) Proportion Over Age 65", "6) Population per km^2",
  "7) Median Annual Income", "8) Time At Workplace",
  "9) Time At Home", "10) Time At Transit Stations",
  "17) First dose vacc", "18) Second dose vacc",
  "19) Third dose vacc")


#Next we have a look at how good our fit is
betas_1_10 <- stan_plot(stanfit, pars = sprintf('betas[%s]',c(1:10,17,18,19)),
                        fill_color = "#e54339",
                        show_density = FALSE)
#ci_level: 0.8 (80% intervals)
#outer_level: 0.95 (95% intervals)
  betas_1_10 + scale_y_continuous(breaks = c(10:1),
                                              labels =labels_hold) +
  geom_vline(xintercept = 0, color = "#9BC362", lty = 5, size = 1) +
    ggtitle("Covariate Coefficients") +
    xlim(c(-0.4, 0.45)) +
  #theme(axis.text.y = element_text(angle = 35, hjust = 1, vjust = 0)) +
    theme( # remove the vertical grid lines
      panel.grid.major.x = element_blank() ,
      # explicitly set the horizontal lines (or they will disappear too)
      panel.grid.major.y = element_line( size=.1, color="grey" ),
      #axis.text.y = element_markdown(size = rel(1.2)),
      axis.text.y = element_text(size = rel(1.2)),
      axis.title.y = element_blank(),
      legend.text = element_text(size = rel(1.2)),
      legend.title = element_text(size = rel(1.3)),
      axis.text.x = element_blank()
    ) -> betas_1_10

  betas_11_13 <- stan_plot(stanfit, pars = sprintf('betas[%s]',11:13),
                          fill_color = "#e54339",
                          show_density = FALSE)
  #ci_level: 0.8 (80% intervals)
  #outer_level: 0.95 (95% intervals)
  betas_11_13 + scale_y_continuous(breaks = c(3:1),
                                  labels = c("11) Alpha Proportion", "12) Delta Proportion", "13) Omicron Proportion")) +
    geom_vline(xintercept = 0, color = "#9BC362", lty = 5, size = 1) +
    ggtitle("SARS-CoV-2 Variant Coefficients") +
    xlim(c(-0.4, 0.45)) +
    #theme(axis.text.y = element_text(angle = 35, hjust = 1, vjust = 0)) +
    theme( # remove the vertical grid lines
      panel.grid.major.x = element_blank() ,
      # explicitly set the horizontal lines (or they will disappear too)
      panel.grid.major.y = element_line( size=.1, color="grey" ),
      axis.text.y = element_text(size = rel(1.2)),
      axis.title.y = element_blank(),
      axis.text.x = element_blank()
    )  -> betas_11_13
  
  betas_14_16 <- stan_plot(stanfit, pars = sprintf('betas[%s]',14:16),
                           fill_color = "#e54339",
                           show_density = FALSE)
  #ci_level: 0.8 (80% intervals)
  #outer_level: 0.95 (95% intervals)
  betas_14_16 + scale_y_continuous(breaks = c(3:1),
                                   labels = c("14) Unringfenced",
                                              "15) Outbreak Management", "16) ASC infection control")) +
    geom_vline(xintercept = 0, color = "#9BC362", lty = 5, size = 1) +
    ggtitle("Funding Coefficients") +
    xlim(c(-0.4, 0.45)) +
    #theme(axis.text.y = element_text(angle = 35, hjust = 1, vjust = 0)) +
    theme( # remove the vertical grid lines
      panel.grid.major.x = element_blank() ,
      # explicitly set the horizontal lines (or they will disappear too)
      panel.grid.major.y = element_line( size=.1, color="grey" ),
      axis.text.y = element_text(size = rel(1.2)),
      axis.text.x = element_text(size = rel(1.4)),
      axis.title.y = element_blank()
    )  -> betas_14_16
  
  
  Fig3_plot <- plot_grid(betas_1_10, betas_11_13, betas_14_16,
                         nrow = 3, rel_heights = c(10,3,3), align = "v")

  
  ggsave(filename = "f3_betas.png",
         path = 'Case_Outputs\\fig_demos', plot = Fig3_plot,
         dpi=300, height=10, width=9, units="in")
  
  ggsave(filename = "f3_betas.tiff",
         path = 'Case_Outputs\\fig_demos', plot = Fig3_plot,
         dpi=300, height=10, width=9, units="in")
  
  ggsave(filename = "f3_betas.pdf",
         path = 'Case_Outputs\\fig_demos', plot = Fig3_plot,
         dpi=300, height=10, width=9, units="in")
  ########################################
#FIGURE 4 COUNTERFACTUAL
#We'll have a series of stacked line plots showing the national total of cases under different 
  #counterfactuals
  
#IMD all set to min or max.
#outbreak management fund all set to 0 or 1.
  FIG4_OFF <- TRUE
  
  if(FIG4_OFF){
    
  }else{
  
  
#We start with IMD
#So who has the worst IMD?  
best_IMD_name <- fig2_data$areaName[which(fig2_data$IMD_Average_score == min(fig2_data$IMD_Average_score))]
best_IMD_value <- fig2_data$IMD_Average_score[which(fig2_data$IMD_Average_score == min(fig2_data$IMD_Average_score))]

worst_IMD_name <- fig2_data$areaName[which(fig2_data$IMD_Average_score == max(fig2_data$IMD_Average_score))]
worst_IMD_value <- fig2_data$IMD_Average_score[which(fig2_data$IMD_Average_score == max(fig2_data$IMD_Average_score))]

#Let's change our x matrix then

x_IMD_worst_counterfactual <- x
x_IMD_worst_counterfactual[,,4] <- max(x[,,4])

x_IMD_best_counterfactual <- x
x_IMD_best_counterfactual[,,4] <- min(x[,,4])

#Furthermore, we're going to need Population data in index order
Population_Data <- unique(select(Case_Rates_Data, Population, INDEX))
Population_Data <- Population_Data[order(Population_Data$INDEX),]

#Now we calculate the new y_approx values
#If TRUE then we don't run all the long code, we just get the final data frames at the end
Preload_data <- FALSE
#Do we want to then factor in the Poisson uncertainty?
include_Poisson <- FALSE

#The first episode scaling factor we'll use:
Best_Factor <- 1.167667668

#To preserve memory we do each scenario one at-a-time
if(Preload_data){
  #load("CI_data_outputs.RData")
} else{
  
  
  
  
  list_of_draws <- extract(stanfit)
  print(names(list_of_draws))
  n_draws <- length(list_of_draws$sqrtQ)
  #We have 32000 draws, that's because we had 2000 iterations (after a 2000 warm-up) and 16 chains. 16*2000
  
  #We now want to have our 32,000 associated draws for y_approx
  y_approx_IMD_worst <- array(data = NA, dim = c((final_week-1),306,n_draws))
  y_approx_IMD_best <- array(data = NA, dim = c((final_week-1),306,n_draws))
  
  for(j in 1:n_draws){
    
    #Now keep in mind, our susceptible proxy may be a bit different here.
    #Task 13 shows how we came up with a new way to build a value for "first episodes"
    #At each time-step, we need to calculate the new number of first episodes for that time-step
    #Keep a running total over time, and the susceptible proxy can be rebuilt from that
    if(scale_by_susceptible_pool){
      model_susc_scale <- list_of_draws$susc_scaling[j]
    }
    #Another thing to keep in mind, is that we don't want to calculate the cases from E, that was the previous cases
    #in the actual model fit, we need to build a NEW E to go along with it
    
    counterfactual_first_episodes_total_worst <- array(0, dim = c(N,T))
    counterfactual_susceptible_proxy_worst <- array(0, dim = c(N,T))
    counterfactual_E_worst <- array(0, dim = c(T,N))
    counterfactual_E_neighbours_worst <- array(0, dim = c(N,T))
    
    counterfactual_first_episodes_total_best <- array(0, dim = c(N,T))
    counterfactual_susceptible_proxy_best <- array(0, dim = c(N,T))
    counterfactual_E_best <- array(0, dim = c(T,N))
    counterfactual_E_neighbours_best <- array(0, dim = c(N,T))
    
    #First, we do the i = 1 case
    if(scale_by_susceptible_pool){
      #Susceptible proxy is a 306x95
      counterfactual_E_worst[1,] <- E[1,]
      counterfactual_first_episodes_total_worst[,1] <- E[1,]
      counterfactual_susceptible_proxy_worst[,1] <- 1-(counterfactual_first_episodes_total_worst[,1]/Population_Data$Population)
      
      counterfactual_E_best[1,] <- E[1,]
      counterfactual_first_episodes_total_best[,1] <- E[1,]
      counterfactual_susceptible_proxy_best[,1] <- 1-(counterfactual_first_episodes_total_best[,1]/Population_Data$Population)
      
      #Also need to calculate E_neighbours, the number of neighborhood cases
      #TODO: Future runs of task 09 will output W_reduced, but for now:
      load('W.RData')
      W_reduced <- W[Population_Data$INDEX, Population_Data$INDEX]
      
      counterfactual_E_neighbours_worst <- W_reduced%*%t(counterfactual_E_worst)
      counterfactual_E_neighbours_best <- W_reduced%*%t(counterfactual_E_best)
      
      y_approx_IMD_worst[1,,j] <- as.numeric(log((model_susc_scale*counterfactual_susceptible_proxy_worst[,1])*(counterfactual_E_worst[1,] + (list_of_draws$zetas[j,] *counterfactual_E_neighbours_worst[,1])))) + x_IMD_worst_counterfactual[1,,]%*%list_of_draws$betas[j,] + (list_of_draws$beta_random_walk[j,1]) + list_of_draws$theta[j,]
      y_approx_IMD_best[1,,j] <- as.numeric(log((model_susc_scale*counterfactual_susceptible_proxy_best[,1])*(counterfactual_E_best[1,] + (list_of_draws$zetas[j,] *counterfactual_E_neighbours_best[,1])))) + x_IMD_best_counterfactual[1,,]%*%list_of_draws$betas[j,] + (list_of_draws$beta_random_walk[j,1]) + list_of_draws$theta[j,]
      
      
    }else{
      counterfactual_E_worst[1,] <- E[1,]
      counterfactual_first_episodes_total_worst[,1] <- E[1,]
      counterfactual_susceptible_proxy_worst <- array(1, dim = c(N,T))
      counterfactual_E_best[1,] <- E[1,]
      counterfactual_first_episodes_total_best[,1] <- E[1,]
      counterfactual_susceptible_proxy_best[,1] <- array(1, dim = c(N,T))
      
      load('W.RData')
      W_reduced <- W[Population_Data$INDEX, Population_Data$INDEX]
      
      counterfactual_E_neighbours_worst <- W_reduced%*%t(counterfactual_E_worst)
      counterfactual_E_neighbours_best <- W_reduced%*%t(counterfactual_E_best)
      
      
      y_approx_IMD_worst[1,,j] <- as.numeric(log(counterfactual_susceptible_proxy_worst[,1]*(counterfactual_E_worst[1,] + (list_of_draws$zetas[j,] *counterfactual_E_neighbours_worst[,1])))) + x_IMD_worst_counterfactual[1,,]%*%list_of_draws$betas[j,] + (list_of_draws$beta_random_walk[j,1]) + list_of_draws$theta[j,]
      y_approx_IMD_best[1,,j] <- as.numeric(log(counterfactual_susceptible_proxy_best[,1]*(counterfactual_E_best[1,] + (list_of_draws$zetas[j,] *counterfactual_E_neighbours_best[,1])))) + x_IMD_best_counterfactual[1,,]%*%list_of_draws$betas[j,] + (list_of_draws$beta_random_walk[j,1]) + list_of_draws$theta[j,]
      
    }
    
    #Now for the rest, we will be updating the arrays in each timestep with the y values from the previous step
    
    for(i in 2:T){
      
      if(scale_by_susceptible_pool){
        #Susceptible proxy is a 306x95
        counterfactual_E_worst[i,] <- exp(y_approx_IMD_worst[(i-1),,j])
        counterfactual_first_episodes_total_worst[,i] <- counterfactual_first_episodes_total_worst[,(i-1)] +  counterfactual_E_worst[i,]*min(1,(Best_Factor*(1-(counterfactual_first_episodes_total_worst[,(i-1)]/Population_Data$Population))))
        counterfactual_susceptible_proxy_worst[,i] <- 1-(counterfactual_first_episodes_total_worst[,i]/Population_Data$Population)
        
        counterfactual_E_best[i,] <- exp(y_approx_IMD_best[(i-1),,j])
        counterfactual_first_episodes_total_best[,i] <- counterfactual_first_episodes_total_best[,(i-1)] +  counterfactual_E_best[i,]*min(1,(Best_Factor*(1-(counterfactual_first_episodes_total_best[,(i-1)]/Population_Data$Population))))
        counterfactual_susceptible_proxy_best[,i] <- 1-(counterfactual_first_episodes_total_best[,i]/Population_Data$Population)
        
        counterfactual_E_neighbours_worst <- W_reduced%*%t(counterfactual_E_worst)
        counterfactual_E_neighbours_best <- W_reduced%*%t(counterfactual_E_best)
        
        y_approx_IMD_worst[i,,j] <- as.numeric(log((model_susc_scale*counterfactual_susceptible_proxy_worst[,i])*(counterfactual_E_worst[i,] + (list_of_draws$zetas[j,] *counterfactual_E_neighbours_worst[,i])))) + x_IMD_worst_counterfactual[i,,]%*%list_of_draws$betas[j,] + (list_of_draws$beta_random_walk[j,i]) + list_of_draws$theta[j,]
        y_approx_IMD_best[i,,j] <- as.numeric(log((model_susc_scale*counterfactual_susceptible_proxy_best[,i])*(counterfactual_E_best[i,] + (list_of_draws$zetas[j,] *counterfactual_E_neighbours_best[,i])))) + x_IMD_best_counterfactual[i,,]%*%list_of_draws$betas[j,] + (list_of_draws$beta_random_walk[j,i]) + list_of_draws$theta[j,]
        
      
        }else{
          
          counterfactual_E_worst[i,] <- y_approx_IMD_worst[(i-1),,j]
          counterfactual_first_episodes_total_worst[,i] <- counterfactual_first_episodes_total_worst[,(i-1)] +  counterfactual_E_worst[i,]*min(1,(Best_Factor*(1-(counterfactual_first_episodes_total_worst[,(i-1)]/Population_Data$Population))))
          counterfactual_E_best[i,] <- y_approx_IMD_best[(i-1),,j]
          counterfactual_first_episodes_total_best[,i] <- counterfactual_first_episodes_total_best[,(i-1)] +  counterfactual_E_best[i,]*min(1,(Best_Factor*(1-(counterfactual_first_episodes_total_best[,(i-1)]/Population_Data$Population))))
          counterfactual_E_neighbours_worst <- W_reduced%*%t(counterfactual_E_worst)
          counterfactual_E_neighbours_best <- W_reduced%*%t(counterfactual_E_best)
          
          
        y_approx_IMD_worst[i,,j] <- as.numeric(log(counterfactual_susceptible_proxy_worst[,i]*(counterfactual_E_worst[i,] + (list_of_draws$zetas[j,] *counterfactual_E_neighbours_worst[,i])))) + x_IMD_worst_counterfactual[i,,]%*%list_of_draws$betas[j,] + (list_of_draws$beta_random_walk[j,i]) + list_of_draws$theta[j,]
        y_approx_IMD_best[i,,j] <- as.numeric(log(counterfactual_susceptible_proxy_best[,i]*(counterfactual_E_best[i,] + (list_of_draws$zetas[j,] *counterfactual_E_neighbours_best[,i])))) + x_IMD_best_counterfactual[i,,]%*%list_of_draws$betas[j,] + (list_of_draws$beta_random_walk[j,i]) + list_of_draws$theta[j,]
        
      }
      
    }
    
  }
  
  print("finished building y arrays")
  
  if(include_Poisson){
    mean_y_IMD_worst <- array(dim=c(95,306))
    median_y_IMD_worst <- array(dim=c(95,306))
    CI_y_IMD_worst <- array(dim=c(2,95,306))
    
    mean_y_IMD_best <- array(dim=c(95,306))
    median_y_IMD_best <- array(dim=c(95,306))
    CI_y_IMD_best <- array(dim=c(2,95,306))
    
    for(i in 1:95){
      for(j in 1:306){
        print(i)
        hold_values <- rep(NA, n_draws*1000)
        for(k in 1:n_draws){
          #first worst
          hold_values[(((k-1)*1000)+1):(((k-1)*1000)+1000)] <- rpois(1000, exp(y_approx_IMD_worst[i,j,k]))
        }
        mean_y_IMD_worst[i,j] <- mean(hold_values)
        median_y_IMD_worst[i,j] <- median(hold_values)
        CI_y_IMD_worst[,i,j] <- quantile(hold_values, c(0.025, 0.975))
        #################now best
        hold_values <- rep(NA, n_draws*1000)
        for(k in 1:n_draws){
          hold_values[(((k-1)*1000)+1):(((k-1)*1000)+1000)] <- rpois(1000, exp(y_approx_IMD_best[i,j,k]))
        }
        mean_y_IMD_best[i,j] <- mean(hold_values)
        median_y_IMD_best[i,j] <- median(hold_values)
        CI_y_IMD_best[,i,j] <- quantile(hold_values, c(0.025, 0.975))
      }
    }
    
    rm(hold_values)
  } else{
    
    #and lastly, we want to then collapse this down to the mean, median, and 95% CIs
    
    # Calculate the mean/median along the 3rd dimension
    mean_y_IMD_worst <- apply(y_approx_IMD_worst, c(1, 2), mean)
    median_y_IMD_worst <- apply(y_approx_IMD_worst, c(1, 2), median)
    
    # Calculate 95% credible interval
    CI_y_IMD_worst <- apply(y_approx_IMD_worst, c(1, 2), function(x) quantile(x, c(0.025, 0.975)))
    
    #I may as well take the exponential here
    mean_y_IMD_worst <- exp(mean_y_IMD_worst)
    median_y_IMD_worst <- exp(median_y_IMD_worst)
    CI_y_IMD_worst <- exp(CI_y_IMD_worst)
    
    
    #AND BEST
    
    mean_y_IMD_best <- apply(y_approx_IMD_best, c(1, 2), mean)
    median_y_IMD_best <- apply(y_approx_IMD_best, c(1, 2), median)
    
    # Calculate 95% credible interval
    CI_y_IMD_best <- apply(y_approx_IMD_best, c(1, 2), function(x) quantile(x, c(0.025, 0.975)))
    
    #I may as well take the exponential here
    mean_y_IMD_best <- exp(mean_y_IMD_best)
    median_y_IMD_best <- exp(median_y_IMD_best)
    CI_y_IMD_best <- exp(CI_y_IMD_best)
    
    #We can remove the y_approx arrays now to save memory
    rm(y_approx_IMD_best, y_approx_IMD_worst)
    
  }
  #Next, we need a dataframe that has the LTLA, date, week_cases, and then the mean, median and lower upper in it as well
  
  LTLAs_by_Index <- unique(Case_Rates_Data[,c(1,3,4,5)])
  LTLAs_by_Index <- LTLAs_by_Index[order(LTLAs_by_Index$INDEX),]
  #Major error spotted here! The cases are in the wrong place! The dates aren't the right order!
  #Remember, our main default is using the P2 Case linelist data. y = Linelist_P2_PCR_Next_Week_Cases
  #Only the pillar 2, and PCR_only cases from the direct England linelist
  
  AllDates <- sort(unique(Case_Rates_Data$date_begin))
  #Also remember, because the y data here are technically speaking the "next week" values, that
  #we want to use +7 to the dates
  AllDates <- AllDates + 7
  
  Model_fit_data_best <- data.frame(areaCode = rep(NA, 95*306),
                               areaName = rep(NA, 95*306),
                               date = as.Date(rep("2020-07-07", 95*306)),
                               Population = rep(NA, 95*306),
                               Real_Cases = rep(NA, 95*306),
                               Model_mean = rep(NA, 95*306),
                               Model_median = rep(NA, 95*306),
                               Model_lower = rep(NA, 95*306),
                               Model_upper = rep(NA, 95*306),
                               Model_scenario = rep("best", 95*306))
  
  Model_fit_data_worst <- data.frame(areaCode = rep(NA, 95*306),
                                    areaName = rep(NA, 95*306),
                                    date = as.Date(rep("2020-07-07", 95*306)),
                                    Population = rep(NA, 95*306),
                                    Real_Cases = rep(NA, 95*306),
                                    Model_mean = rep(NA, 95*306),
                                    Model_median = rep(NA, 95*306),
                                    Model_lower = rep(NA, 95*306),
                                    Model_upper = rep(NA, 95*306),
                                    Model_scenario = rep("worst", 95*306))
  
  print("Filling data frames")
  
  for(i in 1:95){
    for( j in 1:306){
      data_index <- ((i-1)*306) + j
      Model_fit_data_best$areaCode[data_index] <- LTLAs_by_Index$areaCode[j]
      Model_fit_data_best$areaName[data_index] <- LTLAs_by_Index$areaName[j]
      Model_fit_data_best$date[data_index] <- as.Date(AllDates[i])
      Model_fit_data_best$Population[data_index] <- LTLAs_by_Index$Population[j]
      Model_fit_data_best$Real_Cases[data_index] <- y[i,j]
      Model_fit_data_best$Model_mean[data_index] <- mean_y_IMD_best[i,j]
      Model_fit_data_best$Model_median[data_index] <- median_y_IMD_best[i,j]
      Model_fit_data_best$Model_lower[data_index] <- CI_y_IMD_best[1,i,j]
      Model_fit_data_best$Model_upper[data_index] <- CI_y_IMD_best[2,i,j]
    }
  }
  
  for(i in 1:95){
    for( j in 1:306){
      data_index <- ((i-1)*306) + j
      Model_fit_data_worst$areaCode[data_index] <- LTLAs_by_Index$areaCode[j]
      Model_fit_data_worst$areaName[data_index] <- LTLAs_by_Index$areaName[j]
      Model_fit_data_worst$date[data_index] <- as.Date(AllDates[i])
      Model_fit_data_worst$Population[data_index] <- LTLAs_by_Index$Population[j]
      Model_fit_data_worst$Real_Cases[data_index] <- y[i,j]
      Model_fit_data_worst$Model_mean[data_index] <- mean_y_IMD_worst[i,j]
      Model_fit_data_worst$Model_median[data_index] <- median_y_IMD_worst[i,j]
      Model_fit_data_worst$Model_lower[data_index] <- CI_y_IMD_worst[1,i,j]
      Model_fit_data_worst$Model_upper[data_index] <- CI_y_IMD_worst[2,i,j]
    }
  }
  
  #We also want the respective england totals now.
  england_daily_total_best <- Model_fit_data_best %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(Real_Cases = sum(Real_Cases),
                     Model_mean = sum(Model_mean),
                     Model_median = sum(Model_median),
                     Model_lower = sum(Model_lower),
                     Model_upper = sum(Model_upper))
  
  england_daily_total_worst <- Model_fit_data_worst %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(Real_Cases = sum(Real_Cases),
                     Model_mean = sum(Model_mean),
                     Model_median = sum(Model_median),
                     Model_lower = sum(Model_lower),
                     Model_upper = sum(Model_upper))
  
}

#Save these outputs for pre-loading on future runs
save(england_daily_total_best, england_daily_total_worst,
     Model_fit_data_worst, Model_fit_data_best, file = "IMD_counterfactual_data.RData")


#Now we stick it together with the existing datasets.

england_daily_total_best$scenario <- "Best"
england_daily_total_worst$scenario <- "Worst"
england_daily_total$scenario <- "Model Fit"

england_daily_total_IMDs <- rbind(england_daily_total, england_daily_total_best, england_daily_total_worst)
#"#FF7F50" - bad counterfactual, "#9370DB" - good counterfactual

ggplot() +
  geom_line(data = england_daily_total_IMDs, aes(x = date, y = Model_mean / 1000,
                                            color = scenario), size = 1) +
  geom_rect(data = lockdown_shades, aes(xmin = as.Date(date_start), xmax = as.Date(date_end),
                                        ymin = 1500, ymax = 1520),
            fill = c('#FFB6B6', "#FFD6A5", '#FFB6B6', "#FFD6A5", '#FFB6B6', "#FFD6A5", "#C8E7C1"),
            show.legend = FALSE) +
  geom_ribbon(data = england_daily_total_IMDs,
              aes(x = date, ymin = Model_lower / 1000, ymax = Model_upper / 1000,
                  fill = scenario), alpha = 0.3, show.legend = FALSE) +
  theme_classic() + ylab('Weekly New Cases (thousands)') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y") +
  coord_cartesian(xlim = c(as.Date('2020-05-01'), as.Date('2022-04-01'))) +
  
  
  #geom_vline(xintercept = as.Date(variant_dates), alpha = 0.9, color = 'black') +
  geom_vline(xintercept = as.Date(grey_lines), alpha = 0.7, color = 'gray23', lty = 'dashed') +
  xlab("Date") +
  #annotate('text', x = as.Date("2020-07-26") - 13, label = "Alpha emergence", y = 500, colour = "black", size = 4, angle = 90) +
  #annotate('text', x = as.Date("2021-03-28") - 13, label = "Delta emergence", y = 500, colour = "black", size = 4, angle = 90) +
  #annotate('text', x = as.Date("2021-09-12") - 13, label = "Omicron emergence", y = 500, colour = "black", size = 4, angle = 90) +
  
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.2)),
        legend.title = element_text(size = rel(1.3))) +
  ggtitle("Weekly total Pillar 2 and PCR cases reported in England - IMD Counterfactuals ") +
  #geom_point(data = england_daily_total, aes(x = date, y = Real_Cases / 1000,
  #                                           color = "Data"),
  #           alpha = 0.7, shape = 18) +
  
  scale_color_manual(
    breaks = c("Model Fit", "Worst", "Best"),
    labels = c("Model Fit", "High IMD", "Low IMD"),
    values = c("#9BC362", "#FF7F50", "#9370DB")
    #values = c("Data" = "black", "Model Fit" = "#9BC362")
                     ) +
  scale_fill_manual(
    breaks = c("Model Fit", "Worst", "Best"),
    labels = c("Model Fit", "High IMD", "Low IMD"),
    values = c("#9BC362", "#FF7F50", "#9370DB")
    #values = c("Data" = "black", "Model Fit" = "#9BC362")
  ) +
  # Modify legend labels
  #guides(color = guide_legend(override.aes = list(shape = c(18, NA), linetype = c(NA, 1)))) +
  labs(color = "Scenario") -> IMD_plot

ggsave(filename = "f4_IMD.png",
       path = 'Case_Outputs\\fig_demos', plot = IMD_plot,
       dpi=300, height=10, width=9, units="in")

ggsave(filename = "f4_IMD.tiff",
       path = 'Case_Outputs\\fig_demos', plot = IMD_plot,
       dpi=300, height=10, width=9, units="in")

ggsave(filename = "f4_IMD.pdf",
       path = 'Case_Outputs\\fig_demos', plot = IMD_plot,
       dpi=300, height=10, width=9, units="in")

#Let's also output the totals for reporting:
IMD_counterfactual_totals <- data.frame(scenario = c("Model Fit", "High IMD", "Low IMD"),
                                        mean_total = c(sum(england_daily_total$Model_mean),
                                                      sum(england_daily_total_worst$Model_mean),
                                                      sum(england_daily_total_best$Model_mean)),
                                        lower_total = c(sum(england_daily_total$Model_lower),
                                                        sum(england_daily_total_worst$Model_lower),
                                                        sum(england_daily_total_best$Model_lower)),
                                        upper_total = c(sum(england_daily_total$Model_upper),
                                                        sum(england_daily_total_worst$Model_upper),
                                                        sum(england_daily_total_best$Model_upper))
                                        )
write.csv(IMD_counterfactual_totals, "IMD_counterfactual_totals.csv", row.names=FALSE)

#Let's also output the LTLA specific spots while we're at it.
dir.create("Case_Outputs\\LTLA_IMD_counterfactuals")
Model_fit_data$Model_scenario <- "Model Fit"
Model_fit_total_IMDs <- rbind(Model_fit_data, Model_fit_data_best, Model_fit_data_worst)


LTLA_names <- unique(Model_fit_data$areaName)
for(i in 1:length(LTLA_names)){
  
  areaName_hold <- LTLA_names[i]
  
  ggplot() +
    geom_line(data = filter(Model_fit_total_IMDs, areaName == areaName_hold), aes(x = date, y = Model_mean / Population,
                                                   color = Model_scenario), size = 1) +
    # geom_rect(data = lockdown_shades, aes(xmin = as.Date(date_start), xmax = as.Date(date_end),
    #                                       ymin = 0.7, ymax = 0.75),
    #           fill = c('#FFB6B6', "#FFD6A5", '#FFB6B6', "#FFD6A5", '#FFB6B6', "#FFD6A5", "#C8E7C1"),
    #           show.legend = FALSE) +
    geom_ribbon(data = filter(Model_fit_total_IMDs, areaName == areaName_hold),
                aes(x = date, ymin = Model_lower / Population, ymax = Model_upper / Population,
                    fill = Model_scenario), alpha = 0.3, show.legend = FALSE) +
    theme_classic() + ylab('Weekly Case Rates') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_date(date_breaks = "2 month", date_labels = "%b %y") +
    coord_cartesian(xlim = c(as.Date('2020-05-01'), as.Date('2022-04-01'))) +
    
    
    #geom_vline(xintercept = as.Date(variant_dates), alpha = 0.9, color = 'black') +
    geom_vline(xintercept = as.Date(grey_lines), alpha = 0.7, color = 'gray23', lty = 'dashed') +
    xlab("Date") +
    #annotate('text', x = as.Date("2020-07-26") - 13, label = "Alpha emergence", y = 500, colour = "black", size = 4, angle = 90) +
    #annotate('text', x = as.Date("2021-03-28") - 13, label = "Delta emergence", y = 500, colour = "black", size = 4, angle = 90) +
    #annotate('text', x = as.Date("2021-09-12") - 13, label = "Omicron emergence", y = 500, colour = "black", size = 4, angle = 90) +
    
    theme(axis.text = element_text(size = rel(1.2)),
          axis.title = element_text(size = rel(1.3)),
          legend.text = element_text(size = rel(1.2)),
          legend.title = element_text(size = rel(1.3))) +
    ggtitle(sprintf("Weekly Case Rates in %s - IMD Counterfactuals ",areaName_hold)) +
    #geom_point(data = england_daily_total, aes(x = date, y = Real_Cases / 1000,
    #                                           color = "Data"),
    #           alpha = 0.7, shape = 18) +
    
    scale_color_manual(
      breaks = c("Model Fit", "worst", "best"),
      labels = c("Model Fit", "High IMD", "Low IMD"),
      values = c("#9BC362", "#FF7F50", "#9370DB")
      #values = c("Data" = "black", "Model Fit" = "#9BC362")
    ) +
    scale_fill_manual(
      breaks = c("Model Fit", "worst", "best"),
      labels = c("Model Fit", "High IMD", "Low IMD"),
      values = c("#9BC362", "#FF7F50", "#9370DB")
      #values = c("Data" = "black", "Model Fit" = "#9BC362")
    ) +
    # Modify legend labels
    #guides(color = guide_legend(override.aes = list(shape = c(18, NA), linetype = c(NA, 1)))) +
    labs(color = "Scenario") -> plot_hold
  
  png(file=sprintf("Case_Outputs\\LTLA_IMD_counterfactuals\\model_fit_%s_%s.png", i, areaName_hold),
      width=1440, height=1080, res = 150)
  plot(plot_hold)
  dev.off()
  
  
  
}

rm(betas_1_10, betas_11_13, betas_14_16,
   england_daily_total_IMDs, england_IMD, england_prop_white,
   Fig1_plot, fig2_data, Fig2_plot, Fig3_plot,
   IMD_counterfactual_totals, london_IMD,
   LTLA_A_data, LTLA_A_plot,
   LTLA_B_data, LTLA_B_plot,
   mean_y_IMD_best, mean_y_IMD_worst, median_y_IMD_best, median_y_IMD_worst,
   plot_hold)


################################
# Now for the outbreak management fund
#Different funding amounts were given out in each financial year, 
#so we're going to treat each year separately.
Case_Rates_Data %>%
  filter(Week == 10) %>% #Any week will do
  select(areaCode, areaName, INDEX, Population, contain_outbreak_management) -> fund_data_2021
Case_Rates_Data %>%
  filter(Week == 90) %>% #Any week will do
  select(areaCode, areaName, INDEX, Population, contain_outbreak_management) -> fund_data_2022

fund_data_2021$fund_by_pop <- fund_data_2021$contain_outbreak_management/fund_data_2021$Population
fund_data_2022$fund_by_pop <- fund_data_2022$contain_outbreak_management/fund_data_2022$Population

#So who has the most from the fund?  
best_fund2021_name <- fund_data_2021$areaName[which(fund_data_2021$fund_by_pop == max(fund_data_2021$fund_by_pop))]
best_fund2021_value <- fund_data_2021$fund_by_pop[which(fund_data_2021$fund_by_pop == max(fund_data_2021$fund_by_pop))]
best_fund2022_name <- fund_data_2022$areaName[which(fund_data_2022$fund_by_pop == max(fund_data_2022$fund_by_pop))]
best_fund2022_value <- fund_data_2022$fund_by_pop[which(fund_data_2022$fund_by_pop == max(fund_data_2022$fund_by_pop))]

worst_fund2021_name <- fund_data_2021$areaName[which(fund_data_2021$fund_by_pop == min(fund_data_2021$fund_by_pop))]
worst_fund2021_value <- fund_data_2021$fund_by_pop[which(fund_data_2021$fund_by_pop == min(fund_data_2021$fund_by_pop))]
worst_fund2022_name <- fund_data_2022$areaName[which(fund_data_2022$fund_by_pop == min(fund_data_2022$fund_by_pop))]
worst_fund2022_value <- fund_data_2022$fund_by_pop[which(fund_data_2022$fund_by_pop == min(fund_data_2022$fund_by_pop))]

#Let's change our x matrix then

x_fund_worst_counterfactual <- x
for(i in 1:T){
  x_fund_worst_counterfactual[i,,15] <- min(x[i,,15])
}

x_fund_best_counterfactual <- x
for(i in 1:T){
  x_fund_best_counterfactual[i,,15] <- max(x[i,,15])
}

x_fund_none_counterfactual <- x
for(i in 1:T){
  x_fund_none_counterfactual[i,,15] <- (0 - mean(Case_Rates_Data$contain_outbreak_management/Case_Rates_Data$Population))/sd(Case_Rates_Data$contain_outbreak_management/Case_Rates_Data$Population)
}

#To conserve memory we'll do each scenario separately
#Now we calculate the new y_approx values
#If TRUE then we don't run all the long code, we just get the final data frames at the end
Preload_data <- FALSE
#Do we want to then factor in the Poisson uncertainty?
include_Poisson <- FALSE

if(Preload_data){
  #load("CI_data_outputs.RData")
} else{
  
  
  
  
  list_of_draws <- extract(stanfit)
  print(names(list_of_draws))
  n_draws <- length(list_of_draws$sqrtQ)
  #We have 32000 draws, that's because we had 2000 iterations (after a 2000 warm-up) and 16 chains. 16*2000
  
  #We now want to have our 32,000 associated draws for y_approx
  y_approx_fund_worst <- array(data = NA, dim = c((final_week-1),306,n_draws))
  
  for(j in 1:n_draws){
    
    if(scale_by_susceptible_pool){
      model_susc_scale <- list_of_draws$susc_scaling[j]
    }
    
    for(i in 1:T){
      if(scale_by_susceptible_pool){
        y_approx_fund_worst[i,,j] <- as.numeric(log((model_susc_scale*susceptible_proxy[,i])*(E[i,] + (list_of_draws$zetas[j,] *E_neighbours[,i])))) + x_fund_worst_counterfactual[i,,]%*%list_of_draws$betas[j,] + (list_of_draws$beta_random_walk[j,i]) + list_of_draws$theta[j,]
       
      }else{
        y_approx_fund_worst[i,,j] <- as.numeric(log(susceptible_proxy[,i]*(E[i,] + (list_of_draws$zetas[j,] *E_neighbours[,i])))) + x_fund_worst_counterfactual[i,,]%*%list_of_draws$betas[j,] + (list_of_draws$beta_random_walk[j,i]) + list_of_draws$theta[j,]
        
      }
    }
    
  }
  
  print("finished building y arrays")
  
  if(include_Poisson){
    mean_y_fund_worst <- array(dim=c(95,306))
    median_y_fund_worst <- array(dim=c(95,306))
    CI_y_fund_worst <- array(dim=c(2,95,306))
    for(i in 1:95){
      for(j in 1:306){
        print(i)
        hold_values <- rep(NA, n_draws*1000)
        for(k in 1:n_draws){
          #first worst
          hold_values[(((k-1)*1000)+1):(((k-1)*1000)+1000)] <- rpois(1000, exp(y_approx_fund_worst[i,j,k]))
        }
        mean_y_fund_worst[i,j] <- mean(hold_values)
        median_y_fund_worst[i,j] <- median(hold_values)
        CI_y_fund_worst[,i,j] <- quantile(hold_values, c(0.025, 0.975))
      }
    }
    rm(hold_values)
  } else{
    
    #and lastly, we want to then collapse this down to the mean, median, and 95% CIs
    
    # Calculate the mean/median along the 3rd dimension
    mean_y_fund_worst <- apply(y_approx_fund_worst, c(1, 2), mean)
    median_y_fund_worst <- apply(y_approx_fund_worst, c(1, 2), median)
    
    # Calculate 95% credible interval
    CI_y_fund_worst <- apply(y_approx_fund_worst, c(1, 2), function(x) quantile(x, c(0.025, 0.975)))
    
    #I may as well take the exponential here
    mean_y_fund_worst <- exp(mean_y_fund_worst)
    median_y_fund_worst <- exp(median_y_fund_worst)
    CI_y_fund_worst <- exp(CI_y_fund_worst)
    
    #We can remove the y_approx arrays now to save memory
    rm(y_approx_fund_worst)
    
  }
  #Next, we need a dataframe that has the LTLA, date, week_cases, and then the mean, median and lower upper in it as well
  
  LTLAs_by_Index <- unique(Case_Rates_Data[,c(1,3,4,5)])
  LTLAs_by_Index <- LTLAs_by_Index[order(LTLAs_by_Index$INDEX),]
  #Major error spotted here! The cases are in the wrong place! The dates aren't the right order!
  #Remember, our main default is using the P2 Case linelist data. y = Linelist_P2_PCR_Next_Week_Cases
  #Only the pillar 2, and PCR_only cases from the direct England linelist
  
  AllDates <- sort(unique(Case_Rates_Data$date_begin))
  #Also remember, because the y data here are technically speaking the "next week" values, that
  #we want to use +7 to the dates
  AllDates <- AllDates + 7

  Model_fit_data_worst <- data.frame(areaCode = rep(NA, 95*306),
                                     areaName = rep(NA, 95*306),
                                     date = as.Date(rep("2020-07-07", 95*306)),
                                     Population = rep(NA, 95*306),
                                     Real_Cases = rep(NA, 95*306),
                                     Model_mean = rep(NA, 95*306),
                                     Model_median = rep(NA, 95*306),
                                     Model_lower = rep(NA, 95*306),
                                     Model_upper = rep(NA, 95*306),
                                     Model_scenario = rep("worst", 95*306))
  
  print("Filling data frames")
  
  for(i in 1:95){
    for( j in 1:306){
      data_index <- ((i-1)*306) + j
      Model_fit_data_worst$areaCode[data_index] <- LTLAs_by_Index$areaCode[j]
      Model_fit_data_worst$areaName[data_index] <- LTLAs_by_Index$areaName[j]
      Model_fit_data_worst$date[data_index] <- as.Date(AllDates[i])
      Model_fit_data_worst$Population[data_index] <- LTLAs_by_Index$Population[j]
      Model_fit_data_worst$Real_Cases[data_index] <- y[i,j]
      Model_fit_data_worst$Model_mean[data_index] <- mean_y_fund_worst[i,j]
      Model_fit_data_worst$Model_median[data_index] <- median_y_fund_worst[i,j]
      Model_fit_data_worst$Model_lower[data_index] <- CI_y_fund_worst[1,i,j]
      Model_fit_data_worst$Model_upper[data_index] <- CI_y_fund_worst[2,i,j]
    }
  }
  
  england_daily_total_worst <- Model_fit_data_worst %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(Real_Cases = sum(Real_Cases),
                     Model_mean = sum(Model_mean),
                     Model_median = sum(Model_median),
                     Model_lower = sum(Model_lower),
                     Model_upper = sum(Model_upper))
  
}

##########
if(Preload_data){
  #load("CI_data_outputs.RData")
} else{
  
  
  
  
  list_of_draws <- extract(stanfit)
  print(names(list_of_draws))
  n_draws <- length(list_of_draws$sqrtQ)
  #We have 32000 draws, that's because we had 2000 iterations (after a 2000 warm-up) and 16 chains. 16*2000
  
  #We now want to have our 32,000 associated draws for y_approx
  y_approx_fund_best <- array(data = NA, dim = c((final_week-1),306,n_draws))
  
  for(j in 1:n_draws){
    
    if(scale_by_susceptible_pool){
      model_susc_scale <- list_of_draws$susc_scaling[j]
    }
    
    for(i in 1:T){
      if(scale_by_susceptible_pool){
        y_approx_fund_best[i,,j] <- as.numeric(log((model_susc_scale*susceptible_proxy[,i])*(E[i,] + (list_of_draws$zetas[j,] *E_neighbours[,i])))) + x_fund_best_counterfactual[i,,]%*%list_of_draws$betas[j,] + (list_of_draws$beta_random_walk[j,i]) + list_of_draws$theta[j,]
        
      }else{
        y_approx_fund_best[i,,j] <- as.numeric(log(susceptible_proxy[,i]*(E[i,] + (list_of_draws$zetas[j,] *E_neighbours[,i])))) + x_fund_best_counterfactual[i,,]%*%list_of_draws$betas[j,] + (list_of_draws$beta_random_walk[j,i]) + list_of_draws$theta[j,]
        
      }
    }
    
  }
  
  print("finished building y arrays")
  
  if(include_Poisson){
    mean_y_fund_best <- array(dim=c(95,306))
    median_y_fund_best <- array(dim=c(95,306))
    CI_y_fund_best <- array(dim=c(2,95,306))
    
    for(i in 1:95){
      for(j in 1:306){
        print(i)
        hold_values <- rep(NA, n_draws*1000)
        for(k in 1:n_draws){
          hold_values[(((k-1)*1000)+1):(((k-1)*1000)+1000)] <- rpois(1000, exp(y_approx_fund_best[i,j,k]))
        }
        mean_y_fund_best[i,j] <- mean(hold_values)
        median_y_fund_best[i,j] <- median(hold_values)
        CI_y_fund_best[,i,j] <- quantile(hold_values, c(0.025, 0.975))
      }
    }
    rm(hold_values)
  } else{
    
    #and lastly, we want to then collapse this down to the mean, median, and 95% CIs

    
    mean_y_fund_best <- apply(y_approx_fund_best, c(1, 2), mean)
    median_y_fund_best <- apply(y_approx_fund_best, c(1, 2), median)
    
    # Calculate 95% credible interval
    CI_y_fund_best <- apply(y_approx_fund_best, c(1, 2), function(x) quantile(x, c(0.025, 0.975)))
    
    #I may as well take the exponential here
    mean_y_fund_best <- exp(mean_y_fund_best)
    median_y_fund_best <- exp(median_y_fund_best)
    CI_y_fund_best <- exp(CI_y_fund_best)
    
    #We can remove the y_approx arrays now to save memory
    rm(y_approx_fund_best)
    
  }
  #Next, we need a dataframe that has the LTLA, date, week_cases, and then the mean, median and lower upper in it as well
  
  LTLAs_by_Index <- unique(Case_Rates_Data[,c(1,3,4,5)])
  LTLAs_by_Index <- LTLAs_by_Index[order(LTLAs_by_Index$INDEX),]
  #Major error spotted here! The cases are in the wrong place! The dates aren't the right order!
  #Remember, our main default is using the P2 Case linelist data. y = Linelist_P2_PCR_Next_Week_Cases
  #Only the pillar 2, and PCR_only cases from the direct England linelist
  
  AllDates <- sort(unique(Case_Rates_Data$date_begin))
  #Also remember, because the y data here are technically speaking the "next week" values, that
  #we want to use +7 to the dates
  AllDates <- AllDates + 7
  
  Model_fit_data_best <- data.frame(areaCode = rep(NA, 95*306),
                                    areaName = rep(NA, 95*306),
                                    date = as.Date(rep("2020-07-07", 95*306)),
                                    Population = rep(NA, 95*306),
                                    Real_Cases = rep(NA, 95*306),
                                    Model_mean = rep(NA, 95*306),
                                    Model_median = rep(NA, 95*306),
                                    Model_lower = rep(NA, 95*306),
                                    Model_upper = rep(NA, 95*306),
                                    Model_scenario = rep("best", 95*306))
  
  print("Filling data frames")
  
  for(i in 1:95){
    for( j in 1:306){
      data_index <- ((i-1)*306) + j
      Model_fit_data_best$areaCode[data_index] <- LTLAs_by_Index$areaCode[j]
      Model_fit_data_best$areaName[data_index] <- LTLAs_by_Index$areaName[j]
      Model_fit_data_best$date[data_index] <- as.Date(AllDates[i])
      Model_fit_data_best$Population[data_index] <- LTLAs_by_Index$Population[j]
      Model_fit_data_best$Real_Cases[data_index] <- y[i,j]
      Model_fit_data_best$Model_mean[data_index] <- mean_y_fund_best[i,j]
      Model_fit_data_best$Model_median[data_index] <- median_y_fund_best[i,j]
      Model_fit_data_best$Model_lower[data_index] <- CI_y_fund_best[1,i,j]
      Model_fit_data_best$Model_upper[data_index] <- CI_y_fund_best[2,i,j]
    }
  }
  
  #We also want the respective england totals now.
  england_daily_total_best <- Model_fit_data_best %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(Real_Cases = sum(Real_Cases),
                     Model_mean = sum(Model_mean),
                     Model_median = sum(Model_median),
                     Model_lower = sum(Model_lower),
                     Model_upper = sum(Model_upper))
  
}
#########
#Actually, for funding we also want to consider the "No funding" scenario:

##########
if(Preload_data){
  #load("CI_data_outputs.RData")
} else{
  
  
  
  
  list_of_draws <- extract(stanfit)
  print(names(list_of_draws))
  n_draws <- length(list_of_draws$sqrtQ)
  #We have 32000 draws, that's because we had 2000 iterations (after a 2000 warm-up) and 16 chains. 16*2000
  
  #We now want to have our 32,000 associated draws for y_approx
  y_approx_fund_none <- array(data = NA, dim = c((final_week-1),306,n_draws))
  
  for(j in 1:n_draws){
    
    if(scale_by_susceptible_pool){
      model_susc_scale <- list_of_draws$susc_scaling[j]
    }
    
    for(i in 1:T){
      if(scale_by_susceptible_pool){
        y_approx_fund_none[i,,j] <- as.numeric(log((model_susc_scale*susceptible_proxy[,i])*(E[i,] + (list_of_draws$zetas[j,] *E_neighbours[,i])))) + x_fund_none_counterfactual[i,,]%*%list_of_draws$betas[j,] + (list_of_draws$beta_random_walk[j,i]) + list_of_draws$theta[j,]
        
      }else{
        y_approx_fund_none[i,,j] <- as.numeric(log(susceptible_proxy[,i]*(E[i,] + (list_of_draws$zetas[j,] *E_neighbours[,i])))) + x_fund_none_counterfactual[i,,]%*%list_of_draws$betas[j,] + (list_of_draws$beta_random_walk[j,i]) + list_of_draws$theta[j,]
        
      }
    }
    
  }
  
  print("finished building y arrays")
  
  if(include_Poisson){
    mean_y_fund_none <- array(dim=c(95,306))
    median_y_fund_none <- array(dim=c(95,306))
    CI_y_fund_none <- array(dim=c(2,95,306))
    
    for(i in 1:95){
      for(j in 1:306){
        print(i)
        hold_values <- rep(NA, n_draws*1000)
        for(k in 1:n_draws){
          hold_values[(((k-1)*1000)+1):(((k-1)*1000)+1000)] <- rpois(1000, exp(y_approx_fund_none[i,j,k]))
        }
        mean_y_fund_none[i,j] <- mean(hold_values)
        median_y_fund_none[i,j] <- median(hold_values)
        CI_y_fund_none[,i,j] <- quantile(hold_values, c(0.025, 0.975))
      }
    }
    rm(hold_values)
  } else{
    
    #and lastly, we want to then collapse this down to the mean, median, and 95% CIs
    
    
    mean_y_fund_none <- apply(y_approx_fund_none, c(1, 2), mean)
    median_y_fund_none <- apply(y_approx_fund_none, c(1, 2), median)
    
    # Calculate 95% credible interval
    CI_y_fund_none<- apply(y_approx_fund_none, c(1, 2), function(x) quantile(x, c(0.025, 0.975)))
    
    #I may as well take the exponential here
    mean_y_fund_none <- exp(mean_y_fund_none)
    median_y_fund_none <- exp(median_y_fund_none)
    CI_y_fund_none <- exp(CI_y_fund_none)
    
    #We can remove the y_approx arrays now to save memory
    rm(y_approx_fund_none)
    
  }
  #Next, we need a dataframe that has the LTLA, date, week_cases, and then the mean, median and lower upper in it as well
  
  LTLAs_by_Index <- unique(Case_Rates_Data[,c(1,3,4,5)])
  LTLAs_by_Index <- LTLAs_by_Index[order(LTLAs_by_Index$INDEX),]
  #Major error spotted here! The cases are in the wrong place! The dates aren't the right order!
  #Remember, our main default is using the P2 Case linelist data. y = Linelist_P2_PCR_Next_Week_Cases
  #Only the pillar 2, and PCR_only cases from the direct England linelist
  
  AllDates <- sort(unique(Case_Rates_Data$date_begin))
  #Also remember, because the y data here are technically speaking the "next week" values, that
  #we want to use +7 to the dates
  AllDates <- AllDates + 7
  
  Model_fit_data_none <- data.frame(areaCode = rep(NA, 95*306),
                                    areaName = rep(NA, 95*306),
                                    date = as.Date(rep("2020-07-07", 95*306)),
                                    Population = rep(NA, 95*306),
                                    Real_Cases = rep(NA, 95*306),
                                    Model_mean = rep(NA, 95*306),
                                    Model_median = rep(NA, 95*306),
                                    Model_lower = rep(NA, 95*306),
                                    Model_upper = rep(NA, 95*306),
                                    Model_scenario = rep("none", 95*306))
  
  print("Filling data frames")
  
  for(i in 1:95){
    for( j in 1:306){
      data_index <- ((i-1)*306) + j
      Model_fit_data_none$areaCode[data_index] <- LTLAs_by_Index$areaCode[j]
      Model_fit_data_none$areaName[data_index] <- LTLAs_by_Index$areaName[j]
      Model_fit_data_none$date[data_index] <- as.Date(AllDates[i])
      Model_fit_data_none$Population[data_index] <- LTLAs_by_Index$Population[j]
      Model_fit_data_none$Real_Cases[data_index] <- y[i,j]
      Model_fit_data_none$Model_mean[data_index] <- mean_y_fund_none[i,j]
      Model_fit_data_none$Model_median[data_index] <- median_y_fund_none[i,j]
      Model_fit_data_none$Model_lower[data_index] <- CI_y_fund_none[1,i,j]
      Model_fit_data_none$Model_upper[data_index] <- CI_y_fund_none[2,i,j]
    }
  }
  
  #We also want the respective england totals now.
  england_daily_total_none <- Model_fit_data_none %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(Real_Cases = sum(Real_Cases),
                     Model_mean = sum(Model_mean),
                     Model_median = sum(Model_median),
                     Model_lower = sum(Model_lower),
                     Model_upper = sum(Model_upper))
  
}
#########

#Now we stick it together with the existing datasets.
#One thing I've realised is that we need to factor in the "susceptible scaling term".
##TODO: Have a think about doing that

england_daily_total_best$scenario <- "Best"
england_daily_total_worst$scenario <- "Worst"
england_daily_total_none$scenario <- "None"

england_daily_total_funds <- rbind(england_daily_total, england_daily_total_best, england_daily_total_worst, england_daily_total_none)
#"#FF7F50" - bad counterfactual, "#9370DB" - good counterfactual

ggplot() +
  geom_line(data = england_daily_total_funds, aes(x = date, y = Model_mean / 1000,
                                                 color = scenario), size = 1) +
  geom_rect(data = lockdown_shades, aes(xmin = as.Date(date_start), xmax = as.Date(date_end),
                                        ymin = max(england_daily_total_funds$Model_upper)/1000 + 20,
                                        ymax = max(england_daily_total_funds$Model_upper)/1000 + 40),
            fill = c('#FFB6B6', "#FFD6A5", '#FFB6B6', "#FFD6A5", '#FFB6B6', "#FFD6A5", "#C8E7C1"),
            show.legend = FALSE) +
  geom_ribbon(data = england_daily_total_funds,
              aes(x = date, ymin = Model_lower / 1000, ymax = Model_upper / 1000,
                  fill = scenario), alpha = 0.3, show.legend = FALSE) +
  theme_classic() + ylab('Weekly New Cases (thousands)') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y") +
  coord_cartesian(xlim = c(as.Date('2020-05-01'), as.Date('2022-04-01'))) +
  
  
  #geom_vline(xintercept = as.Date(variant_dates), alpha = 0.9, color = 'black') +
  geom_vline(xintercept = as.Date(grey_lines), alpha = 0.7, color = 'gray23', lty = 'dashed') +
  xlab("Date") +
  #annotate('text', x = as.Date("2020-07-26") - 13, label = "Alpha emergence", y = 500, colour = "black", size = 4, angle = 90) +
  #annotate('text', x = as.Date("2021-03-28") - 13, label = "Delta emergence", y = 500, colour = "black", size = 4, angle = 90) +
  #annotate('text', x = as.Date("2021-09-12") - 13, label = "Omicron emergence", y = 500, colour = "black", size = 4, angle = 90) +
  
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.2)),
        legend.title = element_text(size = rel(1.3))) +
  ggtitle("Weekly total Pillar 2 and PCR cases reported in England - Outbreak Management Fund Counterfactuals ") +
  #geom_point(data = england_daily_total, aes(x = date, y = Real_Cases / 1000,
  #                                           color = "Data"),
  #           alpha = 0.7, shape = 18) +
  
  scale_color_manual(
    breaks = c("Model Fit", "Worst", "Best", "None"),
    labels = c("Model Fit", "Less Funding", "Higher Funding", "No Funding"),
    values = c("#9BC362", "#FF7F50", "#9370DB", "#2B2B2B")
    #values = c("Data" = "black", "Model Fit" = "#9BC362")
  ) +
  scale_fill_manual(
    breaks = c("Model Fit", "Worst", "Best", "None"),
    labels = c("Model Fit", "High IMD", "Low IMD", "No Funding"),
    values = c("#9BC362", "#FF7F50", "#9370DB", "#2B2B2B")
    #values = c("Data" = "black", "Model Fit" = "#9BC362")
  ) +
  # Modify legend labels
  #guides(color = guide_legend(override.aes = list(shape = c(18, NA), linetype = c(NA, 1)))) +
  labs(color = "Scenario") -> funding_plot


ggsave(filename = "f4_funding.png",
       path = 'Case_Outputs\\fig_demos', plot = funding_plot,
       dpi=300, height=10, width=9, units="in")

ggsave(filename = "f4_funding.tiff",
       path = 'Case_Outputs\\fig_demos', plot = funding_plot,
       dpi=300, height=10, width=9, units="in")

ggsave(filename = "f4_funding.pdf",
       path = 'Case_Outputs\\fig_demos', plot = funding_plot,
       dpi=300, height=10, width=9, units="in")

#Let's also output the totals for reporting:
funding_counterfactual_totals <- data.frame(scenario = c("Model Fit", "Less Funding", "More Funding", "No Funding" ),
                                        mean_total = c(sum(england_daily_total$Model_mean),
                                                       sum(england_daily_total_worst$Model_mean),
                                                       sum(england_daily_total_best$Model_mean),
                                                       sum(england_daily_total_none$Model_mean)),
                                        lower_total = c(sum(england_daily_total$Model_lower),
                                                        sum(england_daily_total_worst$Model_lower),
                                                        sum(england_daily_total_best$Model_lower),
                                                        sum(england_daily_total_none$Model_lower)),
                                        upper_total = c(sum(england_daily_total$Model_upper),
                                                        sum(england_daily_total_worst$Model_upper),
                                                        sum(england_daily_total_best$Model_upper),
                                                        sum(england_daily_total_none$Model_upper))
)
write.csv(funding_counterfactual_totals, "funding_counterfactual_totals.csv", row.names=FALSE)


################################
#Let's also output the LTLA specific spots while we're at it.
dir.create("Case_Outputs\\LTLA_funding_counterfactuals")
#Model_fit_data$Model_scenario <- "Model Fit"
Model_fit_total_funding <- rbind(Model_fit_data, Model_fit_data_best, Model_fit_data_worst, Model_fit_data_none)


LTLA_names <- unique(Model_fit_data$areaName)
for(i in 1:length(LTLA_names)){
  
  areaName_hold <- LTLA_names[i]
  
  ggplot() +
    geom_line(data = filter(Model_fit_total_funding, areaName == areaName_hold), aes(x = date, y = Model_mean / Population,
                                                                                  color = Model_scenario), size = 1) +
    # geom_rect(data = lockdown_shades, aes(xmin = as.Date(date_start), xmax = as.Date(date_end),
    #                                       ymin = 0.7, ymax = 0.75),
    #           fill = c('#FFB6B6', "#FFD6A5", '#FFB6B6', "#FFD6A5", '#FFB6B6', "#FFD6A5", "#C8E7C1"),
    #           show.legend = FALSE) +
    geom_ribbon(data = filter(Model_fit_total_funding, areaName == areaName_hold),
                aes(x = date, ymin = Model_lower / Population, ymax = Model_upper / Population,
                    fill = Model_scenario), alpha = 0.3, show.legend = FALSE) +
    theme_classic() + ylab('Weekly Case Rates') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_date(date_breaks = "2 month", date_labels = "%b %y") +
    coord_cartesian(xlim = c(as.Date('2020-05-01'), as.Date('2022-04-01'))) +
    
    
    #geom_vline(xintercept = as.Date(variant_dates), alpha = 0.9, color = 'black') +
    geom_vline(xintercept = as.Date(grey_lines), alpha = 0.7, color = 'gray23', lty = 'dashed') +
    xlab("Date") +
    #annotate('text', x = as.Date("2020-07-26") - 13, label = "Alpha emergence", y = 500, colour = "black", size = 4, angle = 90) +
    #annotate('text', x = as.Date("2021-03-28") - 13, label = "Delta emergence", y = 500, colour = "black", size = 4, angle = 90) +
    #annotate('text', x = as.Date("2021-09-12") - 13, label = "Omicron emergence", y = 500, colour = "black", size = 4, angle = 90) +
    
    theme(axis.text = element_text(size = rel(1.2)),
          axis.title = element_text(size = rel(1.3)),
          legend.text = element_text(size = rel(1.2)),
          legend.title = element_text(size = rel(1.3))) +
    ggtitle(sprintf("Weekly Case Rates in %s - Outbreak Management Funding Counterfactuals ",areaName_hold)) +
    #geom_point(data = england_daily_total, aes(x = date, y = Real_Cases / 1000,
    #                                           color = "Data"),
    #           alpha = 0.7, shape = 18) +
    
    scale_color_manual(
      breaks = c("Model Fit", "worst", "best", "none"),
      labels = c("Model Fit", "High IMD", "Low IMD", "No Funding"),
      values = c("#9BC362", "#FF7F50", "#9370DB", "#2B2B2B")
      #values = c("Data" = "black", "Model Fit" = "#9BC362")
    ) +
    scale_fill_manual(
      breaks = c("Model Fit", "worst", "best", "none"),
      labels = c("Model Fit", "High IMD", "Low IMD", "No Funding"),
      values = c("#9BC362", "#FF7F50", "#9370DB", "#2B2B2B")
      #values = c("Data" = "black", "Model Fit" = "#9BC362")
    ) +
    # Modify legend labels
    #guides(color = guide_legend(override.aes = list(shape = c(18, NA), linetype = c(NA, 1)))) +
    labs(color = "Scenario") -> plot_hold
  
  png(file=sprintf("Case_Outputs\\LTLA_funding_counterfactuals\\model_fit_%s_%s.png", i, areaName_hold),
      width=1440, height=1080, res = 150)
  plot(plot_hold)
  dev.off()
  
  
  
}




################################################################
}