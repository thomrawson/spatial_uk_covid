#orderly::orderly_develop_start("22_fig5_model_comp", use_draft = "newer", parameters = list(tree_depth = 15, n_chains = 16))

#This script loads in the stanfit object and outputs some demos of paper figures to play around with

dir.create("Case_Outputs")
dir.create("Case_Outputs\\fits")


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

#---------------------------------------------------------------------------
dir.create("Case_Outputs\\task20_LTLA_fits")
dir.create("Case_Outputs\\task20g_LTLA_fits")
dir.create("Case_Outputs\\task20j_LTLA_fits")
dir.create("Case_Outputs\\task20r_LTLA_fits")

dir.create("Case_Outputs\\task20_LTLA_infection_rates")
dir.create("Case_Outputs\\task20g_LTLA_infection_rates")
dir.create("Case_Outputs\\task20j_LTLA_infection_rates")
dir.create("Case_Outputs\\task20r_LTLA_infection_rates")

load("CI_data_outputs_20.RData")
############################################################################
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
                                        ymin = max(england_daily_total$Model_upper/1000)-30,
                                        ymax = max(england_daily_total$Model_upper/1000)),
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
  labs(color = "")  -> england_plot_20

######################################


png(file="england_fit_20.png",
    width=1440, height=1080, res = 150)
plot(england_plot_20)
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
  
  png(file=sprintf("Case_Outputs\\task20_LTLA_fits\\model_fit_%s_%s.png", i, areaName_hold),
      width=1440, height=1080, res = 150)
  plot(plot_hold)
  dev.off()
}



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
    ggtitle(sprintf("Weekly case rates reported in %s", areaName_hold)) +
    geom_point(data = filter(Model_fit_data, areaName == areaName_hold), aes(x = date, y = Real_Cases/Population), alpha = 0.7, shape = 18,
               color = 'black') -> plot_hold
  
  png(file=sprintf("Case_Outputs\\task20_LTLA_infection_rates\\case_rate_fit_%s_%s.png", i, areaName_hold),
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
                                        ymin = 0.029, ymax = 0.03),
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
  ylim(c(0,0.03)) +
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
        axis.text.x = element_text(size = rel(1.2)),
        axis.title.x = element_text(size = rel(1.3)) ) +
  ggtitle(sprintf("%s ", LTLA_A_name)) +
  geom_point(data = LTLA_A_data, aes(x = date, y = Real_Cases / Population,
                                     color = "Real Data"),
             alpha = 0.7, shape = 18, show.legend = FALSE) +
  scale_color_manual(values = c("Real Data" = "black", "Model Fit" = "#9BC362")) +
  scale_fill_manual(values = c("Model Fit" = "#9BC362")) +  # Adjust fill for the ribbon legend
  # Modify legend labels
  guides(color = guide_legend(override.aes = list(shape = c(18, NA), linetype = c(NA, 1)))) +
  labs(color = "Legend")  -> LTLA_A_plot_20

ggplot() +
  geom_rect(data = lockdown_shades, aes(xmin = as.Date(date_start), xmax = as.Date(date_end),
                                        ymin = 0.029, ymax = 0.03),
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
  ylim(c(0,0.03)) +
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
        axis.text.x = element_text(size = rel(1.2)),
        axis.title.x = element_text(size = rel(1.3)) ) +
  ggtitle(sprintf("%s ", LTLA_B_name)) +
  geom_point(data = LTLA_B_data, aes(x = date, y = Real_Cases / Population,
                                     color = "Real Data"),
             alpha = 0.7, shape = 18, show.legend = FALSE) +
  scale_color_manual(values = c("Real Data" = "black", "Model Fit" = "#9BC362")) +
  scale_fill_manual(values = c("Model Fit" = "#9BC362")) +  # Adjust fill for the ribbon legend
  # Modify legend labels
  guides(color = guide_legend(override.aes = list(shape = c(18, NA), linetype = c(NA, 1)))) +
  labs(color = "Legend")  -> LTLA_B_plot_20


##########################

#Now stick it all together in a cowplot.


LTLAs_plot_20 <- plot_grid(LTLA_A_plot_20, LTLA_B_plot_20, labels = c('A', 'B'), nrow = 2)

#Fig1_plot <- plot_grid(england_plot, LTLAs_plot, nrow = 2, labels = c("A", ""))

#Export the plot
ggsave(filename = "LTLAs_plot_20.png",
       path = 'Case_Outputs\\fits', plot = LTLAs_plot_20,
       dpi=300, height=11, width=11, units="in")

ggsave(filename = "LTLAs_plot_20.tiff",
       path = 'Case_Outputs\\fits', plot = LTLAs_plot_20,
       dpi=300, height=11, width=11, units="in")

ggsave(filename = "LTLAs_plot_20.pdf",
       path = 'Case_Outputs\\fits', plot = LTLAs_plot_20,
       dpi=300, height=11, width=11, units="in")

################################################################################
#NOW 20 G
################################################################################
load("CI_data_outputs_20g.RData")
############################################################################
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
                                        ymin = max(england_daily_total$Model_upper/1000)-30,
                                        ymax = max(england_daily_total$Model_upper/1000)),
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
  labs(color = "")  -> england_plot_20g

######################################


png(file="england_fit_20g.png",
    width=1440, height=1080, res = 150)
plot(england_plot_20g)
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
  
  png(file=sprintf("Case_Outputs\\task20g_LTLA_fits\\model_fit_%s_%s.png", i, areaName_hold),
      width=1440, height=1080, res = 150)
  plot(plot_hold)
  dev.off()
}



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
    ggtitle(sprintf("Weekly case rates reported in %s", areaName_hold)) +
    geom_point(data = filter(Model_fit_data, areaName == areaName_hold), aes(x = date, y = Real_Cases/Population), alpha = 0.7, shape = 18,
               color = 'black') -> plot_hold
  
  png(file=sprintf("Case_Outputs\\task20g_LTLA_infection_rates\\case_rate_fit_%s_%s.png", i, areaName_hold),
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
                                        ymin = 0.029, ymax = 0.03),
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
  ylim(c(0,0.03)) +
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
        axis.text.x = element_text(size = rel(1.2)),
        axis.title.x = element_text(size = rel(1.3)) ) +
  ggtitle(sprintf("%s ", LTLA_A_name)) +
  geom_point(data = LTLA_A_data, aes(x = date, y = Real_Cases / Population,
                                     color = "Real Data"),
             alpha = 0.7, shape = 18, show.legend = FALSE) +
  scale_color_manual(values = c("Real Data" = "black", "Model Fit" = "#9BC362")) +
  scale_fill_manual(values = c("Model Fit" = "#9BC362")) +  # Adjust fill for the ribbon legend
  # Modify legend labels
  guides(color = guide_legend(override.aes = list(shape = c(18, NA), linetype = c(NA, 1)))) +
  labs(color = "Legend")  -> LTLA_A_plot_20g

ggplot() +
  geom_rect(data = lockdown_shades, aes(xmin = as.Date(date_start), xmax = as.Date(date_end),
                                        ymin = 0.029, ymax = 0.03),
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
  ylim(c(0,0.03)) +
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
        axis.text.x = element_text(size = rel(1.2)),
        axis.title.x = element_text(size = rel(1.3)) ) +
  ggtitle(sprintf("%s ", LTLA_B_name)) +
  geom_point(data = LTLA_B_data, aes(x = date, y = Real_Cases / Population,
                                     color = "Real Data"),
             alpha = 0.7, shape = 18, show.legend = FALSE) +
  scale_color_manual(values = c("Real Data" = "black", "Model Fit" = "#9BC362")) +
  scale_fill_manual(values = c("Model Fit" = "#9BC362")) +  # Adjust fill for the ribbon legend
  # Modify legend labels
  guides(color = guide_legend(override.aes = list(shape = c(18, NA), linetype = c(NA, 1)))) +
  labs(color = "Legend")  -> LTLA_B_plot_20g


##########################

#Now stick it all together in a cowplot.


LTLAs_plot_20g <- plot_grid(LTLA_A_plot_20g, LTLA_B_plot_20g, labels = c('A', 'B'), nrow = 2)

#Fig1_plot <- plot_grid(england_plot, LTLAs_plot, nrow = 2, labels = c("A", ""))

#Export the plot
ggsave(filename = "LTLAs_plot_20g.png",
       path = 'Case_Outputs\\fits', plot = LTLAs_plot_20g,
       dpi=300, height=11, width=11, units="in")

ggsave(filename = "LTLAs_plot_20g.tiff",
       path = 'Case_Outputs\\fits', plot = LTLAs_plot_20g,
       dpi=300, height=11, width=11, units="in")

ggsave(filename = "LTLAs_plot_20g.pdf",
       path = 'Case_Outputs\\fits', plot = LTLAs_plot_20g,
       dpi=300, height=11, width=11, units="in")


################################################################################
#NOW 20 J
################################################################################
################################################################################
load("CI_data_outputs_20j.RData")
############################################################################
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
                                        ymin = max(england_daily_total$Model_upper/1000)-30,
                                        ymax = max(england_daily_total$Model_upper/1000)),
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
  labs(color = "")  -> england_plot_20j

######################################


png(file="england_fit_20j.png",
    width=1440, height=1080, res = 150)
plot(england_plot_20j)
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
  
  png(file=sprintf("Case_Outputs\\task20j_LTLA_fits\\model_fit_%s_%s.png", i, areaName_hold),
      width=1440, height=1080, res = 150)
  plot(plot_hold)
  dev.off()
}



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
    ggtitle(sprintf("Weekly case rates reported in %s", areaName_hold)) +
    geom_point(data = filter(Model_fit_data, areaName == areaName_hold), aes(x = date, y = Real_Cases/Population), alpha = 0.7, shape = 18,
               color = 'black') -> plot_hold
  
  png(file=sprintf("Case_Outputs\\task20j_LTLA_infection_rates\\case_rate_fit_%s_%s.png", i, areaName_hold),
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
                                        ymin = 0.029, ymax = 0.03),
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
  ylim(c(0,0.03)) +
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
        axis.text.x = element_text(size = rel(1.2)),
        axis.title.x = element_text(size = rel(1.3)) ) +
  ggtitle(sprintf("%s ", LTLA_A_name)) +
  geom_point(data = LTLA_A_data, aes(x = date, y = Real_Cases / Population,
                                     color = "Real Data"),
             alpha = 0.7, shape = 18, show.legend = FALSE) +
  scale_color_manual(values = c("Real Data" = "black", "Model Fit" = "#9BC362")) +
  scale_fill_manual(values = c("Model Fit" = "#9BC362")) +  # Adjust fill for the ribbon legend
  # Modify legend labels
  guides(color = guide_legend(override.aes = list(shape = c(18, NA), linetype = c(NA, 1)))) +
  labs(color = "Legend")  -> LTLA_A_plot_20j

ggplot() +
  geom_rect(data = lockdown_shades, aes(xmin = as.Date(date_start), xmax = as.Date(date_end),
                                        ymin = 0.029, ymax = 0.03),
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
  ylim(c(0,0.03)) +
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
        axis.text.x = element_text(size = rel(1.2)),
        axis.title.x = element_text(size = rel(1.3)) ) +
  ggtitle(sprintf("%s ", LTLA_B_name)) +
  geom_point(data = LTLA_B_data, aes(x = date, y = Real_Cases / Population,
                                     color = "Real Data"),
             alpha = 0.7, shape = 18, show.legend = FALSE) +
  scale_color_manual(values = c("Real Data" = "black", "Model Fit" = "#9BC362")) +
  scale_fill_manual(values = c("Model Fit" = "#9BC362")) +  # Adjust fill for the ribbon legend
  # Modify legend labels
  guides(color = guide_legend(override.aes = list(shape = c(18, NA), linetype = c(NA, 1)))) +
  labs(color = "Legend")  -> LTLA_B_plot_20j


##########################

#Now stick it all together in a cowplot.


LTLAs_plot_20j <- plot_grid(LTLA_A_plot_20j, LTLA_B_plot_20j, labels = c('A', 'B'), nrow = 2)

#Fig1_plot <- plot_grid(england_plot, LTLAs_plot, nrow = 2, labels = c("A", ""))

#Export the plot
ggsave(filename = "LTLAs_plot_20j.png",
       path = 'Case_Outputs\\fits', plot = LTLAs_plot_20j,
       dpi=300, height=11, width=11, units="in")

ggsave(filename = "LTLAs_plot_20j.tiff",
       path = 'Case_Outputs\\fits', plot = LTLAs_plot_20j,
       dpi=300, height=11, width=11, units="in")

ggsave(filename = "LTLAs_plot_20j.pdf",
       path = 'Case_Outputs\\fits', plot = LTLAs_plot_20j,
       dpi=300, height=11, width=11, units="in")


################################################################################
#NOW 20 R
################################################################################
################################################################################
load("CI_data_outputs_20r.RData")
############################################################################
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
                                        ymin = max(england_daily_total$Model_upper/1000)-30,
                                        ymax = max(england_daily_total$Model_upper/1000)),
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
  labs(color = "")  -> england_plot_20r

######################################


png(file="england_fit_20r.png",
    width=1440, height=1080, res = 150)
plot(england_plot_20r)
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
  
  png(file=sprintf("Case_Outputs\\task20r_LTLA_fits\\model_fit_%s_%s.png", i, areaName_hold),
      width=1440, height=1080, res = 150)
  plot(plot_hold)
  dev.off()
}



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
    ggtitle(sprintf("Weekly case rates reported in %s", areaName_hold)) +
    geom_point(data = filter(Model_fit_data, areaName == areaName_hold), aes(x = date, y = Real_Cases/Population), alpha = 0.7, shape = 18,
               color = 'black') -> plot_hold
  
  png(file=sprintf("Case_Outputs\\task20r_LTLA_infection_rates\\case_rate_fit_%s_%s.png", i, areaName_hold),
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
                                        ymin = 0.029, ymax = 0.03),
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
  ylim(c(0,0.03)) +
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
        axis.text.x = element_text(size = rel(1.2)),
        axis.title.x = element_text(size = rel(1.3)) ) +
  ggtitle(sprintf("%s ", LTLA_A_name)) +
  geom_point(data = LTLA_A_data, aes(x = date, y = Real_Cases / Population,
                                     color = "Real Data"),
             alpha = 0.7, shape = 18, show.legend = FALSE) +
  scale_color_manual(values = c("Real Data" = "black", "Model Fit" = "#9BC362")) +
  scale_fill_manual(values = c("Model Fit" = "#9BC362")) +  # Adjust fill for the ribbon legend
  # Modify legend labels
  guides(color = guide_legend(override.aes = list(shape = c(18, NA), linetype = c(NA, 1)))) +
  labs(color = "Legend")  -> LTLA_A_plot_20r

ggplot() +
  geom_rect(data = lockdown_shades, aes(xmin = as.Date(date_start), xmax = as.Date(date_end),
                                        ymin = 0.029, ymax = 0.03),
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
  ylim(c(0,0.03)) +
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
        axis.text.x = element_text(size = rel(1.2)),
        axis.title.x = element_text(size = rel(1.3)) ) +
  ggtitle(sprintf("%s ", LTLA_B_name)) +
  geom_point(data = LTLA_B_data, aes(x = date, y = Real_Cases / Population,
                                     color = "Real Data"),
             alpha = 0.7, shape = 18, show.legend = FALSE) +
  scale_color_manual(values = c("Real Data" = "black", "Model Fit" = "#9BC362")) +
  scale_fill_manual(values = c("Model Fit" = "#9BC362")) +  # Adjust fill for the ribbon legend
  # Modify legend labels
  guides(color = guide_legend(override.aes = list(shape = c(18, NA), linetype = c(NA, 1)))) +
  labs(color = "Legend")  -> LTLA_B_plot_20r


##########################

#Now stick it all together in a cowplot.


LTLAs_plot_20r <- plot_grid(LTLA_A_plot_20r, LTLA_B_plot_20r, labels = c('A', 'B'), nrow = 2)

#Fig1_plot <- plot_grid(england_plot, LTLAs_plot, nrow = 2, labels = c("A", ""))

#Export the plot
ggsave(filename = "LTLAs_plot_20r.png",
       path = 'Case_Outputs\\fits', plot = LTLAs_plot_20r,
       dpi=300, height=11, width=11, units="in")

ggsave(filename = "LTLAs_plot_20r.tiff",
       path = 'Case_Outputs\\fits', plot = LTLAs_plot_20r,
       dpi=300, height=11, width=11, units="in")

ggsave(filename = "LTLAs_plot_20r.pdf",
       path = 'Case_Outputs\\fits', plot = LTLAs_plot_20r,
       dpi=300, height=11, width=11, units="in")

#################################
#Export the A and B comparisons
LTLAs_plot_A_comp <- plot_grid(LTLA_A_plot_20, LTLA_A_plot_20g,
                               LTLA_A_plot_20j, LTLA_A_plot_20r,
                               labels = c('A) task 20', 'B) task 20g',
                                          'C) task 20j', 'D) task 20r'), nrow = 2)

#Fig1_plot <- plot_grid(england_plot, LTLAs_plot, nrow = 2, labels = c("A", ""))

#Export the plot
ggsave(filename = "LTLAs_plot_A_comp.png",
       path = 'Case_Outputs', plot = LTLAs_plot_A_comp,
       dpi=300, height=11, width=11, units="in")

ggsave(filename = "LTLAs_plot_A_comp.tiff",
       path = 'Case_Outputs', plot = LTLAs_plot_A_comp,
       dpi=300, height=11, width=11, units="in")

ggsave(filename = "LTLAs_plot_A_comp.pdf",
       path = 'Case_Outputs', plot = LTLAs_plot_A_comp,
       dpi=300, height=11, width=11, units="in")

#############################################
LTLAs_plot_B_comp <- plot_grid(LTLA_B_plot_20, LTLA_B_plot_20g,
                               LTLA_B_plot_20j, LTLA_B_plot_20r,
                               labels = c('A) task 20', 'B) task 20g',
                                          'C) task 20j', 'D) task 20r'), nrow = 2)

#Fig1_plot <- plot_grid(england_plot, LTLAs_plot, nrow = 2, labels = c("A", ""))

#Export the plot
ggsave(filename = "LTLAs_plot_B_comp.png",
       path = 'Case_Outputs', plot = LTLAs_plot_B_comp,
       dpi=300, height=11, width=11, units="in")

ggsave(filename = "LTLAs_plot_B_comp.tiff",
       path = 'Case_Outputs', plot = LTLAs_plot_B_comp,
       dpi=300, height=11, width=11, units="in")

ggsave(filename = "LTLAs_plot_B_comp.pdf",
       path = 'Case_Outputs', plot = LTLAs_plot_B_comp,
       dpi=300, height=11, width=11, units="in")