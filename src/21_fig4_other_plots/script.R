#orderly::orderly_develop_start("21_fig4_other_plots", use_draft = "newer", parameters = list(tree_depth = 15, n_chains = 16))

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


############################################################################
#FIG 2
############################################################################
#This figure will show regional variation in certain covariates
#IMD, and, 


load("Boundaries_Data.RData")
#Remove to just the indices I've modelled:
areaCodes_used <- unique(Case_Rates_Data$areaCode)
Boundaries_reduced <- filter(Boundaries, CODE %in% areaCodes_used)


#CAN DO THIS BETTER:
zetas_mean <- get_posterior_mean(stanfit, pars = 'zetas')
zetas_mean <- as.data.frame(zetas_mean[,(n_chains+1)])

Boundaries_reduced$zetas <- as.numeric(zetas_mean[,1]) 
#Tomato Red: '#FF6347'
# Cyan: '#28A1D7'
#PLOT standard
  ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = zetas)) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  c("#28A1D7", "white", "#FF6347"),  # Blue, white, and red colors
    values = scales::rescale(c(min(Boundaries_reduced$zetas), median(Boundaries_reduced$zetas), max(Boundaries_reduced$zetas)))
  ) +
  ggtitle(expression(paste("Nearest-neighbour spatial kernel value,", zeta, ", by LTLA"))) +
    labs(fill = "Zeta") +
  theme_void() +
    theme(plot.margin = unit(c(0, 0, 0, 0.5), "cm"))  -> england_zetas
  
  ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
    geom_sf(aes(fill = zetas), show.legend = FALSE) +
    #scale_fill_viridis_c() +
    scale_fill_gradientn(
      colours =  c("#28A1D7", "white", "#FF6347"),  # Blue, white, and red colors
      values = scales::rescale(c(min(Boundaries_reduced$zetas), median(Boundaries_reduced$zetas), max(Boundaries_reduced$zetas)))
    ) +
    #ggtitle("Average Index of Multiple Deprivation (IMD) by LTLA") +
    labs(fill = "Average zeta Score") +
    theme_void() +
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))  -> london_zetas
  
  ##########################################################
  #And now we do the same thing for Theta!
  ##########################################################
  
  #CAN DO THIS BETTER:
  theta_mean <- get_posterior_mean(stanfit, pars = 'theta')
  theta_mean <- as.data.frame(theta_mean[,(n_chains+1)])
  
  Boundaries_reduced$thetas <- as.numeric(theta_mean[,1]) 
  #Tomato Red: '#FF6347'
  # Cyan: '#28A1D7'
  #PLOT standard
  ggplot(Boundaries_reduced) +
    geom_sf(aes(fill = thetas)) +
    #scale_fill_viridis_c() +
    scale_fill_gradientn(
      colours =  c("#39C687", "white", "#C63978"),  # Blue, white, and red colors
      values = scales::rescale(c(min(Boundaries_reduced$thetas), median(Boundaries_reduced$thetas), max(Boundaries_reduced$thetas)))
    ) +
    ggtitle(expression(paste("Spatial error term, ", theta, ", by LTLA"))) +
    labs(fill = "Theta") +
    theme_void() +
    theme(plot.margin = unit(c(0, 0, 0, 0.5), "cm"))  -> england_theta
  
  ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
    geom_sf(aes(fill = thetas), show.legend = FALSE) +
    #scale_fill_viridis_c() +
    scale_fill_gradientn(
      colours =  c("#39C687", "white", "#C63978"),  # Blue, white, and red colors
      values = scales::rescale(c(min(Boundaries_reduced$thetas), median(Boundaries_reduced$thetas), max(Boundaries_reduced$thetas)))
    ) +
    #ggtitle("Average Index of Multiple Deprivation (IMD) by LTLA") +
    labs(fill = "Average theta Score") +
    theme_void() +
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))  -> london_theta
  


  
  
Fig_AB_plot <- plot_grid(england_zetas, england_theta, 
                       nrow = 1, labels = c("A", "B"), align = "v", axis = "bt") #+
  #theme(plot.background = element_rect(fill = "white"))

ggsave(filename = "ab_fig.png",
       path = 'Case_Outputs\\', plot = Fig_AB_plot,
       dpi=300, height=6.31, width=11, units="in")

ggsave(filename = "ab_fig.tiff",
       path = 'Case_Outputs\\', plot = Fig_AB_plot,
       dpi=300, height=6.31, width=11, units="in")

ggsave(filename = "ab_fig.pdf",
       path = 'Case_Outputs\\', plot = Fig_AB_plot,
       dpi=300, height=6.31, width=11, units="in")

#########################################
#FIGURE C
#Now the random walk plot.
#Plot Random Walk but with CrI too
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

random_walk_summaries <- summary(stanfit, pars = c('beta_random_walk'))
random_walk_summaries <- random_walk_summaries$summary

All_dates <- sort(unique(Case_Rates_Data$date_begin))
rw_data <- data.frame( Week = All_dates, mean = random_walk_summaries[,1],
                       lower = random_walk_summaries[,4],
                       upper = random_walk_summaries[,8])

ggplot(rw_data) +
  geom_line(aes(x = Week, y = mean), color = "#39C687") +
  geom_ribbon(aes(x = Week, ymin = lower, ymax = upper), fill = "#39C687", alpha = 0.4) +
  ylab(bquote(z[t] * " value")) + theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_cartesian(xlim = c(as.Date('2020-05-01'), as.Date('2022-04-01'))) +
  geom_rect(data = lockdown_shades, aes(xmin = as.Date(date_start), xmax = as.Date(date_end),
                                        ymin = max(rw_data$upper)-0.08,
                                        ymax = max(rw_data$upper)),
            fill = c('#FFB6B6', "#FFD6A5", '#FFB6B6', "#FFD6A5", '#FFB6B6', "#FFD6A5", "#C8E7C1"),
            show.legend = FALSE) +
  # geom_vline(xintercept = as.Date(variant_dates), alpha = 0.9, color = 'black') +
  geom_vline(xintercept = as.Date(grey_lines), alpha = 0.7, color = 'gray23', lty = 'dashed') +
  xlab("Date") +
  ggtitle("Random Walk term trajectory") +
  # annotate('text', x = as.Date("2020-07-26") - 13, label = "Alpha emergence", y = -1.3, colour = "black", size = 4, angle = 90) +
  # annotate('text', x = as.Date("2021-03-28") - 13, label = "Delta emergence", y = -1.3, colour = "black", size = 4, angle = 90) +
  # annotate('text', x = as.Date("2021-09-12") - 13, label = "Omicron emergence", y = 0.3, colour = "black", size = 4, angle = 90) +
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.2)),
        legend.title = element_text(size = rel(1.3))) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y") -> rw_plot


Fig_plot <- plot_grid(Fig_AB_plot, rw_plot, 
                         nrow = 2, labels = c("", "C"), rel_heights = c(0.6,0.4),
                      align = "v", axis = "bt") +
  theme(plot.background = element_rect(fill = "white"))

ggsave(filename = "fig.png",
       path = 'Case_Outputs\\', plot = Fig_plot,
       dpi=300, height=10, width=11, units="in")

ggsave(filename = "fig.tiff",
       path = 'Case_Outputs\\', plot = Fig_plot,
       dpi=300, height=10, width=11, units="in")

ggsave(filename = "fig.pdf",
       path = 'Case_Outputs\\', plot = Fig_plot,
       dpi=300, height=10, width=11, units="in")