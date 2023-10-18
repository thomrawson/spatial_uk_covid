#orderly::orderly_develop_start("16_IMD_quintiles", use_draft = "newer", parameters = list(tree_depth = 15, n_chains = 16))

#This script loads in the stanfit object and looks at how case rates vary between the 5 IMQ quintiles
load("model_data.RData")

dir.create("Case_Outputs")

################################################################################
Case_Rates_Data <- Case_Rates_Data %>%
  mutate(IMD_quintile = ntile(IMD_Average_score, 5))

#test
sort(unique(Case_Rates_Data$IMD_Average_score))
#26.79 - 45 is the highest (5)
# 21.06 - 26.79 (4)
#16.27 - 21.06 (3)
#12.31 - 16.27 (2)
#5.54 - 12.31 is the lowest IMD (1)
################################################################
#Let's make a Case rates column:
Case_Rates_Data$Case_Rates <- Case_Rates_Data$Week_Cases/Case_Rates_Data$Population

quintile_data <- Case_Rates_Data %>%
  dplyr::group_by(IMD_quintile, date_begin) %>%
  dplyr::summarise(
    mean_Case_Rates = mean(Case_Rates),
    lower_Case_Rates = quantile(Case_Rates, 0.025),
    upper_Case_Rates = quantile(Case_Rates, 0.975)
  ) %>%
  ungroup()

quintile_data$IMD_quintile <- as.factor(quintile_data$IMD_quintile)

#custom_palette <- c("#4CAF50", "#8BC34A", "#FFC107", "#FF9800", "#F44336")
#custom_palette <- c("#4CAF50", "#8BC34A", "#FFC107", "#EB7253", "#e54339")
custom_palette <- c("#17bebb", "#8BC061", "#FFC107", "#F28220", "#e54339")

#17bebb
#custom_palette <- c("#A5D46E", "#D0A75C", "#EB7253")

ggplot(quintile_data, aes(x = date_begin, y = mean_Case_Rates, group = IMD_quintile, fill = IMD_quintile)) +
  geom_line(aes(color = IMD_quintile), size = 1) +
  geom_ribbon(aes(ymin = lower_Case_Rates, ymax = upper_Case_Rates, fill = IMD_quintile), alpha = 0.09) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y") +
  coord_cartesian(xlim = c(as.Date('2020-05-01'), as.Date('2022-03-01'))) +
  labs(x = "Date", y = "Average Case Rates", color = "IMD Quintile") +
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.2)),
        legend.title = element_text(size = rel(1.3))) +
  ggtitle("Average COVID-19 Case Rates in UK LTLAs \nby IMD Quintile ") +
  scale_color_manual(name = "IMD Quintile", values = custom_palette,
                     labels = c("1 - Lowest \nvalues",
                                "2", "3", "4", "5 - Highest \nvalues")) + 
  scale_fill_manual(name = "IMD Quintile", values = custom_palette,
                    labels = c("1 - Lowest \nvalues",
                               "2", "3", "4", "5 - Highest \nvalues")) -> quintile_plot


#Export the plot
ggsave(filename = "IMD_quintile_plot.png",
       path = 'Case_Outputs', plot = quintile_plot,
       dpi=300, height=6.31, width=11, units="in")

ggsave(filename = "IMD_quintile_plot.tiff",
       path = 'Case_Outputs', plot = quintile_plot,
       dpi=300, height=6.31, width=11, units="in")

ggsave(filename = "IMD_quintile_plot.pdf",
       path = 'Case_Outputs', plot = quintile_plot,
       dpi=300, height=6.31, width=11, units="in")

#Now do the same for prop. white british
################################################################################
Case_Rates_Data <- Case_Rates_Data %>%
  mutate(prop_white_quintile = ntile(prop_white_british, 5))

#test
sort(unique(Case_Rates_Data$prop_white_british))
#96 - 100 is the highest (5)
# 92.2 - 96 (4)
#86.6- 92.2 (3)
#75.9 - 86.6 (2)
#15.4 - 75.9 is the lowest IMD (1)
################################################################

prop_white_data <- Case_Rates_Data %>%
  dplyr::group_by(prop_white_quintile, date_begin) %>%
  dplyr::summarise(
    mean_Case_Rates = mean(Case_Rates),
    lower_Case_Rates = quantile(Case_Rates, 0.025),
    upper_Case_Rates = quantile(Case_Rates, 0.975)
  ) %>%
  ungroup()

prop_white_data$prop_white_quintile <- as.factor(prop_white_data$prop_white_quintile)

ggplot(prop_white_data, aes(x = date_begin, y = mean_Case_Rates, group = prop_white_quintile, fill = prop_white_quintile)) +
  geom_line(aes(color = prop_white_quintile), size = 1) +
  geom_ribbon(aes(ymin = lower_Case_Rates, ymax = upper_Case_Rates, fill = prop_white_quintile), alpha = 0.09) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y") +
  coord_cartesian(xlim = c(as.Date('2020-05-01'), as.Date('2022-03-01'))) +
  labs(x = "Date", y = "Average Case Rates", color = "IMD Quintile") +
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.2)),
        legend.title = element_text(size = rel(1.3))) +
  ggtitle("Average COVID-19 Case Rates in UK LTLAs \nby Proportion White British Quintile ") +
  scale_color_manual(name = "Prop. White \nBritish Quintile", values = custom_palette) + 
  scale_fill_manual(name = "Prop. White \nBritish Quintile", values = custom_palette) -> prop_white_quintile_plot


#Export the plot
ggsave(filename = "prop_white_quintile_plot.png",
       path = 'Case_Outputs', plot = prop_white_quintile_plot,
       dpi=300, height=6.31, width=11, units="in")

ggsave(filename = "prop_white_quintile_plot.tiff",
       path = 'Case_Outputs', plot = prop_white_quintile_plot,
       dpi=300, height=6.31, width=11, units="in")

ggsave(filename = "prop_white_quintile_plot.pdf",
       path = 'Case_Outputs', plot = prop_white_quintile_plot,
       dpi=300, height=6.31, width=11, units="in")


#Then I want the regional shading plots
############################################################################
#FIG 2
############################################################################
#This figure will show regional variation in certain covariates
#IMD, and, prop_white_british


####################################################
#I want to output plots for LTLA_A, and LTLA_B to compare
LTLA_A_name <- "Barking and Dagenham"
LTLA_B_name <- "Torbay"
LTLA_A_data <- filter(Case_Rates_Data, areaName == LTLA_A_name)
LTLA_B_data <- filter(Case_Rates_Data, areaName == LTLA_B_name)

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
    #colours =  c("#9BC362", "white", "#e54339"),  # Blue, white, and red colors
    colours = c("#17bebb", "#ffc914", "#e4572e"),
    values = scales::rescale(c(min(Boundaries_reduced$IMD_Average_score), median(Boundaries_reduced$IMD_Average_score), max(Boundaries_reduced$IMD_Average_score)))
  ) +
  ggtitle("Average Index of Multiple Deprivation (IMD) by LTLA") +
  labs(fill = "Average \nIMD Score") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0.5), "cm")) -> england_IMD
#+
  # Highlight the two specific regions
  #geom_sf(data = Boundaries_reduced %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
  #        color = "blue", fill = NA, size = 0.8) -> england_IMD

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = IMD_Average_score), show.legend = FALSE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    #colours =  c("#9BC362", "white", "#e54339"),  # Blue, white, and red colors
    colours = c("#17bebb", "#ffc914", "#e4572e"),
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
    #colours =  c("#9BC362", "white", "#e54339"),  # Blue, white, and red colors
    colours = c("#17bebb", "#ffc914", "#e4572e"),
    values = scales::rescale(c(min(Boundaries_reduced$prop_white_british), median(Boundaries_reduced$prop_white_british), max(Boundaries_reduced$prop_white_british)))
  ) +
  ggtitle("White British Proportion of LTLA Population") +
  labs(fill = "Proportion \nWhite British") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0.5, 0, 0), "cm")) -> england_prop_white  #+
  # Highlight the two specific regions
  #geom_sf(data = Boundaries_reduced %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
  #        color = "blue", fill = NA, size = 0.8) -> england_prop_white


Fig2_plot <- plot_grid(england_IMD, england_prop_white, 
                       nrow = 1, labels = c("A", "B"), align = "v", axis = "bt") +
  theme(plot.background = element_rect(fill = "white"))

ggsave(filename = "spatial_covariates.png",
       path = 'Case_Outputs', plot = Fig2_plot,
       dpi=300, height=6.31, width=11, units="in")

ggsave(filename = "spatial_covariates.tiff",
       path = 'Case_Outputs', plot = Fig2_plot,
       dpi=300, height=6.31, width=11, units="in")

ggsave(filename = "spatial_covariates.pdf",
       path = 'Case_Outputs', plot = Fig2_plot,
       dpi=300, height=6.31, width=11, units="in")


Fig2_plot <- plot_grid(england_IMD, england_prop_white, 
                       nrow = 1, labels = c("C", "D"), align = "v", axis = "bt") #+
  #theme(plot.background = element_rect(fill = "white"))

combined_quintile <- plot_grid(quintile_plot+xlab(""), prop_white_quintile_plot,
                               ncol = 1, labels = c("A","B"),
                               align = "v", axis = "tb")

Total_Fig_plot <- plot_grid(combined_quintile, Fig2_plot,
                            nrow = 1,
                            #labels = c("A","B",""),
                            rel_widths = c(1.25,2)
                            ) +
  theme(plot.background = element_rect(fill = "white"))

ggsave(filename = "Final_fig1.png",
       path = 'Case_Outputs', plot = Total_Fig_plot,
       dpi=300, height=6, width=18, units="in")

ggsave(filename = "Final_fig1.tiff",
       path = 'Case_Outputs', plot = Total_Fig_plot,
       dpi=300, height=6, width=18, units="in")

ggsave(filename = "Final_fig1.pdf",
       path = 'Case_Outputs', plot = Total_Fig_plot,
       dpi=300, height=6, width=18, units="in")


#Try another way around
Fig2_plot <- plot_grid(england_IMD, england_prop_white, 
                       ncol = 1, labels = c("A", "C"), align = "v", axis = "bt") 

combined_quintile <- plot_grid(quintile_plot+xlab(""), prop_white_quintile_plot,
                               ncol = 1, labels = c("B","D"),
                               align = "v", axis = "tb")

Total_Fig_plot <- plot_grid(Fig2_plot, combined_quintile,
                            nrow = 1,
                            #labels = c("A","B",""),
                            rel_widths = c(1,0.8))

# now add the title
title <- ggdraw() + 
  draw_label(
    "Variation in socio-demographic factors by LTLA and the respective stratification of COVID-19 case rates",
    fontface = 'bold',
    x = 0,
    hjust = 0,
    size = 20
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
plot_grid(
  title, Total_Fig_plot,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.06, 1)
) +
  theme(plot.background = element_rect(fill = "white", color = "white")) -> Total_Fig_plot_title

ggsave(filename = "Final_fig2.png",
       path = 'Case_Outputs', plot = Total_Fig_plot_title,
       dpi=300, height=15, width=14, units="in")

ggsave(filename = "Final_fig2.tiff",
       path = 'Case_Outputs', plot = Total_Fig_plot_title,
       dpi=300, height=15, width=14, units="in")

ggsave(filename = "Final_fig2.pdf",
       path = 'Case_Outputs', plot = Total_Fig_plot_title,
       dpi=300, height=15, width=14, units="in")
