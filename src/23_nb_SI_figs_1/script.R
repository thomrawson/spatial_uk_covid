#orderly::orderly_develop_start("23_nb_SI_figs_1", use_draft = "newer", parameters = list(tree_depth = 15, n_chains = 16))

#This script loads in the stanfit object and outputs some demos of paper figures to play around with
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

#colours_list2 <- c("#26547c", "#ffd166", "#ef476f")
colours_list <- c("#17bebb", "#ffc914", "#e4572e")

############################################################################
# 1) prop_asian
############################################################################
#This figure will show regional variation in covariates



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
  select(areaCode, areaName, INDEX, prop_asian) -> fig_data

fig_data <- fig_data[order(fig_data$INDEX),]

Boundaries_reduced$prop_asian <- as.numeric(fig_data$prop_asian) 

variable_vector <- Boundaries_reduced$prop_asian
# gb_cities <- read.csv("gb_cities.csv")
# #Manually trim off the ones we don't want.
# gb_cities <- gb_cities[1:30,]
# gb_cities <- gb_cities[-c(6,7,9,16,17,18,19,21,22,24,25,27,29,30),]
# 
# gb_cities %>%
#   st_as_sf(coords = c("lng", "lat"), crs = 4326) -> gb_cities

#PLOT standard
  ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = prop_asian), show.legend = FALSE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Asian proportion of the population by LTLA") +
    labs(fill = "Proportion Asian") +
  theme_void() +
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_asian
    # Highlight the two specific regions
  #geom_sf(data = Boundaries_reduced %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
  #        color = "blue", fill = NA, size = 0.8) 
  
  ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
    geom_sf(aes(fill = prop_asian), show.legend = TRUE) +
    #scale_fill_viridis_c() +
    scale_fill_gradientn(
      colours =  colours_list,  # Blue, white, and red colors
      values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
      limits = c(min(variable_vector),max(variable_vector))
    ) +
    #ggtitle("Average Index of Multiple Deprivation (IMD) by LTLA") +
    labs(fill = "Proportion Asian") +
    theme_void() +
    theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_asian
    # Highlight the two specific regions
    #geom_sf(data = Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),] %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
    #        color = "blue", fill = NA, size = 0.8) 
  


  combined_plot <- plot_grid(england_asian, london_asian, nrow = 1, rel_widths = c(2.5,1)) +
    theme(plot.background = element_rect(fill = "white", colour = "white"))
  

ggsave(filename = "prop_asian.png",
       path = 'Case_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")

# ggsave(filename = "f2_covariates.tiff",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")
# 
# ggsave(filename = "f2_covariates.pdf",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")

#########################################
############################################################################
# 1) prop_black
############################################################################
#This figure will show regional variation in covariates



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
  select(areaCode, areaName, INDEX, prop_black_afr_car) -> fig_data

fig_data <- fig_data[order(fig_data$INDEX),]

Boundaries_reduced$prop_black_afr_car <- as.numeric(fig_data$prop_black_afr_car) 

variable_vector <- Boundaries_reduced$prop_black_afr_car
# gb_cities <- read.csv("gb_cities.csv")
# #Manually trim off the ones we don't want.
# gb_cities <- gb_cities[1:30,]
# gb_cities <- gb_cities[-c(6,7,9,16,17,18,19,21,22,24,25,27,29,30),]
# 
# gb_cities %>%
#   st_as_sf(coords = c("lng", "lat"), crs = 4326) -> gb_cities

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = prop_black_afr_car), show.legend = FALSE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Black African / Caribbean proportion of the population by LTLA") +
  labs(fill = "Proportion Black") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_black
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = prop_black_afr_car), show.legend = TRUE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  #ggtitle("Average Index of Multiple Deprivation (IMD) by LTLA") +
  labs(fill = "Proportion Black") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_black
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),] %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 



combined_plot <- plot_grid(england_black, london_black, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "prop_black_afr_car.png",
       path = 'Case_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")

# ggsave(filename = "f2_covariates.tiff",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")
# 
# ggsave(filename = "f2_covariates.pdf",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")

#########################################
############################################################################
# 3) prop_mixed_multiple + prop_other
############################################################################
#This figure will show regional variation in covariates



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
  select(areaCode, areaName, INDEX, prop_other, prop_mixed_multiple) -> fig_data

fig_data <- fig_data[order(fig_data$INDEX),]

Boundaries_reduced$prop_other <- as.numeric(fig_data$prop_mixed_multiple) + as.numeric(fig_data$prop_other)

variable_vector <- Boundaries_reduced$prop_other
# gb_cities <- read.csv("gb_cities.csv")
# #Manually trim off the ones we don't want.
# gb_cities <- gb_cities[1:30,]
# gb_cities <- gb_cities[-c(6,7,9,16,17,18,19,21,22,24,25,27,29,30),]
# 
# gb_cities %>%
#   st_as_sf(coords = c("lng", "lat"), crs = 4326) -> gb_cities

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = prop_other), show.legend = FALSE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle(" All other ethnicity proportion of the population by LTLA") +
  labs(fill = "Proportion Other \nEthnicity") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_other
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = prop_other), show.legend = TRUE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  #ggtitle("Average Index of Multiple Deprivation (IMD) by LTLA") +
  labs(fill = "Proportion Other \nEthnicity") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_other
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),] %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 



combined_plot <- plot_grid(england_other, london_other, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "prop_other.png",
       path = 'Case_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")

# ggsave(filename = "f2_covariates.tiff",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")
# 
# ggsave(filename = "f2_covariates.pdf",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")

#########################################
############################################################################
# 4) prop_white_british + prop_all_other_white
############################################################################
#This figure will show regional variation in covariates



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
  select(areaCode, areaName, INDEX, prop_white_british, prop_all_other_white) -> fig_data

fig_data <- fig_data[order(fig_data$INDEX),]

Boundaries_reduced$prop_white <- as.numeric(fig_data$prop_white_british) + as.numeric(fig_data$prop_all_other_white)

variable_vector <- Boundaries_reduced$prop_white
# gb_cities <- read.csv("gb_cities.csv")
# #Manually trim off the ones we don't want.
# gb_cities <- gb_cities[1:30,]
# gb_cities <- gb_cities[-c(6,7,9,16,17,18,19,21,22,24,25,27,29,30),]
# 
# gb_cities %>%
#   st_as_sf(coords = c("lng", "lat"), crs = 4326) -> gb_cities

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = prop_white), show.legend = FALSE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("White proportion of the population by LTLA") +
  labs(fill = "Proportion White") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_white
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = prop_white), show.legend = TRUE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  #ggtitle("Average Index of Multiple Deprivation (IMD) by LTLA") +
  labs(fill = "Proportion White") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_white
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),] %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 



combined_plot <- plot_grid(england_white, london_white, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "prop_white.png",
       path = 'Case_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")

# ggsave(filename = "f2_covariates.tiff",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")
# 
# ggsave(filename = "f2_covariates.pdf",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")

#########################################

############################################################################
# 5) IMD_Average_score
############################################################################
#This figure will show regional variation in covariates



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
  select(areaCode, areaName, INDEX, IMD_Average_score) -> fig_data

fig_data <- fig_data[order(fig_data$INDEX),]

Boundaries_reduced$IMD_Average_score <- as.numeric(fig_data$IMD_Average_score) 

variable_vector <- Boundaries_reduced$IMD_Average_score

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = IMD_Average_score), show.legend = FALSE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Index of Multiple Deprivation (IMD) by LTLA") +
  labs(fill = "IMD") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_IMD
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = IMD_Average_score), show.legend = TRUE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  #ggtitle("Average Index of Multiple Deprivation (IMD) by LTLA") +
  labs(fill = "IMD") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_IMD
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),] %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 



combined_plot <- plot_grid(england_IMD, london_IMD, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "IMD_Average_score.png",
       path = 'Case_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")

# ggsave(filename = "f2_covariates.tiff",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")
# 
# ggsave(filename = "f2_covariates.pdf",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")

#########################################
############################################################################
# 5) prop_o65
############################################################################
#This figure will show regional variation in covariates



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
  select(areaCode, areaName, INDEX, prop_o65) -> fig_data

fig_data <- fig_data[order(fig_data$INDEX),]

Boundaries_reduced$prop_o65 <- as.numeric(fig_data$prop_o65) 

variable_vector <- Boundaries_reduced$prop_o65

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = prop_o65), show.legend = FALSE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Proportion of population over the age of 65 by LTLA") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_o65
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = prop_o65), show.legend = TRUE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  #ggtitle("Average Index of Multiple Deprivation (IMD) by LTLA") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_o65
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),] %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 



combined_plot <- plot_grid(england_o65, london_o65, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "prop_o65.png",
       path = 'Case_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")

# ggsave(filename = "f2_covariates.tiff",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")
# 
# ggsave(filename = "f2_covariates.pdf",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")

#########################################
#########################################
############################################################################
# Pop_per_km2
############################################################################
#This figure will show regional variation in covariates



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
  select(areaCode, areaName, INDEX, Pop_per_km2) -> fig_data

fig_data <- fig_data[order(fig_data$INDEX),]

Boundaries_reduced$Pop_per_km2 <- as.numeric(fig_data$Pop_per_km2) 

variable_vector <- Boundaries_reduced$Pop_per_km2

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = Pop_per_km2), show.legend = FALSE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Population density by LTLA (Population per square kilometer)") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_popden
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = Pop_per_km2), show.legend = TRUE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  #ggtitle("Average Index of Multiple Deprivation (IMD) by LTLA") +
  labs(fill = "Population Density") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_popden
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),] %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 



combined_plot <- plot_grid(england_popden, london_popden, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "Pop_per_km2.png",
       path = 'Case_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")

# ggsave(filename = "f2_covariates.tiff",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")
# 
# ggsave(filename = "f2_covariates.pdf",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")

#########################################
############################################################################
# Median_annual_income 2020/21
############################################################################
#This figure will show regional variation in covariates



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
  select(areaCode, areaName, INDEX, Median_annual_income) -> fig_data

fig_data <- fig_data[order(fig_data$INDEX),]

Boundaries_reduced$Median_annual_income <- as.numeric(fig_data$Median_annual_income)/1000

variable_vector <- Boundaries_reduced$Median_annual_income

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = Median_annual_income), show.legend = FALSE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Median annual income (\u00a3 thousand) by LTLA for 2020/21 financial year") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_income
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = Median_annual_income), show.legend = TRUE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  #ggtitle("Average Index of Multiple Deprivation (IMD) by LTLA") +
  labs(fill = "Median Income") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_income
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),] %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 



combined_plot <- plot_grid(england_income, london_income, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "Median_annual_income_2020_21.png",
       path = 'Case_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")

# ggsave(filename = "f2_covariates.tiff",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")
# 
# ggsave(filename = "f2_covariates.pdf",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")

#########################################
############################################################################
# Median_annual_income 2021/22
############################################################################
#This figure will show regional variation in covariates



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
  filter(Week == 90) %>% #Any week will do
  select(areaCode, areaName, INDEX, Median_annual_income) -> fig_data

fig_data <- fig_data[order(fig_data$INDEX),]

Boundaries_reduced$Median_annual_income <- as.numeric(fig_data$Median_annual_income)/1000

variable_vector <- Boundaries_reduced$Median_annual_income

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = Median_annual_income), show.legend = FALSE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Median annual income (\u00a3 thousand) by LTLA for 2021/22 financial year") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_income
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = Median_annual_income), show.legend = TRUE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  #ggtitle("Average Index of Multiple Deprivation (IMD) by LTLA") +
  labs(fill = "Median Income") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_income
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),] %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 



combined_plot <- plot_grid(england_income, london_income, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "Median_annual_income_2021_22.png",
       path = 'Case_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")

# ggsave(filename = "f2_covariates.tiff",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")
# 
# ggsave(filename = "f2_covariates.pdf",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")

#########################################
############################################################################
# unringfenced 2020/21
############################################################################
#This figure will show regional variation in covariates



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
  select(areaCode, areaName, INDEX, unringfenced, Population) -> fig_data

fig_data <- fig_data[order(fig_data$INDEX),]

Boundaries_reduced$unringfenced <- (as.numeric(fig_data$unringfenced)/as.numeric(fig_data$Population))*1000000

variable_vector <- Boundaries_reduced$unringfenced

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = unringfenced), show.legend = FALSE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Unringfenced COVID-19 funding (\u00a3 thousand) per thousand people by LTLA for 2020/21 financial year") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_unringfenced
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = unringfenced), show.legend = TRUE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  #ggtitle("Average Index of Multiple Deprivation (IMD) by LTLA") +
  labs(fill = "Unringfenced \nFunding") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_unringfenced
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),] %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 



combined_plot <- plot_grid(england_unringfenced, london_unringfenced, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "unringfenced_2020_21.png",
       path = 'Case_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")

# ggsave(filename = "f2_covariates.tiff",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")
# 
# ggsave(filename = "f2_covariates.pdf",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")

#########################################
############################################################################
# unringfenced 2021/22
############################################################################
#This figure will show regional variation in covariates



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
  filter(Week == 90) %>% #Any week will do
  select(areaCode, areaName, INDEX, unringfenced, Population) -> fig_data

fig_data <- fig_data[order(fig_data$INDEX),]

Boundaries_reduced$unringfenced <- (as.numeric(fig_data$unringfenced)/as.numeric(fig_data$Population))*1000000

variable_vector <- Boundaries_reduced$unringfenced

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = unringfenced), show.legend = FALSE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Unringfenced COVID-19 funding (\u00a3 thousand) per thousand people by LTLA for 2021/22 financial year") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_unringfenced
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = unringfenced), show.legend = TRUE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  #ggtitle("Average Index of Multiple Deprivation (IMD) by LTLA") +
  labs(fill = "Unringfenced \nFunding") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_unringfenced
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),] %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 



combined_plot <- plot_grid(england_unringfenced, london_unringfenced, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "unringfenced_2021_22.png",
       path = 'Case_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")

# ggsave(filename = "f2_covariates.tiff",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")
# 
# ggsave(filename = "f2_covariates.pdf",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")

#########################################
############################################################################
# contain_outbreak_management2020/21
############################################################################
#This figure will show regional variation in covariates



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
  select(areaCode, areaName, INDEX, contain_outbreak_management, Population) -> fig_data

fig_data <- fig_data[order(fig_data$INDEX),]

Boundaries_reduced$contain_outbreak_management <- (as.numeric(fig_data$contain_outbreak_management)/as.numeric(fig_data$Population))*1000000

variable_vector <- Boundaries_reduced$contain_outbreak_management

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = contain_outbreak_management), show.legend = FALSE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Contain Outbreak Management Fund (COMF) funding (\u00a3 thousand) per thousand people by LTLA for 2020/21 financial year") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_comf
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = contain_outbreak_management), show.legend = TRUE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  #ggtitle("Average Index of Multiple Deprivation (IMD) by LTLA") +
  labs(fill = "COMF Funding") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_comf
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),] %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 



combined_plot <- plot_grid(england_comf, london_comf, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "contain_outbreak_management_2020_21.png",
       path = 'Case_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")

# ggsave(filename = "f2_covariates.tiff",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")
# 
# ggsave(filename = "f2_covariates.pdf",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")

#########################################
############################################################################
# contain_outbreak_management2021/22
############################################################################
#This figure will show regional variation in covariates



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
  filter(Week == 90) %>% #Any week will do
  select(areaCode, areaName, INDEX, contain_outbreak_management, Population) -> fig_data

fig_data <- fig_data[order(fig_data$INDEX),]

Boundaries_reduced$contain_outbreak_management <- (as.numeric(fig_data$contain_outbreak_management)/as.numeric(fig_data$Population))*1000000

variable_vector <- Boundaries_reduced$contain_outbreak_management

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = contain_outbreak_management), show.legend = FALSE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Contain Outbreak Management Fund (COMF) funding (\u00a3 thousand) per thousand people by LTLA for 2021/22 financial year") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_comf
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = contain_outbreak_management), show.legend = TRUE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  #ggtitle("Average Index of Multiple Deprivation (IMD) by LTLA") +
  labs(fill = "COMF Funding") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_comf
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),] %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 



combined_plot <- plot_grid(england_comf, london_comf, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "contain_outbreak_management_2021_22.png",
       path = 'Case_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")

# ggsave(filename = "f2_covariates.tiff",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")
# 
# ggsave(filename = "f2_covariates.pdf",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")

#########################################
############################################################################
# ASC_infection_control_fund 2020/21
############################################################################
#This figure will show regional variation in covariates



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
  select(areaCode, areaName, INDEX, ASC_infection_control_fund, Population) -> fig_data

fig_data <- fig_data[order(fig_data$INDEX),]

Boundaries_reduced$ASC_infection_control_fund <- (as.numeric(fig_data$ASC_infection_control_fund)/as.numeric(fig_data$Population))*1000000

variable_vector <- Boundaries_reduced$ASC_infection_control_fund

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = ASC_infection_control_fund), show.legend = FALSE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Adult Social Care (ASC) infection control funding (\u00a3 thousand) per thousand people by LTLA for 2020/21 financial year") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_asc
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = ASC_infection_control_fund), show.legend = TRUE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  #ggtitle("Average Index of Multiple Deprivation (IMD) by LTLA") +
  labs(fill = "ASC Funding") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_asc
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),] %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 



combined_plot <- plot_grid(england_asc, london_asc, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "ASC_infection_control_fund_2020_21.png",
       path = 'Case_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")

# ggsave(filename = "f2_covariates.tiff",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")
# 
# ggsave(filename = "f2_covariates.pdf",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")

#########################################
############################################################################
# ASC_infection_control_fund 2021/22
############################################################################
#This figure will show regional variation in covariates



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
  filter(Week == 90) %>% #Any week will do
  select(areaCode, areaName, INDEX, ASC_infection_control_fund, Population) -> fig_data

fig_data <- fig_data[order(fig_data$INDEX),]

Boundaries_reduced$ASC_infection_control_fund <- (as.numeric(fig_data$ASC_infection_control_fund)/as.numeric(fig_data$Population))*1000000

variable_vector <- Boundaries_reduced$ASC_infection_control_fund

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = ASC_infection_control_fund), show.legend = FALSE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Adult Social Care (ASC) infection control funding (\u00a3 thousand) per thousand people by LTLA for 2021/22 financial year") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_asc
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = ASC_infection_control_fund), show.legend = TRUE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  #ggtitle("Average Index of Multiple Deprivation (IMD) by LTLA") +
  labs(fill = "ASC Funding") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_asc
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),] %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 



combined_plot <- plot_grid(england_asc, london_asc, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "ASC_infection_control_fund_2021_22.png",
       path = 'Case_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")

# ggsave(filename = "f2_covariates.tiff",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")
# 
# ggsave(filename = "f2_covariates.pdf",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")

#########################################

#For these future plots, they change weekly, so we're going to plot weekly box-plots I think is the best way.
AllDates <- sort(unique(Case_Rates_Data$date_begin))
T <- length(AllDates)

grey_lines <- c(
  "2020-06-01", ## End of first stay-at-home-order
  "2020-11-05", ## Start of second stay at home order
  "2020-12-02", ## End of second stay-at-home order 
  "2021-01-06", ## Start of third stay-at-home order
  "2021-03-08", ## End of third stay-at-home order (step 1)
  "2021-07-19" ## End of steps, no more legal restrictions
)

lockdown_shades <- data.frame(date_start = as.Date(c("2020-04-01", grey_lines)),
                              date_end = as.Date(c(grey_lines, '2022-04-01' )),
                              rect_bottom = rep(775, 7),
                              rect_top = rep(800, 7),
                              rect_col = c("red", "orange", "red", "orange", "red", "orange", "green"))

###########################################
##########################################
#  Start with Time at Workplace 
##########################################

#Make a plot of boxplots for each week (so boxplot shows 306 LTLAs)
#Make the data
Plot_data <- data.frame(Time = rep(as.Date("01/01/1999"),(306*T)),
                         time_spent = rep(0,(306*T)))
for(i in 1:T){
  Plot_data$Time[(((i-1)*306)+1):((i)*306)] <- AllDates[i]
  
  reduced_data <- filter(Case_Rates_Data, date_begin == AllDates[i])
  
  Plot_data$time_spent[(((i-1)*306)+1):((i)*306)] <- reduced_data$workplaces_percent_change_from_baseline
}


ggplot(Plot_data, aes(x=Time, y=time_spent, group = Time)) + 
  geom_boxplot(alpha = 0.2) + ylab("Percent change") + xlab("Date") +
  #ylim(c(0,plyr::round_any(max(Plot_data$time_spent), 0.02, f=ceiling))) + 
  geom_hline(yintercept = 0, color = "red") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y") + 
  ggtitle("Percent change in people visiting workplaces compared to pre-pandemic over all LTLAs") + 
  #geom_vline(xintercept = as.Date(grey_lines), alpha = 1, color = 'darkred', lty = 'dashed') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> time_box

ggsave(filename = "workplace_boxplot.png",
       path = 'Case_Outputs', plot = time_box,
       dpi=300, height=9, width=11, units="in")

#We also plot the average value over all time


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
  #filter(areaName == "Halton") %>% #Any week will do
  select(areaCode, areaName, INDEX, workplaces_percent_change_from_baseline) -> fig_data

agg_fig_data <- fig_data %>%
  group_by(areaCode, areaName, INDEX) %>%
  dplyr::summarize(mean_workplaces_percent_change = mean(workplaces_percent_change_from_baseline))

fig_data <- agg_fig_data[order(agg_fig_data$INDEX),]

Boundaries_reduced$workplaces <- as.numeric(fig_data$mean_workplaces_percent_change)
                                  
variable_vector <- Boundaries_reduced$workplaces

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = workplaces), show.legend = FALSE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Mean percent difference in people visiting workplaces compared to pre-pandemic (averaged over all weeks)") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_workplaces
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = workplaces), show.legend = TRUE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  #ggtitle("Average Index of Multiple Deprivation (IMD) by LTLA") +
  labs(fill = "Workplace \nVisits") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_workplaces
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),] %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 



combined_plot <- plot_grid(england_workplaces, london_workplaces, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "workplaces.png",
       path = 'Case_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")

# ggsave(filename = "f2_covariates.tiff",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")
# 
# ggsave(filename = "f2_covariates.pdf",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")

#########################################

###########################################
##########################################
# transit stations
##########################################

#Make a plot of boxplots for each week (so boxplot shows 306 LTLAs)
#Make the data
Plot_data <- data.frame(Time = rep(as.Date("01/01/1999"),(306*T)),
                        time_spent = rep(0,(306*T)))
for(i in 1:T){
  Plot_data$Time[(((i-1)*306)+1):((i)*306)] <- AllDates[i]
  
  reduced_data <- filter(Case_Rates_Data, date_begin == AllDates[i])
  
  Plot_data$time_spent[(((i-1)*306)+1):((i)*306)] <- reduced_data$transit_stations_percent_change_from_baseline
}


ggplot(Plot_data, aes(x=Time, y=time_spent, group = Time)) + 
  geom_boxplot(alpha = 0.2) + ylab("Percent change") + xlab("Date") +
  #ylim(c(0,plyr::round_any(max(Plot_data$time_spent), 0.02, f=ceiling))) + 
  geom_hline(yintercept = 0, color = "red") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y") + 
  ggtitle("Percent change in people visiting transit stations compared to pre-pandemic over all LTLAs") + 
  #geom_vline(xintercept = as.Date(grey_lines), alpha = 1, color = 'darkred', lty = 'dashed') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> time_box

ggsave(filename = "transit_boxplot.png",
       path = 'Case_Outputs', plot = time_box,
       dpi=300, height=9, width=11, units="in")

#We also plot the average value over all time


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
  #filter(areaName == "Halton") %>% #Any week will do
  select(areaCode, areaName, INDEX, transit_stations_percent_change_from_baseline) -> fig_data

agg_fig_data <- fig_data %>%
  group_by(areaCode, areaName, INDEX) %>%
  dplyr::summarize(transit_stations_percent_change_from_baseline = mean(transit_stations_percent_change_from_baseline))

fig_data <- agg_fig_data[order(agg_fig_data$INDEX),]

Boundaries_reduced$transit <- as.numeric(fig_data$transit_stations_percent_change_from_baseline)

variable_vector <- Boundaries_reduced$transit

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = transit), show.legend = FALSE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Mean percent difference in people visiting transit stations compared to pre-pandemic (averaged over all weeks)") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_transit
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = transit), show.legend = TRUE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  #ggtitle("Average Index of Multiple Deprivation (IMD) by LTLA") +
  labs(fill = "Transit \nStation \nVisits") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_transit
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),] %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 



combined_plot <- plot_grid(england_transit, london_transit, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "transit.png",
       path = 'Case_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")

# ggsave(filename = "f2_covariates.tiff",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")
# 
# ggsave(filename = "f2_covariates.pdf",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")

#########################################
###########################################
##########################################
# residential
##########################################

#Make a plot of boxplots for each week (so boxplot shows 306 LTLAs)
#Make the data
Plot_data <- data.frame(Time = rep(as.Date("01/01/1999"),(306*T)),
                        time_spent = rep(0,(306*T)))
for(i in 1:T){
  Plot_data$Time[(((i-1)*306)+1):((i)*306)] <- AllDates[i]
  
  reduced_data <- filter(Case_Rates_Data, date_begin == AllDates[i])
  
  Plot_data$time_spent[(((i-1)*306)+1):((i)*306)] <- reduced_data$residential_percent_change_from_baseline
}


ggplot(Plot_data, aes(x=Time, y=time_spent, group = Time)) + 
  geom_boxplot(alpha = 0.2) + ylab("Percent change") + xlab("Date") +
  #ylim(c(0,plyr::round_any(max(Plot_data$time_spent), 0.02, f=ceiling))) + 
  geom_hline(yintercept = 0, color = "red") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y") + 
  ggtitle("Percent change in hours spent in residential areas compared to pre-pandemic over all LTLAs") + 
  #geom_vline(xintercept = as.Date(grey_lines), alpha = 1, color = 'darkred', lty = 'dashed') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> time_box

ggsave(filename = "residential_boxplot.png",
       path = 'Case_Outputs', plot = time_box,
       dpi=300, height=9, width=11, units="in")

#We also plot the average value over all time


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
  #filter(areaName == "Halton") %>% #Any week will do
  select(areaCode, areaName, INDEX, residential_percent_change_from_baseline) -> fig_data

agg_fig_data <- fig_data %>%
  group_by(areaCode, areaName, INDEX) %>%
  dplyr::summarize(residential_percent_change_from_baseline = mean(residential_percent_change_from_baseline))

fig_data <- agg_fig_data[order(agg_fig_data$INDEX),]

Boundaries_reduced$residential <- as.numeric(fig_data$residential_percent_change_from_baseline)

variable_vector <- Boundaries_reduced$residential

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = residential), show.legend = FALSE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Mean percent difference in hours spent in residential areas compared to pre-pandemic (averaged over all weeks)") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_residential
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = residential), show.legend = TRUE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  #ggtitle("Average Index of Multiple Deprivation (IMD) by LTLA") +
  labs(fill = "Residential \nVisits") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_residential
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),] %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 



combined_plot <- plot_grid(england_residential, london_residential, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "residential.png",
       path = 'Case_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")

# ggsave(filename = "f2_covariates.tiff",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")
# 
# ggsave(filename = "f2_covariates.pdf",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")

#########################################

#AND NOW VARIANTS!

###########################################
##########################################
# Wild_type
##########################################

#Make a plot of boxplots for each week (so boxplot shows 306 LTLAs)
#Make the data
Plot_data <- data.frame(Time = rep(as.Date("01/01/1999"),(306*T)),
                        time_spent = rep(0,(306*T)))
for(i in 1:T){
  Plot_data$Time[(((i-1)*306)+1):((i)*306)] <- AllDates[i]
  
  reduced_data <- filter(Case_Rates_Data, date_begin == AllDates[i])
  
  Plot_data$time_spent[(((i-1)*306)+1):((i)*306)] <- reduced_data$s_Wild_prop
}


ggplot(Plot_data, aes(x=Time, y=time_spent, group = Time)) + 
  geom_boxplot(alpha = 0.2) + ylab("Percent change") + xlab("Date") +
  #ylim(c(0,plyr::round_any(max(Plot_data$time_spent), 0.02, f=ceiling))) + 
  geom_hline(yintercept = 0, color = "red") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y") + 
  ggtitle("Proportion of cases that are wild-type variant over all LTLAs") + 
  #geom_vline(xintercept = as.Date(grey_lines), alpha = 1, color = 'darkred', lty = 'dashed') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> time_box

ggsave(filename = "s_wildtype_boxplot.png",
       path = 'Case_Outputs', plot = time_box,
       dpi=300, height=9, width=11, units="in")

#We also plot the average value over all time


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
  #filter(areaName == "Halton") %>% #Any week will do
  select(areaCode, areaName, INDEX, s_Wild_prop) -> fig_data

agg_fig_data <- fig_data %>%
  group_by(areaCode, areaName, INDEX) %>%
  dplyr::summarize(s_Wild_prop = mean(s_Wild_prop))

fig_data <- agg_fig_data[order(agg_fig_data$INDEX),]

Boundaries_reduced$s_Wild_prop <- as.numeric(fig_data$s_Wild_prop)

variable_vector <- Boundaries_reduced$s_Wild_prop

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = s_Wild_prop), show.legend = FALSE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Mean proportion of cases that were wild-type variant (averaged over all weeks)") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_s_Wild_prop
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = s_Wild_prop), show.legend = TRUE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  #ggtitle("Average Index of Multiple Deprivation (IMD) by LTLA") +
  labs(fill = "Mean \nWild-type \nProportion") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_s_Wild_prop
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),] %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 



combined_plot <- plot_grid(england_s_Wild_prop, london_s_Wild_prop, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "s_Wild_prop.png",
       path = 'Case_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")

# ggsave(filename = "f2_covariates.tiff",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")
# 
# ggsave(filename = "f2_covariates.pdf",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")

#########################################
###########################################
##########################################
# Alpha type
##########################################

#Make a plot of boxplots for each week (so boxplot shows 306 LTLAs)
#Make the data
Plot_data <- data.frame(Time = rep(as.Date("01/01/1999"),(306*T)),
                        time_spent = rep(0,(306*T)))
for(i in 1:T){
  Plot_data$Time[(((i-1)*306)+1):((i)*306)] <- AllDates[i]
  
  reduced_data <- filter(Case_Rates_Data, date_begin == AllDates[i])
  
  Plot_data$time_spent[(((i-1)*306)+1):((i)*306)] <- reduced_data$s_Alpha_prop
}


ggplot(Plot_data, aes(x=Time, y=time_spent, group = Time)) + 
  geom_boxplot(alpha = 0.2) + ylab("Percent change") + xlab("Date") +
  #ylim(c(0,plyr::round_any(max(Plot_data$time_spent), 0.02, f=ceiling))) + 
  geom_hline(yintercept = 0, color = "red") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y") + 
  ggtitle("Proportion of cases that are Alpha variant over all LTLAs") + 
  #geom_vline(xintercept = as.Date(grey_lines), alpha = 1, color = 'darkred', lty = 'dashed') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> time_box

ggsave(filename = "s_alpha_boxplot.png",
       path = 'Case_Outputs', plot = time_box,
       dpi=300, height=9, width=11, units="in")

#We also plot the average value over all time


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
  #filter(areaName == "Halton") %>% #Any week will do
  select(areaCode, areaName, INDEX, s_Alpha_prop) -> fig_data

agg_fig_data <- fig_data %>%
  group_by(areaCode, areaName, INDEX) %>%
  dplyr::summarize(s_Alpha_prop = mean(s_Alpha_prop))

fig_data <- agg_fig_data[order(agg_fig_data$INDEX),]

Boundaries_reduced$s_Alpha_prop <- as.numeric(fig_data$s_Alpha_prop)

variable_vector <- Boundaries_reduced$s_Alpha_prop

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = s_Alpha_prop), show.legend = FALSE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Mean proportion of cases that were Alpha variant (averaged over all weeks)") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_s_Alpha_prop
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = s_Alpha_prop), show.legend = TRUE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  #ggtitle("Average Index of Multiple Deprivation (IMD) by LTLA") +
  labs(fill = "Mean \nAlpha \nProportion") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_s_Alpha_prop
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),] %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 



combined_plot <- plot_grid(england_s_Alpha_prop, london_s_Alpha_prop, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "s_Alpha_prop.png",
       path = 'Case_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")

# ggsave(filename = "f2_covariates.tiff",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")
# 
# ggsave(filename = "f2_covariates.pdf",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")

#########################################
###########################################
##########################################
# Delta type
##########################################

#Make a plot of boxplots for each week (so boxplot shows 306 LTLAs)
#Make the data
Plot_data <- data.frame(Time = rep(as.Date("01/01/1999"),(306*T)),
                        time_spent = rep(0,(306*T)))
for(i in 1:T){
  Plot_data$Time[(((i-1)*306)+1):((i)*306)] <- AllDates[i]
  
  reduced_data <- filter(Case_Rates_Data, date_begin == AllDates[i])
  
  Plot_data$time_spent[(((i-1)*306)+1):((i)*306)] <- reduced_data$s_Delta_prop
}


ggplot(Plot_data, aes(x=Time, y=time_spent, group = Time)) + 
  geom_boxplot(alpha = 0.2) + ylab("Percent change") + xlab("Date") +
  #ylim(c(0,plyr::round_any(max(Plot_data$time_spent), 0.02, f=ceiling))) + 
  geom_hline(yintercept = 0, color = "red") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y") + 
  ggtitle("Proportion of cases that are Delta variant over all LTLAs") + 
  #geom_vline(xintercept = as.Date(grey_lines), alpha = 1, color = 'darkred', lty = 'dashed') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> time_box

ggsave(filename = "s_delta_boxplot.png",
       path = 'Case_Outputs', plot = time_box,
       dpi=300, height=9, width=11, units="in")

#We also plot the average value over all time


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
  #filter(areaName == "Halton") %>% #Any week will do
  select(areaCode, areaName, INDEX, s_Delta_prop) -> fig_data

agg_fig_data <- fig_data %>%
  group_by(areaCode, areaName, INDEX) %>%
  dplyr::summarize(s_Delta_prop = mean(s_Delta_prop))

fig_data <- agg_fig_data[order(agg_fig_data$INDEX),]

Boundaries_reduced$s_Delta_prop <- as.numeric(fig_data$s_Delta_prop)

variable_vector <- Boundaries_reduced$s_Delta_prop

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = s_Delta_prop), show.legend = FALSE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Mean proportion of cases that were Delta variant (averaged over all weeks)") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_s_Delta_prop
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = s_Delta_prop), show.legend = TRUE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  #ggtitle("Average Index of Multiple Deprivation (IMD) by LTLA") +
  labs(fill = "Mean \nDelta \nProportion") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_s_Delta_prop
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),] %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 



combined_plot <- plot_grid(england_s_Delta_prop, london_s_Delta_prop, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "s_Delta_prop.png",
       path = 'Case_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")

# ggsave(filename = "f2_covariates.tiff",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")
# 
# ggsave(filename = "f2_covariates.pdf",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")

#########################################
###########################################
##########################################
# Omicron type
##########################################

#Make a plot of boxplots for each week (so boxplot shows 306 LTLAs)
#Make the data
Plot_data <- data.frame(Time = rep(as.Date("01/01/1999"),(306*T)),
                        time_spent = rep(0,(306*T)))
for(i in 1:T){
  Plot_data$Time[(((i-1)*306)+1):((i)*306)] <- AllDates[i]
  
  reduced_data <- filter(Case_Rates_Data, date_begin == AllDates[i])
  
  Plot_data$time_spent[(((i-1)*306)+1):((i)*306)] <- reduced_data$s_Omicron_prop
}


ggplot(Plot_data, aes(x=Time, y=time_spent, group = Time)) + 
  geom_boxplot(alpha = 0.2) + ylab("Percent change") + xlab("Date") +
  #ylim(c(0,plyr::round_any(max(Plot_data$time_spent), 0.02, f=ceiling))) + 
  geom_hline(yintercept = 0, color = "red") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y") + 
  ggtitle("Proportion of cases that are Omicron variant over all LTLAs") + 
  #geom_vline(xintercept = as.Date(grey_lines), alpha = 1, color = 'darkred', lty = 'dashed') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> time_box

ggsave(filename = "s_omicron_boxplot.png",
       path = 'Case_Outputs', plot = time_box,
       dpi=300, height=9, width=11, units="in")

#We also plot the average value over all time


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
  #filter(areaName == "Halton") %>% #Any week will do
  select(areaCode, areaName, INDEX, s_Omicron_prop) -> fig_data

agg_fig_data <- fig_data %>%
  group_by(areaCode, areaName, INDEX) %>%
  dplyr::summarize(s_Omicron_prop = mean(s_Omicron_prop))

fig_data <- agg_fig_data[order(agg_fig_data$INDEX),]

Boundaries_reduced$s_Omicron_prop <- as.numeric(fig_data$s_Omicron_prop)

variable_vector <- Boundaries_reduced$s_Omicron_prop

#PLOT standard
ggplot(Boundaries_reduced) +
  geom_sf(aes(fill = s_Omicron_prop), show.legend = FALSE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector)))
  ) +
  ggtitle("Mean proportion of cases that were Omicron variant (averaged over all weeks)") +
  labs(fill = "Over 65s \nProportion") +
  theme_void() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) -> england_s_Omicron_prop
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 

ggplot(Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),]) +
  geom_sf(aes(fill = s_Omicron_prop), show.legend = TRUE) +
  #scale_fill_viridis_c() +
  scale_fill_gradientn(
    colours =  colours_list,  # Blue, white, and red colors
    values = scales::rescale(c(min(variable_vector), median(variable_vector), max(variable_vector))),
    limits = c(min(variable_vector),max(variable_vector))
  ) +
  #ggtitle("Average Index of Multiple Deprivation (IMD) by LTLA") +
  labs(fill = "Mean \nOmicron \nProportion") +
  theme_void() +
  theme(plot.margin = unit(c(0, 1, 0, 0), "cm")) -> london_s_Omicron_prop
# Highlight the two specific regions
#geom_sf(data = Boundaries_reduced[grepl( 'London', Boundaries_reduced$DESCRIPTIO, fixed = TRUE),] %>% filter(CODE %in% c(LTLA_A_data$areaCode, LTLA_B_data$areaCode)),
#        color = "blue", fill = NA, size = 0.8) 



combined_plot <- plot_grid(england_s_Omicron_prop, london_s_Omicron_prop, nrow = 1, rel_widths = c(2.5,1)) +
  theme(plot.background = element_rect(fill = "white", colour = "white"))


ggsave(filename = "s_Omicron_prop.png",
       path = 'Case_Outputs', plot = combined_plot,
       dpi=300, height=9, width=11, units="in")

# ggsave(filename = "f2_covariates.tiff",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")
# 
# ggsave(filename = "f2_covariates.pdf",
#        path = 'Case_Outputs\\fig_demos', plot = Fig2_plot,
#        dpi=300, height=9, width=11, units="in")

#########################################