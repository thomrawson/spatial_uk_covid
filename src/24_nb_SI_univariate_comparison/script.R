#orderly::orderly_develop_start("24_nb_SI_univariate_comparison")

dir.create("Case_Outputs")


################################################################################
################################################################################
#load in our main analysis
df_hold <- read.csv("Data/main_summaries_ALL.csv")
df_hold <- df_hold[c(3:18),c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")


#Format it into the way we want for the eventual plot
Plot_data <- data.frame(Variable = c("1) Proportion Asian", "2) Proportion Black Afr/Car",
                                     "3) Proportion Other Ethnicity", "4) IMD Average Score",
                                     "5) Proportion Over Age 65", "6) Population per km^2",
                                     "7) Median Annual Income", "8) Time At Workplace",
                                     "9) Time At Home", "10) Time At Transit Stations",
                                     "11) Alpha Proportion", "12) Delta Proportion", 
                                     "13) Omicron Proportion", "14) Unringfenced",
                                     "15) Outbreak Management", "16) ASC infection control"),
                        model_type = rep("Multivariate", 16),
                        mean = df_hold$mean,
                        lower = df_hold$lower,
                        upper = df_hold$upper)

############################################################################
#Now load in and add the univariates to the main df
new_variable_name <- "1) Proportion Asian"

df_hold <- read.csv("Data/main_summaries_01_prop_asian.csv")
df_hold <- df_hold[3,c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")

new_row <- data.frame(Variable = new_variable_name,
                      model_type = "Univariate",
                      mean = df_hold$mean,
                      lower = df_hold$lower,
                      upper = df_hold$upper)

Plot_data <- rbind(Plot_data, new_row)

##########################################################################
############################################################################
#Now load in and add the univariates to the main df
new_variable_name <- "2) Proportion Black Afr/Car"

df_hold <- read.csv("Data/main_summaries_02_black_afr_car.csv")
df_hold <- df_hold[3,c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")

new_row <- data.frame(Variable = new_variable_name,
                      model_type = "Univariate",
                      mean = df_hold$mean,
                      lower = df_hold$lower,
                      upper = df_hold$upper)

Plot_data <- rbind(Plot_data, new_row)

##########################################################################
############################################################################
#Now load in and add the univariates to the main df
new_variable_name <- "3) Proportion Other Ethnicity"

df_hold <- read.csv("Data/main_summaries_03_prop_other.csv")
df_hold <- df_hold[3,c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")

new_row <- data.frame(Variable = new_variable_name,
                      model_type = "Univariate",
                      mean = df_hold$mean,
                      lower = df_hold$lower,
                      upper = df_hold$upper)

Plot_data <- rbind(Plot_data, new_row)

##########################################################################
############################################################################
#Now load in and add the univariates to the main df
new_variable_name <- "4) IMD Average Score"

df_hold <- read.csv("Data/main_summaries_04_IMD.csv")
df_hold <- df_hold[3,c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")

new_row <- data.frame(Variable = new_variable_name,
                      model_type = "Univariate",
                      mean = df_hold$mean,
                      lower = df_hold$lower,
                      upper = df_hold$upper)

Plot_data <- rbind(Plot_data, new_row)

##########################################################################
############################################################################
#Now load in and add the univariates to the main df
new_variable_name <- "5) Proportion Over Age 65"

df_hold <- read.csv("Data/main_summaries_05_prop_o65.csv")
df_hold <- df_hold[3,c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")

new_row <- data.frame(Variable = new_variable_name,
                      model_type = "Univariate",
                      mean = df_hold$mean,
                      lower = df_hold$lower,
                      upper = df_hold$upper)

Plot_data <- rbind(Plot_data, new_row)

##########################################################################
############################################################################
#Now load in and add the univariates to the main df
new_variable_name <- "6) Population per km^2"

df_hold <- read.csv("Data/main_summaries_06_Pop_per_km2.csv")
df_hold <- df_hold[3,c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")

new_row <- data.frame(Variable = new_variable_name,
                      model_type = "Univariate",
                      mean = df_hold$mean,
                      lower = df_hold$lower,
                      upper = df_hold$upper)

Plot_data <- rbind(Plot_data, new_row)

##########################################################################
############################################################################
#Now load in and add the univariates to the main df
new_variable_name <- "7) Median Annual Income"

df_hold <- read.csv("Data/main_summaries_07_Median_annual_income.csv")
df_hold <- df_hold[3,c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")

new_row <- data.frame(Variable = new_variable_name,
                      model_type = "Univariate",
                      mean = df_hold$mean,
                      lower = df_hold$lower,
                      upper = df_hold$upper)

Plot_data <- rbind(Plot_data, new_row)

##########################################################################
############################################################################
#Now load in and add the univariates to the main df
new_variable_name <- "8) Time At Workplace"

df_hold <- read.csv("Data/main_summaries_08_workplaces.csv")
df_hold <- df_hold[3,c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")

new_row <- data.frame(Variable = new_variable_name,
                      model_type = "Univariate",
                      mean = df_hold$mean,
                      lower = df_hold$lower,
                      upper = df_hold$upper)

Plot_data <- rbind(Plot_data, new_row)

##########################################################################
############################################################################
#Now load in and add the univariates to the main df
new_variable_name <- "9) Time At Home"

df_hold <- read.csv("Data/main_summaries_09_residential.csv")
df_hold <- df_hold[3,c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")

new_row <- data.frame(Variable = new_variable_name,
                      model_type = "Univariate",
                      mean = df_hold$mean,
                      lower = df_hold$lower,
                      upper = df_hold$upper)

Plot_data <- rbind(Plot_data, new_row)

##########################################################################
############################################################################
#Now load in and add the univariates to the main df
new_variable_name <- "10) Time At Transit Stations"

df_hold <- read.csv("Data/main_summaries_10_transit.csv")
df_hold <- df_hold[3,c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")

new_row <- data.frame(Variable = new_variable_name,
                      model_type = "Univariate",
                      mean = df_hold$mean,
                      lower = df_hold$lower,
                      upper = df_hold$upper)

Plot_data <- rbind(Plot_data, new_row)

##########################################################################
############################################################################
#Now load in and add the univariates to the main df
new_variable_name <- "11) Alpha Proportion"

df_hold <- read.csv("Data/main_summaries_11_Alpha.csv")
df_hold <- df_hold[3,c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")

new_row <- data.frame(Variable = new_variable_name,
                      model_type = "Univariate",
                      mean = df_hold$mean,
                      lower = df_hold$lower,
                      upper = df_hold$upper)

Plot_data <- rbind(Plot_data, new_row)

##########################################################################
############################################################################
#Now load in and add the univariates to the main df
new_variable_name <- "12) Delta Proportion"

df_hold <- read.csv("Data/main_summaries_12_Delta.csv")
df_hold <- df_hold[3,c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")

new_row <- data.frame(Variable = new_variable_name,
                      model_type = "Univariate",
                      mean = df_hold$mean,
                      lower = df_hold$lower,
                      upper = df_hold$upper)

Plot_data <- rbind(Plot_data, new_row)

##########################################################################
############################################################################
#Now load in and add the univariates to the main df
new_variable_name <- "13) Omicron Proportion"

df_hold <- read.csv("Data/main_summaries_13_Omicron.csv")
df_hold <- df_hold[3,c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")

new_row <- data.frame(Variable = new_variable_name,
                      model_type = "Univariate",
                      mean = df_hold$mean,
                      lower = df_hold$lower,
                      upper = df_hold$upper)

Plot_data <- rbind(Plot_data, new_row)

##########################################################################
############################################################################
#Now load in and add the univariates to the main df
new_variable_name <- "14) Unringfenced"

df_hold <- read.csv("Data/main_summaries_14_unringfenced.csv")
df_hold <- df_hold[3,c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")

new_row <- data.frame(Variable = new_variable_name,
                      model_type = "Univariate",
                      mean = df_hold$mean,
                      lower = df_hold$lower,
                      upper = df_hold$upper)

Plot_data <- rbind(Plot_data, new_row)

##########################################################################
############################################################################
#Now load in and add the univariates to the main df
new_variable_name <- "15) Outbreak Management"

df_hold <- read.csv("Data/main_summaries_15_COMF.csv")
df_hold <- df_hold[3,c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")

new_row <- data.frame(Variable = new_variable_name,
                      model_type = "Univariate",
                      mean = df_hold$mean,
                      lower = df_hold$lower,
                      upper = df_hold$upper)

Plot_data <- rbind(Plot_data, new_row)

##########################################################################
############################################################################
#Now load in and add the univariates to the main df
new_variable_name <- "16) ASC infection control"

df_hold <- read.csv("Data/main_summaries_16_ASC.csv")
df_hold <- df_hold[3,c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")

new_row <- data.frame(Variable = new_variable_name,
                      model_type = "Univariate",
                      mean = df_hold$mean,
                      lower = df_hold$lower,
                      upper = df_hold$upper)

Plot_data <- rbind(Plot_data, new_row)

##########################################################################

#Now, let's ignore the variants because, of course, they're a bit different.
Plot_data <- filter(Plot_data, !(Variable %in% c("11) Alpha Proportion", "12) Delta Proportion", 
                                                 "13) Omicron Proportion")))

Plot_data$Variable <- factor(Plot_data$Variable, levels = rev(unique(Plot_data$Variable)))


ggplot(Plot_data) +
  geom_vline(xintercept = 0, color = "grey", lty = 5, size = 1) +
  geom_errorbar(aes(x = 0, xmin = lower, xmax = upper, y = Variable, color = model_type), size = 1, position = position_dodge(width = 0.5)) +
  geom_point(aes(x = mean, y = Variable, color = model_type), size = 2.5, position = position_dodge(width = 0.5)) +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.2)),
        legend.title = element_text(size = rel(1.3))) +
  ggtitle("Comparing multivariate and univariate model output") +
  xlab("Beta coefficient value") +
  labs(color = "Model type") +
  scale_color_manual(values=c( "#17bebb", "#e4572e")) -> fig_1
  
ggsave(filename = "multi_uni_comp1.png",
       path = 'Case_Outputs', plot = fig_1,
       dpi=300, height=6, width=9, units="in")

#Arguably best to leave out funding if you're not also including variant

#Now, let's ignore the variants because, of course, they're a bit different.
Plot_data <- filter(Plot_data, !(Variable %in% c("11) Alpha Proportion", "12) Delta Proportion", 
                                                 "13) Omicron Proportion", "14) Unringfenced",
                                                 "15) Outbreak Management", "16) ASC infection control")))

Plot_data$Variable <- factor(Plot_data$Variable, levels = rev(unique(Plot_data$Variable)))


ggplot(Plot_data) +
  geom_vline(xintercept = 0, color = "grey", lty = 5, size = 1) +
  geom_errorbar(aes(x = 0, xmin = lower, xmax = upper, y = Variable, color = model_type), size = 1, position = position_dodge(width = 0.5)) +
  geom_point(aes(x = mean, y = Variable, color = model_type), size = 2.5, position = position_dodge(width = 0.5)) +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.2)),
        legend.title = element_text(size = rel(1.3))) +
  ggtitle("Comparing multivariate and univariate model output") +
  xlab("Beta coefficient value") +
  labs(color = "Model type") +
  scale_color_manual(values=c( "#17bebb", "#e4572e")) -> fig_2

ggsave(filename = "multi_uni_comp2.png",
       path = 'Case_Outputs', plot = fig_2,
       dpi=300, height=6, width=9, units="in")


#####################################################################
#####################################################################

#Now we compare different data streams;
#swapping the cases from PCR only linelist to pillar 1 + pillar 2 and LFD
#Changing SGTF defined variant to dashboard VAM data
#Or both!
#load in our main analysis
df_hold <- read.csv("Data/main_summaries_ALL.csv")
df_hold <- df_hold[c(3:18),c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")


#Format it into the way we want for the eventual plot
Plot_data <- data.frame(Variable = c("1) Proportion Asian", "2) Proportion Black Afr/Car",
                                     "3) Proportion Other Ethnicity", "4) IMD Average Score",
                                     "5) Proportion Over Age 65", "6) Population per km^2",
                                     "7) Median Annual Income", "8) Time At Workplace",
                                     "9) Time At Home", "10) Time At Transit Stations",
                                     "11) Alpha Proportion", "12) Delta Proportion", 
                                     "13) Omicron Proportion", "14) Unringfenced",
                                     "15) Outbreak Management", "16) ASC infection control"),
                        model_type = rep("Main Model", 16),
                        mean = df_hold$mean,
                        lower = df_hold$lower,
                        upper = df_hold$upper)

#Load in the different cases
df_hold <- read.csv("Data/main_summaries_dashboard_cases.csv")
df_hold <- df_hold[c(3:18),c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")
Hold_data <- data.frame(Variable = c("1) Proportion Asian", "2) Proportion Black Afr/Car",
                                     "3) Proportion Other Ethnicity", "4) IMD Average Score",
                                     "5) Proportion Over Age 65", "6) Population per km^2",
                                     "7) Median Annual Income", "8) Time At Workplace",
                                     "9) Time At Home", "10) Time At Transit Stations",
                                     "11) Alpha Proportion", "12) Delta Proportion", 
                                     "13) Omicron Proportion", "14) Unringfenced",
                                     "15) Outbreak Management", "16) ASC infection control"),
                        model_type = rep("Dashboard Cases", 16),
                        mean = df_hold$mean,
                        lower = df_hold$lower,
                        upper = df_hold$upper)

Plot_data <- rbind(Plot_data, Hold_data)

#Load in the different variants
df_hold <- read.csv("Data/main_summaries_dashboard_variant.csv")
df_hold <- df_hold[c(3:18),c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")
Hold_data <- data.frame(Variable = c("1) Proportion Asian", "2) Proportion Black Afr/Car",
                                     "3) Proportion Other Ethnicity", "4) IMD Average Score",
                                     "5) Proportion Over Age 65", "6) Population per km^2",
                                     "7) Median Annual Income", "8) Time At Workplace",
                                     "9) Time At Home", "10) Time At Transit Stations",
                                     "11) Alpha Proportion", "12) Delta Proportion", 
                                     "13) Omicron Proportion", "14) Unringfenced",
                                     "15) Outbreak Management", "16) ASC infection control"),
                        model_type = rep("Dashboard Variants", 16),
                        mean = df_hold$mean,
                        lower = df_hold$lower,
                        upper = df_hold$upper)

Plot_data <- rbind(Plot_data, Hold_data)

#Load in the different variants
df_hold <- read.csv("Data/main_summaries_dashboard_cases_AND_variant.csv")
df_hold <- df_hold[c(3:18),c(1,2,5,9)]
colnames(df_hold) <- c("Variable", "mean", "lower", "upper")
Hold_data <- data.frame(Variable = c("1) Proportion Asian", "2) Proportion Black Afr/Car",
                                     "3) Proportion Other Ethnicity", "4) IMD Average Score",
                                     "5) Proportion Over Age 65", "6) Population per km^2",
                                     "7) Median Annual Income", "8) Time At Workplace",
                                     "9) Time At Home", "10) Time At Transit Stations",
                                     "11) Alpha Proportion", "12) Delta Proportion", 
                                     "13) Omicron Proportion", "14) Unringfenced",
                                     "15) Outbreak Management", "16) ASC infection control"),
                        model_type = rep("Dashboard Cases \nand Variants", 16),
                        mean = df_hold$mean,
                        lower = df_hold$lower,
                        upper = df_hold$upper)

Plot_data <- rbind(Plot_data, Hold_data)

Plot_data$Variable <- factor(Plot_data$Variable, levels = rev(unique(Plot_data$Variable)))


ggplot(Plot_data) +
  geom_vline(xintercept = 0, color = "grey", lty = 5, size = 1) +
  geom_errorbar(aes(x = 0, xmin = lower, xmax = upper, y = Variable, color = model_type), size = 1, position = position_dodge(width = 0.6)) +
  geom_point(aes(x = mean, y = Variable, color = model_type), size = 2.5, position = position_dodge(width = 0.6)) +
  theme_bw() +
  theme(axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.2)),
        legend.title = element_text(size = rel(1.3)),
        legend.spacing.y = unit(0.3, 'cm')) +
  ggtitle("Comparing model output for different data streams") +
  xlab("Beta coefficient value") +
  labs(color = "Data sources") +
  scale_color_manual(breaks = c("Main Model",
                                "Dashboard Cases",
                                "Dashboard Variants",
                                "Dashboard Cases \nand Variants"), 
                     values=c( "#17bebb", "#e4572e", "#00C853", "#FFB300")) +
  #theme(legend.spacing.y = unit(1.0, 'cm')) +
  guides(color= guide_legend(byrow = TRUE)) -> data_streams_plot

ggsave(filename = "data_streams_plot.png",
       path = 'Case_Outputs', plot = data_streams_plot,
       dpi=300, height=9, width=10, units="in")
