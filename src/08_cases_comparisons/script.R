#This script is designed to plot the different metrics of daily cases for each
#LTLA to compare

linelist <- readRDS("reduced_linelist.rds")
linelist_LTLAs <- unique(linelist$ltla_code)
linelist_names <-unique(linelist$ltla_name)

cases <- read.csv("ltla_cases_2023_06_22.csv")
cases_LTLAs <- unique(cases$areaCode)
cases_names <- unique(cases$areaName)

#sum(linelist_LTLAs %in% cases_LTLAs)
#linelist_names[which(!(linelist_LTLAs %in% cases_LTLAs))]
#cases_names[which(!(cases_LTLAs %in% linelist_LTLAs))]
#cases_LTLAs[which(!(cases_LTLAs %in% linelist_LTLAs))]

#For simplicity's sake, we'll combine only the ones that exist in both for now.

pillar_2_dashboard <- read.csv("ltla_pillar_2_2023_06_22.csv")
PCR_only_dashboard <- read.csv("ltla_PCR_only_2023_06_22.csv")
LFD_confirmed_PCR_dashboard <- read.csv("ltla_LFD_confirmed_PCR_2023_06_22.csv")
LFD_only_dashboard <- read.csv("ltla_LFD_only_2023_06_22.csv")


#combine all the dashboard data together
dashboard_data <- full_join(cases, pillar_2_dashboard, by=c("areaCode", "areaName", "areaType", "date"))
dashboard_data <- full_join(dashboard_data, PCR_only_dashboard, by=c("areaCode", "areaName", "areaType", "date"))
dashboard_data <- full_join(dashboard_data, LFD_confirmed_PCR_dashboard, by=c("areaCode", "areaName", "areaType", "date"))
dashboard_data <- full_join(dashboard_data, LFD_only_dashboard, by=c("areaCode", "areaName", "areaType", "date"))


dashboard_data$date <- as.Date(dashboard_data$date)

colnames(linelist) <- c("areaCode", "areaName", "date", "Linelist_P2_PCR")
Total_data <- full_join(dashboard_data, linelist, by=c("areaCode", "areaName", "date"))


#Clip off the dates I don't care about
Total_data <- filter(Total_data, date < as.Date("2022-05-01"))
Total_data <- filter(Total_data, date > as.Date("2020-04-01"))


Total_data %>%                                        # Specify data frame
  group_by(date) %>%                         # Specify group indicator
  summarise_at(vars(newCasesBySpecimenDate, newCasesPillarTwoBySpecimenDate,
                    newCasesPCROnlyBySpecimenDate, newCasesLFDConfirmedPCRBySpecimenDate,
                    newCasesLFDOnlyBySpecimenDate, Linelist_P2_PCR),              # Specify column
               sum, na.rm = TRUE) -> UK_totals_data

Total_data %>% 
  filter(grepl('E', areaCode)) %>%
  group_by(date) %>%                         # Specify group indicator
  summarise_at(vars(newCasesBySpecimenDate, newCasesPillarTwoBySpecimenDate,
                    newCasesPCROnlyBySpecimenDate, newCasesLFDConfirmedPCRBySpecimenDate,
                    newCasesLFDOnlyBySpecimenDate, Linelist_P2_PCR),              # Specify column
               sum, na.rm = TRUE) -> England_totals_data
#In general, for the first chunk, Pillar 2 PCR seems to be just a few less than dashboard Pillar 2
#Then, LFTs come in, and instead it looks like, roughly speaking, Dashboard Pillar 2 = linelist_p2_pcr + dashboard LFD only + dashboard LFD confirmed via PCR

#The minor discrepancies are due to mental health hospitals not appearing in the linelist, but are in dashboard


UK_week_totals_data <- UK_totals_data %>% 
  group_by(week_begin = floor_date(date, "weeks", week_start = 1))%>% 
  summarise_if(is.numeric, sum)

colnames(UK_week_totals_data) <- c("week_begin", "Total_Cases", "Total_Pillar2", "PCR_Only", "LFD_confirmed_PCR", "LFD_Only", "Linelist_P2_PCR")

Eng_week_totals_data <- England_totals_data %>% 
  group_by(week_begin = floor_date(date, "weeks", week_start = 1))%>% 
  summarise_if(is.numeric, sum)

colnames(Eng_week_totals_data) <- c("week_begin", "Total_Cases", "Total_Pillar2", "PCR_Only", "LFD_confirmed_PCR", "LFD_Only", "Linelist_P2_PCR")


UK_totals_long <- UK_week_totals_data %>% 
  pivot_longer(
    cols = Total_Cases:Linelist_P2_PCR, 
    names_to = "Data_Source",
    values_to = "Cases"
  )

Eng_totals_long <- Eng_week_totals_data %>% 
  pivot_longer(
    cols = Total_Cases:Linelist_P2_PCR, 
    names_to = "Data_Source",
    values_to = "Cases"
  )

#Change factor order:
UK_totals_long$Data_Source <- factor(UK_totals_long$Data_Source, levels = c("Total_Cases",
                                                                            "Total_Pillar2",
                                                                            "Linelist_P2_PCR",
                                                                            "PCR_Only",
                                                                            "LFD_confirmed_PCR",
                                                                            "LFD_Only"))

Eng_totals_long$Data_Source <- factor(Eng_totals_long$Data_Source, levels = c("Total_Cases",
                                                                            "Total_Pillar2",
                                                                            "Linelist_P2_PCR",
                                                                            "PCR_Only",
                                                                            "LFD_confirmed_PCR",
                                                                            "LFD_Only"))

UK_totals_long$linetype <- ifelse(UK_totals_long$Data_Source == "Linelist_P2_PCR", "--", "-")

Eng_totals_long$linetype <- ifelse(Eng_totals_long$Data_Source == "Linelist_P2_PCR", "--", "-")


ggplot(UK_totals_long) +
  geom_line(aes(x = week_begin, y = Cases, color = Data_Source, lty = linetype), size = 1, alpha = 0.5) +
  theme_minimal() + ggtitle("UK COVID-19 Cases by data source") + guides(lty = "none") -> UK_totals_plot

ggplot(Eng_totals_long) +
  geom_line(aes(x = week_begin, y = Cases, color = Data_Source, lty = linetype), size = 1, alpha = 0.5) +
  theme_minimal() + ggtitle("England COVID-19 Cases by data source") + guides(lty = "none") -> Eng_totals_plot

#Also a reduced version with only these types:
data_filter <- c("Total_Cases", "Total_Pillar2", "Linelist_P2_PCR")
data_filter2 <- c("Total_Cases", "Linelist_P2_PCR")

ggplot(filter(UK_totals_long, Data_Source %in% data_filter)) +
  geom_line(aes(x = week_begin, y = Cases, color = Data_Source, lty = linetype), size = 1, alpha = 0.5) +
  theme_minimal() + ggtitle("UK COVID-19 Cases by data source") + guides(lty = "none") -> UK_totals_plot_2

ggplot(filter(Eng_totals_long, Data_Source %in% data_filter)) +
  geom_line(aes(x = week_begin, y = Cases, color = Data_Source, lty = linetype), size = 1, alpha = 0.5) +
  theme_minimal() + ggtitle("England COVID-19 cases by data source") + guides(lty = "none") -> Eng_totals_plot_2

ggplot(filter(Eng_totals_long, Data_Source %in% data_filter2)) +
  geom_line(aes(x = week_begin, y = Cases/1000, color = Data_Source), size = 1.2, alpha = 0.8) +
  theme_bw() + ggtitle("England COVID-19 Cases by data source") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %y") +
  xlab("Date") + ylab("Cases (thousands)") +
  theme(axis.text.y = element_text(size = rel(1.2)),
        axis.title.y = element_text(size = rel(1.3)),
        legend.text = element_text(size = rel(1.2)),
        legend.title = element_text(size = rel(1.3))) +
  scale_color_manual(labels = c("Dashboard Cases",
                                "Line List Cases"), 
                     values=c( "#17bebb", "#e4572e")) +
  labs(color = "Data Source") -> Eng_totals_plot_3


dir.create("Outputs")
ggsave("Outputs/UK_cases.png", UK_totals_plot, bg = "white", width = 10, height = 6)
ggsave("Outputs/Eng_cases.png", Eng_totals_plot, bg = "white", width = 10, height = 6)
ggsave("Outputs/UK_cases_reduced.png", UK_totals_plot_2, bg = "white", width = 10, height = 6)
ggsave("Outputs/Eng_cases_reduced.png", Eng_totals_plot_2, bg = "white", width = 10, height = 6)
ggsave("Outputs/Eng_cases_reduced2.png", Eng_totals_plot_3, bg = "white", width = 10, height = 6)

#Now, we repeat, but for every LTLA
dir.create("Outputs/LTLA_plots")

all_LTLAs <- unique(Total_data$areaCode)
all_LTLA_names <- unique(Total_data$areaName)

for(i in 1:length(all_LTLAs)){
  LTLA_hold <- all_LTLAs[i]
  name_hold <- all_LTLA_names[i]
  
  data_hold <- filter(Total_data, areaCode == LTLA_hold)
  
  data_hold <- data_hold %>% 
    group_by(week_begin = floor_date(date, "weeks", week_start = 1))%>% 
    summarise_if(is.numeric, sum)
  
  colnames(data_hold) <- c("week_begin", "Total_Cases", "Total_Pillar2", "PCR_Only", "LFD_confirmed_PCR", "LFD_Only", "Linelist_P2_PCR")
  
  data_hold <- data_hold %>% 
    pivot_longer(
      cols = Total_Cases:Linelist_P2_PCR, 
      names_to = "Data_Source",
      values_to = "Cases"
    )
  
  #Change factor order:
  data_hold$Data_Source <- factor(data_hold$Data_Source, levels = c("Total_Cases",
                                                                              "Total_Pillar2",
                                                                              "Linelist_P2_PCR",
                                                                              "PCR_Only",
                                                                              "LFD_confirmed_PCR",
                                                                              "LFD_Only"))
  
  data_hold$linetype <- ifelse(data_hold$Data_Source == "Linelist_P2_PCR", "--", "-")
  
  ggplot(data_hold) +
    geom_line(aes(x = week_begin, y = Cases, color = Data_Source, lty = linetype), size = 1, alpha = 0.5) +
    theme_minimal() + ggtitle(sprintf("%s - %s", LTLA_hold, name_hold)) + guides(lty = "none") -> plot_hold
  
  ggsave(sprintf("Outputs/LTLA_plots/%s_%s.png", LTLA_hold, name_hold), plot_hold, bg = "white", width = 10, height = 6)
  
  
}
