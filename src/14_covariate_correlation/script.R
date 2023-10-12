#orderly::orderly_develop_start("14_covariate_correlation", use_draft = "newer", parameters = list(tree_depth = 15, total_iterations = 4000, n_chains = 16))

#This script loads in the Case Rates data object and investigates correlation between covariates
# The end goal is to know which covariates to include an interaction effect term for.
load("model_data.RData")
dir.create("Outputs")

Case_Rates_Data$financial_year <- if_else(Case_Rates_Data$date_begin < as.Date("2021-04-06"), "2020/21", "2021/22")

GGally::ggpairs(Case_Rates_Data,
                columns = c("IMD_Average_score", "residential_percent_change_from_baseline", "unringfenced"),
                title = "Correlation between the 16 model covariates analysed by financial year",
                upper = list(continuous = wrap("cor", size = 8)),
                aes(color = financial_year,  # Color by group (cat. variable)
                    alpha = 0.5),
                columnLabels = c("IMD", "Residential Movement", "Unringfenced funding")
                ) + theme_bw() + theme(panel.grid.major = element_blank(),
                                       panel.grid.minor = element_blank())

#In general, as IMD increases, residential mobility decreases (corr: -0.2) 
#i.e. more deprived areas didn't actually stay at home as much
Case_Rates_Data$prop_white <- Case_Rates_Data$prop_white_british + Case_Rates_Data$prop_all_other_white

GGally::ggpairs(Case_Rates_Data, 
                       columns = c(#"prop_asian", "prop_black_afr_car",
                                   #"prop_other",
                         "prop_white",
                       "IMD_Average_score", "prop_o65", "Pop_per_km2",
                       "Median_annual_income",
                       "workplaces_percent_change_from_baseline", 
                       "transit_stations_percent_change_from_baseline",
                       "residential_percent_change_from_baseline",
                       #"s_Alpha_prop",
                       #"s_Delta_prop",
                       #"s_Omicron_prop",
                       "unringfenced", "contain_outbreak_management",
                       "ASC_infection_control_fund"),
                title = "Correlation between the 16 model covariates analysed by financial year",
                upper = list(continuous = wrap("cor", size = 6)),
                aes(color = financial_year,  # Color by group (cat. variable)
                    alpha = 0.5),
                columnLabels = c(#"Asian prop.", "Black/Afr./Car. prop.",
                                 #"Other eth. prop.",
                                  "White prop.",
                                 "IMD score", "Prop. over 65", "Pop. density",
                                 "Median income",
                                 "workplaces visits", 
                                 "transit station visits",
                                 "residential duration",
                                 #"Prop. Alpha",
                                 #"Prop. Delta",
                                 #"Prop. Omicron",
                                 "Unringfenced funding", "COMF funding",
                                 "ASC funding")
                ) + theme_bw() + theme(panel.grid.major = element_blank(),
                                       panel.grid.minor = element_blank()) -> ggmatrix

ggsave(filename = "Case_rates_ggmatrix.png",
       path = 'Outputs', plot = ggmatrix,
       dpi=300, height=12, width=16, units="in")

#NOW DO FOR JUST THE FOUR ETHNICITIES SEPARATELY


#Let's do some other style of plots

# packages = c("ggplot2", "dplyr", "grid", "ggthemes","plotly", "forcats",
#              "reshape2", "corrplot","ggcorrplot","Hmisc")
# package.check <- lapply(packages, FUN = function(x) {
#   if (!require(x, character.only = TRUE)) {
#     install.packages(x, dependencies = TRUE)
#     library(x, character.only = TRUE)
#   }
# })

corr <- select(Case_Rates_Data, 
               Population,
               prop_asian, prop_black_afr_car, prop_other,
               IMD_Average_score, prop_o65, Pop_per_km2,
               Median_annual_income,
               workplaces_percent_change_from_baseline, residential_percent_change_from_baseline,
               transit_stations_percent_change_from_baseline,
               s_Alpha_prop, s_Delta_prop, s_Omicron_prop,
               unringfenced, contain_outbreak_management, ASC_infection_control_fund)

corr$unringfenced <- corr$unringfenced/corr$Population
corr$contain_outbreak_management <- corr$contain_outbreak_management/corr$Population
corr$ASC_infection_control_fund <- corr$ASC_infection_control_fund/corr$Population
corr <- corr[,-1]

#Use library Hmisc to get correlation cofficients and p-matrix
#Correlation methods: (“pearson”, “kendall”, “spearman”)

# Lets create function to perform correlations
mycorr<-function (data, type){
  result<-Hmisc::rcorr(as.matrix(data), type=type)
  result$r
}
# Get the correlation cofficents
# use function to get cor.cofficients, rounding results by two digits after point and
# droping column first from corr data file as it contains names of the lines
corr_mat_pe<-round(mycorr(corr, type="pearson"),2) 
corr_mat_sp<-round(mycorr(corr, type="spearman"),2)
head(corr_mat_pe[, 1:4])
head (corr_mat_sp[, 1:4])

#now get matrix of p-values using cor_pmat function from package corrplot package
pmat_pe <-  ggcorrplot::cor_pmat(corr_mat_pe)
head(pmat_pe[, 1:4])

pmat_sp <-  ggcorrplot::cor_pmat(corr_mat_sp)
head(pmat_sp[, 1:4]) 

#Visualize the correlation matrix through heatmaps in ggcorrplot package

myggcorr<-function(cor_cof, p.mat){
  ggcorrplot::ggcorrplot(cor_cof, method="circle",hc.order = FALSE,outline.col = "blue", 
             type="lower", lab=TRUE,  p.mat =p.mat, insig = "blank",pch = 4, 
             pch.col="black", pch.cex = 5,
             show.diag = FALSE, lab_col = "black", lab_size = 2, sig.level =c(0.1,0.05,0.01),
             tl.cex=10, tl.col="black", tl.srt=45, digits=2)
}

# Now plot correlation heatmap method pearson using function myggcorr
myggcorr(cor_cof=corr_mat_pe, p.mat=pmat_pe) -> pmat_pe_plot
pmat_pe_plot + ggtitle("Significant correlation via Pearson's rank") +
  theme(plot.background = element_rect(fill = "white"))-> pmat_pe_plot

ggsave(filename = "pmat_pe.png",
       path = 'Outputs', plot = pmat_pe_plot,
       dpi=300, height=9, width=12, units="in")

# Now plot correlation heat map method spearman
myggcorr(cor_cof=corr_mat_sp, p.mat=pmat_sp) -> pmat_sp_plot
pmat_sp_plot + ggtitle("Significant correlation via Spearman") +
  theme(plot.background = element_rect(fill = "white"))-> pmat_sp_plot

ggsave(filename = "pmat_sp.png",
       path = 'Outputs', plot = pmat_sp_plot,
       dpi=300, height=9, width=12, units="in")
