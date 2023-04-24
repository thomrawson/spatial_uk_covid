#MASTER SPATIAL MODELLING RUN SCRIPT

data_prep <- orderly::orderly_run("00_case_data")
orderly::orderly_commit(data_prep)

#Followed by the actual stanfit task:

#Define the covariates you want to include in the model.
#Options are:

#Population
#pop_per_km2
#Median_age
#VaccPercentage_FirstDose
#VaccPercentage_SecondDose
#VaccPercentage_ThirdDose
#prop_whitebritish (ADD MORE OF THESE)
#IMD_Average_score
#IMD_Rank_of_average_score
#mean_age
#prop_over65
#mean_popden
#Median_annual_income
#no_jobs
#retail_and_recreation_mobility
#grocery_and_pharmacy_mobility
#parks_mobility
#transit_stations_mobility
#workplace_mobility
#residential_mobility
#Alpha_proportion
#Delta_proportion
#Delta_AY_4_2_proportion
#Omicron_BA_1_proportion
#Omicron_BA_2_proportion
#Other_variants_proportion
#Omicron_BQ_1_proportion
#Omicron_BA_4_proportion
#Omicron_BA_5_proportion

####################
#I use "default" = 



model_fit <- orderly::orderly_run("01a_model_fit", 
                                  parameters = list(warmup_iterations = 200,
                                                    total_iterations = 1000,
                                                    tree_depth = 8,
                                                    covariates = "default",
                                                    scale_by_number_of_neighbours = TRUE,
                                                    scale_by_susceptible_pool = FALSE,
                                                    spatial_kernel = "neighbours",
                                                    algorithm = "NUTS"))  #HMC or "NUTS" (default)

orderly::orderly_commit(model_fit)

#develop the code
orderly::orderly_develop_start("01a_model_fit", 
                     parameters = list(warmup_iterations = 200,
                                       total_iterations = 1000,
                                       tree_depth = 8,
                                       covariates = "default",
                                       scale_by_number_of_neighbours = FALSE,
                                       algorithm = "NUTS"))  #HMC or "NUTS" (default)

orderly::orderly_develop_clean()


#Model without hospitalisations
model_fit_no_hosp <- orderly::orderly_run("01b_model_fit_no_hospital", 
                                  parameters = list(warmup_iterations = 100,
                                                    total_iterations = 200,
                                                    tree_depth = 6,
                                                    covariates = "default",
                                                    scale_by_number_of_neighbours = FALSE,
                                                    scale_by_susceptible_pool = FALSE,
                                                    spatial_kernel = "gravity",
                                                    algorithm = "NUTS"))  #HMC or "NUTS" (default)

orderly::orderly_commit(model_fit_no_hosp)

#200/1000: 8 td : 35mins PC NUTS
#200/1000: 8 td : bloody ages PC HMC

#100/500 8 tree_depth = 20mins on laptop The largest R-hat is 4.41
#200/1000 8 tree_depth = 60mins on laptop. The largest R-hat is 4.54 #20221129-182400-edab68de

#100/500 : 9 tree depth : 30mins PC The largest R-hat is 3.63 #20221129-162746-530dca60
#200/1000: 9 td : 90mins PC The largest R-hat is 4.53, indicating chains have not mixed #20221129-185913-9aa3d73d

#200/1000 : 12 td: 8 hours PC The largest R-hat is 3.14 #20221129-233450-5e332b7d


#RUNTIMEs
#First run, 500iteration, 15 tree_depth, 43 hours
#First run, 500iteration, 10 tree_depth, 1.5 hours, as expected, this is faster by factor of ~2^5



#Reduced to just one theta
#500iteration, 10 tree_depth, 0.7 hours

#Reduced to 306 thetas
#500iteration, 12 tree_depth,  2.8 hours, and warnings about tree depth:
#Warning messages:
# 1: In system(paste(CXX, ARGS), ignore.stdout = TRUE, ignore.stderr = TRUE) :
#   'C:/rtools40/usr/mingw_/bin/g++' not found
# 2: There were 1053 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 12. See
# http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded 
# 3: There were 4 chains where the estimated Bayesian Fraction of Missing Information was low. See
# http://mc-stan.org/misc/warnings.html#bfmi-low 
# 4: Examine the pairs() plot to diagnose sampling problems
# 
# 5: The largest R-hat is 2.36, indicating chains have not mixed.
# Running the chains for more iterations may help. See
# http://mc-stan.org/misc/warnings.html#r-hat 
# 6: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
# Running the chains for more iterations may help. See
# http://mc-stan.org/misc/warnings.html#bulk-ess 
# 7: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
# Running the chains for more iterations may help. See
# http://mc-stan.org/misc/warnings.html#tail-ess 

#I think it should really be just thetas for each location


###############
# 02 - Assessing the model fits
model_assessment <- orderly::orderly_run("02a_model_summaries",
                                         parameters = list(tree_depth = 14,
                                                           scale_by_susceptible_pool = TRUE,
                                                           spatial_kernel = "neighbours"),
                                         use_draft = "newer")


orderly::orderly_commit(model_assessment)


#Develop
orderly::orderly_develop_start("02a_model_summaries",
                     parameters = list(tree_depth = 14,
                                       scale_by_susceptible_pool = FALSE,
                                       spatial_kernel = "neighbours"),
                     use_draft = "newer")

model_assessment <- orderly::orderly_run("02b_model_summaries_no_hospital",
                                         parameters = list(tree_depth = 14,
                                                           scale_by_susceptible_pool = FALSE,
                                                           spatial_kernel = "neighbours"),
                                         use_draft = "newer")


orderly::orderly_commit(model_assessment)
