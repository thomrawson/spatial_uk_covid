#This script loads in the stanfit object and outputs some demos of paper figures to play around with
load("stanfit.RData")
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
  scale_by_susceptible_pool: %s \n
  cases_type: %s \n
  use_SGTF_data: %s \n
  final_week: %s \n
  random_walk_prior_scale: %s \n
  rw_penalty: %s \n
  print_extra_gof:  %s ", tree_depth, n_chains, 
                        scale_by_susceptible_pool, cases_type,
                         use_SGTF_data, final_week,
                        random_walk_prior_scale, rw_penalty, print_extra_gof)

fileConn<-file("parameters_used.txt")
writeLines(param_string, fileConn)
close(fileConn)



