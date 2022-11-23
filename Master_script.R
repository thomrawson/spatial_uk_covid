#MASTER SPATIAL MODELLING RUN SCRIPT

data_prep <- orderly::orderly_run("00_case_data")
orderly::orderly_commit(data_prep)

#Followed by the actual stanfit task:
model_fit <- orderly::orderly_run("01a_model_fit", 
                                  parameters = list(warmup_iterations = 100,
                                                    total_iterations = 500,
                                                    tree_depth = 13))

orderly::orderly_commit(model_fit)

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
