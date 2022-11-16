#MASTER SPATIAL MODELLING RUN SCRIPT

data_prep <- orderly::orderly_run("00_case_data")
orderly::orderly_commit(data_prep)

#Followed by the actual stanfit task:
model_fit <- orderly::orderly_run("01a_model_fit", 
                                  parameters = list(warmup_iterations = 100,
                                                    total_iterations = 500,
                                                    tree_depth = 12))
orderly::orderly_commit(model_fit)

#RUNTIMEs
#First run, 500iteration, 15 tree_depth, 43 hours
#First run, 500iteration, 10 tree_depth, 1.5 hours, as expected, this is faster by factor of ~2^5

#Reduced to just one theta
#500iteration, 10 tree_depth, 0.7 hours

#Reduced to 306 thetas
#500iteration, 12 tree_depth,  hours

#I think it should really be just thetas for each location
