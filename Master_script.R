#MASTER SPATIAL MODELLING RUN SCRIPT

data_prep <- orderly::orderly_run("00_case_data")
orderly::orderly_commit(data_prep)

#Followed by the actual stanfit task:
model_fit <- orderly::orderly_run("01a_model_fit", 
                                  parameters = list(warmup_iterations = 100,
                                                    total_iterations = 250))
orderly::orderly_commit(model_fit)