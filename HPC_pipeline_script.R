#Run the pipeline on the HPC

#MODEL FIT
rm(list=ls())

packages_list <- c("tidyverse", "rstan", "orderly", "BH",
                   "StanHeaders", "RcppEigen")

options(didehpc.cluster = "fi--didemrchnb",
        didehpc.template = "GeneralNodes",
        didehpc.cores = 4,
        didehpc.wholenode = FALSE)


root <- "contexts"
#src <- conan::conan_sources(NULL, "https://ncov-ic.github.io/drat")
ctx <- context::context_save(root, packages = packages_list)

cfg <- didehpc::didehpc_config(cluster = "covid",
                               template = 'AllNodes',
                               cores = 32)



obj <- didehpc::queue_didehpc(ctx, config = cfg)
obj$cluster_load(TRUE)


#Like the queue object, obj, task objects are R6 objects that can be used to get 
#information and results back from the task.
model_fit_pre_hosp <- obj$enqueue(orderly::orderly_run("01a_model_fit", 
                                                       parameters = list(warmup_iterations = 1000,
                                                                         total_iterations = 5000,
                                                                         tree_depth = 14,
                                                                         covariates = "default",
                                                                         scale_by_number_of_neighbours = FALSE)))

#20221221-155237-d2899a1f
model_fit_pre_hosp$status()
model_fit_pre_hosp$times()
model_fit_pre_hosp$result()
model_fit_pre_hosp$log()
#duration:

########################################################################
#Nearest neighbor models

#scale by susceptibles OFF
scenario1_short <- obj2$enqueue(orderly::orderly_run("01a_model_fit", 
                                                     parameters = list(warmup_iterations = 300,
                                                                       total_iterations = 1500,
                                                                       tree_depth = 12,
                                                                       covariates = "default",
                                                                       scale_by_number_of_neighbours = FALSE,
                                                                       scale_by_susceptible_pool = FALSE,
                                                                       spatial_kernel = "neighbours",
                                                                       algorithm = "NUTS")) )

#20221222-001256-12972a75
scenario1_short$status()
scenario1_short$times()
scenario1_short$result()
scenario1_short$log()
#duration: 14.34 hours   (51607 seconds)

#scale by susceptibles ON
scenario2_short <- obj2$enqueue(orderly::orderly_run("01a_model_fit", 
                                                     parameters = list(warmup_iterations = 300,
                                                                       total_iterations = 1500,
                                                                       tree_depth = 12,
                                                                       covariates = "default",
                                                                       scale_by_number_of_neighbours = FALSE,
                                                                       scale_by_susceptible_pool = TRUE,
                                                                       spatial_kernel = "neighbours",
                                                                       algorithm = "NUTS")) )

#20221222-002540-d99960db
scenario2_short$status()
scenario2_short$times()
scenario2_short$result()
scenario2_short$log()
#duration: 14.43 hours (51963 seconds)

########################################################################
#Gravity model kernel


#scale by susceptibles OFF
scenario3_short <- obj2$enqueue(orderly::orderly_run("01a_model_fit", 
                                                     parameters = list(warmup_iterations = 300,
                                                                       total_iterations = 800,
                                                                       tree_depth = 9,
                                                                       covariates = "default",
                                                                       scale_by_number_of_neighbours = FALSE,
                                                                       scale_by_susceptible_pool = FALSE,
                                                                       spatial_kernel = "gravity",
                                                                       algorithm = "NUTS")) )

#20221222-003142-df9bbde5
scenario3_short$status()
scenario3_short$times()
scenario3_short$result()
scenario3_short$log()
#duration: 15.3 hours (54947 seconds)

#scale by susceptibles ON
scenario4_short <- obj2$enqueue(orderly::orderly_run("01a_model_fit", 
                                                     parameters = list(warmup_iterations = 300,
                                                                       total_iterations = 800,
                                                                       tree_depth = 9,
                                                                       covariates = "default",
                                                                       scale_by_number_of_neighbours = FALSE,
                                                                       scale_by_susceptible_pool = TRUE,
                                                                       spatial_kernel = "gravity",
                                                                       algorithm = "NUTS")) )

#20221222-003446-a6c2c1a3
scenario4_short$status()
scenario4_short$times()
scenario4_short$result()
scenario4_short$log()
#duration: 17.63 hours (63464 seconds)

###########################################################################
## LONG RUNS
##########################################################################

#Nearest neighbor models

#scale by susceptibles OFF
scenario1_long <- obj2$enqueue(orderly::orderly_run("01a_model_fit", 
                                                    parameters = list(warmup_iterations = 500,
                                                                      total_iterations = 5000,
                                                                      tree_depth = 14,
                                                                      covariates = "default",
                                                                      scale_by_number_of_neighbours = FALSE,
                                                                      scale_by_susceptible_pool = FALSE,
                                                                      spatial_kernel = "neighbours",
                                                                      algorithm = "NUTS")) )

#20221222-003943-94620618
scenario1_long$status()
scenario1_long$times()
scenario1_long$result()
scenario1_long$log()
#duration:

#scale by susceptibles ON
scenario2_long <- obj2$enqueue(orderly::orderly_run("01a_model_fit", 
                                                    parameters = list(warmup_iterations = 500,
                                                                      total_iterations = 5000,
                                                                      tree_depth = 14,
                                                                      covariates = "default",
                                                                      scale_by_number_of_neighbours = FALSE,
                                                                      scale_by_susceptible_pool = TRUE,
                                                                      spatial_kernel = "neighbours",
                                                                      algorithm = "NUTS")) )

#20221222-004106-dba3fa2f
scenario2_long$status()
scenario2_long$times()
scenario2_long$result()
scenario2_long$log()
#duration:

########################################################################
#Gravity model kernel


#scale by susceptibles OFF
scenario3_long <- obj2$enqueue(orderly::orderly_run("01a_model_fit", 
                                                    parameters = list(warmup_iterations = 500,
                                                                      total_iterations = 3000,
                                                                      tree_depth = 12,
                                                                      covariates = "default",
                                                                      scale_by_number_of_neighbours = FALSE,
                                                                      scale_by_susceptible_pool = FALSE,
                                                                      spatial_kernel = "gravity",
                                                                      algorithm = "NUTS")) )

#20221222-004235-7c135b2b
scenario3_long$status()
scenario3_long$times()
scenario3_long$result()
scenario3_long$log()
#duration:

#scale by susceptibles ON
scenario4_long <- obj2$enqueue(orderly::orderly_run("01a_model_fit", 
                                                    parameters = list(warmup_iterations = 500,
                                                                      total_iterations = 3000,
                                                                      tree_depth = 12,
                                                                      covariates = "default",
                                                                      scale_by_number_of_neighbours = FALSE,
                                                                      scale_by_susceptible_pool = TRUE,
                                                                      spatial_kernel = "gravity",
                                                                      algorithm = "NUTS")) )

#20221222-004351-1aedc395
scenario4_long$status()
scenario4_long$times()
scenario4_long$result()
scenario4_long$log()
#duration:

###########################################################################
###########################################################################
###########################################################################
## NO HOSPITALISATIONS IN MODEL
###########################################################################
###########################################################################
###########################################################################
obj_no_hosp <- didehpc::queue_didehpc(ctx, config = cfg)
obj_no_hosp$cluster_load(TRUE)
########################################################################
#Nearest neighbor models

#scale by susceptibles OFF
scenario1_no_hosp_short <- obj_no_hosp$enqueue(orderly::orderly_run("01b_model_fit_no_hospital", 
                                                                    parameters = list(warmup_iterations = 300,
                                                                                      total_iterations = 1500,
                                                                                      tree_depth = 12,
                                                                                      covariates = "default",
                                                                                      scale_by_number_of_neighbours = FALSE,
                                                                                      scale_by_susceptible_pool = FALSE,
                                                                                      spatial_kernel = "neighbours",
                                                                                      algorithm = "NUTS")) )

#20221222-181755-448159b8
scenario1_no_hosp_short$status()
scenario1_no_hosp_short$times()
scenario1_no_hosp_short$result()
scenario1_no_hosp_short$log()
#duration: 

#scale by susceptibles ON
scenario2_no_hosp_short <- obj_no_hosp$enqueue(orderly::orderly_run("01b_model_fit_no_hospital", 
                                                                    parameters = list(warmup_iterations = 300,
                                                                                      total_iterations = 1500,
                                                                                      tree_depth = 12,
                                                                                      covariates = "default",
                                                                                      scale_by_number_of_neighbours = FALSE,
                                                                                      scale_by_susceptible_pool = TRUE,
                                                                                      spatial_kernel = "neighbours",
                                                                                      algorithm = "NUTS")) )

#20221222-181854-00644cf8
scenario2_no_hosp_short$status()
scenario2_no_hosp_short$times()
scenario2_no_hosp_short$result()
scenario2_no_hosp_short$log()
#duration: 

########################################################################
#Gravity model kernel


#scale by susceptibles OFF
scenario3_no_hosp_short <- obj_no_hosp$enqueue(orderly::orderly_run("01b_model_fit_no_hospital", 
                                                                    parameters = list(warmup_iterations = 300,
                                                                                      total_iterations = 800,
                                                                                      tree_depth = 9,
                                                                                      covariates = "default",
                                                                                      scale_by_number_of_neighbours = FALSE,
                                                                                      scale_by_susceptible_pool = FALSE,
                                                                                      spatial_kernel = "gravity",
                                                                                      algorithm = "NUTS")) )

#20221222-181933-696d839e
scenario3_no_hosp_short$status()
scenario3_no_hosp_short$times()
scenario3_no_hosp_short$result()
scenario3_no_hosp_short$log()
#duration: 

#scale by susceptibles ON
scenario4_no_hosp_short <- obj_no_hosp$enqueue(orderly::orderly_run("01b_model_fit_no_hospital", 
                                                                    parameters = list(warmup_iterations = 300,
                                                                                      total_iterations = 800,
                                                                                      tree_depth = 9,
                                                                                      covariates = "default",
                                                                                      scale_by_number_of_neighbours = FALSE,
                                                                                      scale_by_susceptible_pool = TRUE,
                                                                                      spatial_kernel = "gravity",
                                                                                      algorithm = "NUTS")) )

#20221222-182009-d92aabac
scenario4_no_hosp_short$status()
scenario4_no_hosp_short$times()
scenario4_no_hosp_short$result()
scenario4_no_hosp_short$log()
#duration:

###########################################################################
## LONG RUNS
##########################################################################

#Nearest neighbor models

#scale by susceptibles OFF
scenario1_no_hosp_long <- obj_no_hosp$enqueue(orderly::orderly_run("01b_model_fit_no_hospital", 
                                                                   parameters = list(warmup_iterations = 500,
                                                                                     total_iterations = 5000,
                                                                                     tree_depth = 14,
                                                                                     covariates = "default",
                                                                                     scale_by_number_of_neighbours = FALSE,
                                                                                     scale_by_susceptible_pool = FALSE,
                                                                                     spatial_kernel = "neighbours",
                                                                                     algorithm = "NUTS")) )

#20221222-182041-bfa62c02
scenario1_no_hosp_long$status()
scenario1_no_hosp_long$times()
scenario1_no_hosp_long$result()
scenario1_no_hosp_long$log()
#duration:

#scale by susceptibles ON
scenario2_no_hosp_long <- obj_no_hosp$enqueue(orderly::orderly_run("01b_model_fit_no_hospital", 
                                                                   parameters = list(warmup_iterations = 500,
                                                                                     total_iterations = 5000,
                                                                                     tree_depth = 14,
                                                                                     covariates = "default",
                                                                                     scale_by_number_of_neighbours = FALSE,
                                                                                     scale_by_susceptible_pool = TRUE,
                                                                                     spatial_kernel = "neighbours",
                                                                                     algorithm = "NUTS")) )

#20221222-182116-2bada71a
scenario2_no_hosp_long$status()
scenario2_no_hosp_long$times()
scenario2_no_hosp_long$result()
scenario2_no_hosp_long$log()
#duration:

########################################################################
#Gravity model kernel


#scale by susceptibles OFF
scenario3_no_hosp_long <- obj_no_hosp$enqueue(orderly::orderly_run("01b_model_fit_no_hospital", 
                                                                   parameters = list(warmup_iterations = 500,
                                                                                     total_iterations = 3000,
                                                                                     tree_depth = 12,
                                                                                     covariates = "default",
                                                                                     scale_by_number_of_neighbours = FALSE,
                                                                                     scale_by_susceptible_pool = FALSE,
                                                                                     spatial_kernel = "gravity",
                                                                                     algorithm = "NUTS")) )

#20221222-182203-f1089229
scenario3_no_hosp_long$status()
scenario3_no_hosp_long$times()
scenario3_no_hosp_long$result()
scenario3_no_hosp_long$log()
#duration:

#scale by susceptibles ON
scenario4_no_hosp_long <- obj_no_hosp$enqueue(orderly::orderly_run("01b_model_fit_no_hospital", 
                                                                   parameters = list(warmup_iterations = 500,
                                                                                     total_iterations = 3000,
                                                                                     tree_depth = 12,
                                                                                     covariates = "default",
                                                                                     scale_by_number_of_neighbours = FALSE,
                                                                                     scale_by_susceptible_pool = TRUE,
                                                                                     spatial_kernel = "gravity",
                                                                                     algorithm = "NUTS")) )

#20221222-182237-3d0d5aec
scenario4_no_hosp_long$status()
scenario4_no_hosp_long$times()
scenario4_no_hosp_long$result()
scenario4_no_hosp_long$log()
#duration:

#############################################################################################
#
#SMOOTH SPLINE 01C
rm(list=ls())

packages_list <- c("tidyverse", "mgcv", "sf", "nimble", "coda",
                   "mgcViz", "spdep", "data.table", "cowplot", "orderly")

#options(didehpc.cluster = "fi--didemrchnb",
#        didehpc.template = "GeneralNodes",
#        didehpc.cores = 4,
#        didehpc.wholenode = FALSE)
#options(
#  didehpc.username = "trawson",
#  didehpc.home = "~/net/home")


root <- "contexts"
#src <- conan::conan_sources(NULL, "https://ncov-ic.github.io/drat")
ctx <- context::context_save(root, packages = packages_list)

cfg <- didehpc::didehpc_config(cluster = "covid",
                               template = 'AllNodes',
                               cores = 32)



obj <- didehpc::queue_didehpc(ctx, config = cfg)
obj$cluster_load(TRUE)


#Like the queue object, obj, task objects are R6 objects that can be used to get 
#information and results back from the task.
spline_fit <- obj$enqueue(orderly::orderly_run("01c_smooth_spline"))


spline_fit$status()
spline_fit$times()
spline_fit$result()
spline_fit$log()
#20230216-180306-9695848f

#STAN 01c
packages_list2 <- c("tidyverse", "mgcv", "sf", "nimble", "coda", "orderly",
                   "mgcViz", "spdep", "data.table", "cowplot", "rstan",
                   "BH",
                   "StanHeaders", "RcppEigen")



ctx2 <- context::context_save(root, packages = packages_list2)


obj2 <- didehpc::queue_didehpc(ctx2, config = cfg)
obj2$cluster_load(TRUE)

spline_stan_fit <- obj2$enqueue(orderly::orderly_run("01c_smooth_spline_STAN"))


spline_stan_fit$status()
spline_stan_fit$times()
spline_stan_fit$result()
spline_stan_fit$log()


