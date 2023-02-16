#Run the pipeline on the HPC

#MODEL FIT
rm(list=ls())

packages_list <- c("tidyverse", "rstan", "orderly", "BH",
                   "StanHeaders", "RcppEigen")

#options(didehpc.cluster = "fi--didemrchnb",
#        didehpc.template = "GeneralNodes",
#        didehpc.cores = 4,
#        didehpc.wholenode = FALSE)


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
model_fit <- obj$enqueue(orderly::orderly_run("01a_model_fit", 
                                              parameters = list(warmup_iterations = 100,
                                                                total_iterations = 500,
                                                                tree_depth = 14)))


model_fit$status()
model_fit$times()
model_fit$result()
model_fit$log()


#############################################################################################
#
#SMOOTH SPLINE 01C
rm(list=ls())

packages_list <- c("tidyverse", "mgcv", "sf", "nimble", "coda",
                   "mgcViz", "spdep", "data.table", "cowplot")

#options(didehpc.cluster = "fi--didemrchnb",
#        didehpc.template = "GeneralNodes",
#        didehpc.cores = 4,
#        didehpc.wholenode = FALSE)


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
model_fit <- obj$enqueue(orderly::orderly_run("01c_smooth_spline"))


model_fit$status()
model_fit$times()
model_fit$result()
model_fit$log()
