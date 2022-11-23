#Installing the necessary packages for submitting jobs to the DIDE cluster

 install.packages("drat") # if needed
drat:::add("mrc-ide")
install.packages("didehpc")

didehpc::didehpc_config()
