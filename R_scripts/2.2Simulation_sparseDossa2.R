# Script to simulate 10 synthetic datasets for each template with metaSPARSim


################################
# Set correct working directory
sys_info <- Sys.info()

if(sys_info[["sysname"]]=="Darwin" && sys_info[["login"]]=="root"){
  setwd("/Users/evakohnert/Documents/PhD/Microbiome/Benchmark/BenchmarkStudy_MicrobiomeSyntheticData/R_scripts")
}
if(sys_info[["sysname"]]=="Windows" && grep("kreutz",sys_info[["login"]],T)){
  setwd("E:/clemens/Repositories/BenchmarkStudy_MicrobiomeSyntheticData/R_scripts")
}
if(sys_info[["sysname"]]=="Linux" && sys_info[["login"]]=="kohnert"){
  setwd("/h/kohnert/BenchmarkStudy_MicrobiomeSyntheticData/R_scripts")
}
if(sys_info[["sysname"]]=="Linux" && sys_info[["login"]]=="ckreutz"){
  if(sys_info[["nodename"]]=="imbip-compute-214")
    setwd("~/BenchmarkStudy_MicrobiomeSyntheticData_25Mar24/R_scripts")
  else
    setwd("~/BenchmarkStudy_MicrobiomeSyntheticData/R_scripts")
}


# Initialization, i.e. sourcing R-functions, loading packages and setting project_path
source("project_init.R")
# project_init()
###############################

params <- readRDS("../Results/1.2_sparseDOSSA/params.RDS")

# Get input data for metaSPARSim
data_to_compare <- bs_getData()

#d <- bs_subset(data_to_compare,c(9,14)) # select two small ones
d <- data_to_compare

# Add simulated data - metaSPARSim
d <- bs_simulateData(d, nsim=10, simulator="sparsedossa2", file=".", 
                        parallelMode = T, runOption="normal",
                     paramsWorkspace = params, maxNcore=15)
#                   paramsWorkspace = "../Results/1.2_sparseDOSSA/params.RDS")

saveRDS(d,file="../Results/2.2_sparseDOSSA/data_to_compare.RDS")

