# Script to simulate 1 synthetic datasets for each template with metaSPARSim and to save estimated simulations parameters for the following simulations


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
project_init(bsLibPath="~/Benchmark/R_functions")
###############################



# Get input data for metaSPARSim
data_to_compare <- bs_getData()
#data_to_compare <- bs_subset(data_to_compare,c(9,14)) # select two small ones

cfg <- bs_config("Kohnert_VS_2024",simulator = "metasparsim") # validation study 2024

### Add simulated data - metaSPARSim
try(dir.create(cfg$outfolder1))
data_to_compare <- bs_simulateData(data_to_compare, nsim=1, simulator=cfg$simulator, file=cfg$outfolder1, parallelMode = T, runOption="normal")
data_to_compare <- bs_compress(data_to_compare)
saveRDS(data_to_compare,file=paste0(cfg$outfolder1,"/data_to_compare.RDS"))

params <- list()
for(i in 1:length(data_to_compare)){
 params[[i]] <- data_to_compare[[i]]$metaSPARSim_1$params
}
saveRDS(params,paste0(cfg$outfolder1,"/params.RDS"))

## We don't have simulations in datasets 2, 13, 26, 27
bs_checkParams(params)


############################
# Reducing the effect size:

cfg2 <- bs_config("Kohnert_VS_2024",simulator = "metasparsim_partReg") # validation study 2024

try(dir.create(cfg2$outfolder1))
data_to_compare <- bs_simulateData(data_to_compare, nsim=1, simulator=cfg2$simulator, file=cfg2$outfolder1, parallelMode = T,runOption="normal")
data_to_compare <- bs_compress(data_to_compare)
saveRDS(data_to_compare,file=paste0(cfg2$outfolder1,"/data_to_compare.RDS"))

params <- list()
for(i in 1:length(data_to_compare)){
  for(j in 1:length(data_to_compare[[i]])){
    if(bs_isa(data_to_compare[[i]][[j]],"sim_result"))
      params[[i]] <- data_to_compare[[i]]$params
  }
}
saveRDS(params,paste0(cfg2$outfolder1,"/params.RDS"))

bs_checkParams(params)


