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


folders <- c("../Results/4.1_metaSPARSim_DA/",
             "../Results/4.2_metaSPARSim_ZerosAdded_DA/",
             "../Results/4.3_metaSPARSim_filtered_DA/", 
             "../Results/4.4_metaSPARSim_filtered_ZerosAdded_DA/")

folders <- c("../Results/4.1_sparseDOSSA_DA/",
             "../Results/4.2_sparseDOSSA_ZerosAdded_DA/",
             "../Results/4.3_sparseDOSSA_filtered_DA/", 
             "../Results/4.4_sparseDOSSA_filtered_ZerosAdded_DA/")

for(folder in folders){
  d <- readRDS(paste0(folder,"data_to_compare.RDS"))
  d <- bs_decompress(d)
  sink(paste0(folder,"bs_check.log"))
  names(d)
  bs_checkData(d)
  sink()
}
