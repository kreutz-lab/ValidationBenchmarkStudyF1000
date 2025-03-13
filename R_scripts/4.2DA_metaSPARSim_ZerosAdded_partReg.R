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

dataFolder <- "../Results_partReg/3.2_metaSPARSim_ZerosAdded_DataProps"
resultFolder <- "../Results_partReg/4.2_metaSPARSim_ZerosAdded_DA" 
prefix <- "4.2_partReg_"

source("script_4.x_DA.R")

#resultFolder <- "../Results_partReg/4.2_metaSPARSim_ZerosAdded_DA_ancom_Nearing" 
#source("script_4.x_DA_ancom_Nearing.R")
