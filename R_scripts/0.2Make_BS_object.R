# Script to build the Benchmark specific object "data_to_compare" with all data sets

###############################
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
  setwd("~/BenchmarkStudy_MicrobiomeSyntheticData/R_scripts")
}


# Initialization, i.e. sourcing R-functions, loading packages and setting project_path
source("project_init.R")
# project_init()
###############################

# Get all data sets  
data_to_compare <- bs_getData()

# Eliminate redundant data properties:
saveRDS(data_to_compare,"../Data/data_to_compare.RDS")

# Make test object with 3 project, each 500 features
data_to_compare_test <- bs_getData(files.names =  c("ArcticFireSoils","wood_plastic_kesy","BISCUIT"), nfeature = 500)
saveRDS(data_to_compare_test,"../Data/data_to_compare_testobject.RDS")

# Make test object with all projects, each 200 features
data_to_compare_test2 <- bs_getData(nfeature = 200)
saveRDS(data_to_compare_test2,"../Data/data_to_compare_200features.RDS")

