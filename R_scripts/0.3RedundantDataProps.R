# Script for identifying redundant data properties.

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


data_to_compare <- readRDS("../Data/data_to_compare.RDS")
data_to_compare <- bs_addDataProp(data_to_compare)

saveRDS(data_to_compare,file = "../Data/data_to_compare.withDataProps.RDS")

cat("Determine non-redundant data properties...\n")
redundant <- bs_identifyRedundantDataProps(data_to_compare,outFile = "../Data/RedundantDataProps.pdf")
saveRDS(redundant,"../Data/RedundantDataProps.RDS")

