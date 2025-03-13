# Script to calculate data properties for all data set and to calculate the comparison of data properties 


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


# Get input data for metaSPARSim
data_to_compare <- readRDS("../Results/2.1_metaSPARSim/data_to_compare.RDS")
data_to_compare <- bs_decompress(data_to_compare)
# Remove data sets that have no simulated data
data_to_compare <- bs_checkData(data_to_compare,eliminateZeroSimus=TRUE)
#data_to_compare <- bs_subset(data_to_compare,c(9,14)) # select two small ones

#data_to_compare <- bs_subset(data_to_compare,!names(data_to_compare)=="ob_zupancic") # is not in Nearing

# Prevalence filter as in Nearing et al.:
data_to_compare <- bs_filterPrevalence(data_to_compare, prevalence=0.1)


resultFolder <- "../Results/3.3_metaSPARSim_Filtered_DataProps"
prefix <- "3.3"
source("script_3x_DataProperties.R")

