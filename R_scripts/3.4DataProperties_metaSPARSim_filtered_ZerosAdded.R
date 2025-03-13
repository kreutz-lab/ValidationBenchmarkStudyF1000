# Script to calculate data properties for real and synthetic data


################################
# Set correct working directosry
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
#data_to_compare <- bs_compress(data_to_compare) # bs_addExperimentalZeros does not always word for decompressed

data_to_compare <- bs_decompress(data_to_compare)
data_to_compare <- bs_checkData(data_to_compare,eliminateZeroSimus = T)

data_to_compare <- bs_addExperimentalZeros(data_to_compare,makeCopies = F)
# Prevalence filter as in Nearing et al.:
data_to_compare <- bs_filterPrevalence(data_to_compare, prevalence=0.1)


resultFolder <- "../Results/3.4_metaSPARSim_filtered_ZerosAdded_DataProps"
prefix <- "3.4"
source("script_3x_DataProperties.R")

