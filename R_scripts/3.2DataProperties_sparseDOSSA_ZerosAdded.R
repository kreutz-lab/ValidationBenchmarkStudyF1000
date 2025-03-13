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

# Get input data for sparseDOSSA
data_to_compare <- readRDS("../Results/2.2_sparseDOSSA/data_to_compare.RDS")
#data_to_compare <- bs_compress(data_to_compare) # bs_addExperimentalZeros does not always word for decompressed

data_to_compare <- bs_decompress(data_to_compare)
data_to_compare <- bs_annotateObject(data_to_compare)
data_to_compare <- bs_checkData(data_to_compare,eliminateZeroSimus = T)
data_to_compare <- bs_addExperimentalZeros(data_to_compare,makeCopies = F)


resultFolder <- "../Results/3.2_sparseDOSSA_ZerosAdded_DataProps"
prefix <- "3.2"
source("script_3x_DataProperties.R")

