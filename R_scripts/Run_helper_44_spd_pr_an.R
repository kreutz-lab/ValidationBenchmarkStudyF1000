rm(list=ls())

sys_info <- Sys.info()

if(sys_info[["sysname"]]=="Darwin" && sys_info[["login"]]=="root"){
  setwd("/Users/evakohnert/Documents/PhD/Microbiome/Benchmark/BenchmarkStudy_MicrobiomeSyntheticData/R_scripts")
}
if(sys_info[["sysname"]]=="Windows" && sys_info[["login"]]=="srv-kreutz"){
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

source("project_init.R")

 doMissings = F
 option = "4.4_pr_spd_an_" # dummy
 
 pattern <- "*_index\\d+\\.Rdata$"
 fileForNames <- "../Results/3.4_sparseDOSSA_filtered_ZerosAdded_DataProps/data_to_compare.RDS"
 targetFolder <- "../Results/4.4_sparseDOSSA_filtered_ZerosAdded_DA_ancom_Nearing/"

 source("helper_4.x_collectAndSaveDA.R")
