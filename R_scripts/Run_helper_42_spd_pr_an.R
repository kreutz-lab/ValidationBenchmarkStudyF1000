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
 option = "4.2_pr_spd_an_" # dummy
 
 pattern <- "*_index\\d+\\.Rdata$"
 fileForNames <- paste0(bs_getPath(3.2,"spd",nearing=F,partReg=F),"data_to_compare.RDS")  # "../Results/3.2_sparseDOSSA_ZerosAdded_DataProps/data_to_compare.RDS"
 targetFolder <- bs_getPath(4.2,"spd",nearing=T,partReg=F) # ../Results/4.2_sparseDOSSA_ZerosAdded_DA_ancom_Nearing/"

 source("helper_4.x_collectAndSaveDA.R")
