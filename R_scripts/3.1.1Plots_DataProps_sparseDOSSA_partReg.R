# Script to make Boxplots and PCA plots of data properties

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

dataFolder <- "../Results_partReg/3.1_sparseDOSSA_DataProps"
resultFolder <- "../Results_partReg/3.1_sparseDOSSA_DataProps"
prefix <- "3.1.1pR"

tryCatch({
  source("script_3.x.x_Plots_DataProps.R")
}, error=function(e){
  try(save.image(paste0("Error_",prefix,".Rdata")))
  cat("Error script_3.x.x_Plots_DataProps.R, prefix= prefix\n",conditionMessage(e),"\n",file=paste0(prefix,"_Error.log"),append = T)
})

###############################
dataFolder <- "../Results_partReg/3.2_sparseDOSSA_ZerosAdded_DataProps"
resultFolder <- "../Results_partReg/3.2_sparseDOSSA_ZerosAdded_DataProps"
prefix <- "3.2.1pR"

tryCatch({
  source("script_3.x.x_Plots_DataProps.R")
}, error=function(e){
  try(save.image(paste0("Error_",prefix,".Rdata")))
  cat("Error script_3.x.x_Plots_DataProps.R, prefix= prefix\n",conditionMessage(e),"\n",file=paste0(prefix,"_Error.log"),append = T)
})

###############################
dataFolder <- "../Results_partReg/3.3_sparseDOSSA_Filtered_DataProps"
resultFolder <- "../Results_partReg/3.3_sparseDOSSA_Filtered_DataProps"
prefix <- "3.3.1pR"

tryCatch({
  source("script_3.x.x_Plots_DataProps.R")
}, error=function(e){
  try(save.image(paste0("Error_",prefix,".Rdata")))
  cat("Error script_3.x.x_Plots_DataProps.R, prefix= prefix\n",conditionMessage(e),"\n",file=paste0(prefix,"_Error.log"),append = T)
})

###############################
dataFolder <- "../Results_partReg/3.4_sparseDOSSA_filtered_ZerosAdded_DataProps"
resultFolder <- "../Results_partReg/3.4_sparseDOSSA_filtered_ZerosAdded_DataProps"
prefix <- "3.4.1pR"

tryCatch({
  source("script_3.x.x_Plots_DataProps.R")
}, error=function(e){
  try(save.image(paste0("Error_",prefix,".Rdata")))
  cat("Error script_3.x.x_Plots_DataProps.R, prefix= prefix\n",conditionMessage(e),"\n",file=paste0(prefix,"_Error.log"),append = T)
})

