# Script to analyze the results from differential abundance tests on sparseDOSSA data
# This corresponds to Aim 2 - secondary outcomes
# 13 conclusions from Box2 are tested


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


warning("5.xAim2_secondary_sparseDOSSA: Do not process simus without added zeros!")


for(dataSetFilter in c("sim","exp","simEquiv","expAll")){
#for(dataSetFilter in c("simEquiv")){
    ########### Specify the data to be used here (unfiltered number)
  simulator = "spd" ## SparseDOSSA
  analysis = 4.2
  partReg = F
  ########### End of specification ##############
  
  # filtered, i.e. 4.4 spd 
  dataFolder <- bs_getPath(analysis+0.2,simulator,partReg = partReg) # 
  dfFolder <- bs_getPath(analysis+1.2,simulator,partReg = partReg,nameAim2 = "Aim2_primary") # if required, df_significance is loaded from this folder
  outFolder <- bs_getPath(analysis+1.2,simulator,partReg = partReg,nameAim2 = "Aim2_secondary") # 
  dfFolder <- paste0(sub("/$", "",dfFolder), "_", dataSetFilter)
  outFolder <- paste0(sub("/$", "",outFolder), "_", dataSetFilter)
  source("script_5.x_Aim2_secondary_filtered_or_unfiltered.R")
  
  dataFolder <- bs_getPath(analysis,simulator,partReg = partReg) # something like "../Results/4.1_metaSPARSim_DA"
  dfFolder <- bs_getPath(analysis+1,simulator,partReg = partReg,nameAim2 = "Aim2_primary") # something like "../Results/5.1_Aim2_primary_metaSPARSim"; if required, df_significance is loaded from this folder
  outFolder <- bs_getPath(analysis+1,simulator,partReg = partReg,nameAim2 = "Aim2_secondary") # something like "../Results/5.1_Aim2_secondary_metaSPARSim"
  dfFolder <- paste0(sub("/$", "",dfFolder), "_", dataSetFilter)
  outFolder <- paste0(sub("/$", "",outFolder), "_", dataSetFilter)
  source("script_5.x_Aim2_secondary_filtered_or_unfiltered.R")
  
  # both 
  dfFolderFiltered <- bs_getPath(analysis+1.2,simulator,partReg = partReg,nameAim2 = "Aim2_primary") # if required, df_significance is loaded from this folder
  dfFolderFiltered <- paste0(sub("/$", "",dfFolderFiltered), "_", dataSetFilter)

  outFolder <- bs_getPath(analysis+1,simulator,partReg = partReg,nameAim2 = "Aim2_secondary") # something like "../Results/5.1_Aim2_secondary_metaSPARSim"
  outFolder <- sub(as.character(analysis+1),paste0(as.character(analysis+1),"+",as.character(analysis+1.2)), outFolder, fixed=T)
  outFolder <- paste0(sub("/$", "",outFolder), "_", dataSetFilter)
  source("script_5.x_Aim2_secondary_filtered+nonFiltered.R")
  
  
}

