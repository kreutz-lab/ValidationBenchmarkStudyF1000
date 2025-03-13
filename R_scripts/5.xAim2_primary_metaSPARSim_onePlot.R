
# Script to analyze the results from differential abundance tests on metaSPARSim data
# This corresponds to Aim 2 - primary outcomes
# barplot to summarize the proportion of shared significant features across all syntehtic data sets
# 13 conclusions from Box1 are tested

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

useNearingNames = F

# Normal simulation (without zeros added):
for(dataSetFilter in c("sim","exp","expAll")){
  #for(dataSetFilter in c("sim","exp")){ 
  dataFolder <- "../Results/4.1_metaSPARSim_DA"
  dfFolder <- "../Results/5.1.1_Aim2_primary_metaSPARSim"  # for saving DF* and profile_list
  rm(dfFolderFiltered)  # not required => be sure that is is not used and an old value is used
  outFolder <- "../Results/5.1.1_Aim2_primary_metaSPARSim"

  dfFolder <- paste0(dfFolder, "_", dataSetFilter)
  # dfFolderFiltered <- paste0(dfFolderFiltered, "_", dataSetFilter)
  outFolder <- paste0(outFolder, "_", dataSetFilter)
  #source("script_5.x_Aim2_primary_filtered_or_unfiltered.R")
  #source("script_5.x_Aim2_Plots.R")
 
   #######
  print(dataFolder)
  print(outFolder)
  
  DF_filt <- readRDS(paste0(dataFolder,"/DF_filt.RDS"))
  
  ############################################
  ## Now filter according to exp or simu #####
  DF_filt <- bs_apply_dataSetFilter(DF_filt,dataSetFilter)
  ## End filter according to exp or simu #####
  ############################################

  tmp <- bs_DA_plotFunctions(DF_filt,  plotType="OverlapHist")
  pdf(file=paste0(outFolder,"/OverlapHist_sorted.pdf"), width=12,height=20)
  print(tmp)
  dev.off()
  #####
  
  dataFolder <- "../Results/4.3_metaSPARSim_filtered_DA"
  dfFolder <- "../Results/5.3.1_Aim2_primary_metaSPARSim_Filtered" # for saving DF*  and profile_list
  rm(dfFolderFiltered)  # not required => be sure that is is not used and an old value is used
  outFolder <- "../Results/5.3.1_Aim2_primary_metaSPARSim_Filtered"

  dfFolder <- paste0(dfFolder, "_", dataSetFilter)
  # dfFolderFiltered <- paste0(dfFolderFiltered, "_", dataSetFilter)
  outFolder <- paste0(outFolder, "_", dataSetFilter)
  #source("script_5.x_Aim2_primary_filtered_or_unfiltered.R")
  #source("script_5.x_Aim2_Plots.R")
  #######
  DF_filt <- readRDS(paste0(dataFolder,"/DF_filt.RDS"))
  
  ############################################
  ## Now filter according to exp or simu #####
  DF_filt <- bs_apply_dataSetFilter(DF_filt,dataSetFilter)
  ## End filter according to exp or simu #####
  ############################################

  tmp <- bs_DA_plotFunctions(DF_filt,  plotType="OverlapHist")
  pdf(file=paste0(outFolder,"/OverlapHist_sorted.pdf"), width=12,height=20)
  print(tmp)
  dev.off()
  #####
  
  rm(dataFolder)                                             # not required any more
  dfFolder <- "../Results/5.1.1_Aim2_primary_metaSPARSim"    # if required, df_significance is loaded from this folder
  dfFolderFiltered <- "../Results/5.3.1_Aim2_primary_metaSPARSim_Filtered"  # if required, df_significance for filtered data is loaded from this folder
  outFolder <- "../Results/5.1.1+5.3.1_Aim2_primary_metaSPARSim" # results are written, here

  dfFolder <- paste0(dfFolder, "_", dataSetFilter)
  dfFolderFiltered <- paste0(dfFolderFiltered, "_", dataSetFilter)
  outFolder <- paste0(outFolder, "_", dataSetFilter)
  #source("script_5.x_Aim2_primary_filtered+nonFiltered.R")
  
}



###################
# Zeros added:
for(dataSetFilter in c("sim","exp","expAll")){
  #for(dataSetFilter in c("sim","exp")){ 
  dataFolder <- "../Results/4.2_metaSPARSim_ZerosAdded_DA"
  dfFolder <- "../Results/5.2.1_Aim2_primary_metaSPARSim"  # for saving DF* and profile_list
  rm(dfFolderFiltered)  # not required => be sure that is is not used and an old value is used
  outFolder <- "../Results/5.2.1_Aim2_primary_metaSPARSim_ZerosAdded"
  
  dfFolder <- paste0(dfFolder, "_", dataSetFilter)
  # dfFolderFiltered <- paste0(dfFolderFiltered, "_", dataSetFilter)
  outFolder <- paste0(outFolder, "_", dataSetFilter)
 # source("script_5.x_Aim2_primary_filtered_or_unfiltered.R")
  #source("script_5.x_Aim2_Plots.R")
  #######
  DF_filt <- readRDS(paste0(dataFolder,"/DF_filt.RDS"))
  
  ############################################
  ## Now filter according to exp or simu #####
  DF_filt <- bs_apply_dataSetFilter(DF_filt,dataSetFilter)
  ## End filter according to exp or simu #####
  ############################################

  tmp <- bs_DA_plotFunctions(DF_filt,  plotType="OverlapHist")
  pdf(file=paste0(outFolder,"/OverlapHist_sorted.pdf"), width=12,height=20)
  print(tmp)
  dev.off()
  #####
  
  
  dataFolder <- "../Results/4.4_metaSPARSim_filtered_ZerosAdded_DA"
  dfFolder <- "../Results/5.4.1_Aim2_primary_metaSPARSim_Filtered" # for saving DF*  and profile_list
  rm(dfFolderFiltered)  # not required => be sure that is is not used and an old value is used
  outFolder <- "../Results/5.4.1_Aim2_primary_metaSPARSim_filtered_ZerosAdded"
  
  dfFolder <- paste0(dfFolder, "_", dataSetFilter)
  # dfFolderFiltered <- paste0(dfFolderFiltered, "_", dataSetFilter)
  outFolder <- paste0(outFolder, "_", dataSetFilter)
 # source("script_5.x_Aim2_primary_filtered_or_unfiltered.R")
  #source("script_5.x_Aim2_Plots.R")
  #######
  DF_filt <- readRDS(paste0(dataFolder,"/DF_filt.RDS"))
  
  ############################################
  ## Now filter according to exp or simu #####
  DF_filt <- bs_apply_dataSetFilter(DF_filt,dataSetFilter)
  ## End filter according to exp or simu #####
  ############################################

  tmp <- bs_DA_plotFunctions(DF_filt,  plotType="OverlapHist")
  pdf(file=paste0(outFolder,"/OverlapHist_sorted.pdf"), width=12,height=20)
  print(tmp)
  dev.off()
  #####
  
  
  rm(dataFolder)                                             # not required any more
  dfFolder <- "../Results/5.2.1_Aim2_primary_metaSPARSim"    # if required, df_significance is loaded from this folder
  dfFolderFiltered <- "../Results/5.4.1_Aim2_primary_metaSPARSim_Filtered"  # if required, df_significance for filtered data is loaded from this folder
  outFolder <- "../Results/5.2.1+5.4.1_Aim2_primary_metaSPARSim_ZerosAdded" # results are written, here
  
  dfFolder <- paste0(dfFolder, "_", dataSetFilter)
  dfFolderFiltered <- paste0(dfFolderFiltered, "_", dataSetFilter)
  outFolder <- paste0(outFolder, "_", dataSetFilter)
 # source("script_5.x_Aim2_primary_filtered+nonFiltered.R")
}

