# This script generates OverlapHist plots for:
# a) after aggregating over all datasets
# b) for each data set individually

rm(list=ls())

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

source("project_init.R")


folders <- c("../Results/5.2_Aim2_primary_sparseDOSSA_ancom_Nearing_simEquiv/",
             "../Results/5.4_Aim2_primary_sparseDOSSA_ancom_Nearing_simEquiv/",
             "../Results_partReg/5.2_Aim2_primary_metaSPARSim_ancom_Nearing_simEquiv/",
             "../Results_partReg/5.4_Aim2_primary_metaSPARSim_ancom_Nearing_simEquiv/")

dataSetFilter <- "simEquiv"

#folder <- folders[2]

for(i in 1:length(folders)){
  folder <- folders[i]
  rm(DF_filt)

  DF_filt <- readRDS(paste0(folder,"/DF_filt.RDS"))
  
  ############################################
  ## Now filter according to exp or simu #####
  DF_filt <- bs_apply_dataSetFilter(DF_filt,dataSetFilter,dfFolder=folder)
  ## End filter according to exp or simu #####
  ############################################
  
  if(i==1)
    titel = "sparseDOSSA2, un-filtered"
  if(i==2)
    titel = "sparseDOSSA2, filtered"
  if(i==3)
    titel = "metaSPARSim, un-filtered"
  if(i==4)
    titel = "metaSPARSim, filtered"
  
  myplot <- bs_DA_plotFunctions(DF_filt, useNearingNames=F, plotType="heatmapScaled", rowOrdering = "nearing", plotArg = list(label=titel))
  #print(myplot)
  pdf(file=paste0(folder,"/HeatmapManually.pdf"),height=8,width=7)
  print(myplot)
  dev.off()

  myplot <- bs_DA_plotFunctions(DF_filt, useNearingNames=F, plotType="heatmapScaled2", rowOrdering = "nearing", plotArg = list(label=titel))
  #print(myplot)
  pdf(file=paste0(folder,"/HeatmapManually2.pdf"),height=8,width=7)
  print(myplot)
  dev.off()
}

