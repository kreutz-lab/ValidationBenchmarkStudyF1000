
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

if(!exists("doPlots"))
  doPlots <- T

# Normal simulation (without zeros added):
for(dataSetFilter in c("simEquiv","sim","exp","expAll")){

  ########### Specify the data to be used here (unfiltered number)
  simulator = "msp"
  analysis = 4.2
  partReg = T
  ########### End of specification ##############
  
  dataFolder <- bs_getPath(analysis,simulator,partReg = partReg) # something like "../Results/4.1_metaSPARSim_DA"
  dfFolder <- bs_getPath(analysis+1,simulator,partReg = partReg,nameAim2 = "Aim2_primary") # "../Results/5.1.1_Aim2_primary_metaSPARSim"  # for saving DF* and profile_list
  dfFolder <- paste0(sub("/$", "",dfFolder), "_", dataSetFilter)
  outFolder <- dfFolder
  if(dataSetFilter=="simEquiv"){
    File <- paste0(bs_getPath(analysis+1,simulator,partReg = partReg),"ToRemove_because_outlierInSimilarityWithTemplate.RDS")
    File <- sub("5.._Aim1","5.1_Aim1",File)
    msb.copyToFolder(File,dfFolder) 
  }
  source("script_5.x_Aim2_primary_filtered_or_unfiltered.R")
  if(doPlots)
    source("script_5.x_Aim2_Plots.R")
  
  
  dataFolder <- bs_getPath(analysis+0.2,simulator,partReg = partReg) #"../Results/4.3_metaSPARSim_filtered_DA"
  dfFolder <- bs_getPath(analysis+1.2,simulator,partReg = partReg,nameAim2 = "Aim2_primary") #"../Results/5.3.1_Aim2_primary_metaSPARSim_filtered" # for saving DF*  and profile_list
  dfFolder <- paste0(sub("/$", "",dfFolder), "_", dataSetFilter)
  # dfFolderFiltered <- paste0(dfFolderFiltered, "_", dataSetFilter)
  outFolder <- dfFolder
  if(dataSetFilter=="simEquiv"){
    # Use ToRemove* from unfiltered:
    File <- paste0(bs_getPath(analysis+1,simulator,partReg = partReg),"ToRemove_because_outlierInSimilarityWithTemplate.RDS")
    File <- sub("5.._Aim1","5.1_Aim1",File)
    msb.copyToFolder(File,dfFolder) 
  }
  
  source("script_5.x_Aim2_primary_filtered_or_unfiltered.R")
  if(doPlots)
    source("script_5.x_Aim2_Plots.R")
  
  
  dfFolder <- bs_getPath(analysis+1,simulator,partReg = partReg,nameAim2 = "Aim2_primary") # "../Results/5.1.1_Aim2_primary_metaSPARSim"    # if required, df_significance is loaded from this folder
  dfFolderFiltered <-bs_getPath(analysis+1.2,simulator,partReg = partReg,nameAim2 = "Aim2_primary") # "../Results/5.3.1_Aim2_primary_metaSPARSim_filtered"  # if required, df_significance for filtered data is loaded from this folder
  outFolder <- sub(as.character(analysis+1),paste0(as.character(analysis+1),"+",as.character(analysis+1.2)), outFolder, fixed=T)

  dfFolder <- paste0(sub("/$", "",dfFolder), "_", dataSetFilter)
  dfFolderFiltered <- paste0(sub("/$", "",dfFolderFiltered), "_", dataSetFilter)
#  outFolder <- paste0(sub("/$", "",outFolder), "_", dataSetFilter)
  if(doPlots)
    source("script_5.x_Aim2_primary_filtered+nonFiltered.R")
  
}

