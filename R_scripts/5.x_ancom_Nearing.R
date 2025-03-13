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

try(dir.create("../Results_Supp"))

########### Specify the data to be used here (only unfiltered numbers, both are done)
simulators = c("spd","msp") 
analyses = c(4.2, 4.2)
partRegs = c(F, T)

#simulators = c("msp")
#analyses = c(4.2)
#partRegs = c(T)

#simulators = c("spd")
#analyses = c(4.2)
#partRegs = c(F)

nearing = T
########### End of specification ##############

doPlots <- F

for(s in 1:length(simulators)){
  simulator <- simulators[s]
  analysis <- analyses[s]
  partReg <- partRegs[s]
  cat("analysis=",analysis,"simulator=",simulator,"partReg=",partReg,"\n")
  
  if(simulator=="msp"){
    #dataSetFilters <- c("simEquivSpd","simEquiv","exp","sim","expAll")
    dataSetFilters <- "simEquivSpd" # c("simEquiv","simEquivNoAncom","simEquivNoLefse")
  }else{
    #dataSetFilters <- c("simEquiv","exp","sim","expAll")
    dataSetFilters <- c("simEquiv","simEquivNoAncom","simEquivNoLefse","simEquivSpd")
  }
  
  ####################
  # The following lines replace bs_ancombc by ancom_Nearing:
  #################### 
  
  ### unfiltered:
  cat("5.x_ancom_Nearing.R replacing ancombc by ancomNearing (unfiltered) and producing DF.RDS for analysing hypotheses ... \n")
  notNearingFolder <- bs_getPath(analysis,simulator,nearing = F,partReg = partReg)  
  nearingFolder <- bs_getPath(analysis,simulator, nearing = nearing ,partReg = partReg)
  if(file.exists(paste0(nearingFolder,"/data_to_compare.all.RDS"))
     && file.info(paste0(nearingFolder,"/data_to_compare.RDS"))$mtime < file.info(paste0(nearingFolder,"/data_to_compare.all.RDS"))$mtime
     && file.info(paste0(notNearingFolder,"/data_to_compare.RDS"))$mtime < file.info(paste0(nearingFolder,"/data_to_compare.all.RDS"))$mtime){
    cat("Replacement already done!\n")
  } else {
    # Replacement required:
    dnot <- readRDS(paste0(notNearingFolder,"/data_to_compare.RDS"))
    dnear <- readRDS(paste0(nearingFolder,"/data_to_compare.RDS"))
    dall <- bs_ReplaceAncomDA(dnot,dnear)
    DF <- bs_DA2dataFrame(dall)                      # This step updates DF and is essential to integrate ancom_Nearing in hypothesis tests
    saveRDS(DF,file=paste0(nearingFolder,"/DF.RDS"))
    saveRDS(dall,paste0(nearingFolder,"/data_to_compare.all.RDS"))
  }
  
  ### filtered:
  cat("5.x_ancom_Nearing.R replacing ancombc by ancomNearing (filtered) and producing DF.RDS for analysing hypotheses ... \n")
  notNearingFolder <- bs_getPath(analysis+.2,simulator,nearing = F,partReg = partReg)  
  nearingFolder <- bs_getPath(analysis+.2,simulator, nearing = nearing ,partReg = partReg)
  if(file.exists(paste0(nearingFolder,"/data_to_compare.all.RDS"))
     && file.info(paste0(nearingFolder,"/data_to_compare.RDS"))$mtime < file.info(paste0(nearingFolder,"/data_to_compare.all.RDS"))$mtime
     && file.info(paste0(notNearingFolder,"/data_to_compare.RDS"))$mtime < file.info(paste0(nearingFolder,"/data_to_compare.all.RDS"))$mtime){
    cat("Replacement already done!\n")
  } else {
    # Replacement required:
    dnot <- readRDS(paste0(notNearingFolder,"/data_to_compare.RDS"))
    dnear <- readRDS(paste0(nearingFolder,"/data_to_compare.RDS"))
    dall <- bs_ReplaceAncomDA(dnot,dnear)
    DF <- bs_DA2dataFrame(dall)                    # This step updates DF and is essential to integrate ancom_Nearing in hypothesis tests
    saveRDS(DF,file=paste0(nearingFolder,"/DF.RDS"))
    saveRDS(dall,paste0(nearingFolder,"/data_to_compare.all.RDS"))
  }
  rm(list=c("nearingFolder","DF","dall","dnot","dnear")) # remove variables to be save memory and be sure that nothing old is used

  
  ####################
  # Now analyse hypotheses (aim2 primary and secondary)  
  ####################
  
  for(dataSetFilter in dataSetFilters){

    #### Primary analyses:
    ## Primary, unfiltered:
    dataFolder <- bs_getPath(analysis,simulator, nearing = nearing ,partReg = partReg)
    dfFolder <- bs_getPath(analysis+1,simulator, nearing = nearing ,partReg = partReg,nameAim2 = "Aim2_primary") # e.g. "../Results_Supp/5.1_Aim2_primary_metaSPARSim_ancom_Nearing"  # for saving DF* and profile_list
    dfFolder <- paste0(sub("/$", "",dfFolder), "_", dataSetFilter)
    outFolder <- dfFolder # e.g. "../Results_Supp/5.1_Aim2_primary_metaSPARSim_ancom_Nearing"
    
    if(grepl("simEquiv",dataSetFilter)){
      # ToRemove_because_outlierInSimilarityWithTemplate.RDS is only done earlier for nearing=F
      File <- paste0(bs_getPath(analysis+1,simulator, nearing = F ,partReg = partReg),"ToRemove_because_outlierInSimilarityWithTemplate.RDS")
      File <- sub("5.._Aim1","5.1_Aim1",File)
      msb.copyToFolder(File,dfFolder) 
    }
    prefix <- bs_processResultFoldernames(outFolder,"dataName")
    source("script_5.x_Aim2_primary_filtered_or_unfiltered.R")
    # save.image("2.Rdata")
    if(doPlots)
      source("script_5.x_Aim2_Plots.R")
    suppressWarnings(rm(list=c("dataFolder","dfFolder","outFolder","DF","data_to_compare","DF_filt","DF_significance")))
    
    ## Primary, filtered:
    dataFolder <- bs_getPath(analysis+0.2,simulator, nearing = nearing ,partReg = partReg) #"../Results/4.3_metaSPARSim_filtered_DA"
    dfFolder <- bs_getPath(analysis+1.2,simulator, nearing = nearing ,partReg = partReg,nameAim2 = "Aim2_primary") #"../Results/5.3.1_Aim2_primary_metaSPARSim_filtered" # for saving DF*  and profile_list
    dfFolder <- paste0(sub("/$", "",dfFolder), "_", dataSetFilter)
    outFolder <- dfFolder

    if(grepl("simEquiv",dataSetFilter)){
      # ToRemove_because_outlierInSimilarityWithTemplate.RDS is only done earlier for nearing=F
      # Use ToRemove* from unfiltered:
      File <- paste0(bs_getPath(analysis+1,simulator, nearing = F,partReg = partReg),"ToRemove_because_outlierInSimilarityWithTemplate.RDS")
      File <- sub("5.._Aim1","5.1_Aim1",File)
      msb.copyToFolder(File,dfFolder) 
    }
    source("script_5.x_Aim2_primary_filtered_or_unfiltered.R")
    prefix <- bs_processResultFoldernames(outFolder,"dataName")
    # save.image("4.Rdata")
    if(doPlots)
      source("script_5.x_Aim2_Plots.R")
    suppressWarnings(rm(list=c("dataFolder","dfFolder","outFolder","DF","data_to_compare","DF_filt","DF_significance")))
    
    
    ## Primary, both
    dataFolder <- bs_getPath(analysis,simulator, nearing = nearing ,partReg = partReg)
    dfFolder <- bs_getPath(analysis+1,simulator, nearing = nearing ,partReg = partReg,nameAim2 = "Aim2_primary") # "../Results/5.1.1_Aim2_primary_metaSPARSim"    # if required, df_significance is loaded from this folder
    dfFolderFiltered <-bs_getPath(analysis+1.2, nearing = nearing ,simulator,partReg = partReg,nameAim2 = "Aim2_primary") # "../Results/5.3.1_Aim2_primary_metaSPARSim_filtered"  # if required, df_significance for filtered data is loaded from this folder
    outFolder <- bs_getPath(analysis+1,simulator, nearing = nearing ,partReg = partReg,nameAim2 = "Aim2_primary") # "../Results/5.1.1_Aim2_primary_metaSPARSim"    # if required, df_significance is loaded from this folder
    outFolder <- sub(as.character(analysis+1),paste0(as.character(analysis+1),"+",as.character(analysis+1.2)), outFolder, fixed=T)

    dfFolder <- paste0(sub("/$", "",dfFolder), "_", dataSetFilter)
    dfFolderFiltered <- paste0(sub("/$", "",dfFolderFiltered), "_", dataSetFilter)
    outFolder <- paste0(sub("/$", "",outFolder), "_", dataSetFilter)
    # save.image("5.Rdata")
    source("script_5.x_Aim2_primary_filtered+nonFiltered.R")
    prefix <- bs_processResultFoldernames(outFolder,"dataName")
    # save.image("6.Rdata")
#    if(doPlots)
#      source("script_5.x_Aim2_Plots.R")
    suppressWarnings(rm(list=c("dataFolder","dfFolder","outFolder","DF","data_to_compare","DF_filt","DF_significance")))
    
    
    #### Secondary analyses:
    
    # unfiltered:
    dataFolder <- bs_getPath(analysis,simulator,partReg = partReg, nearing = nearing ) # something like "../Results/4.1_metaSPARSim_DA"
    dfFolder <- bs_getPath(analysis+1,simulator,partReg = partReg, nearing = nearing ,nameAim2 = "Aim2_primary") # something like "../Results/5.1_Aim2_primary_metaSPARSim"; if required, df_significance is loaded from this folder
    outFolder <- bs_getPath(analysis+1,simulator,partReg = partReg, nearing = nearing ,nameAim2 = "Aim2_secondary") # something like "../Results/5.1_Aim2_secondary_metaSPARSim"
    dfFolder <- paste0(sub("/$", "",dfFolder), "_", dataSetFilter)
    outFolder <- paste0(sub("/$", "",outFolder), "_", dataSetFilter)

    source("script_5.x_Aim2_secondary_filtered_or_unfiltered.R")
    prefix <- bs_processResultFoldernames(outFolder,"dataName")
    # save.image("7.Rdata")
 #   if(doPlots)
 #     source("script_5.x_Aim2_Plots.R")
    suppressWarnings(rm(list=c("dataFolder","dfFolder","outFolder","DF","data_to_compare","DF_filt","DF_significance")))
    
    # filtered:
    dataFolder <- bs_getPath(analysis+0.2,simulator,partReg = partReg, nearing = nearing ) # 
    dfFolder <- bs_getPath(analysis+1.2,simulator,partReg = partReg, nearing = nearing ,nameAim2 = "Aim2_primary") # if required, df_significance is loaded from this folder
    outFolder <- bs_getPath(analysis+1.2,simulator,partReg = partReg, nearing = nearing ,nameAim2 = "Aim2_secondary") # 
    dfFolder <- paste0(sub("/$", "",dfFolder), "_", dataSetFilter)
    outFolder <- paste0(sub("/$", "",outFolder), "_", dataSetFilter)

    source("script_5.x_Aim2_secondary_filtered_or_unfiltered.R")
    prefix <- bs_processResultFoldernames(outFolder,"dataName")
    if(doPlots)
      source("script_5.x_Aim2_Plots.R")
    suppressWarnings(rm(list=c("dataFolder","dfFolder","outFolder","DF","data_to_compare","DF_filt","DF_significance")))
    
    # both 
    dataFolder <- bs_getPath(analysis,simulator,partReg = partReg, nearing = nearing ) # 
    dfFolder <- bs_getPath(analysis+1,simulator,partReg = partReg, nearing = nearing ,nameAim2 = "Aim2_primary") # something like "../Results/5.1_Aim2_primary_metaSPARSim"; if required, df_significance is loaded from this folder
    dfFolder <- paste0(sub("/$", "",dfFolder), "_", dataSetFilter)
    dfFolderFiltered <- bs_getPath(analysis+1.2,simulator, nearing = nearing ,partReg = partReg,nameAim2 = "Aim2_primary") # if required, df_significance is loaded from this folder
    dfFolderFiltered <- paste0(sub("/$", "",dfFolderFiltered), "_", dataSetFilter)
    
    outFolder <- bs_getPath(analysis+1,simulator, nearing = nearing ,partReg = partReg,nameAim2 = "Aim2_secondary") # something like "../Results/5.1_Aim2_secondary_metaSPARSim"
    outFolder <- sub(as.character(analysis+1),paste0(as.character(analysis+1),"+",as.character(analysis+1.2)), outFolder, fixed=T)
    outFolder <- paste0(sub("/$", "",outFolder), "_", dataSetFilter)

    source("script_5.x_Aim2_secondary_filtered+nonFiltered.R")
    prefix <- bs_processResultFoldernames(outFolder,"dataName")
  #  if(doPlots)
  #    source("script_5.x_Aim2_Plots.R")
    suppressWarnings(rm(list=c("dataFolder","dfFolder","outFolder","DF","data_to_compare","DF_filt","DF_significance")))
    
  }  
}

cat("\n 5.x_ancom_Nearing.R finished :-) \n")
