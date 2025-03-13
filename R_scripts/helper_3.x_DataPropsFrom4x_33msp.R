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
simulators = c("msp")
analyses = c(3.3)
partRegs = c(T) 
###############################

nearing = F

for(s in 1:length(simulators)){
  simulator <- simulators[s]
  analysis <- analyses[s]
  partReg <- partRegs[s]
  folder3 <- bs_getPath(analysis,simulator, nearing = nearing ,partReg = partReg)
  folder4 <- bs_getPath(analysis+1,simulator, nearing = nearing ,partReg = partReg)

cat("folder4 <- folder3 # folder4 not yet calculated\n")
folder4 <- folder3 # folder4 not yet calculated
  
  cat("analysis=",analysis,"simulator=",simulator,"partReg=",partReg,"\n")
  cat("folder4=",folder4,"\n")
  cat("-> folder3=",folder3,"\n")
  
  file3 <- paste0(folder3,"data_to_compare.RDS")
  file4 <- paste0(folder4,"data_to_compare.RDS")
  
  data_to_compare <- bs_decompress(readRDS(file4))

  # remove DA
  for(j in 1:length(data_to_compare))
    for(k in 1:length(data_to_compare[[j]]))
      if("DA" %in% names(data_to_compare[[j]][[k]]))
        data_to_compare[[j]][[k]]$DA <- NULL

  cat("Saving ",file3,"...\n")
  saveRDS(bs_compress(data_to_compare),file3)


  prefix <- bs_path2prefix(folder3)
  ## Updating data Props:
  resultFolder <- folder3
  cat("prefix=",prefix,"\n")
  source("script_3x_DataProperties.R")
  
  ## Plotting data Props:
  dataFolder <- folder3
  resultFolder <- folder3
  source("script_3.x.x_Plots_DataProps.R")
  
}
