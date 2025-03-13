# This helper script creates DF.RDS from data_to_compare.RDS in the Results folders specified below

######

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

simulators = c("spd","spd","spd","spd","msp","msp","msp","msp") 
analyses = c(4.1, 4.2, 4.3, 4.4, 4.1, 4.2, 4.3, 4.4)
nearing = F

folders <- c()
for(i in 1:length(simulators)){
  for(partReg in c(T,F)){
    tmpFolder <- bs_getPath(analyses[i], simulators[i],partReg = partReg,nearing=nearing)
    if(file.exists(paste0(tmpFolder,"/data_to_compare.RDS")))
      folders <- c(folders,tmpFolder)
  }
}

cat("folders: \n",folders)

# folders <- c("../Results/4.1_metaSPARSim_DA",
#              "../Results/4.2_metaSPARSim_ZerosAdded_DA",
#              "../Results/4.3_metaSPARSim_filtered_DA",
#              "../Results/4.4_metaSPARSim_filtered_ZerosAdded_DA")

for(targetFolder in folders){
  d <- readRDS(paste0(targetFolder,"/data_to_compare.RDS"))
  d <- bs_decompress(d)
  DF <- bs_DA2dataFrame(d)
  saveRDS(DF,paste0(targetFolder,"/DF.RDS"))
}

