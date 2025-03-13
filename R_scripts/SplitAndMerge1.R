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



#d <- readRDS("../Results/3.1_metaSPARSim_DataProps/data_to_compare.RDS")
#d <- bs_decompress(d)
d <- bs_getData()

d <- bs_subset(d,c(4,6,8,9,18))
#d <- bs_subset(d,c(9,18))
d <- bs_annotateObject(d)
d <- bs_DA(d,doSlowMethods = T,maxRunTime = 60,parallelMode = T, keepTestResult = T, doMemoryCount=T)

ks <- c(2,3,5,10)
dsplit <- list()
for(ik in 1:length(ks)){
  k <- ks[ik]
  dsplit[[ik]] <- bs_SM_split(d,k=k)
  dsplit[[ik]] <- bs_DA(dsplit[[ik]],doSlowMethods = T,maxRunTime = 60,parallelMode = T, keepTestResult = T,doMemoryCount=T)
  dsplit[[ik]] <- bs_SM_merge(dsplit[[ik]])
}

try(dir.create("../Results/SplitAndMerge",showWarnings = F))

saveRDS("../Results/SplitAndMerge/data_to_compare.RDS")
saveRDS(dsplit,"../Results/SplitAndMerge/dsplit.RDS")

