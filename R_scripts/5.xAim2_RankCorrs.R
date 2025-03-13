# Script to analyze the results from differential abundance tests on metaSPARSim data
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



# This script searches all df_H_true_all.RDS for
patternSim <- "simEquiv"
patternExp <- "exp"
searchFolder <- "../Results/"
searchFile <- "df_H_true_all.RDS"


dfHtrue_sim <- grep(patternSim,list.files(searchFolder,searchFile,recursive = T,full.names = T),value = T)
dfHtrue_exp <- gsub(patternSim,patternExp,dfHtrue_sim)

dfsRankCor <- list()
for(i in 1:length(dfHtrue_sim)){
#  try(dfsRankCor[[i]] <- bs_correlateMismatch(dfHtrue_exp[i],dfHtrue_sim[i]))
  dfsRankCor[[i]] <- bs_correlateMismatch(dfHtrue_exp[i],dfHtrue_sim[i])
}

names(dfsRankCor) <- dfHtrue_sim

saveRDS(dfsRankCor,file = paste0(searchFolder,"/","dfsRankCor.RDS"))

cat("\n5.xAim2_RankCorrs.R finished :-)\n\n\n")
