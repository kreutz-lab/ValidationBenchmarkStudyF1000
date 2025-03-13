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




 files_list <- list.files(path = "../Results", pattern = "data_to_compare\\.RDS$", recursive = TRUE, full.names = TRUE)
#files_list <- list.files(path = "../Results_partReg", pattern = "data_to_compare\\.RDS$", recursive = TRUE, full.names = TRUE)

# files_list <- files_list[grep("4.2_me",files_list,T)]
 
 warning("No 1.1*")
 files_list <- files_list[grep("1.1",files_list,fixed=T,invert=T)]
 warning("No 2.*")
 files_list <- files_list[grep("2.",files_list,fixed=T,invert=T)]
 
for(file in files_list){
  try({
    print(file)
    d <- readRDS(file)
    d <- bs_decompress(d)
    
    d <- bs_addDataProp(d)

    saveRDS(bs_compress(d),file=file)
    rm(d)
    gc()
 })
  
}
