# Script to simulate 10 synthetic datasets for each template with metaSPARSim


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



# Get input data for metaSPARSim
data_to_compare <- bs_getData()

data_to_compare <- bs_subset(data_to_compare,!names(data_to_compare)=="ArcticFreshwaters")

#data_to_compare <- bs_subset(data_to_compare,c(9,14)) # select two small ones



# Create directory to save simulated data
outFolder <- "../Results_partReg/2.1_metaSPARSim"
prefix <- "2.3_"
simulator <- "metasparsim_partReg"

paramsWorkspace <- list(reg="../Results/1.1_metaSPARSim/params.RDS",  # regulated
                        nonreg="../Results_partReg/1.1_metaSPARSim/params.RDS") # unregulated

try(dir.create(outFolder))

msb.registerDoParallel(min(parallel::detectCores()-1,length(data_to_compare)))
# registerDoParallel(cores = min(parallel::detectCores(),length(data_to_compare)))

namen <- names(data_to_compare)
for(index in 1:length(data_to_compare)){
  data_to_compare[[index]]$name <- namen[index]
  data_to_compare[[index]]$index <- index 
  data_to_compare[[index]]$pfad <- getwd()
  data_to_compare[[index]]$simulator <- simulator
  data_to_compare[[index]]$paramsWorkspace <- paramsWorkspace
}

data_to_compare <- foreach(D=data_to_compare) %dopar% {
  tryCatch({
    setwd(D$pfad)
    index <- D$index
    simulator <- D$simulator
    paramsWorkspace <- D$paramsWorkspace
    
    cat(D$pfad,"\n",file=paste0(prefix,"LoopOverTemplates.log"),append = T)
    
    source("project_init.R")
    # project_init()
    
    D <- bs_simulateData(list(D), nsim=10, simulator=simulator, 
                         file=".", parallelMode = T, 
                         paramsWorkspace = paramsWorkspace)
    D <- D[[1]]
    
    saveRDS(D,file=paste0(outFolder,"/data_to_compare_",index,".RDS"))
    
  }, error=function(e){
    try(save(list="D",file=paste0("Error_",prefix,"index",index,".Rdata")))
    cat("Error (index=",index,"): ",conditionMessage(e),"\n",file=paste0(prefix,"LoopOverTemplates.log"),append = T)
  })
  return(D)
}

msb.unregisterDoParallel()    
for(index in 1:length(data_to_compare)){
  data_to_compare[[index]]$name <- NULL
  data_to_compare[[index]]$index <- NULL 
  data_to_compare[[index]]$pfad <- NULL
}
names(data_to_compare) <- namen # foreach returns a list but without names: copy them manually
attr(data_to_compare, "bstype") <- "data_list"
data_to_compare <- bs_annotateObject(data_to_compare)

# Add information about sim meta data
data_to_compare <- bs_addSimulationMetaData(data_to_compare)
# Compress
data_to_compare <- bs_compress(data_to_compare)
saveRDS(data_to_compare,file=paste0(outFolder,"/data_to_compare.RDS"))


