# Script to simulate 10 synthetic datasets for each template with metaSPARSim

rm(list=ls())


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

propRegs <- seq(0,1,length.out=6) # does not play a role, here
# propRegs <- seq(0,1,length.out=3)


# Get input data for metaSPARSim
data_to_compare <- bs_getData()

data_to_compare <- bs_subset(data_to_compare,!names(data_to_compare)=="ArcticFreshwaters")

data_to_compare <- data_to_compare[rep(9,length(propRegs))] # select small one
names(data_to_compare) <- paste("propReg",propRegs,sep="")
attr(data_to_compare,"bstype") <-"data_list"

gc()

# Create directory to save simulated data
outFolder <- "../Results_partReg/2.1_metaSPARSim_Test"

simulator <- "metasparsim_nonreg"
simulator <- "metasparsim_partreg"

prefix <- paste0("2.3_",simulator)

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
  data_to_compare[[index]]$propReg <- propRegs[index]
  data_to_compare[[index]]$simulator <- simulator
  data_to_compare[[index]]$paramsWorkspace <- paramsWorkspace
}

data_to_compare <- foreach(D=data_to_compare) %do% {
  # tryCatch({
    setwd(D$pfad)
    index <- D$index
    propReg <- D$propReg
    simulator <- D$simulator
    paramsWorkspace <- D$paramsWorkspace
    
    cat(D$pfad,"\n",file=paste0(prefix,"LoopOverTemplates.log"),append = T)
    
    source("project_init.R")
    # project_init()
  
    D <- NULL
    if(simulator == "metasparsim_nonreg"){
      source("bs_simulateData_old.R")
      Dold <- bs_simulateData_old(list(D), nsim=1, simulator="metasparsim_nonreg", 
                                  file=".", parallelMode = F)
      D <- bs_simulateData_old(list(D), nsim=1, simulator=simulator, 
                               file=".", parallelMode = F)
      print("bs_simulateData_old...")
    }
    if(simulator == "metasparsim_partreg"){
      Dnew <- bs_simulateData(list(D), nsim=1, simulator="metasparsim_partreg", 
                           file=".", parallelMode = F, 
                           paramsWorkspace = paramsWorkspace)
      D <- bs_simulateData(list(D), nsim=1, simulator=simulator, 
                           file=".", parallelMode = F, 
                           paramsWorkspace = paramsWorkspace)
      print("bs_simulateData...")
    }
    D <- D[[1]]
    
    saveRDS(D,file=paste0(outFolder,"/data_to_compare_",index,".RDS"))
    
  # }, error=function(e){
  #   try(save(list="D",file=paste0("Error_",prefix,"index",index,".Rdata")))
  #   cat("Error (index=",index,"): ",conditionMessage(e),"\n",file=paste0(prefix,"LoopOverTemplates.log"),append = T)
  # })
  return(D)
}

names(data_to_compare) <- paste("propReg",propRegs)
attr(data_to_compare,"bstype") <-"data_list"

# d <- bs_addDataProp(d)
D <- bs_annotateObject(data_to_compare)
D <- bs_addDataProp(D)
D <- bs_diffDataProp(D)

DF <- bs_summarize_dataPropCmp(D)

DF$comparison <- gsub("metaSPARSim","mSS",DF$comparison)
DF$comparison <- gsub("sparseDOSSA2","spD",DF$comparison)
DF$comparison <- gsub("original","orig",DF$comparison)


# prefix <- "3.1test"
resultFolder <- outFolder
# source("script_3x_DataProperties.R")

# source("script_3x_DataProperties.R")
pdf(file = paste0(resultFolder,"/dataPropCmp_yfree_",simulator,".pdf"),width = 18,height=12)
try(  print(bs_plot_dataPropCmp(DF,scales="free_y")) )
dev.off()



msb.unregisterDoParallel()
# for(index in 1:length(data_to_compare)){
#   data_to_compare[[index]]$name <- NULL
#   data_to_compare[[index]]$index <- NULL 
#   data_to_compare[[index]]$pfad <- NULL
# }
# names(data_to_compare) <- namen # foreach returns a list but without names: copy them manually
# attr(data_to_compare, "bstype") <- "data_list"
# 
# # Add information about sim meta data
# data_to_compare <- bs_addSimulationMetaData(data_to_compare)
# # Compress
# data_to_compare <- bs_compress(data_to_compare)
# saveRDS(data_to_compare,file=paste0(outFolder,"/data_to_compare.RDS"))

saveRDS(D,file=paste0(outFolder,"/2.1partReg_testen_",simulator,".RDS"))
