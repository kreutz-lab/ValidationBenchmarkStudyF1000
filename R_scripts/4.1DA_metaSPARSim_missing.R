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

try(dir.create("../Results/4.1_metaSPARSim_DA"))

d <- readRDS("../Results/4.1_metaSPARSim_DA/data_to_compare_tmp.RDS")

# Get input data for metaSPARSim
data_to_compare <- readRDS("../Results/3.1_metaSPARSim_DataProps/data_to_compare.RDS")
data_to_compare <- bs_decompress(data_to_compare)
#data_to_compare <- bs_subset(data_to_compare,c(9,14)-1) # select two small ones

# Subset which is not yet done:
data_to_compare <- bs_subset(data_to_compare,which(names(data_to_compare) %in% setdiff(names(data_to_compare),names(d)))) 

data_to_compare <- bs_checkData(data_to_compare,eliminateZeroSimus = T)


# sort according to size
ndat <- array()
for(i in 1:length(data_to_compare)){
  data_to_compare[[i]]$path <- getwd()
  data_to_compare[[i]]$index <- i
  ndat[i] <- prod(dim(data_to_compare[[i]]$original$counts))
}

# change the order in order to start with the smallest ones:
iorder <- order(ndat)
bstype <- attr(data_to_compare,"bstype")
data_to_compare <- data_to_compare[iorder]
attr(data_to_compare,"bstype") <- bstype



# parallel loop over all datasets
msb.registerDoParallel(min(18,min(parallel::detectCores()-1,length(data_to_compare))))
# registerDoParallel(cores = min(parallel::detectCores(),length(data_to_compare)))

namen <- names(data_to_compare)
for(index in 1:length(data_to_compare)){
  data_to_compare[[index]]$index <- index 
  data_to_compare[[index]]$pfad <- getwd()
}

data_to_compare <- bs_annotateObject(data_to_compare)

#for(i in 1:length(data_to_compare)){
data_to_compare <- foreach(D=data_to_compare) %dopar% {

  tryCatch({
    setwd(D$pfad)
    cat("DA for data_project ",D$index,"\n",file="4.1_foreach.log",append = T)
    
    source("project_init.R")
    # project_init()
    
    D <- bs_decompress(D)
    
    doDA = T
  
    if(doDA){  # DA analysis, time consuming
#      D <- bs_DA(D,whichMethods = "edgeR",maxFeatures = 1000, parallelMode = T)
      D <- bs_DA(D, parallelMode = F, doSlowMethods = T, maxRunTime = 60)
      #D <- bs_DA_linux(D,whichMethods = "glmmTMB",maxFeatures = 5000) # linux, without microbiomeMarker package
      
    }
    
    if(!doDA){ # load d*.RDS from folder "../Results/2023-11-14-Done" because bs_DA were done in parts due to bugs
      
      fils <- list.files("../Results/2023-11-14-Done",pattern="d*RDS",full.names = T)
      namen <- names(D)
      for(i in 1:length(fils)){
        print(fils[i])
        id <- as.numeric(sub("../Results/2023-11-14-Done/d","",strsplit(fils[i],"_")[[1]][1]))
        if(length(id)!=1)
          stop("id could not be determined")
        
        name <- sub(".RDS","",strsplit(fils[i],"[0-9]_")[[1]][2])
        ii <- grep(name,namen)
        if(length(ii)!=1)
          stop("Assignment does not work.")
        
        tmp <- readRDS(fils[i])
        D[[ii]][[id]] <- tmp[[1]]
      }
    }
    
    #saveRDS(D,"../Results/2023_11_14-adding_glmmTMB_d.RDS")
    try(save(list="D",file=paste0("4.1_index",D$index,".Rdata")))
    
    cat("DA for data_project ",D$index,"finished \n",file="4.1_foreach.log",append = T)
    
  }, error=function(e){
    try(save(list="D",file=paste0("Error_4.1_index",D$index,".Rdata")))
    cat("Error (index=",D$index,"): ",conditionMessage(e),"\n",file="4.1_foreach.log",append = T)
  })
  return(D)
}

# doParallel::stopImplicitCluster()
msb.unregisterDoParallel()    
for(i in 1:length(data_to_compare)){
  data_to_compare[[i]]$index <- NULL 
  data_to_compare[[i]]$pfad <- NULL
}
names(data_to_compare) <- namen # foreach returns a list but without names: copy them manually
attr(data_to_compare, "bstype") <- "data_list"

saveRDS(bs_compress(data_to_compare),file="../Results/4.1_metaSPARSim_DA/data_to_compare_tmp2.RDS")


option = "4.1"
source("helper_4.x_collectAndSaveDA.R")
