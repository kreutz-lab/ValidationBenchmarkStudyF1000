# This script estimates simulation parameters for sparseDOSSA2 and saves them as *.RDS
#
# Prerequisites:
# - install package sparsedossa: BiocManager::install("sparseDOSSA")
# - download sparseDossa2 in home directory: git clone https://github.com/biobakery/SparseDOSSA2.git

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

# initialization, i.e. sourcing R_functions, loading package and setting project_path

source("project_init.R")

 d <- readRDS(paste0("../Data/data_to_compare.RDS"))
# d <- readRDS(paste0(project_path,"Data/data_to_compare_testobject.RDS"))
#d <- readRDS("../Data/data_to_compare_200features.RDS")

#d <- readRDS("~/2022_microbiome_da_benchmarking/Analyses/Results/2023_02_24_realvssim/data_to_compare.RDS")

try(
  dir.create("../Results/1.2_sparseDOSSA",showWarnings = F)
)

nfeat <- array()
for(i in 1:length(d)){
  d[[i]]$path <- getwd()
  d[[i]]$index <- i
  nfeat[i] <- dim(d[[i]]$original$counts)[1]
}

# change the order in order to start with the smallest ones:
iorder <- order(nfeat)
bstype <- attr(d,"bstype")
d <- d[iorder]
attr(d,"bstype") <- bstype

doParallel::registerDoParallel(1) #min(parallel::detectCores(),length(d)))

params <- foreach(D = d, .packages = c('digest')) %dopar% {
  tryCatch({
    
    setwd(D$path)
    source("project_init.R")

    if(Sys.info()[["sysname"]]=="Linux")
      anzGB = 100 # size of RAM allocation (be careful on small machines)
    else
      anzGB = 10 # size of RAM allocation (be careful on small machines)
    options(future.globals.maxSize = anzGB*1024 * 1024^2) # allow more RAM usage
    
    # print(paste0("Estimate sparseDOSSA2 Params for", names(counts.list[i]),names(counts.list[[i]][j])))
    cat("One parallel job started ...",as.character(format(Sys.time(), "%X  %b %d %Y")),"\n",file="estimateSparseDossa_foreach.log",append = T)
    cat(D$index,"th data template.\n",file="estimateSparseDossa_foreach.log",append = T)
    
    if(is.character(D$original$counts)){
      D <- bs_decompress(D)
    }
    
    countsInGroups <- bs_countsInGroups(D$original$counts,D$original$meta$condition)
    conds <- names(countsInGroups)
    fits <- list()
    
    for(i in 1:length(countsInGroups)){
      cond <- conds[i]
      time_start <-  as.character(Sys.time())
      SparseDOSSA2_fit <- fit_SparseDOSSA2(data=countsInGroups[[i]], control=list(verbose = TRUE,debug_dir = "./"))
      SparseDOSSA2_fit$checksum <- digest::digest(countsInGroups[[i]])
      SparseDOSSA2_fit$info <- msb.info()
      attr(SparseDOSSA2_fit$info,"bstype") <- "msb.info"
      save(SparseDOSSA2_fit,D,file=paste0("../Results/1.2_sparseDOSSA/estimateSparseDossa_foreach",D$index,as.character(cond),".Rdata"))
      fits[[as.character(cond)]] <- SparseDOSSA2_fit
      fits[[as.character(cond)]]$time_start <- time_start
      fits[[as.character(cond)]]$time_end <-  as.character(Sys.time())
    }
    return(fits)
    
  }, error=function(e){
    try(save(fits,D,file=paste0("Error_in_estimateSparseDossa_foreach",D$index,".Rdata")))
    cat("Error :",conditionMessage(e),"\n")
    cat("Error :",conditionMessage(e),"\n",file="estimateSparseDossa_foreach.log",append = T)
    return(conditionMessage(e))
  })
} 

doParallel::stopImplicitCluster()
names(params) <- names(d)  # foreach returns a list but without names: copy them manually


inFolder <- "../Results/1.2_sparseDOSSA/"
outFile <- paste0("../Results/1.2_sparseDOSSA/paramsSparseDossa2",msb.info()[["date"]],".RDS")
#outFile <- paste0("../Results/1.2_sparseDOSSA/params.RDS")

bs_sparseDossa_makeParamsRDS(inFolder=inFolder,outFile=outFile)

