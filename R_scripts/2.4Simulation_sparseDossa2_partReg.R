# Script to simulate 10 synthetic datasets for each template with sparseDOSSA2 
# and the partReg option.
#
# Since pvalues are needed an in order to keep additional computations minimal,
# the results from the standard sparseDOSSA simus are used.


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

inFolder <- "../Results/4.2_sparseDOSSA_ZerosAdded_DA" #"../Results/4.1_sparseDOSSA_DA/"
outFolder <- "../Results_partReg/2.1_sparseDOSSA/"
paramsPartReg <- readRDS("../Results/1.2_sparseDOSSA/paramsSparseDossa22024-11-23.RDS")
#paramsPartReg <- readRDS("paramsSparseDossa2_partReg2024-11-17.RDS")

# Get input data for metaSPARSim
d <- bs_decompress(readRDS(paste0(inFolder,"/data_to_compare.RDS")))
#d <- bs_subset(d,c(9,14)) # select two small ones

for(i in 1:length(d)){

  D <- bs_subset(d,i)
  meta <- D[[1]]$original$meta
  meta$condition <- "all"
  D[[1]]$original$meta <- meta

  # remove simus in D:
  raus <- c()
  namen <- names(D[[1]])
  for(j in 1:length(namen)){
    if(bs_isa(D[[1]][[namen[j]]],"sim_result"))
      raus <- c(raus,namen[j])
  }
  for(j in raus)
    D[[1]][[j]] <- NULL
  
  params <- NULL
  try(params <- bs_paramsDefault_SparseDOSSA2(D[[1]],paramsWorkspace=paramsPartReg)) # use loaded paramsPartReg
  
  for(j in 1:length(d[[i]])){
    if(bs_isa(d[[i]][[j]],"sim_result")){# only replace simulated data in sim_result
      
      simReg <- d[[i]][[j]]$counts
      
      if(!is.null(params)){
        simNoReg <- bs_SparseDOSSA2(template=params,new_features=F)
        simPartReg <- as.data.frame(simNoReg$counts)
        names(simPartReg) <- names(simReg)
        
        # The following line is correctly placed (it should be called several time to have different random results)
        isReg <- bs_pvals2isReg(pvals = d[[i]]$original$DA$p_value,counts = d[[i]]$original$counts)  
        simPartReg[which(isReg),] <- simReg[which(isReg),]
      
        d[[i]][[j]]$counts <- simPartReg # overwrite with partReg
      }else{
        warning("No params found for i=",i)
      }
    } # only replace simulated data in sim_result
  } # loop over all data sets  
} # loop over all templates


saveRDS(bs_compress(d),file=paste0(outFolder,"/data_to_compare.RDS"))

