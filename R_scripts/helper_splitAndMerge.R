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




# files_list <- list.files(path = "../Results", pattern = "data_to_compare\\.RDS$", recursive = TRUE, full.names = TRUE)
#files_list <- list.files(path = "../Results_partReg", pattern = "data_to_compare\\.RDS$", recursive = TRUE, full.names = TRUE)

# files_list <- files_list[grep("Results/4",files_list)]

# warning("Only sparseDossa!")
# files_list <- files_list[grep("spars",files_list)]

#folder_list <- c("../Results/4.2_sparseDOSSA_ZerosAdded_DA/","../Results/4.4_sparseDOSSA_filtered_ZerosAdded_DA/")

#folder_list <- c("../Results_partReg/4.2_metaSPARSim_ZerosAdded_DA/")
#folder_list <- c("../Results_partReg/4.4_metaSPARSim_filtered_ZerosAdded_DA/")

folder_list <- c("../Results_partReg/4.2_metaSPARSim_ZerosAdded_DA/",
  "../Results_partReg/4.4_metaSPARSim_filtered_ZerosAdded_DA/")

#folder_list <- c("../Results/4.3_metaSPARSim_filtered_DA/",
#                 "../Results/4.4_metaSPARSim_filtered_ZerosAdded_DA/")


for(folder in folder_list){
  file <- paste0(folder,"data_to_compare.RDS")
  #  try({
  print(file)
  d <- readRDS(file)
  d <- bs_decompress(d)
  
  namen <- names(d)
  
  allTests <- c()
  for(j in 1:length(d))
    for(k in 1:length(d[[j]]))
      if("DA" %in% names(d[[j]][[k]])){
        tests <- colnames(d[[j]][[k]]$DA$p_value)
        allTests <- unique(c(allTests,tests))
      }

  allTests <- setdiff(allTests,"")  # eliminate empty colname
  
  for(j in 1:length(d)){
    splitRequired <- F
    
    for(k in 1:length(d[[j]])){
      if("DA" %in% names(d[[j]][[k]])){

        # eliminate columns with missing colname        
        drin <- which("" != colnames(d[[j]][[k]]$DA$logFold))
        d[[j]][[k]]$DA$logFold <- d[[j]][[k]]$DA$logFold[,drin]
        drin <- which("" != colnames(d[[j]][[k]]$DA$p_value))
        d[[j]][[k]]$DA$p_value <- d[[j]][[k]]$DA$p_value[,drin]
        drin <- which("" != colnames(d[[j]][[k]]$DA$p_adjusted))
        d[[j]][[k]]$DA$p_adjusted <- d[[j]][[k]]$DA$p_adjusted[,drin]
        
        ## Be sure that p_adjust fits to p_value:
        # 28.12.24: I don't understand why it is missing for j=9, k=9, test=metagenomeSeq
        # The following lines are a nasty workaround:
        d[[j]][[k]]$DA$p_adjusted <- NA*d[[j]][[k]]$DA$p_value
        tests <- colnames(d[[j]][[k]]$DA$p_value)
        for(test in tests){
          d[[j]][[k]]$DA$p_adjusted[,test] <- p.adjust(d[[j]][[k]]$DA$p_value[,test])
        }
        
        
        testNames <- colnames(d[[j]][[k]]$DA$p_value)
        missingTests <- setdiff(allTests,testNames)
        if(length(missingTests)>0){
          for(test in missingTests){
            cat(j,k,": The following test is missing:",test,"\n")
            
            testNames <- c(testNames,test)
            d[[j]][[k]]$DA$logFold <- cbind(d[[j]][[k]]$DA$logFold,array(NA,dim=nrow(d[[j]][[k]]$DA$logFold)))
            d[[j]][[k]]$DA$p_value <- cbind(d[[j]][[k]]$DA$p_value,array(NA,dim=nrow(d[[j]][[k]]$DA$p_value)))
            d[[j]][[k]]$DA$p_adjusted <- cbind(d[[j]][[k]]$DA$p_adjusted,array(NA,dim=nrow(d[[j]][[k]]$DA$p_adjusted)))
          }
          colnames(d[[j]][[k]]$DA$logFold) <- testNames
          colnames(d[[j]][[k]]$DA$p_value) <- testNames
          colnames(d[[j]][[k]]$DA$p_adjusted) <- testNames
        }
        
        for(test in colnames(d[[j]][[k]]$DA$p_value)){
          cat(j,k,test,": splitting required: ",sum(!is.na(d[[j]][[k]]$DA$p_value[,test]))==0,"\n")
          
          if(sum(!is.na(d[[j]][[k]]$DA$p_value[,test]))==0){
            
            splitRequired <- T
            
            d_j <- msb.subset(d,j)
            d_j[[1]] <- msb.subset(d_j[[1]],k)
            
            saveRDS(d_j,"BeforeSplitAndMerg.RDS")
            print(paste0("Split & merge for ",namen[j],"j = ",j, ", k=",k, " (test=",test,") started..."))
            
            d_j1 <- d_j
            d_j2 <- d_j
            
            rows1 <- seq(1,nrow(d_j1[[1]][[1]]$counts),by=2)
            rows2 <- seq(2,nrow(d_j1[[1]][[1]]$counts),by=2)
            
            d_j1[[1]][[1]]$counts <- d_j1[[1]][[1]]$counts[rows1,]
            d_j2[[1]][[1]]$counts <- d_j2[[1]][[1]]$counts[rows2,]
            
            
            # Define the function to run in parallel
            # d_j1 <- bs_DA(d_j1, overwriteOldDA=T, parallelMode = F, doSlowMethods = T, maxRunTime = 60, whichMethods=c(test), keepTestResult = F)
            # d_j2 <- bs_DA(d_j2, overwriteOldDA=T, parallelMode = F, doSlowMethods = T, maxRunTime = 60, whichMethods=c(test), keepTestResult = F)
            run_bs_DA <- function(d_j, test) {
              bs_DA(d_j, overwriteOldDA=T, parallelMode=F, doSlowMethods=T, maxRunTime=60, whichMethods=c(test), keepTestResult=F)
            }
            data_list <- list(d_j1, d_j2)
            # Run in parallel
            # library(parallel)
            results <- mclapply(data_list, run_bs_DA, test, mc.cores = 2)  # Adjust mc.cores based on your CPU
            # Extract results
            d_j1 <- results[[1]]
            d_j2 <- results[[2]]
            
            try(d[[j]][[k]]$DA$logFold[rows1,test] <- d_j1[[1]][[1]]$DA$logFold)
            try(d[[j]][[k]]$DA$p_value[rows1,test] <- d_j1[[1]][[1]]$DA$p_value)
            # try(d[[j]][[k]]$DA$p_adjusted[rows1,test] <- d_j1[[1]][[1]]$DA$p_adjusted)
            
            try(d[[j]][[k]]$DA$logFold[rows2,test] <- d_j2[[1]][[1]]$DA$logFold)
            try(d[[j]][[k]]$DA$p_value[rows2,test] <- d_j2[[1]][[1]]$DA$p_value)
            # try(d[[j]][[k]]$DA$p_adjusted[rows2,test] <- d_j2[[1]][[1]]$DA$p_adjusted)
            
            # Do adjusting for from all combined p_values:
            try(d[[j]][[k]]$DA$p_adjusted[,test] <- p.adjust(d[[j]][[k]]$DA$p_value[,test],method = "BH"))
            
            print(paste0("Split & merge for ",namen[j],"j = ",j, ", k=",k, " done. Now NAs in pvalues? ",sum(!is.na(d[[j]][[k]]$DA$p_value[,test]))," (out of ",nrow(d[[j]][[k]]$DA$p_value),")"))
            if(is.null(d[[j]][[k]]$DA$splitTests))
              d[[j]][[k]]$DA$splitTests <- c()
            d[[j]][[k]]$DA$splitTests <- c(d[[j]][[k]]$DA$splitTests,test)

          }
        }
      }
    }
    if(splitRequired)
      saveRDS(d,file="LastSplitAndMerge_d.RDS")
  }
  
  sink(sub("data_to_compare.RDS","bs_checkDA.sm.log",file))
  try(d <- bs_checkData(d))
  try(print(bs_checkDA(d)))
  sink()
  
  saveRDS(bs_compress(d),file=sub(".RDS",".sm.RDS",file))
  rm(d)
  gc()

  cat("splitAndMerge finished for file",file,"\n")  
  
} # all folders
