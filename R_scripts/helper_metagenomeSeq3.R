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

files_list <- c("../Results_partReg/4.4_metaSPARSim_filtered_ZerosAdded_DA/data_to_compare.RDS",
                "../Results_partReg/4.2_metaSPARSim_ZerosAdded_DA/data_to_compare.RDS",        
                "../Results/4.4_sparseDOSSA_filtered_ZerosAdded_DA/data_to_compare.RDS",
                "../Results/4.2_sparseDOSSA_ZerosAdded_DA/data_to_compare.RDS")
#files_list <- files_list[4]


#files_list <- c(list.files(path = "../Results", pattern = "data_to_compare\\.RDS$", recursive = TRUE, full.names = TRUE),
#                list.files(path = "../Results_partReg", pattern = "data_to_compare\\.RDS$", recursive = TRUE, full.names = TRUE))

#files_list <- files_list[grep("/4.",files_list,T)]
#files_list <- files_list[grep("Results_partReg",files_list,T)]

#warning("Only 4.4_sparseDossa!")
#files_list <- files_list[grep("4.4_sp",files_list)]

whichTest <- c("metagenomeSeq")

for(file in files_list){
  try({
    if(!file.exists(sub(".RDS",".2.RDS",file))){
      
      cat(whichTest, " will be repeated for ",file," ...\n")
      d <- readRDS(file)
      d <- bs_decompress(d)
      d1 <- bs_DA(d, overwriteOldDA=T, parallelMode = T, doSlowMethods = T, maxRunTime = 60, whichMethods=whichTest)
      
      saveRDS(d1, file="d1_tmp.RDS")
      for(j in 1:length(d1)){
        for(k in 1:length(d1[[j]])){
          if(bs_isa(d[[j]][[k]],c("data_template","sim_result"))){
            if(!"DA" %in% names(d[[j]][[k]]))
              d[[j]][[k]]$DA <- list()
            
            if(!"logFold" %in% names(d[[j]][[k]]$DA)){
              try(d[[j]][[k]]$DA$logFold <- d1[[j]][[k]]$DA$logFold)
              try(d[[j]][[k]]$DA$p_value <- d1[[j]][[k]]$DA$p_value)
              try(d[[j]][[k]]$DA$p_adjusted <- d1[[j]][[k]]$DA$p_adjusted)
            }else{
              
              if(whichTest %in% colnames(d[[j]][[k]]$DA$logFold)){
                try(d[[j]][[k]]$DA$logFold[,whichTest] <- d1[[j]][[k]]$DA$logFold)
                try(d[[j]][[k]]$DA$p_value[,whichTest] <- d1[[j]][[k]]$DA$p_value)
                try(d[[j]][[k]]$DA$p_adjusted[,whichTest] <- d1[[j]][[k]]$DA$p_adjusted)
              }else{
                try(cbind(d[[j]][[k]]$DA$logFold, setNames(d1[[j]][[k]]$DA$logFold,whichTest)))
                try(cbind(d[[j]][[k]]$DA$p_value, setNames(d1[[j]][[k]]$DA$p_value,whichTest)))
                try(cbind(d[[j]][[k]]$DA$p_adjusted,setNames(d1[[j]][[k]]$DA$p_adjusted,whichTest)))
              }
            }
          }
        }
      }
      
      sink(sub("data_to_compare.RDS","bs_checkDA.log",file))
      try(d <- bs_checkData(d))
      try(print(bs_checkDA(d)))
      sink()
      
      saveRDS(bs_compress(d),file=sub(".RDS",".2.RDS",file))
      rm(d)
      gc()
      cat(whichTest, " is finished for ",file," :-) \n")
    }  
  })
}
