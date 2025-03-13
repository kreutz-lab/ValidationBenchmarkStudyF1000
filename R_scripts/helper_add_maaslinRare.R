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
 
 warning("Only 4.4_sparseDossa!")
 files_list <- files_list[grep("4.4_sp",files_list)]
 
for(file in files_list){
  try({
    print(file)
    d <- readRDS(file)
    d <- bs_decompress(d)
    d1 <- bs_DA(d, overwriteOldDA=T, parallelMode = T, doSlowMethods = T, maxRunTime = 60, whichMethods=c("Maaslin2_rare"))

    saveRDS(d1, file="d1_tmp.RDS")
    for(j in 1:length(d1)){
      for(k in 1:length(d1[[j]])){
	if(!"DA" %in% names(d[[j]][[k]]))
           d[[j]][[k]]$DA <- list()

        if(!"logFold" %in% names(d[[j]][[k]]$DA)){
           try(d[[j]][[k]]$DA$logFold <- d1[[j]][[k]]$DA$logFold)
           try(d[[j]][[k]]$DA$p_value <- d1[[j]][[k]]$DA$p_value)
           try(d[[j]][[k]]$DA$p_adjusted <- d1[[j]][[k]]$DA$p_adjusted)
        }else{

        if("Maaslin2_rare" %in% colnames(d[[j]][[k]]$DA$logFold)){
          try(d[[j]][[k]]$DA$logFold[,"Maaslin2_rare"] <- d1[[j]][[k]]$DA$logFold)
          try(d[[j]][[k]]$DA$p_value[,"Maaslin2_rare"] <- d1[[j]][[k]]$DA$p_value)
          try(d[[j]][[k]]$DA$p_adjusted[,"Maaslin2_rare"] <- d1[[j]][[k]]$DA$p_adjusted)
        }else{
          try(cbind(d[[j]][[k]]$DA$logFold,Maaslin2_rare = d1[[j]][[k]]$DA$logFold))
          try(cbind(d[[j]][[k]]$DA$p_value,Maaslin2_rare = d1[[j]][[k]]$DA$p_value))
          try(cbind(d[[j]][[k]]$DA$p_adjusted,Maaslin2_rare = d1[[j]][[k]]$DA$p_adjusted))
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
 })
  
}
