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

files_list <- c("../Results_partReg/4.4_metaSPARSim_filtered_ZerosAdded_DA/data_to_compare.2.RDS",
                "../Results_partReg/4.2_metaSPARSim_ZerosAdded_DA/data_to_compare.2.RDS",        
                "../Results/4.4_sparseDOSSA_filtered_ZerosAdded_DA/data_to_compare.2.RDS",       
                "../Results/4.2_sparseDOSSA_ZerosAdded_DA/data_to_compare.2.RDS")
files_list <- files_list[4]

# files_list <- list.files(path = "../Results", pattern = "data_to_compare\\.RDS$", recursive = TRUE, full.names = TRUE)
#files_list <- list.files(path = "../Results_partReg", pattern = "data_to_compare\\.RDS$", recursive = TRUE, full.names = TRUE)
# files_list <- files_list[grep("Results/4.3",files_list,fixed=T)]

#files_list <- grep(".2.RDS",list.files("..","data_to_compare.2.RDS",full.names = T, recursive = T),fixed=T , value=T)


# files_list <- c("../Results/4.2_sparseDOSSA_ZerosAdded_DA/data_to_compare.RDS","../Results/4.4_sparseDOSSA_filtered_ZerosAdded_DA/data_to_compare.RDS")
#../Results/4.3_metaSPARSim_filtered_DA/data_to_compare.RDS",
#                "../Results/4.4_metaSPARSim_filtered_ZerosAdded_DA/data_to_compare.RDS")


# warning("Only metasparsim!")
# files_list <- files_list[grep("meta",files_list)]

for(file in files_list){
  #  try({
  if(file.exists(file)){
    ##### This part is not always necessary
    fileNew <- sub(".2.RDS",".RDS",file,fixed = T)
    cat(file," -> ",fileNew,"\n")
    file.copy(from = file, to = fileNew, overwrite = TRUE)
    file.remove(file)
    #####
    
    d <- readRDS(fileNew)
    d <- bs_decompress(d)
    # d1 <- bs_DA(d, overwriteOldDA=T, parallelMode = T, doSlowMethods = T, maxRunTime = 60, whichMethods=c("Maaslin2_rare"))
    # 
    # saveRDS(d1, file="d1_tmp.RDS")
    # for(j in 1:length(d1)){
    #   for(k in 1:length(d1[[j]])){
    #     if("Maaslin2_rare" %in% colnames(d[[j]][[k]]$DA$logFold)){
    #       try(d[[j]][[k]]$DA$logFold[,"Maaslin2_rare"] <- d1[[j]][[k]]$DA$logFold)
    #       try(d[[j]][[k]]$DA$p_value[,"Maaslin2_rare"] <- d1[[j]][[k]]$DA$p_value)
    #       try(d[[j]][[k]]$DA$p_adjusted[,"Maaslin2_rare"] <- d1[[j]][[k]]$DA$p_adjusted)
    #     }else{
    #       try(cbind(d[[j]][[k]]$DA$logFold,Maaslin2_rare = d1[[j]][[k]]$DA$logFold))
    #       try(cbind(d[[j]][[k]]$DA$p_value,Maaslin2_rare = d1[[j]][[k]]$DA$p_value))
    #       try(cbind(d[[j]][[k]]$DA$p_adjusted,Maaslin2_rare = d1[[j]][[k]]$DA$p_adjusted))
    #     }
    #   }
    # }
    # 
    # sink(sub("data_to_compare.RDS","bs_checkDA.log",file))
    # try(d <- bs_checkData(d))
    # try(print(bs_checkDA(d)))
    # sink()
    
    DF <- bs_DA2dataFrame(d)
    saveRDS(DF,file=sub("data_to_compare.RDS","DF.RDS",fileNew))
    
    rm(d)
    gc()
    # })
  }
}
