

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

######## metaSPARSim partReg ########

dfFolder <- "../Results_partReg/3.1_metaSPARSim_DataProps"
outFolder <- "../Results_partReg/5.1_Aim1_metaSPARSim"
if(dir.exists(outFolder) && dir.exists(dfFolder)){
  source("script_5.x_Aim1_EquivalenceTests.R")
}else{
  warning(outFolder, " or ", dfFolder, "does not exist !!!!!!!!!!!!")
}

dfFolder <- "../Results_partReg/3.2_metaSPARSim_ZerosAdded_DataProps"
outFolder <- "../Results_partReg/5.1_Aim1_metaSPARSim_ZerosAdded"
if(dir.exists(outFolder) && dir.exists(dfFolder)){
  source("script_5.x_Aim1_EquivalenceTests.R")
}else{
  warning(outFolder, " or ", dfFolder, "does not exist !!!!!!!!!!!!")
}

dfFolder <- "../Results_partReg/3.3_metaSPARSim_Filtered_DataProps"
outFolder <- "../Results_partReg/5.1_Aim1_metaSPARSim_filtered"
if(dir.exists(outFolder) && dir.exists(dfFolder)){
  source("script_5.x_Aim1_EquivalenceTests.R")
}else{
  warning(outFolder, " or ", dfFolder, "does not exist !!!!!!!!!!!!")
}

dfFolder <- "../Results_partReg/3.4_metaSPARSim_filtered_ZerosAdded_DataProps"
outFolder <- "../Results_partReg/5.1_Aim1_metaSPARSim_filtered_ZerosAdded"
if(dir.exists(outFolder) && dir.exists(dfFolder)){
  source("script_5.x_Aim1_EquivalenceTests.R")
}else{
  warning(outFolder, " or ", dfFolder, "does not exist !!!!!!!!!!!!")
}

######## SparseDOSSA partReg ########

dfFolder <- "../Results_partReg/3.1_sparseDOSSA_DataProps"
outFolder <- "../Results_partReg/5.1_Aim1_sparseDOSSA"
if(dir.exists(outFolder) && dir.exists(dfFolder)){
  source("script_5.x_Aim1_EquivalenceTests.R")
}else{
  warning(outFolder, " or ", dfFolder, "does not exist !!!!!!!!!!!!")
}

dfFolder <- "../Results_partReg/3.2_sparseDOSSA_ZerosAdded_DataProps"
outFolder <- "../Results_partReg/5.1_Aim1_sparseDOSSA_ZerosAdded"
if(dir.exists(outFolder) && dir.exists(dfFolder)){
  source("script_5.x_Aim1_EquivalenceTests.R")
}else{
  warning(outFolder, " or ", dfFolder, "does not exist !!!!!!!!!!!!")
}

dfFolder <- "../Results_partReg/3.3_sparseDOSSA_Filtered_DataProps"
outFolder <- "../Results_partReg/5.1_Aim1_sparseDOSSA_filtered"
if(dir.exists(outFolder) && dir.exists(dfFolder)){
  source("script_5.x_Aim1_EquivalenceTests.R")
}else{
  warning(outFolder, " or ", dfFolder, "does not exist !!!!!!!!!!!!")
}

dfFolder <- "../Results_partReg/3.4_sparseDOSSA_filtered_ZerosAdded_DataProps"
outFolder <- "../Results_partReg/5.1_Aim1_sparseDOSSA_filtered_ZerosAdded"
if(dir.exists(outFolder) && dir.exists(dfFolder)){
  source("script_5.x_Aim1_EquivalenceTests.R")
}else{
  warning(outFolder, " or ", dfFolder, "does not exist !!!!!!!!!!!!")
}



##
pattern <- "NumberOfNonEquivalentDataProps.RDS"
numberFiles <- c(list.files(path="../Results/",pattern=pattern,recursive = T, full.names = T),
                 list.files(path="../Results_partReg//",pattern=pattern,recursive = T, full.names = T))
numbers <- NULL
for(file in numberFiles){
  tmp <- readRDS(file)
  if(is.null(numbers))
    numbers <- tmp
  else
    numbers <- rbind(numbers, tmp)
}
numbers$PropEquivSignificant <- numbers$NumberEquivSignificant/numbers$NumberEquivTests
row.names(numbers) <- sub("/","",sub("../","",sub(pattern,"",numberFiles)))
saveRDS(numbers,"../Results_partReg/NumberOfNonEquivalentDataProps_all.RDS")
print(numbers)


