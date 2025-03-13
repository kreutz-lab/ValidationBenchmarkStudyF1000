### This script needs to be run only once in the beginning. 
### It creates the RDS object containing all the files.names.RDS
### Saves information about the count and meta data in info_inputData.xlsx 
### Saves the cleaned meta data objects in Data/meta.data.cleaned
### Saves TSS and GMPR normalized data objects in count.data.norm & count.data.norm_GMPR

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


### Get names of all input files
# Names of all ASV files
files.counts <- list.files("../Data/count.data")
#Extract data name
files.names <- lapply(files.counts, function(x) gsub("\\_ASVs.*","",x))
# Save
saveRDS(files.names,"../Data/files.names.RDS")

# Save all datasets and their respective meta data in one list
allData.list <- list()

for(i in files.names){
  allData.list[[i]]$counts <- readRDS(paste0("../Data/count.data/",i,"_ASVs_table.RDS"))
  allData.list[[i]]$meta <- readRDS(paste0("../Data/meta.data/",i,"_metadata.rds"))
}


# Collect information about count data and meta data. For some data sets the number of samples in count data does not match the number of samples for which there is meta data.
# All data sets have only one column in meta data, indicating the condition

info_inputData <- data.frame(matrix(ncol=7, nrow=0))

for(i in 1:length(allData.list)){
  DataSet <- names(allData.list[i])
  DataName <- DataSet
  N.feature <- nrow(allData.list[[i]]$counts)
  N.samples <- ncol(allData.list[[i]]$counts)
  N.feature.meta <- nrow(allData.list[[i]]$meta)
  N.columns.meta <- ncol(allData.list[[i]]$meta)
  meta.data <- colnames(allData.list[[i]]$meta)
  N.conditions <- length(levels(as.factor((allData.list[[i]]$meta)[[1]])))
  Sparsity <- sum(!allData.list[[i]]$counts>0)/nrow(allData.list[[i]]$counts)/ncol(allData.list[[i]]$counts)
  data.tmp <- c(DataSet,DataName, N.feature,N.samples, N.feature.meta, N.columns.meta,meta.data,N.conditions,Sparsity)
  info_inputData <- rbind(info_inputData,data.tmp)
  
}
colnames(info_inputData) <- c("DataSet","NameInNearing","N.feature","N.samples","N.feature.meta", "N.columns.meta","meta.groupname","N.groups","Sparsity")
info_inputData$Sparsity <- round(as.numeric(info_inputData$Sparsity),3)

xls <- openxlsx::read.xlsx("../Data/SampleAnnotation.xlsx")
namesNearing <- xls$Dataset
names(namesNearing) <- xls$data_template # our names
namesNearing <- namesNearing[!is.na(names(namesNearing))]
for(i in 1:length(namesNearing[i])){
  ind <- grep(names(namesNearing)[i],info_inputData$NameInNearing)
  info_inputData$NameInNearing[ind] <- namesNearing[i]
}
openxlsx::write.xlsx(info_inputData,"../Data/info_inputData.xlsx")

# Create directory to store cleaned meta data
try(dir.create("../Data/meta.data.cleaned"))

# Save cleaned meta data object. Cleaned means, that all rows for which there is no count data are being removed. 
for(i in 1:length(allData.list)){
  counts <- allData.list[[i]]$counts
  meta <- allData.list[[i]]$meta
  colnames(meta) <- "condition"
  meta$condition <- as.factor(meta$condition)
  # Remove meta data for which there are no counts
  shared <- which(rownames(meta) %in% colnames(counts))
  meta <- meta[shared, , drop=FALSE]
  
  saveRDS(meta, paste0("../Data/meta.data.cleaned/",names(allData.list[i]),"_metadata.RDS"))
}

#### Normalize data with TSS
try(dir.create("../Data/count.data.norm"))

for(i in 1:length(allData.list)){
  counts <- allData.list[[i]]$counts
  counts.norm <- apply(counts, 2, function(x){x/sum(x)})
  
  saveRDS(counts.norm, paste0("../Data/count.data.norm/",names(allData.list[i]),"_ASVs_norm.RDS"))
}

#### Normalize data with GMPR
try(dir.create("../Data/count.data.norm_GMPR"))

seed <- .Random.seed

for(i in 1:length(allData.list)){
  counts <- as.matrix(allData.list[[i]]$counts)
  
  set.seed(123)
  gmpr.size.factors <- GMPR(counts, ct.min = 1, trace = TRUE, nsamples=Inf, nfeature=20000)
  #  gmpr.size.factors <- GMPR(counts, ct.min = 1, trace = TRUE, nsamples=Inf, nfeature=1000)
  
  if(length(gmpr.size.factors)!=ncol(counts)){ # GMPR failed
    
    warning("i=",i,": length(gmpr.size.factors)!=ncol(counts) => use count.data.norm without GMPR instead.")
    saveRDS(counts.norm, paste0("../Data/count.data.norm_GMPR/",names(allData.list[i]),"_ASVs_norm.RDS"))
    sink("GMPR_failed.log")
    Sys.time()
    print(files.names[i])
    sink()
    
  } else {
    
    gmpr.size.factors[is.na(gmpr.size.factors)] <- median(gmpr.size.factors,na.rm=T) # size factors for columns without enough overlap
    counts.norm <- t(t(counts) / gmpr.size.factors)
    saveRDS(counts.norm, paste0("../Data/count.data.norm_GMPR/",names(allData.list[i]),"_ASVs_norm.RDS"))
    
  }
}
.Random.seed <- seed


# Check whether NA in GMPR normalized data:
anzNA2 <- array()
for(i in 1:length(allData.list)){
  tmp <- readRDS(paste0("../Data/count.data.norm_GMPR/",names(allData.list[i]),"_ASVs_norm.RDS"))
  anzNA2[i] <- sum(is.na(tmp))
}

if(sum(anzNA2>0)>0)
  warning("NA in the following data sets:",which(anzNA2>0))
