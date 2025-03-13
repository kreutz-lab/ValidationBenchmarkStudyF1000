## This script adds ASV count.tables from the download of the Nearing paper to our data folder:
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


try(file.remove("0.0_MakeCountTables.log"))

sink("0.0_MakeCountTables.log")


if(sys_info[["sysname"]]=="Windows")
  nearingFolder <- "e:/clemens/Projekte/_Mikrobiom/Nearing22/Hackathon/Studies/"
if(sys_info[["sysname"]]=="Darwin")
  nearingFolder <- ""
if(sys_info[["sysname"]]=="Linux")
  nearingFolder <- "~ckreutz/Nearing22/Hackathon/Studies/" # funktioniert auch für Eva


folders <- list.files(nearingFolder)
nearingFiles <- array()
for(i in 1:length(folders)){
  nearingFiles[i] <- paste0(folders[i],"/",folders[i],"_ASVs_table.tsv")
}

for(i in 1:length(nearingFiles)){
  nearingFile <- nearingFiles[i]
  
  # Read data from Nearing:
  n <- NULL
  try(n <- read.table(paste0(nearingFolder,nearingFile), sep="\t", header=TRUE, comment.char=""), silent = T)
  if(is.null(n))# sometimes the first line has to be skipped
    n <- read.table(paste0(nearingFolder,nearingFile), sep="\t", header=TRUE, comment.char="",skip = 1)
  
  if(is.character(n[2,2])){
    print("char for ",i)
  }
  
  nM <- n[,-1]
  row.names(nM) <- n[,1]
  
  # meta data
  metaFile <- sub("_ASVs_table.tsv","_metadata.csv",nearingFile)
  meta <- NULL
  if(file.exists(paste0(nearingFolder,metaFile)))
    meta <- read.table(paste0(nearingFolder,metaFile), sep="\t", header=TRUE, comment.char="")
  if(is.null(meta) && file.exists(sub("data.csv",".tsv",paste0(nearingFolder,metaFile)))) # try whether this exists
    meta <- read.table(sub("data.csv",".tsv",paste0(nearingFolder,metaFile)), sep="\t", header=TRUE, comment.char="")
  if(is.null(meta) && file.exists(sub("csv","tsv",paste0(nearingFolder,metaFile)))) # try whether this exists
    meta <- read.table(sub("csv","tsv",paste0(nearingFolder,metaFile)), sep="\t", header=TRUE, comment.char="")
  if(is.null(meta))
    stop("meta data not found")
  
  # custom replacement steps for some data sets:
  # if(nearingFile=="asd_son/asd_son_ASVs_table.tsv")
  #   meta[,1] <- sub("-",".",meta[,1])
  # if(nearingFile=="cdi_vincent/cdi_vincent_ASVs_table.tsv")
  #   meta[,1] <- sub("-",".",meta[,1])
  # if(nearingFile=="crc_zeller/crc_zeller_ASVs_table.tsv")
  #   meta[,1] <- sub("-",".",meta[,1])
  # if(nearingFile=="MALL/MALL_ASVs_table.tsv")
  #   meta[,1] <- sub("-",".",sub("-",".",meta[,1]))
  # if(nearingFile=="t1d_alkanani/t1d_alkanani_ASVs_table.tsv")

  if(ncol(meta)==1)
    meta <- cbind(rownames(meta),meta)
  
  # ensure that row.names are the 1st column:
  meta[,1] <- sub("-",".",sub("-",".",meta[,1]))
  colnames(nM) <- sub("-",".",sub("-",".",colnames(nM)))
  
  rfDat <- order(colnames(nM))
  nM <- nM[,rfDat]
  
  rfMeta <- order(meta[,1])
  meta <- meta[rfMeta,]

  if(length(intersect(colnames(nM),meta[,1]))<nrow(meta)/2){
    # if(identical(sort(colnames(nM)),sort(paste("X",meta[,1],sep="")))){
    meta[,1] <- paste("X",meta[,1],sep="")
    # }
    # else{
    #   stop("!identical(colnames(nM),meta[,1])")
    # }
  }

  NotInMeta <- setdiff(colnames(nM),meta[,1])
  if(length(NotInMeta)>0){
    dim(nM)
    nrow(meta)
    print(nearingFile)
    print(paste("Not found in meta data:", NotInMeta))
    warning("Not found in meta data:", paste(NotInMeta))
  }
  NotInData <- setdiff(meta[,1],colnames(nM))
  if(length(NotInData)>0){
    dim(nM)
    nrow(meta)
    print(nearingFile)
    print(paste("Not found in data:", NotInData))
    warning("Not found in data:", paste(NotInData))
  }
  
  inter <- intersect(meta[,1],colnames(nM))

  rownames(meta) <- meta[,1]
  meta <- meta[inter,]
  nM <- nM[,inter]
  
  
  # now sort according to comparison levels:
  if(ncol(meta)==2)
    compLev <- unique(meta[,2])
  else
    stop("ncol(meta)!=2")
  
  if(length(compLev)!=2){
    compLev
    stop("length(compLev)!=2")
  }
  rfComp <- c(which(meta[,2]==compLev[1]),which(meta[,2]==compLev[2]))
  meta <- meta[rfComp,]
  nM <- nM[,rfComp]
  
  if(ncol(meta)>1){
    rn <- meta[,1]
    meta <- data.frame(condition=meta[,-1])
    rownames(meta) <- rn
  }
  meta[,1] <- as.factor(meta[,1]) # make factor
  names(meta)<-"condition" # ensure this
  
  # if(!identical(colnames(nM),rownames(meta[,1])))
  #   stop("!identical(colnames(nM),rownames(meta[,1]))")
  
  for(i in 1:5){ # do it five times (until notthing changes) because column filtering can have an impact on row filtering and vice versa
    # filter out rows with no counts:
    levs <- unique(meta[,1])
    for(lev in levs){ # for each condition
      ind = which(lev==meta[,1])
      nM <- nM[rowSums(nM[,ind])>0,]
    }
    
    # filter out columns with no counts:
    drin <- which(colSums(nM)>=100)
    nM <- nM[,drin]
    meta <- meta[drin,,drop=FALSE]
  }
  
  # Writing
  file <- sub("tsv","RDS",sub(".*/","",nearingFile)) # RDS filename
  saveRDS(nM,file=paste0("../Data/count.data/",file)) # write RDS
  
  fileMeta <- sub("ASVs_table.RDS","metadata.rds",file) # we lower case suffix before
  
  saveRDS(meta,file=paste0("../Data/meta.data/",fileMeta)) # write RDS
#  saveRDS(meta,file=paste0("../Data/meta.data.cleaned/",fileMeta)) # write RDS
  
  print(head(data.frame(meta,as.data.frame(colnames(nM))),n=50)) # as check
  
}

warnings()

sink()
