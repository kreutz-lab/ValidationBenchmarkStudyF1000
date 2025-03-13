#' project_init
#'
#' This file should be source to prepare the R environment 
#'
#' Sourcing functions, loading libraries, setting project_path
#'
#' @param 
#' @param p
#' @return
#' 
#' @examples
#' 
#' # project_init() # default packages
#' project_init(c("metaSPARSim","ggplot2")) # if only specific packages, i.e. those two, should be loaded
#'
#' @seealso \code{\link{anotherFunction}}
#' @keywords aKeywordSuchAsBenchmarking
#' @export

project_init <- function(packages=NULL,bsLibPath="../R_functions"){
  
  dummy <- runif(1) # draw a random number to ensure that the RNG is initialized

  if(is.null(packages)){
    # add new/additional packages in alphabetic order:
    try(suppressPackageStartupMessages(library(amap, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(ape, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(ALDEx2, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(ANCOMBC, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(Biobase, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(BimodalIndex, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(Cairo, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(callr, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(cluster, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(compositions, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(corncob, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(doParallel, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(digest, quietly = T, warn.conflicts = F)) ) # for checksums
    try(suppressPackageStartupMessages(library(dplyr, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(DESeq2, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(edgeR, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(exactRankTests, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(fdrtool, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(ggfortify, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(ggplot2, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(ggpubr, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(ggsci, quietly = T, warn.conflicts = F)) ) #to get color palettes: pa_npg()...
    try(suppressPackageStartupMessages(library(glmmTMB, quietly = T, warn.conflicts = F)) ) #to get color palettes: pa_npg()...
    try(suppressPackageStartupMessages(library(Maaslin2, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(metaSPARSim, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(metagenomeSeq, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(microbiomeMarker, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(MIDASim, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(nlme, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(openxlsx, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(parallel, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(phyloseq, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(parallelDist, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(profmem, quietly = T, warn.conflicts = F)) ) # for measuring memory usage
    try(suppressPackageStartupMessages(library(pROC, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(qvalue, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(reshape2, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(rpart, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(rpart.plot, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(R.utils, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(Rfast, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(RColorBrewer, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(SparseDOSSA2, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(scales, quietly = T, warn.conflicts = F)) )# show_col
    try(suppressPackageStartupMessages(library(tidyverse, quietly = T, warn.conflicts = F)) )
    try(suppressPackageStartupMessages(library(vegan, quietly = T, warn.conflicts = F)) )
    
  }
  else{
    for(i in 1:length(packages)){
      try(suppressPackageStartupMessages(library(paste0(packages[i]), quietly = T, warn.conflicts = F)) )
    }
  }
  
  files.sources = list.files(paste0(bsLibPath), pattern="*.R$", full.names=TRUE)
  #sapply(files.sources, source)
  missing <- c()
  
  if(length(files.sources)>0){
    vars <- ls(".GlobalEnv")
    for(i in 1:length(files.sources)){
      cat("Sourcing ",files.sources[i],"...\n")
      tryCatch(source(files.sources[i]), 
               error=function(e){
                 cat(files.source[i], "could not be sourced in project_init. Error:",conditionMessage(e),"\n")
               })
    }
    
    # Check whether all function exected are available:
    newvars <- setdiff(ls(".GlobalEnv"),c(vars,"vars")) # new variables == new functions by source loop before
    if(length(newvars)>0){
      for(i in 1:length(newvars)){
        print(newvars[i])
        missing <- c(missing,msb.findCalledFunctions(get(newvars[i])))
      }
    }
  }
  
  if(is.null(missing))
    cat("No missing functions found.","\n")
  else
    cat("The following functions are missing: ", paste(missing,collapse=", "),"\n")
  
  
  pfad <- getwd()
  project_path <<- sub("BenchmarkStudy_MicrobiomeSyntheticData/.*", "BenchmarkStudy_MicrobiomeSyntheticData/", pfad) # create this variable in the global environment
  bsLibPath_used <<- bsLibPath
  
  cat(paste0("project_path = ",project_path,"\n"))
  
  
  ## Check sparsedossa2
  if(is.null(packages)){
    tryCatch({
      library(SparseDOSSA2)
    },
    error=function(e){bs_sparseDossaInit()}) # here, sparseDOSSA2 is sourced manually
    
  }
}

# Initialization, i.e. sourcing R-functions, loading packages and setting project_path
bsLibPath <- "E:/clemens/Repositories/BenchmarkStudy_MicrobiomeSyntheticData/R_functions/"
sys_info <- Sys.info()

if(sys_info[["sysname"]]=="Darwin" && sys_info[["login"]]=="root"){
  bsLibPath="/Users/evakohnert/Documents/PhD/Microbiome/Benchmark/BenchmarkStudy_MicrobiomeSyntheticData/R_functions/"
}
if(sys_info[["sysname"]]=="Windows" && grep("kreutz",sys_info[["login"]],T)){
  bsLibPath = "E:/clemens/Repositories/BenchmarkStudy_MicrobiomeSyntheticData/R_functions/"
}
if(sys_info[["sysname"]]=="Linux" && sys_info[["login"]]=="kohnert"){
  bsLibPath = "~/BenchmarkStudy_MicrobiomeSyntheticData/R_functions/"
}
if(sys_info[["sysname"]]=="Linux" && sys_info[["login"]]=="ckreutz"){
  bsLibPath = "~/BenchmarkStudy_MicrobiomeSyntheticData/R_functions/"
}


if(!exists("bsLibPath") || is.null(bsLibPath)){
  cat(paste0("Calling ../R_functions\n"))
  bsLibPath <<- getwd()
  project_init(bsLibPath="../R_functions")
}else{
  cat(paste0("Sourcing functions in ",bsLibPath,"../R_functions\n"))
  project_init(bsLibPath=paste0(bsLibPath,"/../R_functions"))
}
