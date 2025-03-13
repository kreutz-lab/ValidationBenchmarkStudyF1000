# This function applies filtering of the significance data.frame
# to do hypothesis testing in scripts_5* on different data subsets
#
# dataSetFilter   "exp"     only experimetnal templates 
#                 "sim"     only simulated data
#                 "expAll"  only exp. templates AND all projects merged
#                 "simEquiv"  only simulated data that pass equivalence tests
#                 "none"    no filter
#                 "simEquivSpd" simEquiv & only the data set that are available in sparseDossa2
#
# The result of the equivalence tests is taken from
#
# Example:
# dfFolder <- "../Results/5.1.1_Aim2_primary_metaSPARSim_exp"
# DF_significance <- readRDS(paste0(dfFolder,"/","DF_significance.RDS"))
# DF_significance <- bs_apply_dataSetFilter(DF_significance,"exp")


bs_apply_dataSetFilter <- function(DF_significance,dataSetFilter,dfFolder){
  
  if(dataSetFilter=="sim"){
    indSimu <- sort(unique(c(grep("^sparseDossa",DF_significance$dataset,ignore.case = TRUE),
                             grep("^metaSPARSim",DF_significance$dataset,ignore.case = TRUE))))
    DF_significance <- DF_significance[indSimu,]
    cat("\nFiltering with dataSetFilter=",dataSetFilter,"done. \n\n")
  }
  if(grepl("simEquiv",dataSetFilter)){ # simulations that pass equivalence criterion
    # First apply normal "sim" dataSetFilter:
    DF_significance <- bs_apply_dataSetFilter(DF_significance,"sim")
    
    # Then remove projects that have to be removed
    remFile <- paste0(dfFolder,"/ToRemove_because_outlierInSimilarityWithTemplate.RDS")
    if(!file.exists(remFile))
      stop(paste0("bs_apply_dataSetFilter(): ",remFile," does not exist! Please check why!"))
    
    toRem <- readRDS(remFile)
    drin = array(TRUE,dim=nrow(DF_significance))
    if(!is.null(toRem)){
      for(weg in names(toRem))
        drin[DF_significance$project==weg] <- FALSE
      DF_significance <- DF_significance[drin,]
    }
    
    if(dataSetFilter=="simEquivSpd"){
      templates_spd <- readRDS("../Data/templatesSpd.RDS")
      DF_significance <- DF_significance[DF_significance$project %in% templates_spd,]
    }
    if(dataSetFilter=="simEquivNoAncom"){
      DF_significance <- DF_significance[, names(DF_significance)!= "ancombc"]
    }
    if(dataSetFilter=="simEquivNoLefse"){
      DF_significance <- DF_significance[, names(DF_significance)!= "bs_lefse"]
    }
    
    cat("\nFiltering with dataSetFilter=",dataSetFilter,"done. \n\n")
  }
  if(dataSetFilter=="exp"){
    DF_significance <- DF_significance[DF_significance$dataset=="original",]
    cat("\nFiltering with dataSetFilter=",dataSetFilter,"done. \n\n")
  }
  if(dataSetFilter=="expAll"){
    DF_significance <- DF_significance[DF_significance$dataset=="original",]
    DF_significance$project[1:nrow(DF_significance)] <- "all"
    DF_significance$project_dataset <- DF_significance$dataset
    cat("\nFiltering with dataSetFilter=",dataSetFilter,"done. \n\n")
  }
  if(dataSetFilter=="expAll")
    cat("\ndataSetFilter=",dataSetFilter,": No filtering done. \n\n")
  
  
  if(!dataSetFilter %in% c("sim","exp","expAll","simEquiv","simEquivSpd","none","simEquivNoLefse","simEquivNoAncom") && dataSetFilter!="")
    stop("dataSetFilter not found.")
  
  return(DF_significance)
}