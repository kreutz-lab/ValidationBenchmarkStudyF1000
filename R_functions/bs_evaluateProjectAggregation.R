# bs_evaluateProjectAggregation("../Results")
# bs_evaluateProjectAggregation("../Results_partReg")

bs_evaluateProjectAggregation <- function(searchFolder=".."){
  
  fils <- list.files(searchFolder,pattern="^H_true_all.RDS",recursive = T,full.names = T)
  folders <- sub("H_true_all.RDS$","",fils)
  res <- NULL
  
  
  for(folder in folders){
    file2 <- NULL
    if(file.exists(paste0(folder,"/DF_significance.RDS")))
      file2 <- paste0(folder,"/DF_significance.RDS")
    # testFolder <- paste0(sub("5.4","5.2",folder),"/DF_significance.RDS")
    # if(is.null(file2) && file.exists(testFolder))
    #   file2 <- testFolder
    testFolder <- paste0(sub("5.2+5.4","5.2",folder,fixed=T),"/DF_significance.RDS")
    if(is.null(file2) && file.exists(testFolder))
      file2 <- testFolder
    # testFolder <- paste0(sub("secondary","primary",sub("5.4","5.2",folder)),"/DF_significance.RDS")
    # if(is.null(file2) && file.exists(testFolder))
    #   file2 <- testFolder
    testFolder <- paste0(sub("secondary","primary",sub("5.2+5.4","5.2",folder,fixed=T)),"/DF_significance.RDS")
    if(is.null(file2) && file.exists(testFolder))
      file2 <- testFolder
    
    if(!is.null(file2))
    {
      
      dfH0 <- readRDS(paste0(folder,"/H_true_all.RDS"))
      DF_significance <- readRDS(file2)
      
      df <- dfH0
      df$project <- df$dataSet
      df$project <- sub("_sparse.*","",df$project)
      df$project <- sub("_meta.*","",df$project)
      
      projects <- unique(df$project)
      nfeatures <- array()
      df$nfeature <- array()
      for(i in 1:length(projects)){
        project <- projects[i]
        nfeatures[i] <- sum(DF_significance[,"project"]==project)
        df$nfeature[df$project==project] <- nfeatures[i]
      }
      names(nfeatures) <- projects
      df$w <- df$nfeature/sum(nfeatures,na.rm=T)
      
      hypos <- unique(df$hypoNr)
      for(hypo in hypos){
        ind <- which(df$hypoNr==hypo)
        tmp <- data.frame(folder=folder,
                          hypothesis=hypo,
                          H_true = mean(df$H_true[ind],na.rm=T),
                          H_true_weighted = weighted.mean(df$H_true[ind],df$w[ind],na.rm=T))
        if(is.null(res))
          res <- tmp
        else
          res <- rbind(res,tmp)
      }
      
    }else{
      cat("Could not do bs_evaluateProjectAggregation in folder ",folder, " probably due to missing DF_significance.RDS\n")
      cat(paste0(sub("secondary","primary",sub("5.2+5.4","5.2",folder)),"/DF_significance.RDS"),"\n")
    }
    
  }
  saveRDS(res,paste0(searchFolder,"/aim2res_differentAggregation.RDS"))
  write.xlsx(res,file=paste0(searchFolder,"/aim2res_differentAggregation.xlsx"))
  
  return(res)
}