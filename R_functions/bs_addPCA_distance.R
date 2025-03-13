# This function add PCA distance to the data property data.frame
# For simulated data: distance to template
# For data templates: distance to overall mean over all templates
#
# Example:
# pca_result <- readRDS("../Results/3.1_metaSPARSim_DataProps/pcaObject_Log.RDS")
# DF_summary <- readRDS("../Results/3.1_metaSPARSim_DataProps/DF_summary.RDS")
# DF_summary <- bs_addPCA_distance(DF_summary, pca_result)

bs_addPCA_distance <- function(DF_summary, pca_result){
  namen <- dimnames(pca_result$x)[[1]]
  dfPCA <- NULL
  # Find for each row in DF_summary pca.x and pca.y
  for(i in 1:nrow(DF_summary)){
    # find correct dataset name && template name in namen, i.e. in dimnames(pca_result)
    ind <- intersect(grep(paste0(DF_summary$name[i],"_"),namen), grep(DF_summary$data_template[i],namen))
    if(length(ind)<1)
      cat(DF_summary$data_template[i],DF_summary$name[i]," not found (i.e. seems not available in the PCA).\n")
    if(length(ind)>1)
      stop(paste0(DF_summary$name[i],DF_summary$data_template[i]," found multiple times."))
    
    if(length(ind)==1){
      dfTmp <- data.frame(name=DF_summary$name[i], 
                          data_template = DF_summary$data_template[i],
                          pca_x = pca_result$x[ind,1], 
                          pca_y = pca_result$x[ind,2])
      if(is.null(dfPCA)){
        dfPCA <- dfTmp
      }else{
        # dfPCA <- rbind(dfPCA,dfTmp)
        dfPCA[i,] <- dfTmp
      }
    } 
  }
  
  pcaDist <- array(0,nrow(DF_summary))
  isOriginal <- DF_summary$name=="original"
  meanX <- mean(dfPCA$pca_x[isOriginal],na.rm=T)
  meanY <- mean(dfPCA$pca_y[isOriginal],na.rm=T)
  
  for(template in unique(dfPCA$data_template)){
    isProject <- DF_summary$data_template==template
    indOriginal <- which(isProject & isOriginal)
    indSimu <- which(isProject & !isOriginal)
    for(i in indSimu){
      pcaDist[i] <- sqrt((dfPCA$pca_x[i] - dfPCA$pca_x[indOriginal])^2 + (dfPCA$pca_y[i] - dfPCA$pca_y[indOriginal])^2) # euclidean distance
    }  
    pcaDist[indOriginal] <- sqrt((meanX - dfPCA$pca_x[indOriginal])^2 + (meanY - dfPCA$pca_y[indOriginal])^2) # euclidean distance
    
  }
  
  return(data.frame(DF_summary,pcaDistance=pcaDist))
  
}