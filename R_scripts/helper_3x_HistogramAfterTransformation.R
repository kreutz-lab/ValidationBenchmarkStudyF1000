# This scripts only produces the histograms and can be used if this failed before.
# 
# This is usually not required and part of script_3x_DataProperties.R


for(resultFolder in c("../Results/3.1_metaSPARSim_DataProps",
                      "../Results/3.1_sparseDOSSA_DataProps",
                      "../Results/3.2_metaSPARSim_ZerosAdded_DataProps",
                      "../Results/3.2_sparseDOSSA_ZerosAdded_DataProps",
                      "../Results/3.3_metaSPARSim_Filtered_DataProps",
                      "../Results/3.3_sparseDOSSA_Filtered_DataProps",
                      "../Results/3.4_metaSPARSim_filtered_ZerosAdded_DataProps",
                      "../Results/3.4_sparseDOSSA_filtered_ZerosAdded_DataProps")){
  
  DF_summary <- readRDS(paste0(resultFolder,"/DF_summary.RDS"))
  
  
  ## Shapiro Wilk Test and log2-Transformation if indicated:
  
  doLog <- array(NA,ncol(DF_summary))
  dfLog <- DF_summary
  namen <- names(dfLog)
  for(i in 1:ncol(DF_summary)){
    if(is.numeric(DF_summary[,i]) && min(DF_summary[,i],na.rm = T)>0){
      if(sum(!is.na(DF_summary[,i]))>=3)
        pSW <- shapiro.test(DF_summary[,i])$p.value
      else
        pSW <- 1
      
      if(sum(!is.na(DF_summary[,i]))>=3)
        pSWLog <- shapiro.test(log2(DF_summary[,i]))$p.value
      else
        pSWLog <- 1
      
      if(pSWLog>pSW){
        dfLog[,i] <- log2(DF_summary[,i])
        doLog[i] <- TRUE
        namen[i] <- paste0("log2(",namen[i],")")
      }
      else
        doLog[i] <- FALSE
    }
  }
  names(dfLog) <- namen
  print(paste0(sum(doLog,na.rm=T)," data properties require log2-transformation."))
  print(paste0(sum(!doLog,na.rm=T)," data properties require NO transformation."))
  
  
  # Create histograms with faceting
  library(tidyr)
  df_long <- pivot_longer(dfLog[,!is.na(doLog)], cols = everything(), names_to = "Variable", values_to = "Value")
  
  
  p <- ggplot(df_long, aes(x = Value)) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +  # Adjust bin size as needed
    facet_wrap(~ Variable, scales = "free_x") +  # Free scales if the ranges are very different
    labs(title = "Histograms of All Data Properties",
         x = "Value",
         y = "Count") +
    theme_minimal()
  
  pdf(file=paste0(resultFolder,"/HistogramAfterTransformation.pdf"),width = 20, height=14)
  print(p)
  dev.off()
  
}
