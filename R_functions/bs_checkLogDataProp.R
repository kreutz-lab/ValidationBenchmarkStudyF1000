# This function is part of script_3x
# It checks all data properties with shapiro.test and applies log-transformation if 
# the p-value is large on the log-scale. This would indicate that the data prop
# is more similar to normal distibution on the log-scale.
#
# Example:
# data_to_compare <- bs_addDataProp(data_to_compare)
# data_to_compare <- bs_annotateObject(data_to_compare)
# data_to_compare <- bs_diffDataProp(data_to_compare)
# DF <- bs_summarize_dataPropCmp(data_to_compare)
# dfLog <- bs_checkLogDataProp(DF)
# saveRDS(dfLog, paste0(resultFolder,"/DF_summary_log.RDS"))

bs_checkLogDataProp <- function(DF_summary){
  
  ## Shapiro Wilk Test and log2-Transformation if indicated:
  doLog <- array(NA,ncol(DF_summary))
  dfLog <- DF_summary
  namen <- names(dfLog)
  for(i in 1:ncol(DF_summary)){
    if(is.numeric(DF_summary[,i])){
      DF_summary[is.infinite(DF_summary[,i]),i] <- NA # if infinity
      if(min(DF_summary[,i],na.rm = T)<=1e-10){
        doLog[i] <- F
      }else{
        pSW <- 1
        if(sum(!is.na(DF_summary[,i]))>=3)
          try(pSW <- shapiro.test(DF_summary[,i])$p.value)

        pSWLog <- 1
        if(sum(!is.na(DF_summary[,i]))>=3 && min(DF_summary[,i],na.rm=T)>0)
          try(pSWLog <- shapiro.test(log2(DF_summary[,i]))$p.value)

        if(pSWLog>pSW){
          dfLog[,i] <- log2(DF_summary[,i])
          doLog[i] <- TRUE
          namen[i] <- paste0("log2(",namen[i],")")
        }else{
          doLog[i] <- FALSE
        }
      }
    }
  }
  names(dfLog) <- namen
  print(paste0(sum(doLog,na.rm=T)," data properties require log2-transformation."))
  print(paste0(sum(!doLog,na.rm=T)," data properties require NO transformation."))
  
  return(list(dfLog=dfLog,doLog=doLog))
  
}