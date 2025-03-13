# This function combines several columns of p-values
# The distribution is the same as the distribution given by the targetName column.
#
# Examples:
# 
# pvals <- data_to_compare[[1]]$original$DA$p_value
# pnew <- msb.combine_pvalues(pvals,"Maaslin2",setdiff(colnames(pvals),"Maaslin2"))


msb.combine_pvalues <- function(pvals,targetName,mergeNames){
  if(is.data.frame(pvals))
    pvals <- as.matrix(pvals)
  
  if(!is.matrix(pvals))
    stop("msb.combine_pvalues requires pvals as matrix.")
  if(is.null(colnames(pvals)))
    stop("msb.combine_pvalues needs colnames.")
  
  if(!is.array(mergeNames))
  cn <- colnames(pvals)
  for(mergeName in mergeNames){
   cn[length(cn)+1] <- paste0(targetName,"<-",mergeName)
   pnew <- msb.combine_pvalueArrays(pvals[,targetName],pvals[,mergeName])
   pvals <- cbind(pvals,pnew)
  }
  colnames(pvals) <- cn
  return(pvals)
}

msb.combine_pvalueArrays <- function(pvalsTarget,pvalsMerge){
  pnew <- geometricmeanRow(cbind(pvalsTarget,pvalsMerge),na.rm=T)
  
  pvalsTarget[is.na(pvalsTarget)] <- pvalsMerge[is.na(pvalsTarget)]
  
  # Make distribution like pvalsTarget (to get same FDRs)
  rf <- order(pnew)
  pnew[rf] <- sort(pvalsTarget,na.last=T)
  
  return(pnew)
}