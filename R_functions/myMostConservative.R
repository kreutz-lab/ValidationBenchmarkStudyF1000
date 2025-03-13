# Template for using a custom test function
#
# Example:
## 1) Testing the function
# res <- myTestTemplate(count=data_to_compare[[i]][[j]]$counts[1:300,],meta=data_to_compare[[i]][[j]]$meta)
## 2) Using the function in the pipeline withoutTestInfo
# data_to_compare <- bs_DA(data_to_compare, parallelMode = F, doSlowMethods = F, whichMethods="myTestTemplate")
## 3) Using the function in the pipeline with TestInfo
# data_to_compare <- bs_DA(data_to_compare, parallelMode = F, doSlowMethods = F, whichMethods="myTestTemplate",myTestInfo=list(a=1,b=2,...))
#  4) Using the function with results of a fit (to save time):
# data_to_compare <- bs_DA(data_to_compare, whichMethods="myTestTemplate", keepTestResult=T)
# data_to_compare <- bs_DA(data_to_compare, whichMethods="anotherTestUsingResultOfmyTestTemplate") # usage via DA$myTestTemplate 


myMostConservative <- function(counts,meta,groupVariable="condition",fileName=NULL,myTestInfo=NULL, DA){
  
  if(inherits(counts,"data.frame"))
    counts <- as.matrix(counts)
  
  if(!all(!is.na(counts)))
    warning("custom Testfunction: NAs in counts.")
  if(sum(counts<0)>0)
    warning("custom Testfunction: Counts have negative values.")

  ##############
  # Replace the following lines by your test:
  # use myTestInfo for additions infos you need for the test
  # Use DA$... if a result from another call should be used

  # Check existance of previous p-values:
  if(!"p_value" %in% names(DA) || ncol(DA$p_value)<1) 
    stop("myMaPvalue.R: no p_values in DA => perform bs_DA first.")
  
  
  pvalues <- apply(DA$p_value,MARGIN = 1, FUN=function(x){max(x,na.rm=T)})
  
  minFold <- apply(DA$logFold,MARGIN = 1, FUN=function(x){min(abs(x),na.rm=T)}); # minimal fold
  signMin <- apply(DA$logFold,MARGIN = 1, FUN=function(x){sign(x[which.min(abs(x))])}); # sign of the minimal fold
  
  foldChanges <- minFold*signMin
  res <- list(comment="maximal p-value, minimal fold-changes") # this variable can take arbitrary output of the test
  
  # End of replacing the following lines by your test:
  ##############
  
  out <- list(pvalue=pvalues, logFoldChange=foldChanges, padj=p.adjust(pvalues,method = "BH"), result=res  )
  return(out)
}
