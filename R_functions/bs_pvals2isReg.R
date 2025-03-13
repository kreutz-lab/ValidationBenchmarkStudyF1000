# This function draws the logical vector isReg from pvals.
# 1) If a matrix of p-values is provided, for a vector is built by randomly drawing 
#      a column for each row (i.e. for each feature randomly choose the test)
# 2) It calls msb.sampleDiffRegFromPvalues for different sparsity filtering thresolds

bs_pvals2isReg <- function(pvals,counts){
  # 1) Handle pvals matrix (e.g. from different tests):
  if(is.matrix(pvals)){
    cat("bs_pvals2isReg.R: pvals matrix provided: Make a single column-vector by randomly drawing the column.\n")
    pvalVector <- array(NA,dim=nrow(pvals))
    for(r in 1:length(pvalVector)){
      notNA <- which(!is.na(pvals[r,]))
      if(length(notNA)>=1){
        whichTest <- sample(notNA,1)
        pvalVector[r] <- pvals[r,whichTest]
      }
    }
  }else{
    pvalVector <- pvals
  }
  
  # 2) Different sparsity thresholds:
  thresh <- seq(0.2,1,by=0.05)
  anzReg <- array()
  isRegList <- list()
  forSave <- list()
  for(i in 1:length(thresh)){
    prop0 <- rowSums(counts==0)/dim(counts)[2]
    irow <- prop0<thresh[i]
    if(sum(irow)>10){
      isRegList[[i]] <- array(F,dim=length(pvalVector))
      # hist(pvalVector[irow])
      isRegList[[i]][irow] <- msb.sampleDiffRegFromPvalues(pvalVector[irow])
      anzReg[i] <- sum(isRegList[[i]])
    }
    else{
      isRegList[[i]] <- NA
      anzReg[i] <- NA
    }
  }
  isReg <- isRegList[[which.max(anzReg)]]
  
  return(isReg)  
}