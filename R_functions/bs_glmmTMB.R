#' bs_glmmTMB
#'
#' Differential abundance analysis using glmmTMB (zero inflated NB)
#' 
#' ATTENTION 1:
#' The 2nd coefficient is returned, if more complex forumlas are used please check!!
#'
#'
#' @param 
#' @param 
#'
#' @return
#' 
#' @examples
#' 
#' res <- bs_glmmTMB(d$MALL$metaSPARSimNonReg_1$counts,d$MALL$original$meta)
#' 
#'
#' @seealso \code{\link{mb.glmmTMB.core}}
#' @keywords differential abundance analysis
#' @export

bs_glmmTMB <- function(counts,meta,formula=as.formula("counts~condition"),fileName=NULL){
  
  if(inherits(counts,"data.frame"))
    counts <- as.matrix(counts)
  
  if(!all(!is.na(counts)))
    warning("bs_DESeq: NAs in counts.")
  if(sum(counts<0)>0)
    warning("bs_DESeq: Counts have negative values.")
  
  libSize <- colSums(counts)
  
  res <- list()
  for(i in 1:nrow(counts)){
    cat(".")
    if(i %% 100 == 0)
      cat("\n")
    
    df <- data.frame(meta,counts=counts[i,],libSize = libSize)
    fit <- glmmTMB::glmmTMB(formula = formula,  
                   data=df,
                   ziformula= as.formula("~1"),
                   family= "nbinom2")
    coefs <- as.data.frame(coefficients(summary(fit))$cond)
    
    if(fit$fit$convergence!=0) # check convergence
      coefs[,1:4] <- NA # overwrite if not converged

    icoef <- grep("condition",rownames(coefs))
    res[["pvalue"]][i] <- coefs[icoef,"Pr(>|z|)"] 
    res[["log2FoldChange"]][i] <- coefs[icoef,"Estimate"]
    
    ## Leave out larges count in each group
    
  }

  res[["coefname"]] <- row.names(coefs)[2]
  if(as.character(formula)[3]!="condition + libSize")
    warning("bs_glmmTMB: The 2nd coefficient is returned, if more complex forumlas are used please check!!")
  
  res[["padj"]] <- p.adjust(res[["pvalue"]],method="BH")

  names(res[["pvalue"]]) <- row.names(counts)
  names(res[["log2FoldChange"]]) <- row.names(counts)
  names(res[["padj"]]) <- row.names(counts)
  
  out <- list(pvalue=res$pvalue, logFoldChange=res$log2FoldChange, padj=res$padj, result=res  )
  return(out)
  #write.table(res, file=args[3], quote=FALSE, sep="\t", col.names = NA)
}
