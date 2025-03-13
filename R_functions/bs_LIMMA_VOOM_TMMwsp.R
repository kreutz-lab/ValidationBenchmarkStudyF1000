# Limma Voom TMMwsp
#
# deps = c("edgeR")
# for (dep in deps){
#   if (dep %in% installed.packages()[,"Package"] == FALSE){
#     if (!requireNamespace("BiocManager", quietly = TRUE))
#       install.packages("BiocManager")
#     
#     BiocManager::install(deps)
#   }
#   library(dep, character.only = TRUE)
# }
#
#
# Example:
# res <- bs_limma_voom_TMMwsp(count=d[[i]][[j]]$counts[1:300,],meta=d[[i]][[j]]$meta)

 
bs_limma_voom_TMMwsp <- function(counts,meta,groupVariable="condition",fileName=NULL){
  
  if(inherits(counts,"data.frame"))
    counts <- as.matrix(counts)
  
  if(!all(!is.na(counts)))
    warning("bs_limma_voom_TMMwsp: NAs in counts.")
  if(sum(counts<0,na.rm=T)>0)
    warning("bs_limma_voom_TMMwsp: Counts have negative values.")
  
  
  # DGE_LIST <- DGEList(ASV_table)
  ASV_table <- counts # renaming from bs terminology to Nearing
  groupings <- meta  # renaming from bs terminology to Nearing
  DGE_LIST <- DGEList(counts=counts,group=as.numeric(as.factor(meta[,groupVariable])))
  
  ### do normalization    
  ### Reference sample will be the sample with the highest read depth
  
  ### check if upper quartile method works for selecting reference
  Upper_Quartile_norm_test <- edgeR::calcNormFactors(DGE_LIST, method="upperquartile")
  
  summary_upper_quartile <- summary(Upper_Quartile_norm_test$samples$norm.factors)[3]
  if(is.na(summary_upper_quartile) | is.infinite(summary_upper_quartile)){
    message("Upper Quartile reference selection failed will use find sample with largest sqrt(read_depth) to use as reference")
    Ref_col <- which.max(colSums(sqrt(ASV_table)))
    DGE_LIST_Norm <- edgeR::calcNormFactors(DGE_LIST, method = "TMMwsp", refColumn = Ref_col)
    # fileConn<-file(args[[4]])
    # writeLines(c("Used max square root read depth to determine reference sample"), fileConn)
    cat(c("Used max square root read depth to determine reference sample \n"))
    # close(fileConn)
  }else{
    DGE_LIST_Norm <- edgeR::calcNormFactors(DGE_LIST, method="TMMwsp")
  }
  
  ## make matrix for testing
  colnames(groupings) <- c("comparison")
  mm <- model.matrix(~comparison, groupings)
  
  voomvoom <- voom(DGE_LIST_Norm, mm, plot=F)
  
  fit <- lmFit(voomvoom,mm)
  fit <- eBayes(fit)
  res <- topTable(fit, coef=2, n=nrow(DGE_LIST_Norm), sort.by="none")
  # write.table(res, file=args[3], quote=F, sep="\t", col.names = NA)
  
  out <- list(pvalue=res$P.Value, logFoldChange=res$logFC, padj=res$adj.P.Val, result=res  )
  return(out)
}

