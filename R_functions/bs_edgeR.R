# edgeR
#
#   Calling DESeq2 for doing differential abundance analysis
#
# The following packages are required "edgeR", "phyloseq" and loaded in bs_init()


# # ### Taken from phyloseq authors at: https://joey711.github.io/phyloseq-extensions/edgeR.html
# phyloseq_to_edgeR = function(physeq, group, method="RLE", ...){
#   #   require("edgeR")
#   #   require("phyloseq")
#   # Enforce orientation.
#   if( !taxa_are_rows(physeq) ){ physeq <- t(physeq) }
#   x = as(otu_table(physeq), "matrix")
#   # Add one to protect against overflow, log(0) issues.
#   x = x + 1
#   # Check `group` argument
#   if( identical(all.equal(length(group), 1), TRUE) & nsamples(physeq) > 1 ){
#     # Assume that group was a sample variable name (must be categorical)
#     group = get_variable(physeq, group)
#   }
#   # Define gene annotations (`genes`) as tax_table
#   taxonomy = tax_table(physeq, errorIfNULL=FALSE)
#   if( !is.null(taxonomy) ){
#     taxonomy = data.frame(as(taxonomy, "matrix"))
#   }
#   # Now turn into a DGEList
#   y = DGEList(counts=x, group=group, genes=taxonomy, remove.zeros = TRUE, ...)
#   # Calculate the normalization factors
#   z = calcNormFactors(y, method=method)
#   # Check for division by zero inside `calcNormFactors`
#   if( !all(is.finite(z$samples$norm.factors)) ){
#     stop("Something wrong with edgeR::calcNormFactors on this data,
#          non-finite $norm.factors, consider changing `method` argument")
#   }
#   # Estimate dispersions
#   return(estimateTagwiseDisp(estimateCommonDisp(z)))
# }
#
# Example:
# res <- bs_edgeR(count=d[[i]][[j]]$counts[1:300,],meta=d[[i]][[j]]$meta)


# Run edgeR
bs_edgeR <- function(counts,meta,groupVariable="condition",fileName=NULL){
  
  if(inherits(counts,"data.frame"))
    counts <- as.matrix(counts)
  
  if(!all(!is.na(counts)))
    warning("bs_DESeq: NAs in counts.")
  if(sum(counts<0)>0)
    warning("bs_DESeq: Counts have negative values.")

  moreThan0 <- colSums(counts) > 0 # 
  counts <- counts[,moreThan0] # only consider those samples, since edgeR cannot analyze columns with only zeros
  meta <- meta[moreThan0, , drop=F] # only consider those samples, since edgeR cannot analyze columns with only zeros

  
  # Turn into DGEList
  dgeList <- DGEList(counts=counts,group=as.numeric(as.factor(meta[,groupVariable])))
  # Calculate normalization factor
#  dgeList <- edgeR::calcNormFactors(dgeList, method="RLE")
  dgeList <- edgeR::calcNormFactors(dgeList) # RLE does not work => normfactors are NA
  # Estimate dispersion
  dgeList <- edgeR::estimateDisp(dgeList)
  # Apply test
  res = edgeR::exactTest(dgeList)
  
  out <- list(pvalue=res@.Data[[1]]$PValue, logFoldChange=res@.Data[[1]]$logFC, padj=p.adjust(res@.Data[[1]]$PValue,method = "BH"), result=res  )
  return(out)
}
