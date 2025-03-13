# bs_adapt
#
#   Calling ADAPT::adapt for doing differential abundance analysis
#
# maxRunTimeInMin maximal Runtime in minutes
#
# Required packages (loaded in bs_init)
#
# Example:
# res <- bs_distinctTest(count=d[[i]][[j]]$counts[1:300,],meta=d[[i]][[j]]$meta)


bs_adapt <- function(counts, meta, groupVariable="condition", baseCondition=NULL, 
                      fileName=NULL, doRarefy=F, prev.filter=0, depth.filter=100) {
  
  if (inherits(counts, "data.frame")) 
    counts <- as.matrix(counts)
  
  # Optional rarefaction
  if (doRarefy) {
    tmp <- bs_doRarefy(counts, meta)
    counts <- tmp$counts
    meta <- tmp$meta
  }
  
  # Basic checks
  if (!all(!is.na(counts))) 
    warning("bs_adapt: NAs in counts.")
  if (sum(counts < 0, na.rm = T) > 0) 
    warning("bs_adapt: Counts have negative values.")
  
  # Convert counts to relative abundance for ADAPT
  rel_abundance <- sweep(counts, 2, colSums(counts), "/")
  
  # Ensure group variable is a factor
  meta[[groupVariable]] <- as.factor(meta[[groupVariable]])
  
  # Run ADAPT
  res <- NULL
  marker <- list(pvalue = array(dim = nrow(counts)))
  
  phylo <- bs_data2phyloseq(counts,meta,buildTree = F)
  
  try({
    res <- ADAPT::adapt(
      input_data = phylo,
      cond.var = groupVariable,  # Grouping variable
      base.cond = baseCondition,         # Reference group (optional)
      prev.filter = prev.filter,         # Remove low-prevalence taxa
      depth.filter = depth.filter        # Remove low-depth samples
    )
  })
  
  if (is.null(res)) {
    warning("Error: ADAPT did not succeed.")
  } else {
    marker$pvalue <- res@details$pval
    marker$logFoldChange <- res@details$log10foldchange * log2(10) # trsform from log10 to log2
    print(paste0("ADAPT succeeded with ", sum(!is.na(marker$pvalue)), " significant p-values."))
  }
  
  # Format the output similar to LEfSe function
  out <- list(
    pvalue = marker$pvalue,
    logFoldChange = marker$logFoldChange,
    padj = p.adjust(marker$pvalue, method = "BH"),
    result = res
  )
  
  return(out)
}
