# bs_DESeq
#
#   Calling DESeq2 for doing differential abundance analysis
#
# Example:
# res <- bs_DESeq(count=d[[i]][[j]]$counts,meta=d[[i]][[j]]$meta)


bs_DESeq <- function(counts,meta,design=as.formula("~condition"),contrast=NULL, fileName=NULL)
{
  if(inherits(counts,"data.frame"))
    counts <- as.matrix(counts)

  if(!all(!is.na(counts)))
    warning("bs_DESeq: NAs in counts.")
  if(sum(counts<0)>0)
    warning("bs_DESeq: Counts have negative values.")
  
  moreThan0 <- colSums(counts) > 0 #
  counts <- counts[,moreThan0] # only consider those samples, since DESeq cannot analyze columns with only zeros
  meta <- meta[moreThan0, , drop=F] # only consider those samples, since DESEq cannot analyze columns with only zeros

  dds <- DESeq2::DESeqDataSetFromMatrix(countData = counts,
                                        colData = meta,
                                        design = design)
  dds_res <- DESeq2::DESeq(dds, sfType = "poscounts")
  
  if(is.null(contrast))
    res <- results(dds_res, tidy=T, format="DataFrame")
  else
    res <- results(dds_res, tidy=T, format="DataFrame", contrast = contrast)
  
  rownames(res) <- res$row
  res <- res[,-1]
  
  out <- list(pvalue=res$pvalue, logFoldChange=res$log2FoldChange, padj=res$padj, result=res  )
  return(out)
  #write.table(res, file=args[3], quote=FALSE, sep="\t", col.names = NA)
}

