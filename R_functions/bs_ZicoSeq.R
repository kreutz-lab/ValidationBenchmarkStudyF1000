#
# Example:
# load("counts_meta.Rdata")
# res <- bs_zicoseq(counts=counts,meta=meta)

bs_zicoseq <- function(counts, meta, groupVariable = "condition", adjVariable = NULL, fileName = NULL, min_nonzero_threshold=2) {
  if (inherits(counts, "data.frame"))
    counts <- as.matrix(counts)
  
  if (!all(!is.na(counts)))
    warning("bs_zicoseq: NAs in counts.")
  if (sum(counts < 0) > 0)
    warning("bs_zicoseq: Counts have negative values.")
  
  if (!all(colnames(counts) %in% rownames(meta))) {
    stop("Column names of counts and row names of meta do not match.")
  }
  counts <- counts[, colnames(counts) %in% rownames(meta), drop = FALSE]
  meta <- meta[colnames(counts), , drop = FALSE]
  group <- as.factor(meta[, groupVariable])
  
  # Filter out features with fewer than min_nonzero_threshold nonzero values:
  retained_indices <- array(T,dim=nrow(counts))
  for(grouplevel in levels(group)){
    nonzero_counts <- rowSums(counts[,group==grouplevel] > 0)
    retained_indices <- retained_indices & nonzero_counts >= min(sum(group==grouplevel),min_nonzero_threshold)
  }
  
  if (length(retained_indices) == 0) {
    stop("No features remain after filtering. Please check your filtering criteria.")
  }
  
  filtered_counts <- counts[retained_indices, , drop = FALSE]
  
  if (!requireNamespace("GUniFrac", quietly = TRUE)) {
    stop("The ZicoSeq function from the GUniFrac package is required but is not installed. Install it using BiocManager::install('GUniFrac').")
  }
  
  zicoseq_res <- GUniFrac::ZicoSeq(
    meta.dat = meta,
    feature.dat = filtered_counts,
    grp.name = groupVariable,
    adj.name = adjVariable,
    feature.dat.type = "count", p.max = Inf
  )
  
  # Initialize result vectors with NA
  pvalue <- rep(NA, nrow(counts))
  logFoldChange <- rep(NA, nrow(counts))
  padj <- rep(NA, nrow(counts))
  
  # Assign results to the retained indices
  # ind <- match(rownames(counts),zicoseq_res$filter.features)
  # found <- which(!is.na(ind))
  # ind <- ind[found]
  # pvalue[found] <- zicoseq_res$p.raw[ind]
  # # logFoldChange is not directly provided by ZicoSeq; use NA
  # padj[found] <- zicoseq_res$p.adj.fdr[ind]

  pvalue[retained_indices] <- zicoseq_res$p.raw
  # logFoldChange is not directly provided by ZicoSeq; use NA
  padj[retained_indices] <- zicoseq_res$p.adj.fdr
  
  out <- list(
    pvalue = pvalue,
    logFoldChange = logFoldChange,
    padj = padj,
    result = zicoseq_res
  )
  
  if (!is.null(fileName)) {
    saveRDS(out, file = fileName)
  }
  
  return(out)
}
