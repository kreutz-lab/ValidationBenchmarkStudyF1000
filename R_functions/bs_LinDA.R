#
# Example:
# load("counts_meta.Rdata")
# res1 <- bs_linda(counts=counts,meta=meta)

# Run LinDA
bs_linda <- function(counts, meta, groupVariable = "condition", fileName = NULL, lib.cut=100) {
  # Ensure counts is a matrix
  if (inherits(counts, "data.frame"))
    counts <- as.matrix(counts)
  
  if (!all(!is.na(counts)))
    warning("bs_linda: NAs in counts.")
  if (sum(counts < 0) > 0)
    warning("bs_linda: Counts have negative values.")
  
  # Ensure colnames(counts) and rownames(meta) coincide
  if (!all(colnames(counts) %in% rownames(meta))) {
    stop("Column names of counts and row names of meta do not match.")
  }
  counts <- counts[, colnames(counts) %in% rownames(meta), drop = FALSE]
  meta <- meta[colnames(counts), , drop = FALSE]
  
  # Filter out samples with zero counts across all features
  moreThan0 <- colSums(counts) > 0
  if (sum(moreThan0) == 0) {
    stop("No samples with non-zero counts remain after filtering.")
  }
  counts <- counts[, moreThan0, drop = FALSE] # Only keep samples with non-zero counts
  meta <- meta[moreThan0, , drop = FALSE] # Update metadata accordingly
  
  # Convert group variable to a factor
  meta[, groupVariable] <- as.factor(meta[, groupVariable])
  
  # Load LinDA package
  if (!requireNamespace("LinDA", quietly = TRUE)) {
    stop("The LinDA package is required but is not installed. Install it using install.packages('LinDA').")
  }

  # Fix formula check to ensure single logical condition
  if (!is.character(groupVariable)) {
    stop("groupVariable must be a character string representing a column in meta.")
  }
  
  # Run LinDA
  linda_results <- LinDA::linda(
    otu.tab = counts,
    meta = meta,
    formula = '~ condition',
    alpha = 1, # Significance threshold for FDR control
    prev.cut = 0, # Prevalence filter
    lib.cut = lib.cut, # Minimum library size filter
    winsor = TRUE  # Winsorization to reduce outlier effects
  )
  
  # Extract results
  res <- linda_results$output$condition
  out <- list(
    pvalue = res$`pvalue`,
    logFoldChange = res$log2FoldChange,
    padj = res$`padj`)
  
  if(length(out$pvalue)!=nrow(counts))
    stop("length(out$pvalue)!=nrow(counts)")
  
  return(out)
}
