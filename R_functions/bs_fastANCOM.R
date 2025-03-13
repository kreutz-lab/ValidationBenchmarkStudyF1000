#' fastANCOM Differential Abundance Analysis
#'
#' @param counts A matrix of counts (rows as features, columns as samples).
#' @param meta A data frame of metadata with rownames matching colnames of counts.
#' @param groupVariable A character string specifying the column in meta indicating the group variable. Default is "condition".
#' @param fileName Optional, name of the file to save the results.
#'
#' @return A list containing pvalue, logFoldChange, padj, and the raw result.
#'
#' @examples
#' # Example:
#' counts <- matrix(rpois(100, lambda = 10), nrow = 10)
#' meta <- data.frame(condition = rep(c("A", "B"), each = 5), row.names = paste0("Sample", 1:10))
#' res <- bs_fastANCOM(counts = counts, meta = meta)
#'
#' load("counts_meta.Rdata")
#' res <- bs_fastANCOM(counts = counts, meta = meta)
#' 
#' @export
bs_fastANCOM <- function(counts, meta, groupVariable = "condition", fileName = NULL) {
  if (inherits(counts, "data.frame"))
    counts <- as.matrix(counts)
  
  if (!all(!is.na(counts)))
    warning("bs_fastANCOM: NAs in counts.")
  if (sum(counts < 0) > 0)
    warning("bs_fastANCOM: Counts have negative values.")
  
  if (!all(colnames(counts) %in% rownames(meta))) {
    stop("Column names of counts and row names of meta do not match.")
  }
  counts <- counts[, colnames(counts) %in% rownames(meta), drop = FALSE]
  meta <- meta[colnames(counts), , drop = FALSE]
  
  group <- as.factor(meta[, groupVariable])
  
  if (!requireNamespace("fastANCOM", quietly = TRUE)) {
    stop("The fastANCOM package is required but is not installed. Install it using devtools::install_github('zdk123/fastANCOM').")
  }
  
  tryCatch({
    # Perform fastANCOM analysis
    fit <- fastANCOM::fastANCOM(Y = t(counts), x = group)
    results <- fit$results
    
    # Extract p-values, logFoldChange, and adjusted p-values
    pvalue <- fit$results$final$log2FC.pval
    logFoldChange <- fit$results$final$log2FC
    padj <- fit$results$final$log2FC.qval
    
    # Create output list
    out <- list(
      pvalue = pvalue,
      logFoldChange = logFoldChange,
      padj = padj,
      result = fit
    )
    
    if (!is.null(fileName)) {
      saveRDS(out, file = fileName)
    }
    
    return(out)
  }, error = function(e) {
    stop("fastANCOM analysis failed: ", e$message)
  })
}
