#' ZINQ Analysis
#'
#' @param counts A matrix of counts.
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
#' res2 <- bs_zinq(counts=counts, meta=meta)
#'
#' @export
#' 
bs_zinq <- function(counts, meta, groupVariable = "condition", fileName = NULL) {
  if (inherits(counts, "data.frame"))
    counts <- as.matrix(counts)
  
  if (!all(!is.na(counts)))
    warning("bs_zinq: NAs in counts.")
  if (sum(counts < 0) > 0)
    warning("bs_zinq: Counts have negative values.")
  
  if (!all(colnames(counts) %in% rownames(meta))) {
    stop("Column names of counts and row names of meta do not match.")
  }
  counts <- counts[, colnames(counts) %in% rownames(meta), drop = FALSE]
  meta <- meta[colnames(counts), , drop = FALSE]
  
  moreThan0 <- colSums(counts) > 0
  if (sum(moreThan0) == 0) {
    stop("No samples with non-zero counts remain after filtering.")
  }
  counts <- counts[, moreThan0, drop = FALSE]
  meta <- meta[moreThan0, , drop = FALSE]
  
  group <- as.factor(meta[, groupVariable])
  
  if (!requireNamespace("ZINQ", quietly = TRUE)) {
    stop("The ZINQ package is required but is not installed. Install it using devtools::install_github('wdl2459/ZINQ-v2').")
  }
  
  results_list <- lapply(1:nrow(counts), function(i) {
    dat <- data.frame(y = counts[i, ], condition = group)
    tryCatch({
      res <- ZINQ::ZINQ_tests(
        formula.logistic = y ~ condition,
        formula.quantile = y ~ condition,
        C = "condition",
        data = dat
      )
      return(res$pvalue.logistic)
    }, error = function(e) {
      return(NA)
    })
  })
  
  pvalue <- as.vector(do.call(rbind, results_list))
  logFoldChange <- array(dim=length(pvalue))
  padj <- p.adjust(pvalue,method="BH")
  
  out <- list(
    pvalue = pvalue,
    logFoldChange = logFoldChange,
    padj = padj
  )
  
  if (length(out$pvalue) != nrow(counts))
    stop("length(out$pvalue) != nrow(counts)")
  
  return(out)
}
