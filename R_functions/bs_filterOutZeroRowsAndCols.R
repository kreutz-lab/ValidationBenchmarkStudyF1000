# Function to iteratively filter out rows and columns with no positive count values
#
#
# Example:
# count_matrix <- matrix(sample(0:5, 25, replace = TRUE), nrow = 5, ncol = 5)
# colnames(count_matrix) <- paste("Sample", 1:5, sep = "")
# rownames(count_matrix) <- paste("Gene", 1:5, sep = "")
# print(count_matrix)
# result <- bs_filterOutZeroRowsAndCols(count_matrix)

bs_filterOutZeroRowsAndCols <- function(mat) {
  row_indices <- 1:nrow(mat)
  col_indices <- 1:ncol(mat)
  
  counter <- 0
  
  repeat {
    # Identify rows and columns with all zero counts
    zero_rows <- rowSums(mat > 0) == 0
    zero_cols <- colSums(mat > 0) == 0
    
    # Check if there are any rows or columns to remove
    if (any(zero_rows) || any(zero_cols) && counter<10) {
      counter <- counter + 1
      # Remove rows and columns with all zero counts
      mat <- mat[!zero_rows, , drop = FALSE]
      mat <- mat[, !zero_cols, drop = FALSE]
      
      # Update the indices
      row_indices <- row_indices[!zero_rows]
      col_indices <- col_indices[!zero_cols]
    } else {
      # Break the loop if no more rows or columns need to be removed
      break
    }
  }
  return(list(filtered_matrix = mat, row_indices = row_indices, col_indices = col_indices))
}
