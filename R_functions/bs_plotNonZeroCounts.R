# Function to create a horizontal barplot of non-zero counts for LR columns
#
# Example usage:
# bs_plotNonZeroCounts(dfSelected)
bs_plotNonZeroCounts <- function(dfSelected, pattern="^LR") {
  library(dplyr)
  library(ggplot2)
  library(stringr)
  
  # Get column names that start with "LR"
  names(dfSelected) <- sub("LR.(Inter","(Inter",names(dfSelected),fixed=T)  # to exclude intercept from plotting
  lr_cols <- names(dfSelected)[grep(pattern, names(dfSelected))]
  
  if (length(lr_cols) == 0) {
    stop("No columns starting with ",pattern," found in the dataset")
  }
  
  # Calculate number of non-zero values for each LR column
  nonzero_counts <- sapply(dfSelected[, lr_cols, drop = FALSE], function(x) sum(x != 0, na.rm = TRUE))
  
  # Create a dfSelected frame for plotting
  plot_data <- data.frame(
    column = sub("^.","",sub(pattern,"",names(nonzero_counts))),
    count = nonzero_counts
  ) %>%
    # Sort in descending order
    arrange(count)
  
  # Convert column to factor with levels in the sorted order
  # This ensures the bars are displayed in the correct order
  plot_data$column <- factor(plot_data$column, levels = plot_data$column)
  
  # Create the horizontal barplot
  ggplot(plot_data, aes(x = count, y = column)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    # Add count labels at the end of each bar
    geom_text(aes(label = count), hjust = -0.2, size = 3.5) +
    # Add labels and title
    labs(
      title = "Number of selections of DCs",
      #subtitle = paste(length(lr_cols), "DCs out of ", length(names(nonzero_counts))," selected"),
      x = "Number of hypotheses",
      y = NULL  # Remove y-axis label since column names are shown
    ) +
    # Expand the x-axis to make room for labels
    scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
    # Use a clean theme
    theme_minimal() +
    # Customize theme elements
    theme(
      plot.title = element_text(face = "bold"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.y = element_text(size = 10)
    )
}

