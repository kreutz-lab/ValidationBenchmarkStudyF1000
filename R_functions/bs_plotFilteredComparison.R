# This function creates an alternative plot comparing filtered vs unfiltered proportion_Htrue
# with colors based on dataName

# Example usage:
# dfSelected <- bs_summarize_aim2regression(pattern2=c("simEquiv","Nearing"),
#                                          patternNot = c("NoAncom","NoLefse","simEquivSpd"),
#                                          outFile = "aim2regression_PaperVersion.xlsx")
# bs_plotFilteredComparison(dfSelected)
# 


bs_plotFilteredComparison <- function(dfSelected,max_overlaps=30,labelLb=-Inf, labelUb = Inf){
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(ggrepel)  # For non-overlapping text labels
  
  tmp <- dfSelected[,1:8]
  tmp$dataName <- sub("5.2+5.4_spD_ancom_Nearing_simEquiv","sparseDOSSA2",tmp$dataName, fixed=T)
  tmp$dataName <- sub("5.2+5.4_mSP_ancom_Nearing_simEquiv","metaSPARSim",tmp$dataName, fixed=T)
  tmp$proportion_Htrue <- tmp$propHtrue
  
  # Reshape data to wide format, but now comparing filtered vs unfiltered
  # First ensure we have consistent naming for filtered levels
  tmp$filtered <- ifelse(tolower(tmp$filtered) == "un-filtered", "Un-filtered", tmp$filtered)
  
  plot_data <- tmp %>%
    # Select relevant columns
    select(dataName, filtered, hypoName, proportion_Htrue) %>%
    # Pivot to wide format with filtered status as columns
    pivot_wider(
      id_cols = c(dataName, hypoName),
      names_from = filtered,
      values_from = proportion_Htrue
    ) %>%
    # Rename columns to ensure they match expected names
    rename_with(~ ifelse(. == "Un-filtered", "Un-filtered", .), everything()) %>%
    # Filter out rows where either value is NA
    filter(!is.na(Filtered), !is.na(`Un-filtered`))
  
  # Create labels for points where 0.2 < proportion_Htrue < 0.8 in either filtered status
  plot_data <- plot_data %>%
    mutate(
      show_label = (Filtered > labelLb & Filtered < labelUb) | 
        (`Un-filtered` > labelLb & `Un-filtered` < labelUb)
    )
  
  if(!is.infinite(labelLb) || !is.infinite(labelUb))
    subtitel <- paste0("Labels shown for hypotheses with ",labelLb," < proportion_Htrue < ",labelUb)
  else
    subtitel = ""
  
  # Create the scatter plot
  myplot <- ggplot(plot_data, aes(x = `Un-filtered`, y = Filtered, color = dataName)) +
    # Add points
    geom_point(size = 3, alpha = 0.7) +
    # Add text labels for selected points
    geom_text_repel(
      data = filter(plot_data, show_label),
      aes(label = hypoName),
      size = 3,
      box.padding = 0.5,
      point.padding = 0.3,
      force = 3,               # Increase repulsion force
      max.overlaps = max_overlaps,  # Allow more overlaps
      segment.color = "grey50"
    ) +
    # Add diagonal line for reference
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    # Add proper labels
    labs(
      title = "Proportion H=true for filtered vs unfiltered",
      subtitle = subtitel,
      x = "Unfiltered proportion H=true",
      y = "Filtered proportion H=true",
      color = "Simulation Tool"
    ) +
    theme(
      plot.title = element_text(size = 16)  # Increase title size (default is usually 11 or 12)
    ) +
    # Set color scale - using a different palette for contrast with the other plot
    scale_color_brewer(palette = "Dark2") +
    # Set limits to be the same on both axes
    coord_fixed(ratio = 1, xlim = c(0, 1), ylim = c(0, 1)) +
    # Use a clean theme
    theme_minimal() +
    # Customize theme elements
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  return(myplot)
}

