# This function uses the outcome of bs_summarize_aim2regression and plots
# the proportions of hypothesis validations of the two simululation tools against
# each other. Colors are chosen according to filtered.
#
# Example:
# dfSelected <- bs_summarize_aim2regression(pattern2=c("simEquiv","Nearing"),patternNot = c("NoAncom","NoLefse","simEquivSpd"),outFile = "aim2regression_PaperVersion.xlsx")
# bs_plotHypothesisComparison(dfSelected)

bs_plotHypothesisComparison <- function(dfSelected,max_overlaps=30,labelLb=-Inf, labelUb = Inf){
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(ggrepel)  # For non-overlapping text labels
  
  tmp <- dfSelected[,1:8]
  tmp$dataName <- sub("5.2+5.4_spD_ancom_Nearing_simEquiv","sparseDOSSA2",tmp$dataName,fixed=T)
  tmp$dataName <- sub("5.2+5.4_mSP_ancom_Nearing_simEquiv","metaSPARSim",tmp$dataName,fixed=T)
  tmp$proportion_Htrue <- tmp$propHtrue
  
  #print(head(tmp))
  
  # Reshape data to wide format for comparison
  plot_data <- tmp %>%
    # Select relevant columns
    select(dataName, filtered, hypoName, proportion_Htrue) %>%
    # Pivot to wide format with dataName as columns
    pivot_wider(
      id_cols = c(filtered, hypoName),
      names_from = dataName,
      values_from = proportion_Htrue
    ) %>%
    # Filter out rows where either value is NA (might happen if a hypothesis 
    # doesn't exist in both datasets)
    filter(!is.na(sparseDOSSA2), !is.na(metaSPARSim))
  
  # Create labels for points where 0.2 < proportion_Htrue < 0.8 in either dataset
  plot_data <- plot_data %>%
    mutate(
      show_label = (sparseDOSSA2 > labelLb & sparseDOSSA2 < labelUb) | 
        (metaSPARSim > labelLb & metaSPARSim < labelUb)
    )

    if(!is.infinite(labelLb) || !is.infinite(labelUb))
    subtitel <- paste0("Labels shown for hypotheses with ",labelLb," < proportion_Htrue < ",labelUb)
  else
    subtitel = ""
  
  # Create the scatter plot
  myplot <- ggplot(plot_data, aes(x = sparseDOSSA2, y = metaSPARSim, color = filtered)) +
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
      title = "Proportion H=true for the two simulation tools",
      subtitle = "Labels shown for hypotheses with 0.2 < proportion_Htrue < 0.8",
      x = "sparseDOSSA2 proportion H=true",
      y = "metaSPARSim proportion H=true",
      color = "Filtered Status"
    ) +
    theme(
      plot.title = element_text(size = 16)  # Increase title size (default is usually 11 or 12)
    )+
    # Set color scale
    scale_color_brewer(palette = "Set1") +
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