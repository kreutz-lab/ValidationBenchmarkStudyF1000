# Plot the number of significant features vs. the sensitivity derived from simgulations
#
# A kind of correlation should be seen for confirming that sens. derived from simulations is realistic
#
# Example:
# out <- bs_DA2ROC(d)
# dfSens <- data.frame(out$dfTarget)
# plot <- bs_DA_plotSensVsSignificant(d,dfSens)
# print(plot)

bs_DA_plotSensVsSignificant <- function(d,dfSens){
  
  for(name in names(d)){
    rownames(dfSens) <- dfSens$Test
    dfSig <- data.frame(anzSig=colSums(d[[1]]$original$DA$p_adjusted<0.05,na.rm=T))
    
    df <- merge(dfSens,dfSig, by=0, all=T)
    
    my_colors <- bs_colorsTests(df$Test)
    
    out <- ggplot(df, aes(x = sensitivity, y = anzSig, label = Test, color=Test)) +
      geom_point() +  # Add points
      geom_text(vjust = -1, hjust = 0.5, size = 3) +  # Add text labels
      theme_bw() +  # Use a clean theme
      scale_color_manual(values = my_colors) +  # Apply the custom colors
      scale_fill_manual(values = my_colors) +  # Apply the
      labs(title = "Sensitivity  vs.  Number of significant", x = "Sensitivity", y = "No. of p.adjusted < 0.05") +  # Add labels
      theme(
        axis.text = element_text(size = 10),  # Adjust the font size of axis labels
        axis.title = element_text(size = 14),  # Adjust the font size of axis titles
        plot.title = element_text(size = 16)  # Adjust the font size of the plot title
      )
  }
  out$corDf <- data.frame(pearson = cor(df$sensitivity,df$anzSig,method = "pearson"),
                      spearman = cor(df$sensitivity,df$anzSig,method = "spearman")) 
  rownames(out$corDf) <- names(d)
  
  return(out)
}