bs_plotProfiles <- function(profile_list,file=NULL,titel=""){
  
  # Determine the number of rows and columns for the layout
  num_plots <- length(profile_list)
  num_cols <- ceiling(sqrt(num_plots))  # You can adjust this based on your preference
  num_rows <- ceiling(num_plots / num_cols)
  
  # Open a PDF device
  if(!is.null(file)){
    save(profile_list,file,titel, file=paste0(file,".Rdata"))  # 
    pdf(file, width = 12, height = 12)  # Adjust width and height as needed
  }
  
  # Set up the multi-panel layout
  par(mfrow = c(num_rows, num_cols), mar = c(2, 2, 2, 2)*1)  # Reduce the margins
  
  # Plot each matrix in a separate panel
  for (i in seq_along(profile_list)) {
    mat <- profile_list[[i]]
    image(t(mat), main = names(profile_list)[i], ylab = "Features", xlab = "Samples", axes = FALSE, cex.main=0.7)
    axis(1, at = seq(0, 1, length.out = ncol(mat)), labels = colnames(mat), las = 2, las = 2, cex.axis=0.7)
    axis(2, at = seq(0, 1, length.out = nrow(mat)), labels = rownames(mat), las = 2, las = 2, cex.axis=0.7)
    box()
    if(i == ceiling(sqrt(num_plots))/2)
      mtext(titel, outer = F, cex = 1.5)
  }
  
  
  # Close the PDF device
  if(!is.null(file))
    dev.off()
  
}