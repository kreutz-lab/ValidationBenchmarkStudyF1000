# bs_plotSignificance
#
#   This function was implemented to plot significance over all projects and tests as an image (=heatmap)
#
# # Example:
# dfFolder <- "../Results/5.1.1_Aim2_primary_metaSPARSim_exp"
# DF_significance <- readRDS(paste0(dfFolder,"/","DF_significance.RDS"))
#
# bs_plotSignificance(DF_significance, file="out.pdf",titel="Testname\n Hypothesis is ...")
# bs_plotSignificance(cbind(DF_significance,otherLogicalColumns), file="out.pdf",titel="Testname\n Hypothesis is ...")


bs_plotSignificance <- function(DF_significance,file=NULL,titel=""){
  
  test_cols <- which(sapply(DF_significance, is.logical)) # indices
  test_names <- names(DF_significance)[test_cols] 
  # data_names <- unique(DF_significance$project_dataset) 
  # project_names <- unique(DF_significance$project) 
  
  sigMatrix <- as.matrix(DF_significance[,test_names])
  colnames(sigMatrix) <- test_names
  
  if(!is.null(file)){
    save(DF_significance, titel, file, file=paste0(file,".Rdata"))
    pdf(file,width=15,height=15)
  }
  image(t(sigMatrix),ylab="features (all projects)",xlab="tests",main=titel,useRaster = TRUE, axes=F)
  axis(1, at = seq(0, 1, length.out = ncol(sigMatrix)), labels = colnames(sigMatrix),cex.axis=0.4)
  axis(2, tick = F, labels = FALSE)
  #axis(2, at = seq(0, 1, length.out = nrow(sigMatrix)), labels = 1:nrow(sigMatrix), las = 1)
  title(main = titel)
  
  project_changes <- which(diff(as.numeric(factor(DF_significance$project))) != 0)
  for (pos in project_changes) {
    abline(h = pos / nrow(sigMatrix), col = "black", lwd = 1)
  }
  
  if(!is.null(file))
    dev.off()
  
}
  