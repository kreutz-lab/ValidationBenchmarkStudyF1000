bs_metagenomeSeq_zig <- function(counts, meta, groupVariable="condition", fileName=NULL){
  # Convert counts to matrix if needed
  if(inherits(counts, "data.frame")) counts <- as.matrix(counts)
  
  # Handle conditions - only analyze first two levels
  condLevels <- levels(as.factor(meta[,groupVariable]))
  if(length(condLevels) > 2){
    drin <- meta[,groupVariable]==condLevels[1] | meta[,groupVariable]==condLevels[2]
    meta <- meta[drin, , drop=F]
    meta[,groupVariable] <- as.factor(as.character(meta[,groupVariable])) # reset factor levels
    counts <- counts[, drin]
    warning("bs_metagenomeSeq can only work with two conditions, only the first two conditions are analyzed, the remaining samples do not enter.")
  } else if(length(condLevels) < 2) {
    stop("length(condLevels) < 2")
  }
  
  # Input validation
  if(!all(!is.na(counts))) warning("bs_metagenomeSeq: NAs in counts.")
  if(sum(counts < 0, na.rm=TRUE) > 0) warning("bs_metagenomeSeq: Counts have negative values.")
  
  # Remove samples with zero counts
  moreThan0 <- colSums(counts) > 0
  counts <- counts[, moreThan0] # Actually remove zero-count columns
  meta <- meta[moreThan0, , drop=F]
  
  # Create MRexperiment object
  pheno <- Biobase::AnnotatedDataFrame(meta)
  feature_data <- data.frame("Feature" = rownames(counts))
  rownames(feature_data) <- feature_data$Feature
  feature_data <- Biobase::AnnotatedDataFrame(feature_data)
  
  test_obj <- metagenomeSeq::newMRexperiment(counts = counts, 
                                             phenoData = pheno, 
                                             featureData = feature_data)
  
  # Normalize with cumulative sum scaling
  test_obj_norm <- metagenomeSeq::cumNorm(test_obj)
  
  # Create model matrix
  formula <- as.formula(paste("~", groupVariable))
  mod <- model.matrix(formula, data = pData(test_obj_norm))
  
  # Fit ZIG model instead of feature model
  zigFit <- metagenomeSeq::fitZig(test_obj_norm, mod)
  
  # Extract results
  res_table <- metagenomeSeq::MRcoefs(zigFit, number = nrow(counts))
  
  # Format results similar to original function
  out <- list(
    pvalue = res_table$pvalues,
    logFoldChange = res_table$logFC,
    padj = res_table$adjPvalues,
    result = res_table$pvalues
  )
  
  return(out)
}