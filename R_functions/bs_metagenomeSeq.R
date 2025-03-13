# Metagenome Seq
#

#
# Example:
# res <- bs_metagenomeSeq(count=d[[i]][[j]]$counts[1:300,],meta=d[[i]][[j]]$meta)


bs_metagenomeSeq <- function(counts,meta,groupVariable="condition",fileName=NULL){
  
  #if(inherits(counts,"data.frame"))
   # counts <- as.matrix(counts)
  
  # Make sure meta and counts are ordered the same way
  meta <- meta[match(colnames(counts), rownames(meta)),, drop=FALSE]
  
  condLevels <- levels(as.factor(meta[,groupVariable]))
  # only select samples that belong to the first two conditions:
  if(length(condLevels)>2){
    drin <- meta[,groupVariable]==condLevels[1] | meta[,groupVariable]==condLevels[2]
    meta <- meta[drin,,drop=F]
    meta[,groupVariable] <- as.factor(as.character(meta[,groupVariable])) # necessary to update factor levels
    counts <- counts[,drin]
    warning("bs_metagenomeSeq can only work with two conditions, only the first two conditions are analyzed, the remaing samples do not enter.")
  }else if(length(condLevels)<2)
    stop("length(condLevels)<2")
  
  if(!all(!is.na(counts)))
    warning("bs_metagenomeSeq: NAs in counts.")
  if(sum(counts<0,na.rm=T)>0)
    warning("bs_meatgenomeSeq: Counts have negative values.")
  
  moreThan0 <- colSums(counts) > 0 #
  counts <- counts[,moreThan0] # only consider those samples, since cannot analyze columns with only zeros
  meta <- meta[moreThan0, , drop=F] # only consider those samples, since cannot analyze columns with only zeros

  ASV_table <- counts # renaming from bs terminology to Nearing
  
  data_list <- list()
  data_list[["counts"]] <- ASV_table
  data_list[["taxa"]] <- rownames(ASV_table)
  
  groupings <- meta
  pheno <- Biobase::AnnotatedDataFrame(groupings)
  pheno
  #counts <- Biobase::AnnotatedDataFrame(ASV_table)
  feature_data <- data.frame("ASV"=rownames(ASV_table),
                             "ASV2"=rownames(ASV_table))
  feature_data <- Biobase::AnnotatedDataFrame(feature_data)
  rownames(feature_data) <- feature_data@data$ASV
  
  
  test_obj <- metagenomeSeq::newMRexperiment(counts = data_list$counts, phenoData = pheno, featureData = feature_data)
  
  p <- metagenomeSeq::cumNormStat(test_obj, pFlag = T)
  p
  
  test_obj_norm <- metagenomeSeq::cumNorm(test_obj, p=p)
  
  fromula <- as.formula(paste("~ 1 +", paste(groupVariable, collapse = " + ")))
  pd <- pData(test_obj_norm)
  mod <- model.matrix(fromula, data=pd)
  regres <- metagenomeSeq::fitFeatureModel(test_obj_norm, mod)
  
#  res_table <- metagenomeSeq::MRfulltable(regres, number = length(rownames(ASV_table)))
  
  #### Get test results
    out <- list(pvalue=regres@pvalues, logFoldChange=regres@fitZeroLogNormal$logFC, padj=p.adjust(regres@pvalues,method="BH"), result=regres  )
  return(out)
}

