# Maaslin

# deps = c("Maaslin2")
# for (dep in deps){
#   if (dep %in% installed.packages()[,"Package"] == FALSE){
#     if (!requireNamespace("BiocManager", quietly = TRUE))
#       install.packages("BiocManager")
#     
#     BiocManager::install(deps)
#   }
#   library(dep, character.only = TRUE)
# }
# 
#
#
# Example:
# res <- bs_Maaslin3(count=d[[i]][[j]]$counts[1:300,],meta=d[[i]][[j]]$meta)


bs_Maaslin3 <- function(counts,meta,groupVariable="condition",fileName=NULL,min_prevalence=0){
  
  if(inherits(counts,"data.frame"))
    counts <- as.matrix(counts)

  condLevels <- levels(as.factor(meta[,groupVariable]))
  # only select samples that belong to the first two conditions:
  if(length(condLevels)>2){
    drin <- meta[,groupVariable]==condLevels[1] | meta[,groupVariable]==condLevels[2]
    meta <- meta[drin,,drop=F]
    meta[,groupVariable] <- as.factor(as.character(meta[,groupVariable])) # necessary to update factor levels
    counts <- counts[,drin]
    warning("bs_Maaslin3 can only work with two conditions, only the first two conditions are analyzed, the remaing samples do not enter.")
  }else if(length(condLevels)<2)
    stop("length(condLevels)<2")
  
  if(!all(!is.na(counts)))
    warning("bs_Maaslin3: NAs in counts.")
  if(sum(counts<0,na.rm=T)>0)
    warning("bs_Maaslin3: Counts have negative values.")

  #if(sum(colSums(counts > 0) > 7)<1){
    warning("Note: Maaslin filters the data several time and e.g. according to \n colSums(data_zeros > min_abundance) > min_samples with min_abundance=0 und min_samples=7")
    warning("If this criterion is not met, then an error is thrown.")
  #}
  
  groupings <- meta  # renaming from bs terminology to Nearing
  ASV_table <- counts # renaming from bs terminology to Nearing

  output <- "tmp" # output folder for results, if written
  
  res <- suppressMessages(suppressWarnings(maaslin3::maaslin3( 
    ASV_table, groupings, output, transform = "LOG",
    fixed_effects = groupVariable, min_prevalence = min_prevalence,
    standardize = FALSE, max_significance = Inf)))
  
  unlink('output', recursive=TRUE)
  logging::logReset()

  if(length(intersect(row.names(counts),res$fit_data_abundance$results$feature))>nrow(counts)*0.01){
    res$fit_data_abundance$results <- res$fit_data_abundance$results[match(row.names(counts) , res$fit_data_abundance$results$feature), ]
  }else{
    res$fit_data_abundance$results <- res$fit_data_abundance$results[match(paste("X",row.names(counts),sep="") , res$fit_data_abundance$results$feature), ]
  }
    
  pvals <- setNames(res$fit_data_abundance$results$pval_individual,res$fit_data_abundance$results$feature)
  qvals <- setNames(res$fit_data_abundance$results$qval_individual,res$fit_data_abundance$results$feature)

  # Here, pval_individual from abundance data is con
  out <- list(pvalue=pvals, 
              logFoldChange=pvals, 
              padj=qvals, 
              result=res$fit_data_abundance$results  )
  return(out)
}

