# ALDEX
#
#   Calling ALDEX for doing differential abundance analysis
#
# Installation via
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("ALDEx2")
# library("ALDEx2")
#
# Example:
# res <- bs_aldex(count=d[[i]][[j]]$counts[1:300,],meta=d[[i]][[j]]$meta)

bs_aldex <- function(counts,meta,groupVariable="condition",fileName=NULL){

  cat("bs_aldex: Please note, that aldex is a Monte-Carlo method that is (at least in this implemntation) not deterministic, i.e. repeating leads not to exactly the same result!\n")  
  if(inherits(counts,"data.frame"))
    counts <- as.matrix(counts)
  
  if(!all(!is.na(counts)))
    warning("bs_aldex: NAs in counts.")
  if(sum(counts<0,na.rm=T)>0)
    warning("bs_aldex: Counts have negative values.")
  
  
# results <- aldex(reads=ASV_table, conditions = groupings[,1], mc.samples = 128, test="t", effect=TRUE,
#                  include.sample.summary = FALSE, verbose=T, denom="all")
# 

  condLevels <- levels(meta[,groupVariable])
  # only select samples that belong to the first two conditions:
  if(length(condLevels)>2){
    drin <- meta[,groupVariable]==condLevels[1] | meta[,groupVariable]==condLevels[2]
    meta <- meta[drin,,drop=F]
    meta[,groupVariable] <- as.factor(as.character(meta[,groupVariable])) # necessary to update factor levels
    counts <- counts[,drin]
    warning("ALDEx2 can only work with two conditions, only the first two conditions are analyzed, the remaing samples do not enter.")
  }
  
  res <- ALDEx2::aldex(reads=counts, conditions = as.character(meta[,groupVariable]), mc.samples = 128, test="t", effect=TRUE,
                   include.sample.summary = FALSE, verbose=T, denom="all")
  
  # we = welch
  # wi = wilcoxon
  pvalue <- array(NA,dim=nrow(counts))
  logFoldChange <- array(NA,dim=nrow(counts))
  padj <- array(NA,dim=nrow(counts))
  ind <- which(rowSums(counts)>0)
  
  # ensure same ordering
  if (!all(rownames(counts[ind,]) == rownames(res))) {
    res <- res[match(rownames(counts[ind,]), rownames(res)), ]
  }

  pvalue[ind] <- res$wi.ep
  logFoldChange[ind] <- res$effect
  padj[ind] <- res$wi.eBH
  
  out <- list(pvalue=pvalue, logFoldChange=logFoldChange, padj=padj, result=res  )
  return(out)
}
