# Rarefying like in Nearing et al
#
# @param counts feature x samples
#
# Example:
# tmp <- bs_doRarefy(counts,meta)
# counts <- tmp$counts
# meta <- tmp$meta


bs_doRarefy <- function(counts,meta,minCounts=2000){
  
  cat("bs_doRarefy: Rarefy is (in this implementation) a random process, i.e. repetition leads to different outcomes.\n")
  if(ncol(counts)!=nrow(meta))
    stop("bs_doRarefy: ncol(counts)!=nrow(meta)")
  
  moreThan2000 <- colSums(counts) >= minCounts # Number from Nearing et. al
  
  if(sum(moreThan2000)>2){
    counts <- counts[,moreThan2000] # only consider those samples (as in Nearing)
    meta <- meta[moreThan2000, , drop=F] # only consider those samples (as in Nearing)
    
    samples <- min(colSums(counts))
    counts <- t(vegan::rrarefy(t(counts),samples))
    print(paste0("rrarefy (SubsampleSize=",samples,")..."))
  }
  else # do nothing
    warning("rrarefy not done because too few sample has >= counts.")
  
  return(list(counts=counts,meta=meta))
  
}