#counts <- dfanzL$Overlap
# counts2intDensity(counts)
#
# This density function also works for count data that is not discrete, e.g. due to bias correction
# It assigns proportions to the closest integer.
# Example: A count 1.2 will add 0.2 to 1 and 0.8 to 2.
#
# Example:
# counts <- c(1, 2, 3, 1.2, 2.7, 3.5, 4.9, 2.3)
# msb.counts2intDensity(counts)

msb.counts2intDensity <- function(counts,xs=NULL){
  
  if(sum(!is.na(counts))>0){
    if(is.null(xs))
      xs <- floor(min(counts,na.rm=T)):ceiling(max(counts,na.rm=T))
    densi <- list(x=xs,y=array(dim=length(xs)))
    for(i in 1:length(xs)){
      x <- xs[i]
      closeToInt <- which(counts>x-1 & counts<x+1)
      
      diffToX <- counts[closeToInt]-x # range: -1 ... 1
      diffToX_neg <- diffToX[diffToX<0]
      diffToX_pos <- diffToX[diffToX>0]
      densi$y[i] <- sum(counts==x,na.rm=T) + sum(1-abs(diffToX_neg),na.rm=T) + sum(1-diffToX_pos,na.rm=T) 
    }
  }else{
    densi <- NULL
  }
  
  return(densi)
}