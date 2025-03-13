# Wilcoxon CLR
#
#   Wilcoxon Test
#
# Example:
# res <- bs_wilcox.test(count=d[[i]][[j]]$counts[1:300,],meta=d[[i]][[j]]$meta)


bs_wilcox.test <- function(counts,meta,groupVariable="condition",fileName=NULL){
  
  if(inherits(counts,"data.frame"))
    counts <- as(counts,"matrix")

  condLevels <- levels(as.factor(meta[,groupVariable]))
  # only select samples that belong to the first two conditions:
  if(length(condLevels)>2){
    drin <- meta[,groupVariable]==condLevels[1] | meta[,groupVariable]==condLevels[2]
    meta <- meta[drin,,drop=F]
    meta[,groupVariable] <- as.factor(as.character(meta[,groupVariable])) # necessary to update factor levels
    counts <- counts[,drin]
    warning("bs_wilcox.test can only work with two conditions, only the first two conditions are analyzed, the remaing samples do not enter.")
  }else if(length(condLevels)<2)
    stop("length(condLevels)<2")
  
  if(!all(!is.na(counts)))
    warning("bs_wilcox.test: NAs in counts.")
  if(sum(counts<0,na.rm=T)>0)
    warning("bs_wilcox.test: Counts have negative values.")
  
  ASV_table <- counts # renaming from bs terminology to Nearing
  groupings <- meta  # renaming from bs terminology to Nearing
  # colnames(groupings)
  # colnames(groupings)[1] <- "places"
  # 
  #add pseudo count
  CLR_table <- data.frame(apply(ASV_table + 1, 2, function(x){log(x) - mean(log(x))}))
  ## get clr table
  
  #apply wilcox test 
  # pvals <- apply(CLR_table, 1, function(x) wilcox.test(x ~ groupings[,1], exact=F)$p.value)
  pvals <- apply(CLR_table, 1, function(x) wilcox.test(x ~ groupings[,groupVariable], exact=F)$p.value)
  levs <- as.character(unique(groupings[,groupVariable]))
  if(length(levs)!=2)
    warning("bs_wilcox.test: length(levels(meta[,groupVariable])) != 2")
  
  clrMatrix <- as.matrix(CLR_table)
  effectSize <- rowMedians(clrMatrix[,as.character(meta[,groupVariable])==levs[2]],na.rm=T)-rowMedians(clrMatrix[,as.character(meta[,groupVariable])==levs[1]],na.rm=T)
  
  # write.table(pvals, file=args[[3]], sep="\t", col.names = NA, quote=F)
  
  out <- list(pvalue=pvals, logFoldChange=effectSize, padj=p.adjust(pvals,method="BH"), result=pvals  )
  return(out)
}

