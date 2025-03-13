#' T-test rare
#'
#' @param counts
#' @param meta meta data as data.frame
#' @param groupVariable, default: "condition"
#'
#' @return a list consisting of 
#' pvalue
#' logFoldChange
#' padj and 
#' result object
#' 
#' @examples
#' #Example:
#' d <- readRDS(paste0(bs_path,"Analyses/Data/example_data/bs_object2.RDS"))
#' res <- bs_ttest(count=d[[1]][[1]]$counts[1:300,],meta=d[[1]][[1]]$meta)
#'
#'
#' @seealso \code{\link{bs_DA}}
#' @keywords aKeywordSuchAsBenchmarking
#' @export


bs_ttest_rare <- function(counts,meta,groupVariable="condition",fileName=NULL){
  
  if(inherits(counts,"data.frame"))
    counts <- as.matrix(counts)
  
  condLevels <- levels(as.factor(meta[,groupVariable]))
  # only select samples that belong to the first two conditions:
  if(length(condLevels)>2){
    drin <- meta[,groupVariable]==condLevels[1] | meta[,groupVariable]==condLevels[2]
    meta <- meta[drin,,drop=F]
    meta[,groupVariable] <- as.factor(as.character(meta[,groupVariable])) # necessary to update factor levels
    counts <- counts[,drin]
    warning("bs_ttest_rare can only work with two conditions, only the first two conditions are analyzed, the remaing samples do not enter.")
  }else if(length(condLevels)<2)
    stop("length(condLevels)<2")
  
  if(!all(!is.na(counts)))
    warning("bs_ttest: NAs in counts.")
  if(sum(counts<0,na.rm=T)>0)
    warning("bs_ttest: Counts have negative values.")
  
  ASV_table <- counts # renaming from bs terminology to Nearing
  groupings <- meta  # renaming from bs terminology to Nearing
 

  ASV_table <- as.data.frame(counts)
  
  ## Rarefy the data
  tmp <- bs_doRarefy(ASV_table,groupings)
  ASV_table_rare <- tmp$counts
  groupings <- tmp$meta

  #ASV_table_rare <- NULL
  #try(ASV_table_rare <- rrarefy(ASV_table, sample=min.sample.size))
  #if(is.null(ASV_table_rare))
   # save.image("rarefy_error.Rdata")
  
  #apply t-test to rarefied table
  pvals <- apply(ASV_table_rare, 1, function(x) t.test(x ~ groupings[,groupVariable], exact=F)$p.value)
  levs <- as.character(unique(groupings[,groupVariable]))
  if(length(levs)!=2)
    warning("bs_ttest: length(levels(meta[,groupVariable])) != 2")
  
  ASVMatrix <- as.matrix(ASV_table_rare)
  effectSize <- log2(rowMeans(ASVMatrix[,as.character(groupings[,groupVariable])==levs[2]],na.rm=T))-log2(rowMeans(ASVMatrix[,as.character(groupings[,groupVariable])==levs[1]],na.rm=T))
  
  # write.table(pvals, file=args[[3]], sep="\t", col.names = NA, quote=F)
  # print(pvals)
  out <- list(pvalue=pvals, logFoldChange=effectSize, padj=p.adjust(pvals,method="BH"), result=pvals  )
  return(out)
}

