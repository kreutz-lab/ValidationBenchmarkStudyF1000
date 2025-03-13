#' bs_lefse
#'
#' Differential abundance analysis using 
#' Liner discriminant analysis (LDA) effect size (LEFSe)
#' 
#' https://rdrr.io/github/yiluheihei/microbiomeMarker 
#'
#' 
#' Package installation:
#' install.packages("remotes")
#' remotes::install_github("yiluheihei/microbiomeMarker")
#' 
#' LeFSe Parameters kw_cutoff, lda_cutoff, wilcoxon_cutoff are "switched off"
#' i.e. in a way that all p-values are available 
#' 
#' @param counts
#' @param meta
#' @param groupVariable="condition"
#'
#' @return 
#' 
#' @examples
#' # The function must be applied to the rarefied data!
#' res <- bs_lefse(count=d[[i]][[j]]$counts[1:300,],meta=d[[i]][[j]]$meta)
#' 
#'
#' @seealso \code{\link{anotherFunction}}
#' @keywords aKeywordSuchAsBenchmarking
#' @export

bs_lefse2 <- function(counts,meta,groupVariable="condition",fileName=NULL,doRarefy=T){
  
  if(inherits(counts,"data.frame"))
    counts <- as.matrix(counts)
  
  if(doRarefy){
    tmp <- bs_doRarefy(counts,meta)
    counts <- tmp$counts
    meta <- tmp$meta
  }
  
  if(!all(!is.na(counts)))
    warning("bs_wilcox.test: NAs in counts.")
  if(sum(counts<0,na.rm=T)>0)
    warning("bs_wilcox.test: Counts have negative values.")
  
  phylo <- bs_data2phyloseq(counts,meta,buildTree = F)
  #print(phylo)
  
  # Run Lefse - rarefy the data
  #res <- microbiomeMarker::run_lefse(phylo,group = groupVariable, norm = "rarefy", kw_cutoff = 1, wilcoxon_cutoff=1, lda_cutoff=0)
  res <- NULL
  marker <- list(pvalue=array(dim=nrow(counts)))
  
#  Delta <- 0 # relax thresholds, i.e. less output p-values if lefse does not run 
#  while(is.null(res) && Delta<1){
    try(res <- microbiomeMarker::run_lefse(phylo,group = groupVariable, 
                                           norm="TSS",  # Nearing did TSS after rarefying
                                           kw_cutoff = 1,
                                           wilcoxon_cutoff=1,
                                           lda_cutoff=0))
                                           #lda_cutoff=Delta))
#    Delta <- Delta + 0.05
  # lda_cutoff to output as many features as possible, 0 does frequently not work
#  }
  if(is.null(res))
    warning("Error: lefse did not succeed.")
  else{
    
    marker <- microbiomeMarker::marker_table(res)
    
    # Sort marker table in the same way as counts
    marker <- marker[match(paste("s__",row.names(counts),sep=""), marker$feature), ]
    print(paste0("lefse succeeded with ",sum(!is.na(marker$pvalue))," significant p-values."))
  }

  out <- list(pvalue=marker$pvalue, logFoldChange=NA*marker$pvalue, padj=p.adjust(marker$pvalue,method="BH"), result=res  )
  return(out)
}

