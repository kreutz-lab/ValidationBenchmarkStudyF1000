# ANCOM-BC
#'
#'  Performs ANCOM-BC differential abundandance analysis
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
#' #Installation via: 
#'
#' deps = c("exactRankTests", "nlme", "dplyr", "ggplot2", "compositions")
#' for (dep in deps){
#'   if (dep %in% installed.packages()[,"Package"] == FALSE){
#'     install.packages(dep)
#'   }
#'   library(dep, character.only = TRUE)
#' }
#' if (!requireNamespace("BiocManager", quietly = TRUE))
#'   install.packages("BiocManager")
#' BiocManager::install("ANCOMBC")
#' library(ANCOMBC)
#' 
#' 
#' #Example:
#' d <- readRDS(paste0(bs_path,"Analyses/Data/example_data/bs_object2.RDS"))
#  res <- bs_ancombc(count=d[[1]][[1]]$counts[1:300,],meta=d[[1]][[1]]$meta)
#'
#'
#' @seealso \code{\link{bs_DA}}
#' @keywords aKeywordSuchAsBenchmarking
#' @export


### Helper function for ANCOM:
bs_ancombc <- function(counts, meta, groupVariable="condition",fileName=NULL,p_adj_method="BH"){

  if(inherits(counts,"data.frame")) # convert to matrix if data.frame
    counts <- as.matrix(counts)
  
  condLevels <- levels(as.factor(meta[,groupVariable]))
  # only select samples that belong to the first two conditions:
  if(length(condLevels)>2){
    drin <- meta[,groupVariable]==condLevels[1] | meta[,groupVariable]==condLevels[2]
    meta <- meta[drin,,drop=F]
    meta[,groupVariable] <- as.factor(as.character(meta[,groupVariable])) # necessary to update factor levels
    counts <- counts[,drin]
    warning("bs_ancombc can only work with two conditions, only the first two conditions are analyzed, the remaing samples do not enter.")
  }else if(length(condLevels)<2)
    stop("length(condLevels)<2")
  
  if(!all(!is.na(counts))){
    counts[is.na(counts)] <- 0
    warning("bs_ancom: NA in counts => will be replaced by zero.")
  }
  if(sum(counts<0,na.rm=T)>0)
    warning("bs_ancom: Counts have ",sum(counts<0)," negative values.")
  
  phylo <- bs_data2phyloseq(counts,meta,buildTree = F)
  
  # Nearing used another (deprecated) implementation of the ANCOM function, here we use the current version
  res <- ancombc2(data=phylo, fix_formula=groupVariable, p_adj_method = p_adj_method, tax_level = "Species")
  
  # Mapping result taxa to rows of count matrix:
  ind_lfc <- grep(paste0("lfc_",groupVariable),names(res$res))
  ind_p <- grep(paste0("p_",groupVariable),names(res$res))
  ind_q <- grep(paste0("q_",groupVariable),names(res$res))
  
  mapped_lfc <- setNames(res$res[[ind_lfc]][match(rownames(counts), res$res$taxon)],rownames(counts))
  mapped_p <- setNames(res$res[[ind_p]][match(rownames(counts), res$res$taxon)],rownames(counts))
  mapped_q <- setNames(res$res[[ind_q]][match(rownames(counts), res$res$taxon)],rownames(counts))
  
  out <- list(pvalue=mapped_p, logFoldChange=mapped_lfc, padj=mapped_q, result=res  )
  return(out)
  
}
