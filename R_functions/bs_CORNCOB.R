# Corncob 
#
#   Calling Corncob for doing differential abundance analysis
#
# maxRunTimeInMin maximal Runtime in minutes
#
# Required packages (loaded in bs_init)
# library(corncob)
# library(phyloseq)
#
# Example:
# res <- bs_corncob(count=d[[i]][[j]]$counts[1:300,],meta=d[[i]][[j]]$meta)

bs_corncob <- function(counts,meta,design=as.formula("~condition"),fileName=NULL,maxRunTimeInMin=NULL){
  
  if(inherits(counts,"matrix"))
    counts <- as.data.frame(counts)
  
  if(!all(!is.na(counts)))
    warning("bs_corncob: NAs in counts.")
  if(sum(counts<0,na.rm=T)>0)
    warning("bs_corncob: Counts have negative values.")
  
  bs_corncob_core <- function(counts, meta, design) {
    library(corncob)
    print(class(design))  # Check the class of 'design'
    
    res <- corncob::differentialTest(formula = design,
                                     phi.formula = design,
                                     phi.formula_null = design,
                                     formula_null = ~ 1,
                                     test = "Wald", data = as.data.frame(counts),
                                     sample_data = meta,
                                     boot = FALSE,
                                     fdr_cutoff = Inf
    )
    # No log2FC available for cornob, one could maybe use effect size estimates instead, see res$result$significant_models[[2]]$coefficients
    # see https://github.com/bryandmartin/corncob/issues/64
    out <- list(pvalue=res$p, logFoldChange=NA*res$p, padj=res$padj, result=res  )
    # out <- list(pvalue=runif(nrow(counts)), logFoldChange=NA*runif(nrow(counts)), padj=runif(nrow(counts)), result="test"  )
    return(out)
  }
  
  if(is.null(maxRunTimeInMin)){
    out <- bs_corncob_core(counts,meta,design)
    return(out)
  }else{ # else-case, i.e. when maxRunTimeInMin was set

    args_list <- list(
      counts = counts,
      meta = meta,
      design = design
    )
    
    # Run the function with a timeout
    try(out <- callr::r_safe(
      func = bs_corncob_core,
      args = args_list,
      timeout = maxRunTimeInMin*60,  # Timeout in seconds
    ))
    
    # Check results
    if (is.null(out)) {
      cat("run_corncob_analysis was terminated due to timeout.\n")
      return(list(pvalue=NA*nrow(counts), logFoldChange=NA*nrow(counts), padj=NA*nrow(counts), result=NULL  ))
    } else {
      return(out)
    }
    
  } # end of else-case, i.e. when maxRunTimeInMin was set
}

