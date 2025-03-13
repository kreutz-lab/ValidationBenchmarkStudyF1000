# distinctTest
#
#   Calling distinctTest for doing differential abundance analysis
#
# maxRunTimeInMin maximal Runtime in minutes
#
# Required packages (loaded in bs_init)
#
# Example:
# res <- bs_distinctTest(count=d[[i]][[j]]$counts[1:300,],meta=d[[i]][[j]]$meta)

bs_distinctTest <- function(counts,meta,design=as.formula("~condition"),fileName=NULL,maxRunTimeInMin=NULL,min_non_zero_cells=1){
  
  if(!inherits(counts,"matrix"))
    counts <- as.matrix(counts)
  
  if(!all(!is.na(counts)))
    warning("bs_distinctTest: NAs in counts.")
  if(sum(counts<0,na.rm=T)>0)
    warning("bs_distinctTest: Counts have negative values.")
  
  condition <- meta$condition
  design <- model.matrix(design, data = meta)
  
  bs_distinctTest_core <- function(counts, meta, design,min_non_zero_cells) {
    library(distinct)
    library(SummarizedExperiment)
    
    meta <- data.frame(meta, cluster_id = rep("A",nrow(meta)), sample_id = row.names(meta))
    
    # Create SummarizedExperiment without rowData
    se <- SummarizedExperiment::SummarizedExperiment(
      assays = list(logcounts = log2(counts + 1)),  # Log-normalized counts
      colData = meta
    )

    old_seed <- .Random.seed
    set.seed(61217) # seed from distinct::distinct_test docu
    res <- distinct::distinct_test(x=se, design = design, name_assays_expression="logcounts",min_non_zero_cells = min_non_zero_cells)
    .Random.seed <- old_seed
    
    
    p <- res$p_val
    padj <- res$p_adj.glb
    if(sum(res$filtered)>0){
      p[res$filtered] <- NA # overwrite 1 by NA
      padj[res$filtered] <- NA # overwrite 1 by NA
    }
    
    out <- list(pvalue=p, logFoldChange=NA*p, padj=padj, result=res)
    # out <- list(pvalue=runif(nrow(counts)), logFoldChange=NA*runif(nrow(counts)), padj=runif(nrow(counts)), result="test"  )
    return(out)
  }
  
  if(is.null(maxRunTimeInMin)){
    out <- bs_distinctTest_core(counts,meta,design,min_non_zero_cells)
    return(out)
  }else{ # else-case, i.e. when maxRunTimeInMin was set

    args_list <- list(
      counts = counts,
      meta = meta,
      design = design,
      min_non_zero_cells = min_non_zero_cells
    )
    
    # Run the function with a timeout
    try(out <- callr::r_safe(
      func = bs_distinctTest_core,
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

