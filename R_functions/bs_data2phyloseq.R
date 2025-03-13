#' bs_data2phyloseq
#'
#' converts a data set to a phyloseq object 
#'
#' @param counts either count matrix or data_set with counts and meta as list elements

#' @param meta
#'
#' @return
#' 
#' @examples
#' phylo <- bs_data2phyloseq(d[[1]][[1]]$counts,d[[1]][[1]]$meta)
#' phylo <- bs_data2phyloseq(d[[1]][[1]])
#'
#' @seealso \code{\link{anotherFunction}}
#' @keywords aKeywordSuchAsBenchmarking
#' @export 

bs_data2phyloseq <- function(counts,meta,buildTree=TRUE)
{
  # counts are data_set with counts and meta as list elements
  if(any(msb.attrCompare(counts,"bstype",c("data_template","sim_result")))){
    meta <- counts$meta
    counts <- counts$counts
  }
  
  OTU <- phyloseq::otu_table(counts,taxa_are_rows=TRUE)
  if(!is.null(row.names(counts))){
    tax_tab <- matrix(row.names(counts),ncol=1)
    row.names(tax_tab) <- row.names(counts)
  }
  else
    tax_tab <- matrix(paste("dummy",seq(1:nrow(counts))),ncol=1)
                    
  TAX <- phyloseq::tax_table(tax_tab,nrow(counts)) # dummy tax_table
  row.names(TAX) <-row.names(OTU)
  colnames(TAX) <- "Species"
  sam_data <- phyloseq::sample_data(meta)
  
  if(buildTree){
    physeq <- phyloseq(OTU,TAX)
    # Built tree
    r.tree <- rtree(ntaxa(physeq),rooted=TRUE,tip.label=taxa_names(physeq))
    
    # Add meta data and tree to phyloseq object
    physeq <- phyloseq::merge_phyloseq(physeq,sam_data,r.tree)
  }
  else
    physeq <- phyloseq::phyloseq(OTU,TAX,sam_data=sam_data)
  
  return(physeq)
}
