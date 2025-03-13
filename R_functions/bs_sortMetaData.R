#' bs_sortMetaData
#'
#' This function sorts $meta to have the same order as colnames($counts) 
#'
#'
#' @param data_list object e.g. data_to_compare
#'
#' @return data_list object
#' 
#' @examples
#' 
#' d <- readRDS(file="../R_scripts/data_to_compare.RDS")
#' data.frame(count=colnames(d[[1]][[1]]$counts),meta=rownames(d[[1]][[1]]$meta))
#' d2 <- bs_sortMetaData(d)
#' data.frame(count=colnames(d2[[1]][[1]]$counts),meta=rownames(d2[[1]][[1]]$meta))
#' 
#'
#' 
#' @seealso \code{\link{anotherFunction}}
#' @keywords aKeywordSuchAsBenchmarking
#' @export

bs_sortMetaData <- function(d){
  if(!msb.attrCompare(d,"bstype","data_list"))
    stop("bs_sortMetaData is up to now only implemented for a data_list (whole data_to_compare object)")

  for(i in 1:length(d)){ # loop over all data_projects
    if(msb.attrCompare(d[[i]],"bstype","data_project")){
      for(j in 1:length(d[[i]])){ # loop over all data sets
        if(msb.attrCompare(d[[i]][[j]],"bstype","data_template")){
          if(is.null(colnames(d[[i]][[j]]$counts)))
             stop("No colnames found: Maybe you have to call bs_decompress first?")
          row_indices <- match(colnames(d[[i]][[j]]$counts), rownames(d[[i]][[j]]$meta))  
          d[[i]][[j]]$meta <- d[[i]][[j]]$meta[row_indices,,drop=F]
          
        }
      }
    }
  }
  return(d)
}
