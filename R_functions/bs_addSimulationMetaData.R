#' bs_addSimulationMetaData
#'
#' creates a meta data data.frame from colnames if $meta does not exist 
#'
#' details
#'
#' @param 
#' @param 
#'
#' @return
#' 
#' @examples
#' 
#'
#' 
#'
#' @seealso \code{\link{anotherFunction}}
#' @keywords aKeywordSuchAsBenchmarking
#' @export

bs_addSimulationMetaData <- function(bsobj){
  
  if(!msb.attrCompare(bsobj,"bstype","data_list"))
    stop("bs_addSimulationMetaData only implemented for bs_type=data_list.")
  
  for(i in 1:length(bsobj)){
    for(j in 1:length(bsobj[[i]])){
      if(msb.attrCompare(bsobj[[i]][[j]],"bstype","sim_result")){
        if(is.null(bsobj[[i]][[j]]$meta) || nrow(bsobj[[i]][[j]]$meta)==0 ){
          # Colnames are saved as an attribute in the bsobj 
          #cn <- attr(bsobj[[i]][[j]]$counts, "colnames")
          cn <- colnames(bsobj[[i]][[j]]$counts)
          meta <- data.frame(condition=as.factor(gsub("_R[0-9]+$","",cn)))
          row.names(meta) <- cn
          bsobj[[i]][[j]]$meta <- meta
        }
      }
      
    }
  }
  return(bsobj)
}
