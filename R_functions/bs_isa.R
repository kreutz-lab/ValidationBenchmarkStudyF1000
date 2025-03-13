#' bs_isa
#'
#' Function for testing whether an object is a specific bs_type
#'
#' It basically only checks whether the bs_type is defined and whether it fits.
#' This function is only to make the code slightly shorter and to handle all cases at one point.
#'
#' @param object to be tested
#' @param bstypeNames the names of the bstype, all available ones are in bs_types.txt
#'
#' @return logical indicating whether the object has the correct bs_type
#' 
#' @examples
#  d <- readRDS("data_to_compare.RDS")
#' bs_isa(d[[1]],"data_project")
#' sapply(d[[1]],function(x){bs_isa(x,c("data_template","sim_result"))})
#' 
#' bs_isa(d) # only return the bstype
#'
#' @seealso \code{\link{msb.attrCompare}}
#' @keywords attributes
#' @export

bs_isa <- function(object,bstypeNames=NULL){
  if(is.null(bstypeNames))
    return(attr(object,"bstype"))
  
  bstype <- attr(object,"bstype")
  if(!is.null(bstype))   
    return(any(bstype==bstypeNames))
  else
    return(FALSE)
}
