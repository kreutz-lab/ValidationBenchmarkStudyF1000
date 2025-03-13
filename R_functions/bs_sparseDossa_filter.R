#' bs_sparseDossa_filter
#'
#' This function can be applied to select a subset of features and/or samples 
#' of the fitted parameters of SparseDOSSA2.
#'
#' All list elements which have the length of the number of features or samples will be subsetted.
#' Note that this might not work perfectly.
#'
#' @param indF indeces of features to be kept [default: all]
#' @param indS indices of samples to be kept [default: all]
#' @param NFold number of features before filtering (argument mainly to enable recursive calling)
#' @param NSold number of samples before filtering (argument mainly to enable recursive calling)
#'
#' @return filtered recursive list
#' 
#' @examples
#' # load parameters:
#' params <- readRDS(paste0(bs_path,"/Analyses/R_functions/paramsSparseDossa2.RDS"))
#' para <- params[1]
#' msb.str(para)
#' sim <- SparseDOSSA2::SparseDOSSA2(template=para)
#' # now select subset:
#' para2 <- bs_sparseDossa_filter(para,indF=1:100,indS=1:10)
#' msb.str(para2)
#' sim2 <- SparseDOSSA2::SparseDOSSA2(template=para2)
#' 
#'
#' @seealso \code{\link{anotherFunction}}
#' @keywords aKeywordSuchAsBenchmarking
#' @export

bs_sparseDossa_filter <- function(para, indF=NULL, indS=NULL, 
                                  NFold=length(para$EM_fit$fit$pi0),
                                  NSold=length(which(para$l_filtering$ind_sample)))
{
  
  if(is.null(indF))
    indF <- 1:NFold
  if(is.null(indS))
    indS <- 1:NSold
  
  lev1 <- names(para)
  for(i1 in 1:length(lev1)){
    if(length(para[[lev1[i1]]])==NSold) # filter samples
      para[[lev1[i1]]] <- para[[lev1[i1]]][indS]
    if(length(para[[lev1[i1]]])==NFold) # filter features
      para[[lev1[i1]]] <- para[[lev1[i1]]][indF]
    
    if(length(dim(para[[lev1[i1]]]))==2){ # matrix
      if(dim(para[[lev1[i1]]])[1]==NSold  && dim(para[[lev1[i1]]])[2]==NSold) # sample x sample: filter samples
        para[[lev1[i1]]] <- para[[lev1[i1]]][indS,indS]
      
      if(dim(para[[lev1[i1]]])[1]==NFold  && dim(para[[lev1[i1]]])[2]==NFold) # feature x feature: filter features
        para[[lev1[i1]]] <- para[[lev1[i1]]][indF,indF]
    }
      
    
    if(is.list(para[[lev1[i1]]])){
      para[[lev1[i1]]] <- bs_sparseDossa_filter(para[[lev1[i1]]],indF = indF, indS = indS, NFold = NFold, NSold = NSold)
    }
  }
  return(para)
}
