#' bs_addTruth
#'
#' Eliminates features which are not prevalent in xx % of samples
#' 
#' The following elements are filtered:
#' counts 
#' metaSparsim$params$intensity
#' metaSparsim$params$variability
#'
#' This function implements the prealence filter as in Nearing 2022:
#' "We chose to either use
#' no prevalence filtering (Fig. 1a) or a 10% prevalence filter that
#' removed any ASVs found in fewer than 10% of samples within each dataset (Fig. 1b)."
#'
#' @param prevalence Either 
#'  1) a proportion >0 (if proportion<1) or 
#'  2) the nunber of samples >0 required to be kept in the data
#'
#' @return data_to_compare object
#' 
#' @examples
#' 
#' d2 <- bs_addTruth(d)
#'
#' @seealso \code{\link{anotherFunction}}
#' @keywords aKeywordSuchAsBenchmarking
#' @export

bs_addTruth <- function(d, prevalence=1){
  if(!msb.attrCompare(d,"bstype","data_list"))
    stop("bs_checkData is up to now only implemented for a data_list (whole data_to_compare object, do subsetting via msb.subset)")
  
  # if(prevalence>1)
  #   stop("Please specify prevalence filter as proportion <1.")
  
  namen <- names(d)
  for(i in 1:length(d)){ # loop over all data_projects
    if(msb.attrCompare(d[[i]],"bstype","data_project")){
      namen2 <- names(d[[i]])
      for(j in 1:length(d[[i]])){ # loop over all data sets
        if(any(msb.attrCompare(d[[i]][[j]],"bstype",c("data_template","sim_result")))){
          
        }
        if(any(msb.attrCompare(d[[i]][[j]],"bstype",c("data_template","sim_result")))){
          # metaSparSimNonReg:
          if(length(grep("metaSPARSimNonReg_",namen2[j]))>0 || length(grep("metaSparSimNonReg_",namen2[j]))>0){
            conds <- names(d[[i]][[j]]$params)
            Means <- d[[i]][[j]]$params[[conds[1]]]$intensity
            for(k in 2:length(conds)){
              Means <- cbind(Means,d[[i]][[j]]$params[[conds[k]]]$intensity)
            }
            d[[i]][[j]]$truth <- rowSums(abs(rowMeans(Means,na.rm=T)-Means))>1e-6
          }
          else if("truth" %in% names(d[[i]][[j]]))
            print(paste0("Truth already available for ",namen2[j]))
          else{
            warning("Adding the truth is currently only implemented for metaSPARSimNonReg and not for ",namen2[j])
          }
            
        }
      }
    }
  }


  return(d)
}


