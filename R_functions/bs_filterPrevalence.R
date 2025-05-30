#' bs_filterPrevalence
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
#' d2 <- bs_filterPrevalence(d)
#'
#' @seealso \code{\link{anotherFunction}}
#' @keywords aKeywordSuchAsBenchmarking
#' @export

bs_filterPrevalence <- function(d, prevalence=1){
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
          counts <- d[[i]][[j]]$counts
          
          anz <- rowSums(counts>0)
          if(prevalence<1){
            prop <- anz/ncol(counts)
            drin <- prop>=prevalence
            cat("Prevalence filter (",prevalence*100,"%) for ",namen[i],"$",namen2[j],": ",sum(drin)," passed, ",sum(!drin)," (=",sum(!drin)/length(drin)*100,"%) filtered out.\n")
          }
          else{
            drin <- anz>=prevalence
            cat("Prevalence filter (N=",prevalence,") for ",namen[i],"$",namen2[j],": ",sum(drin)," passed, ",sum(!drin)," (=",sum(!drin)/length(drin)*100,"%) filtered out.\n")
          }
          if(sum(drin)<1)
            stop("No feature satisfies prevalence filter for: $",namen[i], "$", namen2[j])
          counts <- counts[drin,]
          d[[i]][[j]]$counts <- counts
          
          # Filter normalized counts in the same way:
          if(sum(names(d[[i]][[j]])=="counts.norm")>0)
            d[[i]][[j]]$counts.norm <- d[[i]][[j]]$counts.norm[drin,]
          
          
          if(length(grep("metaSPARSim_",namen2[j]))>0){
            conds <- names(d[[i]][[j]]$params)
            for(k in 1:length(conds)){
              d[[i]][[j]]$params[[conds[k]]]$intensity <- d[[i]][[j]]$params[[conds[k]]]$intensity[drin]
              d[[i]][[j]]$params[[conds[k]]]$variability <- d[[i]][[j]]$params[[conds[k]]]$variability[drin]
            }
          }
          
          if(length(grep("sparseDossa_",namen2[j]))>0){
            try(d[[i]][[j]]$params <- NULL) # delete
            # for(k in 1:length(conds)){
            #   # SparseDOSSA filters out features, as indicated in $l_filtering$ind_feature
            #   # Schritt 1: mappen von drin auf die gefilterten indizes
            #   drinMapped <- drin[ d[[i]][[j]]$params[[conds[k]]]$l_filtering$ind_feature ]
            #   
            #   
            #   d[[i]][[j]]$params[[conds[k]]]$EM_fit$fit$pi0 <- d[[i]][[j]]$params[[conds[k]]]$EM_fit$fit$pi0[drinMapped]
            #   d[[i]][[j]]$params[[conds[k]]]$EM_fit$fit$mu <- d[[i]][[j]]$params[[conds[k]]]$EM_fit$fit$mu[drinMapped]
            #   d[[i]][[j]]$params[[conds[k]]]$EM_fit$fit$mu <- d[[i]][[j]]$params[[conds[k]]]$EM_fit$fit$mu[drinMapped]
            #   
            #   d[[i]][[j]]$params[[conds[k]]]$ <- d[[i]][[j]]$params[[conds[k]]]$variability[drin]
            # }
          }
          
        }
      }
    }
  }

  return(d)
}


