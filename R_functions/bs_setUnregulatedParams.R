#' bs_setUnregulatedParams
#'
#' This function propagates information about un-regulated features to the bs_object
#'
#' Currently, only metaSPARSim is implemented. Here, intensity- and variability parameters are set equal in groups.
#'
#' @param params the simulation parameters estimated from the data 
#'    e.g. via metaSPARSim::estimate_parameter_from_data
#' @param simulator e.g. "metasparsim" or "sparsedossa2"
#'    The same values which are available in bs_simulateData(...,simulator=...)
#'
#' @return
#' 
#' @examples
#' pvals <- bs_edgeR(D$original$counts,D$original$meta)$pvalue
#' isReg <- msb.sampleDiffRegFromPvalues(pvals)
#' params2 <- bs_setUnregulatedParams(D$metaSPARSim_0$params,simulator="metasparsim",!isReg)
#' sum(params2[[1]]$intensity==params2[[2]]$intensity)
#' 
#'
#' @seealso \code{\link{anotherFunction}}
#' @keywords aKeywordSuchAsBenchmarking
#' @export

bs_setUnregulatedParams <- function(params, paramsAll=NULL, simulator, notRegulated){
  
  if(simulator!="metasparsim" &&  simulator!="metasparsim_partreg" && simulator!="metasparsim_nonreg")
    warning("bs_setUnregulatedParams: Up to now only metasparsim implemented! Add sparseDossa and others if required.")
  
  if(!is.logical(notRegulated))
    stop("bs_setUnregulatedParams: !is.logical(notRegulated")
  
  conds <- names(params)
  if(sum(notRegulated)<1){
    for(k in 1:length(conds)){
      params[[conds[k]]]$info <- "No unregulated features."
    }
    cat("No unregulated features.\n")
    return(params)
  }
  # if(simulator=="metasparsim" || simulator=="metasparsim_partreg"){
  if(simulator=="metasparsim_partreg" || simulator=="metasparsim_nonreg"){
    if(!is.null(paramsAll) && simulator=="metasparsim_partreg"){
      for(k in 1:length(conds)){
        params[[conds[k]]]$info <- paste0(sum(notRegulated)," features get parameters from paramsAll")
        params[[conds[k]]]$intensity[notRegulated] <- paramsAll[[1]]$intensity[notRegulated]
        params[[conds[k]]]$variability[notRegulated] <- paramsAll[[1]]$variability[notRegulated]
      }
    }else{
      randomRef <- round(1+runif(1))
      for(k in setdiff(1:length(conds),randomRef)){
        params[[conds[k]]]$randomRef <- randomRef
        params[[conds[k]]]$info <- paste0(sum(notRegulated)," features get parameters from ",randomRef,"th condition ",conds[1])
        params[[conds[k]]]$intensity[notRegulated] <- params[[conds[randomRef]]]$intensity[notRegulated]
        params[[conds[k]]]$variability[notRegulated] <- params[[conds[randomRef]]]$variability[notRegulated]
        params[[conds[k]]]$ref <- conds[randomRef]
      }
    }
  }else if(simulator=="metasparsim_partreg" || simulator=="metasparsim_nonreg"){
    if(!is.null(paramsAll) && simulator=="metasparsim_partreg"){
      for(k in 1:length(conds)){
        params[[conds[k]]]$info <- paste0(sum(notRegulated)," features get parameters from paramsAll")
        params[[conds[k]]]$intensity[notRegulated] <- paramsAll[[1]]$intensity[notRegulated]
        params[[conds[k]]]$variability[notRegulated] <- paramsAll[[1]]$variability[notRegulated]
      }
    }else{
      randomRef <- round(1+runif(1))
      for(k in setdiff(1:length(conds),randomRef)){
        params[[conds[k]]]$randomRef <- randomRef
        params[[conds[k]]]$info <- paste0(sum(notRegulated)," features get parameters from ",randomRef,"th condition ",conds[1])
        params[[conds[k]]]$intensity[notRegulated] <- params[[conds[randomRef]]]$intensity[notRegulated]
        params[[conds[k]]]$variability[notRegulated] <- params[[conds[randomRef]]]$variability[notRegulated]
        params[[conds[k]]]$ref <- conds[randomRef]
      } 
    }
  }else if(simulator=="midasim_partreg" || simulator=="midasim_nonreg"){
    if(!is.null(paramsAll) && simulator=="midasim_partreg"){
      for(k in 1:length(conds)){
        params[[conds[k]]]$info <- paste0(sum(notRegulated)," features get parameters from paramsAll")
        params[[conds[k]]]$mean.rel.abund[notRegulated] <- paramsAll[[1]]$mean.rel.abund[notRegulated]
        params[[conds[k]]]$taxa.1.prop[notRegulated] <- paramsAll[[1]]$taxa.1.prop[notRegulated]*paramsAll[[1]]$n.sample/params[[conds[k]]]$n.sample
        params[[conds[k]]]$notRegulated <- notRegulated
        #fnames <- colnames(params[[conds[k]]]$mat01)
        #for(i in 1:length(params[[conds[k]]]$rel.abund.1)){
        #  drin <- intersect(names(paramsAll[[1]]$rel.abund.1[i]),fnames)
        #  params[[conds[k]]]$rel.abund.1[i] <- paramsAll[[1]]$rel.abund.1[i][]
        #}
      }
    }else{
      stop("Random reference (for no regulation) is not yet implemented.")
      # randomRef <- round(1+runif(1))
      # for(k in setdiff(1:length(conds),randomRef)){
      #   params[[conds[k]]]$randomRef <- randomRef
      #   params[[conds[k]]]$info <- paste0(sum(notRegulated)," features get parameters from ",randomRef,"th condition ",conds[1])
      #   params[[conds[k]]]$intensity[notRegulated] <- params[[conds[randomRef]]]$intensity[notRegulated]
      #   params[[conds[k]]]$variability[notRegulated] <- params[[conds[randomRef]]]$variability[notRegulated]
      #   params[[conds[k]]]$ref <- conds[randomRef]
    }
  }else{
    for(k in 1:length(conds))
      params[[conds[k]]]$info <- paste0("No feature is set as unregulated (simulator=",simulator,")")
  }
  if(simulator!="metasparsim" && simulator!="metasparsim_partreg")
    warning("bs_setUnregulatedParams: Only metasparsim implemented so far.")
  
  return(params)
}
