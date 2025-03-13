# This function calls SparseDOSSA2, but also works for parameters fitted individually
# on different conditions.
#
#   template  is the params object

bs_SparseDOSSA2 <- function(template, n_sample=NULL, new_features=F, n_feature=NULL, 
                            spike_metadata = "none", 
                            metadata_effect_size = 1, perc_feature_spiked_metadata = 0.05, 
                            metadata_matrix = NULL,
                            median_read_depth = 50000,verbose = TRUE){
  
  if(!is.character(template))
    params <- template
  else
    stop("bs_SparseDOSSA2: is.character(template) not yet implemented.")
  
  if(!new_features)
    n_feature <- length(params[[1]]$l_filtering$ind_feature)

  if(is.list(params) && "EM_fit" %in% names(params[[1]])){# several conditions
    sim_data <- list()
    conds <- names(params)
    
    if(is.null(n_sample))
      nsTotal <- sum(sapply(params,function(x){length(x$l_filtering$ind_sample)}))
    else{
      if(length(n_sample)==length(params))
        nsTotal <- sum(n_sample)
      else
        nsTotal <- n_sample*length(params) # n_sample for each condition
    }
      
    simcounts <- matrix(data=0,nrow=n_feature,ncol=nsTotal)
    ns <- 0
    
    col_namen <- array()
    row_namen <- paste("Feature ",1:n_feature,sep="_")
    for(ip in 1:length(params)){
      if(is.null(n_sample))
         n_sampleTmp <- length(params[[ip]]$EM_fit$l_filtering$ind_sample)
      else
         n_sampleTmp <- n_sample
      
      simresult <- SparseDOSSA2(template=params[[ip]],new_features=F, n_sample=n_sampleTmp, n_feature=n_feature)
      nsNew <- ncol(simresult$simulated_data)
      col_namen[ns+(1:nsNew)] <- paste(conds[ip],1:nsNew,sep="_")
      row_namen[params[[ip]]$l_filtering$ind_feature] <- names(params[[ip]]$EM_fit$fit$mu)
      simcounts[params[[ip]]$l_filtering$ind_feature,ns+(1:nsNew)] <- simresult$simulated_data
      ns <- ns + nsNew
    }
    simcounts <- simcounts[,1:ns] # only those that are filled
    colnames(simcounts) <- col_namen
    rownames(simcounts) <- row_namen
    simcounts <- simcounts[,colSums(simcounts,na.rm=T)>0]
    sim_data <- list(counts=simcounts, params=params)
  
  } else { # only one condition
    if(is.null(n_sample))
      n_sample <- length(params$l_filtering$ind_sample)
    # simresult <- SparseDOSSA2::SparseDOSSA2(template=params) # does only work with the SparseDOSSA2 package, not with bs_sparseDossaInit()
    simresult <- SparseDOSSA2(template=params, new_features=new_features, n_sample=n_sample, n_feature=n_feature)
    simcounts <- simresult$simulated_data
    row_namen <- paste("Feature ",1:n_feature,sep="_")
    row_namen[params$l_filtering$ind_feature] <- names(params$EM_fit$fit$mu)
    
    colnames(simcounts) <- paste("Sample",1:ncol(simcounts),sep="_")
    sim_data <- list(counts=simcounts, params=params)
  }
  
  return(sim_data)
}