#' First step in the split & merge procedure:
#'   Create data sets by splitting
#'
#' @param d data object e.g. data_to_compare
#'  either data_list or data_project
#' @param k number of splits
#' 
#' Example:
#' dsplit <- bs_SM_split(d)

bs_SM_split <- function(d,k=2){
  
  if(bs_isa(d,"data_project")){# handle this case
    dlist <- list(element=d)
    attr(dlist, "bstype") <- "data_list"
    dlist <- bs_annotateObject(dlist)
    
    dlist <- bs_DA(dlist, design=design, groupVariable=groupVariable, doSlowMethods=doSlowMethods, maxFeatures=maxFeatures, keepTestResult=keepTestResult,
                   whichMethods=whichMethods,
                   whichData=whichData,
                   overwriteOldDA=overwriteOldDA, parallelMode = parallelMode)
    return(dlist[[1]])
  } # data_project
  else
  { # data_list
    
    if(!bs_isa(d,"data_list"))
      stop("bs_createDataLabels is up to now only implemented for a data_list (whole data_to_compare object)")
    
    if(sum(names(d[[1]][[1]])=="project_label")<1)
      stop("bs_DA: call bs_annotateObject() first because project_label etc required.")
    
    
    
    namen <- names(d)
    dsplit <- list()
    # 
    for(i in 1:length(d)){
      namen2 <- names(d[[i]])
      dsplit[[i]] <- list()
      newNames <- array()
      
      if(bs_isa(d[[i]],c("data_project"))){
        for(j in 1:length(d[[i]])){
          j_split <- length(dsplit[[i]])+1 # target index
          if(bs_isa(d[[i]][[j]],c("data_template","sim_result"))){
            counts <- d[[i]][[j]]$counts
            indices <- bs_SM_drawIndices(nrow(counts),k=k)
            
            for(ik in 1:length(indices)){
              dsplit[[i]][[j_split]] <- d[[i]][[j]]
              dsplit[[i]][[j_split]]$counts <- counts[indices[[ik]],]
              dsplit[[i]][[j_split]]$SM.row.indices <- indices[[ik]]
              
              dsplit[[i]][[j_split]]$DA <- NULL
              newNames[j_split] <- paste0(namen2[j],".split",ik)
              attr(dsplit[[i]][[j_split]],"bstype") <- attr(d[[i]][[j]],"bstype")
              j_split <- length(dsplit[[i]])+1 # update target index
            }
            
          }else{ # not data set
            newNames[j_split] <- namen2[j]
            dsplit[[i]][[j_split]] <- d[[i]][[j]]
          }
        }
      }else{ # no data_project
        dsplit[[i]] <- d[[i]]
      }
      attr(dsplit[[i]],"bstype") <- attr(d[[i]],"bstype")
      names(dsplit[[i]]) <- newNames
    }
    
  }
  names(dsplit) <- namen
  attr(dsplit,"bstype") <- attr(d,"bstype")
  return(dsplit)
}

bs_SM_drawIndices <- function(nfeature,k,seed=123){
  if(k>nfeature){
    warning("k>nfeature: set k=nfeature")
    k = nfeature
  }
  
  # Step 1: Save the current state of the RNG
  original_rng_state <- get(".Random.seed", envir = .GlobalEnv)
  
  # Step 2: Set the RNG seed
  set.seed(seed)
  
  # Step 3: Generate random numbers
  random_numbers <- runif(nfeature)  # Generate 5 uniformly distributed random numbers
  rf <- order(random_numbers)
  
  # Step 4: Restore the original RNG state
  assign(".Random.seed", original_rng_state, envir = .GlobalEnv)
  
  # Split indices:
  indices <- list()
  
  blockLength <- nfeature/k # as proportion
  for(i in 1:k){
    block <- floor((i-1)*blockLength+1) : floor(i*blockLength) 
    indices[[i]] <- rf[block]
  }
  
  return(indices)
}
