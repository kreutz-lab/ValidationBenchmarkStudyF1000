#' bs_paramsDefault_Metasparsim
#'
#' This functions loads simulation parameters from an workspace and can be used as subsitution of
#' estimation from data template. It is faster and thus used for runoption="fast" 
#'
#' It is checked whether params with the same number of features, samples and with the same conditions
#' exists. Otherwise it is "sampled" from params with most features. Here "sampled" means random sampling
#' from features and cyclic replication from samples (e.g. 1,2,3,4,5,1,2,3) if 8 samples are required and 
#' only 5 are available (see rep(...length.out))
#'
#' @param D a data project (i.e. bs_type "data_project")
#' @param paramsWorkspace the RDS workspace containing params 
#' @param oneForAll one set of simulation parameters for all Samples? (no regulation)
#'        Default: F
#'
#' @return
#' 
#' @examples
#' 
#' # Create workspace params
#' d <- readRDS(paste0(bs_path,"Analyses/Results/data_to_compare.RDS"))
#' params <- list()
#' for(i in 1:length(d)){
#'  params[[i]] <- d[[i]]$metaSPARSim_0$params
#'  }
#' saveRDS(params,"params.RDS")
#' 
#' 
#' d <- readRDS(paste0(bs_path,"Analyses/Results/data_to_compare.RDS"))
#' tmp <- bs_paramsDefault_Metasparsim(d[[1]]) 
#' msb.str(tmp)
#'
#' tmp <- bs_paramsDefault_Metasparsim(d[[2]])
#' msb.str(tmp)
#'
#' @seealso \code{\link{anotherFunction}} 
#' @keywords aKeywordSuchAsBenchmarking
#' @export

bs_paramsDefault_Metasparsim <- function(D,paramsWorkspace=paste0("../Results/1.1_metaSPARSim/params.RDS", oneForAll=F)){
  if(!file.exists(paramsWorkspace))
    stop("paramsWorkspace=",paramsWorkspace," does not exist.")
  print("Try to load simulation params from workspace.")
  params <- readRDS(paramsWorkspace)
  params <- params[sapply(params,is.list)] # remove all non-list elements (occur when estimate_params failed in params.RDS)
  
  nfeature <- lapply(params,function(x){length(x[[1]]$intensity)})
  nsamples <- lapply(params,function(x){sum(sapply(x,function(y){length(y$lib_size)}))})
  
  if(sum(names(D)=="original")==0)
    warning("sum(names(D)=='original')==0: $original should be in data_project.")
  
  sameNfs <- which(nrow(D$original$counts)==nfeature & ncol(D$original$counts)==nsamples)
  p <- NULL
  if(length(sameNfs)>0){ # proper params found
    p <- params[[sameNfs[1]]]
    if(oneForAll){
      cat("bs_paramsDefault_Metasparsim: One parameter set for all samples.\n")
    } else if(length(setdiff(unique(D$original$meta$condition),names(p)))>0) # conditions do not fit
      p <- NULL
    else{
      print(paste0("Matching params ",nrow(D$original$counts),"x",ncol(D$original$counts)," with conditions ",paste(unique(D$original$meta$condition),sep=",",collapse=",")," found."))
    }
  }
  
  if(is.null(p)){ # no matching params: sample features from largest, replicate samples
    save(list=c("params","D"),file="bs_paramsDefault_Metasparsim_Error.RData")
    print("No exactly matching params found, sampling from the one with most features applied ...")
    
    # I switched off this option => introduced an error message
    stop("No exactly matching params found, sampling from the one with most features applied ...")
    
    nfeature <- unlist(nfeature)
    maxNf <- which(nfeature==max(nfeature))
    params <- params[[maxNf[1]]]
    conds <- unique(D$original$meta$condition)
    p <- list()
    for(i in 1:length(conds))
    {
      if(maxNf>nrow(D$original$counts))
        ind <- sample(maxNf,nrow(D$original$counts))
      else
        ind <- sample(maxNf,nrow(D$original$counts),replace = TRUE)
      
      p[[conds[i]]] <- list(intensity = params[[1]]$intensity[ind],
                            variability = params[[1]]$variability[ind],
                            lib_size = rep(params[[1]]$lib_size,length.out=ncol(D$original$counts))
                            )
    }
  }
    
  return(p)
}
