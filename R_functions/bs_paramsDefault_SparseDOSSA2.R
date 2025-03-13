#' bs_paramsDefault_SparseDOSSA2
#'
#' This functions loads simulation parameters from an workspace and can be used as substitution of
#' estimation from data template. It is faster and thus used for runoption="fast" 
#'
#' It is first checked, whether the checksum (calculated from the count matrix) match.
#'
#' If not, it is checked whether params with the same number of features, samples and with the same conditions
#' exists. Otherwise it is "sampled" from params with most features. Here "sampled" means random sampling
#' from features and cyclic replication from samples (e.g. 1,2,3,4,5,1,2,3) if 8 samples are required and 
#' only 5 are available (see rep(...length.out))
#'
#' @param D a data project (i.e. bs_type "data_project")
#' @param paramsWorkspace the RDS workspace containing params or list of fitted parameters
#' @param allowSampling Should parameters be sampled from the largest dataset, if not matching params are found?
#'        Default: FALSE
#'
#' @return
#' 
#' @examples
#' 
#' d <- readRDS(paste0(bs_path,"Analyses/Results/data_to_compare.RDS"))
#' tmp <- bs_paramsDefault_SparseDOSSA2(d[[1]]) 
#' msb.str(tmp) 
#'
#' tmp <- bs_paramsDefault_SparseDOSSA2(d[[2]])
#' msb.str(tmp)
#' 
#' # Loading workspace once and loop (faster):
#' params <- readRDS("../Results/1.2_sparseDOSSA/params.RDS"))
#' simparams <- list
#' for(i in 1:length(d))
#'   simparams[[i]] <- bs_paramsDefault_SparseDOSSA2(d[[i]],params)
#'
#' # Create workspace params:
#' inFolder <- "../Results/1.2_sparseDOSSA/"
#' outFile <- paste0("../Results/1.2_sparseDOSSA/params.RDS")
#' bs_sparseDossa_makeParamsRDS(inFolder=inFolder,outFile=outFile)
#' 
#' 
#' @seealso \code{\link{anotherFunction}}
#' @keywords aKeywordSuchAsBenchmarking
#' @export

bs_paramsDefault_SparseDOSSA2 <- function(D,paramsWorkspace="../Results/1.2_metaSPARSim/params.RDS",allowSampling=F){
  if(is.null(paramsWorkspace))
    paramsWorkspace <- paste0(bs_path,"../R_functions/paramsSparseDossa2.RDS")
  
  params <- NULL
  if(is.list(paramsWorkspace)){ # it is already a list for fitted workspaces
    paramsWorkspace <- paramsWorkspace[sapply(paramsWorkspace,is.list)] # remove all non-list elements (occur when estimate_params failed in params.RDS)
    if("EM_fit" %in% names(paramsWorkspace[[1]]))
      params <- paramsWorkspace 
  }
  if(is.null(params)){
    if(is.character(paramsWorkspace)){
      print("Try to load simulation params from workspace...")
      params <- readRDS(paramsWorkspace)
      print("Reading workspace finished.")
    }else{
      save(paramsWorkspace,file="bs_paramsDefault_SparseDOSSA2_error.RDS")
      stop("paramsWorkspace is neither a string nor an appropriate list, check bs_paramsDefault_SparseDOSSA2_error.RDS")
    }
  }
  params <- params[sapply(params,is.list)] # remove all non-list elements (occur when estimate_params failed in params.RDS)
  checkSums <- sapply(params,function(x)x$checksum)
  
  countsInGroups <- bs_countsInGroups(D$original$counts,D$original$meta$condition)
  param <- list() # for the found params
  for(g in 1:length(countsInGroups)){
    checkSumData <- digest::digest(countsInGroups[[g]])
    cat("checksum data=",checkSumData,"\n")
    passt <- which(checkSums==checkSumData)
    if(length(passt)>0){
      cat("Perfect! Matching params for data checksum found for condition ",g,"\n")
      param[[g]] <- params[[passt[1]]]
    }else{ 
      cat("No matching checksum found for condition ",g,"\n")
      nfeature <- lapply(params,function(x){length(x$l_filtering$ind_feature)})
      nsamples <- lapply(params,function(x){length(x$l_filtering$ind_sample)})
      
      sameNs <- which(nrow(countsInGroups[[g]])==nfeature & ncol(countsInGroups[[g]])==nsamples)
      p <- NULL
      if(length(sameNs)>0){ # proper params found
        print(paste0("Matching params for nrow=",nrow(countsInGroups[[g]]),", ncol=",ncol(countsInGroups[[g]])," found."))
        p <- params[[sameNs[1]]]
        ## The following does not work, names(counts) sind feature-names, diese ind aber in sparseDossa Result nicht verfügbar.
        # if(length(setdiff(unique(names(countsInGroups[[g]])),names(p)))>0) # conditions do not fit
        #   p <- NULL
        # else
        #   print(paste0("Matching params ",nrow(countsInGroups[[g]]),"x",ncol(countsInGroups[[g]])," with conditions ",paste(unique(names(countsInGroups[[g]])),sep=",",collapse=",")," found."))
      }
      
      if(is.null(p)){ # no matching params: sample features from largest, replicate samples
        print("No exactly matching params found...")
        if(allowSampling){
          cat("Sampling of params from the largest dataset...\n")
          rf <- order(unlist(nfeature),decreasing=T)
          iuse <- rf[min(5,length(rf))] # use the fith largest (not the largest because of potential memory problems)
          maxNf <- length(params[[iuse]]$EM_fit$fit$pi0)
          maxNs <- nsamples[[iuse]]
          
          if(nrow(countsInGroups[[g]])>10000 && allowInterrupts()){
            cat(paste0("bs_paramsDefault_SparseDOSSA2(): sampling params with more than 10000 feature might cause memory problems\n",
                       "  because nfeature x nfeature matrix is created.\n",
                       "  Alternatively, try to estimate params for that data on a large machine.\n"))
            anzGB = 64 # size of RAM allocation (be careful on small machines)
            options(future.globals.maxSize = anzGB*1024 * 1024^2) # allow more RAM usage
          }
          if(maxNf>nrow(countsInGroups[[g]]))
            indF <- sample(maxNf,nrow(countsInGroups[[g]])) 
          else
            indF <- sample(maxNf,nrow(countsInGroups[[g]]),replace=TRUE)
          
          if(maxNs>ncol(countsInGroups[[g]]))
            indS <- sample(maxNs,ncol(countsInGroups[[g]]))
          else
            indS <- sample(maxNs,ncol(countsInGroups[[g]]),replace=TRUE)
          
          tryCatch(
            p <- bs_sparseDossa_filter(params[[iuse]],indF=indF,indS=indS),
            error=function(e){
              msg = paste0("! bs_paramsDefault_SparseDOSSA2.R: Could not sample parameters, maybe memory problem. nrow(countsInGroups[[g]])=",nrow(countsInGroups[[g]]))
              print(msg)
              warning(msg)
              return(NULL)
            })
        }else{
          cat("\nNo matching params found.\n")
          cat("Solution1: Estimate params first and add it to the paramsWorkspace, e.g via script 1.2 or bs_sparseDossa_makeParamsRDS.\n")
          cat("Solution2: Set argument allowSampling=TRUE for sampling params from the larget other dataset.\n")
          stop(paste0("bs_paramsDefault_SparseDOSSA2: No matching params found in and allowSampling=FALSE."))
        }
      }else{
        param[[g]] <- p
      }
    } # else checksum does not match
  }# all groups (i.e. conditions)
  
  names(param) <- names(countsInGroups)
  return(param)
}


