# Check whether metaSPARSim params are okay
#
# @return: anzNA in intensity, variability, libsize (see below)
#
## Example:
#
# cmp <- bs_checkParams(data_to_compare)
#
# bs_checkParams(data_to_compare[[1]]$metaSPARSim_1$params)
#
# $anzNA_intensity
# [,1] [,2]
# [1,]    0    0
# 
# $anzNA_libsize
# [,1] [,2]
# [1,]    0    0
# 
# $anzNA_variability
# [,1]      [,2]
# [1,] 0.6695652 0.5878261
# 
bs_checkParams <- function(d_or_params){
  
  if(bs_isa(d_or_params,"data_list")){
    d <- d_or_params
    namen1 <- names(d)
    params <- list()
    namen <- NULL
    for(i in 1:length(d)){
      namen2 <- names(d[[i]])
      
      for(j in 1:length(d[[i]])){
        if("params" %in% names(d[[i]][[j]])){
          params <- append(params,list(d[[i]][[j]]$params))
          namen <- c(namen,paste0(namen1[i],"_",namen2[j]))
        }
      }
    }
    names(params) <- namen
  }
  else{
    params <- d_or_params
  }
  
  if(length(params)<1)
    stop("length(params)<1 in bs_checkParams.")
  
  if("intensity" %in% names(params[[1]])) # no list of params => make one
    params <- list(condition=params) 
  
  anzNAi <- matrix(NA,nrow=length(params),ncol=length(params[[1]]))
  anzNAv <- matrix(NA,nrow=length(params),ncol=length(params[[1]]))
  anzNAl <- matrix(NA,nrow=length(params),ncol=length(params[[1]]))
  same <- list(intensityNum=array(dim=length(params)),   intensityProp = array(dim=length(params)), 
               variabilityNum=array(dim=length(params)), variabilityProp = array(dim=length(params)))
  
  for(i in 1:length(params)){
    if(!is.null(params[[i]])){
      for(j in 1:length(params[[i]])){ # Loop over conditions
        anzNAi[i,j] <- sum(is.na(params[[i]][[j]]$intensity))/length(params[[i]][[j]]$intensity)
        anzNAv[i,j] <- sum(is.na(params[[i]][[j]]$variability))/length(params[[i]][[j]]$variability)
        anzNAl[i,j] <- sum(is.na(params[[i]][[j]]$lib_size))/length(params[[i]][[j]]$lib_size)
      }
      if(length(params[[i]])==2){ # Genau 2 conditions => Checke wie oft params gleich
        cmp <- params[[i]][[1]]$intensity==params[[i]][[2]]$intensity
        same$intensityNum[i] <- sum(cmp,na.rm=T)  
        same$intensityProp[i] <- sum(cmp,na.rm=T) / sum(!is.na(cmp)) 
        
        cmp <- params[[i]][[1]]$variability==params[[i]][[2]]$variability
        same$variabilityNum[i] <- sum(cmp,na.rm=T)  
        same$variabilityProp[i] <- sum(cmp,na.rm=T) / sum(!is.na(cmp)) 
      }
    }
  }
  
  if(sum(is.na(anzNAi))>0 || sum(is.na(anzNAl))>0 || sum(is.na(anzNAv))>0){
    warning("Parameters for some data sets could not be estimated")
  }
  if(sum(is.na(anzNAi))==0 && sum(is.na(anzNAl))==0 && sum(is.na(anzNAv))==0){
    if(sum(anzNAi,na.rm=T)>0 || sum(anzNAl,na.rm=T)>0 || sum(anzNAv,na.rm=T)>0)
      warning("Some parameters are NA")
    if(sum(anzNAi)==0 && sum(anzNAl)==0 && sum(anzNAv)==0)
      print("All parameters could be estimated")
  }
  
  return(list(anzNA_intensity = anzNAi, anzNA_libsize = anzNAl, anzNA_variability = anzNAv, same=same))
} 
