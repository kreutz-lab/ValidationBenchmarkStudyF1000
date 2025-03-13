# Usually:
#test_names <- names(DF_significance)[test_cols] 
#data_names <- unique(DF_significance$project_dataset) 


bs_calculateOverlapProfiles <- function(DF_significance,data_names,test_names){

  profile_list <- list()
#  profile_listOld <- list()

  for(i in 1:length(data_names)){  # all
    cat(".") # one point per data_name
    data_name <- data_names[i]
    df_current <- DF_significance[DF_significance$project_dataset == data_name,]  # only result for current data_name
    
    # Begin of hypothesis-specific code
    # Calculate overlap "profiles" first:
    profile <- matrix(array(0,dim=length(test_names)*(length(test_names)-1)),nrow=length(test_names)) # here I will count how many others are significant
    #profileOld <- matrix(array(NA,dim=length(test_names)*(length(test_names)-1)),nrow=length(test_names)) # here I will count how many others are significant
    dimnames(profile)[[1]] <- test_names
    dimnames(profile)[[2]] <- as.character(1:dim(profile)[2])
#    dimnames(profileOld)[[1]] <- test_names
#    dimnames(profileOld)[[2]] <- as.character(1:dim(profile)[2])
    
    for(i1 in 1:length(test_names)){
      test1 <- test_names[i1] # profile of this test, i.e. count how often are other tests significant
      otherTests <- setdiff(test_names,test1)
      # For significant test1, how many others (streched to 0:length(otherTests) if NA)
      whichSig <- which(df_current[,test1])
      if(length(whichSig)>0){
        anzOthers <- msb.counts2intDensity(rowSums(df_current[whichSig,otherTests],na.rm=T) * length(otherTests) / rowSums(!is.na(df_current[whichSig,otherTests]),na.rm=T))
        # eliminate 0:
        anzOthers$y <- anzOthers$y[anzOthers$x>0]
        anzOthers$x <- anzOthers$x[anzOthers$x>0]
        profile[i1,anzOthers$x] <- anzOthers$y
      }
#      for(anz in 1:length(otherTests)){
#        profileOld[i1,anz] = sum(df_current[,test1] & rowSums(df_current[,otherTests],na.rm=T)==anz ,na.rm=T)
#      }
    }
#    profile_listOld[[i]] <- profileOld # store it for other analyses
    profile_list[[i]] <- profile # store it for other analyses
  }
  names(profile_list) <- data_names
  #names(profile_listOld) <- data_names
  
  return(profile_list)
}
