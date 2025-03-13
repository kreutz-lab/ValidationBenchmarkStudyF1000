# Script to analyze the results from differential abundance tests on filtered and unfiltered data
# A subset of the 13 conclusions from Box1 are tested

print("Script script_5.x_Aim2_primary_filtered+nonFiltered.R started ...")

if(!dir.exists(dataFolder) || !dir.exists(dfFolder)){
  warning(dataFolder, " or ", dfFolder, "does not exist !!!!!!!!!!!!")
  
}else{
  
  prefix <- bs_processResultFoldernames(outFolder,"dataName")
  
  try(file.remove(paste0(outFolder,"/H_true_all.RDS"))) # to be sure that nothing accumulates
  try(file.remove(paste0(outFolder,"/df_H_true_all.RDS"))) # to be sure that nothing accumulates

  print(paste0("prefix=",prefix))
  print(paste0("dataFolder=",dataFolder))
  print(paste0("dfFolder=",dfFolder))
  print(paste0("dfFolderFiltered=",dfFolderFiltered))
  print(paste0("outFolder=",outFolder))
  print(paste0("dataSetFilter=",dataSetFilter))
  save(dataFolder,dfFolder,outFolder,dataSetFilter,dfFolderFiltered,prefix,file=paste0(outFolder,"/folders.Rdata"))
  
  
  ###############################
  
  data_to_compare <- readRDS(paste0(dataFolder,"/data_to_compare.RDS"))
  
  try(dir.create(outFolder))
  
  #Hier könnte man auch direkt einlesen, wie bei den profilen oder?
  # DF_significance for unfilted data
  DF_significance <- readRDS(file=paste0(dfFolder,"/DF_significance.RDS"))
  
  # Additionally get DF_significance for filtered data
  DF_significance_filt <- readRDS(file=paste0(dfFolderFiltered ,"/DF_significance.RDS"))
  
  ############################################
  ## Now filter according to exp or simu #####
  DF_significance <- bs_apply_dataSetFilter(DF_significance,dataSetFilter,dfFolder=dfFolder)
  DF_significance_filt <- bs_apply_dataSetFilter(DF_significance_filt,dataSetFilter,dfFolder=dfFolder)
  
  ## End filter according to exp or simu #####
  ############################################
  
  
  # Read consistency profiles for filtered and unfiltered:
  profile_list <- readRDS(paste0(dfFolder,"/profile_list.RDS"))
  profile_list_filt <- readRDS(paste0(dfFolderFiltered,"/profile_list.RDS"))
  
  # 
  test_cols <- which(sapply(DF_significance, is.logical)) # indices
  test_names <- names(DF_significance)[test_cols] 
  data_names <- unique(DF_significance$project_dataset) 
  data_names_filt <- unique(DF_significance_filt$project_dataset) 
  data_names.keep <- intersect(data_names,data_names_filt)
  
  # # Only keep projects that occur in filtered and unfiltered data
  # keep_project <- intersect(DF_significance_filt$project,DF_significance$project)
  # DF_significance.keep <- DF_significance[DF_significance$project %in% keep_project,]
  # DF_significance_filt.keep <- DF_significance_filt[DF_significance_filt$project %in% keep_project,]
  # 
  # # Get data_names of remaining data sets
  # data_names.keep <- unique(DF_significance.keep$project_dataset) 
  
  aim2_results <- list() # this list will only contain results for the subset of hyptheses where both (filtered and unfiltered) is required
  aim2_regression <- list() # 
  
  
  ### Hypothesis 8: The shape of the overlap profiles for all methods except both limma voom approaches is mainly determined by the exp. data set and the DA method but only little of the fact whether data has been filtered. 
  profile_list <- readRDS(paste0(dfFolder,"/","profile_list.RDS")) # read overlap profiles calculated before
  profile_list_filt <- readRDS(paste0(dfFolderFiltered,"/","profile_list.RDS")) # read overlap profiles calculated before
  
  profile_names <- intersect(names(profile_list), names(profile_list_filt))
  profile_list <- profile_list[profile_names]
  profile_list_filt <- profile_list_filt[profile_names]
  
  # convert frequencies in profile_list to numbers
  Profile_list <- list()
  Profile_list_filt <- list()
  for(i in 1:length(profile_names)){
    Profile_list[[profile_names[i]]] <- list()
    Profile_list_filt[[profile_names[i]]] <- list()
    
    profile <- profile_list[[profile_names[i]]]
    profile_filt <- profile_list_filt[[profile_names[i]]]
    tests <- intersect(rownames(profile),rownames(profile_filt))
    
    for(test in tests){
      Profile_list[[profile_names[i]]][[test]] <- rep(1:ncol(profile),profile[test,])
      Profile_list_filt[[profile_names[i]]][[test]] <- rep(1:ncol(profile_filt),profile_filt[test,])
    }
  }
  # End convert frequencies in profile_list to numbers
  
  H_true <- matrix(NA,nrow=length(profile_list),ncol=4)
  rownames(H_true) <- names(profile_list)
  colnames(H_true) <- c("vs_otherData","vs_otherData_Filt","vs_otherTest","vs_otherTest_Filt")
  
  # Begin of hypothesis-specific code
  # For each profile and test: compare wether filt_vs_unfit are more similar than test_vs_otherTest and than profile_vs_otherProfile
  cat("\nPrimary Hypothesis 8 requires many ks.tests and is therefore slow. Be patient ...\n\n")
  for(i in 1:length(Profile_list)){  # all
    cat(".") # one point per data_name
    profile <- Profile_list[[i]]
    profile_filt <- Profile_list_filt[[i]]
    
    tests <- intersect(names(profile),names(profile_filt))
    tests <- setdiff(tests,c("limma_voom","limma_voom_TMMwsp")) # Hypothesis does not include both limma_voom approaches
    for(test in tests){
      if(sum(profile[[test]])>=10 && sum(profile_filt[[test]])>=10){ # at least 10 significant ones in both
        ks_Filter <- ks.test(profile[[test]],profile_filt[[test]],exact=F)$statistic
        
        otherProfiles <- Profile_list[-i]
        otherProfiles_filt <- Profile_list_filt[-i]
        for(otherProfile in otherProfiles){
          if(sum(otherProfile[[test]])>=10){ # at least 10 significant ones in both
            ks_data <- ks.test(profile[[test]],otherProfile[[test]],exact=F)$statistic
            H_true[i,"vs_otherData"] <- ks_Filter<ks_data
          }
        }
        for(otherProfile_filt in otherProfiles_filt){
          if(sum(otherProfile_filt[[test]])>=10){ # at least 10 significant ones in both
            ks_data_filt <- ks.test(profile_filt[[test]],otherProfile_filt[[test]],exact=F)$statistic
            H_true[i,"vs_otherData_Filt"] <- ks_Filter<ks_data_filt
          }
        }
        
        otherTests <- setdiff(tests,test)
        for(otherTest in otherTests){
          if(sum(profile[[otherTest]])>=10){ # at least 10 significant ones in both
            ks_test <- ks.test(profile[[test]],profile[[otherTest]],exact=F)$statistic
            H_true[i,"vs_otherTest"] <- ks_Filter<ks_test
          }
          if(sum(t(profile_filt[[otherTest]]))>=10){ # at least 10 significant ones in both
            ks_test_filt <- ks.test(profile_filt[[test]],profile_filt[[otherTest]],exact=F)$statistic
            H_true[i,"vs_otherTest_Filt"] <- ks_Filter<ks_test_filt
          }
        }
      }    
    }
  }
  
  # End of hypothesis-specific code
  cat("\n")
  
  aim2_results <- rbind(aim2_results,
                        bs_validationOutput(H_true, threshold=0.95, prefix=prefix, 
                                            hypoNr=8,
                                            hypothesis="The shape of the overlap profiles for all methods except both limma voom approaches is mainly determined by the exp. data set and the DA method but only little of the fact whether data has been filtered. ",
                                            test=""))
  
  
  # Macht der Plot hier Sinn?
  #hypothesisPlots(DF_significance,aim2_results,profiles = profiles_list, outDir=outFolder)
  
  aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare, outDir=outFolder, aim2res = aim2_results,
                                                                           load_H_true_all=F))
  # aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare,useProject = 1, outDir=outFolder, aim2res = aim2_results))
  
  
  
  
  ### Hypothesis 9: For filtered data, for both limma voom approaches the proportion of identified features that are also identified by the majority of other tests is larger than for un-filtered data 
  try({
    H_true <- matrix(array(NA,dim=length(data_names.keep)*2),ncol=2)
    rownames(H_true) <- data_names.keep
    colnames(H_true) <- c("limma_voom","limma_voom_TMMwsp")
    
    df_tmp_unfiltered <- data.frame()
    df_tmp_filtered <- data.frame()
    
    for(i in 1:length(data_names.keep)){  # all
      cat(".") # one point per data_name
      data_name <- data_names.keep[i]
      df_current_unfiltered <- DF_significance[DF_significance$project_dataset == data_name,]  # only result for current data_name
      df_current_filtered <- DF_significance_filt[DF_significance_filt$project_dataset == data_name,] 
      
      if(nrow(df_current_unfiltered)>=10 && nrow(df_current_filtered)>=10){
        # Compare test results that are present in both filtered and unfiltered data
        test_cols.tmp <- intersect(names(which(sapply(df_current_unfiltered, is.logical))),names(which(sapply(df_current_filtered, is.logical))))
        
        # Get percentage of identified features for both limma methods that are also identified by the majority of all other tests (define majority as 80%)
        
        df_tmp_unfiltered[i,"limma_voom"] <- sum(df_current_unfiltered[df_current_unfiltered$limma_voom == TRUE & (rowSums(df_current_unfiltered[,test_cols.tmp], na.rm=TRUE) >=  0.8 * (length(test_cols.tmp)-1)),"limma_voom"], na.rm = TRUE) / nrow(df_current_unfiltered)
        
        df_tmp_unfiltered[i,"limma_voom_TMMwsp"] <- sum(df_current_unfiltered[df_current_unfiltered$limma_voom_TMMwsp == TRUE & (rowSums(df_current_unfiltered[,test_cols.tmp], na.rm = TRUE) >= 0.8 * (length(test_cols.tmp)-1)),"limma_voom_TMMwsp"], na.rm = TRUE) / nrow(df_current_unfiltered)
        
        
        df_tmp_filtered[i,"limma_voom"] <- sum(df_current_filtered[df_current_filtered$limma_voom==TRUE & (rowSums(df_current_filtered[,test_cols.tmp], na.rm = TRUE) >= 0.8 * (length(test_cols.tmp)-1)),"limma_voom"], na.rm = TRUE) / nrow(df_current_filtered)
        
        df_tmp_filtered[i,"limma_voom_TMMwsp"] <- sum(df_current_filtered[df_current_filtered$limma_voom_TMMwsp==TRUE & (rowSums(df_current_filtered[,test_cols.tmp], na.rm = TRUE) >= 0.8 * (length(test_cols.tmp)-1)),"limma_voom_TMMwsp"], na.rm = TRUE) / nrow(df_current_filtered)
        
        H_true[i,"limma_voom"] <- df_tmp_filtered[i,"limma_voom"] > df_tmp_unfiltered[i,"limma_voom"]
        H_true[i,"limma_voom_TMMwsp"] <- df_tmp_filtered[i,"limma_voom_TMMwsp"] > df_tmp_unfiltered[i,"limma_voom_TMMwsp"]
      }
    }
    
    cat("\n")
    
    aim2_results <- rbind(aim2_results,
                          bs_validationOutput(H_true, threshold=0.95, prefix=prefix, 
                                              hypoNr=9,
                                              hypothesis="For filtered data, for both limma voom approaches the proportion of identified features that are also identified by the majority of other tests is larger than for un-filtered data.",
                                              test=""))
    
    # Macht der Plot hier Sinn?
    #hypothesisPlots(DF_significance,aim2_results,profiles = profiles_list, outDir=outFolder)
    
    aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare, outDir=outFolder, aim2res = aim2_results))
    # aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare,useProject = 1, outDir=outFolder, aim2res = aim2_results))
    
    
    
    
    
    ### Hypothesis 11 : The proportion of features identified by all except one DA method is larger for prevalence-filtered data.
    
    H_true <- array(NA,dim=length(data_names.keep))
    names(H_true) <- data_names.keep
    
    for(i in 1:length(data_names.keep)){  # all
      cat(".") # one point per data_name
      data_name <- data_names.keep[i]
      df_current_unfiltered <- DF_significance[DF_significance$project_dataset == data_name,]  # only result for current data_name
      df_current_filtered <- DF_significance_filt[DF_significance_filt$project_dataset == data_name,] 
      
      # Compare test results that are present in both filtered and unfiltered data
      test_cols.tmp <- intersect(names(which(sapply(df_current_unfiltered, is.logical))),names(which(sapply(df_current_filtered, is.logical))))
      
      # For each test: Which proportion is found by all DA methods?
      all_unfiltered <- sum(rowSums(!df_current_unfiltered[,test_cols.tmp], na.rm=TRUE)<=1) # number of features where <=1 tests are not significant
      all_filtered <- sum(rowSums(!df_current_filtered[,test_cols.tmp], na.rm=TRUE)<=1) # number of features where <=1 tests are not significant
      
      # Is proportion in filtered data larger compared to unfiltered?
      H_true[i] <- all_unfiltered/nrow(df_current_unfiltered) < all_filtered/nrow(df_current_filtered)
    }
    cat("\n")
    
    aim2_results <- rbind(aim2_results,
                          bs_validationOutput(H_true, threshold=0.95, prefix=prefix, 
                                              hypoNr=11,
                                              hypothesis="The proportion of features identified by all except one DA method is larger for prevalence-filtered data.",
                                              test=""))
    
    #Macht der Plot hier Sinn?
    #hypothesisPlots(DF_significance,aim2_results,profiles = profiles_list, outDir=outFolder)
    
    aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare, outDir=outFolder, aim2res = aim2_results))
    # aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare,useProject = 1, outDir=outFolder, aim2res = aim2_results))
    
    
    
    
    
    ### Hypothesis 12: For filtered data, the consistency profiles of corncob, metagenomeSeq, and DESeq2 
    ### are more similar to the more extreme methods than for unfiltered data. 
    ### (as for Hypothesis 9 defined as the most extreme 2 profiles of other DA methods).
    ###
    ### Original wording: "12.	In contrast with the unfiltered results, corncob, metagenomeSeq, 
    ### and DESeq2 had lower proportions of ASVs at intermediate consistency ranks for prevalence-filtered data. 
    
    H_true <- matrix(nrow=length(data_names.keep),ncol=3)
    rownames(H_true) <- data_names.keep
    colnames(H_true) <- c("corncob","DESeq","metagenomeSeq")
    
    H_true_new <- H_true # alternative analysis, i.e. different from protocol
    
    for(i in 1:length(data_names.keep)){  # all
      cat(".") # one point per data_name
      data_name <- data_names.keep[i]
      df_current_unfiltered <- DF_significance[DF_significance$project_dataset == data_name,]  # only result for current data_name
      df_current_filtered   <- DF_significance_filt[DF_significance_filt$project_dataset == data_name,] 
      
      # Compare test results that are present in both filtered and unfiltered data
      test_cols.tmp <- intersect(names(which(sapply(df_current_unfiltered, is.logical))),names(which(sapply(df_current_filtered, is.logical))))
      
      profile      <- profile_list[[data_name]]
      profile_filt <- profile_list_filt[[data_name]]
      
      # calculate the expectations of the profiles
      means <- array(NA, dim=length(test_cols.tmp),dimnames = list(test_cols.tmp))
      means_filt <- array(NA, dim=length(test_cols.tmp),dimnames = list(test_cols.tmp))
      for(test in test_cols.tmp){
        if(sum(profile_filt[test,])>=10 && sum(profile[test,])>=10)
          means[test] <- sum(1:ncol(profile)*profile[test,],na.rm=T)/sum(profile[test,],na.rm = T)
        if(sum(profile_filt[test,])>=10 && sum(profile[test,])>=10)
          means_filt[test] <- sum(1:ncol(profile_filt)*profile_filt[test,],na.rm=T)/sum(profile_filt[test,],na.rm = T)
      }
      
      if(sum(!is.na(means))>3 && sum(!is.na(means_filt))>3){
        # which test has the 2nd most extreme (2nd smallest and 2nd largest mean)
        secondExtreme      <- names(means[which(means==sort(means,decreasing = F)[2])])[1] # test with 2nd smallest
        secondExtreme_filt <- names(means_filt[which(means_filt==sort(means_filt,decreasing = F)[2])])[1]
        secondExtreme2      <- names(means[which(means==sort(means,decreasing = T)[2])])[1] # test 2nd largest 
        secondExtreme_filt2 <- names(means_filt[which(means_filt==sort(means_filt,decreasing = T)[2])])[1]
        
        for(test in colnames(H_true)){
          # do ks.test of the profile for the test compared with the 2nd most extreme ones, large p-value means NOT extreme
          if(sum(profile[test,])>=10 && sum(profile[secondExtreme,])>=10 && sum(profile[secondExtreme2,])>=10 && 
             sum(profile_filt[test,])>=10 && sum(profile_filt[secondExtreme_filt,])>=10 && sum(profile_filt[secondExtreme_filt2,])>=10){
            pks1 <- ks.test(rep(1:ncol(profile),profile[test,]),rep(1:ncol(profile),profile[secondExtreme,]))$p.value
            pks2 <- ks.test(rep(1:ncol(profile),profile[test,]),rep(1:ncol(profile),profile[secondExtreme2,]))$p.value
            pks <- min(pks1,pks2)  # minimal p-value for most distant (if intermediate, than this p-value is as large as possible)
            
            pks1 <- ks.test(rep(1:ncol(profile_filt),profile_filt[test,]),rep(1:ncol(profile),profile[secondExtreme_filt,]))$p.value
            pks2 <- ks.test(rep(1:ncol(profile_filt),profile_filt[test,]),rep(1:ncol(profile),profile[secondExtreme_filt2,]))$p.value
            pks_filt <- max(pks1,pks2)
            
            H_true[i,test] <- pks > pks_filt 
          }
        }
      }
      
      # In contrast to the protocol, I now think, it might be better to check whether means[htest] is closer to the middle of the profile
      for(test in colnames(H_true_new)){
        if(!is.na(means[test]) && !is.na(means_filt[test]))
          H_true_new[i,test] <- abs(means[test] - ncol(profile))  < abs(means_filt[test] - ncol(profile_filt))
      }
      
    }
    cat("\n")
    warning("This code has to be checked, could not be tested since profile_list for filtered data was missing.")
    
    aim2_results <- rbind(aim2_results,
                          bs_validationOutput(H_true, threshold=0.95, prefix=prefix, 
                                              hypoNr=12,
                                              hypothesis="For filtered data, the consistency profiles of corncob, metagenomeSeq, and DESeq2 are more similar to the more extreme methods than for unfiltered data.",
                                              test=""))
    
    # aim2_results <- rbind(aim2_results,
    #                      bs_validationOutput(H_true_new, threshold=0.95, prefix=prefix, 
    #                                          hypoNr=12.2,
    #                                          hypothesis="12b (new approach): For filtered data, the consistency profiles of corncob, metagenomeSeq, and DESeq2 are more similar to the more extreme methods.",
    #                                          test=""))
    
    
    
    #Macht der Plot hier Sinn?
    #hypothesisPlots(DF_significance,aim2_results,profiles = profiles_list, outDir=outFolder)
    
    aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare, outDir=outFolder, aim2res = aim2_results))
    # aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare,useProject = 1, outDir=outFolder, aim2res = aim2_results))
    
    
    ### Save result
    saveRDS(aim2_results,paste0(outFolder,"/","aim2_results.RDS"))
    saveRDS(aim2_regression,paste0(outFolder,"/","aim2_regression.RDS"))
  })
  
  ##################
  cat("\n\n script_5.x_Aim2_primary_filtered+nonFiltered.R finished :-)\n\n")
}