cat("\n\n script_5.x_Aim2_primary_filtered_or_unfiltered.R started ... \n")

if(!dir.exists(dataFolder) ){
  cat("script_5.x_Aim2_primary_filtered_or_unfiltered.R skipped: ",dataFolder, " or ", dfFolder, "does not exist !!!!!!!!!!!! \n")
  warning(dataFolder, " or ", dfFolder, "does not exist, script_5.x_Aim2_primary_filtered_or_unfiltered.R skipped !!!!!!!!!!!!")
  
}else{
  
  
  try(file.remove(paste0(outFolder,"/H_true_all.RDS"))) # to be sure that nothing accumulates
  try(file.remove(paste0(outFolder,"/df_H_true_all.RDS"))) # to be sure that nothing accumulates
  
  prefix <- bs_processResultFoldernames(outFolder,"dataName")
  
  cat("prefix=",prefix, "for aim2_results \n")
  cat("dataFolder=",dataFolder, "  # for reading data_to_compare.RDS, DF.RDS and saving DF_filt\n")
  cat("dfFolder=",dfFolder,"   # for saving DF_significance.RDS and saving/reading profile_lists\n")
  cat("outFolder=",outFolder,"   # for hypothesis plots, aim2results.RDS and aim2regression.RDS \n")
  cat("dataSetFilter=",dataSetFilter,"\n")
  save(dataFolder,dfFolder,outFolder,dataSetFilter,prefix,file=paste0(outFolder,"/folders.Rdata"))
  
  
  if(!exists("doPlots"))
    doPlots <- T
  
  # if(file.exists(paste0(dataFolder,"/data_to_compare.all.RDS"))) # would contain all tests if nearing=T
  ## data_to_compare is only used for getting data_props (i.e. it's okay if not all tests are there, e.g. only ancomNearing)
  ## The results after writing ancomNearing in ancombc are in DF.RDS
  data_to_compare <- readRDS(paste0(dataFolder,"/data_to_compare.RDS"))
  
  try(dir.create(outFolder))
  try(dir.create(dfFolder))
  
  # Get test results for metaSPARSim
  DF <- readRDS(paste0(dataFolder,"/DF.RDS"))
  
  ## Own p.adjust
  # Calculate adjusted p-values using p.adjust() within each combination of test, dataset, and project
  DF <- DF %>%
    group_by(test, dataset, project) %>%
    mutate(p_adjusted_CK = p.adjust(p_value, method = "fdr")) %>%
    ungroup()
  
  # replace missing padj by padj_ck:
  DF[is.na(DF$p_adjusted),"p_adjusted"] <- DF[is.na(DF$p_adjusted),"p_adjusted_CK"]
  #hist(DF$p_adjusted) # looks better
  #hist(DF$p_adjusted_CK)
  
  
  # Add information about significance
  DF <- data.frame(DF,significant=DF$p_adjusted<=0.05)
  
  ## Filter out projects=templates that cannot be simulated realistically
  # This is only required here, because scripts for filtered+unfiltered and for secondary aims load DF_significance.RDS created and saved below
  if(dataSetFilter=="simEquiv"){
    raus <- readRDS(paste0(dfFolder,"/ToRemove_because_outlierInSimilarityWithTemplate.RDS"))
    if(length(raus)>0){
      raus <- raus[raus] # keep only values and names where value = TRUE
      for(toRemove in names(raus)){
        cat("simEquiv: Remove template ",toRemove," because it occurs in ToRemove_because_outlierInSimilarityWithTemplate.RDS...\n")
        DF <- DF[toRemove != DF$project,]
      }
    }else{
      cat("simEquiv: Nothing to remove according to ToRemove_because_outlierInSimilarityWithTemplate.RDS \n")
    }
  }
  ## End of: Filter out projects=templates that cannot be simulated realistically
  
  # Remove tests without results 
  DF_filt <- DF[!is.na(DF$p_value),]
  # If there are duplicates (because analyses have been merged) remove them
  DF_filt$ID <- paste0(DF_filt$dataset,DF_filt$test, DF_filt$feature)
  DF_filt <- DF_filt %>% distinct(ID, .keep_all = TRUE)
  # What percentage of features remain?
  cat("Remove tests without results: What percentage of features remain?",(nrow(DF_filt)/nrow(DF))*100,"\n")
  
  # Save filtered DF with information about significance
  saveRDS(DF_filt, paste0(dfFolder,"/DF_filt.RDS"))
  
  
  ##########################
  # Preparing data for testing hypotheses of aim 2 - primary outcomes , Box 1
  
  # Get data frame with templates and features as rows and information about significance for each test in columns
  DF_significance <- pivot_wider(DF_filt, names_from =test, values_from=significant,id_cols = c("feature","dataset","project"))
  
  # Spalte zur Selektion der ca. 380 Datensätze hinzufügen:
  DF_significance <- data.frame(DF_significance,
                                project_dataset = paste(DF_significance$project, DF_significance$dataset, sep="_")) 
  # Save
  saveRDS(DF_significance, file=paste0(dfFolder,"/","DF_significance.RDS"))
  
  ############################################
  ## Now filter according to exp or simu #####
  DF_significance <- bs_apply_dataSetFilter(DF_significance,dataSetFilter,dfFolder=dfFolder)
  ## End filter according to exp or simu #####
  ############################################
  
  
  test_cols <- which(sapply(DF_significance, is.logical)) # indices
  test_names <- names(DF_significance)[test_cols] 
  data_names <- unique(DF_significance$project_dataset) 
  project_names <- unique(DF_significance$project) 
  
  
  #################################################
  #### Build and save list of the overlap profiles:
  profile_list <- bs_calculateOverlapProfiles(DF_significance,data_names,test_names)
  saveRDS(profile_list,paste0(dfFolder,"/profile_list.RDS"))
  
  # Save all profiles in one table
  df_list <- imap(profile_list, ~ as.data.frame(.x) %>% mutate(template = .y,test = rownames(.x)))
  # Combine all data frames into a single data frame
  all_profiles <- bind_rows(df_list)
  saveRDS(all_profiles,paste0(dfFolder,"/all_profiles.RDS"))
  openxlsx::write.xlsx(all_profiles,paste0(dfFolder,"/all_profiles.xlsx") )
  
  
  # profiles obtained by merging all datasets of a project:  (otherwise Hypothesis 8 requires around 3 Mio ks.tests)
  profile_listP <- list()
  for(i in 1:length(project_names)){  # all
    cat(".") # one point per data_name
    project_name <- project_names[i]
    df_current <- DF_significance[DF_significance$project == project_name,]  # only result for current project_name
    
    # Begin of hypothesis-specific code
    # Calculate overlap "profiles" first:
    profile <- matrix(array(NA,dim=length(test_names)*(length(test_names)-1)),nrow=length(test_names)) # here I will count how many others are significant
    dimnames(profile)[[1]] <- test_names
    dimnames(profile)[[2]] <- as.character(1:dim(profile)[2])
    
    for(i1 in 1:length(test_names)){
      test1 <- test_names[i1] # profile of this test, i.e. count how often are other tests significant
      otherTests <- setdiff(test_names,test1)
      for(anz in 1:length(otherTests)){
        profile[i1,anz] = sum(df_current[,test1] & rowSums(df_current[,otherTests],na.rm=T)==anz ,na.rm=T)
      }
    }
    profile_listP[[i]] <- profile # store it for other analyses
  }
  names(profile_listP) <- project_names
  saveRDS(profile_listP,paste0(dfFolder,"/profile_list_projects.RDS"))
  
  #############################
  ########## Hypotheses:
  profile_listP <- readRDS(paste0(dfFolder,"/profile_list_projects.RDS"))
  profile_list <- readRDS(paste0(dfFolder,"/profile_list.RDS"))
  
  #######
  aim2_results <- list() # in this list the results will be stored
  aim2_regression <- list()
  
  ### Hypothesis 1 : For UNFILTERED data the proportion of features jointly found as significant by limma voom TMM and limma voom TMMwsp but by less than 50% of the other methods, is larger than the overlap with more than 50% of the other methods.
  H_true <- array(NA,dim=length(data_names))
  names(H_true) <- data_names
  
  for(i in 1:length(data_names)){  # all
    cat(".") # one point per data_name
    data_name <- data_names[i]
    df_current <- DF_significance[DF_significance$project_dataset == data_name,]  # only result for current data_name
    otherTests <- setdiff(test_names,c("limma_voom","limma_voom_TMMwsp"))
    # Count features that are significant for both limma methods and for less than 50% of the other methods
    limma_few_others <- sum(df_current[,"limma_voom"] & df_current[,"limma_voom_TMMwsp"] & rowSums(df_current[,otherTests] < rowSums(!is.na(df_current[,otherTests])),na.rm=T)*0.5,na.rm=T)
    # Count features that are significant for both limma methods and for more than 50% of the other methods
    limma_most_others <- sum(df_current[,"limma_voom"] & df_current[,"limma_voom_TMMwsp"] & rowSums(df_current[,otherTests] >= rowSums(!is.na(df_current[,otherTests])),na.rm=T)*0.5,na.rm=T)
    
    H_true[i] <- limma_few_others > limma_most_others
    if(sum(df_current[,"limma_voom"] & df_current[,"limma_voom_TMMwsp"] ,na.rm=T)<1 || !"limma_voom" %in% colnames(df_current))
      H_true[i] <- NA
  }
  cat("\n")
  
  
  aim2_results <- rbind(aim2_results,
                        bs_validationOutput(H_true, threshold=0.95, prefix=prefix, 
                                            hypoNr=1.1,
                                            hypothesis="For UNFILTERED data the proportion of features jointly found as significant by limma voom TMM and limma voom TMMwsp but by less than 50% of the other methods, is larger than the overlap with more than 50% of the other methods",
                                            test="limma_voom_TMMwsp"))
  
  if(doPlots)
    hypothesisPlots(DF_significance,aim2_results[nrow(aim2_results),], profiles = profiles_list, outDir=outFolder)
  
  aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare, outDir=outFolder, aim2res = aim2_results,
                                                                           load_H_true_all=F))
  # aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare,useProject = 1, outDir=outFolder, aim2res = aim2_results,load_H_true_all=F))
  
  
  
  ### Hypothesis 2: For UNFILTERED data the overlap of features jointly found as significant by limma voom TMM and limma voom TMMwsp with features found by Wilcoxon CLR is larger than the overlap with all other DA methods. 
  H_true <- array(NA,dim=length(data_names))
  names(H_true) <- data_names
  
  for(i in 1:length(data_names)){  # all
    cat(".") # one point per data_name
    data_name <- data_names[i]
    df_current <- DF_significance[DF_significance$project_dataset == data_name,]  # only result for current data_name
    
    # Begin of hypothesis-specific code
    # Now loop over all test combis:
    counter <- 0
    otherTests <- setdiff(test_names,c("limma_voom","limma_voom_TMMwsp"))
    for(i1 in 1:length(otherTests)){
      otherTest <- otherTests[i1]
      counter <- counter+1
      
      overlap = sum( df_current[,"limma_voom"] & df_current[,"limma_voom_TMMwsp"] & df_current[,otherTest] ,na.rm=T)
      if(counter==1)
        df_tmp <- data.frame(otherTest=otherTest, overlap=overlap) # first row
      else
        df_tmp <- rbind(df_tmp, data.frame(otherTest=otherTest, overlap=overlap)) # add one row
    }
    
    hRows = df_tmp$otherTest=="bs_wilcox.test" 
    H_true[i] <- df_tmp$overlap[hRows] > max(df_tmp$overlap[!hRows])  # is overlap with bs_wilcox.test larger than the max overlap of all other tests?
    # End of hypothesis-specific code
  }
  cat("\n")
  
  aim2_results <- rbind(aim2_results,
                        bs_validationOutput(H_true, threshold=0.95, prefix=prefix, 
                                            hypoNr=2,
                                            hypothesis="For UNFILTERED data the overlap of features jointly found as significant by limma voom TMM and limma voom TMMwsp with features found by Wilcoxon CLR is larger than the overlap with all other DA methods",
                                            test="both limma vooms & wilcoxon"))
  
  if(doPlots)
    hypothesisPlots(DF_significance,aim2_results[nrow(aim2_results),],profiles = profiles_list, outDir=outFolder)
  
  aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare, outDir=outFolder, aim2res = aim2_results))
  # aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare,useProject = 1, outDir=outFolder, aim2res = aim2_results))
  
  
  
  
  ### Hypothesis 3: For UNFILTERED data, the Kolmogorov-Smirnov test statistic D when comparing the profile for Wilcoxon CLR and Wilcoxon rare is larger than for other pairs of methodson average. 
  H_true <- array(NA,dim=length(data_names))
  names(H_true) <- data_names
  
  #profile_list <- list()
  for(i in 1:length(data_names)){  # all
    cat(".") # one point per data_name
    data_name <- data_names[i]
    profile <- profile_list[[data_name]]
    
    df_tmp <- NULL
    # Now loop over all test combis:
    for(i1 in 1:(length(test_names)-1)){
      test1 <- test_names[i1]
      for(i2 in (i1+1):length(test_names)){
        test2 <- test_names[i2]
        if(sum(profile[test1,])>=10 && sum(profile[test2,])>=10){
          p.ks = ks.test( rep(1:ncol(profile),profile[test2,]),rep(1:ncol(profile),profile[test1,]) )$statistic
          if(is.null(df_tmp))
            df_tmp <- data.frame(test1=test1, test2=test2, p.ks=p.ks) # first row
          else
            df_tmp <- rbind(df_tmp, data.frame(test1=test1, test2=test2, p.ks=p.ks)) # add one row
        }
      }
    }
    
    hRows = df_tmp$test1=="bs_wilcox.test" & df_tmp$test2=="bs_wilcox_rare.test"
    if(sum(hRows)==1)
      H_true[i] <- df_tmp$p.ks[hRows] > mean(df_tmp$p.ks[!hRows], na.rm=TRUE) 
    # End of hypothesis-specific code
  }
  cat("\n")
  
  aim2_results <- rbind(aim2_results,
                        bs_validationOutput(H_true, threshold=0.95, prefix=prefix, 
                                            hypoNr=3,
                                            hypothesis="For UNFILTERED data, the Kolmogorov-Smirnov test statistic D when comparing the profile for Wilcoxon CLR and Wilcoxon rare is larger than for other pairs of methodson average.",
                                            test="wilcoxon CLR & wilcoxon"))
  
  if(doPlots)
    hypothesisPlots(DF_significance,aim2_results[nrow(aim2_results),],profiles = profile_list, outDir=outFolder)
  
  aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare, outDir=outFolder, aim2res = aim2_results))
  #aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare,useProject = 1, outDir=outFolder, aim2res = aim2_results))
  
  
  
  
  ### Hypothesis 4: For UNFILTERED data, MaAsLin2 and MaAsLin2-rare have a more similar profile (larger test statistic D) than a randomly selected pair of methods. 
  H_true <- array(NA,dim=length(data_names))
  names(H_true) <- data_names
  
  #profile_list <- readRDS(paste0(dfFolder,"/","profile_list.RDS")) # read overlap profiles calculated before
  
  for(i in 1:length(data_names)){  # all
    df_tmp <- NULL
    
    cat(".") # one point per data_name
    data_name <- data_names[i]
    df_current <- DF_significance[DF_significance$project_dataset == data_name,]  # only result for current data_name
    
    # Begin of hypothesis-specific code
    # Now loop over all test combis:
    profile <- profile_list[[data_name]]
    for(i1 in 1:(length(test_names)-1)){
      test1 <- test_names[i1]
      for(i2 in (i1+1):length(test_names)){
        test2 <- test_names[i2]
        #counter <- counter+1
        
        if(sum(profile[test1,])>=10 && sum(profile[test2,])>=10){
          p.ks = ks.test( rep(1:ncol(profile),profile[test1,]), rep(1:ncol(profile),profile[test2,]))$statistic
          if(is.null(df_tmp))
            df_tmp <- data.frame(test1=test1, test2=test2, p.ks=p.ks) # first row
          else
            df_tmp <- rbind(df_tmp, data.frame(test1=test1, test2=test2, p.ks=p.ks)) # add one row
        }
      }
    }
    
    hRows = df_tmp$test1=="Maaslin2" & df_tmp$test2=="Maaslin2_rare"
    if(sum(hRows)==1){
      randomOtherRows <- sample(which(!hRows),1000,replace = T)
      H_true[i] <- df_tmp$p.ks[hRows] < mean(df_tmp$p.ks[randomOtherRows], na.rm=TRUE)
    }
    # End of hypothesis-specific code
  }
  cat("\n")
  
  
  aim2_results <- rbind(aim2_results,
                        bs_validationOutput(H_true, threshold=0.95, prefix=prefix, 
                                            hypoNr=4,
                                            hypothesis="For UNFILTERED data, MaAsLin2 and MaAsLin2-rare have a more similar profile (larger test statistic D) than a randomly selected pair of methods. ",
                                            test="Maaslin rare & Maaslin"))
  
  if(doPlots)
    hypothesisPlots(DF_significance,aim2_results[nrow(aim2_results),],profiles = profile_list, outDir=outFolder)
  
  aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare, outDir=outFolder, aim2res = aim2_results))
  # aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare,useProject = 1, outDir=outFolder, aim2res = aim2_results))
  
  
  ### Hypothesis 5: For UNFILTERED data, ALDEx2 and ANCOM-BC identify more features that were also identified by all except 3 (i.e. 10 out of 13) other methods
  H_true <- matrix(array(NA,dim=length(data_names)*2),ncol=2)
  rownames(H_true) <- data_names
  colnames(H_true) <- c("ALDEx2","ancombc")
  H_true <- H_true[,intersect(colnames(H_true),test_names),drop=F] # handling if a test is not available
  
  for(i in 1:length(data_names)){  # all
    cat(".") # one point per data_name
    data_name <- data_names[i]
    df_current <- DF_significance[DF_significance$project_dataset == data_name,]  # only result for current data_name
    
    for(test in colnames(H_true)){
      # Begin of hypothesis-specific code
      otherTests <- setdiff(test_names,test)
      
      tenOrMore <- rowSums(df_current[,otherTests],na.rm=T)>=length(otherTests)-3
      lessThanTen <- rowSums(df_current[,otherTests],na.rm=T)<length(otherTests)-3
      H_true[i,test] <- sum( df_current[,test] & tenOrMore ,na.rm=T) > sum( df_current[,test] & lessThanTen ,na.rm=T)
      if(sum(df_current[,test],na.rm=T)<1 || !test %in%colnames(df_current)) # No significant ones found by the test
        H_true[i,test] <- NA
    }
    # End of hypothesis-specific code
  }
  cat("\n")
  
  aim2_results <- rbind(aim2_results,
                        bs_validationOutput(H_true, threshold=0.95, prefix=prefix, 
                                            hypoNr=5,
                                            hypothesis="For UNFILTERED data, ALDEx2 and ANCOM-BC identify more features that were also identified by all except 3 (i.e. 10 out of 13) other methods",
                                            test=""))
  # Plots for ALDEx2 and ANCOM-BC
  if(doPlots)
    hypothesisPlots(DF_significance[which(DF_significance$ALDEx2),],aim2_results[nrow(aim2_results)-1,],profiles = profile_list, outDir=outFolder)
  if(doPlots)
    hypothesisPlots(DF_significance[which(DF_significance$ancombc),],aim2_results[nrow(aim2_results),],profiles = profile_list, outDir=outFolder)
  
  aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare, outDir=outFolder, aim2res = aim2_results))
  #aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare,useProject = 1, outDir=outFolder, aim2res = aim2_results))
  
  
  
  
  ### Hypothesis 6: For UNFILTERED data, EdgeR and LEfSe identify a larger percentage of features that are not identified by any other tool, compared to the same percentage for all other methods.
  # Calculate percentage of features found by either edgeR or LefSE but by none other tool. For all remaining tools calculate percentage of features found by that tool, but by none other tool. Compare
  
  H_true <- matrix(array(NA,dim=length(data_names)*2),ncol=2)
  rownames(H_true) <- data_names
  colnames(H_true) <- c("EdgeR","bs_lefse")
  H_true <- H_true[,intersect(colnames(H_true),test_names),drop=F] # handling if a test is not available
  df_tmp <- data.frame()
  
  for(i in 1:length(data_names)){  # all
    cat(".") # one point per data_name
    data_name <- data_names[i]
    df_current <- DF_significance[DF_significance$project_dataset == data_name,]  # only result for current data_name
    
    # Begin of hypothesis-specific code
    df_current <- df_current[rowSums(df_current[,test_names],na.rm=T)==1,] # only rows with exactly one significant results play a role (using only those makes code faster)
    if(nrow(df_current)>=1){
      # Now loop over all test combis:
      for(test in test_names){ 
        otherTests <- setdiff(test_names,test)
        # are more found by that test, as by any other? (I restricted this to only features with exactly one significant test above)
        df_tmp[i,test] <- sum(df_current[, test], na.rm = TRUE) > sum(rowSums(df_current[, otherTests], na.rm = TRUE),na.rm=T)
      }
      
      # Check if percentage of features identified by edgeR / lefse but none of the other tests is larger then for all other tests
      for(test in colnames(H_true)){
        otherTests <- setdiff(test_names,test)
        H_true[i,test] <- df_tmp[i,test] # > max(df_tmp[i,otherTests]) 
        if(sum(df_current[,test],na.rm=T)<1 || !test %in% colnames(df_current)) # No significant ones found by the test
          H_true[i,test] <- NA
      }
    }
    # End of hypothesis-specific code
  }
  cat("\n")
  
  aim2_results <- rbind(aim2_results,
                        bs_validationOutput(H_true, threshold=0.95, prefix=prefix, 
                                            hypoNr=6,
                                            hypothesis="For UNFILTERED data, EdgeR and LEfSe identify a larger percentage of features that are not identified by any other tool, compared to the same percentage for all other methods.",
                                            test=""))
  
  if(doPlots)
    hypothesisPlots(DF_significance,aim2_results[nrow(aim2_results)-1,],profiles = profile_list, outDir=outFolder)
  if(doPlots)
    hypothesisPlots(DF_significance,aim2_results[nrow(aim2_results),],profiles = profile_list, outDir=outFolder)
  
  aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare, outDir=outFolder, aim2res = aim2_results))
  # aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare,useProject = 1, outDir=outFolder, aim2res = aim2_results))
  
  
  
  
  ##### 
  ### Hypothesis 7: For UNFILTERED data, for corncob, metagenomeSeq, and DESeq2, there are always multiple other methods (i.e. at least 2 out of 10 other DA methods) that have a more extreme consistency profile.
  H_true <- matrix(NA,nrow=length(data_names),ncol=3)
  rownames(H_true) <- data_names
  colnames(H_true) <- c("corncob","DESeq","metagenomeSeq")
  
  # profile_list <- readRDS(paste0(dfFolder,"/","profile_list.RDS")) # read overlap profiles calculated before
  
  means <- matrix(nrow=length(data_names),ncol=length(test_names))
  rownames(means) <- data_names
  colnames(means) <- test_names
  for(i in 1:length(data_names)){  # all
    cat(".") # one point per data_name
    data_name <- data_names[i]
    df_current <- DF_significance[DF_significance$project_dataset == data_name,]  # only result for current data_name
    
    # Begin of hypothesis-specific code
    # Now loop over all test combis:
    profile <- profile_list[[data_name]]
    
    for(test in test_names){
      means[data_name,test] <- sum(1:ncol(profile)*profile[test,],na.rm=T)/sum(profile[test,],na.rm = T) # expectation of the histogram
    }
    
    for(test in colnames(H_true)){
      otherTests <- setdiff(test_names,test)
      twoLarger <- sum(means[data_name,otherTests] > means[data_name,test],na.rm=T) >=2
      twoSmaller <- sum(means[data_name,otherTests] < means[data_name,test],na.rm=T) >=2
      H_true[i,test] <- twoLarger && twoSmaller
      if(sum(df_current[,test],na.rm=T)<1 || !test %in% colnames(df_current)) # No significant ones found by the test
        H_true[i,test] <- NA
    }
    
    # End of hypothesis-specific code
  }
  cat("\n")
  
  aim2_results <- rbind(aim2_results,
                        bs_validationOutput(H_true, threshold=0.95, prefix=prefix, 
                                            hypoNr=7,
                                            hypothesis="For UNFILTERED data, for corncob, metagenomeSeq, and DESeq2, there are always multiple other methods (i.e. at least 2 out of 10 other DA methods) that have a more extreme consistency profile.",
                                            test=""))
  
  if(doPlots){
    hypothesisPlots(DF_significance,aim2_results[nrow(aim2_results)-2,],profiles = profile_list, outDir=outFolder)
    hypothesisPlots(DF_significance,aim2_results[nrow(aim2_results)-1,],profiles = profile_list, outDir=outFolder)
    hypothesisPlots(DF_significance,aim2_results[nrow(aim2_results),],profiles = profile_list, outDir=outFolder)
  }
  
  aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare, outDir=outFolder,aim2res = aim2_results))
  # aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare,useProject = 1, outDir=outFolder,aim2res = aim2_results))
  
  
  
  
  ### Hypothesis 8: The shape of the overlap profiles is mainly determined by the exp. data set and the DA method but only little of the fact whether data has been filtered. 
  print("Hypothesis 8 requires filtered and unfiltered data and is done elsewhere.")
  
  ### Hypothesis 9: For filtered data, for both limma voom approaches the proportion of identified features that are also identified by the majority of other tests is larger than for un-filtered data 
  print("Hypothesis 9 requires filtered and unfiltered data and is done elsewhere.")
  
  
  ### Hypothesis 10: For FILTERED data, the overlap profile of Wilcoxon CLR is bimodal.
  test <- "bs_wilcox.test"
  #profile_list <- readRDS(paste0(dfFolder,"/","profile_list.RDS")) # read overlap profiles calculated before
  
  H_true <- array(NA,dim=length(profile_list))
  names(H_true) <- names(profile_list)
  
  # Begin of hypothesis-specific code
  BI <- array(dim=length(profile_list)) # bimodality index
  for(i in 1:length(profile_list)){  # all
    cat(".") # one point per data_name
    profile <- profile_list[[i]]
    
    if(sum(t(profile[test,]))>=10){ # at least 10 significant ones
      BI[i] <- BimodalIndex::bimodalIndex(t(profile[test,]),verbose = F)$BI
      H_true[i] <- BI[i] > 1.1 # cutoff according to documentation, i.e. the paper of Wang et al.
    }
  }
  # End of hypothesis-specific code
  cat("\n")
  
  aim2_results <- rbind(aim2_results,
                        bs_validationOutput(H_true, threshold=0.95, prefix=prefix, 
                                            hypoNr=10,
                                            hypothesis="For FILTERED data, the overlap profile of Wilcoxon CLR is bimodal.",
                                            test="Wilcoxon CLR"))
  
  if(doPlots)
    hypothesisPlots(DF_significance,aim2_results[nrow(aim2_results),],profiles = profile_list, outDir=outFolder)
  
  aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare, outDir=outFolder,aim2res = aim2_results))
  # aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare,useProject = 1, outDir=outFolder, aim2res = aim2_results))
  
  sink(paste0(outFolder,"/H10_bimodality_index.txt"))
  print(sort(BI))
  quantile(BI,0.05,na.rm=T)
  sink()
  
  ### Hypothesis 11 : The proportion of features identified by all except one DA method is larger for prevalence-filtered data.
  print("Hypothesis 11 requires filtered and unfiltered data and is done elsewhere.")
  
  
  ### Hypothesis 12: For filtered data, the consistency profiles of corncob, metagenomeSeq, and DESeq2 are more similar (than for unfilterd) to the more extreme methods (as for Hypothesis 9 defined as the most extreme 2 profiles of other DA methods).
  print("Hypothesis 12 requires filtered and unfiltered data and is done elsewhere.")
  
  
  ### Hypothesis 13: For FILTERED data, ALDEx2 and ANCOM-II identify more features that were also identified by all except 3 (i.e. 10 out of 13) other methods.
  
  H_true <- matrix(array(NA,dim=length(data_names)*2),ncol=2)
  rownames(H_true) <- data_names
  colnames(H_true) <- c("ALDEx2","ancombc")
  
  # Get percentage of significant features for unfiltered data
  for(i in 1:length(data_names)){  # all
    cat(".") # one point per data_name
    data_name <- data_names[i]
    df_current<- DF_significance[DF_significance$project_dataset == data_name,]  # only result for current data_name
    
    # Begin of hypothesis-specific code
    for(test in colnames(H_true)){
      if(test %in% colnames(df_current)){
        otherTests <- setdiff(test_names,test)
        H_true[i,test] <- sum( df_current[,test] & rowSums(df_current[,otherTests],na.rm=T)>=length(otherTests)-3 ,na.rm=T) > sum( df_current[,test] & rowSums(df_current[,otherTests],na.rm=T)<length(otherTests)-3 ,na.rm=T)
      }
    }
    # End of hypothesis-specific code
  }
  cat("\n")
  
  aim2_results <- rbind(aim2_results,
                        bs_validationOutput(H_true, threshold=0.95, prefix=prefix, 
                                            hypoNr=13,
                                            hypothesis="For FILTERED data, ALDEx2 and ANCOM-II identify more features that were also identified by all except 3 (i.e. 10 out of 13) other methods.",
                                            test=""))
  
  if(doPlots)
    hypothesisPlots(DF_significance,aim2_results[nrow(aim2_results),],profiles = profile_list, outDir=outFolder)
  
  aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare, outDir=outFolder, aim2res = aim2_results))
  # aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare,useProject = 1, outDir=outFolder, aim2res = aim2_results))
  
  ### Save result
  saveRDS(aim2_results,paste0(outFolder,"/","aim2_results.RDS"))
  saveRDS(aim2_regression,paste0(outFolder,"/","aim2_regression.RDS"))
  
  ##################
  cat("\n\nscript_5.x_Aim2_primary_filtered_or_unfiltered.R finished :-)\n\n")
  
}