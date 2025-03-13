cat("\n\n script_5.x_Aim2_secondary_filtered_or_unFiltered.R started ... \n\n")

if(!dir.exists(dataFolder) || !dir.exists(dfFolder)){
  warning(dataFolder, " or ", dfFolder, "does not exist !!!!!!!!!!!!")
  
}else{
  
  try(file.remove(paste0(outFolder,"/H_true_all.RDS"))) # to be sure that nothing accumulates
  try(file.remove(paste0(outFolder,"/df_H_true_all.RDS"))) # to be sure that nothing accumulates
  
  prefix <- bs_processResultFoldernames(outFolder,"dataName")
  
  print(paste0("prefix=",prefix))
  print(paste0("dataFolder=",dataFolder))
  print(paste0("dfFolder=",dfFolder))
  print(paste0("outFolder=",outFolder))
  print(paste0("dataSetFilter=",dataSetFilter))
  
  try(dir.create(outFolder))
  save(dataFolder,dfFolder,outFolder,dataSetFilter,prefix,file=paste0(outFolder,"/folders.Rdata"))
  
  data_to_compare <- readRDS(paste0(dataFolder,"/data_to_compare.RDS"))
  
  ############### Preparing data.frames
  #DF_filt <-  readRDS(paste0(dfFolder,"/DF_filt.RDS"))
  DF_significance <- readRDS(paste0(dfFolder,"/DF_significance.RDS"))
  
  
  ############################################
  ## Now filter according to exp or simu #####
  DF_significance <- bs_apply_dataSetFilter(DF_significance,dataSetFilter,dfFolder=dfFolder)
  
  ## End filter according to exp or simu #####
  ############################################
  
  test_cols <- which(sapply(DF_significance, is.logical)) # indices
  test_names <- names(DF_significance)[test_cols] 

  
  # Proportions of significant features by data set and DA method
  DF_prop <- data.frame()
  
  data_names <- unique(DF_significance$project_dataset)
  DF_prop <- NULL
  for(i in 1:length(data_names)){
    data_name <- data_names[i]
    DF_tmp <- DF_significance[DF_significance$project_dataset==data_name,]
    
    # Calculate proportions of sig features per test
    test_cols <- which(sapply(DF_tmp, is.logical))
    
    prop.tmp <- data.frame(t(colSums(DF_tmp[,test_cols], na.rm=TRUE) / nrow(DF_tmp)))
    prop.tmp$dataset <- unique(DF_tmp$dataset)
    prop.tmp$project <- unique(DF_tmp$project)
    prop.tmp$project_dataset <- data_name
    
    if(is.null(DF_prop))
      DF_prop <- prop.tmp
    else
      DF_prop <- rbind(DF_prop,prop.tmp)
  }
  # Save 
  saveRDS(DF_prop, file=paste0(dfFolder,"/","DF_prop.RDS"))
  #DF_prop <- readRDS(paste0(dfFolder,"/","DF_prop.RDS"))
  
  # Rank transform the proportions
  DF_prop_rank <- DF_prop
  testCols <- !colnames(DF_prop_rank) %in% c("dataset","project","project_dataset")
  for(i in 1:nrow(DF_prop_rank))
    DF_prop_rank[i,testCols] <- rank(DF_prop_rank[i,testCols])
  saveRDS(DF_prop_rank, file=paste0(dfFolder,"/","DF_prop_rank.RDS"))
  #DF_prop_rank <- readRDS(paste0(dfFolder,"/","DF_prop_rank.RDS"))
  
  ########################
  # List to store results
  aim2_results <- list() # this list will only contain results for the subset of hyptheses where both (filtered and unfiltered) is required
  aim2_regression <- list() # 
  
  ######################## 
  # Hypotheses:
  
  ### Hypothesis1: For FILTERED and UNFILTERED data, the percentage of significant features identified by each DA method varies widely across data sets.
  # Convert the data from wide to long format
  DF_long <- DF_prop %>%
    pivot_longer(
      cols = -c(dataset, project, project_dataset), 
      names_to = "DA_method", 
      values_to = "percentage_significant"
    )
  
  p <- ggplot(DF_long, aes(x = percentage_significant)) +
    geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
    facet_wrap(~ DA_method, scales = "free_y") +
    labs(
      title = "Variation in Percentage of Significant Features Identified by Each DA Method",
      x = "Percentage of Significant Features",
      y = "Count"
    ) +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  pdf(file=paste0(outFolder,"/Aim2_Hypothesis1_HistPropSig.pdf"),height=10,width = 10)
  print(p)
  dev.off()
  
  p2 <- ggplot(DF_long, aes(x = percentage_significant)) +
    geom_histogram(binwidth = 0.05, fill = "blue", color = "black", alpha = 0.7) +
    facet_wrap(~ project, scales = "free_y") +
    labs(
      title = "Variation in Percentage of Significant Features Identified in Each Data Project",
      x = "Percentage of Significant Features",
      y = "Count"
    ) +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  pdf(file=paste0(outFolder,"/Aim2_Hypothesis1_HistPropSig2.pdf"),height=10,width = 10)
  print(p2)
  dev.off()
  
  if(dataSetFilter!="expAll"){ # not feasible if no different projects
    if(dataSetFilter=="sim" || dataSetFilter=="simEquiv"){
      resAnova <- aov(percentage_significant ~ dataset*project + DA_method,data=DF_long)
      res <- summary(resAnova)
      coefNr = 3
    }else{
      resAnova <- aov(percentage_significant ~ project + DA_method,data=DF_long)
      res <- summary(resAnova)
      coefNr = 2
    }
    save(resAnova,resAnova,file=paste0(outFolder,"/H1_ANOVA.Rdata"))
    
    aim2_results_tmp <- data.frame(
      prefix = prefix, test=paste(formula(resAnova)[c(2,1,3)],collapse=""), propTrue=NA, 
      outcome=paste0("p-value of ",rownames(res[[1]])[coefNr]),
      outcome_value = res[[1]][coefNr,5],
      threshold = 0.05,
      validated = res[[1]][coefNr,5]<0.05,
      hypoNumber = 1,
      hypothesis = "For FILTERED and UNFILTERED data, the percentage of significant features identified by each DA method varies widely across data sets.")
    aim2_results <- rbind(aim2_results, aim2_results_tmp)
    bs_validationOutput_print(aim2_results_tmp,res)
    #aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(?,data_to_compare, outDir=outFolder, aim2res = aim2_results))
    
    
    if(dataSetFilter=="sim" || dataSetFilter=="simEquiv"){
      resAnova <- aov(percentage_significant ~ dataset*project + DA_method*project,data=DF_long)
      res <- summary(resAnova)
      coefNr = 5
    }else{
      resAnova <- aov(percentage_significant ~ project + DA_method+project,data=DF_long)
      res <- summary(resAnova)
      coefNr = 1
    }
    aim2_results_tmp <- data.frame(
      prefix = prefix, test=paste(formula(resAnova)[c(2,1,3)],collapse=""), propTrue=NA, 
      outcome=paste0("p-value of ",rownames(res[[1]])[coefNr]),
      outcome_value = res[[1]][coefNr,5],
      threshold = 0.05,
      validated = res[[1]][coefNr,5]<0.05,
      hypoNumber = 1,
      hypothesis = "For FILTERED and UNFILTERED data, the percentage of significant features identified by each DA method varies widely across data sets.")
    aim2_results <- rbind(aim2_results, aim2_results_tmp)
    bs_validationOutput_print(aim2_results_tmp,res)
    
    #aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(?,data_to_compare, outDir=outFolder, aim2res = aim2_results))
  }
  
  ### Hypothesis 2: For FILTERED and UNFILTERED data, rankings of the DA methods with respect to the proportion of identified features depend on the data template.
  if(dataSetFilter!="expAll"){ # not feasible if no different projects
    
    # 2-way-ANOVA 
    df_long <- DF_prop_rank %>% melt(DF_prop_rank, id.vars= c("dataset", "project","project_dataset"),
                                     measure.vars = colnames(DF_prop_rank)[!colnames(DF_prop_rank) %in% c("dataset", "project","project_dataset")],
                                     variable.name ="test", value.name="value")
    
    if(dataSetFilter=="sim" || dataSetFilter=="simEquiv"){
      resAnova <- aov(value ~ test*project, data=df_long)
      res <- summary(resAnova)
      coefNr = 3
    }else{
      resAnova <- aov(value ~ test+project, data=df_long)
      res <- summary(resAnova)
      coefNr = 2
    }
    save(resAnova,file=paste0(outFolder,"H2_anova.RData"))
    
    aim2_results_tmp <- data.frame(
      prefix = prefix, test=paste(formula(resAnova)[c(2,1,3)],collapse=""), propTrue=NA, 
      outcome=paste0("p-value of ",rownames(res[[1]])[coefNr]),
      outcome_value = res[[1]][coefNr,5],
      threshold = 0.05,
      validated = res[[1]][coefNr,5]<0.05,
      hypoNumber = 2,
      hypothesis = "For FILTERED and UNFILTERED data, rankings of the DA methods with respect to the proportion of identified features depend on the data template.")
    aim2_results <- rbind(aim2_results, aim2_results_tmp)
    bs_validationOutput_print(aim2_results_tmp,res)
    #aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(?,data_to_compare, outDir=outFolder, aim2res = aim2_results))
  }
  
  ### Hypothesis 3: Rankings of the DA methods with respect to the proportion of identified features depend stronger on the data template in unfiltered data than in filtered data sets.
  print("Hypothesis 3 requires filtered and unfiltered data and is done elsewhere")
  
  ### Hypothesis 4: In unfiltered data, either limma voom TMMwsp, limma voom TMM, Wilcoxon, or LEfSe identify the largest proportion of significant features.
  H_true <- array(NA,dim=length(data_names))
  names(H_true) <- data_names
  
  tests <- intersect(c("limma_voom","limma_voom_TMMwsp","bs_wilcox.test","bs_lefse","EdgeR"),test_names)
  otherTests <- setdiff(test_names,tests)
  
  for(i in 1:length(data_names)){  # all
    cat(".") # one point per data_name
    data_name <- data_names[i]
    df_current <- DF_prop[DF_prop$project_dataset == data_name,]  # only result for current data_name
    
    # Begin of hypothesis-specific code
    if(sum(tests %in% names(df_current))>0)
      H_true[i] <- max(df_current[,tests],na.rm=T) > max(df_current[,otherTests],na.rm=T)
    else
      H_true[i] <- NA
  }
  cat("\n")
  
  aim2_results <- rbind(aim2_results,
                        bs_validationOutput(H_true, threshold=0.95, prefix=prefix, 
                                            hypoNr=4,
                                            hypothesis="In unfiltered data, either limma voom TMMwsp, limma voom TMM, Wilcoxon, or LEfSe identify the largest proportion of significant features.",
                                            test="limma_voom or limma_voom_TMMwsp or bs_wilcox.test or bs_lefse"))
  
  aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare, outDir=outFolder, aim2res = aim2_results,
                                                                           load_H_true_all=F))
  
  
  ### Hypothesis 5: For UNFILTERED data, there are data sets, where edgeR identifies the largest proportion of significant features.
  H_true <- array(NA,dim=length(data_names))
  names(H_true) <- data_names
  
  tests <- c("EdgeR")
  otherTests <- setdiff(test_names,tests)
  
  for(i in 1:length(data_names)){  # all
    cat(".") # one point per data_name
    data_name <- data_names[i]
    df_current <- DF_prop[DF_prop$project_dataset == data_name,]  # only result for current data_name
    
    # Begin of hypothesis-specific code
    H_true[i] <- max(df_current[,tests],na.rm=T) > max(df_current[,otherTests],na.rm=T)
  }
  cat("\n")
  
  aim2_results <- rbind(aim2_results,
                        bs_validationOutput(H_true, threshold=0, prefix=prefix, 
                                            hypoNr=5,
                                            hypothesis="For UNFILTERED data, there are data sets, where edgeR identifies the largest proportion of significant features.",
                                            test="EdgeR"))
  
  aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare, outDir=outFolder, aim2res = aim2_results))
  
  ### Hypothesis 6: For UNFILTERED data, Limma voom TMMwsp identifies the largest proportion of features in the Human-HIV (3) data set.
  H_true <- array(NA,dim=length(data_names))
  names(H_true) <- data_names
  
  tests <- c("limma_voom_TMMwsp")
  otherTests <- setdiff(test_names,tests)
  
  for(i in 1:length(data_names)){  # all
    cat(".") # one point per data_name
    data_name <- data_names[i]
    df_current <- DF_prop[DF_prop$project_dataset == data_name,]  # only result for current data_name
    
    # Begin of hypothesis-specific code
    H_true[i] <- max(df_current[,tests],na.rm=T) > max(df_current[,otherTests],na.rm=T)
  }
  cat("\n")
  
  H_true[!grepl("hiv_noguerajulian",rownames(H_true))] <- NA # other projects than requested
  
  
  aim2_results_tmp <- data.frame(
    prefix = prefix, test="Limma voom TMMwsp", propTrue=NA, 
    outcome= "estimated proportion",
    outcome_value = sum(H_true,na.rm=T) / sum(!is.na(H_true)),
    threshold = 0.95,
    validated = sum(H_true,na.rm=T) / sum(!is.na(H_true)) >= 0.95,
    hypoNumber = 6,
    hypothesis = "For UNFILTERED data, Limma voom TMMwsp identifies the largest proportion of features in the Human-HIV (3) data set.")
  aim2_results <- rbind(aim2_results, aim2_results_tmp)
  bs_validationOutput_print(aim2_results_tmp,paste0("No CI feasible for NI=",sum(!is.na(H_true)),": Only the proportion calculated."))
  
  
  ### Hypothesis 7: For UNFILTERED data, there are data sets, where both limma voom methods identify more than 99% of features as significant
  H_true <- matrix(nrow=length(data_names),ncol=2)
  rownames(H_true) <- data_names
  colnames(H_true) <- c("limma_voom","limma_voom_TMMwsp")
  
  for(i in 1:length(data_names)){  # all
    cat(".") # one point per data_name
    data_name <- data_names[i]
    df_current <- DF_prop[DF_prop$project_dataset == data_name,]  # only result for current data_name
    
    for(test in colnames(H_true)){
      if(test %in% colnames(df_current)){
        # Begin of hypothesis-specific code
        H_true[i,test] <- df_current[,test]>0.99
        # if(df_current[,test]<=0.99){
        #   cat("Secondary hypothesis 7 only proportion ",df_current[,test]," significant for ",test,"\n")
        # }
      }else{
        H_true[i,test] <- NA
      }
    }
  }
  H_true = as.array(H_true[,1] & H_true[,2]) # Hypothesis requires both
  cat("\n")
  
  aim2_results <- rbind(aim2_results,
                        bs_validationOutput(H_true, threshold=0, prefix=prefix, 
                                            hypoNr=7,
                                            hypothesis="For UNFILTERED data, there are data sets, where both limma voom methods identify more than 99% of features as significant",
                                            test="both limma voom methods"))
  
  aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare, outDir=outFolder, aim2res = aim2_results))
  
  
  ### Hypothesis 8: For UNFILTERED data, there are data sets, where Wilcoxon CLR identifies more than 90% of features as significant
  H_true <- array(dim=length(data_names))
  names(H_true) <- data_names
  
  test = "bs_wilcox.test"
  
  for(i in 1:length(data_names)){  # all
    cat(".") # one point per data_name
    data_name <- data_names[i]
    df_current <- DF_prop[DF_prop$project_dataset == data_name,]  # only result for current data_name
    
    # Begin of hypothesis-specific code
    H_true[i] <- df_current[,test]>0.9
  }
  cat("\n")
  
  aim2_results <- rbind(aim2_results,
                        bs_validationOutput(H_true, threshold=0, prefix=prefix, 
                                            hypoNr=8,
                                            hypothesis="For UNFILTERED data, there are data sets, where Wilcoxon CLR identifies more than 90% of features as significant",
                                            test="Wilcoxon CLR"))
  
  aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare, outDir=outFolder, aim2res = aim2_results))
  
  ### Hypothesis 9: For UNFILTERED data, there are data sets, where LEfSe identifies more taxa as significant compared with all other tools
  H_true <- array(dim=length(data_names))
  names(H_true) <- data_names
  
  test = "bs_lefse"
  otherTests <- setdiff(test_names,test)
  
  for(i in 1:length(data_names)){  # all
    cat(".") # one point per data_name
    data_name <- data_names[i]
    df_current <- DF_prop[DF_prop$project_dataset == data_name,]  # only result for current data_name
    
    # Begin of hypothesis-specific code
    if(test %in% names(df_current))
      H_true[i] <- df_current[,test]>max(df_current[,otherTests],na.rm=T)
    else
      H_true[i] <- NA
  }
  cat("\n")
  
  aim2_results <- rbind(aim2_results,
                        bs_validationOutput(H_true, threshold=0, prefix=prefix, 
                                            hypoNr=9,
                                            hypothesis="For UNFILTERED data, there are data sets, where LEfSe identifies more taxa as significant compared with all other tools.",
                                            test="bs_lefse"))
  
  aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare, outDir=outFolder, aim2res = aim2_results))
  
  ### Hypothesis 10: In UNFITLERED data, either ALDEx2 or ANCOM-II identify the fewest significant features.
  H_true <- array(NA,dim=length(data_names))
  names(H_true) <- data_names
  
  tests <- intersect(c("ALDEx2","ancombc"),colnames(DF_prop))
  otherTests <- setdiff(test_names,tests)
  
  for(i in 1:length(data_names)){  # all
    cat(".") # one point per data_name
    data_name <- data_names[i]
    df_current <- DF_prop[DF_prop$project_dataset == data_name,]  # only result for current data_name
    
    # Begin of hypothesis-specific code
    H_true[i] <- min(df_current[,tests],na.rm=T) <= min(df_current[,otherTests],na.rm=T)
  }
  cat("\n")
  
  aim2_results <-rbind(aim2_results,
                       bs_validationOutput(H_true, threshold=0.95, prefix=prefix, 
                                           hypoNr=10,
                                           hypothesis="In UNFITLERED data, either ALDEx2 or ANCOM-II identify the fewest significant features.",
                                           test="ALDEx2 or ANCOM-II"))
  
  aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare, outDir=outFolder, aim2res = aim2_results))
  
  ### Hypothesis 11 is now interpreted as the same like H10 but for filtered data:
  #
  ### Hypothesis 11: In filtered data,  ALDEx2, ANCOM-II do not identify significantly more features than the most conservative tests.
  # H_true <- matrix(nrow=length(data_names),ncol=2)
  # rownames(H_true) <- data_names
  # colnames(H_true) <- c("ALDEx2","ancombc")
  # 
  # tests <- intersect(colnames(H_true),colnames(DF_prop))
  # H_true <- H_true[,tests,drop=F]
  # otherTests <- setdiff(test_names,tests)
  # 
  # for(i in 1:length(data_names)){  # all
  #   cat(".") # one point per data_name
  #   data_name <- data_names[i]
  #   df_current <- DF_prop[DF_prop$project_dataset == data_name,]  # only result for current data_name
  #   
  #   # Begin of hypothesis-specific code
  #   for(test in colnames(H_true)){
  #     if(test %in% colnames(df_current)){
  #       raenge <- rank(df_current[,c(test,otherTests)])
  #       
  #       #  H_true[i,test] <- raenge[test] <= 3 # first entry is test
  #       # The following better accounts for ties:
  #       H_true[i,test] <- sum(raenge<raenge[test],na.rm=T) <= 3 # first entry is test
  #     }
  #   }
  # }
  # cat("\n")
  
  ## To keep the implementation simple, the results of H10 are also used for H11:
  
  aim2_results <- rbind(aim2_results,bs_validationOutput(H_true, threshold=0.95, prefix=prefix, 
                                                         hypoNr=11,
                                                         hypothesis="In filtered data, ALDEx2, ANCOM-II do not identify significantly more features than the most conservative tests.",
                                                         test="ALDEx2 or ANCOM-II"))
  
  aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare, outDir=outFolder, aim2res = aim2_results))
  
  ### Hypothesis 12: No tool (except ALDEx2) identifies a smaller number of features for filtered data.
  print("Hypothesis 12 requires filtered AND unfiltered data and is analyzed elsewhere.")
  
  ### Hypothesis 13: For FILTERED and UNFILTERED data, ANCOM-II identifies the least significant features in total, i.e. when summing ranks of DA methods over all 38 templates.
  dat_names <- unique(DF_prop$dataset) # here over $dataset and not over $project_dataset as for other hypotheses
  H_true <- array(NA,dim=length(dat_names))
  names(H_true) <- dat_names
  
  test <- "ancombc"
  
  
  for(i in 1:length(dat_names)){  # all
    cat(".") # one point per data_name
    dat_name <- dat_names[i]
    df_current <- DF_prop[DF_prop$dataset == dat_name,]  # only result for current data_name
    
    for(j in 1:nrow(df_current)) # loop over all projects to calculate ranks within each project:
      # Begin of hypothesis-specific code
      df_current[j,test_names] <- rank(df_current[j,test_names])
    
    if(test %in% colnames(df_current))
      H_true[i] <- sum(df_current[,test],na.rm=T) == min(colSums(df_current[,test_names]))
  }
  cat("\n")
  
  aim2_results_tmp <- data.frame(
    prefix = prefix, test="ancombc", propTrue=NA, 
    outcome= "estimated proportion",
    outcome_value = sum(H_true,na.rm=T) / sum(!is.na(H_true)),
    threshold = 0.95,
    validated = sum(H_true,na.rm=T) / sum(!is.na(H_true)) >= 0.95,
    hypoNumber = 13,
    hypothesis = "For FILTERED and UNFILTERED data, ANCOM-II identifies the least significant features in total, i.e. when summing ranks of DA methods over all 38 templates.")
  aim2_results <- rbind(aim2_results, aim2_results_tmp)
  bs_validationOutput_print(aim2_results_tmp,paste0("No CI feasible for NI=",sum(!is.na(H_true)),": Only the proportion calculated."))
  
  
  ### Hypothesis 14: there is no method other than EdgeR, LEfSe, limma voom TMMwsp, limma voom TMM, or Wilcoxon CLR that identifies the largest number of significant features in total, i.e. when considering ranks of DA methods over all 38 templates.
  tests <- intersect(c("EdgeR","bs_lefse","limma_voom","limma_voom_TMMwsp","bs_wilcox.test"),test_names)
  otherTests <- setdiff(test_names,tests)
  
  dat_names <- unique(DF_prop$dataset) # here over $dataset and not over $project_dataset as for other hypotheses
  H_true <- matrix(nrow=length(dat_names),ncol=length(tests))
  colnames(H_true) <- tests
  rownames(H_true) <- dat_names
  
  for(i in 1:length(dat_names)){  # all
    cat(".") # one point per data_name
    dat_name <- dat_names[i]
    df_current <- DF_prop[DF_prop$dataset == dat_name,]  # only result for current data_name
    for(test in colnames(H_true)){
      for(j in 1:nrow(df_current)) # loop over all projects to calculate ranks within each project:
        # Begin of hypothesis-specific code
        df_current[j,test_names] <- rank(df_current[j,test_names])
      
      if(test %in% names(df_current))
        H_true[i,test] <- sum(df_current[,test],na.rm=T) >= max(colSums(df_current[,otherTests],na.rm=T),na.rm=T)
      else
        H_true[i,test] <- NA
    }
  }
  cat("\n")
  
  for(test in tests){
    
    aim2_results_tmp <- data.frame(
      prefix = prefix, test=test, propTrue=NA, 
      outcome= "estimated proportion",
      outcome_value = sum(H_true[,test],na.rm=T) / sum(!is.na(H_true[,test])),
      threshold = 0.95,
      validated = sum(H_true[,test],na.rm=T) / sum(!is.na(H_true[,test])) >= 0.95,
      hypoNumber = 14,
      hypothesis = paste0(test,": For FILTERED data, there is no method other than EdgeR, LEfSe, limma voom TMMwsp, limma voom TMM, or Wilcoxon CLR that identifies the largest number of significant features in total, i.e. when considering ranks of DA methods over all 38 templates."))
    aim2_results <- rbind(aim2_results, aim2_results_tmp)
    
    bs_validationOutput_print(aim2_results_tmp,paste0("No CI feasible for NI=",sum(!is.na(H_true)),": Only the proportion calculated."))
  }
  
  ### Save result
  saveRDS(aim2_results,paste0(outFolder,"/","aim2_results.RDS"))
  saveRDS(aim2_regression,paste0(outFolder,"/","aim2_regression.RDS"))
  
  ##################
  cat("\n\nscript_5.x_Aim2_secondary_filtered_or_unfiltered.R finished :-)\n\n")
}