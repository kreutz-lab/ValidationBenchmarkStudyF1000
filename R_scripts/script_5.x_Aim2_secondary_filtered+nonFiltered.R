# Script to analyze the results from differential abundance tests on filtered and unfiltered data
# A subset of the 13 conclusions from Box1 are tested

cat("\n\n script_5.x_Aim2_primary_filtered+nonFiltered.R started ... \n\n")

if(!dir.exists(dataFolder) || !dir.exists(dfFolder)){
  warning(dataFolder, " or ", dfFolder, "does not exist !!!!!!!!!!!!")
  
}else{
  
  try(file.remove(paste0(outFolder,"/H_true_all.RDS"))) # to be sure that nothing accumulates
  try(file.remove(paste0(outFolder,"/df_H_true_all.RDS"))) # to be sure that nothing accumulates

  prefix <- bs_processResultFoldernames(outFolder,"dataName")
  
  print(paste0("prefix=",prefix))
  print(paste0("dataFolder=",dataFolder))
  print(paste0("dfFolder=",dfFolder))
  print(paste0("dfFolderFiltered=",dfFolderFiltered))
  print(paste0("outFolder=",outFolder))
  print(paste0("dataSetFilter=",dataSetFilter))
  
  
  ###############################
  
  
  try(dir.create(outFolder))
  save(dataFolder,dfFolder,outFolder,dataSetFilter,dfFolderFiltered,prefix,file=paste0(outFolder,"/folders.Rdata"))
  
  data_to_compare <- readRDS(paste0(dataFolder,"/data_to_compare.RDS"))
  
  ############### Reading data.frames
  #DF_filt <-  readRDS(paste0(dfFolderFiltered,"/DF_filt.RDS"))
  DF_significance <- readRDS(paste0(dfFolder,"/DF_significance.RDS"))
  DF_significance_filt <- readRDS(paste0(dfFolderFiltered,"/DF_significance.RDS"))

  ############################################
  ## Now filter according to exp or simu #####
  DF_significance <- bs_apply_dataSetFilter(DF_significance,dataSetFilter,dfFolder=dfFolder)
  DF_significance_filt <- bs_apply_dataSetFilter(DF_significance_filt,dataSetFilter,dfFolder=dfFolder)
  
  ## End filter according to exp or simu #####
  ############################################
  
  
  test_cols <- which(sapply(DF_significance, is.logical)) # indices
  test_cols_filt <- which(sapply(DF_significance, is.logical)) # indices
  test_names <- intersect(names(DF_significance_filt)[test_cols_filt], names(DF_significance)[test_cols] )
  data_names <- unique(DF_significance$project_dataset) 
  data_names_filt <- unique(DF_significance_filt$project_dataset) 
  data_names.keep <- intersect(data_names,data_names_filt)
  
  if(length(data_names.keep)<1)
    cat("length(data_names.keep)<1, is this an error?\n")
  
  ########################
  # List to store results
  aim2_results <- list()
  aim2_regression <- list()
  
  ######################## 
  # Hypotheses:
  
  ### Hypothesis 3: Wie H2, aber man muss schauen ob Interaktionsterme in unfiltered > filtered (zählen)
  if(dataSetFilter!="expAll"){ # not feasible if no different projects
    # 2-way-ANOVA unfiltered
    DF_prop_rank <- readRDS(file=paste0(dfFolder,"/","DF_prop_rank.RDS"))
    df_long <- DF_prop_rank %>% melt(DF_prop_rank, id.vars= c("dataset", "project","project_dataset"),
                                     measure.vars = colnames(DF_prop_rank)[!colnames(DF_prop_rank) %in% c("dataset", "project","project_dataset")],
                                     variable.name ="test", value.name="value")
    if(dataSetFilter=="sim" || dataSetFilter=="simEquiv"){
      resAnova <- aov(value ~ test+test:project, data=df_long)
      res <- summary(resAnova)
      coefNr = 2
    }else{
      resAnova <- aov(value ~ test+project, data=df_long)
      res <- summary(resAnova)
      coefNr = 2
    }
    resAnovaUnfilt <- resAnova
    MsUnFilt <- res[[1]][coefNr,"Mean Sq"]
    
    # 2-way-ANOVA filtered
    DF_prop_rank_filt <- readRDS(file=paste0(dfFolderFiltered,"/","DF_prop_rank.RDS"))
    df_long <- DF_prop_rank_filt %>% melt(DF_prop_rank_filt, id.vars= c("dataset", "project","project_dataset"),
                                     measure.vars = colnames(DF_prop_rank)[!colnames(DF_prop_rank_filt) %in% c("dataset", "project","project_dataset")],
                                     variable.name ="test", value.name="value")
    if(dataSetFilter=="sim" || dataSetFilter=="simEquiv"){
      resAnova <- aov(value ~ test+test:project, data=df_long)
      res <- summary(resAnova)
      coefNr = 2
    }else{
      resAnova <- aov(value ~ test+project, data=df_long)
      res <- summary(resAnova)
      coefNr = 2
    }
    resAnovaFilt <- resAnova
    MsFilt <- res[[1]][coefNr,"Mean Sq"]
    
    save(resAnovaFilt,resAnovaUnfilt,file=paste0(outFolder,"/H3_ANOVA.Rdata"))

    aim2_results_tmp <- data.frame(
      prefix = prefix, test=paste(formula(resAnova)[c(2,1,3)],collapse=""), propTrue=NA, 
      outcome=paste0("Comparing interaction Mean-Squares ",rownames(res[[1]])[coefNr],": unfiltered - filtered"),
      outcome_value = MsUnFilt-MsFilt,
      threshold = 0,
      validated = MsUnFilt-MsFilt>0,
      hypoNumber = 3,
      hypothesis = "Rankings of the DA methods with respect to the proportion of identified features depend stronger on the data template in unfiltered data than in filtered data sets.")
    aim2_results <- rbind(aim2_results, aim2_results_tmp)
    bs_validationOutput_print(aim2_results_tmp,res)
    #aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(?,data_to_compare, outDir=outFolder, aim2res = aim2_results))
  }
  
  
  
  ### Hypothesis 12: No tool (except ALDEx2) identifies a smaller number of features for unfiltered data.
  tests <- setdiff(test_names,"ALDEx2")
  
  H_true <- array(data=TRUE,dim=length(data_names.keep))
  names(H_true) <- data_names.keep
  H_trueNA <- array(data=TRUE,dim=length(data_names.keep)) # count NAs as significant
  names(H_trueNA) <- data_names.keep
  #colnames(H_true) <- tests
  
  
  for(i in 1:length(data_names.keep)){  # all
    cat(".") # one point per data_name
    data_name <- data_names.keep[i]
    df_current_unfiltered <- DF_significance[DF_significance$project_dataset == data_name,]  # only result for current data_name
    df_current_filtered <- DF_significance_filt[DF_significance_filt$project_dataset == data_name,] 

    # Is number in filtered data larger compared to unfiltered for all tests? Then leave it TRUE, otherwise FALSE
    for(test in tests){
      aTestFound <- F
      if(test %in% names(df_current_unfiltered) && test %in% names(df_current_filtered)
         && nrow(df_current_unfiltered)>0 && nrow(df_current_filtered)>0){
        if(sum(df_current_unfiltered[,test],na.rm=T) < sum(df_current_filtered[,test],na.rm=T) 
           && sum(df_current_filtered[,test],na.rm=T)>10 && sum(!is.na(df_current_unfiltered[,test]),na.rm=T)>10){
          aTestFound <- T
          H_true[i] <- FALSE
        }
        if((sum(df_current_unfiltered[,test],na.rm=T) < sum(df_current_filtered[,test] | is.na(df_current_filtered[,test]),na.rm=T))
           && (sum(df_current_filtered[,test],na.rm=T)>10  && sum(!is.na(df_current_unfiltered[,test]),na.rm=T)>10)){
          H_trueNA[i] <- FALSE
        }
      }
      if(!aTestFound){
        H_true[i] <- NA
        H_trueNA[i] <- NA
      }
    }
  }
  cat("\n")
  cat("H12: sum(H_true)=",sum(H_true,na.rm=T),"\n")
  cat("H12: sum(!H_true)=",sum(!H_true,na.rm=T),"\n")
  cat("H12: sum(H_trueNA)=",sum(H_trueNA,na.rm=T),"\n")
  cat("H12: sum(!H_trueNA)=",sum(!H_trueNA,na.rm=T),"\n")
  
  
  aim2_results <- rbind(aim2_results,
                        bs_validationOutput(H_true, threshold=0.95, prefix=prefix, 
                                            hypoNr=12,
                                            hypothesis="No tool (except ALDEx2) identifies a smaller number of features for unfiltered data (than for unfiltered).",
                                            test="ALDEx2"))
  
  aim2_regression <- append(aim2_regression, bs_PropsAssociationsWithHtrue(H_true,data_to_compare, outDir=outFolder, aim2res = aim2_results,
                                                                           load_H_true_all=F))
  
  ### Save result
  saveRDS(aim2_results,paste0(outFolder,"/","aim2_results.RDS"))
  saveRDS(aim2_regression,paste0(outFolder,"/","aim2_regression.RDS"))
  
  ##################
  cat("\n\n script_5.x_Aim2_primary_filtered+nonFiltered.R finished :-)\n\n")
  
}