# Example:
# data_to_compare <- bs_DA(data_to_compare)
# out <- bs_DA2ROC(data_to_compare)
bs_DA2ROC <- function(data_to_compare){
  data_to_compare <- bs_addTruth(data_to_compare)
  DF <- bs_DA2dataFrame(data_to_compare,addTruth = T)

  # create and save DF with test results
  DF2 <- DF %>%
    group_by(test, dataset, project) %>%
    mutate(p_adjusted_CK = p.adjust(p_value, method = "fdr")) %>%
    ungroup()
  
  # replace missing padj by padj_ck:
  DF2[is.na(DF2$p_adjusted),"p_adjusted"] <- DF2[is.na(DF2$p_adjusted),"p_adjusted_CK"]
  # Add information about significance
  DF2 <- data.frame(DF2,significant=DF2$p_adjusted<=0.05)
  
  
  DF_filt <- DF2[!is.na(DF2$p_value),]
  # If there are duplicates (because analyses have been merged) remove them
  DF_filt$ID <- paste0(DF_filt$dataset,DF_filt$test, DF_filt$feature)
  DF_filt <- DF_filt %>% distinct(ID, .keep_all = TRUE)
  # What percentage of features remain?
  (nrow(DF_filt)/nrow(DF))*100
  
  
  DF_significance <- pivot_wider(DF_filt, names_from =test, values_from=significant,id_cols = c("feature","dataset","project"))
  
  # Spalte zur Selektion der ca. 380 Datensätze
  DF_significance <- data.frame(DF_significance,
                                project_dataset = paste(DF_significance$project, DF_significance$dataset, sep="_")) 
 
  out <- bs_DA_plotFunctions(DF, useNearingNames=F,plotType="ROC") 
  
  return(out)
}

