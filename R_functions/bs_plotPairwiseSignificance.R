# bs_plotPairwiseSignificance 
#
# Plotting the overlap of the significant outcomes for pairs of tests
# 
# Examples:
# source("E:/clemens/Repositories/BenchmarkStudy_MicrobiomeSyntheticData/R_functions/bs_plotPairwiseSignificance.R")
#
# myplot<-bs_plotPairwiseSignificance(DF_significance,maxSig = 6)
# pdf("bs_plotPairwiseSignificance_maxSig6.pdf",width=15,height=15)
# myplot
# dev.off()
#
# myplot<-bs_plotPairwiseSignificance(DF_significance,anzSig = 2)
# pdf("bs_plotPairwiseSignificance_anzSig2.pdf",width=15,height=15)
# myplot
# dev.off()
#
# myplot<-bs_plotPairwiseSignificance(DF_significance,minSig = 4)
# pdf("bs_plotPairwiseSignificance_minSig4.pdf",width=15,height=15)
# myplot
# dev.off()
#
# tmp <- DF_significance
# tmp$project_dataset <- "all"
# myplot<-bs_plotPairwiseSignificance(tmp,minSig = 4)
# pdf("bs_plotPairwiseSignificance_minSig4_all.pdf",width=15,height=15)
# myplot
# dev.off()



bs_plotPairwiseSignificance <- function(DF_significance, minSig=NULL, maxSig=NULL, anzSig=NULL){

  test_cols <- which(sapply(DF_significance, is.logical)) # indices
  test_names <- names(DF_significance)[test_cols] 
  data_names <- unique(DF_significance$project_dataset) 
  project_names <- unique(DF_significance$project) 
  df_pairwise <- NULL
  
  for(i in 1:length(data_names)){  # all
    cat(".") # one point per data_name
    data_name <- data_names[i]
    df_current <- DF_significance[DF_significance$project_dataset == data_name,]  # only result for current data_name
    
    # Filter w.r.t. number of signifikant tests
    if(!is.null(minSig))
      df_current <- df_current[rowSums(df_current[,test_names],na.rm=T)>=minSig,] # only rows with exactly two significant results play a role (using only those makes code faster)
    if(!is.null(maxSig))
      df_current <- df_current[rowSums(df_current[,test_names],na.rm=T)<=maxSig,] # only rows with exactly two significant results play a role (using only those makes code faster)
    if(!is.null(anzSig))
      df_current <- df_current[rowSums(df_current[,test_names],na.rm=T)==anzSig,] # only rows with exactly two significant results play a role (using only those makes code faster)
    
    # Now loop over all test combis:
    for(i1 in 1:length(test_names)){
      test1 <- test_names[i1]
      for(test2 in test_names){

        overlap = sum( df_current[,test1] & df_current[,test2] ,na.rm=T)
        overlapRelative = overlap / sum( df_current[,test1],na.rm=T)
        if(is.null(df_pairwise))
          df_pairwise <- data.frame(data_name=data_name,test1=test1, test2=test2, overlap=overlap, overlapRelative=overlapRelative) # first row
        else
          df_pairwise <- rbind(df_pairwise, data.frame(data_name=data_name,test1=test1, test2=test2, overlap=overlap, overlapRelative=overlapRelative)) # add one row

      }
    }
  }
  
  df_same <- df_pairwise[df_pairwise$test1==df_pairwise$test2,]
  df_notSame <- df_pairwise[df_pairwise$test1!=df_pairwise$test2,]
  
  df_notSame <- df_notSame %>%
    group_by(data_name) %>%
    mutate(max_value = overlap == max(overlap,na.rm=T)) %>%
    ungroup()
  
  df_same <- data.frame(df_same,max_value=array(F,dim = nrow(df_same)))
  df_pairwise <- rbind(df_notSame,df_same)
  
  df_pairwise$max_value[df_pairwise$overlap<1] <- FALSE
  
  cat("\n")
  titelstring <- "Absolute & relative overlap for two test combinations"
  if(!is.null(minSig))
    titelstring <- paste0(titelstring," (max ",maxSig," significant)")
  if(!is.null(anzSig))
    titelstring <- paste0(titelstring," (",anzSig," significant)")
  if(!is.null(minSig))
    titelstring <- paste0(titelstring," (min ",minSig," significant)")

  if(length(unique(df_pairwise$data_name))>4)
    fs <- 1
  else
    fs <- 3
    
  myplot <- ggplot(df_pairwise, aes(x = test2, y = test1, fill = overlapRelative)) +
    geom_tile(color = "white") +
    geom_text(aes(label = overlap), color = "black", size = fs) +
    geom_text(data = subset(df_pairwise, max_value), aes(label = "O"), color = "red", size = 6, vjust = 0.5) +
    scale_fill_gradient(low = "white", high = "darkred") +
    facet_wrap(~ data_name) +
    labs(title = titelstring,
         x = "Test 2", y = "Test 1") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.subtitle = element_text(size = 6),  # Subtitle font size
          strip.text = element_text(size = 6))
 
  return(myplot) 
}