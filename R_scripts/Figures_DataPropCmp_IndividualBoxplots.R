#setwd("E:/clemens/Repositories/BenchmarkStudy_MicrobiomeSyntheticData/R_scripts")
source("project_init.R")

# folders <- c("../Results/3.2_metaSPARSim_ZerosAdded_DataProps",
#              "../Results_partReg/3.2_sparseDOSSA_ZerosAdded_DataProps",
#              "../Results/3.2_sparseDOSSA_ZerosAdded_DataProps",
#              "../Results_partReg/3.2_metaSPARSim_ZerosAdded_DataProps")

folders <- list.dirs("../Results", full.names = TRUE, recursive = FALSE)
folders <- c(folders,list.dirs("../Results_partReg", full.names = TRUE, recursive = FALSE))
folders <- folders[grepl("^3\\.", basename(folders))]

#folders <- folders[1]

for(i in 1:5)
  try(dev.off())

for(Folder in folders){
  
  load(paste0(Folder,"/dataPropCmp.Rdata"))
  
  ## be sure that this fits:
  DF[,"comparison"] <- "template_vs_template"
  for(pattern in c("metaSPARSim","sparseD")){
    try(DF[grepl(pattern,DF$dataSet1) & !grepl(pattern,DF$dataSet2),"comparison"] <- "template - sim")
    try(DF[grepl(pattern,DF$dataSet2) & !grepl(pattern,DF$dataSet1),"comparison"] <- "template - sim")
    try(DF[grepl(pattern,DF$dataSet1) & grepl(pattern,DF$dataSet2),"comparison"] <- "sim_vs_sim")
  }
  
  #DFplot <- DF[DF$data_prop %in% c("P0","q95","mean_inverseSimpson","bimodality_corr_sample","nsamplesWithCounts"),]
  
  DCs <- setdiff(setdiff(unique(DF$data_prop),readRDS("../Data/RedundantDataProps.RDS")),c("median","P0_sample","P0_feature","sample_means","nsamplesWithCounts"))
  DF <- DF[DF$data_prop %in% DCs,]
  pdf(file = paste0(Folder,"/Boxplots_Differences_DCs_1page.pdf"),width=12,height=16)
  myplotAll <- bs_plot_dataPropCmp(DF,scales="free_y", ncol=4,cmp_type=c("log2ratio"))
  print(myplotAll)
  dev.off()
  
  pdf(file = paste0(Folder,"/Boxplots_Differences_DCs.pdf"),width=10,height=11)
  #for(i in seq(1,(length(DCs)-2),by=2)){
  for(i in 1:length(DCs)){
    
    #    if(i<length(DCs))
    #      DFplot <- DF[DF$data_prop %in% c(DCs[i],DCs[i+1]) & DF$cmp_type=="diff",]
    #    else
    DFplot <- DF[DF$data_prop %in% c(DCs[i]) & DF$cmp_type=="diff",]
    cat(i,": ",DCs[i],", nrow=",nrow(DFplot),"\n")
    if(nrow(DFplot)>10){
      DFplot2 <- DF[DF$data_prop %in% c(DCs[i]) & DF$cmp_type=="log2ratio",]
      if(nrow(DFplot2)>10){
        DFplot2$data_prop <- paste("log2_",DFplot2$data_prop,sep="")
        DFplot <- rbind(DFplot,DFplot2)
      }
      
      DFplot$data_template <- DFplot$data_project
      DFplot <- DFplot[!is.na(DFplot$difference),]
      
      myplot <- NULL
      try(myplot <- bs_plot_dataPropCmp_ck(DFplot,scales="free_y", ncol=1,cmp_type=c("diff","log2ratio")))
      if(!is.null(myplot))
        print(myplot)
    }
  }
  dev.off()
  
  # Only template - sim:
  DF_tmp <- DF[DF$comparison=="template - sim" & DF$cmp_type=="diff",]
  if(nrow(DF_tmp)>10){
    DF_median_diff <- DF_tmp %>%
      group_by(data_project, data_prop) %>%  # Group by the two columns
      summarise(median_difference = median(difference, na.rm = TRUE)) %>%  # Calculate median
      ungroup()  # Remove grouping
    DF_median_diff
    
    DF_tmp <- DF[DF$comparison=="template - sim" & DF$cmp_type=="log2ratio",]
    DF_median_l2r <- DF_tmp %>%
      group_by(data_project, data_prop) %>%  # Group by the two columns
      summarise(median_difference = median(difference, na.rm = TRUE)) %>%  # Calculate median
      ungroup()  # Remove grouping
    DF_median_l2r
  }
}

## PCA

#dataFolder <- "../Results/3.1_metaSPARSim_DataProps/"
#DF_summary <- readRDS(paste0(dataFolder,"/DF_summary.RDS"))
#bs_PCA_dataProp_sim(DF_summary, type="PCA", Folder=dataFolder) 
