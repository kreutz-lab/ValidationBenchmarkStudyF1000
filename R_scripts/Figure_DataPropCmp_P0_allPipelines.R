setwd("E:/clemens/Repositories/BenchmarkStudy_MicrobiomeSyntheticData/R_scripts")
source("project_init.R")
#source("E:/clemens/Repositories/BenchmarkStudy_MicrobiomeSyntheticData/R_functions/bs_plot_dataPropCmp_ck.R")

# folders <- c("../Results/3.1_metaSPARSim_DataProps/",
#              "../Results/3.2_metaSPARSim_ZerosAdded_DataProps/",
#              "../Results_partReg/3.1_metaSPARSim_DataProps/",
#              "../Results_partReg/3.2_metaSPARSim_ZerosAdded_DataProps/",
#              "../Results/3.1_sparseDOSSA_DataProps/",
#              "../Results/3.2_sparseDOSSA_ZerosAdded_DataProps/",
#              "../Results_partReg/3.1_sparseDOSSA_DataProps/",
#              "../Results_partReg/3.2_sparseDOSSA_ZerosAdded_DataProps/")

folders <- c("../Results/3.1_metaSPARSim_DataProps/",
             "../Results/3.2_metaSPARSim_ZerosAdded_DataProps/",
             "../Results_partReg/3.2_metaSPARSim_ZerosAdded_DataProps/")

names(folders) <- "metaSPARSim Pipeline 1"
DFs <- NULL
for(folder in folders){
  cat(folder,"...\n")
  load(paste0(folder,"dataPropCmp.Rdata")) # load DF
  DF[,"comparison"] <- "template_vs_template"
  for(pattern in c("metaSPARSim","sparseD")){
    try(DF[grepl(pattern,DF$dataSet1) & !grepl(pattern,DF$dataSet2),"comparison"] <- "template - sim")
    try(DF[grepl(pattern,DF$dataSet2) & !grepl(pattern,DF$dataSet1),"comparison"] <- "template - sim")
    try(DF[grepl(pattern,DF$dataSet1) & grepl(pattern,DF$dataSet2),"comparison"] <- "sim_vs_sim")
  }
  
  name <- folder
  name <- sub("../Results/3.","in Pipeline ",name,fixed=T)
  name <- sub("../Results_partReg/3.","in Pipeline ",name,fixed=T)
  name <- sub("_DataProps/","",name,fixed=T)
  name <- sub("_ZerosAdded"," (sparsitiy adjusted)",name,fixed=T)
  name <- sub("_"," ",name,fixed=T)
  if(grepl("partReg",folder)){
    name <- paste0(name,"(partial regulation)")    
    name <- sub("(sparsitiy adjusted)(partial regulation)","(sparsity & regulation adjusted) ",name,fixed=T)
    name <- sub("Pipeline 1","Pipeline 3",name,fixed=T)
    name <- sub("Pipeline 2","Pipeline 4",name,fixed=T)
  }
  
  name <- sub("metaSPARSim ","",name)

  #DFplot <- DF[DF$data_prop %in% c("P0","q95","mean_inverseSimpson","bimodality_corr_sample"),]
  #DFplot <- DF[DF$data_prop %in% c("bimodality_corr_feature","bimodality_corr_sample"),]
  DFplot <- DF[DF$data_prop %in% c("P0","Permanova_R2"),]
  
  namen <- array(name,dim=nrow(DFplot))
  DFplot[,"data_prop"] <- paste(DFplot[,"data_prop"],namen,sep=" ")
  DFplot[,"data_prop"] <- sub("Permanova_","",DFplot[,"data_prop"])
  
  if(is.null(DFs))
    DFs <- DFplot
  else
    DFs <- rbind(DFs,DFplot)
}


DFs$data_template <- DFs$data_project
myplot <- bs_plot_dataPropCmp_ck(DFs,scales="free_y", ncol=3)
print(myplot)

pdf(file = paste0("Figure_P0_allPipelines.pdf"),width=17,height=10.2)
print(myplot)
dev.off()
