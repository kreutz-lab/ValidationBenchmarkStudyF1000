# This script generates OverlapHist plots for:
# a) after aggregating over all datasets
# b) for each data set individually

source("project_init.R")

folders <- c("../Results/5.2_Aim2_primary_sparseDOSSA_ancom_Nearing_simEquiv",
             "../Results/5.4_Aim2_primary_sparseDOSSA_ancom_Nearing_simEquiv",
             "../Results_partReg/5.2_Aim2_primary_metaSPARSim_ancom_Nearing_simEquiv",
             "../Results_partReg/5.4_Aim2_primary_metaSPARSim_ancom_Nearing_simEquiv")

#folder <- folders[2]

for(folder in folders){
  rm(df)
  load(paste0(folder,"/OverlapHist.Rdata"))
  
  # aggregated over all datasets:
  dfall <- df
  dfall$feature <- paste(df$feature,dfall$dataset,sep="_")
  dfall$dataset <- sub("_\\d+$", "_all",dfall$dataset)
  
  plots <- bs_DA_plotFunctions(dfall,"OverlapHist",folder=folder) 
  pdf(file=paste0(folder,"/OverlapHist_all.pdf"),width=8,height=16)
  print(plots)
  dev.off()

  # 1 plot per dataset:
  dfall <- df
  dfall$feature <- paste(df$feature,dfall$dataset,sep="_")
  dfall$dataset <- paste(df$project,dfall$dataset,sep="_")
  
  plots <- bs_DA_plotFunctions(dfall,"OverlapHist",folder=folder) 
  pdf(file=paste0(folder,"/OverlapHist_individual.pdf"),width=8,height=16)
  print(plots)
  dev.off()
  
  # 1 plot per project:
  dfall <- df
  dfall$feature <- paste(df$feature,dfall$dataset,sep="_")
  dfall$project <- df$dataset
  dfall$dataset <- df$project
  
  plots <- bs_DA_plotFunctions(dfall,"OverlapHist",folder=folder) 
  pdf(file=paste0(folder,"/OverlapHist_individualProjects.pdf"),width=8,height=16)
  print(plots)
  dev.off()
}

