# Script to make PCA plots of data properties

################################
# Set correct working directory
sys_info <- Sys.info()

if(sys_info[["sysname"]]=="Darwin" && sys_info[["login"]]=="root"){
  setwd("/Users/evakohnert/Documents/PhD/Microbiome/Benchmark/BenchmarkStudy_MicrobiomeSyntheticData/R_scripts")
}
if(sys_info[["sysname"]]=="Windows" && grep("kreutz",sys_info[["login"]],T)){
  setwd("E:/clemens/Repositories/BenchmarkStudy_MicrobiomeSyntheticData/R_scripts")
}
if(sys_info[["sysname"]]=="Linux" && sys_info[["login"]]=="kohnert"){
  setwd("/h/kohnert/BenchmarkStudy_MicrobiomeSyntheticData/R_scripts")
}
if(sys_info[["sysname"]]=="Linux" && sys_info[["login"]]=="ckreutz"){
  if(sys_info[["nodename"]]=="imbip-compute-214")
    setwd("~/BenchmarkStudy_MicrobiomeSyntheticData_25Mar24/R_scripts")
  else
    setwd("~/BenchmarkStudy_MicrobiomeSyntheticData/R_scripts")
}


# Initialization, i.e. sourcing R-functions, loading packages and setting project_path
source("project_init.R")
# project_init()
###############################

folders <- c("../Results/3.3_metaSPARSim_Filtered_DataProps","../Results/3.3_sparseDOSSA_Filtered_DataProps")

for(folder in folders){
  
  try(dir.create(folder))
  
  DF <- readRDS(paste0(folder, "/DF.RDS"))
  DF_summary <- readRDS(paste0(folder, "/DF_summary.RDS"))
  
  DF$comparison <- gsub("metaSPARSim","mSS",DF$comparison)
  DF$comparison <- gsub("sparseDOSSA2","spD",DF$comparison)
  DF$comparison <- gsub("original","orig",DF$comparison)

  # Add template vs template
  DF_TvsT <- bs_diffDataProp_templates(data_to_compare)
  DF_TvsT_summary <- bs_summarize_dataPropCmp_templates(DF_TvsT)
  
  # Merge comparison data frames 
  DF <- rbind(DF[,!colnames(DF)=="comparison2"],DF_TvsT_summary)
  saveRDS(DF,file = paste0(folder, "/DF.RDS"))
  
  ## Plot boxplots comparing data properties 
  png(filename = paste0(folder, "/dataPropCmp.png"),width = 1800,height=1200)
  print(bs_plot_dataPropCmp(DF,scales="free_y"))
  dev.off()
  
  pdf(file = paste0(folder, "/dataPropCmp.pdf"),width=14,height=15)
  print(bs_plot_dataPropCmp(DF,scales="free_y"))
  dev.off()
  
  
  
  # Plot PCA and biplot
  PCA <- bs_PCA_dataProp_sim(DF_summary, type="PCA")
  ggsave(PCA, file = paste0(folder, "/PCA.pdf"), height=10, width=15)
  
  PCA_biplot <- bs_PCA_dataProp_sim(DF_summary, type="Biplot")
  ggsave(PCA_biplot, file=paste0(folder, "/PCA_biplot.pdf"), height=10, width=15)
  
  
}