setwd("E:/clemens/Repositories/BenchmarkStudy_MicrobiomeSyntheticData/R_scripts")

#load("../Results/3.4_metaSPARSim_filtered_ZerosAdded_DataProps/dataPropCmp.Rdata")
load("../Results/3.2_metaSPARSim_ZerosAdded_DataProps/dataPropCmp.Rdata")
#load("../Results/3.1_metaSPARSim_DataProps/dataPropCmp.Rdata")

#source("../../R_scripts/project_init.R")
source("project_init.R")

source("E:/clemens/Repositories/BenchmarkStudy_MicrobiomeSyntheticData/R_functions/bs_plot_dataPropCmp_ck.R")
#DFplot <- DF[DF$data_prop %in% c("P0","q95","mean_inverseSimpson","bimodality_corr_sample"),]


#DFplot <- DF[DF$data_prop %in% c("bimodality_corr_feature","bimodality_corr_sample"),]
#DFplot <- DF[DF$data_prop %in% c("P0"),]
DFplot$data_template <- DFplot$data_project
myplot <- bs_plot_dataPropCmp_ck(DFplot,scales="free_y", ncol=1)
print(myplot)

pdf(file = paste0("test.pdf"),width=10,height=11)
print(myplot)
dev.off()


dataFolder <- "../Results/3.1_metaSPARSim_DataProps/"
DF_summary <- readRDS(paste0(dataFolder,"/DF_summary.RDS"))
bs_PCA_dataProp_sim(DF_summary, type="PCA", folder=dataFolder) 
