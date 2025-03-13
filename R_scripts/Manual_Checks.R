source("project_init.R")

folders <- c("../Results/4.2_sparseDOSSA_ZerosAdded_DA_ancom_Nearing",
             "../Results_partReg/4.2_metaSPARSim_ZerosAdded_DA_ancom_Nearing",
             "../Results/4.4_sparseDOSSA_filtered_ZerosAdded_DA_ancom_Nearing",
             "../Results_partReg/4.4_metaSPARSim_filtered_ZerosAdded_DA_ancom_Nearing/")

summary <- bs_summarize_all(pattern= "data_to_compare.RDS$",outfile="../bs_summarize_used.xlsx",folder=folders[4])

d <- readRDS("../Results_partReg/4.4_metaSPARSim_filtered_ZerosAdded_DA_ancom_Nearing/data_to_compare.all.RDS")
d <- bs_subset(d,which(names(d)=="ibd_papa"))

d <- bs_decompress(d)

#whichMethods = c("Aldex2","ancom", "corncob","DEseq","edgeR","lefse","limma_voom","limma_voom_tmmwsp","Maaslin2", "Maaslin2_rare","metagenomeSeq", "ttest_rare","wilcox","wilcox_rare" )
whichMethods = c("Aldex2","metagenomeSeq" )

d2 <- bs_DA(d,whichMethods = whichMethods, whichData = c("data_template"))
