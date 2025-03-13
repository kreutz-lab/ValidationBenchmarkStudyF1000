#source("project_init.R")
try(rm(dfFolderFiltered))
try(rm(DF_significance_filt))


load("../Results/5.2+5.4_Aim2_secondary_sparseDOSSA_ancom_Nearing_simEquiv/folders.Rdata")
#load("../Results_partReg/5.2+5.4_Aim2_secondary_metaSPARSim_ancom_Nearing_simEquiv/folders.Rdata")
#load("../Results_partReg/5.4_Aim2_secondary_metaSPARSim_ancom_Nearing_simEquiv/folders.Rdata")


prefix <- bs_processResultFoldernames(outFolder,"dataName")
print(paste0("prefix=",prefix))
print(paste0("dataFolder=",dataFolder))
print(paste0("dfFolder=",dfFolder))
try(print(paste0("dfFolderFiltered=",dfFolderFiltered)))
print(paste0("outFolder=",outFolder))
print(paste0("dataSetFilter=",dataSetFilter))
DF_significance <- readRDS(paste0(dfFolder,"/DF_significance.RDS"))
DF_significance <- bs_apply_dataSetFilter(DF_significance,dataSetFilter,dfFolder=dfFolder)
try(DF_significance_filt <- readRDS(paste0(dfFolderFiltered,"/DF_significance.RDS")))
try(  DF_significance_filt <- bs_apply_dataSetFilter(DF_significance_filt,dataSetFilter,dfFolder=dfFolder))
DF_prop <- readRDS(paste0(dfFolder,"/","DF_prop.RDS"))
DF_prop_rank <- readRDS(paste0(dfFolder,"/","DF_prop_rank.RDS"))
aim2_results <- list() # this list will only contain results for the subset of hyptheses where both (filtered and unfiltered) is required
aim2_regression <- list() #


test_cols <- which(sapply(DF_significance, is.logical)) # indices
test_cols_filt <- which(sapply(DF_significance, is.logical)) # indices
data_names <- unique(DF_significance$project_dataset) 

try(test_names <- intersect(names(DF_significance_filt)[test_cols_filt], names(DF_significance)[test_cols] ))
try(data_names_filt <- unique(DF_significance_filt$project_dataset) )
try(data_names.keep <- intersect(data_names,data_names_filt))

#
data_to_compare <- readRDS(paste0(dataFolder,"/data_to_compare.RDS"))
# Now execute the hypothesis by hand:
