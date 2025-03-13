rm(list=ls())

# Script to analyze the results from differential abundance tests on metaSPARSim data
# This corresponds to Aim 2 - primary outcomes
# barplot to summarize the proportion of shared significant features across all syntehtic data sets
# 13 conclusions from Box1 are tested

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

# Suppl. Table number of signficiant "unfiltered - filtered"
source("Analysis_NumberOfSignificantFeatures.R")

#######################################
## Aim 2 results (validation TRUE/FALSE)
df_aim2res <- bs_summarize_aim2results(pattern2=c("simEquiv","Nearing"),outFile = "aim2results_simEquiv+Nearning.xlsx")

# Select data subset here:
# only those on simulated data that pass equivalence tests
df_aim2resEquiv <- df_aim2res[grep("simEquiv",df_aim2res$file),]  
bs_summarize_aim2results_write(df_aim2resEquiv,outFile = "../Results/aim2results_simEquiv.xlsx")


########################################
## Aim 2 logistic regression with data props (selected TRUE/FALSE)

dfSelected <- bs_summarize_aim2regression(pattern2=c("simEquiv","Nearing"),patternNot = c("NoAncom","NoLefse","simEquivSpd"),outFile = "aim2regression_PaperVersion.xlsx")

myplot <- bs_plotHypothesisComparison(dfSelected)
pdf(file="../bs_plotHypothesisComparison.pdf",width=7,height=7)
print(myplot)
dev.off()

myplot <- bs_plotFilteredComparison(dfSelected)
pdf(file="../bs_plotFilteredComparison.pdf",width=7,height=7)
print(myplot)
dev.off()


myplot <- bs_plotNonZeroCounts(dfSelected)
pdf(file="../bs_plotNonZeroCounts.pdf",width=6,height=7)
print(myplot)
dev.off()

# # Select data subset here:
# dfEquiv <- dfSelected[grepl("simEquiv",dfSelected$dataName),]
# # Write all together 
# bs_summarize_aim2regression_write(dfEquiv, outFile="aim2_regression_simEquiv_all.xlsx",splitData = F)
# # splitted according to $dataName:
# # bs_summarize_aim2regression_write(dfEquiv, outFile="aim2_regression_simEquiv_splitted.xlsx")
# 
# df_combined <- bs_combine_aim2summaries(fileAim2result="../Results/aim2results_simEquiv_sorted.xlsx", 
#                                         fileAim2regression="../Results/aim2_regression_simEquiv_all.xlsx")
# 
# wb <- createWorkbook()
# addWorksheet(wb,"df_combined")
# writeData(wb,sheet="df_combined",x=df_combined)
# saveWorkbook(wb,"df_combined_simEquiv.xlsx")


## Additional Figures
source("Figures_DataPropCmp_IndividualBoxplots.R")
source("Figure_DataPropCmp_P0_allPipelines.R")
source("Figure_DataPropCmp_manually.R")
source("Figures_OverlapHist_all_manually.R")

source("Analysis_pvalue_scatterplots")
