rm(list=ls())

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

source("project_init.R")

## Calling all steps of the workflow:


#cat("Wait for one hour...\n")
# Sys.sleep(3600)
#cat("Wait for one hour...\n")
# Sys.sleep(3600)
#cat("Wait for one hour...\n")
# Sys.sleep(3600) 
#cat("Wait for one hour...\n")
# Sys.sleep(3600)
#cat("Wait for one hour...\n")
# Sys.sleep(3600)


# source("0.3RedundantDataProps.R")

# source("0.0MakeCountTables.R")
# source("0.1Prepare_Data.R")
# source("0.2Make_BS_object.R")
# source("0.3RedundantDataProps.R")

#
# source("1.1EstimateParams_metaSPARSim.R")
# source("1.2EstimateParams_sparseDossa.R")
#
# source("2.1metaSPARSim.R")
# source("2.2Simulation_sparseDossa2.R")
# source("2.1metaSPARSim_partReg.R")
#
## metaSPARSim 3x:
# source("3.1DataProperties_metaSPARSim.R")
# source("3.2DataProperties_metaSPARSim_ZerosAdded.R")
# source("3.3DataProperties_metaSPARSim_Filtered.R")
# source("3.4DataProperties_metaSPARSim_filtered_ZerosAdded.R")
#
# source("3.1.1Plots_DataProps_metaSPARSim.R")
# source("3.1.1Plots_DataProps_metaSPARSim_partReg.R")
#
#
## sparseDOSSA 3x:
# source("3.1DataProperties_sparseDOSSA.R")
# source("3.2DataProperties_sparseDOSSA_ZerosAdded.R")
# source("3.3DataProperties_sparseDOSSA_Filtered.R")
# source("3.4DataProperties_sparseDOSSA_filtered_ZerosAdded.R")
#
# source("3.1.1Plots_DataProps_sparseDOSSA.R")
# source("3.1.1Plots_DataProps_sparseDOSSA_partReg.R")
#
## metaSPARSim 4x:
# source("4.1DA_metaSPARSim.R")
# source("4.2DA_metaSPARSim_ZerosAdded.R")
# source("4.3DA_metaSPARSim_filtered.R")
# source("4.4DA_metaSPARSim_filtered_ZerosAdded.R")
#
# option="4.1"
# source("helper_4.x_collectAndSaveDA.R")
#
# option="4.2"
# source("helper_4.x_collectAndSaveDA.R")
#
# option="4.3"
# source("helper_4.x_collectAndSaveDA.R")
#
# option="4.4"
# doMissings = TRUE
# source("helper_4.x_collectAndSaveDA.R")
#
#
## sparseDOSSA 4x:
# source("4.1DA_sparseDOSSA.R")
# source("4.2DA_sparseDOSSA_ZerosAdded.R")
# source("4.3DA_sparseDOSSA_filtered.R")
# source("4.4DA_sparseDOSSA_filtered_ZerosAdded.R")
#
# option="4.1spd"
# source("helper_4.x_collectAndSaveDA.R")
#
# option="4.2spd"
# source("helper_4.x_collectAndSaveDA.R")
#
# option="4.3spd"
# source("helper_4.x_collectAndSaveDA.R")
#
# option="4.4spd"
# doMissings = TRUE
# source("helper_4.x_collectAndSaveDA.R")
#

#source("helper_4.x_dataToCompare2DF.R")

#
## Equivalence tests:
# source("5.xAim1_EquivalenceTests.R")
# source("5.xAim1_EquivalenceTests_partReg.R")


### Results with Nearing's ANCOM:
#doPlots <- T
source("5.x_ancom_Nearing.R")

### Results with ANCOMBC2:
## metaSPARSim 5x:
# source("5.xAim2_primary_metaSPARSim.R")
# source("5.xAim2_secondary_metaSPARSim.R")
#
## sparseDOSSA 5x:
# source("5.xAim2_primary_sparseDOSSA.R")
# source("5.xAim2_secondary_sparseDOSSA.R")
#


## Summarize Hypotheses outcomes:
df_aim2res <- bs_summarize_aim2results()

df_aim2res <- bs_summarize_aim2results(outFile="../Results/aim2results_pr+notpr.xlsx",
   resultFolder=c("../Results/","../Results_partReg/"),
   pattern2=c("simEquiv","Nearing"))

df_aim2res_pr <- bs_summarize_aim2results(outFile="../Results_partReg/aim2results.xlsx",
                                          resultFolder="../Results_partReg/",
                                          pattern2="Nearing")

df_aim2res_notpr <- bs_summarize_aim2results(outFile="../Results/aim2results.xlsx",
                                          resultFolder="../Results/",
                                          pattern2="Nearing")


#bs_summarize_all("data_to_compare.RDS")
#bs_summarize_all("data_to_compare",outfile="bs_summarize_all.inclVorSM.xlsx")

DF_summary <- bs_summarizeDF_all(folder="../",pattern2 = c("Result","/4"))

resAgg <- bs_evaluateProjectAggregation("../Results")
resAgg_pr <- bs_evaluateProjectAggregation("../Results_partReg")

source("6.1Aim2Analyses.R")

source("5.xAim2_RankCorrs.R")

#cat("\n\nRunAll finished :-)\n")

