# BenchmarkStudy_MicrobiomeSyntheticData
Leveraging Synthetic Data to Validate a Benchmark Study for Differential Abundance Tests for 16S Microbiome Sequencing Data


This repository is used for conducting the Benchmark Study according to the following protocol:
ADD_CITATION

## How to run the code

The whole data and results are stored in an object that we call `bs_object` and that is typically named as `d` or `data_to_compare` in the code. It is a hierarchical list. Different entries are labelled with an attribute named as `bs_type`.

### Initialization
Start the analysis via to load all functions and libraries:

`source("project_init.R")`

`project_init() `

You are then in the R_scripts folder, which is the place where analyses are performed.

### Starting the analysis workflow (or parts of it)

Most computations were done on our compute cluster, login via  
`ssh ckreutz@imbis236l40`

The script "RunAll.R" contains all calls that are required to run the whole analysis workflow.
After uncommenting all steps, it can be run on a compute cluster, e.g. via

`nohup /opt/bin/R-4.3 --no-save --no-restore -e 'source("RunAll.R")'  > RunAll_28-May24.out 2>&1 &`


Of course, parts of the analyses can also run, if the required preceeding resuls are available.
These files start with numbers, e.g. running the differential abundance tests for metaSPARSim data via:

`nohup /opt/bin/R-4.3 --no-save --no-restore -e 'source("4.1DA_metaSPARSim.R")'  > RunAll_28-May24.out 2>&1 &`

Practial hints:
* Copy RunAll.R and edit this copy for performing parts of the workflow
* Use  
`htop`   
`ls -lrt`  
`tail -F RunAll_28-May24.out`  
for checking whether everything runs fine or to see intermediate results.
* For finding specific files, the following commands might be helpful:  
`find ../Results -name data_to_compare.RDS -type f -exec ls -lrt {} +`  
`find ../Results -name "*.RDS" -type f -newermt "2023-05-15" -exec ls -lrt {} +`  

The following command only makes `git add` for files that are newer than the specified number of minutes (here 360):

`find ../Results -type f -mmin -360 -exec git add {} +`


### How the code is organized

RunAll.R calls all analysis steps:


```
source("0.0MakeCountTables.R")
source("0.1Prepare_Data.R")
source("0.2Make_BS_object.R")

source("1.1EstimateParams_metaSPARSim.R")
source("1.2EstimateParams_sparseDossa.R")

source("2.1metaSPARSim.R")
source("2.2Simulation_sparseDossa2.R")

# metaSPARSim 3x:
source("3.1DataProperties_metaSPARSim.R")
source("3.2DataProperties_metaSPARSim_ZerosAdded.R")
source("3.3DataProperties_metaSPARSim_Filtered.R")
source("3.4DataProperties_metaSPARSim_filtered_ZerosAdded.R")

source("3.1.1Plots_DataProps_metaSPARSim.R")
source("3.2.1Plots_DataProps_metaSPARSim_ZerosAdded.R")
source("3.3.1Plots_DataProps_metaSPARSim_Filtered.R")
source("3.4.1Plots_DataProps_metaSPARSim_Filtered_ZerosAdded.R")

# sparseDOSSA 3x:
source("3.1DataProperties_sparseDOSSA.R
source("3.2DataProperties_sparseDOSSA_ZerosAdded.R
source("3.3DataProperties_sparseDOSSA_Filtered.R
source("3.4DataProperties_sparseDOSSA_filtered_ZerosAdded.R

source("3.1.1Plots_DataProps_sparseDOSSA.R
source("3.2.1Plots_DataProps_sparseDOSSA_ZerosAdded.R
source("3.3.1Plots_DataProps_sparseDOSSA_Filtered.R
source("3.4.1Plots_DataProps_sparseDOSSA_Filtered_ZerosAdded.R


# Equivalence tests
# Enumerated as 5* but can be done without DA (4*)
# To save time, equivalence test can be run before and then only DA for the best simulation approach can be done
source("5.xAim1_EquivalenceTests.R")

# metaSPARSim 4x:
# Comment out 4.* if not require due to the outcome of equivalence tests
source("4.1DA_metaSPARSim.R")
source("4.2DA_metaSPARSim_ZerosAdded.R")
source("4.3DA_metaSPARSim_filtered.R")
source("4.4DA_metaSPARSim_filtered_ZerosAdded.R")

# The following helper functions check if DA has finished, collect partial resuls in case of failure and complete it.
option="4.1"
source("helper_4.x_collectAndSaveDA.R")

option="4.2"
source("helper_4.x_collectAndSaveDA.R")

option="4.3"
source("helper_4.x_collectAndSaveDA.R")

option="4.4"
doMissings = TRUE
source("helper_4.x_collectAndSaveDA.R")


# sparseDOSSA 4x:
source("4.1DA_sparseDOSSA.R")
source("4.2DA_sparseDOSSA_ZerosAdded.R")
source("4.3DA_sparseDOSSA_filtered.R")
source("4.4DA_sparseDOSSA_filtered_ZerosAdded.R")

## The following part does aim2 analysis with the 4* results
# Alternatively, 5.x_ancom_Nearing.R can be run which replaces ancombc by ancom_Nearing and the does the following 4 analyses
# source("5.x_ancom_Nearing.R")

# metaSPARSim 5x:
source("5.xAim2_primary_metaSPARSim.R")
source("5.xAim2_secondary_metaSPARSim.R")
# sparseDOSSA 5x:
source("5.xAim2_primary_sparseDOSSA.R")
source("5.xAim2_secondary_sparseDOSSA.R")


## Summarize Hypotheses outcomes:
df_aim2res_pr <- bs_summarize_aim2results(outFile="../Results_partReg/aim2results.xlsx",
                                          resultFolder="../Results_partReg/",
                                          pattern2="Nearing")

df_aim2res <- bs_summarize_aim2results(outFile="../Results/aim2results.xlsx",
                                          resultFolder="../Results/",
                                          pattern2="Nearing")

## Summarizing all outcome structures data_to_compare*
bs_summarize_all("data_to_compare.RDS") # used for final results
# bs_summarize_all("data_to_compare",outfile="bs_summarize_all.inclVorSM.xlsx") # all, including before split & merge

```


### 1.2 Estimation of parameters for sparseDOSSA
Estimating the simulation parameters of sparseDOSSA requires a lot of computations, or it does not finish at all (within serveral weeks).

Estimated parameters for each condition in each data template are stored as `estimateSparseDossa_foreach*.Rdata` in `../Results/1.2_sparseDOSSA`. A restart overwrites these files. For using these parameters for simulation, a workspace with all these parameters has to be created. This is done via:

```
inFolder <- "../Results/1.2_sparseDOSSA/"
outFile <- paste0("../Results/1.2_sparseDOSSA/params.RDS")   # this file params.RDS is used by default
bs_sparseDossa_makeParamsRDS(inFolder=inFolder,outFile=outFile)
```

### 5.x Code for validating hypotheses of Nearing et al.

Investigating the hypotheses to be validated and stated in the protocol requires the same analyses performed on many intermediate results. These intermediate results are p_values (or their significance) of all DA tests. These results were calculated for the following data:
* Orignal data templates
* Simulated data (metaSPARSim or sparseDOSSA)
* Simulated data (metaSPARSim or sparseDOSSA) with adjusted proportion of zeros (i.e. zeros are added as they occur in the template)

In the protocol, we distinguish primary and secondary hypotheses, which are addressed in the following scripts:
5.xAim2_primary_sparseDOSSA.R



## Checks and tests
The following functions for summarizing and checking (intermediate) results are available:
* `bs_checkData()` for assessing constency of the data
* `bs_checkDA()` for assessing whether DA tests succeeded
* `bs_checkDAs()` for comparing the result of the DA tests in two `bs_objects` 
* `bs_summarize()` generates a summarizing `data.frame` for a `bs_object`
* `bs_summarize_dataPropCmp()` generates a `data.frame` that summarizes the comparison of data properties.
* `bs_summarize_aim2results("data_to_compare.RDS")` loads all data_to_compare.RDS files and summarizes its content

## Helper scripts
There are scripts that help to accomplish calculation steps that are not succeeded, or to update workspaces.
They are named `helper_*`

If DA did not succeed for all data sets, then the result RDS files of individual steps can be collected and stored as data_to_compare.RDS in the respective folder. For metaSPARSim, the following options are available:  
```
# doMissings = TRUE
option="4.1"
source("helper_4.x_collectAndSaveDA.R")

option="4.2"
source("helper_4.x_collectAndSaveDA.R")

option="4.3"
source("helper_4.x_collectAndSaveDA.R")

option="4.4"
source("helper_4.x_collectAndSaveDA.R")
```

For sparseDOSSA, the following options are available:  

```
# doMissings = TRUE
option="4.1spd"
source("helper_4.x_collectAndSaveDA.R")

option="4.2spd"
source("helper_4.x_collectAndSaveDA.R")

option="4.3spd"
source("helper_4.x_collectAndSaveDA.R")

option="4.4spd"
source("helper_4.x_collectAndSaveDA.R")
```
If you set `doMissings = TRUE`, then missing DAs are tried to be completed. However, this can take hours to days.


