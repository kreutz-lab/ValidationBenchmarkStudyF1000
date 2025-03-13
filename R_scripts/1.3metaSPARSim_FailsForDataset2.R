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


d <- bs_getData()
d2 <- bs_subset(d,2) # Select 2nd data set
d2 <- bs_simulateData(d2,parallelMode = F)

# Error message:
# 
# 371 OTUs have values >0 in more than 20% of samples
# Warning message:
#   In DGEList.default(as.matrix(data[ind_pass_filter, ])) :
#   At least one library size is zero


## Manual check:
D <- d[[2]]
conditions <- split(seq_along(D$original$meta$condition), D$original$meta$condition)
raw_data=D$original$counts
norm_data=D$original$counts.norm

params <- estimate_parameter_from_data(raw_data=raw_data, 
                                       norm_data=norm_data, 
                                       conditions=conditions, intensity_func = "mean", keep_zeros = TRUE)


# 371 OTUs have values >0 in more than 20% of samples
# Error in .compressOffsets(y, lib.size = lib.size, offset = offset) : 
#   offsets must be finite values
# In addition: Warning message:
#   In DGEList.default(as.matrix(data[ind_pass_filter, ])) :
#   At least one library size is zero
