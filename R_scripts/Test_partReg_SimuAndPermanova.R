################################
# Set correct working directosry
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

# rm(list=ls())
# gc()

data_to_compare <- bs_getData()
data_to_compare <- bs_subset(data_to_compare,!names(data_to_compare)=="ArcticFreshwaters")
D <- data_to_compare
gc()
names(D)


# paramsWorkspace <- list(reg="../Results/1.1_metaSPARSim/params.RDS",  # regulated
#                         nonreg="../Results_partReg/1.1_metaSPARSim/params.RDS") # unregulated

source("bs_simulateData_old.R")
Dold <- bs_simulateData_old(D, nsim=5, simulator="metasparsim_nonreg",
                            file=".", parallelMode = T)

Dnormal <- bs_simulateData_old(D, nsim=5, simulator="metasparsim",
                            file=".", parallelMode = T)

Dnew <- bs_simulateData(D, nsim=5, simulator="metasparsim_partreg",
                        file=".", parallelMode = T)


Dold <- bs_annotateObject(Dold)
Dnormal <- bs_annotateObject(Dnormal)
Dnew <- bs_annotateObject(Dnew)

Dold <- bs_addDataProp(Dold)
Dnormal <- bs_addDataProp(Dnormal)
Dnew <- bs_addDataProp(Dnew)

for(i in 1:length(Dold)){
  print(c(templateOld=Dold[[i]]$original$data.Prop$Permanova_R2,
          old1=Dold[[i]]$metaSPARSimNonReg_1$data.Prop$Permanova_R2,
          old2=Dold[[i]]$metaSPARSimNonReg_2$data.Prop$Permanova_R2,
          old3=Dold[[i]]$metaSPARSimNonReg_3$data.Prop$Permanova_R2,
          templateNew=Dnew[[i]]$original$data.Prop$Permanova_R2,
          new1=Dnew[[i]]$metaSPARSimNonReg_1$data.Prop$Permanova_R2,
          new2=Dnew[[i]]$metaSPARSimNonReg_2$data.Prop$Permanova_R2,
          new3=Dnew[[i]]$metaSPARSimNonReg_3$data.Prop$Permanova_R2,
          normal=Dnormal[[i]]$metaSPARSim_1$data.Prop$Permanova_R2))
}

save.image("Test_partReg_SimuAndPermanova.Rdata")


