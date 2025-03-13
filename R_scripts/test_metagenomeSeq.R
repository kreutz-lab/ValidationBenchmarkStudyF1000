#DF <- readRDS("../Results/4.4_sparseDOSSA_filtered_ZerosAdded_DA/DF.RDS")
#DF <- readRDS("../Results/4.2_sparseDOSSA_ZerosAdded_DA/DF.RDS")
DF <- readRDS("../Results_partReg/4.2_metaSPARSim_ZerosAdded_DA/DF.RDS")

ind <- which(DF$test=="metagenomeSeq" & DF$project=="ibd_papa")
head(ind)
DF[head(ind),]
ind2 <- which(DF$test=="EdgeR" & DF$project=="ibd_papa")
DF[head(ind2),]
