## This script can be used to compare Nearing data with count.tables in our data folder:


nearingFolder <- "e:/clemens/Projekte/_Mikrobiom/Nearing22/Hackathon/Studies/"

# bsFile <- "ob_zhu_ASVs_table.RDS"
# nearingFile <- "ob_zhu/ob_zhu_ASVs_table.tsv"

bsFile <- "ob_zupancic_ASVs_table.RDS"
nearingFile <- "ob_zupancic/ob_zupancic_ASVs_table.tsv"

n <- read.table(paste0(nearingFolder,nearingFile), sep="\t", header=TRUE, comment.char="")
nM <- as.matrix(n[,-1])
row.names(nM) <- n[,1]

tmp <- readRDS(paste0("../Data/count.data/",bsFile))


if(sum(abs(nM-tmp)) == 0)
  print(paste0("Data coincides for ",bsFile,"."))

if(sum(abs(nM-tmp))> 0){
  # head of the data:
  head(nM)
  head(tmp)
  # sum over counts:
  sum(nM)
  sum(tmp)
  # prevalence filter
  sum(rowSums(nM>0)/dim(nM)[2] > 0.1)
  sum(rowSums(tmp>0)/dim(tmp)[2] > 0.1)
  stop(paste0("Data does not coincide for ",bsFile,"."))
}



