# Script to calculate data properties for real and synthetic data


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

# 
# 
resultFolder <- "../Results/3.1_sparseDOSSA_DataProps"

data_to_compare <- bs_decompress(readRDS(paste0(resultFolder,"/data_to_compare.RDS")))
DF_summary <- readRDS(paste0(resultFolder,"/DF_summary.RDS"))

tmp <- bs_checkLogDataProp(DF_summary)
doLog <- tmp$doLog
dfLog <- tmp$dfLog
saveRDS(dfLog, paste0(resultFolder,"/DF_summary_log.RDS"))


# Create histograms with faceting
library(tidyr)
df_long <- pivot_longer(dfLog[,!is.na(doLog),drop=F], cols = everything(), names_to = "Variable", values_to = "Value")


p <- ggplot(df_long, aes(x = Value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +  # Adjust bin size as needed
  facet_wrap(~ Variable, scales = "free_x") +  # Free scales if the ranges are very different
  labs(title = "Histograms of All Data Properties",
       x = "Value",
       y = "Count") +
  theme_minimal()

# p + theme(strip.text = element_text(size = rel(0.5)),
#   plot.title = element_text(size = rel(0.8)),     # Decrease title font size 
#   axis.title = element_text(size = rel(0.8)),     # Decrease axis titles font size 
#   axis.text = element_text(size = rel(0.8)),      # Decrease axis text font size 
#   legend.title = element_text(size = rel(0.8)),   # Decrease legend title font size 
#   legend.text = element_text(size = rel(0.8))     # Decrease legend text font size 
# )

pdf(file=paste0(resultFolder,"/HistogramAfterTransformation.pdf"),width = 20, height=14)
print(p)
dev.off()

cat("\n\n script_3x_DataProperties.R finished :-)\n")
