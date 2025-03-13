# This script is called for 3.x analyses and requires three variables:
#
# data_to_compare as data
# resultFolder for results
# prefix  for log-files

print("script_3x_DataProperties.R started ...")
print(paste0("resultFolder: ", resultFolder))
print(paste0("prefix: ", prefix))

try(dir.create(resultFolder))


# Compress if not already compressed
gc() # enfore garbage collection, i.e. free RAM
#data_to_compare <- bs_compress(data_to_compare)


# parallel loop over all datasets
msb.registerDoParallel(min(parallel::detectCores()-1,length(data_to_compare)))
# registerDoParallel(cores = min(parallel::detectCores(),length(data_to_compare)))

namen <- names(data_to_compare)
for(index in 1:length(data_to_compare)){
  data_to_compare[[index]]$index <- index
  data_to_compare[[index]]$pfad <- getwd()
}

data_to_compare <- bs_annotateObject(data_to_compare) # ensure that $meta is available

#for(i in 1:length(data_to_compare)){
data_to_compare <- foreach(D=data_to_compare) %dopar% {
  require(amap)
  require(BimodalIndex)
  require(edgeR)
  require(cluster)
  require(parallelDist)
  require(vegan)

  tryCatch({
    setwd(D$pfad)
    cat("addDataProp for data_project ",D$index,"started: ",as.character(Sys.time()),"\n",file=paste0(prefix,"_foreach.log"),append = T)


    source("project_init.R")
    # project_init()

    # D <- bs_decompress(D) # one data_project ==> should be done outside (and is done outside)
    D <- bs_addDataProp(D)
    #D <- bs_compress(D)
    cat("addDataProp for data_project ",D$index,"finished: ",as.character(Sys.time()),"\n",file=paste0(prefix,"_foreach.log"),append = T)

  }, error=function(e){
    try(save(list="D",file=paste0("Error_3.1_index",D$index,".Rdata")))
    cat("Error (index=",D$index,"): ",conditionMessage(e),"\n",file=paste0(prefix,"_foreach.log"),append = T)
  })
  return(D)
}

# doParallel::stopImplicitCluster()
msb.unregisterDoParallel()
for(index in 1:length(data_to_compare)){
  data_to_compare[[index]]$index <- NULL
  data_to_compare[[index]]$pfad <- NULL
}
names(data_to_compare) <- namen # foreach returns a list but without names: copy them manually
attr(data_to_compare, "bstype") <- "data_list"

saveRDS(bs_compress(data_to_compare),file=paste0(resultFolder,"/data_to_compare.RDS"))

## Difference of data props:
#data_to_compare <- bs_decompress(data_to_compare)
data_to_compare <- bs_annotateObject(data_to_compare)
data_to_compare <- bs_diffDataProp(data_to_compare)
saveRDS(bs_compress(data_to_compare),file=paste0(resultFolder,"/data_to_compare.RDS"))

## Make DF with data prop summary:
# data_to_compare <- bs_decompress(readRDS(paste0(resultFolder,"/data_to_compare.RDS"))
DF <- bs_summarize_dataPropCmp(data_to_compare)

DF$comparison <- gsub("metaSPARSim","mSS",DF$comparison)
DF$comparison <- gsub("sparseDOSSA2","spD",DF$comparison)
DF$comparison <- gsub("original","orig",DF$comparison)
saveRDS(DF,file = paste0(resultFolder,"/DF.RDS"))

# Add template vs template
DF_TvsT <- bs_diffDataProp_templates(data_to_compare)
DF_TvsT_summary <- bs_summarize_dataPropCmp_templates(DF_TvsT)

# Merge comparison data frames
DF <- rbind(DF[,!colnames(DF)=="comparison2"],DF_TvsT_summary)
saveRDS(DF, paste0(resultFolder,"/DF.RDS") )


DF_summary <- bs_summarize(data_to_compare)
saveRDS(DF_summary, paste0(resultFolder,"/DF_summary.RDS"))


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

save(df_long,p,resultFolder,file=paste0(resultFolder,"/HistogramAfterTransformation.Rdata"))
pdf(file=paste0(resultFolder,"/HistogramAfterTransformation.pdf"),width = 20, height=14)
print(p)
dev.off()

cat("\n\n script_3x_DataProperties.R finished :-)\n")
