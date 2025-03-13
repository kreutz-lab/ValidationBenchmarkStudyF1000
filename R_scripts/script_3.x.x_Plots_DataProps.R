# This script is called for 3.x.x analyses and requires three variables:
#
# dataFolder  for loading properties
# resultFolder for results

print("script_3.x.x_DataProperties.R started ...")
print(paste0("resultFolder: ", resultFolder))
print(paste0("dataFolder: ", dataFolder))


try(dir.create(resultFolder))

#Get data - comparison of data props
DF <- readRDS(paste0(dataFolder,"/DF.RDS"))


## Plot boxplots comparing data properties 
#png(filename = paste0(resultFolder,"/dataPropCmp_yfree.png"),width = 1800,height=1200)

save(DF, resultFolder, file = paste0(resultFolder,"/dataPropCmp.Rdata"))

  pdf(file = paste0(resultFolder,"/dataPropCmp_yfree.pdf"),width=14,height=15)
  try( print(bs_plot_dataPropCmp(DF,scales="free_y")) )
  dev.off()

# Boxplots with fixed axis


  pdf(file = paste0(resultFolder,"/dataPropCmp_fixed.pdf"),width=14,height=15)
  try(  print(bs_plot_dataPropCmp(DF,scales="fixed")) )
  dev.off()

# Plot PCA and biplot
# Get object summarizing data properties for all data sets
DF_summary <- readRDS(paste0(dataFolder,"/DF_summary.RDS"))
DF_summary_log <- readRDS(paste0(dataFolder,"/DF_summary_log.RDS"))

try( file.remove(paste0(resultFolder,"/","pcaObject","",".RDS"), showWarnings=F))
try( file.remove(paste0(resultFolder,"/","pcaObject","_Log",".RDS"), showWarnings=F))

try( bs_PCA_dataProp_sim(DF_summary, type="PCA", folder=resultFolder) )
  #ggsave(PCA, file = paste0(resultFolder,"/PCA.pdf"), height=10, width=15)

try( bs_PCA_dataProp_sim(DF_summary, type="Biplot", folder=resultFolder) )
  #ggsave(PCA_biplot, file="paste0(resultFolder,"/PCA_biplot.pdf"), height=10, width=15)

try( bs_PCA_dataProp_sim(DF_summary_log, type="PCA", folder=resultFolder, filename="_Log") )
  #ggsave(PCA, file = "../Results/3.1_metaSPARSim_DataProps/PCA.pdf", height=10, width=15)


try( bs_PCA_dataProp_sim(DF_summary_log, type="Biplot", folder=resultFolder, filename="_Log") )
  #ggsave(PCA_biplot, file="../Results/3.1_metaSPARSim_DataProps/PCA_biplot.pdf", height=10, width=15)


##  Euclidean distance to original in the PCA result:
pca_result_log <- readRDS(paste0(resultFolder,"/pcaObject_Log.RDS"))
DF_summary_log <- bs_addPCA_distance(DF_summary_log, pca_result_log)
pca_result <- readRDS(paste0(resultFolder,"/pcaObject.RDS"))
DF_summary <- bs_addPCA_distance(DF_summary, pca_result)

DF_summary <- bs_addPCA_distance(DF_summary, pca_result)
DF_summary_log <- bs_addPCA_distance(DF_summary_log, pca_result_log)

saveRDS(DF_summary_log, paste0(resultFolder,"/DF_summary_log_PCA.RDS"))
saveRDS(DF_summary, paste0(resultFolder,"/DF_summary_PCA.RDS"))


tmp <- DF_summary_log
names(tmp)<-names(DF_summary)
DF_plot <- rbind(DF_summary,tmp)
DF_plot <- data.frame(DF_plot, logScale=c(rep(FALSE,length.out=nrow(DF_summary)),rep(TRUE,length.out=nrow(DF_summary_log))),isSimulated=!DF_plot$name=="original")
DF_plot <- data.frame(DF_plot,group=paste("Sim=",DF_plot$isSimulated,", log=",DF_plot$logScale))

DF_plot <- data.frame(DF_plot,dataset=paste(DF_plot$data_template,DF_plot$isSimulated,DF_plot$logScale))

p <- ggplot(DF_plot, aes(x = group, y = pcaDistance)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(
    title = "Distance of one template to the mean over all templates, and for simus to their respective template",
    x = "Data set(sim vs. exp), PCA for partial log properties (vs. no log)",
    y = "Distance in 2D PCA")




saveRDS(DF_plot, p, resultFolder, file = paste0(resultFolder,"/Boxplot_PCAdistances.Rdata"))
pdf(file = paste0(resultFolder,"/Boxplot_PCAdistances.pdf"),width=15,height=10)
print(p)
dev.off()

DF_plot <- DF_plot

p <- ggplot(DF_plot, aes(x = dataset, y = pcaDistance, fill=isSimulated)) +
  geom_boxplot(color = "darkblue")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(
    title = "Distances of the template to the mean over all templates, and for simus to their respective template",
    x = "Project and data set(sim vs. exp), PCA for partial log properties (vs. no log)",
    y = "Distance in 2D PCA")


print(p)
saveRDS(DF_plot, p,resultFolder, file = paste0(resultFolder,"/Boxplot_PCAdistances2.Rdata"))
pdf(file = paste0(resultFolder,"/Boxplot_PCAdistances2.pdf"),width=30,height=15)
print(p)
dev.off()

print("script_3.x.x_Plots_DataProps.R finished :-)")
