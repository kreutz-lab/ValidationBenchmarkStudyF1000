cat("\n\n script_5.x_Aim2_Plots.R started ... \n\n")

#print(paste0("prefix=",prefix))
#print(paste0("dataFolder=",dataFolder,"   # for reading DF_filt.RDS "))
print(paste0("dfFolder=",dfFolder,"   # for bs_apply_dataSetFilter"))
print(paste0("outFolder=",outFolder,"   # for saving plots"))
print(paste0("dataSetFilter=",dataSetFilter))

if(!dir.exists(outFolder))
  try(dir.create(outFolder))

DF_filt <- readRDS(paste0(dfFolder,"/DF_filt.RDS"))

############################################
## Now filter according to exp or simu #####
DF_filt <- bs_apply_dataSetFilter(DF_filt,dataSetFilter,dfFolder=dfFolder)
## End filter according to exp or simu #####
############################################


# Nur den OverlapHist plot aufrufen funktionert nicht, bzw. wird nicht abgespeichert
#overlap.plot <- bs_DA_plotFunctions(DF_filt, plotType = "OverlapHist", folder = outFolder)
#ggsave(overlap.plot, paste0(outFolder,"/overlap.pdf"))
# Alle plots - werden erstellt, overlapHist fehlerhaft
bs_DA_plotFunctions(DF_filt,  folder = outFolder,useNearingNames=F)

try(file.remove(paste0(outFolder,"/ROC.pdf")))
try(file.remove(paste0(outFolder,"/Sens_vs_alpha.pdf")))
try(file.remove(paste0(outFolder,"/Spec_vs_alpha.pdf")))



cat("\n\n script_5.x_Aim2_Plots.R finished :-) \n\n")
