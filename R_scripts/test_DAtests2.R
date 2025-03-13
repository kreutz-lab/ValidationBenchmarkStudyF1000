setwd("E:/clemens/Repositories/BenchmarkStudy_MicrobiomeSyntheticData/R_scripts")
source("project_init.R")

d <- bs_getData()

# only use 1 dataset for testing:
#ind <- which(names(d)=="wood_plastic_kesy") # medium size
#ind <- which(names(d)=="t1d_mejialeon") # small
ind <- which(names(d)=="ibd_papa") # small

D <- bs_subset(d,ind)
meta <- D[[1]]$original$meta
counts <- D[[1]]$original$counts

dim(counts)
D <- bs_annotateObject(D)
d1 <- bs_DA(D, overwriteOldDA=T, parallelMode = F, doSlowMethods = T, maxRunTime = 60, whichMethods=c("metagenomeSeq"))

randorder <- sample(1:nrow(counts))  # Random permutation of row indices
inverseOrder <- order(randorder)     # Get the inverse order to undo permutation

result1 <- bs_metagenomeSeq(counts = counts, meta = meta)
result2 <- bs_metagenomeSeq(counts = counts[randorder, ], meta = meta)
#result3 <- bs_lefse(counts = counts, meta = meta)

plot(result1$pvalue, result2$pvalue[inverseOrder])  # diagonal line intended
#plot(result1$pvalue, result3$pvalue)  # Correct comparison


#whichMethods = c("Aldex2","ancom", "corncob","DEseq","edgeR","lefse","limma_voom","limma_voom_tmmwsp","Maaslin2", "Maaslin2_rare","metagenomeSeq", "ttest_rare","wilcox","wilcox_rare" )
bs_applyTests <- function(counts,meta){
  result <- list()
  # result[["adapt"]] <- bs_adapt(counts=counts,meta=meta)
 result[["aldex"]] <- bs_aldex(counts=counts,meta=meta)
 result[["ancombc2"]] <- bs_ancombc(counts=counts,meta=meta)
 result[["fastAncom"]] <- bs_fastANCOM(counts=counts,meta=meta)
 result[["corncob"]] <- bs_corncob(counts=counts,meta=meta)
 result[["DEseq"]] <- bs_DESeq(counts=counts,meta=meta)
 # result[["distinctTest"]] <- bs_distinctTest(counts=counts,meta=meta)
 result[["edgeR"]] <- bs_edgeR(counts=counts,meta=meta)
 result[["lefse"]] <- bs_lefse(counts=counts,meta=meta)
 result[["limma_voom"]] <- bs_limma_voom(counts=counts,meta=meta)
 result[["limma_voom_tmmwsp"]] <- bs_limma_voom_TMMwsp(counts=counts,meta=meta)
 # result[["linda"]] <- bs_linda(counts=counts,meta=meta)
 result[["maaslin2"]] <- bs_Maaslin2(counts=counts,meta=meta)
 result[["Maaslin2_rare"]] <- bs_Maaslin2_rare(counts=counts,meta=meta)
 # result[["maaslin3"]] <- bs_Maaslin3(counts=counts,meta=meta)
 result[["metagenomeSeq"]] <- bs_metagenomeSeq(counts=counts,meta = meta)
 result[["metagenomeSeq_zig"]] <- bs_metagenomeSeq_zig(counts=counts,meta = meta)
 # try(result[["nearing"]] <- bs_ancom_Nearing(counts=counts,meta=meta))
 # result[["ttest"]] <- bs_ttest(counts=counts,meta=meta)
 result[["ttest_rare"]] <- bs_ttest_rare(counts=counts,meta=meta)
 result[["wilcox"]] <- bs_wilcox.test(counts=counts,meta=meta)
 result[["wilcox_rare"]] <- bs_wilcox_rare.test(counts=counts,meta=meta)
 
# result[["zinq"]] <- bs_zinq(counts=counts, meta=meta)
# result[["zicoseq"]] <- bs_zicoseq(counts=counts,meta=meta)
# result[["glmmTMB"]] <- bs_glmmTMB(counts=counts,meta=meta)
###result[["zinbWave"]] <- bs_zinbWave(counts=counts,meta=meta,maxFeatures = 10)
 df <- NULL
 for(test in names(result)){
   if(is.null(df))
     df <- data.frame(result[[test]]$pvalue)
   else
     df <- cbind(df,data.frame(result[[test]]$pvalue))
 }
 names(df) <- names(result)

 return(df)
}

df <- bs_applyTests(counts,meta)
df2 <- bs_applyTests(counts[randorder,],meta)

saveRDS(df,file="helper_test_DAtests.RDS")
saveRDS(df2,file="helper_test_DAtests2.RDS")

print(pairs(df))
print(pairs(df2))

CairoPNG(filename="helper_test_DAtests.PNG",width = 1000,height = 1000)
pairs(df)
dev.off()
CairoPNG(filename="helper_test_DAtests2.PNG",width = 1000,height = 1000)
pairs(df2)
dev.off()

pdf(file = "helper_test_DAtests.pdf", width = 20,height = 20)
pairs(df,cex.labels = 1.5)
dev.off()

pdf(file = "helper_test_DAtests1+2.pdf", width = 20,height = 20)
pairs(cbind(df,df2),cex.labels = 1)
dev.off()

