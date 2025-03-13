# bs_DA_parallel
#
# This function can be used to execute DA methods over all datasets in one project in a parallelized manner.
# It treats multiple datasets (e.g. simulations) of one project as individual projects and then calls bs_DA
#
# Example:
# data_to_compare <- bs_simulateData(data_to_compare, nsim=cfg$Nsimu, simulator=cfg$simulator, file=cfg$outfolder2, parallelMode = F, runOption="fast")
# data_to_compare <- bs_DA_parallel(data_to_compare, whichMethod=c("edgeR"))

bs_DA_parallel <- function(d, design=as.formula("~condition"),groupVariable="condition",doSlowMethods=FALSE,maxFeatures=Inf,keepTestResult=FALSE,
                           whichMethods = c("Aldex2","ancom", "corncob","DEseq","edgeR","lefse","limma_voom","limma_voom_tmmwsp","Maaslin2", "Maaslin2_rare","metagenomeSeq", "ttest_rare","wilcox","wilcox_rare" ),
                           whichData = c("data_template","sim_result"),maxRunTime=Inf,
                           overwriteOldDA = FALSE, parallelMode = T,myTestInfo=NULL){
  
  d <- bs_annotateObject(d) # to be sure 
  
  # Mulitple projects => call bs_DA directly
  if(length(d)>1){
    bs_DA(d=d, design=design,groupVariable=groupVariable,doSlowMethods=doSlowMethods,maxFeatures=maxFeatures,keepTestResult=keepTestResult,
          whichMethods = whichMethods,
          whichData = whichData, maxRunTime=maxRunTime,
          overwriteOldDA = overwriteOldDA, parallelMode = parallelMode, myTestInfo=myTestInfo)
  }else{
    # Treat all data sets as projects
    name <- names(d)[1]
    dnames <- names(d[[1]])
    dd <- list()
    #oldAttr <- array()
    for(i in 1:length(dnames)){
      if(bs_isa(d[[1]][[i]],c("data_template","sim_result"))){
        dd[[dnames[i]]][[dnames[i]]] <- d[[1]][[i]]
        attr(dd[[dnames[i]]],"bstype") <- "data_project"
        attr(dd[[dnames[i]]][[dnames[i]]],"bstype") <- "data_template" # one data_template per project required
      }
    }
    attr(dd,"bstype") <- "data_list"
    
    dd <- bs_annotateObject(dd)
    
    dd <- bs_DA(d=dd, design=design,groupVariable=groupVariable,doSlowMethods=doSlowMethods,maxFeatures=maxFeatures,keepTestResult=keepTestResult,
                whichMethods = whichMethods,
                whichData = whichData, maxRunTime=maxRunTime,
                overwriteOldDA = overwriteOldDA, parallelMode = parallelMode, myTestInfo=myTestInfo)
    
    # Now copy dd...$DA to d...$DA
    for(i in 1:length(dnames)){
      if(bs_isa(d[[1]][[i]],c("data_template","sim_result")))
        d[[1]][[i]]$DA <- dd[[dnames[i]]][[dnames[i]]]$DA
    }
    
    return(d)
  }
  
}

