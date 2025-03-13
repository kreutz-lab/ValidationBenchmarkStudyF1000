#' bs_DA
#'
#' Differential abundance analyses for all datasets in data_list
#'
#' details
#'
#' @param data_list object e.g. data_to_compare
#' @param design formula, e.g. used be DESeq2
#' @param groupVariable used to select the right column in meta (e.g. for aldex2)
#' @param maxFeatures number of features to be analyzed maximally (the first maxFeatures rows)
#' @param keepTestResult default: FALSE (objects calcualted by tests are NOT stored in the object)
#' @param maxRunTime timeout in minutes, default: Inf 
#' @param whichMethods which statistical method should be done, 
#'    Default: c("Aldex2","ancom","corncob","DEseq","edgeR","lefse","limma_voom","limma_voom_tmmwsp","Maaslin2","wilcox","wilcox_rare") 
#'    This argument is case-insensitive, e.g. you can also write "edger" instead of "edgeR"
#'    In case of doSlowMethods = TRUE, some of the specified methods are skipped. Use doSlowMethods=TRUE in order do avoid this
#'    
#'    If custom test function can be called if whichMethods starts with "my", e.g. "myTestTemplate"
#'    
#'    
#' @param whichData=c("data_template","sim_result") can be used to only analyzed simulated or only template data
#' @param overwriteOldDA Can be used to overwrite maybe existing results in $DA from previous calls of this function
#'    Default: FALSE
#' @param myTestInfo Optional list of information provided to myTestFunctions
#' @param doMemoryCount [FALSE] Counting max memory allocation? (Makes code executation slower)
#'    Results are stored in $DA_memory
#'
#' @return data_list object
#' 
#' @examples
#' d <- readRDS(paste0(bs_path,"Analyses/Data/example_data/bs_object2.RDS"))
#' d <- bs_annotateObject(d)
#' 
#' d <- bs_DA(d)
#' d <- bs_DA(d,maxFeatures = 1000)
#' d <- bs_DA(d,whichMethods="glmmTMB") # only one approach
#' 
#' @seealso \code{\link{bs_sortMetaData}}
#' @keywords aKeywordSuchAsBenchmarking
#' @export

bs_DA <- function(d, design=as.formula("~condition"),groupVariable="condition",doSlowMethods=FALSE,maxFeatures=Inf,keepTestResult=FALSE,
                  whichMethods = c("Aldex2","ancom", "corncob","DEseq","edgeR","lefse","limma_voom","limma_voom_tmmwsp","Maaslin2", "Maaslin2_rare","metagenomeSeq", "ttest_rare","wilcox","wilcox_rare" ),
                  whichData = c("data_template","sim_result"), maxRunTime = Inf,
                  overwriteOldDA = FALSE, parallelMode = T, myTestInfo=NULL, doMemoryCount=F){
  
  if(!is.null(myTestInfo) && !is.list(myTestInfo)){
    stop("bs_DA.R: myTestInfo should be NULL or a list.")
  }
  
  if(msb.attrCompare(d,"bstype","data_project")){# handle this case
    dlist <- list(element=d)
    attr(dlist, "bstype") <- "data_list"
    dlist <- bs_annotateObject(dlist)
    
    dlist <- bs_DA(dlist, design=design, groupVariable=groupVariable, doSlowMethods=doSlowMethods, maxFeatures=maxFeatures, keepTestResult=keepTestResult,
                   whichMethods=whichMethods,
                   whichData=whichData,
                   overwriteOldDA=overwriteOldDA, parallelMode = parallelMode, doMemoryCount=doMemoryCount)
    return(dlist[[1]])
  } # data_project
  else
  { # data_list
    
    if(!msb.attrCompare(d,"bstype","data_list"))
      stop("bs_createDataLabels is up to now only implemented for a data_list (whole data_to_compare object)")
    
    if(sum(names(d[[1]][[1]])=="project_label")<1)
      stop("bs_DA: call bs_annotateObject() first because project_label etc required.")
    
    namen <- names(d)
    # Variables for inside parfor:
    for(i in 1:length(d)){
      d[[i]]$name          <- namen[i]
      d[[i]]$design        <- design
      d[[i]]$groupVariable <- groupVariable
      d[[i]]$doSlowMethods <- doSlowMethods
      d[[i]]$maxFeatures   <- maxFeatures
      d[[i]]$keepTestResult<- keepTestResult
      d[[i]]$pfad          <- getwd() 
      d[[i]]$whichData     <- whichData
      d[[i]]$overwriteOldDA<- overwriteOldDA
      d[[i]]$whichMethods  <- whichMethods
      d[[i]]$myTestInfo    <- myTestInfo
      d[[i]]$bsLibPath     <- bsLibPath
      d[[i]]$maxRunTime    <- maxRunTime
      d[[i]]$doMemoryCount <- doMemoryCount
    }
    
    ## Option1: parallel code version:
    if(parallelMode){
      ## Parallel loop
      #doParallel::registerDoParallel(min(parallel::detectCores(),length(d)))
      #      msb.registerDoParallel(min(5,min(parallel::detectCores()-1,length(d))))
      msb.registerDoParallel(min(parallel::detectCores()-1,length(d)))
      # d_out <- foreach::foreach(D = d, .packages = c('ANCOMBC','ALDEx2','corncob','DESeq2','edgeR','Maaslin2','microbiomeMarker','phyloseq','Rfast','glmmTMB')) %dopar% {
      
      d_out <- foreach::foreach(D = d) %dopar% {
        setwd(D$pfad)
        
        bsLibPath <- D$bsLibPath
        source(paste0(D$bsLibPath,"/../R_scripts/project_init.R"))
        
        D <- bs_DA_LoopCore(D)
        D$bsLibPath <- NULL
        
        return(D)
        # non-parallel
        # dout[[i]] <- D
      }
      #doParallel::stopImplicitCluster()
      try(msb.unregisterDoParallel())
      
    } else {
      
      # for(D in d){
      d_out <- foreach::foreach(D = d) %do% {
        setwd(D$pfad)
        bsLibPath <- D$bsLibPath
        source(paste0(D$bsLibPath,"/../R_scripts/project_init.R"))
        
        D <- bs_DA_LoopCore(D)
        D$bsLibPath <- NULL
        
        return(D)
      }
      
      
    }
    
    names(d_out) <- names(d) # foreach returns a list but without names: copy them manually
    
    # attribute bstype is missing
    attr(d_out, "bstype") <- attr(d,"bstype")
    
    return(d_out)
  } # if data_list
}


bs_DA_LoopCore <- function(D){
  # require('ANCOMBC')
  # require('ALDEx2')
  # require('corncob')
  # require('DESeq2')
  # require('edgeR')
  # require('Maaslin2')
  # require('microbiomeMarker')
  # require('phyloseq')
  # require('Rfast')
  # require('glmmTMB')
  
  name          <- D$name
  design        <- D$design
  groupVariable <- D$groupVariable
  doSlowMethods <- D$doSlowMethods
  maxFeatures   <- D$maxFeatures
  keepTestResult<- D$keepTestResult
  whichData     <- D$whichData
  overwriteOldDA<- D$overwriteOldDA
  whichMethods  <- D$whichMethods
  myTestInfo    <- D$myTestInfo
  maxRunTime    <- D$maxRunTime
  doMemoryCount <- D$doMemoryCount
  
  D$name           <- NULL
  D$design         <- NULL
  D$groupVariable  <- NULL
  D$doSlowMethods  <- NULL
  D$maxFeatures    <- NULL
  D$keepTestResult <- NULL
  D$whichData      <- NULL
  D$overwriteOldDA <- NULL
  D$whichMethods   <- NULL
  D$pfad           <- NULL
  D$myTestInfo     <- NULL
  D$maxRunTime     <- NULL
  D$doMemoryCount  <- NULL
  
  if(msb.attrCompare(D,"bstype","data_project")){
    
    for(j in 1:length(D)){ # loop over all data sets
      data_type <- NULL
      try(cat("DA for ",D[[j]]$project_label,"(",D[[j]]$datatype_label,")... \n"), silent=T)
      # try(sink(paste0(D[[j]]$project_label,"_",D[[j]]$datatype_label,".log")))
      
      if(any(msb.attrCompare(D[[j]],"bstype",whichData))){
        metaData <- D[[j]]$meta
        
        counts <- D[[j]]$counts[1:min(maxFeatures,nrow(D[[j]]$counts)),,drop=F]
        if(sum(is.na(counts))>0){
          saveRDS(counts,file="error.RDS")
          stop("NAs in counts.")
        }
        
        # Initialization of $DA (if there are old results, they are overwritten)
        if(overwriteOldDA || !"DA"%in%names(D[[j]])){
          D[[j]]$DA <- list()
          D[[j]]$DA$logFold   <- matrix(NA,nrow=nrow(counts),ncol=1, dimnames=list(rownames(counts),"dummy"))
          D[[j]]$DA$p_value    <- matrix(NA,nrow=nrow(counts),ncol=1, dimnames=list(rownames(counts),"dummy"))
          D[[j]]$DA$p_adjusted <- matrix(NA,nrow=nrow(counts),ncol=1, dimnames=list(rownames(counts),"dummy"))
        }
        
        ## 
        methDF <- data.frame(methName=NA,methFun="NA")
        
        if(!doSlowMethods && length(intersect(c("ancom","ancom_nearing","lefse","corncob"),tolower(whichMethods)))>0)
          warning("doSlowMethods=F: Slow methods are not run, although specified in whichMethods!!! Set doSlowMethods=T if you want to run it.")
        
        ## ALDEx2:
        if(length(intersect("aldex2",tolower(whichMethods)))>0)
          methDF <- rbind(methDF,data.frame(methName="ALDEx2",methFun="bs_aldex"))
        
        ## bs_ancom:
        if(doSlowMethods && length(intersect("ancom",tolower(whichMethods)))>0)
          methDF <- rbind(methDF,data.frame(methName="ancombc",methFun="bs_ancombc"))
        
        ## ancom_nearing:
        if(doSlowMethods && length(intersect("ancom_nearing",tolower(whichMethods)))>0)
          methDF <- rbind(methDF,data.frame(methName="ancom_Nearing",methFun="bs_ancom_Nearing"))
        
        ## DESeq:
        if(length(intersect("deseq",tolower(whichMethods)))>0)
          methDF <- rbind(methDF,data.frame(methName="DESeq",methFun="bs_DESeq"))
        
        ## glmmtmb - not for BS
        if(length(intersect("glmmtmb",tolower(whichMethods)))>0)
          methDF <- rbind(methDF,data.frame(methName="glmmTMB",methFun="bs_glmmTMB"))
        
        ## EdgeR:
        if(length(intersect("edger",tolower(whichMethods)))>0)
          methDF <- rbind(methDF,data.frame(methName="EdgeR",methFun="bs_edgeR"))
        
        #LefSe
        if(doSlowMethods && length(intersect("lefse",tolower(whichMethods)))>0)
          methDF <- rbind(methDF,data.frame(methName="bs_lefse",methFun="bs_lefse"))
        
        #LefSe
        if(doSlowMethods && length(intersect("lefse2",tolower(whichMethods)))>0)
          methDF <- rbind(methDF,data.frame(methName="LEfSe",methFun="bs_lefse2"))

        ## bs_limma_voom
        if(length(intersect("limma_voom",tolower(whichMethods)))>0)
          methDF <- rbind(methDF,data.frame(methName="limma_voom",methFun="bs_limma_voom"))
        
        ## bs_limma_voom_TMMwsp
        if(length(intersect("limma_voom_tmmwsp",tolower(whichMethods)))>0)
          methDF <- rbind(methDF,data.frame(methName="limma_voom_TMMwsp",methFun="bs_limma_voom_TMMwsp"))
        
        ## bs_Maaslin2
        if(length(intersect("maaslin2",tolower(whichMethods)))>0)
          methDF <- rbind(methDF,data.frame(methName="Maaslin2",methFun="bs_Maaslin2"))
        
        ## bs_Maaslin2_rare
        if(length(intersect("maaslin2_rare",tolower(whichMethods)))>0)
          methDF <- rbind(methDF,data.frame(methName="Maaslin2_rare",methFun="bs_Maaslin2_rare"))
        
        ## bs_metagenomeSeq
        if(length(intersect("metagenomeseq",tolower(whichMethods)))>0)
          methDF <- rbind(methDF,data.frame(methName="metagenomeSeq",methFun="bs_metagenomeSeq"))
        
        # t-test rare
        if(length(intersect("ttest_rare",tolower(whichMethods)))>0)
          methDF <- rbind(methDF,data.frame(methName="bs_ttest_rare.test",methFun="bs_ttest_rare"))
        
        # Wilcoxon CLR
        if(length(intersect("wilcox",tolower(whichMethods)))>0)
          methDF <- rbind(methDF,data.frame(methName="bs_wilcox.test",methFun="bs_wilcox.test"))
        
        # Wilcoxon rare
        if(length(intersect("wilcox_rare",tolower(whichMethods)))>0)
          methDF <- rbind(methDF,data.frame(methName="bs_wilcox_rare.test",methFun="bs_wilcox_rare.test"))
        
        ## corncob [is now called last, since exceeding maxRunTime unexpectedly stops all following tests. I dont understand this yet.]
        if(doSlowMethods && length(intersect("corncob",tolower(whichMethods)))>0)
          methDF <- rbind(methDF,data.frame(methName="corncob",methFun="bs_corncob"))
        
        
        ## Custom test functions:
        whichMethodsMy <- grep("^my",whichMethods,value = T)
        for(myMethod in whichMethodsMy){
          methDF <- rbind(methDF,data.frame(methName=myMethod,methFun=myMethod))
        }
        
        # remove 1st row (that was just for initialization the data.frame):
        methDF <- methDF[2:nrow(methDF), ] 
        
        ## Now evaluate all methods in methDF:
        for(im in 1:nrow(methDF)){
          meth_fun <- methDF[im,"methFun"]
          meth_name <- methDF[im,"methName"]
          saveRDS(methDF,"Test.methDF.RDS")
          saveRDS(D,"Test.D.RDS")
          
          tryCatch({
            cat(paste(meth_name,": ",meth_fun,"(...) ... \n"))
            
            ## Define DA-call function:
            doDA <- function(){
              if(length(grep("^my",meth_name))>0){  # my own tests
                res <- withTimeout({
                  get(meth_fun)(counts=counts, meta = metaData, groupVariable=groupVariable, myTestInfo=myTestInfo, DA=D[[j]]$DA)},
                  timeout = maxRunTime*60, onTimeout = "error")
              } else if(meth_fun=="bs_corncob"){ # corncob
                res <- withTimeout({
                  get(meth_fun)(counts=counts, meta = metaData, design=design, maxRunTimeInMin=maxRunTime)},
                  timeout = maxRunTime*60, onTimeout = "error")
              } else if(length(intersect(c("deseq"),tolower(meth_name)))>0){ # Methods that require the design variable (instead of groupVariable)
                res <- withTimeout({
                  get(meth_fun)(counts=counts, meta = metaData, design=design)},
                  timeout = maxRunTime*60, onTimeout = "error")
              } else {  # all other tests
                res <- withTimeout({
                  get(meth_fun)(counts=counts, meta = metaData, groupVariable=groupVariable)},
                  timeout = maxRunTime*60, onTimeout = "error")
              }
              return(res)
            }
            ## End function definition
            
            ## Call the function
            start_time <- Sys.time()
            if(doMemoryCount){
              gc() # garbage collection to make memory usage more interpretable
              library(profmem)
                prof <- profmem({
                  #########
                  # Main Call of DA:
                  res <- doDA()
                  #########
                  D[[j]]$DA_memory[meth_name] <- max(prof$bytes,na.rm=T)
                })
            }else{
              #########
              # Main Call of DA:
              res <- doDA()
              #########
              D[[j]]$DA_memory <- NULL
            }
            if(!"DA_runtime" %in% names(D[[j]]))
              D[[j]]$DA_runtime <- array()
            D[[j]]$DA_runtime[meth_name] <- as.numeric(Sys.time() - start_time) # in seconds

            if(keepTestResult)
              D[[j]]$DA[[meth_name]] <- res$result
            
            if(meth_name %in% colnames(D[[j]]$DA$p_value)){ # if existing: overwrite 
              D[[j]]$DA$logFold[,meth_name] <- res$logFoldChange
              D[[j]]$DA$p_value[,meth_name] <- res$pvalue
              D[[j]]$DA$p_adjusted[,meth_name] <- res$padj
            }else{ # if not yet existing: cbind
              D[[j]]$DA$logFold    <- cbind(D[[j]]$DA$logFold,res$logFoldChange)
              colnames(D[[j]]$DA$logFold)[ncol(D[[j]]$DA$logFold)] <- meth_name
              D[[j]]$DA$p_value     <- cbind(D[[j]]$DA$p_value, res$pvalue)
              colnames(D[[j]]$DA$p_value)[ncol(D[[j]]$DA$p_value)] <- meth_name
              D[[j]]$DA$p_adjusted  <- cbind(D[[j]]$DA$p_adjusted, res$padj)
              colnames(D[[j]]$DA$p_adjusted)[ncol(D[[j]]$DA$p_adjusted)] <- meth_name
            }
            
          }, error=function(e){
            cat(meth_name," failed.\n")
            D[[j]]$DA[[meth_name]] <<- paste0(name, meth_name,":  did not work: ",conditionMessage(e))
            D[[j]]$DA$logFold    <<- cbind(D[[j]]$DA$logFold,NA)
            colnames(D[[j]]$DA$logFold)[ncol(D[[j]]$DA$logFold)] <- meth_name
            D[[j]]$DA$p_value     <<- cbind(D[[j]]$DA$p_value, NA)
            colnames(D[[j]]$DA$p_value)[ncol(D[[j]]$DA$p_value)] <- meth_name
            D[[j]]$DA$p_adjusted  <<- cbind(D[[j]]$DA$p_adjusted, NA)
            colnames(D[[j]]$DA$p_adjusted)[ncol(D[[j]]$DA$p_adjusted)] <- meth_name
            saveRDS(msb.subset(D,j),file=paste0("error_",D[[j]]$project_label,"_",j,"_",meth_name,".RDS"))
          }) # End tryCatch
        }
        
        ## Eliminate dummy column:
        if("dummy" %in% colnames(D[[j]]$DA$logFold)){
          D[[j]]$DA$logFold    <- D[[j]]$DA$logFold[,-1,drop=F]
          D[[j]]$DA$p_value     <- D[[j]]$DA$p_value[,-1,drop=F]
          D[[j]]$DA$p_adjusted  <- D[[j]]$DA$p_adjusted[,-1,drop=F]
        }
        
        try(saveRDS(msb.subset(D,j),file=paste0("d",j,"_",D[[j]]$project_label,"__bs_DA.RDS")), silent=T)
        
      }
      
    } # if data_template or sim_result
    
  } # if data_project
  # try(sink())
  try(save(D,file="D.Rdata"))
  return(D)
  
}
