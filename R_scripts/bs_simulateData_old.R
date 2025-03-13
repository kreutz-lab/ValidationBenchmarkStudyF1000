#' @author Eva Kohnert
#' @names bs_simulateData_old
#' @description For each data template a simulated dataset, simulated with metaSPARSim or sparsedossa, is being added to input object. 
#' Simulation parameters are estimated from real data and can be changed
#' 
#' For having $meta in each simulation, bs_annotateData() has to be called
#' 
#' @param nsim Number of repeated simulations per data template
#' @param file If no output folder is specified output object is stored in ../Results
#' @param simParaChange "add_norm_noise" Adds normal noise to params$condition1/2$variability. Need to specifiy mean and sd of normal distribution from which noise is generated. This is being done for both conditions seperately, therefore mu1 & sd1 and mu2&sd2 need to be set.
#' @param runOption Default: "normal" 
#'                  if set to "fast", then (for code testing purpose) simulation is skipped and the data template is just duplicated 
#' @param simulator (upper/lower case spelling does not matter)
#'        "metasparsim" 
#'        "sparsedossa2"  
#'        "metasparsim_nonreg": this is metasparsim but intensities are set equal for a proper amount of samples, see bs_setUnregulatedParams()
#' @param paramsWorkspace Workspace used to load simulation parameters if runoption="fast"
#'        Default: NULL. This means that NULL is given to the subfunctions bs_paramsDefault* and the default defined there is used.
#'        
#' @example
#' files.sources= list.files("../R_functions", pattern="*.R$", full.names=TRUE)
#' sapply(files.sources, source)
#' data_to_compare <- bs_getData(files.names =  c("ArcticFireSoils", "wood_plastic_kesy", "ob_goodrich","crc_zeller"), nfeature = 100)
#' data_to_compare <- bs_simulateData_old(data_to_compare, nsim=2, simParaChange = "normal_to_phi", simParaChangeList=list(mu1=0.1, sd1=0.0001, mu2=0.15, sd2=0.0001))
#' data_to_compare <- bs_simulateData_old(data_to_compare, nsim=2, runOption="fast")
#' data_to_compare <- bs_simulateData_old(data_to_compare, nsim=1, simulator="sparsedossa2")
#' 
#' d <- readRDS(file="../data_to_compare_oneSimulation.RDS")
#' D <- msb.subset(d,4)
#' D2 <- bs_simulateData_old(D,simulator="metasparsim_nonreg", runOption="fast")


# The function has been called get_metaSPARSim before, this definition
# enables backward compatibility
get_metaSPARSim <- function(...){
  return(bs_simulateData_old(...))
}


## 
bs_simulateData_old <- function(data_to_compare, nsim=1, 
                            file=".", 
                            simParaChange=NULL, 
                            simParaChangeList=NULL,
                            runOption="normal",
                            simulator="metasparsim",
                            parallelMode = T,paramsWorkspace=NULL,
                            maxNcore=100){
  
  if(bs_isa(data_to_compare,"data_project"))
    warning("bs_simulateData_old should be called with a list of data_projects, i.e. a subset of data_to_compare.
            Try to call via bs_simulate(list(D), ...).")
  # require(doParallel)
  
  simulator <- tolower(simulator) # enforce lower case 
  if(simulator!="metasparsim" && simulator!="sparsedossa2" && simulator!="metasparsim_nonreg") 
    stop(paste0("As simulator, only metasparsim or sparsedossa2 implemented, but ",simulator," has been chosen."))
  
  if(nsim>1){
    if(file.exists("bs_simulateData_old_foreach.log")) 
      file.remove("bs_simulateData_old_foreach.log")
    
    for(isim in 1:nsim){
      cat(paste0("\n", "Simulation No. ",isim,"\n"))
      data_to_compare <- bs_simulateData_old(data_to_compare, nsim=1, 
                                         file=NULL,simParaChange=simParaChange, 
                                         simParaChangeList=simParaChangeList,runOption=runOption,
                                         simulator=simulator,parallelMode=parallelMode,
                                         paramsWorkspace=paramsWorkspace)
      # try(save(list="data_to_compare",file=paste0("foreach_bs_simulate_isim",isim,".Rdata")))
    }
    
    if(!is.null(file)){
      dir.create(paste0(file),showWarnings = FALSE)
      saveRDS(data_to_compare, paste0(file,"/data_to_compare.RDS"))
    }
    
    return(data_to_compare)
    
  } # end of nsim > 1
  else {  # nsim=1: oerform a simulation
    
    pfad <- getwd() # paste0(getwd(),"/",file)
    data_to_compare <- bs_decompress(data_to_compare)
    
    for(index in 1:length(data_to_compare)){
      data_to_compare[[index]]$index <- index 
      data_to_compare[[index]]$pfad <- pfad
      data_to_compare[[index]]$simulator <- simulator
      data_to_compare[[index]]$runOption <- runOption
      data_to_compare[[index]]$parallelMode <- parallelMode
      data_to_compare[[index]]$bs_simulate_LoopCore_old <- bs_simulate_LoopCore_old
      data_to_compare[[index]]$simParaChange <- simParaChange
      data_to_compare[[index]]$simParaChangeList <- simParaChangeList
      data_to_compare[[index]]$paramsWorkspace <- paramsWorkspace
      name <- names(data_to_compare)[index]
      name <- name[1:min(10,length(name))]
      if(is.null(name))
        name = "name"
      forRNG <- 1000*abs(as.numeric(Sys.time())*1000 - round(as.numeric(Sys.time())*1000)) # integer dependent on time
      data_to_compare[[index]]$rngInit <- sum(as.integer(charToRaw(name)))+as.integer(forRNG) # add integer dependent on template name
    }
    
    
    ## Option1: parallel code version:
    if(parallelMode){
      # parallel loop over all datasets
      msb.registerDoParallel(min(maxNcore,min(parallel::detectCores()-1,length(data_to_compare))))
      # registerDoParallel(cores = min(parallel::detectCores(),length(data_to_compare)))
      
      result <- foreach(D=data_to_compare) %dopar% {
        # tryCatch({
          # source("bs_simulateData_old.R")
          
          D <- D$bs_simulate_LoopCore_old(D)
        # }, error=function(e){
        #   try(save(list="D",file=paste0("Error_index",D$index,".Rdata")))
        #   cat("Error (index=",D$index,"): ",conditionMessage(e),"\n",file="bs_simulateData_old_foreach.log",append = T)
        # })
        return(D)
      }
      # doParallel::stopImplicitCluster()
      msb.unregisterDoParallel()    
    } 
    else { # Option2: non-parallel calculation:
      
      result <- foreach(D=data_to_compare,.packages = c('metaSPARSim')) %do% {      
        # tryCatch({
          D <- D$bs_simulate_LoopCore_old(D)
        # }, error=function(e){
        #   try(save(list="D",file=paste0("Error_index",D$index,".Rdata")))
        #   cat("Error (index=",D$index,"): ",conditionMessage(e),"\n",file="bs_simulateData_old_foreach.log",append = T)
        # })
        return(D)
      }
    }
    
    # remove fields which were introduced above for being available in %dopar%:
    for(index in 1:length(result)){
      result[[index]]$index <- NULL 
      result[[index]]$pfad <- NULL
      result[[index]]$simulator <- NULL
      result[[index]]$runOption <- NULL
      result[[index]]$parallelMode <- NULL
      result[[index]]$bs_simulate_LoopCore_old <- NULL
      result[[index]]$simParaChange <- NULL
      result[[index]]$simParaChangeList <- NULL
      result[[index]]$paramsWorkspace <- NULL
      result[[index]]$rngInit <- NULL
    }
    
    names(result) <- names(data_to_compare) # foreach returns a list but without names: copy them manually
    
    ## ordinary for loop would look like this:
    #result <- list()
    #for(index in 1:length(data_to_compare)){
    #  D <- data_to_compare[[index]]
    
    # attribute bstype is missing
    attr(result, "bstype") <- "data_list"
    
    return(result)
    
  } # end of nsim==1
  
} # end of bs_simulateData_old()



## The following function will be called either in a %dopar% or in a %do% loop:
bs_simulate_LoopCore_old <- function(D){
  tryCatch({
    pfad <- D$pfad
    runOption <- D$runOption
    simulator <- D$simulator
    simParaChange <- D$simParaChange 
    simParaChangeList <- D$simParaChangeList
    paramsWorkspace <- D$paramsWorkspace
    
    source("bs_simulateData_old.R")
    
    cat(pfad,"\n",file="bs_simulateData_old_foreach.log",append = T)
    setwd(pfad)
    # save(list=c("D","simulator","runOption","bs_path","pfad"),file=paste0("bs_simulate_index",D$index,".Rdata"))
    
    set.seed(D$rngInit)
    cat("D$rngInit",D$rngInit,"\n",file="bs_simulateData_old_foreach.log",append = T)
    
    ### Wird ersetzt durch project_init
    #bs_init() # ensure, all bs function are known, bs_init is made available via .Rprofile
    source("project_init.R")
    # project_init()
    
    if(D$parallelMode){
      cat("One parallel job started ...",as.character(format(Sys.time(), "%X  %b %d %Y")),"\n",file="bs_simulateData_old_foreach.log",append = T)
    }else{
      cat("One job started (non-parallel model) ...",as.character(format(Sys.time(), "%X  %b %d %Y")),"\n",file="bs_simulateData_old_foreach.log",append = T)
    }
    
    # if(!require(SparseDOSSA2))
    #    bs_sparseDossaInit() # here, sparseDOSSA2 is sourced manually
    
    # Get for each condition the corresponding column indices (samples)
    conditions <- split(seq_along(D$original$meta$condition), D$original$meta$condition)
    raw_data=D$original$counts
    
    norm_data=D$original$counts.norm
    
    if(runOption=="fast"){
      if(simulator=="metasparsim" || simulator=="metasparsim_nonreg"){
        cat("Call bs_paramsDefault_Metasparsim ... (index=",D$index,")\n",file="bs_simulateData_old_foreach.log",append = T)
        params <- bs_paramsDefault_Metasparsim(D,paramsWorkspace = paramsWorkspace) # loading from workspace
      }
      if(simulator=="sparsedossa2"){
        cat("Call bs_paramsDefault_SparseDOSSA2 ... (index=",D$index,")\n",file="bs_simulateData_old_foreach.log",append = T)
        params <- bs_paramsDefault_SparseDOSSA2(D,paramsWorkspace=paramsWorkspace) # loading from workspace
      }
    }else{
      if(simulator=="metasparsim" || simulator=="metasparsim_nonreg"){
        cat("Call estimate_parameter_from_data ...(index=",D$index,")\n",file="bs_simulateData_old_foreach.log",append = T)
        start_time <- Sys.time()
        params <- NULL
        try(params <- estimate_parameter_from_data(raw_data=raw_data, norm_data=norm_data, conditions=conditions, intensity_func = "mean", keep_zeros = TRUE))
        if(is.null(params)) # if failed, try to set perc_not_zeros=0
          params <- estimate_parameter_from_data(raw_data=raw_data, norm_data=norm_data, conditions=conditions, intensity_func = "mean", keep_zeros = TRUE, perc_not_zeros=0)
        for(name in names(params)){
          params[[name]]$start_time <- start_time       
          params[[name]]$end_time <- Sys.time()
          params[[name]]$run_time <- (as.numeric(params$end_time)-as.numeric(params$start_time))/length(names(params))
        }
      }
      if(simulator=="sparsedossa2"){
        cat("Call bs_paramsDefault_SparseDOSSA2 ...(index=",D$index,")\n",file="bs_simulateData_old_foreach.log",append = T)
        params <- bs_paramsDefault_SparseDOSSA2(D,paramsWorkspace=paramsWorkspace) # loading from workspace
      }
    }
    
    if(simulator=="metasparsim_nonreg"){
      ## How many features are regulated?
      print("Running bs_edgeR to get p-values ...")
      pvals <- bs_edgeR(D$original$counts,D$original$meta,"condition")$pvalue
      
      # account for zero-inflation by calculating the proportion of regulated features after filtering w.r.t zeros (with different thresholds)
      thresh <- seq(0.1,1,by=0.05)
      anzReg <- array()
      isRegList <- list()
      forSave <- list()
      for(i in 1:length(thresh)){
        prop0 <- rowSums(D$original$counts==0)/dim(D$original$counts)[2]
        irow <- prop0<thresh[i]
        if(sum(irow)>10){
          isRegList[[i]] <- msb.sampleDiffRegFromPvalues(pvals[irow])
          anzReg[i] <- sum(isRegList[[i]])
        }
        else{
          isRegList[[i]] <- NA
          anzReg[i] <- NA
        }
        # forSave[[i]] <- list(pall=p, prop0=prop0, p=pvals[irow],isReg=isRegList[[i]],anzReg=anzReg[i])
        #hist(pvals[irow])
      }
      #cat(anzReg)
      # saveRDS(forSave,file="forSave.RDS")
      
      #          isReg <- msb.sampleDiffRegFromPvalues(pvals)
      isReg <- isRegList[[which.max(anzReg)]]
      cat(sum(isReg)," features are regulated (",sum(isReg)/length(isReg)*100,"%)\n")
      params <- bs_setUnregulatedParams(params,simulator=simulator,notRegulated=!isReg)
      params[[1]]$isReg <- isReg
      params[[2]]$isReg <- isReg
      
    }
    
    if(!is.list(params)) 
      stop("!is.list(params) in bs_simulate: loading/estimating simulation parameters failed.")
    attr(params,"bstype") <- "sim_para"
    
    # Change simulations parameter if intended
    if(!is.null(simParaChange)){
      if(is.null(simParaChangeList)){
        cat("simParaChange has been set, need to provide simParaChangeList (index=",D$index,")\n",file="bs_simulateData_old_foreach.log",append = T)
        stop("simParaChange has been set, need to provide simParaChangeList.")
      }
      params <- bs_doSimParaChange(params,simParaChange=simParaChange,simParaChangeList = simParaChangeList)
    }
    
    # Simulate data
    # if(runOption=="fast"){
      if(simulator=="metasparsim" || simulator=="metasparsim_nonreg"){ # metaSPARSim:
        sim_data <- metaSPARSim(params) #D$original
      }
      if(simulator=="sparsedossa2"){ # SparseDOSSA:
        sim_data <- bs_SparseDOSSA2(template=params,new_features=F)
      }
    # }
    # else{
    #   if(simulator=="metasparsim" || simulator=="metasparsim_nonreg"){ # metaSPARSim:
    #     sim_data <- metaSPARSim(params)
    #   }
    #   if(simulator=="sparsedossa2"){ # SparseDOSSA:
    #     # simresult <- SparseDOSSA2::SparseDOSSA2(template=params) # does only work with the SparseDOSSA2 package, not with bs_sparseDossaInit()
    #     simresult <- SparseDOSSA2(template=params,new_features=F, n_sample=length(params$EM_fit$l_filtering$ind_sample), n_feature=length(params$EM_fit$l_filtering$ind_feature))
    #     sim_data <- list(counts=simresult$simulated_data, params=params,metadata=simresult$spike_metadata)
    #   } 
    # }
    
    attr(sim_data,"bstype") <- "sim_data"
    # The following should be done via 
    # d < bs_filterPrevalence(d,1) 
    # if required (otherwise the row(counts)!=length(params$intensity) etc)
    # # Remove features that are completly zero
    # sim_data$counts <- sim_data$counts[!rowSums(sim_data$counts)==0,]
    attr(sim_data$counts,"bstype") <- "sim_counts"
    
    sim.results.tmp <- list("counts"=sim_data$counts, "gamma"=sim_data$gamma, "params"=sim_data$params)
    attr(sim.results.tmp,"bstype") <- "sim_result"
    
    # Make simulation names distinct
    # CK: In order to prevent accidental overwriting: 
    if(simulator=="metasparsim")
      sim_name <- paste0("metaSPARSim_",1+max(0,msb.ExtractTailingNumbers(names(D),pattern="metaSPARSim")))
    if(simulator=="metasparsim_nonreg")
      sim_name <- paste0("metaSPARSimNonReg_",1+max(0,msb.ExtractTailingNumbers(names(D),pattern="metaSPARSimNonReg")))
    if(simulator=="sparsedossa2")
      sim_name <- paste0("sparseDOSSA2_",1+max(0,msb.ExtractTailingNumbers(names(D),pattern="sparseDOSSA2")))
    
    # sim_name <- paste0("metaSPARSim_",length(D)-1)
    D[[sim_name]] <- sim.results.tmp
    attr(D[[sim_name]],"bstype") <- "sim_result"          ### Braucht man das an dieser Stelle noch?
    
    cat("1st 10 simulated counts:\n",sim_data$counts[1:10],"\n",file="bs_simulateData_old_foreach.log",append = T)
    
    ## parallel code version:
    return(D)
    ## non parallel code version:
    # result[[index]] <- D
    
  }, error=function(e){
    try(save(list="D",file=paste0("Error_bs_simulate_LoopCore_old_index",D$index,".Rdata")))
    cat("Error (index=",D$index,"): ",conditionMessage(e),"\n",file="bs_simulate_LoopCore_old_error.log",append = T)
    return(D)
  })
  
}

