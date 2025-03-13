#' @author Eva Kohnert
#' @names bs_simulateData
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
#'        "metasparsim_partReg": this is metasparsim but for a proper proportion, the simulated parameters are estimated from all samples (non-regulated features) 
#'        the others are taken by estimating parameters in sample grous independently (regulated features), see bs_setUnregulatedParams()
#' @param paramsWorkspace Workspace used to load simulation parameters if runoption="fast"
#'        Default: NULL. This means that NULL is given to the subfunctions bs_paramsDefault* and the default defined there is used.
#' @param propReg Proportion of regulated features, if it should be set from outside (only applies if simu method can deal with it)
#'        
#' @example
#' files.sources= list.files("../R_functions", pattern="*.R$", full.names=TRUE)
#' sapply(files.sources, source)
#' data_to_compare <- bs_getData(files.names =  c("ArcticFireSoils", "wood_plastic_kesy", "ob_goodrich","crc_zeller"), nfeature = 100)
#' data_to_compare <- bs_simulateData(data_to_compare, nsim=2, simParaChange = "normal_to_phi", simParaChangeList=list(mu1=0.1, sd1=0.0001, mu2=0.15, sd2=0.0001))
#' data_to_compare <- bs_simulateData(data_to_compare, nsim=2, runOption="fast")
#' data_to_compare <- bs_simulateData(data_to_compare, nsim=1, simulator="sparsedossa2")
#' 
#' d <- readRDS(file="../data_to_compare_oneSimulation.RDS")
#' D <- msb.subset(d,4)
#' D2 <- bs_simulateData(D,simulator="metasparsim_partReg", runOption="fast")
#'
#' # Simulation of partly regulated data:
#' d9 <- msb.subset(d,9)
#' tmp <- bs_simulateData(d9, nsim=1, simulator="metasparsim_partReg", file=".", parallelMode = F, paramsWorkspace = paramsWorkspace)
#'
#'

# The function has been called get_metaSPARSim before, this definition
# enables backward compatibility
get_metaSPARSim <- function(...){
  return(bs_simulateData(...))
}


## 
bs_simulateData <- function(data_to_compare, nsim=1, 
                            file=".", 
                            simParaChange=NULL, 
                            simParaChangeList=NULL,
                            runOption="normal",
                            simulator="metasparsim",
                            parallelMode = T,paramsWorkspace=NULL,
                            maxNcore=100,propReg=NULL){
  
  
  if(bs_isa(data_to_compare,"data_project"))
    warning("bs_simulateData should be called with a list of data_projects, i.e. a subset of data_to_compare.
            Try to call via bs_simulate(list(D), ...).")
  # require(doParallel)
  
  simulator <- tolower(simulator) # enforce lower case 
  if(!simulator %in% c("metasparsim","metasparsim_partreg","metasparsim_nonreg","sparsedossa2","midasim","midasim_partreg")) 
    stop(paste0("As simulator, only metasparsim or sparsedossa2 implemented, but ",simulator," has been chosen."))
  
  if(!is.null(propReg) && simulator!="metasparsim_partreg")
    stop("simualtor=",simulator, "cannot handle propReg=",propReg)
  
  if(nsim>1){
    if(file.exists("bs_simulateData_foreach.log")) 
      file.remove("bs_simulateData_foreach.log")
    
    for(isim in 1:nsim){
      cat(paste0("\n", "Simulation No. ",isim,"\n"))
      data_to_compare <- bs_simulateData(data_to_compare, nsim=1, 
                                         file=NULL,simParaChange=simParaChange, 
                                         simParaChangeList=simParaChangeList,runOption=runOption,
                                         simulator=simulator,parallelMode=parallelMode,
                                         paramsWorkspace=paramsWorkspace, propReg=propReg)
      # try(save(list="data_to_compare",file=paste0("foreach_bs_simulate_isim",isim,".Rdata")))
    }
    
    if(!is.null(file)){
      dir.create(paste0(file),showWarnings = FALSE)
      saveRDS(data_to_compare, paste0(file,"/data_to_compare.RDS"))
    }
    
    return(data_to_compare)
    
  } # end of nsim > 1
  else {  # nsim=1: perform a simulation
    
    pfad <- getwd() # paste0(getwd(),"/",file)
    data_to_compare <- bs_decompress(data_to_compare)
    
    for(index in 1:length(data_to_compare)){
      data_to_compare[[index]]$index <- index 
      data_to_compare[[index]]$pfad <- pfad
      data_to_compare[[index]]$simulator <- simulator
      data_to_compare[[index]]$runOption <- runOption
      data_to_compare[[index]]$parallelMode <- parallelMode
      data_to_compare[[index]]$bs_simulate_LoopCore <- bs_simulate_LoopCore
      data_to_compare[[index]]$simParaChange <- simParaChange
      data_to_compare[[index]]$simParaChangeList <- simParaChangeList
      data_to_compare[[index]]$paramsWorkspace <- paramsWorkspace
      data_to_compare[[index]]$propReg <- propReg
      data_to_compare[[index]]$bsLibPath_used <- bsLibPath_used
      try(
        data_to_compare[[index]]$partreg_pvals <- data_to_compare[[index]]$original$partreg_pvals
      )
      
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
        tryCatch({
          D <- D$bs_simulate_LoopCore(D)
        }, error=function(e){
          try(save(list="D",file=paste0("Error_index",D$index,".Rdata")))
          cat("Error (index=",D$index,"): ",conditionMessage(e),"\n",file="bs_simulateData_foreach.log",append = T)
        })
        return(D)
      }
      # doParallel::stopImplicitCluster()
      msb.unregisterDoParallel()    
    } 
    else { # Option2: non-parallel calculation:
      
      result <- foreach(D=data_to_compare,.packages = c('metaSPARSim')) %do% {      
        tryCatch({
          D <- D$bs_simulate_LoopCore(D)
        }, error=function(e){
          try(save(list="D",file=paste0("Error_index",D$index,".Rdata")))
          cat("Error (index=",D$index,"): ",conditionMessage(e),"\n",file="bs_simulateData_foreach.log",append = T)
        })
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
      result[[index]]$bs_simulate_LoopCore <- NULL
      result[[index]]$simParaChange <- NULL
      result[[index]]$simParaChangeList <- NULL
      result[[index]]$paramsWorkspace <- NULL
      result[[index]]$rngInit <- NULL
      result[[index]]$propReg <- NULL
      result[[index]]$bsLibPath_used <- NULL
      
      # If available, pvals is copied to $original from the first simu to save time 
      # (otherwise DA methods have to be applied for each simu)
      if(!"partreg_pvals" %in% names(result[[index]]$original)){
        try(result[[index]]$original$partreg_pvals <- result[[index]][[2]]$params[[1]]$pvals,silent = T)
      }
        
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
  
} # end of bs_simulateData()



## The following function will be called either in a %dopar% or in a %do% loop:
bs_simulate_LoopCore <- function(D){
  tryCatch({
    pfad <- D$pfad
    runOption <- D$runOption
    simulator <- D$simulator
    simParaChange <- D$simParaChange 
    simParaChangeList <- D$simParaChangeList
    paramsWorkspace <- D$paramsWorkspace
    propReg <- D$propReg
    
    cat(pfad,"\n",file="bs_simulateData_foreach.log",append = T)
    setwd(pfad)
    # save(list=c("D","simulator","runOption","bs_path","pfad"),file=paste0("bs_simulate_index",D$index,".Rdata"))
    
    set.seed(D$rngInit)
    cat("D$rngInit",D$rngInit,"\n",file="bs_simulateData_foreach.log",append = T)
    
    ### Wird ersetzt durch project_init
    #bs_init() # ensure, all bs function are known, bs_init is made available via .Rprofile
    source(paste0(bsLibPath,"/../R_scripts/project_init.R"))
    #project_init()
    # project_init(bsLibPath=D$bsLibPath_used)

    if(D$parallelMode){
      cat("One parallel job started ...",as.character(format(Sys.time(), "%X  %b %d %Y")),"\n",file="bs_simulateData_foreach.log",append = T)
    }else{
      cat("One job started (non-parallel model) ...",as.character(format(Sys.time(), "%X  %b %d %Y")),"\n",file="bs_simulateData_foreach.log",append = T)
    }
    
    # if(!require(SparseDOSSA2))
    #    bs_sparseDossaInit() # here, sparseDOSSA2 is sourced manually
    
    # Get for each condition the corresponding column indices (samples)
    conditions <- split(seq_along(D$original$meta$condition), D$original$meta$condition)
    conditionsAll <- list(all=1:nrow(D$original$meta))
    
    raw_data=D$original$counts
    norm_data=D$original$counts.norm
    
    cat(paste0("bs_simulate.R: simulator=",simulator,", runOption=",runOption),"\n")
    if(runOption=="fast"){ 
      # load params from workspace:
      if(simulator=="metasparsim"){
        cat("Call bs_paramsDefault_Metasparsim ... (index=",D$index,")\n",file="bs_simulateData_foreach.log",append = T)
        params <- bs_paramsDefault_Metasparsim(D,paramsWorkspace = paramsWorkspace) # loading from workspace
      }else if(simulator=="metasparsim_partreg"){
        cat("Call bs_paramsDefault_Metasparsim ... (index=",D$index,")\n",file="bs_simulateData_foreach.log",append = T)
        tmp <- bs_paramsDefault_Metasparsim(D,paramsWorkspace = paramsWorkspace["reg"]) # loading from workspace
        tmp2 <- bs_paramsDefault_Metasparsim(D,paramsWorkspace = paramsWorkspace["nonreg"]) # loading from workspace
        params <- list(reg=tmp,nonreg=tmp2)
      }else if(simulator=="sparsedossa2"){
        cat("Call bs_paramsDefault_SparseDOSSA2 ... (index=",D$index,")\n",file="bs_simulateData_foreach.log",append = T)
        params <- bs_paramsDefault_SparseDOSSA2(D,paramsWorkspace=paramsWorkspace) # loading from workspace
      }else if(simulator=="midasim"){
        cat("Call bs_calibrate_MIDASim ... (index=",D$index,")\n",file="bs_simulateData_foreach.log",append = T)
        params <- bs_calibrate_MIDASim(D) # loading from workspace
      }else if(simulator=="midasim_partreg"){
        cat("Call bs_calibrate_MIDASim ... (index=",D$index,") for all samples ...\n",file="bs_simulateData_foreach.log",append = T)
        paramsAll <- bs_calibrate_MIDASim(D,conditions = conditionsAll) # loading from workspace
        cat("Call bs_calibrate_MIDASim ... (index=",D$index,") for individual conditions ...\n",file="bs_simulateData_foreach.log",append = T)
        params <- bs_calibrate_MIDASim(D,conditions = conditions) # loading from workspace
      }
    }else{ # no "fast" option
      #
      paramsAll <- NULL
      if(simulator=="metasparsim") {
        cat("Call bs_estimate_parameter_from_data ...(index=",D$index,")\n",file="bs_simulateData_foreach.log",append = T)
        params <- bs_estimate_parameter_from_data(raw_data=raw_data, norm_data=norm_data, conditions=conditions, intensity_func = "mean", keep_zeros = TRUE)
      }else if(simulator=="metasparsim_partreg"){
        cat("Call bs_estimate_parameter_from_data ...(index=",D$index,")\n",file="bs_simulateData_foreach.log",append = T)
        params <- bs_estimate_parameter_from_data(raw_data=raw_data, norm_data=norm_data, conditions=conditions, intensity_func = "mean", keep_zeros = TRUE)
        # Estimate params for all samples in addition
        cat("Call bs_estimate_parameter_from_data ...(index=",D$index,")\n",file="bs_simulateData_foreach.log",append = T)
        paramsAll <- bs_estimate_parameter_from_data(raw_data=raw_data, norm_data=norm_data, conditions=conditionsAll, intensity_func = "mean", keep_zeros = TRUE)
      }else if(simulator=="midasim"){
        cat("Call bs_calibrate_MIDASim ... (index=",D$index,")\n",file="bs_simulateData_foreach.log",append = T)
        params <- bs_calibrate_MIDASim(D) # loading from workspace
      }else if(simulator=="midasim_partreg"){
        cat("Call bs_calibrate_MIDASim ... (index=",D$index,") for all samples ...\n",file="bs_simulateData_foreach.log",append = T)
        paramsAll <- bs_calibrate_MIDASim(D,conditions = conditionsAll) # loading from workspace
        cat("Call bs_calibrate_MIDASim ... (index=",D$index,") for individual conditions ...\n",file="bs_simulateData_foreach.log",append = T)
        params <- bs_calibrate_MIDASim(D,conditions = conditions) # loading from workspace
      }else{
        stop("bs_simulateData.R: Simulator unknown")
      }
      
      if(simulator=="sparsedossa2"){
        cat("Call bs_paramsDefault_SparseDOSSA2 ...(index=",D$index,")\n",file="bs_simulateData_foreach.log",append = T)
        params <- bs_paramsDefault_SparseDOSSA2(D,paramsWorkspace=paramsWorkspace) # loading from workspace
      }
    }
    
    ## Calibrate again (for all samples) if partial regulation and adapt simulation params:
    if(simulator %in% c("metasparsim_partreg", "midasim_partreg", "midasim_partreg")){
      params <- bs_calibrate_simulator_partReg(D,params, paramsAll, simulator=simulator, propReg=propReg,runOption=runOption)
    }
    
    if(!is.list(params)) 
      stop("!is.list(params) in bs_simulate: loading/estimating simulation parameters failed.")
    attr(params,"bstype") <- "sim_para"
    
    # Change simulations parameter if intended
    if(!is.null(simParaChange)){
      if(is.null(simParaChangeList)){
        cat("simParaChange has been set, need to provide simParaChangeList (index=",D$index,")\n",file="bs_simulateData_foreach.log",append = T)
        stop("simParaChange has been set, need to provide simParaChangeList.")
      }
      params <- bs_doSimParaChange(params,simParaChange=simParaChange,simParaChangeList = simParaChangeList)
    }
    
    # Simulate data
    # if(runOption=="fast"){
    if(simulator=="metasparsim" || simulator=="metasparsim_partreg"){ # metaSPARSim:
      sim_data <- metaSPARSim(params) #D$original
      sim_data <- list("counts"=sim_data$counts, "gamma"=sim_data$gamma, "params"=sim_data$params)
    }else if(simulator=="sparsedossa2"){ # SparseDOSSA:
      sim_data <- bs_SparseDOSSA2(template=params,new_features=F)
    }else if(simulator=="midasim_partreg" || simulator=="midasim"){
      sim_data <- bs_MIDASim(params=params)
    }else{
      stop("Simulator ",simulator,"unknown.")
    }
    
    attr(sim_data$counts,"bstype") <- "sim_counts"
    attr(sim_data,"bstype") <- "sim_result"
    
    # The following should be done via 
    # d < bs_filterPrevalence(d,1) 
    # if required (otherwise the row(counts)!=length(params$intensity) etc)
    # # Remove features that are completly zero
    # sim_data$counts <- sim_data$counts[!rowSums(sim_data$counts)==0,]
    
    # Make simulation names distinct
    # CK: In order to prevent accidental overwriting: 
    sim_name <- "noName"
    if(simulator=="metasparsim")
      sim_name <- paste0("metaSPARSim_",1+max(0,msb.ExtractTailingNumbers(names(D),pattern="metaSPARSim")))
    if(simulator=="metasparsim_partreg")
      sim_name <- paste0("metaSPARSimNonReg_",1+max(0,msb.ExtractTailingNumbers(names(D),pattern="metaSPARSimNonReg")))
    if(simulator=="sparsedossa2")
      sim_name <- paste0("sparseDOSSA2_",1+max(0,msb.ExtractTailingNumbers(names(D),pattern="sparseDOSSA2")))
    if(simulator=="midasim")
      sim_name <- paste0("MIDASim",1+max(0,msb.ExtractTailingNumbers(names(D),pattern="sparseDOSSA2")))
    if(simulator=="midasim_partreg")
      sim_name <- paste0("MIDASimPartReg_",1+max(0,msb.ExtractTailingNumbers(names(D),pattern="sparseDOSSA2")))
    
    # sim_name <- paste0("metaSPARSim_",length(D)-1)
    D[[sim_name]] <- sim_data
    cat("1st 10 simulated counts:\n",sim_data$counts[1:10],"\n",file="bs_simulateData_foreach.log",append = T)
    
    ## parallel code version:
    return(D)
    ## non parallel code version:
    # result[[index]] <- D
    
  }, error=function(e){
    print(D$index)
    try(save(list="D",file=paste0("Error_bs_simulate_LoopCore_index",D$index,".Rdata")), silent=T)
    cat("Error (index=",D$index,"): ",conditionMessage(e),"\n",file="bs_simulate_LoopCore_error.log")
    return(D)
  })
  
}

