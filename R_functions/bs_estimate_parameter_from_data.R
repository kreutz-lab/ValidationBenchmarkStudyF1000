# This is a wrapper, i.e. a more robust version of metaSPARSim::estimate_parameter_from_data 

bs_estimate_parameter_from_data <- function(raw_data,norm_data,conditions,intensity_func = "mean", keep_zeros = TRUE){
  
  start_time <- Sys.time()
  
  params <- NULL
  
  for(i in 1:length(conditions))
    if(length(conditions[[i]])<2)
      stop("bs_estimate_parameter_from_data: metaSPARsim requires at least 2 samples per conditions.\n")
  
  # use package function
  msb.tryCatch(expr=quote({
    params <- metaSPARSim::estimate_parameter_from_data(raw_data=raw_data, 
                                                        norm_data=norm_data, 
                                                        conditions=conditions, 
                                                        intensity_func = "mean", 
                                                        keep_zeros = TRUE)
    }))
  
  if(is.null(params)){
    warning(" metaSPARSim::estimate_parameter_from_data failed, try to repeat by removing empty rows and columns ...\n")
    tmp <- bs_filterOutZeroRowsAndCols(raw_data)
    usedRows <- tmp$row_indices 
    usedCols <- tmp$col_indices 
    
    # filtered conditions, transform indices in conditions$... to new condition indices after columns might be filtered out:
    condBool <- list()
    condNew <- list()
    for(name in names(conditions)){
      condBool[[name]] <- array(FALSE,dim=ncol(raw_data))
      condBool[[name]][conditions[[name]]] <- T
      
      condNew[[name]] <- which(condBool[[name]][usedCols])
    }

    msb.tryCatch(expr=quote({
      params <- msp_estimate_parameter_from_data(raw_data=raw_data[usedRows,usedCols], norm_data=norm_data[usedRows,usedCols], conditions=condNew, intensity_func = "mean", keep_zeros = TRUE)
    }))
  }
  
  if(is.null(params)) # if failed, try to set perc_not_zeros=0
    params <- msp_estimate_parameter_from_data(raw_data=raw_data, norm_data=norm_data, conditions=conditions, intensity_func = "mean", keep_zeros = TRUE, perc_not_zeros=0)

  
  for(name in names(params)){
    params[[name]]$start_time <- start_time       
    params[[name]]$end_time <- Sys.time()
    params[[name]]$run_time <- (as.numeric(params$end_time)-as.numeric(params$start_time))/length(names(params))
  }
  attr(params,"bstype") <- "sim_para"
  
  return(params)
}



msp_estimate_parameter_from_data <- function (raw_data, norm_data, conditions, intensity_func = "mean", 
          keep_zeros = TRUE, perc_not_zeros = 0.2) 
{
  N_cond <- length(conditions)
  N_feature <- nrow(raw_data)
  dataset_parameter <- list()
  if (any(lengths(conditions) <= 1)) {
    stop("The conditions should have at least two samples per group.")
  }
  cond_i <- 1
  for (cond in conditions) {
    cond_parameter <- list()
    cond_parameter$intensity <- metaSPARSim::estimate_intensity(data = norm_data[, cond, drop = FALSE], 
                                                    aggregate_func = intensity_func, keep_zeros = keep_zeros)

    variability <- NULL
    try(
      variability <- metaSPARSim::estimate_variability(data = raw_data[, cond, drop = FALSE], 
                                                    variability_func = "dispersion", perc_not_zeros = perc_not_zeros)
    )
    if(is.null(variability)){
      warning(" metaSPARSim::estimate_variability with variability_func = dispersion failed, try to repeat with variability_func = variance ...\n")
      variability <- metaSPARSim::estimate_variability(data = raw_data[,cond, drop = FALSE], 
                                                    variability_func = "variance", perc_not_zeros = perc_not_zeros)
          }
    
    cond_parameter$variability <- variability
    cond_parameter$lib_size <- metaSPARSim::estimate_library_size(data = raw_data[, 
                                                                     cond, drop = FALSE])
    dataset_parameter[[cond_i]] <- cond_parameter
    cond_i <- cond_i + 1
  }
  names(dataset_parameter) <- names(conditions)
  return(dataset_parameter)
}
