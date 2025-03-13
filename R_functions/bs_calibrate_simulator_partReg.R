# params <- bs_calibrate_simulator_partReg(D,propReg)
#
# This function determines params[[cond]]$isReg
# and calls bs_setUnregulatedParams to translate this to the params themselves
#
#
#
# if D$original$partreg_pvals exists, it is chosen 
#    otherwise
# fast DA methods are first used to calculated those p-values
#


bs_calibrate_simulator_partReg <- function(D,params,paramsAll, simulator,propReg=NULL,runOption=runOption){
  if("original" %in% names(D) && "partreg_pvals" %in% names(D$original) && !is.null(D$original$partreg_pvals)){
    cat("partreg_pvals found.\n")
    pvals <- D$original$partreg_pvals
    
  }else{ #  D$original$partreg_pvals does not exist: apply fast DA methods
    cat("metasparsim_partreg: Running all FAST DA to get p-values (doSlowMethods=F) ...\n")
    if(runOption=="fast")
      tmp <- bs_DA(D, design = as.formula("~condition"), groupVariable = "condition",doSlowMethods = F, parallelMode = F,whichMethods = c("edgeR"))
    else
      tmp <- bs_DA(D, design = as.formula("~condition"), groupVariable = "condition",doSlowMethods = F, parallelMode = F)
    
    pvals <- tmp$original$DA$p_value
  }
  
  # Now isReg is calculated from p-values:
  # account for zero-inflation by calculating the proportion of regulated features after filtering w.r.t zeros (with different thresholds)
  counts <- D$original$counts
  if(is.null(propReg)){
    isReg <- bs_pvals2isReg(pvals = pvals,counts = counts)  # 
    cat(sum(isReg)," features are regulated (",sum(isReg)/length(isReg)*100,"%)\n")
  
  } else { # propReg given
    anzReg <- ceiling(nrow(D$original$counts)*propReg)
    isReg <- array(FALSE,dim=nrow(D$original$counts))
    if(anzReg>0)
      isReg[sample(1:nrow(D$original$counts),size = anzReg, replace = F)] <- TRUE        
  }
  
  params <- bs_setUnregulatedParams(params=params, paramsAll=paramsAll, simulator=simulator,notRegulated=!isReg)
  for(cond in names(params)){
    params[[cond]]$isReg <- isReg
    params[[cond]]$pvals <- pvals
  }
  
  return(params) 
  
}

