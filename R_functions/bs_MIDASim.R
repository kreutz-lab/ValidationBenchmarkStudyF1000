# Simulate data using MIDASim for given parameters.
# 
# Parameters are the result of MIDASim::
bs_MIDASim <- function(params){
  
  nsamples <- 0
  simulated.data <- list()
  for(cond in names(params)){
    simulated.data[[cond]] = MIDASim(bs_MIDASim.modify(params[[cond]]))
    nsamples <- max(nsamples,max(params[[cond]]$sampleIndices))
  }
  
  simcounts <- matrix(NA,nrow=ncol(simulated.data[[cond]]$sim_01),ncol=nsamples)
  cns <- paste("Sample",as.character(1:nsamples))
  for(cond in names(params)){
    simcounts[, params[[cond]]$sampleIndices] <- t(simulated.data[[cond]]$sim_count)
    cns[params[[cond]]$sampleIndices] <- rownames(params[[cond]]$mat01)
  }
  
  colnames(simcounts) <- cns
  rownames(simcounts) <- colnames(simulated.data[[cond]]) 
  
  sim_data <- list(counts=simcounts, params=params)
  return(sim_data)
}