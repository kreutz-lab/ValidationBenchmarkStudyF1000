# D   is a data project, i.e. D$original$counts has to exist
#
# 
# 
# Example:
# > conditions <- split(seq_along(meta$condition), meta$condition)
# $K
# [1]   1   2   4   6   9  11  14  17  19  21  23  25  27  29  31  33  35  38  41  43  45  47  50  
# $T
# [1]   3   5   7  12  15  18  20  22  24  26  28  30  32  34  36  39  42  44  46  48  49  51  53  
#
# Example:
# bs_calibrate_MIDASim

bs_calibrate_MIDASim <- function(D,mode="nonparametric",conditions=NULL){
  if(!"original" %in% names(D))
    stop("bs_calibrate_MIDASim needs a data template $original.")
  if(!"counts" %in% names(D$original))
    stop("bs_calibrate_MIDASim needs counts from a data template $original$counts.")
  
  if(!is.list(conditions) && !is.null(conditions))
    stop("bs_calibrate_MIDASim: conditions should be a list of indices.")
  
  counts <- D$original$counts
  
  if(is.null(conditions)){
    conditions <- list(all=1:ncol(counts))
  }
  
  setup <- list()
  for(cond in names(conditions)){
    setup[[cond]] = MIDASim.setup(t(counts[,conditions[[cond]]]), mode = mode) # rows are samples
    setup[[cond]]$sampleIndices <- conditions[[cond]]
  }
  
  # setup2 = MIDASim.modify(setup, lib.size = NULL, mean.rel.abund = NULL,
  #                         gengamma.mu = NULL, sample.1.prop = NULL, 
  #                         taxa.1.prop = NULL)
  
  params <- setup

  return(params)
  
}