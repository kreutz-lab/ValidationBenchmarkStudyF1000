# Creating the data_to_compare object from counts
#
#' @param counts Count matrix nfeature x nsamples
#' @param meta meta data as data.frame
#' @param name project name
#' @param nfeature max number of features, default: NULL (=all)
#' @param counts.norm normalized counts (optional)
#' 
#' @example data_to_compare <- bs_makeData() # Loads all data templates
#' 

bs_makeData <- function(counts, meta, name="expData",nfeature=NULL,counts.norm=counts){
  Data.list <- list()
  
  Data.list[[name]]$original$counts <- as.matrix(counts)
  Data.list[[name]]$original$meta <- meta
  if(!is.null(counts.norm))
    Data.list[[name]]$original$counts.norm <- as.matrix(counts.norm)
  
  # ## Remove samples that do not pass the metaSPARSim filter:
  # conds <- Data.list[[name]]$original$meta$condition
  # condU <- unique(Data.list[[name]]$original$meta$condition)
  # drin <- array(TRUE,dim=length(conds))
  # 
  # for(condi in unique(Data.list[[name]]$original$meta$condition)){
  #   data <- Data.list[[name]]$original$counts[,conds==condi]
  #   perc_not_zeros_per_OTU <- rowSums(data > 0)/ncol(data)
  #   ind_pass_filter <- (perc_not_zeros_per_OTU > 0.2)
  #   cs <- colSums(data[ind_pass_filter,])
  #   drin[conds==condi] <- drin[conds==condi] & cs>=2
  # }
  # 
  # Data.list[[name]]$original$counts      <- Data.list[[name]]$original$counts[,drin]
  # Data.list[[name]]$original$meta        <- Data.list[[name]]$original$meta[drin,,drop=F]
  # Data.list[[name]]$original$counts.norm <- Data.list[[name]]$original$counts.norm[,drin]
  # ## End sample filter
  
  if(!is.null(nfeature)){
    nfeature2 <- min(nrow(Data.list[[name]]$original$counts),nfeature)
    Data.list[[name]]$original$counts <- Data.list[[name]]$original$counts[1:nfeature2,]
    Data.list[[name]]$original$counts.norm <- Data.list[[name]]$original$counts.norm[1:nfeature2,]
  }
  
  attr(Data.list,"bstype") <- "data_list"
  attr(Data.list[[name]],"bstype") <- "data_project"
  attr(Data.list[[name]]$original,"bstype") <- "data_template"
  attr(Data.list[[name]]$original$counts,"bstype") <- "raw_counts"
  attr(Data.list[[name]]$original$meta,"bstype") <- "meta_data"
  attr(Data.list[[name]]$original$counts.norm,"bstype") <- "norm_counts"
  
  return(Data.list)
}

