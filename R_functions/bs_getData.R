# Function to read in all data templates (or specific ones) for benchmark study. Data templates and their corresponding meta data are stored in a list. 
# The returned object is then used for all further analysis


#' @param files.names : Vector of names of data sets. Default: all data sets are loaded
#' @param nfeature Number of feature to load in each data set. Default: entire data sets are loaded
#' @param norm_type: Can choose from "TSS" (Total Sum Sclaing) and "GMPR" (Geometric Mean of Pairwise Ratios). Default: TSS
#' 
# files.names can be loaded like this (To get a list of the names of all data templates):
# files.names <- readRDS("../Data/files.names.RDS")
# Returns a list named by data file, each entry containing counts and meta for that specific data set
#
# Since metaSPARSim calculates the variablity from features with >20% non-zero counts over samples 
# and it does not work if a sample remains with colSum <2, those samples are filtered out.

#' @example data_to_compare <- bs_getData() # Loads all data templates
#' @example data_to_compare <- bs_getData("Exercise", nfeature=500, norm_type="TSS")
#' 

bs_getData <- function(files.names=NULL, nfeature=NULL, norm_type="GMPR"){
  Data.list <- list()
  
  if(is.null(files.names)){
    
    # Load all data sets when no file name is specified
    all.files <- list.files(path="../Data/count.data")
    #Extract data name
    names <- lapply(all.files, function(x) gsub("\\_ASVs.*","",x))
  }
  
  
  
  else{
    names <- files.names
  }
  
  if(norm_type=="GMPR"){
      path_to_norm <- "../Data/count.data.norm_GMPR"
  }
  if(norm_type=="TSS"){
      path_to_norm <- "../Data/count.data.norm"
  }
  
  
  # Load the data into a named list. For each data set there is a counts and meta data slot
  for(i in names){
      
    Data.list[[i]]$original$counts <- readRDS(paste0("../Data/count.data/",i,"_ASVs_table.RDS"))
    Data.list[[i]]$original$meta <- readRDS(paste0("../Data/meta.data.cleaned/",i,"_metadata.RDS"))
    Data.list[[i]]$original$counts.norm <- readRDS(paste0(path_to_norm,"/",i,"_ASVs_norm.RDS"))
    
    # ## Remove samples that do not pass the metaSPARSim filter:
    # conds <- Data.list[[i]]$original$meta$condition
    # condU <- unique(Data.list[[i]]$original$meta$condition)
    # drin <- array(TRUE,dim=length(conds))
    # 
    # for(condi in unique(Data.list[[i]]$original$meta$condition)){
    #   data <- Data.list[[i]]$original$counts[,conds==condi]
    #   perc_not_zeros_per_OTU <- rowSums(data > 0)/ncol(data)
    #   ind_pass_filter <- (perc_not_zeros_per_OTU > 0.2)
    #   cs <- colSums(data[ind_pass_filter,])
    #   drin[conds==condi] <- drin[conds==condi] & cs>=2
    # }
    # 
    # Data.list[[i]]$original$counts      <- Data.list[[i]]$original$counts[,drin]
    # Data.list[[i]]$original$meta        <- Data.list[[i]]$original$meta[drin,,drop=F]
    # Data.list[[i]]$original$counts.norm <- Data.list[[i]]$original$counts.norm[,drin]
    # ## End sample filter
    
    if(!is.null(nfeature)){
      nfeature2 <- min(nrow(Data.list[[i]]$original$counts),nfeature)
      Data.list[[i]]$original$counts <- Data.list[[i]]$original$counts[1:nfeature2,]
      Data.list[[i]]$original$counts.norm <- Data.list[[i]]$original$counts.norm[1:nfeature2,]
    }
    
    
    attr(Data.list,"bstype") <- "data_list"
    attr(Data.list[[i]],"bstype") <- "data_project"
    attr(Data.list[[i]]$original,"bstype") <- "data_template"
    attr(Data.list[[i]]$original$counts,"bstype") <- "raw_counts"
    attr(Data.list[[i]]$original$meta,"bstype") <- "meta_data"
    attr(Data.list[[i]]$original$counts.norm,"bstype") <- "norm_counts"
  }
  
  
  
  return(Data.list)
}

