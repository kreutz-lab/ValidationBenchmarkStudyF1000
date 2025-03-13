#' bs_annotateObject
#'
#' This function annotations the elements of the object, e.g.
#'   1) adds labels to the the count data level, i.e. it adds project name and 
#'   condition to the data_template or simulation_data
#'   2) copies the meta data to all data sets (also to the template itself to have
#'    the same info everywhere). The meta data is called $metaDataFromTemplate
#'
#' @param data_list object e.g. data_to_compare
#' @param doMetaDataResorting default: TRUE (bs_sortMetaData is called)
#'
#' @return data_list object
#' 
#' @examples
#' 
#' d <- readRDS(paste0(bs_path,"Analyses/Data/example_data/bs_object2.RDS"))
#' d2 <- bs_annotateObject(d)
#'
#' @seealso \code{\link{bs_DA}}
#' @keywords aKeywordSuchAsBenchmarking
#' @export

bs_annotateObject <- function(d, doMetaDataResorting=TRUE){
  if(!msb.attrCompare(d,"bstype","data_list"))
    stop("bs_annotateObject is up to now only implemented for a data_list (whole data_to_compare object)")
  
  project_label <- array()
  data_label <- array()
  namen <- names(d)
  
  if(doMetaDataResorting)
    d <- bs_sortMetaData(d)

  for(i in 1:length(d)){ # loop over all data_projects
    if(msb.attrCompare(d[[i]],"bstype","data_project")){
      project_label <- namen[i]
      
      namen2 <- names(d[[i]])
      metaData <- NULL
      for(j in 1:length(d[[i]])){ # loop over all data sets
        data_type <- NULL
        if(msb.attrCompare(d[[i]][[j]],"bstype","data_template")){
          data_type <- attr(d[[i]][[j]],"bstype")
          metaData <- d[[i]][[j]]$meta
        }
        if(msb.attrCompare(d[[i]][[j]],"bstype","sim_result"))
          data_type <- attr(d[[i]][[j]],"bstype")
        
        if(!is.null(data_type)){
          d[[i]][[j]]$project_label <- project_label
          d[[i]][[j]]$datatype_label <- data_type
          # 1st check whether the meta data is a data.frame with $condition as a column
          if(!"condition" %in% names(metaData)){
            print(i)
            print(j)
            print(metaData)
            stop("Meta data does not have a condition column.")
          }
          
          d[[i]][[j]]$metaDataFromTemplate <- metaData
          # create meta data.frame by searching the levels of metaDataFromTemplate$condition in colnames of the counts:
          
          if(data_type=="sim_result"){
            condLevels <- as.character(unique(d[[i]][[j]]$metaDataFromTemplate$condition))
            condLevels <- condLevels[order(nchar(condLevels))] # first small ones
            # cat(i," j=",j,"\n")
            # print(condLevels)
            cns <- colnames(d[[i]][[j]]$counts) # sample colnames
            
            meta <- data.frame(condition=array(NA,dim=nrow(d[[i]][[j]]$metaDataFromTemplate)))
            for(k in 1:length(condLevels)){
              meta$condition[grep(condLevels[k],cns)] <- condLevels[k]
            }
            
            meta$condition <- as.factor(meta$condition)
            if(sum(is.na(meta$condition))>0){
              print(project_label)
              print(data_type)
              print(meta)
              warning("bs_annotate: some colnames are not annotated as condition because not found by grep")
            }
            rownames(meta) <- cns
            d[[i]][[j]]$meta <- meta
          }
        }
      }
    }
  }
  
  return(d)
}
