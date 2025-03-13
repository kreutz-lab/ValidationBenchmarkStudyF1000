#' bs_compress
#'
#' This function removes fields of the bs object to reduce memory requirements
#'
#' The following elements are removed:
#'  - $gamma for bstype=="sim_result"
#'  
#' The following elements are reduced in size
#'  - rownames of template and simulation data are replace by "1", "2", ...
#'  - names of data properties if length>500
#'  - count matrices are converted to integer, if they are integer
#'
#' @param sizeThreshold  Compression is only done, if this threshold for object.size 
#'    is exceeded.
#'    Default sizeThreshold=1e9 (approx. 1 GB)
#' @param strong [FALSE] if this option is set to TRUE, then only data properties are kept
#'    Each counts matrix is saved to an extra file in folder .bs_compress.
#'    The filename and variable name are stored in the object and the process can be 
#'    inverted by bs_decompress.
#'    Only objects with object.size>1e6 are saved
#'
#' @return
#' 
#' @examples
#' 
#' d <- readRDS(paste0(bs_path,"Analyses/Data/example_data/bs_object2.RDS"))
#' object.size(d)
#' d2 <- bs_compress(d,sizeThreshold=1e3)
#' object.size(d2)
#'
#' # Strong compression (counts are saved to files)
#' object.size(d2)
#' d2 <- bs_compress(d,sizeThreshold=1e3)
#' object.size(d2)
#' d3 <- bs_decompress(d2)
#' object.size(d3)
#' 
#' # comparison:
#' plot(head(as.matrix(d[[1]][[1]]$counts)),head(as.matrix(d3[[1]][[1]]$counts)))
#'
#' @seealso \code{\link{bs_decompress}}
#' @keywords compression saving memory
#' @export

bs_compress <- function(input,sizeThreshold=1e9, strong=FALSE){
  if(is.numeric(sizeThreshold))
    if(object.size(input)<sizeThreshold)
      return(input)
  else
    sizeThreshold = 0 # ensure compression for neseted calls

  
  ## Define functions first:
  #
  # convert to integer vector, add colnames, rownames and nrow as attribute
  # if rounding has no effect on the data
  counts2integer <- function(counts){
    mtx <- as.matrix(counts)
    if("bstype" %in% names(attributes(counts)))
      bstype <- attr(counts,"bstype")
    else
      bstype <- NULL
    
    int <- as.integer(mtx)
    if(max(mtx-int,na.rm=T)==0){
      attr(int,"nrow") <- nrow(mtx)
      attr(int,"colnames") <- colnames(mtx)
      attr(int,"rownames") <- rownames(mtx)
      if(!is.null(bstype))
        attr(int,bstype) <- bstype
      
      return(int)
    }
    else
      return(counts)
  }
  
  bs_saveForCompression <- function(x,minSize=1e6){
    if(object.size(x)<minSize) # only do saving if object.size > minSize 
      return(x)
    else{
      out <- Sys.info()
      dir.create(".bs_compress",showWarnings = F)
      fileName <- paste0(as.character(format(Sys.time(), "%Y%m%d_%H%M%S")),"_",ceiling(runif(1,min=0.1,max=1)*1e10),".RDS")
      saveRDS(x,file=paste0(".bs_compress/",fileName))
      return(fileName)
    }
  }
  
    
  ## Now do calculations and call functions:
  if(is.list(input) && length(input)>0){
    for(i in 1:length(input)){
      #if(!is.null(attr(input[[i]],"bstype")) && attr(input[[i]],"bstype") == "sim_result" && !is.null(dim(input[[i]]$counts))){
      if(!is.null(input[[i]]) && bs_isa(input[[i]],"sim_result") && !is.null(dim(input[[i]]$counts))){
          input[[i]]$gamma <- NULL
        if(strong){
          fileName <- bs_saveForCompression(input[[i]]$counts)
          bstype = attr(input[[i]]$counts,"bstype")
          input[[i]]$counts <- fileName 
          attr(input[[i]]$counts,"bstype") <- bstype
        }
        else {
          rownames(input[[i]]$counts) <- seq(1:dim(input[[i]]$counts)[1])
          input[[i]]$counts <- counts2integer(input[[i]]$counts)
        }
      }
      if(!is.null(input[[i]]) && bs_isa(input[[i]],"data_template") && !is.null(dim(input[[i]]$counts))){
        if(strong){
          fileName <- bs_saveForCompression(input[[i]]$counts)
          bstype = attr(input[[i]]$counts,"bstype")
          input[[i]]$counts <- fileName 
          attr(input[[i]]$counts,"bstype") <- bstype
        
          fileName <- bs_saveForCompression(input[[i]]$counts.norm)
          bstype = attr(input[[i]]$counts.norm,"bstype")
          input[[i]]$counts.norm <- fileName 
          attr(input[[i]]$counts.norm,"bstype") <- bstype
        }
        else {
          rownames(input[[i]]$counts) <- seq(1:dim(input[[i]]$counts)[1])
          input[[i]]$counts <- counts2integer(input[[i]]$counts)
          rownames(input[[i]]$counts.norm) <- seq(1:dim(input[[i]]$counts.norm)[1])
        }
      }
      if(!is.null(input[[i]]) && bs_isa(input[[i]],"data_prop")){
        for(j in 1:length(input[[i]])){
          if(length(input[[i]][[j]])>500 && !is.null(names(input[[i]][[j]])))
            names(input[[i]][[j]]) <- seq(1:length(input[[i]][[j]]))
        }
      }
      if(!is.null(input[[i]]))
        input[[i]] <- bs_compress(input[[i]],sizeThreshold = sizeThreshold, strong=strong) 
    }
  }
  return(input)
}

