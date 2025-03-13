#' bs_decompress
#'
#' This function decpresses a bs-object that was compressed by bs_compress
#'
#' Conversion to integer is reversed.
#' Removed elements cannot be (fully) recovered
#' Changed rownames cannot be recovered.
#'
#'
#' @return
#' 
#' @examples
#' 
#' d <- readRDS("data_to_compare_CK2.RDS")
#' d <- readRDS(paste0(bs_path,"Analyses/Data/example_data/bs_object2.RDS"))
#' object.size(d)
#' 
#' # 1. compression
#' d2 <- bs_compress(d,sizeThreshold=1e3)
#' object.size(d2)
#' str(d2[[1]][[1]])
#' 
#' # 2. decompression
#' d3 <- bs_decompress(d2)
#' object.size(d3)
#' str(d3[[1]][[1]])
#' 
#' # comparison
#' plot(head(as.matrix(d[[1]][[1]]$counts)),head(as.matrix(d3[[1]][[1]]$counts)))
#'
#' # Strong compression (counts are saved to files)
#' object.size(d2)
#' d2 <- bs_compress(d,sizeThreshold=1e3,strong=T)
#' object.size(d2)
#' d3 <- bs_decompress(d2)
#' object.size(d3)
#' 
#' # comparison
#' plot(head(as.matrix(d[[1]][[1]]$counts)),head(as.matrix(d3[[1]][[1]]$counts)))
#'
#' @seealso \code{\link{bs_compress}}
#' @export

bs_decompress <- function(input){
  tryCatch({
    
    if(is.list(input) && length(input)>0){
      for(i in 1:length(input)){
        if(!is.null(attr(input[[i]],"bstype")) && attr(input[[i]],"bstype") == "sim_result"){
          if(is.character(input[[i]]$counts)) # strong compression, i.e. saving to files
            input[[i]]$counts <- readRDS(paste0(".bs_compress/",input[[i]]$counts))
          else
            input[[i]]$counts <- integer2counts(input[[i]]$counts)
        }
        if(!is.null(attr(input[[i]],"bstype")) && attr(input[[i]],"bstype") == "data_template"){
          if(is.character(input[[i]]$counts)){ # strong compression, i.e. saving to files
            input[[i]]$counts <- readRDS(paste0(".bs_compress/",input[[i]]$counts))
            input[[i]]$counts.norm <- readRDS(paste0(".bs_compress/",input[[i]]$counts.norm))
          }
          else
            input[[i]]$counts <- integer2counts(input[[i]]$counts)
        }
        input[[i]] <- bs_decompress(input[[i]]) 
      }
    }
  }, error=function(e){
    # try(save.image(file=paste0("bs_decompress_error.Rdata"))) # takes too long
    cat("bs_decompress_error: ",conditionMessage(e),", date: ",as.character(Sys.time()),"\n")
    cat("bs_decompress_error: ",conditionMessage(e),", date: ",as.character(Sys.time()),"\n",file="bs_decompress_error.log",append = T)
  })
  
  return(input)
}

# convert to integer vector, add colnames, rownames and nrow as attribute
integer2counts <- function(counts){
  if(is.data.frame(counts))
    return(counts) # do nothing 

  if("bstype" %in% names(attributes(counts)))
    bstype <- attr(counts,"bstype")
  else
    bstype <- NULL
  
  if(is.integer(counts[1])){
    mtx <- matrix(as.numeric(counts),nrow=attr(counts,"nrow"))
    
    colnames(mtx) <- attr(counts,"colnames") 
    rownames(mtx) <- attr(counts,"rownames")
    
    mtx <- as.data.frame(mtx)
    if(!is.null(bstype))
      attr(mtx,bstype) <- bstype
    
    return(mtx)
  }
  
  else
    return(counts)
  
}
