# This function reads all data_to_compare*.RDS and produces a summary file
#
# The summary xlsx file has three sheeds for summaries at three levels:
# summary_RDS is a summary of the whole RDS file
# summary_projects is a summary of all projects in all RDS
# summary_datasets is a summary of all datasets in all projects in all RDS
#
# pattern   the pattern argument passed to list.files(pattern=pattern, recursive=T, full.names = T)
#
# pattern2  an additional vector of patterns that have to match the result of list.files
#
# Examples:
# summary_RDS <- bs_summarize_all() 
# 
# # One specific folder/file:
# tmp <- bs_summarizeDF_all(outfile="tmp.xlsx",folder="../Results_partReg/")


bs_summarizeDF_all <- function(pattern= "^DF\\.RDS$",outfile="../bs_summarizeDF_all.xlsx",folder="..",pattern2="/4", addSD=T){
  library(openxlsx)
  cat("bs_summarizeDF_all: List.files for pattern=",pattern,":\n")
  fils <- list.files(folder,pattern = pattern, recursive = T, full.names = T)
  cat(paste(fils,collapse="\n"))
  cat("\n\n")
  if(!is.null(pattern2)){
    for(pat in pattern2){
      cat("bs_summarizeDF_all: Only files that match pattern2=",pat," are selected:\n")
      fils <- grep(pat,fils,value = T)
    }
  }
  cat(paste(fils,collapse="\n"))
  
  file_details <- file.info(fils)
  modification_times <- file_details$mtime
  sizes_in_mb <- file_details$size / (1024^2)
  
  summary_RDS <- data.frame(files=fils,
                            saveTimes=modification_times,
                            sizeInMB=sizes_in_mb,
                            asdfasdf=array(data=NA,dim=length(fils))
  )
  
  res <- list()
  for(ii in 1:length(fils)){
    # Read RDS
    file <- fils[ii]
    cat("bs_summarizeDF_all for file ",file,"...\n")
    
    resTmp <- bs_summarizeDF(file)
    for(name in names(resTmp)){
      if(is.null(res[[name]]) || !name %in% names(res)){
        res[[name]] <- resTmp[[name]]
      
        names(res[[name]])[ncol(res[[name]])] <- file
      }else{
        #  res[[name]] <- dplyr::bind_cols(res[[name]],resTmp[[name]])
        res[[name]] <- merge(res[[name]],resTmp[[name]],by="row.names", all = TRUE)
        rownames(res[[name]]) <- res[[name]]$Row.names  # Restore row names
        res[[name]]$Row.names <- NULL  # Remove the redundant column

        names(res[[name]])[ncol(res[[name]])] <- file
      }
    }
    
  }    
  if(addSD){
    for(name in names(res)){
      SD <- as.data.frame(array(dim=nrow(res[[name]])))
      try(SD <- as.data.frame(rowSds(as.matrix(res[[name]]))))
      names(SD) <- "SD"
      res[[name]] <- merge(res[[name]],data.frame(SD=SD),by="row.names", all = TRUE)
      rownames(res[[name]]) <- res[[name]]$Row.names  # Restore row names
      res[[name]]$Row.names <- NULL  # Remove the redundant column
    }
    
  }
  
  print(res)
  
  ## Creating xlsx file
  if(!is.null(outfile))
    wb <- createWorkbook()
  for(col in names(res)){
    # Write each list element into a seperate sheet:
    if(!is.null(outfile)){
      openxlsx::addWorksheet(wb, col)
      openxlsx::writeData(wb, sheet=col, x=res[[col]],rowNames = T)
    }
  }
  if(!is.null(outfile))
    openxlsx::saveWorkbook(wb, outfile, overwrite = TRUE)
  
  return(res)
  
}

