# This function creates params.RDS, a file containing all estimated sparseDOSSA parameters.
# This file is used by bs_paramsDefault_SparseDOSSA2 to find params for a specific dataset before simulation.
#
# inFolder contains individual estimates as workspace named estimateSparseDossa_foreach*.Rdata
#   They are created in script 1.2
#
# Example:
# inFolder <- "../Results/1.2_sparseDOSSA/"
# outFile <- paste0("../Results/1.2_sparseDOSSA/params.RDS")
# bs_sparseDossa_makeParamsRDS(inFolder=inFolder,outFile=outFile)


bs_sparseDossa_makeParamsRDS <- function(inFolder,outFile="params.RDS",pattern="estimateSparseDossa_foreach"){
  
  ## collect all params from estimateSparseDossa_foreach*.RDS and save to one RDS file:
  params <- list()
  fils <- grep(".Rdata",list.files(inFolder,pattern=pattern,full.names = T),value=T)
  file_info <- file.info(fils)
  # Sort files by modification time in descending order
  fils <- fils[order(file_info$mtime, decreasing = TRUE)]
  if(length(fils)>0){
    for(i in 1:length(fils))
    {
      tryCatch({
        SparseDOSSA2_fit <- NULL
        load(fils[i])
        # load(paste0("../Results/1.2_sparseDOSSA/",fils[i]))
        
        if(!is.null(SparseDOSSA2_fit)){
          params[[i]] <- SparseDOSSA2_fit
          params[[i]]$file <- fils[i]
        }
        else
          paste0(fils[i], ": Variable SparseDOSSA2_fit not found.")
      }, error=function(e){
        params[[i]] <- paste0(fils[i], " did not work: ",conditionMessage(e))
      })
    }
  }
  
  saveRDS(params,file=outFile)
  
}