# This function can be used to processes folderNames with numbers 
# to shorter strings to obtained combined indicators of the analysis
#
# For example used by aim2regression and aim2results to process the names in the same way

bs_processResultFoldernames <- function(folderName,option="aim2results"){
  
  out <- NULL
  
  if(tolower(option)=="aim2results"){
    # was used in bs_summarize_aim2results
    folderName <- gsub("/$", "", folderName)
    # if the filename with . is there => remove it:
    withFileName <- grepl("\\.\\w{3}$", basename(folderName))
    folderName[withFileName] <- dirname(folderName[withFileName])
    
    cat("bs_processResultFoldernames for option ",option,"...\n")
    folderName <- sub("5.1+5.3_Aim2_","Both_",folderName,fixed=T)
    folderName <- sub("5.1+5.3_Aim2b_","Both_",folderName,fixed=T)
    folderName <- sub("5.2+5.4_Aim2_","Both_Zeros_",folderName,fixed=T)
    folderName <- sub("5.2+5.4_Aim2b_","Both_Zeros_",folderName,fixed=T)
    folderName <- sub("5.1_Aim2_","",folderName,fixed=T)
    folderName <- sub("5.2_Aim2_","Zeros_",folderName,fixed=T)
    folderName <- sub("5.3_Aim2_","filtered_",folderName,fixed=T)
    folderName <- sub("5.4_Aim2_","filtered+Zeros",folderName,fixed=T)
    folderName <- sub("5.1_Aim2b_","",folderName,fixed=T)
    folderName <- sub("5.3_Aim2b_","filtered_",folderName,fixed=T)
    folderName <- sub("5.2_Aim2b_","Zeros_",folderName,fixed=T)
    folderName <- sub("5.4_Aim2b_","filtered+Zeros",folderName,fixed=T)
    folderName <- sub("secondary","",folderName,fixed=T)
    folderName <- sub("primary","",folderName,fixed=T)
    folderName <- sub("__","_",folderName,fixed=T)
    folderName <- sub("_$","",folderName)
    
    out <- folderName
  }
  if(tolower(option)=="dataname"){
    # was used in bs_summarize_aim2regression
    folderName <- gsub("/$", "", folderName)
    # if the filename with . is there => remove it:
    withFileName <- grepl("\\.\\w{3}$", basename(folderName))
    folderName[withFileName] <- dirname(folderName[withFileName])
    folderName <- basename(folderName)
    
    dataName <- folderName
    dataName <- sub("metaSPARSim","mSP",dataName)
    dataName <- sub("sparseDOSSA","spD",dataName)
    dataName <- sub("ZerosAdded","0add",dataName)
    dataName <- sub("filtered_","",dataName)
    dataName <- gsub("\\+.*?_", "_",dataName)

    dataName <- sub("_Aim2_secondary","",dataName)
    dataName <- sub("_Aim2_secondary","",dataName)
    dataName <- sub("_Aim2_primary","",dataName)
    
    # combine 5.1 and 5.3  
    dataName <- sub("5.1","XXXX",dataName)
    dataName <- sub("5.3","5.1+5.3",dataName)
    dataName <- sub("XXXX","5.1+5.3",dataName)
    
    # combine 5.2 and 5.4
    dataName <- sub("5.2","XXXX",dataName)
    dataName <- sub("5.4","5.2+5.4",dataName)
    dataName <- sub("XXXX","5.2+5.4",dataName)
    out <- dataName
  }
  
  if(is.null(out))
    stop("bs_processResultFoldernames: option=",option," not found!")
  
  return(out)
}