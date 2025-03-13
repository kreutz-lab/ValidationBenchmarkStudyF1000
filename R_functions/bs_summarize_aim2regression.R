# dfSelected <- bs_summarize_aim2regression(folder)
#
# folder  folder for output xlsx file and for searching aim2_regression.RDS if no aim2_regression data.frame is provided.
#
# Example:
# dfSelected <- bs_summarize_aim2regression()
#
#
# dfSelected <- bs_summarize_aim2regression(outFile=NULL) # No writing
#
# dfSelected <- bs_summarize_aim2regression(outFile = NULL)
# dfEquiv <- dfSelected[grepl("simEquiv",dfSelected$dataName),]
# bs_summarize_aim2regression_write(dfEquiv, outFile="aim2_regression_all.xlsx",splitData = F)
# bs_summarize_aim2regression_write(dfEquiv, outFile="aim2_regression_splitted.xlsx")

bs_summarize_aim2regression <- function(folders=c("../Results","../Results_partReg"),outFile="aim2_regression.xlsx",pattern="aim2_regression\\.RDS$",pattern2=NULL,patternNot=NULL){
  
  files_list <- c()
  for(folder in folders){
    files_list <- c(files_list,list.files(path = folder, pattern = pattern, recursive = TRUE, full.names = TRUE))
    #files_list <- "../Results/5.1.1_Aim2_primary_metaSPARSim_simEquiv/aim2_regression.RDS"
  }
  if(!is.null(pattern2)){
    for(pat in pattern2)
      files_list <- grep(pat,files_list,value = T)
  }
  if(!is.null(patternNot)){
    for(pat in patternNot)
      files_list <- grep(pat,files_list,value = T,invert = T)
  }
  
    
    dfSelected <- NULL
    dataAll <- NULL
    for(file in files_list){
      tryCatch({
        succeeded <- FALSE
        #cat("bs_summarize_aim2regression",file,"... \n")
        aim2reg_tmp <- readRDS(file = file)
        if(length(aim2reg_tmp)==0){
          cat("bs_summarize_aim2regression",file," is empty! \n")
          next
        }
        
        dftmp <- bs_summarize_aim2regression_core(aim2reg_tmp)
        dftmp <- cbind(data.frame(file=array(file,dim=nrow(dftmp))), dftmp)
        if(is.null(dfSelected)){
          dfSelected <- dftmp
        }else{
          dfSelected <- dplyr::bind_rows(dfSelected, dftmp)
        }
        
        # for(j in 1:length(aim2reg_tmp)){
        #   dataAlltmp <- aim2reg_tmp[[j]]$data
        #   dataAlltmp <- cbind(data.frame(file=array(file,dim=nrow(dataAlltmp))), dataAlltmp)
        #   dataAlltmp <- cbind(data.frame(hypothesis=array(aim2reg_tmp[[j]]$nameHypo,dim=nrow(dataAlltmp))), dataAlltmp)
        #   if(is.null(dataAll)){
        #     dataAll <- dataAlltmp
        #   } else{
        #     dataAll <- dplyr::bind_rows(dataAll, dataAlltmp)
        #   }
        # }
        
        cat(file," done.\n")
        succeeded <- TRUE
      }, error = function(e){
        # if(!grepl("exp",file))
        #   save(aim2reg_tmp,dfSelected,dataAll,file,file = "error.Rdata")
        cat("Problem in ",file, "\n")
        print(e)
        cat("\n")
      })
      if(!succeeded)
        cat("Problem in ",file, "\n")
      
    }
    # Not available data Props are set to zero
    dfSelected[is.na(dfSelected)] <- FALSE
    
    # Now the excel file in wide format with colored output:
    drin <- array(T,length(names(dfSelected)))
    for(j in 1:ncol(dfSelected)){
      if(is.logical(dfSelected[,j])){
        if(sum(dfSelected[,j],na.rm=T)==0) 
          drin[j] <- F
      }
    }
    dfSelected <- dfSelected[,drin]
    
    # Filtered
    filtered <- array("Un-filtered",dim=nrow(dfSelected))
    filtered[grep("/5.4",dfSelected$file,ignore.case = T,fixed = T)] <- "Filtered"
    filtered[grep("+",dfSelected$file,fixed=T)] <- "both"
    dfSelected <- cbind(data.frame(filtered = filtered),dfSelected)
    
    indPrim <- grep("primary",dfSelected$file)
    dfSelected[indPrim,"hypoName"] <- sub("H","",dfSelected[indPrim,"hypoName"])
    dfSelected[indPrim,"hypoName"] <- as.numeric(dfSelected[indPrim,"hypoName"])+100
    dfSelected[indPrim,"hypoName"] <- paste("H",dfSelected[indPrim,"hypoName"],sep="")
    
    indSec <- grep("secondary",dfSelected$file)
    dfSelected[indSec,"hypoName"] <- sub("secondary","",dfSelected[indSec,"hypoName"])
    dfSelected[indSec,"hypoName"] <- sub("H","",dfSelected[indSec,"hypoName"])
    dfSelected[indSec,"hypoName"] <- as.numeric(dfSelected[indSec,"hypoName"])+200
    dfSelected[indSec,"hypoName"] <- paste("H",dfSelected[indSec,"hypoName"],sep="")
    
    ## Add $dataName
    dataNames <- bs_processResultFoldernames(dfSelected$file, option="dataName")
    dfSelected <- cbind(data.frame(dataName = dataNames),dfSelected)
    
    ## Eliminate not relevant lines:
    raus <- (dfSelected$hypoNumber==13.1 & dfSelected$filtered=="Un-filtered") |
      (dfSelected$hypoNumber==13.1 & dfSelected$filtered=="Un-filtered") |
      dfSelected$hypoNumber<1
    dfSelected <- dfSelected[!raus,]
    
    if(!is.null(outFile)){
      for(folder in folders){
        tmp <- dfSelected
        tmp$prefix <- NULL
        #tmp$dataName <- NULL
        #tmp$resultFileName <- NULL
        bs_summarize_aim2regression_write(tmp, folder = folder, outFile = outFile)
      }
    }    
  return(dfSelected)
}


bs_summarize_aim2regression_core <- function(aim2_regression){
  # First collect all occuring coefs:
  coefNames <- c()
  coefVals <- c()
  for(i in 1:length(aim2_regression)){
    coefNames_i <- "NA"
    coefVals_i <- NA
    try({
      coefNames_i <- rownames(aim2_regression[[i]]$coefs)
      coefVals_i <- aim2_regression[[i]]$coefs[,1]
      coefNames <- c(coefNames,coefNames_i)
      coefVals <- c(coefVals,coefVals_i)
    })
    
  }
  coefNames <- unique(coefNames)
  
  # Eliminate all dataSetCoefs
  raus <- grepl("dataSet*",coefNames)
  coefNames <- coefNames[!raus] 
  coefVals <- coefVals[!raus] 
  
  dfSelected <- NULL
  dataSetFound <- NULL
  rowInfos <- NULL
  for(i in 1:length(aim2_regression)){
    if("hypothesis" %in% names(aim2_regression[[i]])){
      aim2info <- data.frame(hypoName=aim2_regression[[i]]$nameHypo,
                             hypoNumber = aim2_regression[[i]]$hypoNumber,
                             prefix = aim2_regression[[i]]$prefix,
                             N = aim2_regression[[i]]$nHtrue,
                             propHtrue = aim2_regression[[i]]$propHtrue,
                             resultFileName = aim2_regression[[i]]$fileName,
                             hypothesis = aim2_regression[[i]]$hypothesis)
    }else{
      if(!is.null(aim2_regression[[i]]$nameHypo))
        nameHypo <- aim2_regression[[i]]$nameHypo
      else
        nameHypo <- "NA"
      aim2info <- data.frame(hypoName=nameHypo,
                             hypoNumber=NA,
                             prefix = "NA",
                             N = NA,
                             propHtrue = NA,
                             resultFileName = "NA",
                             hypothesis = "NA")
    }
    isFound <- coefNames %in% rownames(aim2_regression[[i]]$coefs)
    coefFound <- array(0,dim=length(rownames(aim2_regression[[i]]$coefs)))
    coefFound[isFound] <- aim2_regression[[i]]$coefs[isFound]
    
    if(is.null(dfSelected)){
      #      dfSelected <- data.frame(t(isFound))
      dfSelected <- data.frame(t(coefFound))
      rowInfos <- aim2info
      colnames(dfSelected) <- paste("LR.",coefNames,sep="")
    }else{
      #dfSelected <- rbind(dfSelected,isFound)
      dfSelected <- rbind(dfSelected,coefFound)
      rowInfos <- rbind(rowInfos,aim2info)
    }
    dataSetFound[i] <- sum(grepl("dataSet*",coefNames))>0
  }
  
  dfSelected <- cbind(rowInfos,dfSelected)
  
  # dfSelected <- cbind(data.frame(dataSet = dataSetFound),dfSelected)
  dfSelected$dataSet <- dataSetFound
  
  return(dfSelected)
}


bs_summarize_aim2regression_write <- function(dfSelected,folder="../Results",outFile=NULL,splitData=T){
  
  # Sort according to sum of TRUE (i.e. the number a property is selected in stepwise regression)
  iProp <- grep("^LR",names(dfSelected))
  anz <- array(Inf,dim=length(names(dfSelected)))
  for(j in iProp){
    anz[j] <- sum(dfSelected[,j]!=0,na.rm=T) 
  }
  names(anz) <- names(dfSelected)
  
  cat("Number of times selected by stepwise regression:\n")
  print(anz[!is.infinite(anz)])
  
  if(!is.null(outFile))
    write.xlsx(as.data.frame(anz[order(anz,decreasing = T)]),paste0(folder,"/",sub(".xlsx",".anz.xlsx",outFile)),rowNames =T)
  
  dfSelected <- dfSelected[,order(anz,decreasing = T)]
  
  
  
  if(splitData){
    splitLevel <- unique(dfSelected$dataName)
    splitValues <- dfSelected$dataName
  }else{
    splitLevel <- "all"
    splitValues <- array("all",dim=nrow(dfSelected))
  }
  
  wb <- createWorkbook()
  
  for(j in 1:length(splitLevel)){
    dfout <- dfSelected[splitValues==splitLevel[j],]
    sheetName =  splitLevel[j]
    sheetName <- sub("ancom_Nearing","Near",sheetName) # make it shorter
    sheetName <- paste0(substr(sheetName,1,min(29,nchar(sheetName))),j)
    print(sheetName)
    
    addWorksheet(wb, sheetName)
    writeData(wb, sheet = sheetName, x = dfout)
    # Create styles
    style1 <- createStyle(fgFill = "darkorange")
    style2 <- createStyle(fgFill = "yellow")
    style3 <- createStyle(fgFill = "lightgreen")
    # Apply styles to specific cells
    namen <- names(dfout)
    for(i in 1:ncol(dfout)){
      if( grepl("^LR",names(dfout)[i]) ){ # All data props start with LR.
        for(j in 1:nrow(dfout)){
          if((dfout[[j,i]])>0)
            addStyle(wb, sheet = sheetName, style = style3, rows = j+1, cols = i, gridExpand = TRUE)
          if((dfout[[j,i]])<0)
            addStyle(wb, sheet = sheetName, style = style1, rows = j+1, cols = i, gridExpand = TRUE)
        }
      }
    }
    
  }
  cat("Write ",paste0(folder,"/",outFile),"...\n")
  if(!is.null(outFile))
    saveWorkbook(wb, paste0(folder,"/",outFile), overwrite = TRUE)
}


bs_summarize_aim2regression_write2 <- function(dfSelected,folder="../Results",outFile="aim2_regression.xlsx",splitData=T){
  
  # Sort according to sum of TRUE (i.e. the number a property is selected in stepwise regression)
  anz <- array(Inf,dim=length(names(dfSelected)))
  for(j in 1:ncol(dfSelected)){
    if(is.logical(dfSelected[,j])){
      anz[j] <- sum(dfSelected[,j],na.rm=T)
    }
  }
  names(anz) <- names(dfSelected)
  
  cat("Number of times selected by stepwise regression:\n")
  print(anz[!is.infinite(anz)])
  
  dfSelected <- dfSelected[,order(anz,decreasing = T)]
  dfout <- dfSelected # write everything
  
  library(openxlsx2)
  
  # Create a new workbook
  wb <- wb_workbook()
  
  # Add a worksheet
  sheetName <- "Sheet1"
  wb$add_worksheet(sheet = sheetName)
  
  # Write data to the worksheet
  wb_add_data(wb, sheet = sheetName, x = dfout)
  
  # Create a conditional formatting rule for a color scale
  # Note: the colors need to be defined in hex format
  wb$add_conditional_formatting(sheet = sheetName, 
                                cols = 1:ncol(dfout), 
                                rows = 1:nrow(dfout) + 1,  # +1 because data starts in row 2
                                rule = c("-5", "0", "5"), 
                                style = createStyle(
                                  gradient = c("#FF0000", "#FFFFFF", "#00FF00")  # Red, White, Green
                                ))
  
  # Save the workbook
  cat("Write ",paste0(folder,"/",outFile),"...\n")
  wb_save(wb, file = paste0(folder,"/",sub(".xlsx","2.xlsx",outFile)), overwrite = TRUE)
  
}