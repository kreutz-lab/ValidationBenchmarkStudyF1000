# This function reads all aim2_results.RDS data.frames, combines them and writes it to an xlsx file
#
# @value df_aim2res The all aim2_resuls merged to a data.frame
# @resultFolder The folder where recursively aim_2result.RDS is searched.
#
# Two xlsx files are written: one in the long and one in the more convenient wide format
#
# Examples:
# df_aim2res <- bs_summarize_aim2results()
#
# bs_summarize_aim2results(outFile = "tmp.xlsx")
#
# pattern2  Additional pattern for selecting a subset, e.g. only specific foldernames
#          
#
## Examples:
## Summarize another folder:
# bs_summarize_aim2results(outFile = "Results_Supp.xlsx",resultFolder="../Results_Supp/")
#
#
# df_aim2res <- bs_summarize_aim2results(outFile="../Results/aim2results.xlsx",
#   resultFolder=c("../Results/","../Results_partReg/"),
#   pattern2=c("simEquiv","Nearing"))



bs_summarize_aim2results <- function(outFile="../Results/aim2results.xlsx",resultFolder="../Results/",pattern1="aim2_results.RDS",pattern2=NULL){
  require(openxlsx)
  
  df_aim2res <- NULL
  files <- c()
  for(resFolder in resultFolder)
    files = c(files,list.files(resFolder,pattern=pattern1,recursive = T,full.names = T))
  
  if(!is.null(pattern2)){
    for(pat in pattern2)
      files <- grep(pat,files,value = T)
  }

  for(i in 1:length(files)){
    file <- files[i]
    cat("bs_summarize_aim2results: Reading ",file,"...\n")
    tmpRes <- readRDS(file)
    tmpRes <- data.frame(tmpRes,file=file)
    
    if(is.null(df_aim2res)){# 1st iteration
      df_aim2res <- tmpRes
    } else {
      df_aim2res <- rbind(df_aim2res,tmpRes)
    }
  }
  
  df_aim2res$filtered <- array("Un-filtered",dim=nrow(df_aim2res))
  cat("bs_summarize_aim2results: Filtering is determined based on filtered, 5.3 and 5.4 as pattern. Might be checked.\n")
  df_aim2res$filtered[grep("filtered",df_aim2res$file,ignore.case = T)] <- "Filtered"
  df_aim2res$filtered[grep("5.3",df_aim2res$file,ignore.case = T)] <- "Filtered"
  df_aim2res$filtered[grep("5.4",df_aim2res$file,ignore.case = T)] <- "Filtered"
  df_aim2res$filtered[grep("+",df_aim2res$file,fixed=T)] <- "both"
  
  df_aim2res$dataType <- sub(".*_(.*)$", "\\1", df_aim2res$prefix)
  
  df_aim2res$simulator <- array("",dim=nrow(df_aim2res))
  df_aim2res$simulator[grep("metaSPAR",df_aim2res$file,ignore.case = T)] <- "metaSPARSim"
  df_aim2res$simulator[grep("parseDOSSA",df_aim2res$file,ignore.case = T)] <- "sparseDOSSA"
  
  # Make it unique:
  #df_aim2res <- df_aim2res[!duplicated(df_aim2res[,c("prefix","test","hypoNumber","hypothesis","simulator")]),]
  
  ## Add 100 for primary aims and 200 for secondary aims
  df_aim2res$hypoNumber[grep("primary",df_aim2res$file)] <- df_aim2res$hypoNumber[grep("primary",df_aim2res$file)]+100
  df_aim2res$hypoNumber[grep("secondary",df_aim2res$file)] <- df_aim2res$hypoNumber[grep("secondary",df_aim2res$file)]+200
  df_aim2res$hypoNumber <- paste("H",df_aim2res$hypoNumber,sep="")
  
  ############
  # Eliminating Hypothesis that are only state for either filtered or unfiltered
  # This might be project specific and subject to naming 
  df_aim2res <- bs_filerAim2Results(df_aim2res)
  ############
  
  
  # Report how many hypotheses are validated
  N <- nrow(df_aim2res)
  nTrue <- sum(df_aim2res$validated,na.rm=T)
  nFalse <- sum(!df_aim2res$validated,na.rm=T)
  nNA <-  sum(is.na(df_aim2res$validated),na.rm=T)
  
  nTrue <- nTrue
  nFalse <- nFalse
  
  cat(nTrue, " out of ",N," hypotheses are validated (",nTrue/N*100,"%)\n")
  cat(nFalse, " out of ",N," hypotheses are NOT validated (",nFalse/N*100,"%)\n")
  cat(nNA, " out of ",N," hypotheses could not be tested, i.e. yield NA (",nNA/N*100,"%)\n")

    
  if(!is.null(outFile))
    dfout <- bs_summarize_aim2results_write(df_aim2res,df_aim2res_wide,outFile)
  else
    dfout <- df_aim2res
      
  return(invisible(dfout))
  
}

bs_summarize_aim2results_write <- function(df_aim2res,df_aim2res_wide,outFile){
  # reorder and remove $file
  df_aim2res <- df_aim2res[,c("hypoNumber","simulator","dataType","prefix","filtered","test","outcome","outcome_value","threshold","propTrue","validated","hypothesis", "file")]
  
  # substitute df_aim2res$prefix in a way that analyses that belong together have the same name
  df_aim2res$prefix <- bs_path2prefix(dirname(df_aim2res$file))
  df_aim2res$prefix <- bs_processResultFoldernames(df_aim2res$prefix,option="aim2results")
  
  sheetName =  as.character(Sys.Date())
  openxlsx::write.xlsx(df_aim2res,file=outFile,sheetName=sheetName)
  # 
  
  prefixUni <- unique(df_aim2res$prefix)
  for(pre in prefixUni){
    cat(pre,": ",sum(df_aim2res$prefix==pre),"\n") 
  }
  
  sheetName =  as.character(Sys.Date())
  sheetName <- sub("ancom_Nearing","Near",sheetName) # make it shorter
  
  rf <- order(paste(df_aim2res$simulator,
                    sub("filtered+","",sub("filtered_","",df_aim2res$prefix,ignore.case = T),
                        df_aim2res$hypoNumber,fixed=T),sep="_"))
  df_sorted <- df_aim2res[rf,]
  
  # convert $validated to character
  df_sorted$validated <- as.character(df_sorted$validated)
  df_sorted$validated[is.na(df_sorted$validated)] <- "Not testable"
  
  wb <- createWorkbook()
  addWorksheet(wb, sheetName)
  writeData(wb, sheet = sheetName, x = df_sorted)
  # Create styles
  style1 <- createStyle(fgFill = "darkorange")
  style2 <- createStyle(fgFill = "yellow")
  style3 <- createStyle(fgFill = "lightgreen")
  # Apply styles to specific cells
  namen <- names(df_sorted)
  for(i in 1:ncol(df_sorted)){
    if(sum(df_sorted[,i]=="TRUE",na.rm=T)>0 | sum(df_sorted[,i]=="FALSE",na.rm=T)>0 | sum(df_sorted[,i]=="Not testable",na.rm=T)>0){
      for(j in 1:nrow(df_sorted)){
        if(!is.na(df_sorted[[j,i]]) && df_sorted[[j,i]]=="TRUE")
          addStyle(wb, sheet = sheetName, style = style3, rows = j+1, cols = i, gridExpand = TRUE)
        if(!is.na(df_sorted[[j,i]]) && df_sorted[[j,i]]=="Not testable")
          addStyle(wb, sheet = sheetName, style = style2, rows = j+1, cols = i, gridExpand = TRUE)
        if(!is.na(df_sorted[[j,i]]) && df_sorted[[j,i]]=="FALSE")
          addStyle(wb, sheet = sheetName, style = style1, rows = j+1, cols = i, gridExpand = TRUE)
      }
    }
  }
  saveWorkbook(wb, sub(".xlsx","_sorted.xlsx",outFile), overwrite = TRUE)
  
  df4wide <- df_sorted
  df4wide$prefix <- sub("_expAll","",df4wide$prefix)
  df4wide$prefix <- sub("_exp","",df4wide$prefix)
  df4wide$prefix <- sub("_simEquiv","",df4wide$prefix)
  df4wide$prefix <- sub("_sim","",df4wide$prefix)
  
  df_aim2res_wide <- df4wide %>% pivot_wider(names_from=c("prefix","simulator"),values_from=c("validated","outcome_value","propTrue"))
  df_aim2res_wide <- df_aim2res_wide[order(df_aim2res_wide$hypoNumber),]
  
  
  # Now the excel file in wide format with colored output:
  wb <- createWorkbook()
  addWorksheet(wb, sheetName)
  writeData(wb, sheet = sheetName, x = df_aim2res_wide)
  # Create styles
  style1 <- createStyle(fgFill = "darkorange")
  style2 <- createStyle(fgFill = "yellow")
  style3 <- createStyle(fgFill = "lightgreen")
  # Apply styles to specific cells
  namen <- names(df_aim2res_wide)
  for(i in 1:ncol(df_aim2res_wide)){
    if(sum(df_aim2res_wide[,i]=="TRUE",na.rm=T)>0 | sum(df_aim2res_wide[,i]=="FALSE",na.rm=T)>0 | sum(df_aim2res_wide[,i]=="Not testable",na.rm=T)>0){
      for(j in 1:nrow(df_aim2res_wide)){
        if(!is.na(df_aim2res_wide[[j,i]]) && (df_aim2res_wide[[j,i]]=="TRUE" || df_aim2res_wide[[j,i]]==TRUE))
          addStyle(wb, sheet = sheetName, style = style3, rows = j+1, cols = i, gridExpand = TRUE)
        if(!is.na(df_aim2res_wide[[j,i]]) && (df_aim2res_wide[[j,i]]=="Not testable"))
          addStyle(wb, sheet = sheetName, style = style2, rows = j+1, cols = i, gridExpand = TRUE)
        if(!is.na(df_aim2res_wide[[j,i]]) && (df_aim2res_wide[[j,i]]=="FALSE" || df_aim2res_wide[[j,i]]==FALSE))
          addStyle(wb, sheet = sheetName, style = style1, rows = j+1, cols = i, gridExpand = TRUE)
      }
    }
  }
  saveWorkbook(wb, sub(".xlsx","_wide.xlsx",outFile), overwrite = TRUE)
  
  return(df_sorted)
}