bs_combine_aim2summaries <- function(fileAim2result="../Results/aim2results.xlsx", 
                                     fileAim2regression="../Results/aim2_regression_all.xlsx",
                                     all.x=TRUE, all.y=FALSE){
  
  require(openxlsx)
  aim2res <- read.xlsx(fileAim2result)
  aim2regress <- read.xlsx(fileAim2regression)
  
  aim2res <- aim2res[!grepl("project",aim2res$test),]
  
  newRowNames <- paste0(aim2res$prefix,"__",aim2res$hypoNumber,"__",aim2res$filtered)
  if(length(unique(newRowNames))<length(newRowNames)){
    warning("!! New rownames for aim2res are not unique !!")
    cat("The following rows are removed: \n")
    print(aim2res[duplicated(newRowNames),])
    drin <- which(!duplicated(newRowNames))
    aim2res <- aim2res[drin,]
    newRowNames <- newRowNames[drin]
  }
  rownames(aim2res) <- newRowNames

  newRowNames <- paste0(aim2regress$dataName,"__",aim2regress$hypoName,"__",aim2regress$filtered)
  if(length(unique(newRowNames))<length(newRowNames)){
    warning("!! New rownames for aim2regress are not unique !!")
    cat("The following rows are removed: \n")
    print(aim2res[duplicated(newRowNames),])
    drin <- which(!duplicated(newRowNames))
    aim2regress <- aim2regress[drin,]
    newRowNames <- newRowNames[drin]
  }
  rownames(aim2regress) <- newRowNames
  
  cat("Number of rows in ",fileAim2result,":",nrow(aim2res),"\n")
  cat("Number of rows in ",fileAim2regression,":",nrow(aim2regress),"\n")
  cat("Number of rows occuring in both files:",length(intersect(rownames(aim2res),rownames(aim2regress))),"\n")

  cat("\nFirst rownames missing in ",fileAim2result,":\n")
  print(head(setdiff(rownames(aim2res),rownames(aim2regress)),n=30))
  cat("\nFirst rownames missing in ",fileAim2regression,":\n")
  print(head(setdiff(rownames(aim2regress),rownames(aim2res)),n=30))
  
  cat("To write everything, use call bs_combine_aim2summaries(... all.y=TRUE). By default, only all lines of aim2res are written.\n")
  
  df_combined <- merge(aim2res, aim2regress, by = "row.names", all.x = all.x, all.y=all.y, suffixes = c(".res",".reg"))

  sheetName <- "combined"
  filename <- sub(".xlsx","_combined.xlsx",fileAim2result)
  
  cat("Writing to ",filename," (might take some time)...\n")
  wb <- createWorkbook()
  addWorksheet(wb, sheetName)
  writeData(wb, sheet = sheetName, x = df_combined)
  # Create styles
  styleRed <- createStyle(fgFill = "red")
  style1 <- createStyle(fgFill = "darkorange")
  style2 <- createStyle(fgFill = "yellow")
  style3 <- createStyle(fgFill = "lightgreen")
  styleGreen <- createStyle(fgFill = "#09a939")
  # Apply styles to specific cells
  namen <- names(df_combined)
  for(i in 1:ncol(df_combined)){
    if(grepl("validate",names(df_combined)[i])){
      for(j in 1:nrow(df_combined)){
        if(!is.na(df_combined[[j,i]]) && (df_combined[[j,i]]=="TRUE" || df_combined[[j,i]]==TRUE))
          addStyle(wb, sheet = sheetName, style = style3, rows = j+1, cols = i, gridExpand = TRUE)
        if(!is.na(df_combined[[j,i]]) && df_combined[[j,i]]=="Not testable")
          addStyle(wb, sheet = sheetName, style = style2, rows = j+1, cols = i, gridExpand = TRUE)
        if(!is.na(df_combined[[j,i]]) && (df_combined[[j,i]]=="FALSE" || df_combined[[j,i]]==FALSE))
          addStyle(wb, sheet = sheetName, style = style1, rows = j+1, cols = i, gridExpand = TRUE)
      }
    }
    # coef
    if( grepl("^LR",names(df_combined)[i]) ){ # All data props start with LR.
      for(j in 1:nrow(df_combined)){
        try({if(df_combined[[j,i]]>=2)
          addStyle(wb, sheet = sheetName, style = styleGreen, rows = j+1, cols = i, gridExpand = TRUE)}, silent = T)
        try({if(df_combined[[j,i]]>0 && df_combined[[j,i]]<2)
          addStyle(wb, sheet = sheetName, style = style3, rows = j+1, cols = i, gridExpand = TRUE)}, silent = T)
        try({if(df_combined[[j,i]]<0 && df_combined[[j,i]]> -2)
          addStyle(wb, sheet = sheetName, style = style1, rows = j+1, cols = i, gridExpand = TRUE)}, silent = T)
        try({if(df_combined[[j,i]]<=-2)
          addStyle(wb, sheet = sheetName, style = styleRed, rows = j+1, cols = i, gridExpand = TRUE)}, silent = T)
      }
    }
  }
  saveWorkbook(wb, filename, overwrite = TRUE)
  
  invisible(return(df_combined))
}
