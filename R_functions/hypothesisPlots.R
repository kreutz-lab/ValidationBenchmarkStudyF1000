# hypothesisPlots(DF_significance,aim2result,outDir=getwd(),profiles){
# 
# Some hypotheses also work with less arguments
#
# aim2result$hypoNumber hypothesis number (required)
# aim2result$test used for labelling      (optional)
 
  
hypothesisPlots <- function(DF_significance,aim2result,outDir=getwd(),profiles){
  if(!aim2result$test %in% names(DF_significance))
    return()
  
  print(aim2result)
  hypoNr <- aim2result$hypoNumber
  
  test_cols <- which(sapply(DF_significance, is.logical)) # indices
  test_names <- names(DF_significance)[test_cols] 

  if(!"hypoNumber" %in%names(aim2result))
    stop("Hypothesis number aim2result$hypoNumber is missing.")
  
  label1 <- paste0(aim2result$prefix,"Hypothesis",hypoNr)
  if("test" %in%names(aim2result))
    label <- paste0(label1,": ",aim2result$test)
  
  titel <- paste0(label,"\n",aim2result$hypothesis)
  
  pdfFile <- paste0(outDir,"/",label1,".pdf")
  pdfFile2 <- paste0(outDir,"/",label1,"_2.pdf")
  
  
  ######### Now depending on hypothesis number: ###
  if(hypoNr %in% c(1,2)){
    if(is.null(outDir))
      bs_plotSignificance(DF_significance,file=NULL,titel=label)
    else
      bs_plotSignificance(DF_significance,file=pdfFile,titel=titel)
  }
  
  if(hypoNr %in% c(3,4)){
    if(is.null(outDir))
      bs_plotProfiles(profiles,NULL,titel=label)
    else
      bs_plotProfiles(profiles,file=pdfFile,titel=label)
  }
  

  if(hypoNr %in% c(1,5,5.1)){
    momTest <- aim2result$test
    df_tmp <- DF_significance[which(DF_significance[[momTest]]),]
    df_tmp <- cbind(df_tmp,
                    moreThanHalf=apply(df_tmp[,test_names],1,function(x){sum(x,na.rm=T)-1 > sum(!is.na(x))*0.5}))

    # sorting appropriately
    df_tmp <- df_tmp[order(df_tmp$project),]
    
    projectLev <- unique(df_tmp$project)
    projectLev <- projectLev[!is.na(projectLev)]
    
    for(proj in projectLev){
      ind <- which(df_tmp$project==projectLev)
      rf <- order(df_tmp[ind,c("moreThanHalf")])
      df_tmp[ind,] <- df_tmp[ind[rf],]
    }
    df_tmp$project[df_tmp$moreThanHalf] <- paste(df_tmp$project[df_tmp$moreThanHalf]," >50%",sep="")
    
    
    df <- df_tmp
    # Extract the logical matrix
    logical_matrix <- as.matrix(df[, sapply(df,is.logical)])  # Adjust the column indices to match your data
    
    # Convert logical matrix to numeric matrix
    # logical_matrix_numeric <- logical_matrix=="TRUE" 
    logical_matrix_numeric <- apply(logical_matrix, 2, as.numeric)
    
    # Create an annotation object for the row axis
    library(ComplexHeatmap)
    
    Farben <- setNames(rainbow(length(unique(df$project))), unique(df$project))
    FarbNamen <- names(Farben)
    for(i in 1:length(Farben)){
      if(length(grep("50%",FarbNamen[i]))==0)
        Farben[i] <- "gray"
      if(is.na(Farben[i]))
        Farben[i] <- "black"
    }

    row_annotation <- ComplexHeatmap::rowAnnotation(
      project = df$project,
      col = list(project = Farben)
    )
    
    # Create the heatmap
    heatmap <- ComplexHeatmap::Heatmap(
      logical_matrix_numeric,
      name = "Logical Values",
      col = c("0" = "yellow", "1" = "darkred"),
      show_row_names = FALSE,
      show_column_names = TRUE,
      row_names_side = "left",
      row_title = "Data sets",
      row_title_gp = gpar(fontsize = 10),
      column_title = "Tests",
      column_title_gp = gpar(fontsize = 10),
      left_annotation = row_annotation,
      cluster_rows = FALSE,
      use_raster = FALSE,
      cluster_columns = FALSE
    )
    
    # Draw the heatmap
    draw(heatmap)
    save(heatmap,logical_matrix_numeric, row_annotation, pdfFile2, file=paste0(pdfFile2,".Rdata"))
    pdf(width=10,height=20,file=pdfFile2)
    draw(heatmap)
    dev.off()
    
  }

  if(hypoNr %in% c(5,5.1, 6,6.1 )){
    # add how often significant
    howOften <- data.frame(apply(DF_significance[,test_names],1,function(x){sum(x,na.rm=T)})==1)
    namen2 <- c("1x")
    for(i in 2:length(test_names)){
      howOften <- data.frame(howOften,apply(DF_significance[,test_names],1,function(x){sum(x,na.rm=T)})==i)
      namen2 <- c(namen2,paste0(i,"x"))
    }
    names(howOften) <- namen2
    
    if(is.null(outDir))
      bs_plotSignificance(cbind(DF_significance,howOften),file=NULL,titel=label)
    else
      bs_plotSignificance(cbind(DF_significance,howOften),file=pdfFile,titel=titel)
    
    
  }
  
  
}



