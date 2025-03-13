# bs_eliminateCorrelatedFeatures
#
# This function calculates pairwise correlations of all numeric columns and eliminates
# redundant columns, i.e. with correlation above a threshold
#
# This is required for eliminating reduncant DCs e.g. before equivalence testing
#
# Example:
#
# df <- bs_eliminateCorrelatedFeatures(df,corMethod = "spearman", corThresh = 0.95, outFile = paste0(outFolder,"/DC_rankCors.pdf"))


bs_eliminateCorrelatedFeatures <- function(df=NULL,data_to_compare=NULL,corMethod="spearman",corThresh=0.95,outFile=NULL){
  
  if(is.null(df) && is.null(data_to_compare))
    stop("bs_eliminateCorrelatedFeatures either requires df or data_to_compare")
  if(is.null(df) && !is.null(data_to_compare))
    df <- bs_summarize(data_to_compare)
  
  ### Check for multi-colinearity:
  # Select only numeric columns
  numeric_data <- df[sapply(df, is.numeric)]
  # Calculate correlation matrix
  cor_matrix <- cor(numeric_data, method = corMethod)
  diag(cor_matrix) <- NA
  
  cor_data <- reshape2::melt(cor_matrix)
  
  
  
  cor_data1 <- cor_data
  myplot1 <- ggplot(cor_data1, aes(Var1, Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(x = "", y = "", title = paste0("Pairwise ",corMethod," correlation"), fill = "Correlation")
  
  cor_data$value[abs(cor_data$value)<corThresh] <- NA

  cor_data2 <- cor_data
  myplot2 <- ggplot(cor_data2, aes(Var1, Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(x = "", y = "", title = paste0("Pairwise ",corMethod," correlation"), fill = "Correlation")
  
  
  raus <- c()
  cor_data$Var1 <- as.character(cor_data$Var1)
  while(max(cor_data$value,na.rm = T)>corThresh && sum(!is.na(cor_data$value))>0){
    jetztRaus <- cor_data$Var1[which.max(abs(cor_data$value))]
    raus <- c(raus,jetztRaus)
    cor_data$value[cor_data$Var1==jetztRaus] <- NA
    cor_data$value[cor_data$Var2==jetztRaus] <- NA 
  }

  myplot3 <- ggplot(cor_data, aes(Var1, Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    labs(x = "", y = "", title = paste0("Pairwise ",corMethod," correlation"), fill = "Correlation")
  
  
  if(length(raus)>0){
    for(i in 1:length(raus)){
      df[raus[i]] <- NULL
    }
  }
  
  # print plot
  if(!is.null(outFile)){
    save(cor_data1, myplot1, file=sub(".pdf","1.Rdata",outFile,fixed=T))
    pdf(file=sub(".pdf","1.pdf",outFile,fixed=T),width = 15, height=15)
    print(myplot1)
    dev.off()
    save(cor_data2, myplot2, file=sub(".pdf","1.Rdata",outFile,fixed=T))
    pdf(file=sub(".pdf","2.pdf",outFile,fixed=T),width = 15, height=15)
    print(myplot2)
    dev.off()
    save(cor_data, myplot3, file=sub(".pdf","1.Rdata",outFile,fixed=T))
    pdf(file=sub(".pdf","3.pdf",outFile,fixed=T),width = 15, height=15)
    print(myplot3)
    dev.off()
  }else{
    print(myplot3)
  }
  
  return(df)
  
}

