
cat("Script script_5.x_Aim1_EquivalenceTests.R started ...\n\n")
print(paste0("dfFolder=",dfFolder))
print(paste0("outFolder=",outFolder))

try(dir.create(outFolder))
require(openxlsx)

pcaFile <- paste0(dfFolder,"/DF_summary_log_PCA.RDS")
if(!file.exists(pcaFile)){
  stop(pcaFile," does not exist. Maybe call 3.1.1Plots_DataProps* before.")
}else{
  df <- readRDS(pcaFile)
}
  
names(df) <- sub("\\.$","",names(df))

df[,"pcaDistance.1"] <- NULL  # Is it twice?
df[,"nsamplesWithCounts"] <- NULL  # Be sure that is is not used
df[,"log2.nsamplesWithCounts"] <- NULL  # Be sure that is is not used



## Eliminate redundant DCs:
raus <- readRDS(paste0(bs_getPath(0),"RedundantDataProps.RDS"))
raus <- c(raus,"sumCounts","ndata","medianDepth") # Be sure that obsolete data props are not used:
cat("\nData props before removing redundant ones (n=",length(names(df)),"):\n")
cat(paste(names(df),collapse="\n"))
df <- df[,!names(df) %in% raus,drop=F]
df <- df[,!names(df) %in% paste("log2.",raus,sep=""),drop=F]
cat("\n Data props after removing redundant ones (n=",length(names(df)),"):\n")
cat(paste(names(df),collapse="\n"))




saveRDS(df,file=paste0(outFolder,"/DF_summary_log_PCA.RDS"))

#df <- bs_eliminateCorrelatedFeatures(df,corMethod = "spearman", corThresh = 0.95, outFile = paste0(outFolder,"/DC_rankCors.pdf"))


## Available data characteristics
# Which data characteristics are there? (All numeric columns in df)
dataChars <- c()
for(i in 1:ncol(df)){
  if(is.numeric(df[,i]))
    dataChars <- c(dataChars, colnames(df)[i])
}
print(dataChars)




## Equivalence tests
### Calculation of mean and SD over templates

#nSigmas = 2 #c(1,2) # how many SDs from exp. templates are defined as equivalence region?
nSigma = 1 #c(1,2) # how many SDs from exp. templates are defined as equivalence region?
#for(nSigma in nSigmas){
  dfResult <- NULL
  dfResult_noNA <- NULL
  
  dfTemplate <- df[df$name=="original",]
  templateMeans <- array(NA, length(dataChars))
  templateSDs <- array(NA, length(dataChars))
  names(templateMeans) <- dataChars
  names(templateSDs) <- dataChars
  
  templateNames <- unique(dfTemplate$data_template) # available names of the experimental data templates
  sumSignificant <- array(0, length(templateNames))
  names(sumSignificant) <- templateNames
  
  for(char in dataChars){
    templateMeans[char] <- mean(dfTemplate[,char],na.rm=T) 
    templateSDs[char] <- nSigma*sd(dfTemplate[,char],na.rm=T) # disagreement with protocol
    
    # propSignificant[char] <- 
    
    for(template in templateNames){
      dfSimu <- df[df$data_template==template,] # only a specific template
      indices <- unique(c(grep("metaSPAR",dfSimu$name,ignore.case = T),
                          grep("sparseDOSSA2*",dfSimu$name,ignore.case = T)))
      
      dfSimu <- dfSimu[indices,] # only metaSPARsim
      
      simMean <- mean(dfSimu[,char],na.rm=T)
      simSD <- sd(dfSimu[,char],na.rm=T)
      simN <- sum(!is.na(dfSimu[,char]))
      
      if(!is.na(simSD)){
        
        if(simSD>0){
          # result <- TOSTER::tsum_TOST(m1 = simMean-templateMeans[char], sd1 = simSD, n1 = simN, eqb = templateSDs[char], hypothesis = "EQU",eqbound_type="raw")
          tryCatch({
            t_test_lower <- t.test(dfSimu[,char], alternative = "greater", mu = templateMeans[char]+templateSDs[char])
            t_test_upper <- t.test(dfSimu[,char], alternative = "less", mu = templateMeans[char]-templateSDs[char])
            
            pval <- min(1,2*min(t_test_lower$p.value,t_test_upper$p.value)) # account for testing twice
            # pval <- min(1,pval*2) # adjust for making two tests
          }, error=function(e){
            # try(save.image(file=paste0("bs_decompress_error.Rdata"))) # takes too long
            cat("pval calculation failed in equivalence testing: ",conditionMessage(e),", date: ",as.character(Sys.time())," for characteristic ",char,"\n")
            #saveRDS(dfSimu,file="t.test_failed.RDS")
          })
        }
        else{ # If simu always has the same value, use quantile over exp. templates as p-value
          ecdf1 = sum(simMean>=dfTemplate[,char],na.rm=T)/sum(!is.na(dfTemplate[,char]))
          ecdf2 = sum(simMean>=dfTemplate[,char],na.rm=T)/sum(!is.na(dfTemplate[,char]))
          pval <- min(1,min(ecdf1,ecdf2)*2) # two-sided
        }
        
        if(pval<0.05){
          sumSignificant[template] <- sumSignificant[template] + 1
          signiChar <- TRUE
        }
        else{
          signiChar <- FALSE
        }
        
        #cat("Equivalence test for ",char," and template ", template,": p=",pval,"\n")
        
        dfTmp <- data.frame(characteristic=char, template=template, dataMean=templateMeans[char], templateSD=templateSDs[char],
                            simN = simN, simMean=simMean, simSD = simSD, lowerBound = templateMeans[char]-templateSDs[char], 
                            upperBound = templateMeans[char]+templateSDs[char],
                            p.value=pval, significant = signiChar)
        
        
        if(is.null(dfResult)){
          dfResult_noNA <-  dfTmp
          dfResult <-  dfTmp
        }else{
          dfResult_noNA <- rbind(dfResult_noNA,dfTmp)
          dfResult <- rbind(dfResult,dfTmp)
        }
      }
    }
    
    if(sum(dfResult$significant[dfResult$characteristic==char],na.rm=T)>length(dfResult$significant[dfResult$characteristic==char])/2)
      dfResult$significant[dfResult$characteristic==char] <- NA
    
  }
  
  
  if(!is.null(dfResult)){
    cns <- dfResult$characteristic
    didLog <- grep("log2.",cns)
    cns <- sub("log2.","",cns)
    cns[didLog] <- paste(cns[didLog],"_log2",sep="")
    dfResult$characteristic <- cns
    
    p <- ggplot(dfResult, aes(x = characteristic, y = template, fill = significant)) +
      geom_tile(color = "white") +
      labs(title = "Non-equivalence indicated by significant equivalence test (see Fig. 2 in protocol)", xlabel="data characteristic")  +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90))
    p <- p + theme(strip.text = element_text(size = rel(1)),
                   plot.title = element_text(size = rel(1.5)),     # Decrease title font size 
                   axis.title = element_text(size = rel(1.5)),     # Decrease axis titles font size 
                   axis.text = element_text(size = rel(1.3)),      # Decrease axis text font size
                   legend.title = element_text(size = rel(1.5)),   # Decrease legend title font size 
                   legend.text = element_text(size = rel(1.5)))     # Decrease legend text font size 
    
    
    save(dfResult,p,outFolder,file=paste0(outFolder,"/Image_SignificantEquivalenceTests_",nSigma,"SD.Rdata"))
    pdf(paste0(outFolder,"/Image_SignificantEquivalenceTests_",nSigma,"SD.pdf"),width = 20,height = 20)
    print(p)
    dev.off()
    
    write.xlsx(dfResult,file=paste0(outFolder,"/Result_EquivalenceTests_",nSigma,"SD.xlsx"))
    write.xlsx(dfResult_noNA,file=paste0(outFolder,"/Result_EquivalenceTests_",nSigma,"SD_noNA.xlsx"))
    
    
    p <- ggplot(dfResult, aes(x = template, group = characteristic)) +
      # Add error bars for dataMean and templateSD
      geom_errorbar(aes(ymin = dataMean - templateSD, ymax = dataMean + templateSD), width=.2, color="blue") +
      # Add error bars for simMean and simSD
      geom_errorbar(aes(ymin = simMean - simSD, ymax = simMean + simSD), width=.5, color="red") +
      # Add line segments for lower and upper bounds
      geom_segment(aes(x = as.numeric(as.factor(template)) - 0.2, xend = as.numeric(as.factor(template)) + 0.2,
                       y = lowerBound, yend = lowerBound), linetype="dashed", color="gray") +
      geom_segment(aes(x = as.numeric(as.factor(template)) - 0.2, xend = as.numeric(as.factor(template)) + 0.2,
                       y = upperBound, yend = upperBound), linetype="dashed", color="gray") +
      # Facet by characteristic
      facet_wrap(~ characteristic, scales = "free_y") +
      labs(title = "Data characteristics analyzed by equivalence test (see Fig. 1 in protocoll)", x = "Template Name", y = paste0("Blue: +/- ",nSigma,"SD of for exp. template, red: +/- 1SD for simulated data")) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90))
    
    save(dfResult, p, outFolder, file=paste0(outFolder,"/Characteristics_for_EquivalenceTests_",nSigma,"SD.Rdata"))
    pdf(paste0(outFolder,"/Characteristics_for_EquivalenceTests_",nSigma,"SD.pdf"),width = 20,height = 20)
    print(p)
    dev.off()
    
  } else {
    cat("script_5.x_Aim1_EquivalenceTests.R: is.null(dfResult) ! Error?\n")
  }
  
  
  
  sumSignificant_NA <- array(NA,dim=length(templateNames))
  names(sumSignificant_NA) <- templateNames
  for(project in templateNames){
    irow <- which(dfResult$template==project)
    sumSignificant_NA[project] <- sum(dfResult[irow,]$significant,na.rm=T)
  }
  
  qs <- quantile(sumSignificant_NA,c(.25,.5,.75))
  qs[2] <- mean(sumSignificant_NA,na.rm=T)
  toRemove <- sumSignificant_NA > qs[3] + 1.5*(qs[3]-qs[1]) | sumSignificant_NA < qs[1] - 1.5*(qs[3]-qs[1])
  #  qs <- quantile(sumSignificant,c(.25,.5,.75))
  #  qs[2] <- mean(sumSignificant,na.rm=T)
  # toRemove <- sumSignificant > qs[3] + 1.5*(qs[3]-qs[1]) | sumSignificant < qs[1] - 1.5*(qs[3]-qs[1])
  toRemove <- toRemove[toRemove] # names are important
  
  cat("The following data sets are indicated to be removed: \n",paste(names(toRemove),collaps="\n"))
  sink(paste0(outFolder,"/ToRemove_because_outlierInSimilarityWithTemplate.txt"))
  cat("The following data sets are indicated to be removed: \n",paste(names(toRemove),collaps="\n"))
  sink()
  
  saveRDS(toRemove,paste0(outFolder,"/ToRemove_because_outlierInSimilarityWithTemplate.RDS"))
  
  sink(paste0(outFolder,"/NumberOfNonEquivalentDataProps.txt"))
  cat("Number of cases where equivalence tests are significant: \n")
  cat(paste0(sum(dfResult$significant,na.rm=T))," out of ",nrow(dfResult))
  sink()
  
  saveRDS(data.frame(NumberEquivSignificant=sum(dfResult$significant,na.rm=T), 
                     NumberEquivTests = nrow(dfResult)),
          file=paste0(outFolder,"/NumberOfNonEquivalentDataProps.RDS"))
  
  
  
  # Sample data
  plot_df <- data.frame(
    NumberNonEquivalence = sumSignificant_NA,
    x = array(1,dim=length(sumSignificant_NA)),
    name = names(sumSignificant_NA)
  )
  # Identify and annotate outliers
  outliers <- plot_df %>%
    mutate(
      is_outlier = (NumberNonEquivalence < quantile(NumberNonEquivalence, 0.25) - 1.5 * IQR(NumberNonEquivalence)) | (NumberNonEquivalence > quantile(NumberNonEquivalence, 0.75) + 1.5 * IQR(NumberNonEquivalence))
    ) %>%
    filter(is_outlier)
  
  # Create the boxplot
  myPlot <- ggplot(plot_df, aes(x = x, y = NumberNonEquivalence)) +
    geom_boxplot(outlier.shape = NA) +  # Suppress default outlier points
    geom_text(data = outliers, aes(label = name, y=NumberNonEquivalence ), color = "darkred", hjust=1.5, size=6) +
    geom_jitter(width = 0.15, height=0.1, alpha = 0.5) + 
    theme_minimal() +
    coord_cartesian(xlim = c(-1, 2)) +  # Adjust x-axis limits
    theme(
      plot.title = element_text(size = 12),       # Title font size
      axis.title.x = element_text(size = 16),     # X-axis title font size
      axis.title.y = element_text(size = 16),     # Y-axis title font size
      axis.text.y = element_text(size = 16),      # Y-axis text font size
      legend.text = element_text(size = 12),      # Legend text font size
      legend.title = element_text(size = 14),      # Legend title font size
      axis.text.x = element_blank(),              # Remove x-axis text (tick labels)
      axis.ticks.x = element_blank()
    ) +
    labs(title = "Outlier templates in equivalence tests", x = "", y = "Number of significant data properties")
  
  print(myPlot)
  save(plot_df, outliers, myPlot, outFolder, file=paste0(outFolder,"/Outlier_NoSignificantEQT_",nSigma,"SD.Rdata"))
  pdf(paste0(outFolder,"/Outlier_NoSignificantEQT_",nSigma,"SD.pdf"),width = 4,height = 6)
  print(myPlot)
  #boxplot(sumSignificant,ylab="Number of significant data properties per data template",main="Are there outlier templates?")  
  dev.off()
  print(sort(sumSignificant_NA))
  
#}



cat("\n\n 5.xAim1_EquivalenceTests.R finished :-)\n\n")

