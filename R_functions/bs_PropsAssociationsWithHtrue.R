# bs_PropsAssociationsWithHtrue(H_true,d, label="Hypothesis 1")
# 
# This function adds data properties to H_true
# Then stepwise regression in forward mode is (i.e. starting with the null model)
# is conducted using scaled predictors.
# 
# H_true data.frame with data_names as a column
# d a dataList, i.e. the data_to_compare object
#
# @param useRanks Should rank transformed predictors be considered in stepwise regression?
#
# @param label required if H_true is an array.
# @param useProject 0: not used as predictor (default)
#        1: only project used
#        2: project and other data properties
# @param load_H_true_all  [TRUE] 
#        TRUE: Loading of H_true_all.RDS if it exists. 
#        FALSE: The existing RDS is overwritten
#
# Output list with following elements:
#  dataframe with H_true and data properties
#  fit fitted logistic model after stepwise forward selection
#
# Example:
# d <- readRDS("../Results/3.1_metaSPARSim_DataProps/data_to_compare.RDS")
# ...

bs_PropsAssociationsWithHtrue <- function(H_true,d, label="H_true", useRanks=F, useProject=0, outDir=NULL,aim2res, load_H_true_all=T){
  
  # make matrix, if an array, use label als colname
  if(!is.matrix(H_true)){
    tmp <- matrix(H_true,nrow=length(H_true))
    rownames(tmp) <- names(H_true)
    colnames(tmp) <- c(label)
    H_true <- tmp
  }
  
  # only the last ncol(H_true) aim2res rows are relevant 
  aim2res <- aim2res[(nrow(aim2res)-ncol(H_true)+1):nrow(aim2res), ]
  
  if(is.null(colnames(H_true)))
    stop("colnames(H_true) required since it will be used as response variable name")
  
  
  ## Call core function for each column:
  propsAssoc <- list()
  for(i in 1:ncol(H_true)){
    tryCatch({
      aim2resInfo <- aim2res[i,c("prefix","hypoNumber","propTrue","hypothesis")]
      aim2resInfo$hypoNumber <- aim2resInfo$hypoNumber#+(i-1)*0.1
      propsAssoc[[i]] <- bs_PropsAssociationsWithHtrue_core(H_true[,i,drop=F], d, useRanks=useRanks, useProject=useProject, outDir=outDir, aim2resInfo=aim2resInfo, load_H_true_all)
    }, error = function(e){
      cat("Problem in H[,",i, "] \n")
      print(e)})
  }
  return(propsAssoc)
}

## Analysis for one column H_true, one aim2resInfo
bs_PropsAssociationsWithHtrue_core <- function(H_true, d, useRanks, useProject, outDir, aim2resInfo, load_H_true_all){
  print(ncol(H_true))
  if(ncol(H_true)>1 || nrow(aim2resInfo)>1)
    stop("bs_PropsAssociationsWithHtrue_core: ncol(H_true)>1 || nrow(aim2resInfo)>1")
  
  colnames(H_true) <- "H_true" # overwrite if the test man has been used as colname
  # Functions
  selectProps <- function(props,dataSet=""){
    namen <- names(props)
    propDf <- NULL
    for(i in 1:length(props)){
      if(is.numeric(props[[i]]) && length(props[[i]])==1){
        if(is.null(propDf))
          propDf <- setNames(data.frame(props[[i]]), namen[i])
        else
          propDf <- cbind(propDf, setNames(data.frame(props[[i]]), namen[i]))
      }
    }
    propDf <- data.frame(propDf, dataSet=array(dataSet,dim = nrow(propDf)))
    return(propDf)
  }
  
  # Convert matrix H_true to data.frame
  H_true <- data.frame(H_true)
  nameHypo = paste0("H",aim2resInfo$hypoNumber)
  cat("bs_PropsAssociationsWithHtrue_core for ",nameHypo,"\n ...")
  
  ## Build data_names from dataList d
  data_names_df <- NULL
  namen <- names(d)
  for(i in 1:length(d)){
    if(bs_isa(d[[i]],"data_project")){
      namen2 <- names(d[[i]])
      for(j in 1:length(d[[i]])){
        if(bs_isa(d[[i]][[j]],c("data_template","sim_result"))){
          tmp <- data.frame(data_name=paste0(namen[i],"_",namen2[j]),i=i,j=j)
          if(is.null(data_names_df))
            data_names_df <- tmp
          else
            data_names_df <- rbind(data_names_df,tmp)
        }
      }
    }
  }
  
  df_Htrue <- NULL
  rns <- NULL
  dnames <- names(d)
  # now add props:
  for(ii in 1:nrow(H_true)){
    cat(".")
    data_name <- rownames(H_true)[ii]
    irow <- which(data_names_df$data_name==data_name)
    if(length(irow)==1){
      projectIndex <- data_names_df$i[irow]
      tmp <- cbind(H_true[ii,],selectProps(d[[projectIndex]][[data_names_df$j[irow]]]$data.Prop, dnames[projectIndex]),data_name=data_name,nameHypo=nameHypo)
      
      if(is.null(df_Htrue)){
        df_Htrue <- tmp
        rns <- c(paste(rownames(H_true[ii,]),data_name, nameHypo,sep="."))
      }
      else{
        df_Htrue <- rbind(df_Htrue,array(NA,dim=ncol(tmp)))
        df_Htrue[nrow(df_Htrue),names(tmp)] <- tmp
        rns <- c(rns,paste(rownames(H_true[ii,]),data_name, nameHypo,sep="."))
      }
    } else{
      if(length(irow)<1)
        cat(data_name,"not found\n")
      else
        cat(data_name,"found ",length(irow),"times\n")
    }
  }
  if(is.null(rns)){
    cat("No matching data names:\n")
    print(rownames(H_true))
    print(data_names_df)
  }
  
  rownames(df_Htrue) <- rns
  cat("\n")
  
  namen <- names(df_Htrue)
  namen[1:ncol(H_true)] <- names(H_true)
  namen <- gsub("-", "_", namen)  # Replace '-' with '_'
  namen <- gsub(" ", "_", namen)  # Replace spaces with '_'
  namen <- make.names(namen)      # Ensure valid name
  names(df_Htrue) <- namen
  
  df_Htrue$dataSet <- as.factor(df_Htrue$dataSet)
  # eliminate props that are missing in more than 50% of rows:
  #keep <- apply(df_Htrue,2,function(x){sum(!is.na(x))})>nrow(df_Htrue)/2
  #keep[1:ncol(H_true)] <- TRUE # keep H_true results in any case
  #df_Htrue <- df_Htrue[,keep]
  
  #eliminate props that that have more missing value than the IQR
  missing_prop <- colMeans(is.na(df_Htrue))
  # Step 2: Identify outlier proportions using IQR method
  Q1 <- quantile(missing_prop, 0.25)  # First quartile
  Q3 <- quantile(missing_prop, 0.75)  # Third quartile
  IQR_val <- Q3 - Q1                 # Interquartile range
  # Define outliers as values greater than Q3 + 1.5 * IQR
  threshold <- Q3 + 1.5 * IQR_val
  # Step 3: Remove outlier columns
  if(sum(missing_prop)>0){
    cat("bs_PropAssicitations: ",sum(missing_prop>threshold)," data properties are missing too frequently:\n")
    cat(paste(names(df_Htrue)[missing_prop>threshold]),"\n")
  }
  df_Htrue <- df_Htrue[, missing_prop <= threshold]
  
  # Add scale predictors and add rank transformed predictors:
  for(col in setdiff(names(df_Htrue),c(names(H_true),c("dataSet","data_name","nameHypo")))){
    tmp <- scale(df_Htrue[,col])
    if(sum(is.na(tmp))>length(tmp)*.5){
      df_Htrue[,col] <- NULL # remove property if too many NAs
    }else{
      df_Htrue[,col] <- tmp
    }
  }
  
  
  namen <- names(df_Htrue) # update if some were removed
  
  if(!useRanks){
    if(useProject==0)
      scopeFormel <-  paste0("~",paste(setdiff(namen[(ncol(H_true)+1):length(namen)],c("dataSet","data_name","nameHypo")),collapse="+"))
    fileName <- paste0("NoRanks_onlyDataProps",nameHypo)
    if(useProject==1){ # only project
      scopeFormel <-  "~dataSet"
      fileName <- paste0("NoRanks_onlyProject_",nameHypo)
    }
    if(useProject==2){ # only project
      scopeFormel <-  paste0("~",paste(namen[(ncol(H_true)+1):length(namen)],collapse="+"))
      fileName <- paste0("NoRanks_DataPropsandProject_",nameHypo)
    }
  }
  
  # Add rank transformed predictors:
  for(col in setdiff(names(df_Htrue),c(names(H_true),c("dataSet","data_name","nameHypo")))){
    df_Htrue[,paste0(col,"_rank")] <- scale(rank(df_Htrue[,col]))
  }
  
  namen <- names(df_Htrue)
  if(useRanks){
    if(useProject==0)
      scopeFormel <-  paste0("~",paste(setdiff(namen[(ncol(H_true)+1):length(namen)],c("dataSet","data_name","nameHypo")),collapse="+"))
    fileName <- paste0("useRanks_onlyDataProps",nameHypo)
    if(useProject==1){ # only project
      scopeFormel <-  "~dataSet"
      fileName <- paste0("useRanks_onlyProject_",nameHypo)
    }
    if(useProject==2){ # only project
      scopeFormel <-  paste0("~",paste(namen[(ncol(H_true)+1):length(namen)],collapse="+"))
      fileName <- paste0("useRanks_DataPropsandProject_",nameHypo)
    }
  }
  
  # Do stepwise regression
  stepwise_models <- list()
  for(response in colnames(H_true)){
    if(sum(df_Htrue[,response],na.rm=T)==0){
      cat("Warning: Hypothesis never fulfilled for ",response," standard logistic regression not applicable.\n")
    }
    if(sum(!df_Htrue[,response],na.rm=T)==0){
      cat("Warning: Hypothesis never rejected for ",response," standard logistic regression not applicable.\n")
    }
    if(sum(!is.na(df_Htrue[,response]),na.rm=T)==0){
      cat("Warning: ",response," is NA throughout.\n")
    }
    
    df_noNA <- na.omit(df_Htrue) # remove rows with NA
    null_model <- glm(as.formula(paste(response, "~ 1")), data = df_noNA, family = binomial) # rows with NA will be omitted
    
    stepwise_model <- "Did not work" # will be overwritten if it works
    stepwise_model <- step(null_model, direction = "both", scope=as.formula(scopeFormel), trace=F, k=log(nrow(df_Htrue)))  # k=... corresponds to BIC which selects less variables than AIC
    
    stepwise_models[[response]] <- stepwise_model
    
    if(!is.null(outDir)){
      try(dir.create(outDir))
      if(length(colnames(H_true)) > 1 && !response==colnames(H_true)[1])
        sink(paste0(outDir,"/LogisticRegression_Result_",fileName,".txt"),append=TRUE)
      else
        sink(paste0(outDir,"/LogisticRegression_Result_",fileName,".txt"))
    }
    cat("Forward selection result for logistic regression:\n\n")
    cat("H_true = \n")
    print(H_true)
    print(summary(stepwise_model))
    
    if(!is.null(outDir))
      sink()
  }
  
  selectedModel <- stepwise_models[[length(stepwise_models)]]
  predictors <- all.vars(formula(selectedModel))[-1]
  
  if(!is.null(outDir)){
    try(dir.create(outDir))
    ## Save workspace with all H_true:
    H_true_all <- data.frame(H_true,dataSet=rownames(H_true),
                             hypoNr = array(nameHypo,dim=nrow(H_true)),
                             folder=array(outDir,dim = nrow(H_true)),
                             fileName=array(fileName,dim=nrow(H_true)),
                             useRanks=array(useRanks,dim=nrow(H_true)))
    if(load_H_true_all && file.exists(paste0(outDir,"/H_true_all.RDS"))){
      H_true_old <- readRDS(paste0(outDir,"/H_true_all.RDS"))
      H_true_all <- dplyr::bind_rows(H_true_old,H_true_all)
    }
    saveRDS(H_true_all,file=paste0(outDir,"/H_true_all.RDS"))
    
    require(rpart)
    require(rpart.plot)
    # Create scaled ranks, i.e. in [0,1]
    df_noNA_ranks <- df_noNA
    for(name in predictors){
      df_noNA_ranks[,name] <- rank(df_noNA_ranks[,name])
      df_noNA_ranks[,name] <- df_noNA_ranks[,name] - min(df_noNA_ranks[,name],na.rm=T)
      df_noNA_ranks[,name] <- df_noNA_ranks[,name] / max(df_noNA_ranks[,name],na.rm=T)
    }
      
    pngfile = paste0(outDir,"/DecisionTree_H",aim2resInfo$hypoNumber,".png")
#    save(stepwise_model, df_noNA, file=paste0(outDir,"/DecisionTree_H",aim2resInfo$hypoNumber,".Rdata"))
    CairoPNG(filename=pngfile,width=600,height=600)
    titel <- bs_replaceFoldername(paste0(outDir,": H",aim2resInfo$hypoNumber))
    subtitel <- aim2resInfo$hypothesis                                  
    rpart.plot(rpart(formula(stepwise_model),data=df_noNA_ranks,method="class", control = rpart.control(maxdepth = 3)),type=5,
               main=titel,sub=subtitel,cex.main = 1.1, cex.sub = 0.65)
    dev.off()
    
    ## Save workspace with all df_H_true:
    df_H_true_all <- df_Htrue
    if(load_H_true_all && file.exists(paste0(outDir,"/df_H_true_all.RDS"))){
      df_H_true_old <- readRDS(paste0(outDir,"/df_H_true_all.RDS"))
      df_H_true_all <- dplyr::bind_rows(df_H_true_old,df_H_true_all)
    }
    saveRDS(df_H_true_all,file=paste0(outDir,"/df_H_true_all.RDS"))
  }  
  
  return(invisible(c(list(coefs = coef(summary(selectedModel)),
                          propHtrue = colSums(H_true,na.rm=T)/colSums(!is.na(H_true)),
                          nHtrue = colSums(!is.na(H_true)),
                          scope=scopeFormel, 
                          predictors = predictors,
                          outDir = outDir,
                          fileName = fileName,
                          #                      data = df_noNA,
                          useRanks=useRanks, useProject=useProject, outDir=outDir,nameHypo=nameHypo), 
                     as.list(aim2resInfo))))
  
}