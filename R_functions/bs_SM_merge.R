#' First step in the split & merge procedure:
#'   Create data sets by splitting
#'
#' @param d data object e.g. data_to_compare
#'  either data_list or data_project
#' @param k number of splits
#' 
#' Example:
#' dsplit <- bs_SM_split(d)
#' dsplit2 <- bs_DA(dsplit2,keepTestResult=T)
#' dsplit <- bs_DA(dsplit,keepTestResult=T)

bs_SM_merge <- function(dsplit){
  
  if(bs_isa(dsplit,"data_project")){# handle this case
    dlist <- list(element=dsplit)
    attr(dlist, "bstype") <- "data_list"
    
    dlist <- bs_SM_split(dlist, design=design, groupVariable=groupVariable, doSlowMethods=doSlowMethods, maxFeatures=maxFeatures, keepTestResult=keepTestResult,
                         whichMethods=whichMethods,
                         whichData=whichData,
                         overwriteOldDA=overwriteOldDA, parallelMode = parallelMode)
    return(dlist[[1]])
  } # data_project
  else
  { # data_list 
    if(!bs_isa(dsplit,"data_list"))
      warning("This function expexts bstype data_list object, such as data_to_compare: might fail ..")
    d <- dsplit
    namen <- names(d)
    for(i in 1:length(d)){
      if(bs_isa(d[[i]],c("data_project"))){
        namen2 <- names(dsplit[[i]])
        splitNames <- grep("split[0-9]+",namen2,value=T)
        splitLevs <- unique(sub("split[0-9]+","",splitNames))
        
        for(splitLev in splitLevs){
          splitNames <- grep(paste0(splitLev,"split[0-9]+"),namen2,value=T)
          nrowMax <- -Inf
          DAnames <- colnames(d[[i]][[splitNames[1]]]$DA$p_values)
          for(splitName in splitNames){
            nrowMax <- max(nrowMax,max(dsplit[[i]][[splitName]]$SM.row.indices,na.rm=T),na.rm=T)
            DAnames <- unique(c(DAnames,colnames(dsplit[[i]][[splitName]]$DA$p_value)))
            d[[i]][[splitName]] <- NULL
          }
          unsplitName <- strtrim(splitLevs,width = nchar(splitLev)-1)
          cat(paste0("d$",namen[i],"$",unsplitName," will be filled by: "))

          # unsplitted object obtains all from the 1st splitted (but counts and DA will be overwritten)
          d[[i]][[unsplitName]] <- dsplit[[i]][[splitNames[1]]] 
          
          # Empty DA matrix:
          d[[i]][[unsplitName]]$DA$logFold <- matrix(nrow=nrowMax,ncol=length(DAnames))
          colnames(d[[i]][[unsplitName]]$DA$logFold) <- DAnames
          d[[i]][[unsplitName]]$DA$p_value <- matrix(nrow=nrowMax,ncol=length(DAnames))
          colnames(d[[i]][[unsplitName]]$DA$p_value) <- DAnames
          d[[i]][[unsplitName]]$DA$p_adjusted <- matrix(nrow=nrowMax,ncol=length(DAnames))
          colnames(d[[i]][[unsplitName]]$DA$p_adjusted) <- DAnames
          rns<-paste("Row",1:nrowMax)
          # Empty counts:
          d[[i]][[unsplitName]]$counts <- matrix(nrow=nrowMax,ncol=ncol(dsplit[[i]][[splitNames[1]]]$counts))
          colnames(d[[i]][[unsplitName]]$counts) <- colnames(dsplit[[i]][[splitNames[1]]]$counts)
          attr(d[[i]][[unsplitName]]$counts,"bstype") <- attr(dsplit[[i]][[splitNames[1]]]$counts,"bstype")
          if(is.data.frame(dsplit[[i]][[splitNames[1]]]$counts))
            d[[i]][[unsplitName]]$counts <- as.data.frame(d[[i]][[unsplitName]]$counts)
          
          # Now fill it up
          for(splitName in splitNames){
            cat(paste0(splitName,", "))
            irow <- dsplit[[i]][[splitName]]$SM.row.indices
            d[[i]][[unsplitName]]$counts[irow,] <- dsplit[[i]][[splitName]]$counts
            rns[irow] <- rownames(dsplit[[i]][[splitName]]$DA$p_value)
            
            for(test in colnames(dsplit[[i]][[splitName]]$DA$p_value)){
              if(test %in% colnames(dsplit[[i]][[splitName]]$DA$logFold))
                d[[i]][[unsplitName]]$DA$logFold[irow,test] <- dsplit[[i]][[splitName]]$DA$logFold[,test]
              if(test %in% colnames(dsplit[[i]][[splitName]]$DA$p_value))
                d[[i]][[unsplitName]]$DA$p_value[irow,test] <- dsplit[[i]][[splitName]]$DA$p_value[,test]
              if(test %in% colnames(dsplit[[i]][[splitName]]$DA$p_adjusted))
                d[[i]][[unsplitName]]$DA$p_adjusted[irow,test] <- dsplit[[i]][[splitName]]$DA$p_adjusted[,test]
            }
          }
          rownames(d[[i]][[unsplitName]]$DA$logFold) <- rns
          rownames(d[[i]][[unsplitName]]$DA$p_value) <- rns
          rownames(d[[i]][[unsplitName]]$DA$p_adjusted) <- rns
          
          cat("\n")
        }
      } # if d[[i]] is data_project
    } # over all d[[i]]
    
  } # if d is not data_project
  
  return(d)
}

