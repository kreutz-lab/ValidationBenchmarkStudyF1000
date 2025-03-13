#' bs_DA2dataFrame
#'
#' Collects the results stored in the object by bs_DA and converts it to a data.frame 
#'
#' details
#'
#' @param d data_to_compare object
#' @param 
#'
#' @return
#' 
#' @examples
#' 
#'
#' 
#'
#' @seealso \code{\link{anotherFunction}}
#' @keywords aKeywordSuchAsBenchmarking
#' @export

bs_DA2dataFrame <- function(d, addTruth=FALSE){
  if(!msb.attrCompare(d,"bstype","data_list"))
    stop("bs_DA2dataFrame is up to now only implemented for a data_list (whole data_to_compare object, do subsetting via msb.subset)")
  
  cat("bs_DA2dataFrame() started...\n")
  namen <- names(d)
  
  df <- NULL
  
  for(i in 1:length(d)){ # loop over all data_projects
    if(msb.attrCompare(d[[i]],"bstype","data_project")){
      
      project_label <- namen[i]
      namen2 <- names(d[[i]])
      print(namen[i])
      
      for(j in 1:length(d[[i]])){ # loop over all data sets
        if(any(msb.attrCompare(d[[i]][[j]],"bstype",c("data_template","sim_result")))){
          namen3 <- names(d[[i]][[j]]) #      
          if(any(namen3=="DA")){
            DA <- d[[i]][[j]]$DA
            
            if(!is.null(nrow(DA$p_value))){
              # rn <- rownames(DA$p_value)
              # if(length(rn)==0)
              rn = paste(namen[i],1:nrow(DA$p_value),sep="_")
              rownames(DA$p_value) <- NULL
              rownames(DA$p_adjusted) <- NULL
              rownames(DA$logFold) <- NULL
              
              testLabels <- setdiff(unique(c(colnames(DA$logFold),colnames(DA$p_value),colnames(DA$p_adjusted))),"")
              if(length(testLabels)>0){
                
                for(k in 1:length(testLabels)){
                  # I have to put it together in this way to account for the fact that some test-results (i.e. columns) might be missing:
                  maxNrow <- max(sapply(list(DA$logFold,DA$p_value,DA$p_adjusted),nrow))
                  threeCols <- matrix(NA,nrow=maxNrow,ncol=4)
                  colnames(threeCols) <- c("logFold","p_value","p_adjusted","truth")
                  if(any(colnames(DA$logFold)==testLabels[k]))
                    threeCols[,"logFold"] <- DA$logFold[,testLabels[k]]
                  if(any(colnames(DA$p_value)==testLabels[k]))
                    threeCols[,"p_value"] <- DA$p_value[,testLabels[k]]
                  if(any(colnames(DA$p_adjusted)==testLabels[k]))
                    threeCols[,"p_adjusted"] <- DA$p_adjusted[,testLabels[k]]
                  # add truth
                  if(any(names(d[[i]][[j]])=="truth"))
                    threeCols[,"truth"] <- d[[i]][[j]]$truth[1:nrow(threeCols)] ## Attention, it is assumed that the first features are tested by DA
                  
                  
                  if(addTruth)
                    dftmp <- data.frame(project=project_label,dataset=namen2[j],test=testLabels[k],truth=threeCols[,"truth"],logFold=threeCols[,"logFold"],p_value=threeCols[,"p_value"],p_adjusted=threeCols[,"p_adjusted"],feature=rn)
                  else
                    dftmp <- data.frame(project=project_label,dataset=namen2[j],test=testLabels[k],logFold=threeCols[,"logFold"],p_value=threeCols[,"p_value"],p_adjusted=threeCols[,"p_adjusted"],feature=rn)
                  
                  if(is.null(df))
                    df <- dftmp
                  else
                    df <- rbind(df,dftmp)
                } # loop over testNames
              }else{
                cat("No testNames found. Maybe all failed?\n")
                failNames <- setdiff(names(DA),c("logFold","p_value","p_adjusted"))
                for(failName in failNames){
                  print(DA[[failName]])
                }
              }
            }
          }
        }
      }
    }
  } 
  
  if(addTruth && sum(!is.na(df$truth))==0)
    warning("addTruth=TRUE, but no truth found. Call bs_addTruth first.")
  # short rownames:
  # df$feature <- substr(df$feature, 1, 25)
  
  cat("bs_DA2dataFrame() finished.\n")
  return(df)
}



