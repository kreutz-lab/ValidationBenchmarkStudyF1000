# This function merges p-values, i.e. calls msb.combine_pvalues for each
# ...$DA$p_value
# It also calculates $DA$p_adjuste
#
# Examples:
# d <- bs_combine_pvalues(data_to_compare,"Maaslin2",setdiff(colnames(pvals),"Maaslin2"))

bs_combine_pvalues <- function(d,targetName,mergeNames){
  
  for(i in 1:length(d)){ # loop over all data_projects
    if(msb.attrCompare(d[[i]],"bstype","data_project")){
      for(j in 1:length(d[[i]])){ # loop over all data sets
        if(bs_isa(d[[i]][[j]],c("data_template","sim_result"))){
          if("DA" %in% names(d[[i]][[j]])){
            if("p_value" %in% names(d[[i]][[j]]$DA)){
              if(targetName %in% colnames(d[[i]][[j]]$DA$p_value)){
                
                d[[i]][[j]]$DA$p_value <- msb.combine_pvalues(d[[i]][[j]]$DA$p_value,targetName = targetName, mergeNames = mergeNames)
                
#                colNews <- setdiff(colnames(d[[i]][[j]]$DA$p_value),colnames(d[[i]][[j]]$DA$p_adjusted))
                colNews <- grep("<-",colnames(d[[i]][[j]]$DA$p_value),value=T)
                cns1 <- colnames(d[[i]][[j]]$DA$p_adjusted)
                cns2 <- colnames(d[[i]][[j]]$DA$logFold)
                for(colNew in colNews){
                  print(colNew)
                  d[[i]][[j]]$DA$p_adjusted <- cbind(d[[i]][[j]]$DA$p_adjusted,p.adjust(d[[i]][[j]]$DA$p_value[,colNew],method = "BH"))
                  d[[i]][[j]]$DA$logFold <- cbind(d[[i]][[j]]$DA$logFold,array(NA,dim=nrow(d[[i]][[j]]$DA$logFold))) # ToDo
                  cns1[length(cns1)+1] <- colNew
                  cns2[length(cns2)+1] <- colNew
                }
                colnames(d[[i]][[j]]$DA$p_adjusted) <- cns1
                colnames(d[[i]][[j]]$DA$logFold) <- cns2
                
              }   
            }
          }
        }
      }
    }
  }
  
  return(d)
}