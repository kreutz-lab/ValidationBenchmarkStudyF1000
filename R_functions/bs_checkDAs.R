# bs_checkDAs(d1,d2)
#
# This function calls bs_checkDA and identifies tests with different availability or
# success.
#
# d1  one data_to_compare object
# d2  another data_to_compare object
#
# Example:
# d1 <- readRDS("../Results/4.3_metaSPARSim_filtered_DA/data_to_compare.RDS")
# d2 <- readRDS("../Results/4.3_metaSPARSim_filtered_DA/data_to_compare_22-May24.RDS")
#
# bs_checkDAs(d1,d2)                # only checks availability
#
# bs_checkDAs(d1,d2,checkOutcome=T) # also checks the agreement of the outcome

bs_checkDAs <- function(d1,d2, checkOutcome=F, fullOutput=F){
  
  cat("\nFailed tests in d1:\n")
  info1 <- bs_checkDA(d1)
  cat("\nFailed tests in d2:\n")
  info2 <- bs_checkDA(d2)
  
  #merging rows in a single character
  merge1 <- apply(info1, 1, function(row) paste(row, collapse = " "))
  merge2 <- apply(info2, 1, function(row) paste(row, collapse = " "))
  
  notIn_1 <- setdiff(merge2,merge1)
  notIn_2 <- setdiff(merge1,merge2)
  
  out <- list(notIn_1=notIn_1, notIn_2=notIn_2)
  
  if(checkOutcome){ # check whether the result coincides
    anzOK   <- 0
    anzDiff <- 0
    different.nrow <- NULL
    different.DAresult <- NULL
    
    namen <- intersect(names(d1),names(d2))
    for(name in namen){
      if(bs_isa(d1[[name]],"data_project")){
        namen2 <- intersect(names(d1[[name]]),names(d2[[name]]))
        for(name2 in namen2){
          if(bs_isa(d1[[name]][[name2]],c("data_template","sim_result"))){
            if("DA" %in% names(d1[[name]][[name2]]) && "DA" %in% names(d2[[name]][[name2]])){
              tests1 <- colnames(d1[[name]][[name2]]$DA$p_value)
              tests2 <- colnames(d2[[name]][[name2]]$DA$p_value)
              
              notIn2 <- setdiff(tests1,tests2)
              if(length(notIn2)>0){
                for(test in notIn2){
                  if(sum(!is.na(d1[[name]][[name2]]$DA$p_value[,test]))>0)
                    cat(test,"not available in d2$",name,"$",name2,"\n")
                }
              }
              notIn1 <- setdiff(tests2,tests1)
              if(length(notIn2)>0){
                for(test in notIn1){
                  if(sum(!is.na(d2[[name]][[name2]]$DA$p_value[,test]))>0){
                    cat(test,"not available in d1$",name,"$",name2,"\n")
                  }
                }
              }
              
              nrow1 <- nrow(d1[[name]][[name2]]$DA$p_value)
              nrow2 <- nrow(d2[[name]][[name2]]$DA$p_value)
              if(nrow1!=nrow2){
                cat("d*$",name,"$",name2, "have unequal nrows, nrow(d1...DA$p_value)=",nrow1,"nrow(d2...DA$p_value)=",nrow2,"\n")
                different.nrow[length(differentNrow)+1] <- paste(name,name2)
              }else{ # same number of rows
                tests <- intersect(tests1,tests2)
                for(test in tests){
                  if(sum(abs(d1[[name]][[name2]]$DA$p_value[,test] - d2[[name]][[name2]]$DA$p_value[,test])>1e-3,na.rm=T)>0){
                    anzDiff <- anzDiff + 1
                    cat("d*$",name,"$",name2, "have different outcomes (p-value difference >1e-3)\n")
                    different.DAresult[length(different.DAresult)+1] <- paste(name,name2,test)
                  }else{
                    anzOK <- anzOK + 1
                    if(fullOutput)
                      cat("OK: ",test," coincides in d*$",name,"$",name2,"\n",sep="")
                  }
                }
              }
            }
          }
        }
      }
    }
    cat("\n\n",anzOK+anzDiff," tests are available in both objects with same nrows:\n")
    cat(anzOK," have identical outcome (pvalue difference <=1e-3).\n")
    cat(anzDiff," have different outcome (pvalue difference >1e-3).\n\n")
    
    if(!is.null(different.nrow))
      out$different.nrow <- different.nrow
    else
      out$different.nrow <- "None."
    
    if(!is.null(different.DAresult))
      out$different.DAresult <- different.DAresult
    else
      out$different.DAresult <- "None."
  }
  
  return(out)
}
