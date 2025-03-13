#This function only checks, whether field $DA is available
#
# Example:
# status <- bs_checkDA(data_to_compare)
# status <- bs_checkDA(data_to_compare,nFailPrint=Inf) # print all failed DAs


bs_checkDA <- function(d, nFailPrint=100){
  
  namen <- names(d)
  df_success <- NULL
  
  tests <- NULL
  for(i in 1:length(d)){
    for(j in 1:length(d[[i]])){
      if(bs_isa(d[[i]][[j]],c("data_template","sim_result"))){
        if("DA" %in% names(d[[i]][[j]]))
          tests <- c(tests,colnames(d[[i]][[j]]$DA$p_value))
      }
    }
  }
  tests <- unique(tests)
  
  
  for(i in 1:length(d)){
    namen2 <- names(d[[i]])
    for(j in 1:length(d[[i]])){
      if(bs_isa(d[[i]][[j]],c("sim_result","data_template"))){
        for(test in tests){
          df_tmp <- data.frame(project=namen[i],dataset=namen2[j],test=test, success=F)
          
          if("DA" %in% names(d[[i]][[j]]))
            if("p_value" %in% names(d[[i]][[j]]$DA) && !is.null(colnames(d[[i]][[j]]$DA$p_value)) && test %in% colnames(d[[i]][[j]]$DA$p_value)){
anz <- 0
try(anz <- sum(!is.na(d[[i]][[j]]$DA$p_value[,test]))>0)

              if(anz>0){
                df_tmp <- data.frame(project=namen[i],dataset=namen2[j],test=test, success=T) # only in this case success
              }
            }
          
          if(is.null(df_success))
            df_success <- df_tmp
          else
            df_success <- rbind(df_success,df_tmp)
        }
      }
    }
  }
  if(!is.null(df_success)){
    if(sum(!df_success$success)> nFailPrint)
      warning(paste0(sum(!df_success$success), " out of ",nrow(df_success)," tests failed, call via  status <- bs_checkDA(...) will show information."))
    else{
      warning(paste0(sum(!df_success$success), " out of ",nrow(df_success)," tests failed, call via  status <- bs_checkDA(...) will show information."))
      print(df_success[!df_success$success,])
    }
  }
  else{
    print("No DA results available !!")
  }
  
  return(invisible(df_success))
}

