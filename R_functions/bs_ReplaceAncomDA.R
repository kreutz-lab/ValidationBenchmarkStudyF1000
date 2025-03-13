# This function replaces the results of our ancombc DA analysis by the
# result of Nearing's implementation,i.e. 
# d...$DA$..[,"ancombc"] is replaced by dnear...$DA$..[,"ancom_Nearing"]
#
# d and dnear are two data_to_compare objects
#
# If this replacement is done, it is indicated in the object via:
# d[[i]][[j]]$DA$ancom_Nearing = T 

bs_ReplaceAncomDA <- function(d,dnear){
  cat("bs_ReplaceAncomDA started ...\n")
  projects <- names(d)
  for(i in 1:length(d)){
    project <- projects[i]
    
    for(j in 1:length(d[[i]])){
      if(bs_isa(d[[i]][[j]],c("data_template","sim_result"))){
        dataset <- names(d[[i]])[j]
        rn1 <- row.names(d[[i]][[j]]$counts)
        notThere <- F
        if(!project %in% names(dnear)){
          cat("Warning: ancomb_nearning not available for project ",project,"\n")
          notThere <- T
        }else{
          if(!dataset %in% names(dnear[[project]])){
            cat("Warning: ancomb_nearning not available for dataset ",dataset," in project ",project,"\n")
            notThere <- T
          }else{
            rn2 <- row.names(dnear[[project]][[dataset]]$counts)
            if(!identical(rn1,rn2))
              stop("row.names(counts) not identical for ",project,", ",dataset)
          }
        }
        
        for(prop in c("logFold","p_value","p_adjusted")){
          if(notThere){
            values <- array(NA)
            d[[i]][[j]]$DA$ancom_Nearing = F 
          }else{
            d[[i]][[j]]$DA$ancom_Nearing = T 
            if(is.array(d[[project]][[dataset]]$DA[[prop]]))
              values <- dnear[[project]][[dataset]]$DA[[prop]]  # add a column
            else
              values <- dnear[[project]][[dataset]]$DA[[prop]][,"ancom_Nearing"]
          }
          if(!"ancombc" %in% colnames(d[[i]][[j]]$DA[[prop]])){
            d[[i]][[j]]$DA[[prop]] <- cbind(d[[i]][[j]]$DA[[prop]],ancombc=array(dim=nrow(d[[i]][[j]]$DA[[prop]])))
          }
          d[[i]][[j]]$DA[[prop]][,"ancombc"] <- values
        }

      } # if a dataset
      
    } # all datasets
  } # all projects
  
  
  cat("bs_ReplaceAncomDA finished.\n")
  return(d)
}


