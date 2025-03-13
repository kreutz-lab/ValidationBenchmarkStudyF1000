# This function reads all data_to_compare*.RDS and produces a summary file
#
# The summary xlsx file has three sheeds for summaries at three levels:
# summary_RDS is a summary of the whole RDS file
# summary_projects is a summary of all projects in all RDS
# summary_datasets is a summary of all datasets in all projects in all RDS
#
# Examples:
# summary_RDS <- bs_summarize_all() 
# 
# # One specific folder/file:
# tmp <- bs_summarize_all(outfile="tmp.xlsx",folder="../Results_partReg/3.3_sparseDOSSA_Filtered_DataProps")


bs_summarize_all <- function(pattern= "data_to_compare",outfile="../bs_summarize_all.xlsx",folder="..",pattern2=NULL){
  library(openxlsx)
  
  fils <- list.files(folder,pattern = pattern, recursive = T, full.names = T)
  if(!is.null(pattern2))
    fils <- grep(pattern2,fils,value = T)
  
  file_details <- file.info(fils)
  modification_times <- file_details$mtime
  sizes_in_mb <- file_details$size / (1024^2)
  
  summary_datasets <- NULL
  summary_projects <- NULL
  summary_RDS <- data.frame(files=fils,
                            saveTimes=modification_times,
                            sizeInMB=sizes_in_mb,
                            sumCounts=array(data=NA,dim=length(fils)),
                            sumCounts_template=array(data=NA,dim=length(fils)),
                            sumCounts_simus=array(data=NA,dim=length(fils)),
                            anzDoneDA=array(data=NA,dim=length(fils)),
                            anzDoneDA_template=array(data=NA,dim=length(fils)),
                            anzDoneDA_simus=array(data=NA,dim=length(fils))
  )
  
  for(ii in 1:length(fils)){
    # Read RDS
    file <- fils[ii]
    cat("bs_summarize_all for file ",file,"...\n")
    
    d <- NULL
    try(d <- bs_decompress(readRDS(file)))
    if(is.null(d) || !bs_isa(d,"data_list")){
      cat(file,": reading failed!\n")
      next
    }
    
    projects <- names(d)
    
    # Call bs_summarize
    tmp <- NULL
    try(tmp <- bs_summarize(d))
    if(is.null(tmp)){
      cat(file,": bs_summarize failed!\n")
      next
    }
    
    tmp <- cbind(data.frame(file=array(file,dim=nrow(tmp))),tmp)
    
    # Extract infos:
    sumCounts <- array(data=NA,dim=length(d))
    sumCounts_template <- array(data=0,dim=length(d))
    sumCounts_simus <- array(data=0,dim=length(d))
    
    anz0 <- array(data=0,dim=length(d))
    anz0_template <- array(data=0,dim=length(d))
    anz0_simus <- array(data=0,dim=length(d))
    
    anzDoneDA <- array(data=0,dim=length(d))
    anzDoneDA_template <- array(data=0,dim=length(d))
    anzDoneDA_simus <- array(data=0,dim=length(d))
    
    ancomNearing <- array(data=0,dim=length(d))
    splitTests <- array(data=0,dim=length(d))

    for(i in 1:length(projects)){
      project <- projects[i]
      sumCounts[i] <- sum(tmp$sumCounts[tmp$data_template==project],na.rm=T)
      for(j in 1:length(d[[i]])){
        # cat(i,j,anzDoneDA,anzDoneDA_template,anzDoneDA_simus,"\n")
        if(bs_isa(d[[i]][[j]],"data_template")){
          sumCounts_template[i] <- sum(d[[i]][[j]]$counts,na.rm=T)
          anz0_template[i] <- anz0_template[i] + sum(d[[i]][[j]]$counts==0,na.rm=T)
          if(!is.null(d[[i]][[j]][["DA"]]) && !is.null(d[[i]][[j]][["DA"]][["p_value"]]))
            anzDoneDA_template[i] <- anzDoneDA_template[i] + sum(colSums(!is.na(d[[i]][[j]][["DA"]]$p_value))>0,na.rm=T)
          if(!is.null(d[[i]][[j]][["DA"]]) && !is.null(d[[i]][[j]][["DA"]][["splitTests"]]))
            splitTests[i] <- splitTests[i] + length(d[[i]][[j]][["DA"]]$splitTests)
        }
        
        if(bs_isa(d[[i]][[j]],"sim_result")){
          sumCounts_simus[i] <- sumCounts_simus[i] + sum(d[[i]][[j]]$counts,na.rm=T)
          anz0_simus[i] <- anz0_simus[i] + sum(d[[i]][[j]]$counts==0,na.rm=T)
          if(!is.null(d[[i]][[j]][["DA"]]) && !is.null(d[[i]][[j]][["DA"]][["p_value"]]))
            anzDoneDA_simus[i] <- anzDoneDA_simus[i] + sum(colSums(!is.na(d[[i]][[j]][["DA"]]$p_value))>0,na.rm=T)
        }
        
        if(bs_isa(d[[i]][[j]],c("data_template","sim_result"))){
          anz0[i] <- anz0[i] + sum(d[[i]][[j]]$counts==0,na.rm=T)
          if(!is.null(d[[i]][[j]][["DA"]]) && !is.null(d[[i]][[j]][["DA"]][["p_value"]])){
            anzDoneDA[i] <- anzDoneDA[i] + sum(colSums(!is.na(d[[i]][[j]][["DA"]]$p_value))>0,na.rm=T)
            if("ancom_Nearing" %in% names(d[[i]][[j]][["DA"]]))
              ancomNearing[i] <- ancomNearing[i] + 1
          }
        }
      }
    }
    
    
    if(is.null(summary_datasets)){
      summary_datasets <- tmp
    }else{
      summary_datasets <- dplyr::bind_rows(summary_datasets,tmp)
    }
    
    tmp_projects <- data.frame(file = array(file,dim=length(d)),
                               projects = projects,
                               ancomNearing=ancomNearing,
                               sumCounts=sumCounts,
                               sumCounts_template=sumCounts_template,
                               sumCounts_simus=sumCounts_simus,
                               anz0=anz0,
                               anz0_template=anz0_template,
                               anz0_simus=anz0_simus,
                               anzDoneDA=anzDoneDA,
                               anzDoneDA_template=anzDoneDA_template,
                               anzDoneDA_simus=anzDoneDA_simus,
                               splitTests=splitTests)
    
    if(is.null(summary_projects)){
      summary_projects <- tmp_projects
    }else{
      summary_projects <- rbind(summary_projects,tmp_projects)
    }
    
    
    summary_RDS[["anzTemplates"]][ii] <- length(projects)
    summary_RDS[["ancomNearing"]][ii] <- sum(tmp_projects$ancomNearing,na.rm=T)
    summary_RDS[["sumCounts"]][ii] <- sum(tmp_projects$sumCounts,na.rm=T)
    summary_RDS[["sumCounts_template"]][ii] <- sum(tmp_projects$sumCounts_template,na.rm=T)
    summary_RDS[["sumCounts_simus"]][ii] <- sum(tmp_projects$sumCounts_simus,na.rm=T)
    summary_RDS[["anz0"]][ii] <- sum(tmp_projects$anz0,na.rm=T)
    summary_RDS[["anz0_template"]][ii] <- sum(tmp_projects$anz0_template,na.rm=T)
    summary_RDS[["anz0_simus"]][ii] <- sum(tmp_projects$anz0_simus,na.rm=T)
    summary_RDS[["anzDoneDA"]][ii] <- sum(tmp_projects$anzDoneDA,na.rm=T)
    summary_RDS[["anzDoneDA_template"]][ii] <- sum(tmp_projects$anzDoneDA_template,na.rm=T)
    summary_RDS[["anzDoneDA_simus"]][ii] <- sum(tmp_projects$anzDoneDA_simus,na.rm=T)
    summary_RDS[["splitTests"]][ii] <- sum(tmp_projects$splitTests,na.rm=T)
    
    wb <- createWorkbook()
    
    openxlsx::addWorksheet(wb, "all_RDS")
    openxlsx::writeData(wb, "all_RDS", summary_RDS)
    
    openxlsx::addWorksheet(wb, "projects")
    openxlsx::writeData(wb, "projects", summary_projects)
    
    openxlsx::addWorksheet(wb, "datasets")
    openxlsx::writeData(wb, "datasets", summary_datasets)
    
    openxlsx::saveWorkbook(wb, paste0("tmp_",basename(outfile)), overwrite = TRUE)
  }
  
  
  wb <- openxlsx::createWorkbook()
  
  openxlsx::addWorksheet(wb, "all_RDS")
  openxlsx::writeData(wb, "all_RDS", summary_RDS)
  
  openxlsx::addWorksheet(wb, "projects")
  openxlsx::writeData(wb, "projects", summary_projects)
  
  openxlsx::addWorksheet(wb, "datasets")
  openxlsx::writeData(wb, "datasets", summary_datasets)
  
  openxlsx::saveWorkbook(wb, outfile, overwrite = TRUE)
  
  return(summary_RDS)
  
  
}

