require(openxlsx)

for(simulator in c("spd","msp")){
  
  if(simulator=="msp"){
    d <- readRDS("../Results_partReg/4.2_metaSPARSim_ZerosAdded_DA_ancom_Nearing/data_to_compare.all.RDS")
    d2 <- readRDS("../Results_partReg/4.4_metaSPARSim_filtered_ZerosAdded_DA_ancom_Nearing/data_to_compare.all.RDS")
  }else{
    d <- readRDS("../Results/4.2_sparseDOSSA_ZerosAdded_DA_ancom_Nearing/data_to_compare.all.RDS")
    d2 <- readRDS("../Results/4.4_sparseDOSSA_filtered_ZerosAdded_DA_ancom_Nearing/data_to_compare.all.RDS")
  }
  
  namen <- intersect(names(d),names(d2))
  
  dfcmp <- list()
  
  for(name in namen){
    namen2 <- setdiff(intersect(names(d[[name]]),names(d2[[name]])),"original")
    for(name2 in namen2){
      if("DA" %in% names(d[[name]][[name2]])){
        for(feld in c("p_value","p_adjusted")){
          tests <- intersect(colnames(d[[name]][[name2]]$DA[[feld]]),colnames(d2[[name]][[name2]]$DA[[feld]]))
          
          dif <- colSums(d[[name]][[name2]]$DA[[feld]][,tests]<0.05,na.rm=T) - colSums(d2[[name]][[name2]]$DA[[feld]][,tests]<0.05,na.rm=T)  
          tmp <- cbind(data.frame(template=name,dataset=name2),as.data.frame(t(dif)),data.frame(min=min(dif,na.rm=T),mean=mean(dif,na.rm=T)))
          if(!feld %in% names(dfcmp))
            dfcmp[[feld]] <- tmp
          else
            dfcmp[[feld]] <- bind_rows(dfcmp[[feld]],tmp)
        }
      }
    }
  }
  
  
  wb <- createWorkbook()
  
  for(sheet in names(dfcmp)){
    tests <- dfcmp[[sheet]][3:ncol(dfcmp[[sheet]])]
    #tests <- setdiff(tests,"ALDEx2")
    means  <- as.data.frame(t(colMeans(dfcmp[[sheet]][,tests],na.rm=T)))
    anzPos <- as.data.frame(t(colSums( dfcmp[[sheet]][,tests]>0,na.rm=T)))
    anzNeg <- as.data.frame(t(colSums( dfcmp[[sheet]][,tests]<0,na.rm=T)))
    
    dfcmp[[sheet]] <- rbind(dfcmp[[sheet]],cbind(data.frame(template="Average",dataset="",means)))
    dfcmp[[sheet]] <- rbind(dfcmp[[sheet]],cbind(data.frame(template="Sum>0",dataset="",anzPos)))
    dfcmp[[sheet]] <- rbind(dfcmp[[sheet]],cbind(data.frame(template="Sum<0",dataset="",anzNeg)))
    
    addWorksheet(wb, sheet)
    writeData(wb, sheet, dfcmp[[sheet]])
  }
  saveWorkbook(wb, paste0("AnzSig_unfilered-filtered.",simulator,".xlsx"), overwrite = TRUE)
  
}

