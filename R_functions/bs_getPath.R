# This function generates all foldernames
#
# number 1.1, 1.2, 2.1, 2.2, 3.1, ...
#
# simulator:
# "msp" = metaSPARSim
# "spd" = sparseDOSSA2
#
# partReg T or F
# nearing T or F (ancom according to Nearing or ancombBC)
#
# nameAim2 "Aim2_primary", "Aim2_secondary"
#
#
# Examples:
# bs_getPath(5.2,"msp",nearing = F,partReg = T,nameAim2 = "Aim2_secondary")
# bs_getPath(5.2,"msp",nearing = F,partReg = F,nameAim2 = "Aim2_primary")
# bs_getPath(5.2,"msp",nearing = F,partReg = F)
# bs_getPath(4.4,"spd",nearing = F,partReg = F)
# bs_getPath(3.4,"spd",nearing = F,partReg = F)
# bs_getPath(3.2,"msp",nearing = F,partReg = T)
# bs_getPath(2.1,"msp")
# bs_getPath(1.2,"spd")
#
#
#
# for(analysis in 3:5){
#   if(analysis %in% c(3,4)){
#     addAll <- c(0.1,0.2,0.3,0.4)
#   }else{
#     addAll <- c(0.1,0.2)
#   }
#   for(toAdd in addAll){
#     number = analysis+toAdd
#     for(simulator in c("msp","spd")){
#       for(nearing in c(T,F)){
#         for(partReg in c(T,F)){
#           pfad <- bs_getPath(number,simulator, nearing = nearing ,partReg = partReg)
#           cat(pfad,"\n")
#           
#           if(analysis==5){
#             pfad <- bs_getPath(number,simulator, nearing = nearing ,partReg = partReg,nameAim2 = "Aim2_primary")
#             cat(pfad,"\n")
#           }
#         }
#       }
#     }
#   }
# }


bs_getPath <- function(number, simulator, partReg=F, nearing=F, nameAim2=""){
  
  folder <- NULL

  ## 0 (data)
  if(number==0){
    folder <- "../Data"
    simulator <- "egal"
  }
  
  ## 1 and 2:
  if(number==1.1 && simulator=="msp" && !partReg)
    folder <- "../Results_notUsed/1.1_metaSPARSim"
  if(number==1.2 && simulator=="spd" && !partReg)
    folder <- "../Results/1.2_sparseDOSSA"
  if(number==2.1 && simulator=="msp" && !partReg)
    folder <- "../Results_notUsed/2.1_metaSPARSim"
  if(number==2.2 && simulator=="spd" && !partReg)
    folder <- "../Results/2.2_sparseDOSSA"
  
  ## 3* msp:
  if(number==3.1 && simulator=="msp" && !partReg)
    folder <- "../Results/3.1_metaSPARSim_DataProps/"
  if(number==3.2 && simulator=="msp" && !partReg)
    folder <- "../Results/3.2_metaSPARSim_ZerosAdded_DataProps/"
  if(number==3.3 && simulator=="msp" && !partReg)
    folder <- "../Results/3.3_metaSPARSim_Filtered_DataProps/"
  if(number==3.4 && simulator=="msp" && !partReg)
    folder <- "../Results/3.4_metaSPARSim_filtered_ZerosAdded_DataProps/"
  ## 3* msp & partreg
  if(number==3.1 && simulator=="msp" && partReg)
    folder <- "../Results_partReg/3.1_metaSPARSim_DataProps/"
  if(number==3.2 && simulator=="msp" && partReg)
    folder <- "../Results_partReg/3.2_metaSPARSim_ZerosAdded_DataProps/"
  if(number==3.3 && simulator=="msp" && partReg)
    folder <- "../Results_partReg/3.3_metaSPARSim_Filtered_DataProps/"
  if(number==3.4 && simulator=="msp" && partReg)
    folder <- "../Results_partReg/3.4_metaSPARSim_filtered_ZerosAdded_DataProps/"
  
  ## 3* spd:
  if(number==3.1 && simulator=="spd" && !partReg)
    folder <- "../Results/3.1_sparseDOSSA_DataProps"
  if(number==3.2 && simulator=="spd" && !partReg)
    folder <- "../Results/3.2_sparseDOSSA_ZerosAdded_DataProps"
  if(number==3.3 && simulator=="spd" && !partReg)
    folder <- "../Results/3.3_sparseDOSSA_Filtered_DataProps"
  if(number==3.4 && simulator=="spd" && !partReg)
    folder <- "../Results/3.4_sparseDOSSA_filtered_ZerosAdded_DataProps"
  ## 3* spd & partReg:
  if(number==3.1 && simulator=="spd" && partReg)
    folder <- "../Results_partReg/3.1_sparseDOSSA_DataProps"
  if(number==3.2 && simulator=="spd" && partReg)
    folder <- "../Results_partReg/3.2_sparseDOSSA_ZerosAdded_DataProps"
  if(number==3.3 && simulator=="spd" && partReg)
    folder <- "../Results_partReg/3.3_sparseDOSSA_Filtered_DataProps"
  if(number==3.4 && simulator=="spd" && partReg)
    folder <- "../Results_partReg/3.4_sparseDOSSA_filtered_ZerosAdded_DataProps"
  
  
  ###########  4.x #################
  
  ## spd 
  if(number==4.1 && simulator=="spd" && !partReg && !nearing)
    folder <- "../Results/4.1_sparseDOSSA_DA"
  if(number==4.2 && simulator=="spd" && !partReg && !nearing)
    folder <- "../Results/4.2_sparseDOSSA_ZerosAdded_DA"
  if(number==4.3 && simulator=="spd" && !partReg && !nearing)
    folder <- "../Results/4.3_sparseDOSSA_filtered_DA"
  if(number==4.4 && simulator=="spd" && !partReg && !nearing)
    folder <- "../Results/4.4_sparseDOSSA_filtered_ZerosAdded_DA"
  ## spd & nearing
  if(number==4.1 && simulator=="spd" && !partReg       && nearing)
    folder <- "../Results/4.1_sparseDOSSA_DA_ancom_Nearing"
  if(number==4.2 && simulator=="spd" && !partReg       && nearing)
    folder <- "../Results/4.2_sparseDOSSA_ZerosAdded_DA_ancom_Nearing"
  if(number==4.3 && simulator=="spd" && !partReg       && nearing)
    folder <- "../Results/4.3_sparseDOSSA_filtered_DA_ancom_Nearing"
  if(number==4.4 && simulator=="spd" && !partReg       && nearing)
    folder <- "../Results/4.4_sparseDOSSA_filtered_ZerosAdded_DA_ancom_Nearing"
  ## spd & partReg
  if(number==4.1 && simulator=="spd" && partReg && !nearing)
    folder <- "../Results_partReg/4.1_sparseDOSSA_DA"
  if(number==4.2 && simulator=="spd" && partReg && !nearing)
    folder <- "../Results_partReg/4.2_sparseDOSSA_ZerosAdded_DA"
  if(number==4.3 && simulator=="spd" && partReg && !nearing)
    folder <- "../Results_partReg/4.3_sparseDOSSA_filtered_DA"
  if(number==4.4 && simulator=="spd" && partReg && !nearing)
    folder <- "../Results_partReg/4.4_sparseDOSSA_filtered_ZerosAdded_DA"
  ## spd  & partReg & nearing
  if(number==4.1 && simulator=="spd" && partReg       && nearing)
    folder <- "../Results_partReg/4.1_sparseDOSSA_DA_ancom_Nearing"
  if(number==4.2 && simulator=="spd" && partReg       && nearing)
    folder <- "../Results_partReg/4.2_sparseDOSSA_ZerosAdded_DA_ancom_Nearing"
  if(number==4.3 && simulator=="spd" && partReg       && nearing)
    folder <- "../Results_partReg/4.3_sparseDOSSA_filtered_DA_ancom_Nearing"
  if(number==4.4 && simulator=="spd" && partReg       && nearing)
    folder <- "../Results_partReg/4.4_sparseDOSSA_filtered_ZerosAdded_DA_ancom_Nearing"
  
  ## msp
  if(number==4.1 && simulator=="msp" && !partReg && !nearing)
    folder <- "../Results/4.1_metaSPARSim_DA"
  if(number==4.2 && simulator=="msp" && !partReg && !nearing)
    folder <- "../Results/4.2_metaSPARSim_ZerosAdded_DA"
  if(number==4.3 && simulator=="msp" && !partReg && !nearing)
    folder <- "../Results/4.3_metaSPARSim_filtered_DA"
  if(number==4.4 && simulator=="msp" && !partReg && !nearing)
    folder <- "../Results/4.4_metaSPARSim_filtered_ZerosAdded_DA"
  ## msp & partReg
  if(number==4.1 && simulator=="msp" && partReg && !nearing)
    folder <- "../Results_partReg/4.1_metaSPARSim_DA"
  if(number==4.2 && simulator=="msp" && partReg && !nearing)
    folder <- "../Results_partReg/4.2_metaSPARSim_ZerosAdded_DA"
  if(number==4.3 && simulator=="msp" && partReg && !nearing)
    folder <- "../Results_partReg/4.3_metaSPARSim_filtered_DA"
  if(number==4.4 && simulator=="msp" && partReg && !nearing)
    folder <- "../Results_partReg/4.4_metaSPARSim_filtered_ZerosAdded_DA"
  ## msp & Nearing
  if(number==4.1 && simulator=="msp" && !partReg    && nearing)
    folder <- "../Results/4.1_metaSPARSim_DA_ancom_Nearing"
  if(number==4.2 && simulator=="msp" && !partReg    && nearing)
    folder <- "../Results/4.2_metaSPARSim_ZerosAdded_DA_ancom_Nearing"
  if(number==4.3 && simulator=="msp" && !partReg    && nearing)
    folder <- "../Results/4.3_metaSPARSim_filtered_DA_ancom_Nearing"
  if(number==4.4 && simulator=="msp" && !partReg    && nearing)
    folder <- "../Results/4.4_metaSPARSim_filtered_ZerosAdded_DA_ancom_Nearing"
  ## msp & partReg & Nearing
  if(number==4.1 && simulator=="msp" && partReg    && nearing)
    folder <- "../Results_partReg/4.1_metaSPARSim_DA_ancom_Nearing"
  if(number==4.2 && simulator=="msp" && partReg    && nearing)
    folder <- "../Results_partReg/4.2_metaSPARSim_ZerosAdded_DA_ancom_Nearing"
  if(number==4.3 && simulator=="msp" && partReg    && nearing)
    folder <- "../Results_partReg/4.3_metaSPARSim_filtered_DA_ancom_Nearing"
  if(number==4.4 && simulator=="msp" && partReg    && nearing)
    folder <- "../Results_partReg/4.4_metaSPARSim_filtered_ZerosAdded_DA_ancom_Nearing"
  
  
  ## 5.x Aim 1 (generated by replacements from 4.x path)
  if(nchar(nameAim2)==0 && number>5 && number <6){
    folder <- bs_getPath(number-1,simulator = simulator, partReg = partReg, nearing = nearing)
    #folder <- sub("_ZerosAdded","",folder) # make names shorter
    folder <- sub("_DA","",folder) # make names shorter
    folder <- sub(as.character(number-1),paste0(as.character(number),"_Aim1"),folder)
  }
  
  ## 5.x Aim 2 (generated by replacements from 4.x path)
  if(nchar(nameAim2)>0 && number>5 && number <6){
    folder <- bs_getPath(number-1,simulator = simulator, partReg = partReg, nearing = nearing)
    folder <- sub("_ZerosAdded","",folder) # make names shorter
    folder <- sub("_filtered","",folder) # make names shorter
    folder <- sub("_DA","",folder) # make names shorter
    folder <- sub(as.character(number-1),paste0(as.character(number),"_",nameAim2),folder)
  }
  
  
  ## Checks at the end:
  if(is.null(folder)){
    cat("number=",number, "simulator=", simulator, "partReg=", partReg, "nearing=", nearing, "\n")
    stop("bs_getPath: folder option not yet defined. Please do it in R_functions/bs_getPath.R now!")
  }else{
    folder <- sub("//","/",paste0(folder,"/"),fixed=T) # Ensure one / at the end
  }
  if(!dir.exists(folder)) 
    cat("bs_getPath Warning! folder",folder,"does not yet exist! \n")
  
  
  return(folder)
  
}