# This function creates the config list

bs_config <- function(configName = NULL, simulator, mainFolder=""){
  
  # make default
  if(is.null(configName)){
    cfg <- list(
      Nsimu = 10,
      mainFolder = mainFolder,
      outfolder1 = paste0("../Results/1_",simulator,"_1Simu"),
      outfolder2 = paste0("../Results/2_",simulator,"_10Simu"),
      outfolder3 = paste0("../Results/3_",simulator,"_DataProps"),
      outfolder4 = paste0("../Results/4_",simulator,"_DA"),
      simulator = simulator,
      doAdd0=T)
  }else{
    
    # first get default, will be partly overwritten.
    cfg <- bs_config(NULL,simulator = simulator)#
    
    ########################################
    ## Validation study Eva Kohnert 2024  ##
    if(configName=="Kohnert_VS_2024"){ # Eva Kohnert's microbiome benchmark validation study 
      
      cfg$outfolder1 <- sub("_1Simu","",cfg$outfolder1)
      cfg$outfolder2 <- sub("_10Simu","",cfg$outfolder2)
      
      sys_info <- Sys.info()
      if(sys_info[["sysname"]]=="Darwin" && sys_info[["login"]]=="root"){
        cfg$mainFolder <- "/Users/evakohnert/Documents/PhD/Microbiome/Benchmark/BenchmarkStudy_MicrobiomeSyntheticData/R_scripts"
      }
      if(sys_info[["sysname"]]=="Windows" && sys_info[["login"]]=="srv-kreutz"){
        cfg$mainFolder <- "E:/clemens/Repositories/BenchmarkStudy_MicrobiomeSyntheticData/R_scripts"
      }
      if(sys_info[["sysname"]]=="Linux" && sys_info[["login"]]=="kohnert"){
        cfg$mainFolder <- "/h/kohnert/BenchmarkStudy_MicrobiomeSyntheticData/R_scripts"
      }
      if(sys_info[["sysname"]]=="Linux" && sys_info[["login"]]=="ckreutz"){
        if(sys_info[["nodename"]]=="imbip-compute-214")
          cfg$mainFolder <- "~/BenchmarkStudy_MicrobiomeSyntheticData_25Mar24/R_scripts"
        else
          cfg$mainFolder <- "~/BenchmarkStudy_MicrobiomeSyntheticData/R_scripts"
      }
      
      if(simulator == "metasparsim_partReg"){
        cfg$outfolder1 <- sub("/Results/","/Results_partReg/",cfg$outfolder1)
        cfg$outfolder2 <- sub("/Results/","/Results_partReg/",cfg$outfolder2)
        cfg$outfolder3 <- sub("/Results/","/Results_partReg/",cfg$outfolder3)
        cfg$outfolder4 <- sub("/Results/","/Results_partReg/",cfg$outfolder4)
      }
    }
    
    ##############################################################
    ## Using best DA method in Eva's application analysis workflow
    if(configName=="Kohnert_2024"){ # Eva Kohnert's analysis workflow
      cfg$outfolder1 <- sub("/Results/1_","/Results/4_1_",cfg$outfolder1)
      cfg$outfolder2 <- sub("/Results/2_","/Results/4_2_",cfg$outfolder2)
      cfg$outfolder3 <- sub("/Results/3_","/Results/4_3_",cfg$outfolder3)
      cfg$outfolder4 <- sub("/Results/4_","/Results/4_4_",cfg$outfolder4)
    }
    
  }
  
  return(cfg)
}

