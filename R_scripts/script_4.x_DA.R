# This script is called for 3.x.x analyses and requires three variables:
#
# dataFolder  for loading properties
# resultFolder for results

if(!dir.exists(dataFolder)){
  warning(dataFolder, " does not exist !!!!!!!!!!!!")
  
}else{
  
  
  
  print("script_4.x_DA.R started ...")
  print(paste0("resultFolder: ", resultFolder))
  print(paste0("dataFolder: ", dataFolder))
  print(paste0("prefix: ", prefix))
  
  
  try(dir.create(resultFolder))
  
  # Get input data for metaSPARSim
  data_to_compare <- readRDS(paste0(dataFolder,"/data_to_compare.RDS"))
  data_to_compare <- bs_decompress(data_to_compare)
  data_to_compare <- bs_annotateObject(data_to_compare)
  data_to_compare <- bs_checkData(data_to_compare,eliminateZeroSimus = T)
  
  #data_to_compare <- bs_subset(data_to_compare,!names(data_to_compare)=="ArcticFreshwaters")
  #data_to_compare <- bs_subset(data_to_compare,c(9,14)-1) # select two small ones
  
  # sort according to size
  ndat <- array()
  for(i in 1:length(data_to_compare)){
    data_to_compare[[i]]$path <- getwd()
    data_to_compare[[i]]$index <- i
    ndat[i] <- prod(dim(data_to_compare[[i]]$original$counts))
  }
  
  # change the order in order to start with the smallest ones:
  iorder <- order(ndat)
  bstype <- attr(data_to_compare,"bstype")
  data_to_compare <- data_to_compare[iorder]
  attr(data_to_compare,"bstype") <- bstype
  
  
  # parallel loop over all datasets
  msb.registerDoParallel(min(10,min(parallel::detectCores()-1,length(data_to_compare))))
  # registerDoParallel(cores = min(parallel::detectCores(),length(data_to_compare)))
  
  namen <- names(data_to_compare)
  for(index in 1:length(data_to_compare)){
    data_to_compare[[index]]$index <- index 
    data_to_compare[[index]]$pfad <- getwd()
    data_to_compare[[index]]$prefix <- prefix
  }
  
  data_to_compare <- bs_annotateObject(data_to_compare)
  
  #for(i in 1:length(data_to_compare)){
  data_to_compare <- foreach(D=data_to_compare) %dopar% {
    
    tryCatch({
      setwd(D$pfad)
      prefix <- D$prefix
      D$prefix <- NULL 
      
      cat("DA for data_project ",D$index,"\n",file=paste0(prefix,"foreach.log"),append = T)
      
      source("project_init.R")
      # project_init()
      
      D <- bs_decompress(D)
      
      doDA = F
      for(ii in 1:length(D))
        doDA = doDA || (!"DA" %in% names(D[[ii]])) # DA everywhere available?
      
      if(doDA){  # DA analysis, time consuming
        #      D <- bs_DA(D,whichMethods = "edgeR",maxFeatures = 1000, parallelMode = T)
        D <- bs_DA(D, parallelMode = F, doSlowMethods = T, maxRunTime = 60)
        #D <- bs_DA_linux(D,whichMethods = "glmmTMB",maxFeatures = 5000) # linux, without microbiomeMarker package
        
      }
      
      # if(!doDA){ # load d*.RDS from folder "../Results/2023-11-14-Done" because bs_DA were done in parts due to bugs
      #   
      #   fils <- list.files("../Results/2023-11-14-Done",pattern="d*RDS",full.names = T)
      #   namen <- names(D)
      #   for(i in 1:length(fils)){
      #     print(fils[i])
      #     id <- as.numeric(sub("../Results/2023-11-14-Done/d","",strsplit(fils[i],"_")[[1]][1]))
      #     if(length(id)!=1)
      #       stop("id could not be determined")
      #     
      #     name <- sub(".RDS","",strsplit(fils[i],"[0-9]_")[[1]][2])
      #     ii <- grep(name,namen)
      #     if(length(ii)!=1)
      #       stop("Assignment does not work.")
      #     
      #     tmp <- readRDS(fils[i])
      #     D[[ii]][[id]] <- tmp[[1]]
      #   }
      # }
      
      #saveRDS(D,"../Results/2023_11_14-adding_glmmTMB_d.RDS")
      try(save(list="D",file=paste0(prefix,"index",D$index,".Rdata")))
      
      cat("DA for data_project ",D$index,"finished \n",file=paste0(prefix,"foreach.log"),append = T)
      
    }, error=function(e){
      try(save(list="D",file=paste0("Error_",prefix,"index",D$index,".Rdata")))
      cat("Error ",prefix," (index=",D$index,"): ",conditionMessage(e),"\n",file=paste0(prefix,"foreach.log"),append = T)
    })
    return(D)
  }
  
  # doParallel::stopImplicitCluster()
  msb.unregisterDoParallel()    
  for(i in 1:length(data_to_compare)){
    data_to_compare[[i]]$index <- NULL 
    data_to_compare[[i]]$pfad <- NULL
  }
  names(data_to_compare) <- namen # foreach returns a list but without names: copy them manually
  attr(data_to_compare, "bstype") <- "data_list"
  
  saveRDS(bs_compress(data_to_compare),file=paste0(resultFolder,"/data_to_compare.RDS"))
  
  status <- bs_checkDA(data_to_compare)
  sink(paste0(resultFolder,"/DA_status.log"))
  print(status)
  sink()
  
  # create and save DF with test results
  DF <- bs_DA2dataFrame(data_to_compare)
  saveRDS(DF,file=paste0(resultFolder,"/DF.RDS"))
  
  # # Move all __bs_DA.RDS files:
  # files <- list.files(path = ".", pattern = "__bs_DA\\.RDS$", full.names = TRUE)
  # destination_paths <- file.path(resultFolder, basename(files))
  # try(file.rename(from = files, to = destination_paths))
  
  
  ## data.frames:
  for(prop in c("logFold","p_value","p_adjusted")){
    DF <- NULL
    for(i in 1:length(data_to_compare)){
      for(j in 1:length(data_to_compare[[i]])){
        if(bs_isa(data_to_compare[[i]][[j]],"data_template") || bs_isa(data_to_compare[[i]][[j]],"sim_result")){
          dataset <- names(data_to_compare[[i]])[j]
          
          DA <- as.data.frame(data_to_compare[[i]][[j]]$DA[[prop]])
          row.names(DA) <- row.names(data_to_compare[[i]][[j]]$counts)
          DA <- data.frame(template = rep(namen[i],length.out=nrow(DA)),
                           dataset=rep(dataset,length.out=nrow(DA)),DA)
          if(is.null(DF)){
            DF <- DA
          }else{
            all_columns <- union(names(DF), names(DA))
            for (col in setdiff(all_columns, names(DF))) {
              DF[[col]] <- NA
            }
            for (col in setdiff(all_columns, names(DA))) {
              DA[[col]] <- NA
            }
            DF <- rbind(DF,DA)
          }
        }
        
      }
    }
    saveRDS(DF,file=paste0(resultFolder,"/DF_",prop,".RDS"))
  }
  
  
  print("script_4.x_DA.R finished :-) ")
  
}
