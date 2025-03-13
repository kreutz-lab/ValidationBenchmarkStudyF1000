# Benchmark Study specific function to calculate data properties with 
# msb.bs_calcPropMatrix (from MSB TOOLBOX!!!)
# for each data set in BS-object (data_to_compare)
#
#
#
# # Example:
# d <- readRDS("data_to_compare.RDS")
# d <- bs_addDataProp(d)

bs_addDataProp <- function(counts.list, file=NULL){
  
  madeList <- F
  if(bs_isa(counts.list,"data_project")){
    counts.list <- list(counts.list) # make a list of data_projects
    madeList = T
  }
  
  zuOrdnung <- list()
  cAsList <- list()
  for(i in 1:length(counts.list)){
    print(paste0("Add data prop to dataset i=",i))
    for(j in 1:length(counts.list[[i]])){
      print(paste("Add data prop to orig or sim j= ",j, bs_isa(counts.list[[i]],"data_project"), bs_isa(counts.list[[i]][[j]],c("data_template","sim_result"))))
      if(bs_isa(counts.list[[i]],"data_project") && bs_isa(counts.list[[i]][[j]],c("data_template","sim_result"))){
        index <- length(cAsList)+1
        zuOrdnung[[index]] <- list(i=i, j=j)
        cAsList[[index]] <- counts.list[[i]][[j]]
        cAsList[[index]]$message <- paste0("msb.bs_calcPropMatrix for", names(counts.list[i]),names(counts.list[[i]][j]))
        cAsList[[index]]$path <- getwd()
      }
    }
  }
  
  if(length(cAsList)==0)
    warning("Error-warning bs_addDataProp: length(cAsList)=0: Something is wrong, maybe the attributes data_project, sim_result, etc. are missing.")
  
  doParallel::registerDoParallel(min(20,min(parallel::detectCores(),length(cAsList))))
  
  oldNames <- names(cAsList)
  #for(i in 1:length(cAsList)){
  # D <- cAsList[[i]]
  cAsList <- foreach(D = cAsList) %dopar% {
    #cAsList <- for(D = cAsList, .packages = c('edgeR','BimodalIndex','amap','cluster')) %dopar% {
    setwd(D$path)
    source("project_init.R")
    tryCatch({ 
      #try(saveRDS(D,file = "lastLoop_1.RDS"))
      # print(paste0("msb.bs_calcPropMatrix for", names(counts.list[i]),names(counts.list[[i]][j])))
      #  print("One parallel job started")
      systime <- as.character(format(Sys.time(), "%X  %b %d %Y"))
      catfile <- paste0("bs_addDataProp_foreach",systime,".log")
      
      cat("One parallel job started ...",systime,"\n",file="bs_addDataProp_foreach.log",append = T)
      cat(D$message,"\n",file=catfile,append = T)
      
      if(is.character(D$counts) || !is.matrix(D$counts)){
        D <- bs_decompress(D)
      }
      
      Prop.tmp <- msb.bs_calcPropMatrix(D$counts, meta.dat=D$meta, file = catfile)
      D$data.Prop <- Prop.tmp
      D$data.Prop.info <- msb.info()
      attr(D$data.Prop.info,"bstype") <- "msb.info"
      #try(saveRDS(D,file = "lastLoop_2.RDS"))
      
      return(D)
      #cAsList[[i]] <- D
      
    }, error=function(e){
      try(save(D,i,file="Error_in_bs_addDataProp_foreachLoop.Rdata"))
      cat("Error :",conditionMessage(e),"\n",file="Error_in_bs_addDataProp_foreachLoop.log",append = T)
      return(D)
    })
  }
  
  doParallel::stopImplicitCluster()
  names(cAsList) <- oldNames # foreach returns a list but without names: copy them manually
  
  for(i in 1:length(cAsList)){
    cAsList[[i]]$message <- NULL # delete to not get problems somewhere else in the code
    cAsList[[i]]$path <- NULL  # delete to not get problems somewhere else in the code
    
    counts.list[[zuOrdnung[[i]]$i]][[zuOrdnung[[i]]$j]] <- cAsList[[i]]
    # attr(counts.list[[zuOrdnung[[i]]$i]][[zuOrdnung[[i]]$j]],"bstype") <- attr(cAsList[[i]],"bstype")
  }
  
  
  # for(i in 1:length(counts.list)){
  #   for(j in 1:length(counts.list[[i]])){
  #
  #     print(paste0("msb.bs_calcPropMatrix for", names(counts.list[i]),names(counts.list[[i]][j])))
  #
  #     if(is.character(counts.list[[i]][[j]]$counts)){
  #       counts.list[[i]][j] <- bs_decompress(counts.list[[i]][j])
  #     }
  #
  #     Prop.tmp <- msb.bs_calcPropMatrix(counts.list[[i]][[j]]$counts)
  #     counts.list[[i]][[j]]$data.Prop <- Prop.tmp
  #     counts.list[[i]][[j]]$data.Prop.info <- msb.info()
  #   }
  #
  # }
  if(!is.null(file)){
    counts.list$save.info <- msb.info(withGitHash = TRUE)
    saveRDS(counts.list, paste0(file,"/data_to_compare.RDS"))
  }
  if(madeList)
    counts.list <- counts.list[[1]] # reverse it
  return(counts.list)
}
