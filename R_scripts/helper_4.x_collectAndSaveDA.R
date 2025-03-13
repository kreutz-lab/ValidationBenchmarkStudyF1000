# This script makes a preliminary result for DA analysis 
# from the workspaces saved for each data_template
# if not completely finished so far.
#
# All workspaces according to the pattern are loaded. 
# Names are assigned from another data_to_compare object
# based on number of rows and columns.
#
# Only tests that are available for any data_set are completed,
# i.e. first the struct is searched for colnames(DA$p_value)

sys_info <- Sys.info()

if(sys_info[["sysname"]]=="Darwin" && sys_info[["login"]]=="root"){
  setwd("/Users/evakohnert/Documents/PhD/Microbiome/Benchmark/BenchmarkStudy_MicrobiomeSyntheticData/R_scripts")
}
if(sys_info[["sysname"]]=="Windows" && grep("kreutz",sys_info[["login"]],T)){
  setwd("E:/clemens/Repositories/BenchmarkStudy_MicrobiomeSyntheticData/R_scripts")
}
if(sys_info[["sysname"]]=="Linux" && sys_info[["login"]]=="kohnert"){
  setwd("/h/kohnert/BenchmarkStudy_MicrobiomeSyntheticData/R_scripts")
}
if(sys_info[["sysname"]]=="Linux" && sys_info[["login"]]=="ckreutz"){
  if(sys_info[["nodename"]]=="imbip-compute-214")
    setwd("~/BenchmarkStudy_MicrobiomeSyntheticData_25Mar24/R_scripts")
  else
    setwd("~/BenchmarkStudy_MicrobiomeSyntheticData/R_scripts")
}


# Initialization, i.e. sourcing R-functions, loading packages and setting project_path
source("project_init.R")
# project_init()
###############################


#option = "4.2pr"
option = "4.2spd_nearing"
doMissings <- T


if(is.null(option))
  stop("Please specify option first (4.1, 4.2, 4.3 or 4.4 as string)")

if(option=="4.2spd_nearing"){
  pattern <- "^4\\.2_index.*\\.Rdata$"
  fileForNames <- paste0((bs_getFolder(3.2,simulator="spd"),"data_to_compare.RDS")
  targetFolder <- "../Results/4.2_metaSPARSim_ZerosAdded_DA_ancom_Nearing"
}

# ####### Option 1 #########
if(option=="4.1"){
  pattern <- "^4\\.1_index.*\\.Rdata$"
  fileForNames <- paste0(bs_getFolder(3.1,simulator="msp"),"data_to_compare.RDS")
  targetFolder <- "../Results/4.1_metaSPARSim_DA"
}
if(option=="4.1pr"){
  pattern <- "^4\\.1_partReg_index.*\\.Rdata$"
  fileForNames <- "../Results_partReg/3.1_metaSPARSim_DataProps/data_to_compare.RDS"
  targetFolder <- "../Results_partReg/4.1_metaSPARSim_DA"
}

####### Option 2 #########
if(option=="4.2"){
  pattern <- "^4\\.2_index.*\\.Rdata$"
  fileForNames <- "../Results/3.2_metaSPARSim_ZerosAdded_DataProps/data_to_compare.RDS"
  targetFolder <- "../Results/4.2_metaSPARSim_ZerosAdded_DA"
}
if(option=="4.2pr"){
  pattern <- "^4\\.2_partReg_index.*\\.Rdata$"
  fileForNames <- "../Results_partReg/3.2_metaSPARSim_ZerosAdded_DataProps/data_to_compare.RDS"
  targetFolder <- "../Results_partReg/4.2_metaSPARSim_ZerosAdded_DA"
}


####### Option 3 #########
if(option=="4.3"){
  pattern <- "^4\\.3_index.*\\.Rdata$"
  fileForNames <- "../Results/3.3_metaSPARSim_Filtered_DataProps/data_to_compare.RDS"
  targetFolder <- "../Results/4.3_metaSPARSim_filtered_DA"
}
if(option=="4.3pr"){
  pattern <- "^4\\.3_partReg_index.*\\.Rdata$"
  fileForNames <- "../Results_partReg/3.3_metaSPARSim_Filtered_DataProps/data_to_compare.RDS"
  targetFolder <- "../Results_partReg/4.3_metaSPARSim_filtered_DA"
}

####### Option 4 #########
if(option=="4.4"){
  pattern <- "^4\\.4_partReg_index.*\\.Rdata$"
  fileForNames <- "../Results_partReg/3.4_metaSPARSim_filtered_ZerosAdded_DataProps/data_to_compare.RDS"
  targetFolder <- "../Results_partReg/4.4_metaSPARSim_filtered_ZerosAdded_DA"
}
if(option=="4.4pr"){
  pattern <- "^4\\.4_index.*\\.Rdata$"
  fileForNames <- "../Results/3.4_metaSPARSim_filtered_ZerosAdded_DataProps/data_to_compare.RDS"
  targetFolder <- "../Results/4.4_metaSPARSim_filtered_ZerosAdded_DA"
}

# ####### sparseDOSSA Option 1 #########
if(option=="4.1spd"){
  pattern <- "^4\\.1(spd)?_index.*\\.Rdata$"
  fileForNames <- "../Results/3.1_sparseDOSSA_DataProps/data_to_compare.RDS"
  targetFolder <- "../Results/4.1_sparseDOSSA_DA"
}
####### sparseDOSSA Option 2 #########
if(option=="4.2spd"){
  pattern <- "^4\\.2(spd)?_index.*\\.Rdata$"
  fileForNames <- "../Results/3.2_sparseDOSSA_ZerosAdded_DataProps/data_to_compare.RDS"
  targetFolder <- "../Results/4.2_sparseDOSSA_ZerosAdded_DA"
}

####### sparseDOSSA Option 3 #########
if(option=="4.3spd"){
  pattern <- "^4\\.3(spd)?_index.*\\.Rdata$"
  fileForNames <- "../Results/3.3_sparseDOSSA_Filtered_DataProps/data_to_compare.RDS"
  targetFolder <- "../Results/4.3_sparseDOSSA_filtered_DA"
}

####### sparseDOSSA Option 4 #########
if(option=="4.4spd"){
  pattern <- "^4\\.4(spd)?_index.*\\.Rdata$"
  fileForNames <- "../Results/3.4_sparseDOSSA_filtered_ZerosAdded_DataProps/data_to_compare.RDS"
  targetFolder <- "../Results/4.4_sparseDOSSA_filtered_ZerosAdded_DA"
}


######## End options ####


#### 
data_to_compare <- readRDS(fileForNames) # data for assigning names
data_to_compare <- bs_decompress(data_to_compare)

nf <- array() # no. of features
ns <- array() # no. of samples
for(i in 1:length(data_to_compare)){
  nf[i] <- NA
  ns[i] <- NA
  if(class(data_to_compare[[i]]$original$counts)=="integer"){
    ns[i] <- length(attr(data_to_compare[[i]]$original$counts,"colnames"))
    nf[i] <- length(attr(data_to_compare[[i]]$original$counts,"rownames"))
  }
  if(class(data_to_compare[[i]]$original$counts)=="data.frame"){
    ns[i] <- ncol(data_to_compare[[i]]$original$counts)
    nf[i] <- nrow(data_to_compare[[i]]$original$counts)
  }
}
namenDF <- data.frame(name=names(data_to_compare), nf = nf, ns=ns)
# rm(data_to_compare) # not requred any more

# Move matching files in subfolders in the targetFolder:
#msb.moveFilesToModificationDayFolders(getwd(), targetFolder=targetFolder, pattern=pattern)
msb.moveFilesToModificationDayFolders(targetFolder, targetFolder=targetFolder, pattern=pattern)

cat("-------------------------------\n")
cat("All *index* files: \n")
cat(paste(list.files(targetFolder, pattern = "index\\d+.Rdata", full.names = TRUE, recursive=T), collapse="\n"))

cat("-------------------------------\n")
matching_files <- list.files(targetFolder, pattern = pattern, full.names = TRUE, recursive=T)
cat("Matching *index* files: \n")
cat(paste(matching_files,collapse = "\n"))
cat("-------------------------------\n")

d <- list()
info <- data.frame(index = array(dim=length(matching_files)),
                   files = matching_files, 
                   names = array(dim=length(matching_files)), 
                   times=array(dim=length(matching_files)))

for(i in 1:length(matching_files)){
  D <- NULL
  filename <- matching_files[i]
  info$times[i] <- file.info(filename)$mtime
  info$index[i] <- i
  
  # try to load D from working directory
  try(load(filename),silent = T) # ignore warnings, it just means that not all 38 workspaces are available
  
  if(!is.null(D)){
    if(bs_isa(D,"data_list"))
      D <- D[[1]]
      
    d[[i]] <- D
    whichOne <- which(namenDF$ns==ncol(D$original$counts) & namenDF$nf==nrow(D$original$counts))

    DAdone <- "DA" %in% names(D$original) && "p_value" %in% names(D$original$DA) && !is.null(colnames(D$original$DA$p_value)) 

    if(length(whichOne)==0){ # search same samplenames
      for(j in 1:length(data_to_compare)){
        if(length(setdiff(colnames(D$original$counts),rownames(data_to_compare[[j]]$original$meta)))==0 && DAdone)
          whichOne <- j
      }
    }
    if(length(whichOne)>0)
      info$names[i] <- namenDF$name[whichOne]
    else
      cat(paste0("\n\nD in ",filename, "cound not be found in data_to_compare\n\n"))
    print(paste0(filename, ": ", info$names[i], ", done."))
  }
}

info <- info[!is.na(info$name),]
info <- info[order(info$name, -info$time), ]
drin <- which(!duplicated(info$name))
raus <- setdiff(1:nrow(info),drin)

# Delete "raus" files (they have a newer version)
for(i in raus){
  if(!is.na(info$name[i])){
    cat(info$files[i]," might have a newer version.\n")
    # file.remove(info$files[i])
  }
}

# Now subset the latest unique ones:
info <- info[drin, ]

d <- d[info$index]
names(d) <- info$names # foreach returns a list but without names: copy them manually
attr(d, "bstype") <- "data_list"

d <- bs_annotateObject(d)

missing <- setdiff(names(data_to_compare),names(d))
if(length(missing)>0)
  print(paste0("Not found: ",missing))


drin <- c()
for(i in 1:length(d)){
  if(!is.null(d[[i]]))
    drin <- c(drin,i)
}
d <- bs_subset(d,drin)

allTests <- c()
for(i in 1:length(d)){
  for(j in 1:length(d[[i]])){
    if(bs_isa(d[[i]][[j]],c("data_template","sim_result"))){
      if(!is.null(d[[i]][[j]]$DA) && !is.null(d[[i]][[j]]$DA$p_value)){
        allTests <- unique(c(allTests,colnames(d[[i]][[j]]$DA$p_value)))
      }
    }
  }
}
cat("\nallTests=",allTests,"\n\n")

saveRDS(bs_compress(d),paste0(targetFolder,"/data_to_compare.RDS"))

df_success <- bs_checkDA(d)
failed <- df_success[!df_success$success,]
print(failed)

# Try to make missing DAs:
if(exists("doMissings") && doMissings && length(missing)>0 && length(allTests)>0){
  save.image(file=paste0(option,"_beforeDoMissings.Rdata"))
  
  cat("doMissings = TRUE: try to ")
  d_missing <- bs_subset(data_to_compare,which(names(data_to_compare) %in% missing))
  names2 <- names(d_missing)
  for(i in 1:length(d_missing)){
    cat(paste0("\nTry to accomplish DA for ",names2[i],"\n\n"))
    d_tmp <- bs_subset(d_missing,i)
    d_tmp <- bs_annotateObject(d_tmp)
    #D <- bs_DA(d_tmp, parallelMode = T, doSlowMethods = T, maxRunTime = 60)
    #D <- bs_DA_parallel(d_tmp, parallelMode = T, doSlowMethods = T, maxRunTime = 60)

    # Call tests one by one: 
    D <- d_tmp
    tests <- allTests 
    if(names2[i]=="GWMC_HOT_COLD" && option=="4.2"){
      warning("Aldex2 is not run for 4.2 and GWMC_HOT_COLD because this presumably crashes the process !!!")
      tests <- setdiff(tests,"Aldex2") 
    } # NO ALDEX! => computer crashes, ALDEX requires a lot of memory
    if(names2[i]=="Office" && option=="4.2"){ # corncob does not suceed (maybe due to 60 min limit)
      tests <- setdiff(tests,"corncob") 
    }
    
    ## Use parallel mode for all, but not for lefse and corncob (because of memory overflow)
    for(test in tests){
      cat(test," in helper_4.x_collectAndSaveDA.R i=",i,", ",names2[i]," started ...",as.character(Sys.time()),"\n")
      if(test!="lefse" && test!="corncob"){
        D <- bs_DA_parallel(D, parallelMode = T, doSlowMethods = T, maxRunTime = 60, whichMethods=c(test))
      }else{ # Lefse requires a lot of memory, corncob uses all processors anyway: call them non-parallel
        D <- bs_DA(D, parallelMode = F, doSlowMethods = T, maxRunTime = 60, whichMethods=c(test))
      }
      saveRDS(D, file = paste0(as.character(Sys.Date()),"_",gsub(":","-",as.character(format(Sys.time(), "%X"))),"_",test,"_helper_4.x_collectAndSave.RDS"))
    }
    
    #try(save(list="D",file=paste0(option,"_index",100+i,".Rdata"))) # I label this workspaces with 100+
    try(save(list="D",file=paste0(option,"_index",300+i,".Rdata"))) # I label this workspaces with 100+
  }
  
  # Now call this script again with new "_index*.Rdata" workspaces
  doMissings <- F
  source("helper_4.x_collectAndSaveDA.R")
  doMissings <- T # reset to old value
} else { # make DF

  DF <- bs_DA2dataFrame(d)
  saveRDS(DF,paste0(targetFolder,"/DF.RDS"))
  
}



