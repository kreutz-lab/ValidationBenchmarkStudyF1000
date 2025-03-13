# List all subdirectories
folders <- list.dirs("../Results", full.names = TRUE, recursive = FALSE)

# Filter folders matching the pattern
folders <- folders[grepl("^3\\.", basename(folders))]

for(folder in folders){
  file <- paste0(folder,"/dataPropCmp.Rdata")
  cat(file,"...\n")
  load(file)
  
  DF[,"comparison"] <- "template_vs_template"
  for(pattern in c("metaSPARSim","sparseD")){
    try(DF[grepl(pattern,DF$dataSet1) & !grepl(pattern,DF$dataSet2),"comparison"] <- "template - sim")
    try(DF[grepl(pattern,DF$dataSet2) & !grepl(pattern,DF$dataSet1),"comparison"] <- "template - sim")
    try(DF[grepl(pattern,DF$dataSet1) & grepl(pattern,DF$dataSet2),"comparison"] <- "sim_vs_sim")
  }
  
  save(DF,resultFolder,file=file)
}

