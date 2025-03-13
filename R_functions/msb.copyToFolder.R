# This function first checks existance of the target folder, creates it if necessary
# and then copies the file to that folder
#
# It is thought as an abbrevation to keep other code short

msb.copyToFolder <- function(file,targetFolder,newFileName=NULL,...){
  if(!dir.exists(targetFolder))
    dir.create(targetFolder)
  
  if(!is.null(newFileName))
    targetName <- paste0(targetFolder,"/",newFileName)
  else
    targetName <- targetFolder
  
  file.copy(file,targetName,...) 
  
}