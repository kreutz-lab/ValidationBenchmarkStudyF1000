# msb.moveFilesToModificationDayFolders
# 
# This function moves all files that match a specific pattern
# to subfolders in the targetDirectory that are created according to the modification day of the matching files.
#
# msb.moveFilesToModificationDayFolders(searchFolder,targetFolder,pattern)

msb.moveFilesToModificationDayFolders <- function(searchFolder=getwd(), targetFolder=getwd(), pattern = "^4\\.1_index.*\\.Rdata$"){

  # List all files in the searchFolder that match the pattern
  matching_files <- list.files(searchFolder, pattern = pattern, full.names = TRUE)
  
  # Get file information
  file_info <- file.info(matching_files)
  
  # Extract modification days
  mod_days <- as.Date(file_info$mtime)
  
  # Create folders for each unique modification day
  unique_days <- as.character(unique(mod_days))
  
  for (day in unique_days) {
    day_folder <- file.path(targetFolder, as.character(day))
    if (!dir.exists(day_folder)) {
      print(paste0("dir.create(",day_folder,")"))
      dir.create(day_folder)
    }
  }
  
  # Move files to the corresponding folder
  for (i in seq_along(matching_files)) {
    file <- matching_files[i]
    day <- mod_days[i]
    destination_folder <- file.path(targetFolder, as.character(day))
    if (!dir.exists(destination_folder))
      dir.create(destination_folder)
    
    file.rename(file, file.path(destination_folder, basename(file)))
    cat(paste0("Moving new result workspace: ",file, " -> ", file.path(destination_folder, basename(file))),"\n")
  }
  
}