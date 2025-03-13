
# Define the directory to search
directory <- getwd()

# Define the specific timepoint
timepoint <- as.POSIXct("2023-05-01 12:00:00")  # Replace with your specific timepoint

# List all files in the directory
files <- list.files(directory, full.names = TRUE)

# Get file information
file_info <- file.info(files)

# Filter files based on the modification time
modified_files <- rownames(file_info[file_info$mtime > timepoint, ])

# Print the list of files modified after the specified timepoint
print(modified_files)
