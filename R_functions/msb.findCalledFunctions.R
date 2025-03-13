#' msb.findCalledFunctions
#'
#' This function parses code and finds all called funtion, 
#' and checks whether they are available in the namespace.
#' 
#' Comparison with nameSpace can be switched of to show all functions used in the code.
#' 
#' Patterns can be specified, i.e. to only print msb. functions.
#'
#' details
#'
#' @param 
#' @param 
#'
#' @return
#' 
#' @examples
#' 
#' # checking a function
#' msb.findCalledFunctions(msb.loadMsbData)
#' 
#' # checking a script
#' rScriptFile <- "mySimulation.R" # has to exist as a file
#' msb.findCalledFunctions(rRscriptFile)
#' 
#' # checking whether all msb.functions in that script are available
#' msb.findCalledFunctions("2023_05_09-Simulate_sparseDossa.R", pattern = "^msb\\.")
#' 
#' # Just print all functions that are called:
#' msb.findCalledFunctions(msb.loadMsbData, notInNamespace=F)
#' 
#' # Just print all msb. functions that are used, e.g. to copy locally:
#' msb.findCalledFunctions("2023_05_09-Simulate_sparseDossa.R", notInNamespace=F, pattern = "^msb\\.")
#' 
#'
#' @seealso \code{\link{anotherFunction}}
#' @keywords aKeywordSuchAsBenchmarking
#' @export

msb.findCalledFunctions <- function(fun, notInNamespace=T, pattern = NULL){
  # Extract the function body and convert it to a character string
  if(class(fun)=="function")
    function_body <- deparse(body(fun))
  if(class(fun)=="character")
    function_body <- readLines(paste0(fun))
  
  
  # elminate comments:
  function_body <- function_body[!grepl("^#", function_body)]
  function_body <- sub("#.*", "", function_body)
  
  # Basic regex to match function calls (this is a simplified example)
  # This regex looks for sequences of characters that might be function calls.
  # Note: This is a very basic pattern and might need adjustments.
  regex_pattern <- "\\b[a-zA-Z._]+\\("
  
  # Find matches
  matches <- gregexpr(regex_pattern, function_body)
  
  # Extract matches
  matched_functions <- unlist(regmatches(function_body, matches))
  
  # Clean up to just get unique function names without parentheses
  function_names <- unique(gsub("\\s*\\($", "", matched_functions))
  
  
  # remove all functions in the namespace:
  if(notInNamespace){
    all_objects <- unlist(lapply(search(), function(env) {
      #if (grepl("^package:", env)) {
      package_name <- sub("^package:", "", env)
      return(ls(name = env))
      #}
    }))
    function_names <- function_names[!function_names %in% all_objects]
  }
  
  # only those that matches the pattern:
  if(!is.null(pattern))
    function_names <- function_names[grep(pattern, function_names)]
  
  # make a message if compared with the nameSpace:
  if(notInNamespace){ 
    if(length(function_names)==0){
      if(is.null(pattern))
        cat("No unknown functions.\n")
      else
        cat("No unknown functions that matches the pattern.\n")
    }
    else
      warning("The following functions are missing: \n",paste(function_names,sep=", ",collapse=", "), "\n >> Either load packages or source code!")
  }
  
  return(function_names)
  
}
