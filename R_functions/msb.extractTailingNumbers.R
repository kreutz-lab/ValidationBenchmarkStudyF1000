#' msb.ExtractTailingNumbers
#'
#' This function finds numbers at the end of strings and returns them as numeric values 
#'
#' Might be used for incremental labels or filenames, e.g. simulation_1, simulation_2, ...
#'
#' @param stringList strings where tailing numbers are searched
#' @param pattern pattern used to subset stringList
#'
#' @return numbers that are found at the end of the strings (as.numeric)
#' 
#' @examples
# numbers <- msb.ExtractTailingNumbers(c("asdfa2","asdfdafasdf5"))
#
# numbers <- msb.ExtractTailingNumbers(names(aList))
# aList[[paste0("Entry_",max(numbers,0)+1)]] <- "aNewElement"

msb.ExtractTailingNumbers <- function(stringList, pattern=NULL, replaceTailingNums = FALSE){
  
  # subsetting, if pattern provided:
  if(!is.null(pattern))
    stringList <- grep(pattern,stringList,value=T)
  
  if(replaceTailingNums){
    for(i in 1:length(stringList))
      stringList[i] <- sub("[0-9]+$","",stringList[i])
    return(stringList)
  }
  else{
    m  <-regexpr("[0-9]+$",stringList)
    numbers <- as.numeric(regmatches(stringList,m))
    return(numbers)
  }
  
}
