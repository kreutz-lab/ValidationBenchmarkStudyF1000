#' bs_countsInGroups
#'
#' Function for splitting a count matrix according to condition (i.e. group) information
#'
#' Required e.g. for estimating simulation parameter group-wise
#'
#' @param 
#' @param 
#'
#' @return
#' 
#' @examples
#' d <- readRDS(paste0(bs_path,"Analyses/Data/example_data/bs_object2.RDS"))
#' countsInGroups <- bs_countsInGroups(d[[1]]$original$counts,d[[1]]$original$meta$condition)
#'
#' 
#'
#' @seealso \code{\link{anotherFunction}}
#' @keywords aKeywordSuchAsBenchmarking
#' @export

bs_countsInGroups <- function(counts,condition){
  conds <- as.factor(condition)
  condLevs <- levels(conds)
  countsInGroups <- list()
  for(i in 1:length(condLevs)){
    inCond <- which(conds==condLevs[i])
    countsInGroups[[i]] <- counts[,inCond]
  }
  names(countsInGroups) <- as.character(condLevs)
  
  return(countsInGroups)
}
