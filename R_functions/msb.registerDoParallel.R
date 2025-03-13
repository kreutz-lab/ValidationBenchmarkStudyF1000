#' msb.registerDoParallel
#'
#' This function calls registerDoParallel if it has not yet done before.
#'
#' This function can be called multiple times without performance loss (unlike foreach::registerDoParallel).
#' The check is done by calling foreach::getDoParRegistered()
#'
#' @param ncores
#'
#' @examples
#'
#' msb.registerDoParallel()
#' msb.registerDoParallel(ncores=4)
#'
#' @seealso \code{\link{foreach::registerDoParallel}} \code{\link{msb.closeAll}}
#' @keywords parallelization
#' @export

msb.registerDoParallel <- function(ncores=parallel::detectCores()){
  if(exists("doParallelization", mode = "logical"))
    if(!doParallelization){ # do nothing, this is a possibility to stop parallelization from outside, e.g. for debugging purpose
      print("doParallelization is switched off.")
      foreach::registerDoSEQ()
      return()
    }

  if(!foreach::getDoParRegistered() || foreach::getDoParWorkers()!=ncores){
    print(paste0("registerDoParallel with ",ncores," cores."))
    doParallel::registerDoParallel(cores = ncores)
  }
}
