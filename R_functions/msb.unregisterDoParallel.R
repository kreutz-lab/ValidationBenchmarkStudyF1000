#' msb.unregisterDoParallel
#'
#' This function calls is the inverse of msb.registerDoParallel if it has not yet done before.
#' It enforces closing all parallel pools.
#'
#'
#' @examples
#'
#' msb.registerDoParallel()
#' msb.unregisterDoParallel()
#'
#' @seealso \code{\link{foreach::registerDoParallel}} \code{\link{msb.closeAll}}
#' @keywords parallelization
#' @export

msb.unregisterDoParallel <- function(){
  try(doParallel::stopImplicitCluster())
  
  unregister_dopar <- function() {
    env <- foreach:::.foreachGlobals
    rm(list=ls(name=env), pos=env)
  }
  
  try(
    unregister_dopar()
  )
}
