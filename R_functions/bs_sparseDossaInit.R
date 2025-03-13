#' bs_sparseDossaInit
#'
#' sourcing sparseDossa files 
#'
#' Installation as package did not work so far.
#' You have to specify the folder where the github download R files are.
#'
#' @param sparseDossaRpath
#'
#' @return
#' 
#' @examples
#' bs_sparseDossaInit() # uses default sparsseDossaRpath
#' 
#' sparseDossaRpath = "~kohnert/SparseDOSSA2/R/"
#' bs_sparseDossaInit(sparseDossaRpath)
#'
#' @seealso \code{\link{anotherFunction}}
#' @keywords aKeywordSuchAsBenchmarking
#' @export

bs_sparseDossaInit <- function(sparseDossaRpath=paste0("~/SparseDOSSA2/R/")){
  
  # required packages (dependencies of sparseDossa2)
  suppressPackageStartupMessages(library(future.apply, quietly = T, warn.conflicts = F)) 
  suppressPackageStartupMessages(library(huge, quietly = T, warn.conflicts = F)) 
  suppressPackageStartupMessages(library(igraph, quietly = T, warn.conflicts = F)) 
  suppressPackageStartupMessages(library(ks, quietly = T, warn.conflicts = F)) 
  suppressPackageStartupMessages(library(magrittr, quietly = T, warn.conflicts = F)) 
  suppressPackageStartupMessages(library(mvtnorm, quietly = T, warn.conflicts = F)) 
  suppressPackageStartupMessages(library(Rmpfr, quietly = T, warn.conflicts = F)) 
  suppressPackageStartupMessages(library(truncnorm, quietly = T, warn.conflicts = F)) 
  
  files = list.files(sparseDossaRpath,pattern="*.R",full.names = TRUE)
  sapply(files,source)
}
