#' msb.gitVersion
#'
#' This function evaluates the current git programming version, i.e. the sha of the last commit
#'
#' All shas can be seen e.g. via git log
#'
#' If this sha is stored, you can return to the programming version via
#' git checkout sha
#'
#' @param
#'
#' @return
#'
#' @examples
#' msb.gitVersion()
#' msb.gitVersion.value  # faster
#'
#' @keywords git
#' @export

msb.gitVersion <- function(silent=FALSE){
  # Check if in a git folder:

  if(system("git status",show.output.on.console=F)==0)
    sha <- system("git rev-parse HEAD",intern=T)
  # else if(!is.null(msb.gitVersion.value))
  #   sha <- msb.gitVersion.value
  else{
    if(!silent)
      print("Not in git folder, cannot store commit sha.")
    sha = NA
  }

  return(sha)
}

# I also call this function to have the variable available
# This allows to evalute the git version, if the working directory is switched
# (after sourcing) to a non-git folder
# Evaluating this variable is also faster than calling git rev-parse HEAD multiple times

#msb.gitVersion.value <- msb.gitVersion()

