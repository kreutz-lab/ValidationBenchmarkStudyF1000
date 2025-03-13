#' msb.info
#'
#' Computer name, user name etc which might help for documentation and for user-specific configs and paths
#'
#' Also MSB-specific intial letters are provided, e.g. if "kreutz" is part of the
#' user name, then msb.info()[["initial"]] = "CK"
#'
#' @param
#'
#' @return
#'
#' @examples
#' msb.info()
#'
#' # user name might serve as foldername:
#' foldername <- msb.info()$user
#'
#' # date might be added to filename:
#' filename <- paste0(msb.info()[["date"]],"_","aFilename")
#'
#'
#' @seealso \code{\link{anotherFunction}}
#' @keywords aKeywordSuchAsBenchmarking
#' @export

msb.info <- function(withGitHash=FALSE){

  out <- Sys.info()
  out[["initial"]] <- msb.user2initial(out[["user"]])

  out[["date"]] <- as.character(Sys.Date())
  out[["time"]] <- gsub(":","-",as.character(format(Sys.time(), "%X")))
  out[["datetime"]] <- paste0(out[["date"]],"_",out[["time"]])

  if(withGitHash)
    out[["git_version"]] <- msb.gitVersion(silent=TRUE)

  out <- out[!names(out) %in% c("release","version","machine","effective_user")]

  return(out)
}

# MSB specific conversion of user names to initial letters
# Can be extended here in the code
msb.user2initial <- function(user){
  if(length(grep("kreutz",tolower(user)))==1)
    initial = "CK"
  else if(length(grep("kohnert",tolower(user)))==1)
    initial = "EK"
  else if(length(grep("kohnert",tolower(user)))==1)
    initial = "EK"
  else if(length(grep("brombach",tolower(user)))==1)
    initial = "EB"
  else if(length(grep("meyring",tolower(user)))==1)
    initial = "CM"
  else if(length(grep("menger",tolower(user)))==1)
    initial = "JM"
  else if(length(grep("litwin",tolower(user)))==1)
    initial = "TL"
  else
    initial=""

  return(initial)
}
