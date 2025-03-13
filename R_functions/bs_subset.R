# input   the list, vector or array where the subset should be taken (maybe also work with other objects)
#
# Subset  which subset should be selected (logical, numeric, function accepted, see examples)
#         If Subset is a function sapply(input,Subset) is used to evaluate the function to all elements.
#         The function should return a logical vector of the same length as input (see examples)
#
# This function calls subset and copies the following attributes to the result:
#   "bstype"
#
# If further attributes should be preserved, please extend this function.
# The sticky package should do that but it cannot append lists.
#
# Example:
# list1 <- list(a=1,b=2,c=3)
# attr(list1,"bstype") <- "test"
# list2 <- subset(list1,c(TRUE,TRUE,FALSE))
# list2
# subset(as.array(list1),c(TRUE,TRUE,FALSE)) # also works for arrays
#
# list2 <- subset(list1,c(TRUE,TRUE,FALSE))
# list2
# list3 <- bs_subset(list1,list1<3)
# list3
# list4 <- bs_subset(list1,-2)
# list4
# list5 <- bs_subset(list1,function(x){x>1})
#
#
# data_to_compare with n different data sets, for each one original and m simulations
# data_to_compare <- bs_subset(data_to_compare, 4)   # Subset for the 4th data set. Now data_to_compare is a list of length 1, and has one original and m simulations
# data_to_compare$art_scher <- bs_subset(data_to_compare$art_scher, c(1:2))  # Now keep for art_scher only the original and the first simulation

bs_subset <- function(input,Subset,...)
{
  preserved <- c("names","bstype") # these attributes are implemented to be copied, names is copied automatically
  
  if(is.logical(Subset)){ 
    # do nothing
  }else if(is.numeric(Subset)){ # convert to logical because generic subset function only allows logicals
    if(sum(Subset<0)==length(Subset)){ # negative indices
      tmp <- rep(TRUE,length=length(input))
      tmp[-Subset] <- FALSE
      Subset <- tmp
    }else{
      tmp <- logical(length=length(input))
      tmp[Subset] <- TRUE
      Subset <- tmp
    }
  }else if(is.function(Subset)){
    Subset <- sapply(input,Subset)
  }else
    stop("Subset has to be logical, numeric, or function")

  a1 <- attributes(input)
  
  out <- subset(input,Subset,...)
  
  if(sum(names(a1)=="bstype")>0){ # input has attribute bstype
    bstype1 <- attr(input,"bstype")
    attr(out,"bstype") <- bstype1      
  }
  
  return(out)
}
