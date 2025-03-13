# This function calls append and ensures that the result has all attributes. 
#
# If further attributes should be preserved, please extend this function.
# The sticky package should do that but it cannot append lists.
#
# input   the list, vector or array where the subset should be taken (maybe also work with other objects)
#
# Subset  which subset should be selected (logical, numeric, function accepted, see examples)
#         If Subset is a function sapply(input,Subset) is used to evaluate the function to all elements.
#         The function should return a logical vector of the same length as input (see examples)
#
#
# Examples:
# list1 <- list(a=1,b=2,c=3)
# attr(list1,"bstype") <- "test"
# attr(list1,"user") <- "CK"
# attr(list2,"testAttribute") <- "testvalue"
#
# list2 <- msb.subset(list1,c(TRUE,TRUE,FALSE))
# list2
# msb.subset(as.array(list1),c(TRUE,TRUE,FALSE)) # also works for arrays
#
# list2 <- msb.subset(list1,c(TRUE,TRUE,FALSE))
# list2
# list3 <- msb.subset(list1,list1<3)
# list3
# list4 <- msb.subset(list1,-2)
# list4
# list5 <- msb.subset(list1,function(x){x>1})

msb.subset <- function(input,Subset,...)
{
  preserved <- c("names") # these attributes are copied automatically
  
  a1 <- attributes(input)
  toCopy1 <- setdiff(names(a1),preserved)
  
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

  out <- subset(input,Subset,...)
  
  for(att in toCopy1){
    value <- attr(input,att)
    attr(out,att) <- value
  }
  
  return(out)
}
