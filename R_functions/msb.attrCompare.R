# This function checks, whether the attribute is available and if so, it checks whether
# the attribute has a specific value
# 
# dummy <- list(a=10,b=12)
# attr(dummy,"anAttributeName") <- "attributeValue"
# attr(dummy,"anAttributeName") 
# attr(dummy,"nonExistingAttribute")=="something"  && TRUE # this does not work in if()
# msb.attrCompare(dummy,"nonExistingAttribute","anOtherValue")
# msb.attrCompare(dummy,"anAttributeName","anOtherValue")
# msb.attrCompare(dummy,"anAttributeName","attributeValue")

msb.attrCompare <- function(obj,attrib,value)
{
  if(!is.null(attr(obj,attrib)))
    out = attr(obj,attrib)==value
  else
    out = FALSE
  return(out)
}

