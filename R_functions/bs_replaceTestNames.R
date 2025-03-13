# Replacing test names to have a standardizes form
# Add further replacements if required.

bs_replaceTestNames <- function(namen){
  
  namen <- sub("ancombc","ANCOM-II",namen)
  namen <- sub("lefse","LEfSe",namen)
  namen <- sub(".test","",namen,T)
  namen <- sub("EdgeR","edgeR",namen)
  namen <- sub("maaslin","MaAsLin",ignore.case = T)
  
  return(namen)
}