#' msb.sampleDiffRegFromPvalues
#'
#' This function draws the right proportion of indeed differentially regulated features
#' from a vector of p-values. 
#'
#' The Proportion of differentially regulated are estimated by qvalue::qvalue(pvalues)$pi0
#'
#' @param pvalues 
#' @param exactProportion if true then, the number of regulated is chosen as close as possible to the 
#' estimated proportion of regulated 
#'
#' @return a logical vector
#' 
#' @examples
#' pvalues <- runif(1000)^2
#' isdiff <- msb.sampleDiffRegFromPvalues(pvalues)
#'
#' png("Example.png")
#' hist(pvalues,col="gray")
#' hist(pvalues[isdiff],add=T,col="blue")
#' legend("topright", legend = c("all p-values", "p-values with fold!=0"), fill = c("gray", "blue"))
#' dev.off()
#' 
#' 
#' # Nicer plots:
# pvalues <- runif(1000)^2
# isdiff <- msb.sampleDiffRegFromPvalues(pvalues)
# df <- data.frame(pvalues, group = factor(isdiff,levels=c(TRUE,FALSE)))
# breaks <- seq(0, 1, length.out = 10)
# out <- ggplot(df, aes(x = pvalues, fill = group)) +
#   geom_histogram(position = 'stack', breaks = breaks) +
#   scale_fill_manual(values = c("FALSE" = "black","TRUE" = "red"),
#                     labels = c("FALSE" = "logFC = 0","TRUE" = "logFC != 0")) +
#   labs(fill = "P-Value Group",
#        x = "p-value",
#        y = "Frequency",
#        title = "Histogram of p-Values") +
#   scale_x_continuous(breaks = seq(0, 1, length.out = 5), limits = c(0, 1)) +
#   theme_minimal() +
#   theme(
#     text = element_text(size = 16), # For overall text size, affects all text elements
#     axis.title = element_text(size = 18), # For axis titles
#     axis.text = element_text(size = 14), # For axis text
#     legend.title = element_text(size = 16), # For legend title
#     legend.text = element_text(size = 14), # For legend text items
#     plot.title = element_text(size = 20, face = "bold")) # For the plot title
# pdf(file="DrawingLFC0.pdf",height=5)
# print(out)
# dev.off()
# 
# out <- ggplot(df, aes(x = pvalues)) +
#   geom_histogram(position = 'identity', breaks = breaks) +
#   labs(x = "p-value",
#        y = "Frequency",
#        title = "Histogram of p-Values") +
#   scale_x_continuous(breaks = seq(0, 1, length.out = 5), limits = c(0, 1)) +
#   theme_minimal() +
#   theme(
#     text = element_text(size = 16), # For overall text size, affects all text elements
#     axis.title = element_text(size = 18), # For axis titles
#     axis.text = element_text(size = 14), # For axis text
#     legend.title = element_text(size = 16), # For legend title
#     legend.text = element_text(size = 14), # For legend text items
#     plot.title = element_text(size = 20, face = "bold")) # For the plot title
# pdf(file="DrawingLFC0_before.pdf",height=5,width = 5.5)
# print(out)
# dev.off()
# 
#'
#' @seealso \code{\link{anotherFunction}}
#' @keywords aKeywordSuchAsBenchmarking
#' @export

msb.sampleDiffRegFromPvalues <- function(pvalues,exactProportion=TRUE){
  
  isna <- is.na(pvalues)
  pvalues <- pvalues[!isna]
  if(length(pvalues)<20)
    warning("msb.sampleDiffRegFromPvalues only works reliably if enough p-values are provided.")
  
  # library(qvalue)
  qobj <- list(pi0=1-max(0,sum(pvalues<0.1)/length(pvalues)-0.1))
  
  tryCatch({qobj <- qvalue::qvalue(p = pvalues)},
           error=function(e){warning("qvalue::pvalue failed, approximation used.")})
  
  Rest <- 1 - qobj$pi0 # proportion of "regulated"

  isdiff = runif(length(pvalues))>p.adjust(pvalues,method="BH")

  # now make it exact:
  # cat("R_est = ",Rest, ", Rsampled=",sum(isdiff)/length(isdiff),"\n")
  
  if(exactProportion){
  anzMissing = round(sum(isdiff) - length(isdiff)*Rest)
  if(anzMissing <= -1){ # too few are isdiff => add some with the smallest p
    # cat("Add ",-anzMissing,"\n")
    pNotYet = pvalues
    pNotYet[isdiff] <- Inf
    rf = order(pNotYet)
    anzAdd <- -anzMissing
    isdiff[rf[1:anzAdd]] <- TRUE
  }
  if(anzMissing >= 1){ # too many are isdiff => remove some randomly
    # cat("Eliminate ",-anzMissing,"\n")
    indNotYet = which(!isdiff);
    anzMissing <- min(anzMissing,length(indNotYet))
    if(length(indNotYet)>0)
      isdiff[sample(indNotYet,anzMissing)] = FALSE
  }
  }
  
  # cat("R_est = ",Rest, ", Rsampled=",sum(isdiff)/length(isdiff),"\n")

  out <- array(data=NA,dim=length(isna))
  out[!isna] <- isdiff
   
  return(out)
}
