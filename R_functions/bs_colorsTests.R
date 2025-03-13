#' bs_colorsTests
#'
#' This file generates colors for displaying the result of different tests.
#' It can be used to always use the same colors for the individual tests. 
#'
#' details
#'
#' @param 
#' @param 
#'
#' @return
#' 
#' @examples
#' my_colors <- bs_colorsTests(data.plot.all$Test)
#' 
#'out <- ggplot(data.plot.all, aes(x = p_adjusted, y = sensitivity, color = Test)) + 
#  geom_line(aes(linetype=isCombined(Test))) +  
#   facet_wrap(~ project) +  # create a panel for each project
#   labs(x = "FDR", y = "Sensitivity") +  # label axes
#   theme_bw() +  # use a theme with a white background
#   theme(strip.text.x = element_text(size = 8),  # adjust facet label size if necessary
#         strip.background = element_rect(colour="white", fill="white")) # adjust facet label background
# out <- out + scale_color_manual(values = my_colors) +
#   scale_linetype_manual(values = c("dashed","solid")) +
#   guides(color = guide_legend(ncol = 1))
# 
#'
#' 
#'
#' @seealso \code{\link{anotherFunction}}
#' @keywords aKeywordSuchAsBenchmarking
#' @export

bs_colorsTests <- function(tests){
  
  # Color palette for ROC curves. Define palette based on 'Paired' color palette 
  color.tmp <- brewer.pal(12,"Paired")
  #colors.used <- colorRampPalette(colors.tmp)(length(unique(tests)))
  
  testLevs <- sort(unique(tests))
  testInserted <- grep("<-",testLevs,fixed=T,value = T)
  testNonInserted <- setdiff(setdiff(testLevs,testInserted),c("random reference","optimal reference","optimal specificity","optimal sensitivity"))
  
  if(length(testInserted)>0 && length(testNonInserted)>0){
    origin <- strsplit(testInserted,"<-")
    for(i in 1:length(origin))
      origin[[i]] <- origin[[i]][2]
    origin <- unlist(origin)
  }
  
  specific <- c("Maaslin2"="#D6604D","glmmTMB<-Maaslin2"="#D6604D",
                "glmmTMB"="gray",
                "random reference"="black",
                "optimal specificity"="yellow4",
                "optimal sensitivity"="yellow3",
                "optimal reference"="yellow2")
  
  if(length(testNonInserted)>0)
    auto <- setdiff(testNonInserted,names(specific))
  else
    auto <- setdiff(testInserted,names(specific))
  
  
  # cols <- setNames(rainbow(length(testLevs)),testLevs)
  # autoCols = setNames(rainbow(length(auto)),auto)
  cols <- setNames(colorRampPalette(color.tmp)(length(testLevs)),testLevs)
  autoCols = setNames(colorRampPalette(color.tmp)(length(auto)),auto)
  for(i in 1:length(auto)){
    cols[auto[i]] <- autoCols[i]
  }
  
  for(i in 1:length(specific)){
    momTest <- names(specific)[i]
    
    if(momTest %in% names(cols)){
      cols[momTest] <- rep(specific[momTest],sum(names(cols)==momTest,na.rm=T))
    }
  }
  
  if(length(testInserted)>0 && length(testNonInserted)>0){
    for(i in 1:length(testInserted))
      cols[testInserted[i]] <- cols[origin[i]]
  }
  
  return(cols)
}
