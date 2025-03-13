#' bs_addExperimentalZeros
#'
#' This function translates an appropriate proportion of zeros from the $original$counts template
#' to simulation counts 
#'
#' 
#'
#' @param d data_list object
#' @param makeCopies [TRUE], if false the original simulation will be replaced
#' @param simulationNames Simulation names that are used to integrate zeros
#'
#' @return
#' 
#' @examples
#' 
#'
#' 
#'
#' @seealso \code{\link{anotherFunction}}
#' @keywords aKeywordSuchAsBenchmarking
#' @export

bs_addExperimentalZeros <- function(d, makeCopies=T, simulationNames=NULL){
  
  if(!msb.attrCompare(d,"bstype","data_list"))
    stop("bs_addExperimentalZeros is only implemented for bstype=data_list")
  
  namen <- names(d)
  namen2 <- list()
  for(i in 1:length(d)){
    namen2[[i]] <- names(d[[i]])
    
    # 1st find exp. count template
    expCounts <- NULL
    for(j in 1:length(d[[i]])){
      if(msb.attrCompare(d[[i]][[j]],"bstype","data_template")){
        expCounts <- as.matrix(d[[i]][[j]]$counts)
        print(paste0("Experimental data found for ",namen[i]))
      }
    } 
      
    # 2. Translation of zeros to simus
    
    # If old simulations should be removed (makeCopies=FALSE), remember indices
    indx.remove <- array()
    
    for(j in 1:length(namen2[[i]])){
      if(!is.null(expCounts)){ # exp. counts available
        
        #for(n in 1:length(namen2)){
          # Wenn simulation, und wenn simulationName passt:
          if(msb.attrCompare(d[[i]][[j]],"bstype","sim_result") && (is.null(simulationNames) || length(intersect(simulationNames,msb.ExtractTailingNumbers(namen2[[i]][j],replaceTailingNums = T)))>0)){
            simCounts <- d[[i]][[j]]$counts
            bstyp <- attr(simCounts,"bstype")
            if(is.data.frame(simCounts))
              warning("is.data.frame(simCounts): Maybe call bs_checkData for conversion first.")
            # make expCounts2 with the same dimensions as simCounts
            if(is.null(simCounts) || is.null(expCounts))
              stop(paste0("bs_addExperimentalZeros: is.null(simCounts) || is.null(expCounts): Maybe you have to call bs_decompress first."))
            if(sum(abs(dim(simCounts)-dim(expCounts)))>0){ # unequal dimensions
              # cyclic recycling
              ind1 <- 1:dim(simCounts)[1]
              ind2 <- 1:dim(simCounts)[2]
              expCounts2 <- expCounts[(ind1-1) %% dim(expCounts)[1]+1, (ind2-1) %% dim(expCounts)[2]+1]  # subsetting or cyclic extension
            }
            else 
              expCounts2 <- expCounts
            
            ind0Sim <- which(simCounts==0)
            ind0Exp <- which(expCounts2==0)
            anz0Exp <- length(ind0Exp)
            anz0Sim <- length(ind0Sim)
            anz0Add <- anz0Exp-anz0Sim
            if(anz0Add>0){
              print(paste0(anz0Add," zeros will be added for ",namen2[[i]][[j]]))
              sumBefore <- sum(simCounts,na.rm = T)
              indAdd0 <- sample(setdiff(ind0Exp,ind0Sim),anz0Add,replace=FALSE)
              simCounts[indAdd0] <- 0
              sumAfter <- sum(simCounts,na.rm = T) # Reduzierung der Counts
              simCounts <- round(simCounts * sumBefore/sumAfter) # Reduzierung korrigieren
            }else{
              indAdd0 <- "none"
              print(paste0(" No zeros will be added for ",namen2[[i]][[j]]," since anzAdd0 is ",anz0Add))
            }
            # the following line places _add0_ inside the simuName, e.g. metaSparsim_1 -> metaSparsim_add0_1
            newName <- paste0(msb.ExtractTailingNumbers(namen2[[i]][j],replaceTailingNums = T),"add0_",msb.ExtractTailingNumbers(namen2[[i]][j]))
            d[[i]][[newName]] <- d[[i]][[j]]
            attr(simCounts,"bstype") <- bstyp # be sure that bstype is conserved
            d[[i]][[newName]]$counts <- simCounts
            d[[i]][[newName]]$indAdd0 <- indAdd0
            
            if(!makeCopies){
              #d[[i]][[j]] <- NULL # overwrite old simu
              # Remember index of simulation to be removed
              indx.remove <- c(indx.remove,j)
            }
              
          #}
        }
      }
      else{
        warning("No experimental counts found for ",namen[i],".")
      }
    }
    # remove old simulations if makdeCopies=FALSE
    if(!is.null(indx.remove))
      d[[i]][indx.remove] <- NULL
  }
  return(d)
}
