#' bs_checkData
#'
#' This function performs several tests on the data:
#' -> replacement of negative numbers with zero (by default)
#' -> replacement of NA with zero (by default)
#' -> conversion of counts to matrices
#'
#' @param data_list object e.g. data_to_compare
#' @param replaceNA default: TRUE 
#' @param replaceNegative default: TRUE 
#' @param eliminateProblematic Removing data_projects where issues>0? default: FALSE
#' @param eliminateZeroSimus Removing data_projects where simulations failed? default: FALSE
#'
#' @return data_list object
#' 
#' @examples
#' 
#' d <- readRDS(file="../R_scripts/data_to_compare.RDS")
#' d2 <- bs_checkData(d)
#'
#' @seealso \code{\link{anotherFunction}}
#' @keywords aKeywordSuchAsBenchmarking
#' @export

bs_checkData <- function(d, replaceNA=TRUE, replaceNegative=TRUE, eliminateProblematic=FALSE, eliminateZeroSimus=FALSE){
  if(!msb.attrCompare(d,"bstype","data_list"))
    stop("bs_checkData is up to now only implemented for a data_list (whole data_to_compare object, do subsetting via msb.subset)")
  
  
  namen <- names(d)
  issues <- array(0,dim = length(d)) # for each data_project, the issue is increase by one if something suspicious is detected.
  warnings <- array(0,dim = length(d)) # for each data_project, the warning is increase by one if something might be suspicous
  templates <- array(0,dim = length(d)) # number of data_templates
  simus <- array(0,dim = length(d)) # number of sim_results
  dataprops <- array(0,dim = length(d)) # number of sim_results
  datapropCmps <- array(0,dim = length(d)) # number of sim_results
  da <- array(0,dim = length(d)) # number of sim_results
  daTests <- array(0,dim = length(d)) # number of sim_results
  for(i in 1:length(d)){ # loop over all data_projects
    dimText <- ""
    
    if(bs_isa(d[[i]],"data_list")) # CK: error in some workspaceds => temporary code for correction
      attr(d[[i]],"bstype") <- "data_project"
    
    if(msb.attrCompare(d[[i]],"bstype","data_project")){
      project_label <- namen[i]
      
      
      namen2 <- names(d[[i]])
      countDims <- NULL
      
      if("data.prop.cmp" %in% names(d[[i]]))
        datapropCmps[i] <- length(d[[i]]$data.prop.cmp)
      
      for(j in 1:length(d[[i]])){ # loop over all data sets
        if(any(msb.attrCompare(d[[i]][[j]],"bstype",c("data_template","sim_result")))){
          if(!"condition" %in% names(d[[1]][[1]]$meta)){
            issues[i] <- issues[i]+1
            warning("bs_checkData: ",namen[i],"$",namen2[j],": No $condition in meta data!")
          }
          
          counts <- d[[i]][[j]]$counts
          if(is.character(counts[1])){
            issues[i] <- issues[i]+1
            warning("bs_checkData: ",namen[i],"$",namen2[j],": Character values in counts!")
          }
          
          if(is.null(nrow(counts))){
            issues[i] <- issues[i]+1
            warning("bs_checkData: ",namen[i],"$",namen2[j],": Counts have no nrow attribute => no matrix. Maybe call bs_decompress!")
          }
          if("bstype" %in% names(attributes(counts)))
            bstype <- attr(counts,"bstype")
          else{
            if(namen2[j]=="original")
              bstype <- "raw_counts" # set it manually
            else
              bstype <- "sim_counts" # set it manually
          }
          
          counts <- as.matrix(counts)
          if(is.null(countDims))
            countDims <- dim(counts)
          
          counts.norm <- d[[i]][[j]]$counts.norm
          if(!is.null(counts.norm) && !"bstype" %in% names(attributes(d[[i]][[j]]$counts.norm)))
            attr(d[[i]][[j]]$counts.norm,"bstype") <- "norm_counts" # set it manually
          
          if(any(msb.attrCompare(d[[i]][[j]],"bstype",c("data_template")))){
            dimText1 <- paste0(", $counts: ",dim(counts)[1],"x",dim(counts)[2])
            dimText <- paste0(", $counts.norm: ",dim(counts.norm)[1],"x",dim(counts.norm)[2])
            templates[i] <- templates[i]+1
          }
          if(any(msb.attrCompare(d[[i]][[j]],"bstype",c("sim_result"))))
            simus[i] <- simus[i]+1
          
          if(sum(names(d[[i]][[j]])=="data.Prop")>0)
            dataprops[i] <- dataprops[i]+1
          
          if(sum(names(d[[i]][[j]])=="DA")>0){
            da[i] <- da[i]+1
            daTests[i] <- daTests[i] + ncol(d[[i]][[j]]$DA$logFold)
          }
          
          if(!is.null(counts.norm) && sum(abs(dim(counts)-dim(counts.norm)))>0){
            issues[i] <- issues[i]+1
            warning("bs_checkData: dim(count)!=dim(counts.norm), ",dim(counts),", ",dim(counts.norm))
          }
          
          if(sum(abs(countDims-dim(counts)))>0){
            warnings[i] <- warnings[i]+1
            warning("bs_checkData: ",namen[i],"$",namen2[j],": Counts different dimentions compared with exp template: ", countDims," vs. ",dim(counts))
          }
          
          if(!all(!is.na(counts))){
            issues[i] <- issues[i]+1
            if(replaceNA){
              counts[is.na(counts)] <- 0
              warning("bs_checkData: ",namen[i],"$",namen2[j],": Counts have NA. Will be replace by 0. ")
            }
            else
              warning("bs_checkData ",namen[i],"$",namen2[j],": NAs in counts.")
          }
          if(sum(counts<0,na.rm=T)>0){
            issues[i] <- issues[i]+1
            if(replaceNegative){
              counts[counts<0] <- 0
              warning("bs_checkData ",namen[i],"$",namen2[j],": Counts have negative values. Will be replace by 0. ")
            }
            else
              warning("bs_checkData ",namen[i],"$",namen2[j],": Counts have negative values. No replacement done.")
          }
          
          if(length(grep("metaSPARSim_",namen2[j]))>0){
            conds <- names(d[[i]][[j]]$params)
            for(k in 1:length(conds)){
              worked <- 0
              try({
                if(is.null(nrow(d[[i]][[j]]$counts)))
                  warning("is.null(nrow(d[[i]][[j]]$counts): counts are no matrices. Maybe you have to call bs_decompress first?")
                else{
                  if(length(d[[i]][[j]]$params[[conds[k]]]$intensity)!=nrow(d[[i]][[j]]$counts)){
                    issues[i] <- issues[i]+1
                    warning("bs_checkData ",namen[i],"$",namen2[j],": length(params$intensity != nrow(counts). ")
                  }
                  if(length(d[[i]][[j]]$params[[conds[k]]]$variability)!=nrow(d[[i]][[j]]$counts)){
                    issues[i] <- issues[i]+1
                    warning("bs_checkData ",namen[i],"$",namen2[j],": length(params$variability != nrow(counts). ")
                  }
                }
                worked <- 1
              })
              if(worked==0){
                issues[i] <- issues[i]+1
                warning("bs_checkData: try params incomplete in ",namen[i],"$",namen2[j],". ")
              }
            }
          }
          
          if(!is.null(bstype))
            attr(counts,bstype) <- bstype
          
          d[[i]][[j]]$counts <- counts
        }
      }
    }
    if(issues[i]==0)
      cat(paste0("No issues for data set ",namen[i],"$counts: ",dim(counts)[1],"x",dim(counts)[2],dimText,"\n"))
    else{
      cat(paste0("!", issues[i]," issues for data set ",namen[i],dimText1,dimText,"\n"))
      warning("Some issues for data set ",namen[i])
    }
  }
  print(data.frame(project=namen, issues=issues, warnings = warnings, templates=templates, simus=simus, data.Props=dataprops, 
                   datapropCmps=datapropCmps, DA=da, DA.tests = daTests))
  
  if(eliminateProblematic && sum(issues)>0){
    warning("Problematic datasets are removed.")
    d <- msb.subset(d,issues==0)
  }
  if(eliminateZeroSimus){
    warning("Datasets without simulated data are removed.")
    d <- msb.subset(d,simus>0)
  }
  
  return(invisible(d))
}


