# This function collects all values calculated in bs_diffDataProp and makes a big data.frame
#
# # Example:
# d <- readRDS("data_to_compare.RDS")
# d <- bs_addDataProp(d)
# d <- bs_diffDataProp(d)
# DF <- bs_summarize_dataPropCmp(d)

bs_summarize_dataPropCmp <- function(dataList) {
  
  if(attr(dataList,"bstype")!="data_list")
    stop("bs_summarize_dataPropCmp is only implemented for bstype data_list.")
  
  propCmpDF <- NULL
  dataNames <- names(dataList)
  
  for(d in 1:length(dataList)){
    dataProject <- dataList[[d]]
    
    for(i in 1:length(dataProject)){ # all fields within a project, e.g. $original, $metaSPARsim_1, ..., and data.prop.cmp
      if(msb.attrCompare(dataProject[[i]],"bstype","dataProp_comparison")){
        propCmp <- dataProject[[i]] # a data.prop.cmp list, usually somethind like data_to_compare$projectName$data.prop.cmp
        
        for(j in 1:length(propCmp)){ # over all fields of the data.prop.cmp, e.g. diff, log2ratio, ...
          if(length(propCmp[[j]])==0){
            print(paste(d,i,j,names(propCmp)[j]," is empty."))
          }else{
            for(k in 1:length(propCmp[[j]])){ # over all data.prop which are compared, e.g. P0, median,...
            print(paste(d,i,j,k))
            cmpMatrix <- propCmp[[j]][[k]] # cmparision, nData x nData (nData = oneTemplate + numberOfSimus)
            print(paste0(dataNames[d], ", Prop=",names(propCmp[[j]])[k], ", dim=", dim(cmpMatrix)[1], "x", dim(cmpMatrix)[1]))
            if(dim(cmpMatrix)[1]>1 && dim(cmpMatrix)[2]>1){
              for(i1 in 1:(dim(cmpMatrix)[1]-1)){
                for(i2 in setdiff(1:dim(cmpMatrix)[2],i1)){
                    # remember the names of all levels (i.e. for each loop index)
                  # Hier kommt ein Fehler, replaceTailingNums=T unused argument....
                  #rn <- msb.ExtractTailingNumbers(rownames(cmpMatrix)[i1],replaceTailingNums = T) # eliminate numbers at the end
                  #cn <- msb.ExtractTailingNumbers(colnames(cmpMatrix)[i2],replaceTailingNums = T) # eliminate numbers at the end
                  tmp <- data.frame(data_project=names(dataList)[d],
                                    data_prop_cmp=names(dataProject)[i],
                                    cmp_type=names(propCmp)[j],
                                    data_prop=names(propCmp[[j]])[k],
                                    dataSet1 = rownames(cmpMatrix)[i1],
                                    dataSet2 = colnames(cmpMatrix)[i2],
                                    difference = cmpMatrix[i1,i2])
                  #comparison = paste(rn,cn,sep="_vs_")
                  
                  
                  if(is.null(propCmpDF))
                    propCmpDF <- tmp
                  else
                    #propCmpDF <- data.table::rbindlist(propCmpDF,tmp) # according to web faster than rbind
                    propCmpDF <- rbind(propCmpDF,tmp) # rbindlist hat nicht funktioniert, da input data frame und keine liste
                }
              }
            }
          } # over all dataProp (e.g. P0, median,...)
          }
        } # over all comparison types (e.g. diff, log2ratio,...)
      } # if bstype=="dataProp_comparison"
    } # over all fields of a data_project
  } # over all data_project in data_list
  
  # add a column indicating whether against the template was compared (or only within simulated)
  # comparison <- paste(sub("_","",msb.ExtractTailingNumbers(propCmpDF$dataSet1,replaceTailingNums = T)),
  #                     sub("_","",msb.ExtractTailingNumbers(propCmpDF$dataSet2,replaceTailingNums = T)),sep="_vs_")
  # comparison <- as.factor(comparison)
  
  if(!is.null(propCmpDF)){
    comparison2 <- rep("sim_vs_sim",nrow(propCmpDF))
    comparison2[propCmpDF$dataSet1=="original"] <- "template - sim"
    comparison2[propCmpDF$dataSet2=="original"] <- "template - sim"
    comparison2[propCmpDF$dataSet1=="original" & propCmpDF$dataSet2=="original"] <- "template - template"
    comparison2 <- relevel(as.factor(comparison2),ref="template - sim")
    
    #propCmpDF$comparison <- as.factor(propCmpDF$comparison)
    
    #propCmpDF <- data.frame(propCmpDF,comparison=comparison,comparison2=comparison2)
    propCmpDF <- data.frame(propCmpDF,comparison2=comparison2)
    return(propCmpDF)
  } else {
    return(NULL)
  }
}
