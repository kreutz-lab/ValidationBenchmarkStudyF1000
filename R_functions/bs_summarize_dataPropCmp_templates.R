# Function to summarize dataPropCmp for all templates in one data frame
# Similar to bs_summarize_dataPropCmp

# Input: List with all comparisons of data properties of the templates - output from bs_diffDataProp_templates

# @Example 
# d <- readRDS("data_to_compare.RDS")
# d <- addDataProp(d)
# cmp_templates <- bs_diffDataProp_templates(d)
# summary_cmp_templates <- bs_summarize_dataPropCmp_templates(cmp_templates) 

bs_summarize_dataPropCmp_templates <- function(cmp_templates){
  
propCmpDF_templates <- NULL

for(j in 1:length(cmp_templates)){ # over all fields of the data.prop.cmp, e.g. diff, log2ratio, ...
  for(k in 1:length(cmp_templates[[j]])){ # over all data.prop which are compared, e.g. P0, median,...
    
    cmpMatrix <- cmp_templates[[j]][[k]] # cmparision, nData x nData (nData = oneTemplate + numberOfSimus)
    #print(paste0(dataNames[d], ", Prop=",names(cmp_templates[[j]])[k], ", dim=", dim(cmpMatrix)[1], "x", dim(cmpMatrix)[1]))
    for(i1 in 1:(dim(cmpMatrix)[1]-1)){
      for(i2 in (i1+1):dim(cmpMatrix)[2]){
        # remember the names of all levels (i.e. for each loop index)
        tmp <- data.frame(data_project=rownames(cmpMatrix)[i1],
                          data_prop_cmp=names(cmp_templates)[k],
                          cmp_type=names(cmp_templates)[j],
                          data_prop=names(cmp_templates[[j]])[k],
                          dataSet1 = rownames(cmpMatrix)[i1],
                          dataSet2 = colnames(cmpMatrix)[i2],
                          difference = cmpMatrix[i1,i2])
        
        if(is.null(propCmpDF_templates))
          propCmpDF_templates <- tmp
        else
          #propCmpDF <- data.table::rbindlist(propCmpDF,tmp) # according to web faster than rbind
          propCmpDF_templates <- rbind(propCmpDF_templates,tmp) # rbindlist hat nicht funktioniert, da input data frame und keine liste
        
      }
    }
  }
}
  propCmpDF_templates$comparison <- rep("template_vs_template", nrow(propCmpDF_templates))
 

return(propCmpDF_templates)
}