# Function to calculate the difference in data Properties between each pair of data templates




# Example:
# d <- readRDS("data_to_compare.RDS")
# d <- addDataProp(d)
# d <- bs_diffDataProp_templates(d)

bs_diffDataProp_templates <- function(input){
  
  if(msb.attrCompare(input,"bstype","data_list")){
    
    # Type of differences that are stored:
    
    dif <- list()
    log2ratio <- list()
    features_ksStat <- list()
    samples_ksStat <- list()
    
    # Get only list elements that are data projects
    isData <- which(sapply(input,function(x){any(msb.attrCompare(x,"bstype",c("data_project")))}))
    
    for(ii1 in 1:length(isData)){#Loop over all templates
      
      i1 <- isData[ii1]
      for(ii2 in 1:length(isData)){
        
        i2 <- isData[ii2]
        
        dp1 <- input[[i1]]$original$data.Prop #Get data props for first template
        if(is.null(dp1))
          warning("No data properties for input[[",i1,"]]")
        dp2 <-  input[[i2]]$original$data.Prop #Get data props for second template
        if(is.null(dp2))
          warning("No data properties for input[[",i2,"]]")
        fields <- intersect(names(dp1),names(dp2))
        
        
        for(f in 1:length(fields)){ # loop over all data.Props
          # Properties which are single numbers:
          if(length(dp1[[fields[f]]])==length(dp2[[fields[f]]]) && length(dp1[[fields[f]]])==1){
            if(is.null(log2ratio[[fields[f]]])){ # 1st call
              # Set dimension and names of matrices to store values for dataProp differences
              dif[[fields[f]]] <- matrix(NA,ncol=length(isData),nrow=length(isData))
              log2ratio[[fields[f]]] <- matrix(NA,ncol=length(isData),nrow=length(isData))
              rownames(dif[[fields[f]]]) <- names(input)[isData]
              colnames(dif[[fields[f]]]) <- names(input)[isData]
              rownames(log2ratio[[fields[f]]]) <- names(input)[isData]
              colnames(log2ratio[[fields[f]]]) <- names(input)[isData]
            }
            # Calculate dif and log2ratio for single number properties
            
            dif[[fields[f]]][ii1,ii2] <- dp1[[fields[f]]]-dp2[[fields[f]]]
            log2ratio[[fields[f]]][ii1,ii2] <- log2(dp1[[fields[f]]]/dp2[[fields[f]]])
            
          }
          
          
          # feature-wise dataProp: calcuate ks.test$statistic:
          warnlevel <- options()$warn
          options(warn=-1) # to prevent warnings of ks.test
         
          if(length(dp1[[fields[f]]])==dp1$nfeature){
            if(is.null(features_ksStat[[fields[f]]])){# 1st call
              # Set dimension and names for matrix to store ks-statistic
              features_ksStat[[fields[f]]] <- matrix(NA,ncol=length(isData),nrow=length(isData))
              rownames(features_ksStat[[fields[f]]]) <- names(input)[isData]
              colnames(features_ksStat[[fields[f]]]) <- names(input)[isData]
            }
            # Calculate ks-stat
            features_ksStat[[fields[f]]][ii1,ii2] <- ks.test(dp1[[fields[f]]],dp2[[fields[f]]],exact=FALSE)$statistic
            
          }
          
          # sample-wise dataProp: calcuate ks.test$statistic:
          
          if(length(dp1[[fields[f]]])==dp1$nsamples){
            if(is.null(samples_ksStat[[fields[f]]])){# 1st call
              # Set dimension and names for matrix to store ks-statistic
              samples_ksStat[[fields[f]]] <- matrix(NA,ncol=length(isData),nrow=length(isData))
              rownames(samples_ksStat[[fields[f]]]) <- names(input)[isData]
              colnames(samples_ksStat[[fields[f]]]) <- names(input)[isData]
            }
            # Calculate ks-stat
            samples_ksStat[[fields[f]]][ii1,ii2] <- ks.test(dp1[[fields[f]]],dp2[[fields[f]]],exact=FALSE)$statistic
          }
          options(warn=warnlevel) # reset warning level
        }
        
      }
    
    }
    list <- list(diff = dif,
                 log2ratio=log2ratio,
                 features_ksStat=features_ksStat,
                 samples_ksStat=samples_ksStat)
    # input$data.prop.cmp.template <- list(diff = dif,
    #                             log2ratio=log2ratio,
    #                             features_ksStat=features_ksStat,
    #                             samples_ksStat=samples_ksStat)
    # attr(input$data.prop.cmp.template,"bstype") <- "dataProp_comparison"
    #input$data.prop.cmp.info <- msb.info()
    
    #attr(input$data.prop.cmp.info,"bstype") <- "dataProp_comparison.info"
  }
  return(list)
  #return(input)
  }
 
    
    
    
    
    
    
    
    
    
    
    
    
   
