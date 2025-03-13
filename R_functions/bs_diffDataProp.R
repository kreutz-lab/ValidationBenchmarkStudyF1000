# Function to calculate the difference in data Properties between each pair of data sets. 




# Example:
# d <- readRDS("data_to_compare.RDS")
# d <- addDataProp(d)
# d <- bs_diffDataProp(d)

bs_diffDataProp <- function(input){
  
  if(msb.attrCompare(input,"bstype","data_project")){
    
    # Type of differences that are stored:
    
    dif <- list()
    log2ratio <- list()
    features_ksStat <- list()
    samples_ksStat <- list()
    
    # which list elements are either "data_template" or "sim_result"?
    isData <- which(sapply(input,function(x){any(msb.attrCompare(x,"bstype",c("data_template","sim_result")))}))
    if(length(isData)<2){
      stop("bs_diffDataProp: length(isData)<2, Not meaningful since less than two dataset are available.")
    }
    
    for(ii1 in 1:length(isData)){ # loop over all data-sets within one project (1st of all pairwise comparisons)
      i1 <- isData[ii1]
      for(ii2 in 1:length(isData)){ # loop over all data-sets within one project  (2nd of all pairwise comparisons)
        i2 <- isData[ii2]
        # one could reduce computation effects by not comparing with itself and i1 vs. i2 == i2 vs. i1
        dp1 <- input[[i1]]$data.Prop
        if(is.null(dp1))
          warning("No data properties for input[[",i1,"]]")
        dp2 <- input[[i2]]$data.Prop
        if(is.null(dp2))
          warning("No data properties for input[[",i2,"]]")
        fields <- intersect(names(dp1),names(dp2))
        
        for(f in 1:length(fields)){ # loop over all data.Props
          # Properties which are single numbers:
          if(length(dp1[[fields[f]]])==length(dp2[[fields[f]]]) && length(dp1[[fields[f]]])==1){
            if(is.null(log2ratio[[fields[f]]])){ # 1st call
              # Set dimension and names of matrices to store values for dataProp differences
              dif[[fields[f]]] <- matrix(NA,ncol=length(input[isData]),nrow=length(input[isData]))
              log2ratio[[fields[f]]] <- matrix(NA,ncol=length(input[isData]),nrow=length(input[isData]))
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
              features_ksStat[[fields[f]]] <- matrix(NA,ncol=length(input[isData]),nrow=length(input[isData]))
              rownames(features_ksStat[[fields[f]]]) <- names(input)[isData]
              colnames(features_ksStat[[fields[f]]]) <- names(input)[isData]
            }
            # Calculate ks-stat
            features_ksStat[[fields[f]]][ii1,ii2] <- ks.test(dp1[[fields[f]]],dp2[[fields[f]]],exact=FALSE)$statistic
            # plots for testing/understanding:
            # if(i1!=i2){
            #   plot(ecdf(dp1[[fields[f]]]))
            #   lines(ecdf(dp2[[fields[f]]]))
            #   plot(quantile(dp1[[fields[f]]],seq(0,1,length.out=100)),
            #        quantile(dp2[[fields[f]]],seq(0,1,length.out=100)),
            #        main=features_ksStat[[fields[f]]][i1,i2])
            # }
          }
          
          # sample-wise dataProp: calcuate ks.test$statistic:
          if(length(dp1[[fields[f]]])==dp1$nsamples){
            if(is.null(samples_ksStat[[fields[f]]])){# 1st call
              # Set dimension and names for matrix to store ks-statistic
              samples_ksStat[[fields[f]]] <- matrix(NA,ncol=length(input[isData]),nrow=length(input[isData]))
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
    
    input$data.prop.cmp <- list(diff = dif,
                                log2ratio=log2ratio,
                                features_ksStat=features_ksStat,
                                samples_ksStat=samples_ksStat)
    attr(input$data.prop.cmp,"bstype") <- "dataProp_comparison"
    input$data.prop.cmp.info <- msb.info()
    
    attr(input$data.prop.cmp.info,"bstype") <- "dataProp_comparison.info"
  }
  
  else if(is.list(input)){
    for(i in 1:length(input)){
      input[[i]] <- bs_diffDataProp(input[[i]])
    }
  }
  return(input)
}

