# This function generates a data.frame which summarizes the object at a specific bstype level.
# Gives a summary or result can be used for plotting.
#
#   bstype    the bstype attribute of the elements of the object, see attributes(bsobj), attributes (bsobj$..)
#             Implemented so far:
#             "data_template"
#             "sim_result"
#             "dataset" means both: "data_template" and "sim_result"
#
# Examples:
#
# bsobj <- readRDS("../Results/data_to_compare.RDS")
# bs_summarize(bsobj,"sim_result")
# bsobj <- addDataProp(bsobj)
# bs_summarize(bsobj,"sim_result")
# bs_summarize(bsobj,"data_template")
# bs_summarize(bsobj,"dataset")
# bs_summarize(bsobj["ArcticFireSoils"],"dataset") # only one data_template


bs_summarize <- function(bsobj,bstype="dataset")
{
  if(is.null(attr(bsobj,"bstype")) || attr(bsobj,"bstype")!="data_list")
      stop("Call bs_summarize only with a 'data_list' Use only one bracket [] if an element of a data_list is to be summarized.")
  
  sumList <- list()
  ii <- 0 # counter
  
  # "sim_result" and "data_template" are handled equally:
  if(bstype=="sim_result" || bstype=="data_template"){ 
    for(i in 1:length(bsobj)){ # loop over all original dataset
      for(j in 1:length(bsobj[[i]])){ # loop over all datasets from an original template
        # cat(i,",",j,"\n")
        if(msb.attrCompare(bsobj[[i]][[j]],"bstype",bstype)){
          dat <- bsobj[[i]][[j]] # for keeping the code short
          ii <- ii+1
          for(name in names(sumList))
            sumList[[name]][ii] <- NA
          
          if(is.null(names(bsobj[[i]])))
            stop("No names bsobj[[i]] available. Did you loose names? Did you subset without msb.subset?")
          
          if(is.null(dim(dat$counts)))
            warning("is.null(dim(dat$counts)): Maybe you have to call bs_decompress first.")
          
          sumList$name[ii] <- names(bsobj[[i]])[[j]] 
          sumList$data_template[ii] <- names(bsobj)[[i]]
#          sumList$ndata[ii] <- prod(dim(dat$counts))
          sumList$nfeature[ii] <- prod(dim(dat$counts)[1])
          sumList$nsamples[ii] <- prod(dim(dat$counts)[2])
#          sumList$sumCounts[ii] <- sum(dat$counts,na.rm=T)
#          sumList$medianDepth[ii] <- median(colSums(dat$counts),na.rm=T)

          if(msb.attrCompare(dat,"bstype","data_template")){
             sumList$sim_param[ii] <- "template"
             sumList$sim_type[ii] <- "template"
             sumList$sim_mu1[ii] <- "template"
             sumList$sim_mu2[ii] <- "template"
             sumList$sim_sd1[ii] <- "template"
             sumList$sim_sd2[ii] <- "template"
          } else if(is.null(dat$params[[1]]$simParaChangeList)){ # no simParaChangeList
              sumList$sim_param[ii] <- "sim_default"
              sumList$sim_type[ii] <- "sim_default"
              sumList$sim_mu1[ii] <- "0"
              sumList$sim_mu2[ii] <- "0"
              sumList$sim_sd1[ii] <- "0"
              sumList$sim_sd2[ii] <- "0"
              
              
          } else {
            sumList$sim_param[ii] <- paste0("mu1(", dat$params[[1]]$simParaChangeList$mu1,")_sd1(",dat$params[[1]]$simParaChangeList$sd1,")_mu2(",dat$params[[1]]$simParaChangeList$mu2,")_sd2(",dat$params[[1]]$simParaChangeList$sd2,")")
          
          sumList$sim_type[ii] <- dat$params[[1]]$simParaChange
          sumList$sim_mu1[ii] <- dat$params[[1]]$simParaChangeList$mu1
          sumList$sim_mu2[ii] <- dat$params[[1]]$simParaChangeList$mu2
          sumList$sim_sd1[ii] <- dat$params[[1]]$simParaChangeList$sd1
          sumList$sim_sd2[ii] <- dat$params[[1]]$simParaChangeList$sd2
          }
          
          if(!is.null(dat$data.Prop)){
            
            for(prop in names(dat$data.Prop)){
              if(length(dat$data.Prop[[prop]])==1){
                sumList[[prop]][ii] <- dat$data.Prop[[prop]]
              }
            }
            # sumList$P0[ii] <- dat$data.Prop$P0
            # sumList$median[ii] <- dat$data.Prop$median
            # sumList$q95[ii] <- dat$data.Prop$q95
            # sumList$q99[ii] <- dat$data.Prop$q99
            # sumList$median_lib_size[ii] <- dat$data.Prop$median_lib_size
            # sumList$mean_lib_size[ii] <- dat$data.Prop$mean_lib_size # Change from mean_meanlib_size
            # sumList$max_libSize[ii] <- dat$data.Prop$max_libSize
            # sumList$min_libSize[ii] <- dat$data.Prop$min_libSize
            # sumList$SD_lib_size[ii] <- dat$data.Prop$SD_lib_size
            # sumList$CV_lib_size[ii] <- dat$data.Prop$CV_lib_size
            # sumList$range_lib_size[ii] <- dat$data.Prop$range_lib_size
            # sumList$mean_sample_richness[ii] <- dat$data.Prop$mean_sample_richness #hat gefehlt
            # sumList$nfeature[ii] <- dat$data.Prop$nfeature
            # sumList$nsamples[ii] <- dat$data.Prop$nsamples
            # sumList$nsamplesWithCounts[ii] <- dat$data.Prop$nsamplesWithCounts #hat gefehlt
            # sumList$corr_libsize_P0sample[ii] <- dat$data.Prop$corr_libsize_P0sample
            # sumList$SD_smallRowMeans[ii] <- dat$data.Prop$SD_smallRowMeans #hat gefehlt
            # sumList$bimodality_corr_feature[ii] <- dat$data.Prop$bimodality_corr_feature
            # sumList$bimodality_corr_sample[ii] <- dat$data.Prop$bimodality_corr_sample
            # sumList$mean_mean_log2cpm[ii] <- dat$data.Prop$mean_mean_log2cpm
            # sumList$SD_mean_log2cpm[ii] <- dat$data.Prop$SD_mean_log2cpm
            # sumList$median_median_log2cpm[ii] <- dat$data.Prop$median_median_log2cpm
            # sumList$SD_median_log2cpm[ii] <- dat$data.Prop$SD_median_log2cpm
            # sumList$mean_var_log2cpm[ii] <- dat$data.Prop$mean_var_log2cpm
            # sumList$SD_var_log2cpm[ii] <- dat$data.Prop$SD_var_log2cpm
            # sumList$mean_sample_means[ii] <- dat$data.Prop$mean_sample_means
            # sumList$SD_mean_sample_means[ii] <- dat$data.Prop$SD_mean_sample_means
            # sumList$mean_corr_sample[ii] <- dat$data.Prop$mean_corr_sample
            # sumList$SD_corr_sample[ii] <- dat$data.Prop$SD_corr_sample
            # sumList$mean_corr_feature[ii] <- dat$data.Prop$mean_corr_feature
            # sumList$SD_corr_feature[ii] <- dat$data.Prop$SD_corr_feature
            # sumList$coef_poly2_varlog2cpm_meanlog2cpm_linear[ii] <- dat$data.Prop$coef_poly2_varlog2cpm_meanlog2cpm_linear
            # sumList$coef_poly2_varlog2cpm_meanlog2cpm_quadratic[ii] <- dat$data.Prop$coef_poly2_varlog2cpm_meanlog2cpm_quadratic
            # sumList$coef_poly1_meanlog2cpm_P0feature_slope[ii] <- dat$data.Prop$coef_poly1_meanlog2cpm_P0feature_slope
            # sumList$coef_hclust_features[ii] <- dat$data.Prop$coef_hclust_features
            # sumList$coef_hclust_samples[ii] <- dat$data.Prop$coef_hclust_samples
            # sumList$Permanova_R2[ii] <- dat$data.Prop$Permanova_R2
            # sumList$Permanova_P_value[ii] <- dat$data.Prop$`Permanova_P-value` #geändert von P_value
          }
          
          sumList$bstype[ii] <- bstype 
        }
      }
    }
  } # "sim_counts", "data_template"
  
  # "dataset": call this function twice with bstype="sim_result" and bstype="data_template"
  if(bstype=="dataset"){
    
    template <- bs_summarize(bsobj,bstype="data_template")
    sim <- bs_summarize(bsobj,bstype="sim_result")
    
    sumList <- rbind(
      bs_summarize(bsobj,bstype="data_template"),
      bs_summarize(bsobj,bstype="sim_result"))

    # reorder rows according to data_template to have original and simulated below each other:
    rf <- order(sumList$data_template)
    sumList <- sumList[rf,]
  }
    
  
  if(length(sumList)==0){
    warning(paste0("No matching elements with bstype=",bstype," found."))
  }
  
  sumDF <- data.frame(sumList)
  return(sumDF)
}
