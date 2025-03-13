# Function to make PCA plot or biplot on scaled data properties for templates and simulations
# 
# Input is summary_DF, data frame with data properties for each data set (output of bs_summarize)

# @example: 
# For all data properties
#summary_DF_all <- readRDS("../Results/2023-04-25-multipleDefaultSim_n10/summary_DF_all.RDS")
# PCA <- bs_PCA_dataProp_sim(summary_DF_all)
# ggsave(path to folder)

# For subset of data properties
#summary_DF_all <- readRDS("´../Results/2023-04-25-multipleDefaultSim_n10/summary_DF_all.RDS")
# list_dataProps <- c("P0", "corr_libsize_P0sample","bimodality_corr_feature","bimodality_corr_sample")
# PCA <- bs_PCA_dataProp_sim(summary_DF_all, list_dataProps=list_dataProps)
# ggsave(path to folder)

# The following libraries are required and loaded in project_init:
 #library(tidyverse)
 #library(ggfortify)
 #library(ggsci) #to get color palettes: pa_npg()...
 #library(scales) # show_col

# Median is excluded form data properties, because it results in NA after scaling




bs_PCA_dataProp_sim <- function(data_PCA, list_dataProps = NULL,
                                title = "Multiple sim realisations - dim red based on data prop",
                                type = "PCA",
                                folder=NULL, filename = "") {
  
  if(is.null(list_dataProps)){
    list_dataProps <- colnames(data_PCA)
    
    # Only use appropriate columns (starting with P0 AND numeric)
    indP0 <- which(list_dataProps=="P0") 
    list_dataProps <- list_dataProps[(indP0):length(list_dataProps)]
    
    # Filter out non-numeric:
    isNum <- array(dim=length(list_dataProps))
    for(ii in 1:length(list_dataProps)){
      isNum[ii] <- is.numeric(data_PCA[,list_dataProps[ii]])
    }
    list_dataProps <- list_dataProps[isNum]
    
    # eliminate "median" (does not work since almost always)
    list_dataProps <- setdiff(list_dataProps,"median")
    
  }
  # list_dataProps = c(
  #   "ndata",
  #   "nfeature",
  #   "nsamples",
  #   "P0",
  #   "q95",
  #   "q99",
  #   "median_lib_size",
  #   "SD_lib_size",
  #   "CV_lib_size",
  #   "max_libSize",
  #   "min_libSize",
  #   "corr_libsize_P0sample",
  #   "bimodality_corr_feature",
  #   "bimodality_corr_sample" ,
  #   "mean_mean_log2cpm",
  #   "median_median_log2cpm",
  #   "mean_var_log2cpm",
  #   "mean_corr_sample",
  #   "mean_corr_feature",
  #   "SD_mean_log2cpm",
  #   "SD_median_log2cpm",
  #   "SD_var_log2cpm" ,
  #   "SD_corr_sample",
  #   "SD_corr_feature",
  #   "coef_poly2_varlog2cpm_meanlog2cpm_linear",
  #   "coef_poly2_varlog2cpm_meanlog2cpm_quadratic",
  #   "coef_poly1_meanlog2cpm_P0feature_slope",
  #   "coef_hclust_features",
  #   "coef_hclust_samples",
  #   "Permanova_R2"
  # ),
  # 
  
  # Get unique name indicating simulation type and corresponding template
  if(!"data_template" %in% names(data_PCA))
    stop("!data_template %in% names(data_PCA) => Check why!")
  
  data_PCA$data <-
    paste(data_PCA$name, data_PCA$data_template, sep = "_")
  rownames(data_PCA) <- NULL
  data_PCA <- column_to_rownames(data_PCA, var = "data")
  # Subset for data properties
  # data_PCA <-
  #   data_PCA[, colnames(data_PCA) %in% c(list_dataProps, "data_template", "sim_param")]
  data_PCA <- data_PCA[, sapply(colnames(data_PCA), function(cn) {
    any(sapply(list_dataProps, function(x) grepl(x, cn)))
  }) | colnames(data_PCA) %in% c("data_template", "sim_param")]
  
  # Scale data properties. Scale works on the columns therefore scaling each data property across data sets
  data_PCA[,!(colnames(data_PCA) %in% c("data_template", "sim_param"))] <-
    scale(data_PCA[,!(colnames(data_PCA) %in% c("data_template", "sim_param"))])
  
  
  # Add label for plot
  data_PCA$label <- data_PCA$data_template
  for (i in 1:nrow(data_PCA)) {
    if (data_PCA$sim_param[i] == "sim_default") {
      data_PCA$label[i] = ""
    }
    else
      (data_PCA$label[i] <- data_PCA$label[i])
  }
  
  # perform imputation, if required
  for(i in 1:ncol(data_PCA)){
    if(is.numeric(data_PCA[,i]) && sum(is.na(data_PCA[,i]))>0){
      data_PCA[,i] <- Hmisc::impute(data_PCA[,i]) # simple median imputation
    }
  }
  
  # Run PCA
  pca <-
    prcomp(na.omit(data_PCA[,!(colnames(data_PCA) %in% c("data_template", "sim_param", "label"))]), scale = FALSE)
  # Save pca object for further analysis
  if(!is.null(folder)){
    saveRDS(pca, paste0(folder,"/","pcaObject",filename,".RDS"))
  }
  
  # Color palettes for data sets (need 37)
  mypal <- c(
    pal_npg("nrc")(10),
    pal_nejm("default")(8),
    pal_tron("legacy")(7),
    pal_nejm("default")(8),
    pal_simpsons("springfield")(7)
  )
  #scales::show_col(mypal)
  
  if (type == "PCA") {
    # Plot PCA
    p=autoplot(pca,
             data = na.omit(data_PCA),
             x = 1,
             y = 2) +
      geom_point(aes(
        size = sim_param,
        color = data_template,
        shape = sim_param
      )) +
      scale_shape_manual(values = c(19, 15)) +
      scale_size_manual(values = c(2, 4)) +
      scale_colour_manual(values = mypal) +
      geom_text(
        aes(label = label),
        vjust = -1,
        hjust = 0.5,
        size = 3,
        fontface = "bold"
      ) +
      theme_bw() +
      theme(axis.text.x = element_text(size=14))+
      theme(axis.text.y = element_text(size=14))+
      theme(legend.text = element_text(size = 15), legend.title = element_text(size = 20)) +  # Adjust size as needed
      theme(axis.title.y = element_text(size = 18, face = "bold"))  + # Adjust size as needed
      theme(axis.title.x = element_text(size = 18, face = "bold"))   # Adjust size as needed
      #ggtitle(paste0(title)) 
      
    
    # Save plot
    if(!is.null(folder)){
      #save(folder,type,filename,p, pca, data_PCA, sim_param,data_template,  mypal, label, file=paste0(folder,"/",type,filename,".Rdata"))
      ggsave(paste0(folder,"/",type,filename,".pdf"),p, height=9, width=15)
    }
  }
  
  if (type == "Biplot") {
    # Plot Biplot
    p=autoplot(
      pca,
      data = na.omit(data_PCA),
      x = 1,
      y = 2,
      loadings = TRUE,
      loadings.colour = "grey",
      loadings.label = TRUE,
      loadings.label.colour = "darkred",
      loadings.label.size = 3
    ) +
      geom_point(aes(
        size = sim_param,
        color = data_template,
        shape = sim_param
      )) +
      scale_shape_manual(values = c(19, 15)) +
      scale_size_manual(values = c(2, 4)) +
      scale_colour_manual(values = mypal) +
      theme_bw() +
      ggtitle(paste0(title))
    
    #Save plot
    if(!is.null(folder)){
      save(p, folder,filename,type, pca, data_PCA, sim_param,data_template,  mypal, file=paste0(folder,"/PCA",filename,"_",type,".Rdata"))
      ggsave(paste0(folder,"/PCA",filename,"_",type,".pdf"),p,height=10, width=15)
    }
    # loadings <- p$data$loadings
    # sorted_loadings <- loadings[order(abs(loadings), decreasing = TRUE)]
    # top_loadings <- head(sorted_loadings, 5)
    # filtered_biplot <- biplot$data[biplot$data$loadings %in% top_loadings, ]
    # 
    # library(ggrepel)
    # p + geom_text_repel(data = filtered_biplot, aes(label = varname), size = 3)+
      
    
    
  }
  return(p)
}



