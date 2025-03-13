# Functions to calculate a set of microbiome specific data properties for one data frame 
# Returns named list

#library(edgeR)
#library(BimodalIndex)
#library(amap)
#library(cluster)
#library(vegan) # for alpha and beta diversities

#library(data.table) # Für rbindlist


# Function to calculate data properties and save them in a named list
msb.bs_calcPropMatrix <- function(dat, meta.dat = NULL, file=NULL){
  
  # Helper function to calculate feature wise spearman correlation.
  # For more than nmaxFeature feature a subset of nmaxFeatu random features is selected to speed up runtime.
  ### !Ändern, dass zB Features mit größter Varianz gewählte werden!
  # method "spearman" (default), "pearson", "correlation" (=centered pearson)
  calc_feature_corr <- function(count.data,nmaxFeature=100,method="spearman"){
    # Transpose input data to calculate correlation between features
    # count.data_t <- t(count.data)
    # Take subset of data, when data set too big
    if(nrow(count.data)>nmaxFeature) # random subset of features are selected
      count.data <- count.data[sample(1:nrow(count.data),nmaxFeature),]
    dis <- cor(t(count.data),t(count.data),method = method,use="na.or.complete")
  }
  
  calc_pilou <- function(count.data){
    # Calculate the Shannon diversity index
    shannon_diversity <- vegan::diversity(count.data, index = "shannon")
    # Calculate species richness
    species_richness <- apply(count.data, 1, function(x) sum(x > 0,na.rm = T))
    # Calculate Pielou's Evenness
    pielou_evenness <- shannon_diversity / log(species_richness)
    return(pielou_evenness)
  }
  
  invSimpFun <- function(dat){
    invSimp <- vegan::diversity(dat, index = "invsimpson")
    invSimp[is.infinite(invSimp)] <- NA
    return(mean(invSimp,na.rm=T))
  }
  
  
  # Helper function to perform centred log-ratio transformation for PERMANOVA effect size.
  pseudocount_and_clr <- function(in_df, pseudocount = 1) {
    # Adds pseudocount to all samples and applies centred log-ratio transformation.
    # Note that this functions assumes that columns are samples and rows are features.
    return(as.data.frame(apply(in_df + pseudocount,2,function(x){log(x) - mean(log(x))})))
  }
  
  dat <- as.matrix(dat)
  
  # Filter out samples that are completly 0
  dat <- dat[,!colSums(abs(dat))==0]
  dat.cpm <- edgeR::cpm(dat, log=TRUE, prior.count = 1)
  if(!is.null(file))
    cat("edgeR::cpm done.\n",file=file,append=T)
  data.prop <- list(
    # Fraction of zeros for entire data set
    "P0" = sum(dat==0,na.rm = T)/length(dat),
    
    # Fraction of zeros per feature
    "P0_feature" = apply(dat==0,1,sum)/ncol(dat),
    
    # Fraction of zeros per sample
    "P0_sample" = apply(dat==0,2,sum)/nrow(dat),
    
    # Median of entire data set.
    "median" = median(dat,na.rm=TRUE),
    
    
    # Quantiles of entire data set
    "q95" = quantile(dat,probs=.95),
    "q99" = quantile(dat,probs=.99),
    
    # Log2 count mean intensity (feature wise)
    "mean_log2cpm" = apply(dat.cpm, 1,mean,na.rm=T),
    "median_log2cpm" = apply(dat.cpm,1, median,na.rm=T),
    
    # Variance of log2 counts
    "var_log2cpm" = apply(dat.cpm, 1, var),
    
    # Library size
    "lib_size" = colSums(dat),
    
    
    # Median / Mean / Max / Min library size
    "median_lib_size" = median(colSums(dat),na.rm = T),
    "mean_lib_size" = mean(colSums(dat),na.rm = T),
    "max_libSize" = max(colSums(dat),na.rm = T),
    "min_libSize" = min(colSums(dat),na.rm = T),
    
    
    # SD and CV of library size
    "SD_lib_size" = sd(colSums(dat),na.rm = T),
    "CV_lib_size"= sd(colSums(dat),na.rm = T)/mean(colSums(dat),na.rm = T) ,
    
    # Read depth range between samples
    "range_lib_size" = diff(range(colSums(dat),na.rm = T)),
    
    # Sample means (mean of each column)
    "sample_means" = apply(dat,2,mean),
    
    # Mean sample ASV richness (How many features >0 per sample)
    "mean_sample_richness" = mean(colSums(dat>0), na.rm=T),
    
    
    # Number of features
    "nfeature" = sum(rowSums(dat,na.rm=T)>0),
    #    "nfeature" = nrow(dat),
    
    "nsamples" = ncol(dat),
    # Number of samples
    #  "nsamplesWithCounts" = sum(colSums(dat,na.rm=T)),
    
    # Sample wise correlation
    "corr_sample" = cor(dat, dat, method="spearman",use="na.or.complete"),
    
    # Feature wise correlation
    "corr_feature" = calc_feature_corr(dat),
    
    # Calculate the Inverse Simpson diversity index
    "mean_inverseSimpson" = invSimpFun(dat),
    
    # Calculate the Inverse Simpson diversity index
    "mean_pilou" = mean(calc_pilou(dat),na.rm=T),
    
    # mean Bray-Curtis dissimilarity
    "mean_brayCurtis" = mean(vegan::vegdist(dat, method = "bray"),na.rm=T)
  )

  if(!is.null(file))
    cat("data prop list calculations done.\n",file=file,append=T)
  
  # Sample wise correlation: library size with proportion of zeros
  data.prop[["corr_libsize_P0sample"]] = cor(data.prop$P0_sample, data.prop$lib_size, method="spearman",use="na.or.complete")
  
  # SD of features with smallest means relative to others
  q10 <- quantile(data.prop[["mean_log2cpm"]],probs = 0.1)
  small <- data.prop[["mean_log2cpm"]] < q10 # 10% features with smalles rowMean
  data.prop[["SD_smallRowMeans"]] = mean(data.prop[["var_log2cpm"]][small],na.rm=T)/mean(data.prop[["var_log2cpm"]][!small],na.rm=T)
  
  # Bimodality of sample- & feature-correlations:
  data.prop[["bimodality_corr_feature"]] = NA
  data.prop[["bimodality_corr_sample"]]  = NA
  
  corrs <- data.prop$corr_feature
  try(data.prop[["bimodality_corr_feature"]] <- bimodalIndex(matrix(corrs[!is.na(corrs)] ,nrow=1),verbose=F)$BI)
  
  if(!is.null(file))
    cat("bimodalIndex done.\n",file=file,append=T)

  cat("bimodality_corr_feature = ",data.prop[["bimodality_corr_feature"]],"\n")
  
  corrs <- data.prop$corr_sample
  try(data.prop[["bimodality_corr_sample"]]  <- bimodalIndex(matrix(corrs[!is.na(corrs)],nrow=1),verbose=F)$BI)
  
  
  # Mean of all feature means
  data.prop[["mean_mean_log2cpm"]] = mean(data.prop$mean_log2cpm,na.rm=T)
  # SD of feature means
  data.prop[["SD_mean_log2cpm"]] = sd(data.prop$mean_log2cpm,na.rm=T)
  
  # Median of all feature medians
  data.prop[["median_median_log2cpm"]] = median(data.prop$median_log2cpm,na.rm=T)
  # SD of feature medians
  data.prop[["SD_median_log2cpm"]] = sd(data.prop$median_log2cpm,na.rm=T)
  
  # Mean of all feature variances
  data.prop[["mean_var_log2cpm"]] = mean(data.prop$var_log2cpm,na.rm=T)
  # SD of feature variances
  data.prop[["SD_var_log2cpm"]] = sd(data.prop$var_log2cpm,na.rm=T)
  
  # Mean of all sample means
  data.prop[["mean_sample_means"]] = mean(data.prop$sample_means,na.rm=T)
  # SD of sample means
  data.prop[["SD_mean_sample_means"]] = sd(data.prop$sample_means,na.rm=T)
  
  # Mean of sample correlation matrix
  data.prop[["mean_corr_sample"]] = mean(data.prop$corr_sample,na.rm=T)
  # SD of sample correlation matrix
  data.prop[["SD_corr_sample"]] = sd(data.prop$corr_sample,na.rm=T)
  # Mean of feature correlation matrix
  data.prop[["mean_corr_feature"]] = mean(data.prop$corr_feature,na.rm=T)
  # SD of feature correlation matrix
  data.prop[["SD_corr_feature"]] = sd(data.prop$corr_feature,na.rm=T)
  
  if(!is.null(file))
    cat("mean and sd calculations done.\n",file=file,append=T)
  
  
  # Quantify mean variance relationship
  # 2nd order polynomial fit of feature mean-variance plot (mean_log2cpm vs variance_log2cpm)
  res <- lm(y~x+I(x^2),data=data.frame(y=data.prop$var_log2cpm,x=data.prop$mean_log2cpm))
  # Linear component
  data.prop[["coef_poly2_varlog2cpm_meanlog2cpm_linear"]] <- NA
  try(data.prop[["coef_poly2_varlog2cpm_meanlog2cpm_linear"]] <- res$coefficients[2])
  # Quadratic component
  data.prop[["coef_poly2_varlog2cpm_meanlog2cpm_quadratic"]] <- NA
  try(data.prop[["coef_poly2_varlog2cpm_meanlog2cpm_quadratic"]] <- res$coefficients[3])
  # plot(data.prop$mean_log2cpm,data.prop$var_log2cpm)
  # points(data.prop$mean_log2cpm,predict(res,data.frame(x=data.prop$mean_log2cpm)),col="red")
  
  # Quantify realtionship of feature sparsity with feature mean expression values
  # Slope in P0_feature vs feature mean plot (P0_feature vs mean_log2cpm)
  res <- lm(y~slope,data=data.frame(slope=data.prop$P0_feature-1,y=data.prop$mean_log2cpm))
  data.prop[["coef_poly1_meanlog2cpm_P0feature_slope"]] <- NA
  try(data.prop[["coef_poly1_meanlog2cpm_P0feature_slope"]] <- res$coefficients[2])
  # plot(data.prop$P0_feature,data.prop$mean_log2cpm)
  # points(data.prop$P0_feature,predict(res,data.frame(slope=data.prop$P0_feature-1)),col="red")
  
  if(!is.null(file))
    cat("lm's done.\n",file=file,append=T)
  
  # Quantify how strong data clusters
  ## coef.hclust for features (TOP500 according to !is.na) and samples
  dat.tmp <- dat[order(rowSums(is.na(dat))), ]
  dat.tmp <- dat.tmp[1:min(500,dim(dat)[1]),1:min(500,dim(dat)[2])] # max. 500 features and samples
  data.prop[["coef_hclust_features"]] <- NA
  try(data.prop[["coef_hclust_features"]] <- coef.hclust(hcluster(dat.tmp)))
  data.prop[["coef_hclust_samples"]] <- NA
  try(data.prop[["coef_hclust_samples"]] <- coef.hclust(hcluster(t(dat.tmp))))
  
  if(!is.null(file))
    cat("hclust done.\n",file=file,append=T)
  
  # PERMANOVA effect size
  data.prop[["Permanova_R2"]] <- NA
  data.prop[["Permanova_P-value"]] <- NA
  try(
    if(!is.null(meta.dat)){
      aitchison <- parallelDist::parallelDist(x=t(pseudocount_and_clr(dat)), method="euclidean", threads=30)
      aitchison_formula <- as.formula(aitchison ~ meta.dat[,1])
      permanova <- data.frame(vegan::adonis2(formula = aitchison_formula, permutations=999))
      data.prop[["Permanova_R2"]] <- as.numeric(permanova[1,"R2"])
      data.prop[["Permanova_P-value"]] <- as.numeric(permanova[1,"Pr..F."])
    }else{
      warning("is.null(meta.dat): You might have to call bs_annotateObject first.")
    }
  )
  
  if(!is.null(file))
    cat("PERMANOVA done, and now finished for one data set.\n",file=file,append=T)
  
  
  attr(data.prop,"bstype") <- "data_prop"
  data.prop
}




