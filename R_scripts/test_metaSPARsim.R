tmp <- d$Office$original

raw_data <- tmp$counts
norm_data <- tmp$counts.norm
meta <- tmp$meta

conditions <- split(seq_along(meta$condition), meta$condition)

drin <- colSums(raw_data[,conditions[[1]]])>100 & colSums(raw_data[,conditions[[2]]])>100
raw_data <- raw_data[,drin]
norm_data <- norm_data[,drin]
meta <- meta[drin,,drop=F]
conditions <- split(seq_along(meta$condition), meta$condition)

drin <- rowSums(raw_data[,conditions[[1]]])>0 & rowSums(raw_data[,conditions[[2]]])>0
raw_data <- raw_data[drin,]
norm_data <- norm_data[drin,]


params <- estimate_parameter_from_data(raw_data=raw_data, norm_data=norm_data, 
                                       conditions=conditions, intensity_func = "mean", 
                                       keep_zeros = TRUE, perc_not_zeros=0)

sim_data <- metaSPARSim(params) #D$original
