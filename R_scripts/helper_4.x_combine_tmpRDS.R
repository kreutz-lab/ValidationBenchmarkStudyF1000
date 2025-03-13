d1 <- readRDS("data_to_compare_tmp.RDS")
d2 <- readRDS("data_to_compare_tmp2.RDS")

##
d1b <- bs_subset(d1,which(names(d1)!="NotFound"))

d1b <- bs_decompress(d1b)
d2 <- bs_decompress(d2)

d <- c(d1b,d2)
attr(d,"bstype") <- "data_list"

d <- bs_compress(d)
saveRDS(d,"data_to_compare.RDS")
