# This function renames our data templates as naming in Nearing et al.
#
bs_renameDataAccordingToNearing <- function(d_or_df,colName=NULL){
  xls <- openxlsx::read.xlsx("../Data/SampleAnnotation.xlsx")
  namesNearing <- xls$Dataset
  names(namesNearing) <- xls$data_template # our names
  namesNearing <- namesNearing[!is.na(names(namesNearing))]
  
  if(is.data.frame(d_or_df)){
    df <- d_or_df
    if(is.null(colName)){
      stop("When bs_renameDataAccordingToNearing is called with a data.frame, a colName has to be specified.")
    }
    if(nrow(df)>1e5)
      cat("Renaming a long data.frame is time consuming, try to make it earlier...\n")
    for(i in 1:length(namesNearing)){
      cat(".")
      ind <- grep(names(namesNearing)[i],df[[colName]])
      df[[colName]][ind] <- namesNearing[i]
    }
    cat("\n")
    out <- df
  }else{ # no data.frame, i.e. something with names, e.g. data_to_compare
    d <- d_or_df
    namen <- names(d)
    
    for(i in 1:length(namesNearing)){
      ind <- grep(names(namesNearing)[i],namen)
      namen[ind] <- namesNearing[i]
    }
    # for(i in 1:length(namen)){
    #   if(length(grep(namen[i], names(namesNearing)){
    #     namen[i] <- namesNearing[namen[i]]
    #   }
    # }
    names(d) <- namen
    out <- d
  }
  return(out)
}