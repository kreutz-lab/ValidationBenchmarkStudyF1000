# bs_validationOutput(H_true, threshold, hypothesis, hypoNr, prefix, test="")
#   
# This function creates the validation output if counting is applied as a data.frame
#
# Example:
# aim2_results <- bs_validationOutput(H_true, threshold=0.95, prefix=prefix, 
#                                     hypoNr=10,
#                                     hypothesis="In unfiltered data, either ALDEx2 or ANCOM-II identify the fewest significant features.",
#                                     test="ALDEx2 or ANCOM-II")
# 
# 
# prefix            test         propTrue      outcome outcome_value threshold  validated   hypoNumber    hypothesis
# 1 5.3.1_Aim2_exp_ ALDEx2 or ANCOM-II       NaN   CI.LB     0.0000000      0.95     FALSE     10   For filtered data, ALDEx2 and ANCOM-II identify more features that were also identified by all except 3 (i.e. 10 out of 13) other methods.

bs_validationOutput <- function(H_true, threshold, hypothesis, hypoNr, prefix, test=""){

  if(is.matrix(H_true) && is.null(colnames(H_true)))
    step("bs_validationOutput.R: H_true requires colnames if used as matrix.")
    
  if(is.matrix(H_true) && ncol(H_true)>1){ # multiple inputs, call this function for each column
    for(i in 1:ncol(H_true)){
      cn <- colnames(H_true)
      if(is.null(cn))
        stop("If a matrix H_true is provided, colnames are required.")
      df_tmp <- bs_validationOutput(H_true=H_true[,i], threshold=threshold, hypothesis=hypothesis, 
                                    hypoNr=hypoNr+(i-1)*0.1, prefix=prefix, test=cn[i])
      if(i==1)
        df_out <- df_tmp
      else
        df_out <- rbind(df_out,df_tmp)
    }
  }
  else{
    
    res <- DescTools::BinomCI(sum(H_true,na.rm=T), sum(!is.na(H_true)), conf.level = 0.95, method = "clopper-pearson")
    df_out <- data.frame(prefix=prefix, 
                         test=test,
                         propTrue = res[1], 
                         outcome="CI.LB",
                         outcome_value=NA,
                         threshold=threshold,
                         validated=NA,  # filled later
                         hypoNumber=hypoNr, 
                         hypothesis=hypothesis)
    
    if(threshold>=0.5 && sum(!is.na(H_true))>20)
      df_out$outcome_value=res[2]
    if(threshold<0.5 && sum(!is.na(H_true))>20)
        df_out$outcome_value=res[2]
    # otherwise 95%CI not feasible
    
    testLargerThreshold <- df_out[,"threshold"]>=0.5 | df_out[,"threshold"]==0
    df_out[testLargerThreshold,"validated"] <- df_out[testLargerThreshold,"outcome_value"] >= df_out[testLargerThreshold,"threshold"]
    
    testSmallerThreshold <- df_out[,"threshold"]<0.5 & df_out[,"threshold"]>0
    df_out[testSmallerThreshold,"validated"] <- df_out[testSmallerThreshold,"outcome_value"] < df_out[testSmallerThreshold,"threshold"]
    
    bs_validationOutput_print(df_out,res)
    
  }
  
  return(df_out)
}

# bs_validationOutput_print(df_out,res)
#   
# df_out   A data.frame, as e.g. produced by bs_validateOutput()
# Looks like:
# prefix                        test  propTrue outcome outcome_value threshold validated hypoNumber
# 1 5.3.1_Aim2_exp_            both limma vooms 0.0000000   CI.LB     0.0000000      0.95     FALSE          1
# 2 5.3.1_Aim2_exp_ both limma vooms & wilcoxon 0.2941176   CI.LB     0.1509837      0.95     FALSE          2
# 3 5.3.1_Aim2_exp_ both limma vooms & wilcoxon 0.0000000   CI.LB     0.0000000      0.95     FALSE          3
# 4 5.3.1_Aim2_exp_ both limma vooms & wilcoxon       NaN   CI.LB     0.0000000      0.95     FALSE          4
# 
# res 
# The result in any form. It is printed via print(res)
# Can be a string, ANOVA output, etc.
bs_validationOutput_print <- function(df_out,res=""){
  cat("\n")
  cat(paste0("\n####  Hypothesis ",df_out$hypoNumber,":\n"))
  wrapped_string <- strwrap(df_out$hypothesis, width = 100)
  cat(paste(wrapped_string, collapse = "\n"))
  cat("\n")
  
  cat(paste0("\nResult for hypothesis ",df_out$hypoNumber," (",df_out$threshold," required for ",df_out$outcome," to validate Nearing et al.):\n"))
  cat(paste0(df_out$outcome,"=",df_out$outcome_value,"\n\n"))
  cat("In more detail: \n")
  print(res)
  if(!is.na(df_out$validated) && df_out$validated)
    cat("\nHypothesis",df_out$hypoNumber,"Validation SUCCEEDED!\n")
  if(is.na(df_out$validated))
    cat("\nHypothesis",df_out$hypoNumber,"could NOT be evaluated (NA)!\n")
  if(!is.na(df_out$validated) && !df_out$validated)
    cat("\nHypothesis",df_out$hypoNumber,": Validation FAILED.\n")
}