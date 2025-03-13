# bs_filerAim2Results(df_aim2res)
# 
#  This is a project-specific function, that removes all hypothesis, that are only
#  stated for one filter setting (filtered or unfiltered).
#
# Example:
# 
# df_aim2res <- bs_filerAim2Results(df_aim2res)

bs_filerAim2Results <- function(df_aim2res){
  ## Remove validations that are only in Nearing's paper only for filtered & unfiltered:
  cat(nrow(df_aim2res)," hypotheses before removing filtered/unfiltered...\n")
  raus <- array(FALSE,dim=nrow(df_aim2res))
  # Primary
  # H1
  raus <- raus | (df_aim2res$hypoNumber=="H101" & df_aim2res$filtered=="Filtered")
  raus <- raus | (df_aim2res$hypoNumber=="H101.1" & df_aim2res$filtered=="Filtered")
  cat(nrow(df_aim2res)-sum(raus)," hypotheses after removing filtered/unfiltered for H101 ...\n")
  # H2
  raus <- raus | (df_aim2res$hypoNumber=="H102" & df_aim2res$filtered=="Filtered")
  cat(nrow(df_aim2res)-sum(raus)," hypotheses after removing filtered/unfiltered for H102 ...\n")
  # H3
  raus <- raus | (df_aim2res$hypoNumber=="H103" & df_aim2res$filtered=="Filtered")
  cat(nrow(df_aim2res)-sum(raus)," hypotheses after removing filtered/unfiltered for H103 ...\n")
  # H4
  raus <- raus | (df_aim2res$hypoNumber=="H104" & df_aim2res$filtered=="Filtered")
  cat(nrow(df_aim2res)-sum(raus)," hypotheses after removing filtered/unfiltered for H104 ...\n")
  # H5
  raus <- raus | (df_aim2res$hypoNumber=="H105" & df_aim2res$filtered=="Filtered")
  raus <- raus | (df_aim2res$hypoNumber=="H105.1" & df_aim2res$filtered=="Filtered")
  cat(nrow(df_aim2res)-sum(raus)," hypotheses after removing filtered/unfiltered for H105 ...\n")
  # H6
  raus <- raus | (df_aim2res$hypoNumber=="H106" & df_aim2res$filtered=="Filtered")
  raus <- raus | (df_aim2res$hypoNumber=="H106.1" & df_aim2res$filtered=="Filtered")
  cat(nrow(df_aim2res)-sum(raus)," hypotheses after removing filtered/unfiltered for H106 ...\n")
  # H7
  raus <- raus | (df_aim2res$hypoNumber=="H107" & df_aim2res$filtered=="Filtered")
  raus <- raus | (df_aim2res$hypoNumber=="H107.1" & df_aim2res$filtered=="Filtered")
  raus <- raus | (df_aim2res$hypoNumber=="H107.2" & df_aim2res$filtered=="Filtered")
  cat(nrow(df_aim2res)-sum(raus)," hypotheses after removing filtered/unfiltered for H107 ...\n")
  # H8 requires both
  # H9 requires both
  # H10
  raus <- raus | (df_aim2res$hypoNumber=="H110" & df_aim2res$filtered=="Un-filtered")
  cat(nrow(df_aim2res)-sum(raus)," hypotheses after removing filtered/unfiltered for H110 ...\n")
  # H11 requires both
  # H12 requires both
  # H13
  raus <- raus | (df_aim2res$hypoNumber=="H113" & df_aim2res$filtered=="Un-filtered")
  raus <- raus | (df_aim2res$hypoNumber=="H113.1" & df_aim2res$filtered=="Un-filtered")
  cat(nrow(df_aim2res)-sum(raus)," hypotheses after removing filtered/unfiltered for H113 ...\n")
  
  # Secondary
  # H1 requires  both
  # H2 requires both
  # H3 requires both
  # H4
  raus <- raus | (df_aim2res$hypoNumber=="H204" & df_aim2res$filtered=="Filtered")
  cat(nrow(df_aim2res)-sum(raus)," hypotheses after removing filtered/unfiltered for H204 ...\n")
  # H5 
  raus <- raus | (df_aim2res$hypoNumber=="H205" & df_aim2res$filtered=="Filtered")
  cat(nrow(df_aim2res)-sum(raus)," hypotheses after removing filtered/unfiltered for H205 ...\n")
  # H6
  raus <- raus | (df_aim2res$hypoNumber=="H206" & df_aim2res$filtered=="Filtered")
  cat(nrow(df_aim2res)-sum(raus)," hypotheses after removing filtered/unfiltered for H206 ...\n")
  # H7
  raus <- raus | (df_aim2res$hypoNumber=="H207" & df_aim2res$filtered=="Filtered")
  cat(nrow(df_aim2res)-sum(raus)," hypotheses after removing filtered/unfiltered for H207 ...\n")
  # H8
  raus <- raus | (df_aim2res$hypoNumber=="H208" & df_aim2res$filtered=="Filtered")
  cat(nrow(df_aim2res)-sum(raus)," hypotheses after removing filtered/unfiltered for H208 ...\n")
  # H9
  raus <- raus | (df_aim2res$hypoNumber=="H209" & df_aim2res$filtered=="Filtered")
  cat(nrow(df_aim2res)-sum(raus)," hypotheses after removing filtered/unfiltered for H209 ...\n")
  # H10
#  raus <- raus | (df_aim2res$hypoNumber=="H210" & df_aim2res$filtered=="Filtered")
#  cat(nrow(df_aim2res)-sum(raus)," hypotheses after removing filtered/unfiltered for H210 ...\n")
  # H11
#  raus <- raus | (df_aim2res$hypoNumber=="H211" & df_aim2res$filtered=="Filtered")
#  raus <- raus | (df_aim2res$hypoNumber=="H211.1" & df_aim2res$filtered=="Filtered")
#  raus <- raus | (df_aim2res$hypoNumber=="H211.2" & df_aim2res$filtered=="Filtered")
#    raus <- raus | (df_aim2res$hypoNumber=="H211" & df_aim2res$filtered=="Un-filtered")
#    raus <- raus | (df_aim2res$hypoNumber=="H211.1" & df_aim2res$filtered=="Un-filtered")
#    raus <- raus | (df_aim2res$hypoNumber=="H211.2" & df_aim2res$filtered=="Un-filtered")
  cat(nrow(df_aim2res)-sum(raus)," hypotheses after removing filtered/unfiltered for H211 ...\n")
  # H12 requires both
  # H13 requires both
  # H14
  raus <- raus | (df_aim2res$hypoNumber=="H214" & df_aim2res$filtered=="Un-filtered")
  cat(nrow(df_aim2res)-sum(raus)," hypotheses after removing filtered/unfiltered for H214 ...\n")
  
  df_aim2res <- df_aim2res[!raus,,drop=F]
  ## End of "Remove validations that are only in Nearing's paper only for filtered & unfiltered"
  
  return(df_aim2res)
}
