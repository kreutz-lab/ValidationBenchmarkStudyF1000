# This function calculates rank correlation of the mismatch between H_true and data characteristics
# between exp and sim.
#
# Input are two names of workspaces that are produced by bs_PropsAssociationsWithHtrue that is
# called in script_5.x_Aim2*
#
#
# Example:
# df_Htrue_exp_file <- "../Results/5.2.2_Aim2_secondary_metaSPARSim_ZerosAdded_exp/df_H_true_all.RDS"
# df_Htrue_sim_file <- "../Results/5.2.2_Aim2_secondary_metaSPARSim_ZerosAdded_simEquiv/df_H_true_all.RDS"
# res <- bs_correlateMismatch(df_Htrue_exp_file,df_Htrue_sim_file)

bs_correlateMismatch <- function(df_Htrue_exp_file,df_Htrue_sim_file){
  cat("bs_correlateMismatch(",df_Htrue_exp_file,",",df_Htrue_sim_file,") ...\n")
  
  df0 <- readRDS(df_Htrue_exp_file)
  df  <- readRDS(df_Htrue_sim_file)
  
  
  df0$dataSet <- as.character(df0$dataSet)
  df$dataSet <- as.character(df$dataSet)
  
  datasets <- unique(df0$dataSet)
  
  chars <- setdiff(colnames(df0),c("H_true","dataSet","data_name","nameHypo","ALDEx2","ancombc","corncob"))
  chars <- chars[grep("_rank$",chars,invert = T)]
  
  hypos <- setdiff(unique(df0$nameHypo),NA)
  
  #
  dfCorrs <- NULL
  ii<- 0
  vals <- list()
  for(hypo in hypos){
    char_diff <- list()
    htrue_diff <- list()
    
    for(char in chars){
      
      for(i in 1:length(datasets)){
        dataset <- datasets[i]
        
        ind0 <- which(df0$dataSet==dataset & df0$nameHypo==hypo)
        ind <- which(df$dataSet==dataset & df$nameHypo==hypo)
        
        htrue_diff[[i]] <- as.numeric(df[ind,"H_true"] - rep(df0[ind0,"H_true"],times=length(ind)))
        char_diff[[i]] <- as.numeric(df[ind,char] - rep(df0[ind0,char],times=length(ind)))
        
      }
      ii <- ii+1

      if(length(unlist(htrue_diff))==length(unlist(char_diff))){
      vals[[ii]] <- data.frame(h_true_diff=unlist(htrue_diff),DC_diff=unlist(char_diff), DC_diff_ranked = rank(unlist(char_diff)))
      if(sum(!is.na(vals[[ii]]$DC_diff))>0 && sd(vals[[ii]]$h_true, na.rm=T)>0 && sd(vals[[ii]]$DC_diff,na.rm=T)>0)
        rangCor <- cor(vals[[ii]]$h_true,vals[[ii]]$DC_diff,method="spearman",use="na.or.complete")
      else
        rangCor <- NA
      }else
        rangCor <- NA
      #print(rangCor)
      tmp <- data.frame(hypothesis=hypo,DC=char,rankCor=rangCor)
      if(!is.null(dfCorrs))
        dfCorrs <- rbind(dfCorrs,tmp)
      else
        dfCorrs <- tmp
    }
  }
  
  create_plots <- function(df,titel,ylabel){
    # Assuming df is your existing data frame
    df_long <- df %>% 
      tidyr::pivot_longer(
        cols = c("DC_diff", "DC_diff_ranked"),
        names_to = "Variable",
        values_to = "Value"
      )
    
    library(ggplot2)
    p <- ggplot(df_long, aes(x = factor(h_true_diff), y = Value)) +
      geom_violin(fill = "#69b3a2", adjust = 1) +  # adjust for smoothing
      geom_boxplot(width = 0.1, fill = "white", color = "black", outlier.shape = NA) +
      facet_wrap(~ Variable, scales = "free_y") +  # separate facets for each variable
      labs(title = titel,
           x = "H_true sim - H_true exp",
           y = ylabel) +
      theme_minimal()
    
    print(p)
  }
  
  # eliminate NA
  dfCorrs <- dfCorrs[!is.na(dfCorrs$rankCor),]
  
  rf1 <- order(dfCorrs$rankCor)
  rf2 <- order(dfCorrs$rankCor,decreasing = T)

  try(file.remove(sub("df_H_true_all.RDS","correlateMismatch.pdf",df_Htrue_sim_file)))
  save(vals, rf1, rf2, dfCorrs, file=sub("df_H_true_all.RDS","correlateMismatch.Rdata",df_Htrue_sim_file))
  pdf(file=sub("df_H_true_all.RDS","correlateMismatch.pdf",df_Htrue_sim_file), width = 10, height = 5)
  for(i in 1:5){
    df <- vals[[rf1[i]]]
    if(!is.null(df))
      create_plots(df,titel=paste0("rankcor(",dfCorrs$DC[rf2[i]], "_diff,",dfCorrs$hypothesis[rf2[i]],"_true_diff)=",dfCorrs$rankCor[rf2[i]]),
                 ylabel = paste0(dfCorrs$DC[rf2[i]], " sim - ", dfCorrs$DC[rf2[i]], "exp"))
  }
  for(i in 1:5){
    df <- vals[[rf2[i]]]
    if(!is.null(df))
      create_plots(df,titel=paste0("rankcor(",dfCorrs$DC[rf2[i]], "_diff,",dfCorrs$hypothesis[rf2[i]],"_true_diff)=",dfCorrs$rankCor[rf2[i]]),
                 ylabel = paste0(dfCorrs$DC[rf2[i]], " sim - ", dfCorrs$DC[rf2[i]], "exp"))
  }
  dev.off()
  
  
  p2 <- ggplot(dfCorrs, aes(x = hypothesis, y = DC, fill = rankCor)) +
    geom_tile(color = "white", size = 0.5) +  # Add white borders for better tile distinction
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0,  # Center the gradient at zero
                         na.value = "grey",  # Color for NA values
                         limits = c(-1, 1),  # Assuming the values range from -1 to 1
                         breaks = c(-1, 0, 1), 
                         labels = c("-1", "0", "1")) +
    labs(title = "RankCor(DC_mismatch,Hypothesis_mismatch)", x = "Hypothesis", y = "DC") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Tilt x labels for better visibility
  
  save(p2, dfCorrs, file=sub("df_H_true_all.RDS","correlateMismatch.Rdata",df_Htrue_sim_file))
  pdf(file=sub("df_H_true_all.RDS","correlateMismatch_Heatmap.pdf",df_Htrue_sim_file), width = 5, height = 10)
  print(p2)
  dev.off()
  
  
  return(list(dfCorrs=dfCorrs,vals=vals))
}