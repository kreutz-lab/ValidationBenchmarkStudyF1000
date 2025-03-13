bs_plot_dataPropCmp_ck <- function(propCmpDF,cmp_type="log2ratio",
         data_props=unique(propCmpDF$data_prop), 
         exclude="",ncol=3,file=NULL,scales="fixed",
         grouping="template_and_sim"){
  
  if(!"difference" %in% names(propCmpDF))
    stop("use comparison data.frame as propCmpDF, i.e. $difference has to be available.")
  
  # Here I rename template_vs_sim => sim_minus_template 
  #  propCmpDF$cmp_type[propCmpDF$cmp_type=="template_vs_sim"] <- "sim_minus_template"
  
  # only plot the subset where cmp_type and data_prop matches the values provided as function arguements
  bol <- propCmpDF$cmp_type==cmp_type &
    propCmpDF$data_prop %in% data_props &
    !is.infinite(propCmpDF$difference) & !propCmpDF$data_prop %in% exclude
  
  DFplot = subset(propCmpDF,bol)
  if(grouping=="template_and_sim"){
    DFplot$comparison <- sub("_vs_"," - ",DFplot$comparison)
    DFplot$comparison <- sub("template - sim","sim - template",DFplot$comparison)
    DFplot$comparison <- factor(DFplot$comparison, levels=c("template - template", "sim - template","sim - sim"))
  }else if(grouping=="all"){
    DFplot$comparison <- gsub("metaSPARSim","mSS",as.character(DFplot$comparison)) # short labels
    DFplot$comparison <- gsub("metaSparSimNonReg","mSSnoReg",DFplot$comparison)
    DFplot$comparison <- gsub("original","orig",DFplot$comparison)
    DFplot$comparison <- gsub("__","_",DFplot$comparison)
    DFplot$comparison <- gsub("_$","",DFplot$comparison)
    DFplot$comparison <- factor(DFplot$comparison)
  }
  else
    stop("Grouping ",grouping, " not implemented.")
  
  # font size:
  nprop = length(unique(DFplot$data_prop))
  if(nprop<4)
    fs <- 24  #10
  else if(nprop<7)
    fs <- 25   #9
  else if(nprop<20)
    fs = 15 #8
  else
    fs = 15 #7
  
  if(length(unique(propCmpDF$data_template))>10){
    fs = round(fs/2)
    fstit = ceiling(fs/2)
  }
  else
    fstit = fs
  
  if(scales=="free_y")
    fsTick = fs
  else
    fsTick = fs
  
  xSegment <- sort(as.numeric(DFplot$comparison)+ 0.5)
  #xSegment <- xSegment[-length(xSegment)]
  
  # Change sign of difference, such that sim - template is plottet!
  DFplot$difference <- -DFplot$difference
  
  # Create a separate dataframe for vertical lines that matches the facet structure
  vertical_lines <- unique(DFplot[, c("data_prop", "comparison")])
  vertical_lines$xSegment <- as.numeric(vertical_lines$comparison) + 0.5
  
  plot.tmp <- ggplot(DFplot, aes(x=comparison,y=difference,color=data_template))+
    geom_hline(yintercept=0, color="black", linewidth=1)+
    geom_boxplot(outlier.size = 0.5, position = position_dodge(width = 0.85)) + 
    facet_wrap(.~data_prop,ncol=ncol,scales=scales) +
    # Use geom_vline with the new dataframe
    geom_vline(
      data = vertical_lines, 
      aes(xintercept = xSegment), 
      color = "gray", 
      linewidth = 2
    ) +
    theme_bw()+
    theme(strip.text = element_text(size = fs, face="bold")) +
    theme(axis.text.x = element_text(size = fsTick, face="bold",angle = 30, hjust = 1)) +
    theme(axis.text.y = element_text(size=fs))+
    theme(plot.title = element_text(size=fstit))+
    theme(panel.grid.major.x = element_blank(),   # Turn off major vertical grid lines
          panel.grid.minor.x = element_blank()) +  # Turn off minor vertical grid lines
    ylab(paste0("difference [",cmp_type,"]")) +
    theme(axis.title.y = element_text(size = 16, face = "bold"))  + # Adjust size as needed
    theme(axis.title.x = element_text(size = 16, face = "bold"))  + # Adjust size as needed
    guides(color = guide_legend(ncol = 1)) +
    theme(legend.text = element_text(size = 11), legend.title = element_text(size = 14))+  # Adjust size as needed
    scale_x_discrete(expand = expansion(mult = c(0.25, -0.02)))  # Start at left edge
  
  
  
  if(!is.null(file)){
    save(plot.tmp, DFplot, file, file=paste0(file,".Rdata"))
    pdf(file=file,width = 12,height=16)
    print(plot.tmp)
    dev.off()
    #  ggsave(paste0(file,"datPropCmp",scales,".pdf")) # klappt nicht bzw. sieht übel aus
    
  }
  return(plot.tmp)
}
