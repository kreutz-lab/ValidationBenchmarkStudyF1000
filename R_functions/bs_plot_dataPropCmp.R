# Function to make boxplots for comparisons of data properties
# Input: propCmpDF (data.frame) 
# 
# @param grouping, default: "template_and_sim" only original vs sim, sim vs. sim
#        "all": All simulations are plotted individually
#
#
# # Example:
# d <- readRDS("data_to_compare.RDS")
# d <- bs_addDataProp(d)
# d <- bs_diffDataProp(d)
# DF <- bs_summarize_dataPropCmp(d)
#
# bs_plot_dataPropCmp(DF)  # all DF$data_prop
#
# # all DF$data_prop, individual y-axis :
# bs_plot_dataPropCmp(DF,scales="free_y")
#
# # exclude data_prop with largest deviations to see more for others:
# bs_plot_dataPropCmp(DF,exclude="bimodality_corr_sample")
#
# # only a single property
# bs_plot_dataPropCmp(DF,data_props="bimodality_corr_sample")
#
# # Grouping/comparing all simulation methods (instead of sim vs. template)
# bs_plot_dataPropCmp(DF,data_props="bimodality_corr_sample",grouping="all")


bs_plot_dataPropCmp <- function(propCmpDF,cmp_type="log2ratio",
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
    DFplot$comparison <- sub("template - sim","template - simus",DFplot$comparison)
    DFplot$comparison <- sub("sim_vs_sim","simus - sim",DFplot$comparison)
    DFplot$comparison <- sub("template_vs_template","templates - template",DFplot$comparison)
    DFplot$comparison <- factor(DFplot$comparison, levels=c("templates - template", "template - simus","simus - sim"))
  }else if(grouping=="all"){
    DFplot$comparison <- gsub("metaSPARSim","mSS",as.character(DFplot$comparison)) # short labels
    DFplot$comparison <- gsub("metaSparSimNonReg","mSSnoReg",DFplot$comparison)
    DFplot$comparison <- gsub("original","orig",DFplot$comparison)
    DFplot$comparison <- gsub("__","_",DFplot$comparison)
    DFplot$comparison <- gsub("_$","",DFplot$comparison)
    DFplot$comparison <- factor(DFplot$comparison)
  }else
    stop("Grouping ",grouping, " not implemented.")
  
  # font size:
  nprop = length(unique(DFplot$data_prop))
  if(nprop<4)
    fs <- 20  #10
  else if(nprop<7)
    fs <- 18   #9
  else if(nprop<20)
    fs = 15 #8
  else
    fs = 15 #7
  
  if(length(unique(propCmpDF$data_project))>10){
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
  
  plot.tmp <- ggplot(DFplot, aes(x=comparison,y=difference,color=data_project))+
    geom_hline(yintercept=0, color="darkred")+
    geom_boxplot(outlier.size = 0.5) + 
    geom_segment(aes(x =  xSegment , xend = xSegment , 
                     y = -Inf, yend = Inf), color = "gray", linewidth=1) +
    facet_wrap(.~data_prop,ncol=ncol,scales=scales) +
    theme_bw()+
    theme(strip.text = element_text(size = fs, face="bold")) +
    theme(axis.text.x = element_text(size = fsTick,face="bold",angle = 30, hjust = 1)) +
    # theme(axis.text.x = element_text(size = fsTick-1,angle=45,hjust=0.95,vjust=0.98)) +
    theme(axis.text.y = element_text(size=fs))+
    theme(plot.title = element_text(size=fstit))+
    theme(panel.grid.major.x = element_blank(),   # Turn off major vertical grid lines
          panel.grid.minor.x = element_blank()) +  # Turn off minor vertical grid lines
    ylab(paste0("difference [",cmp_type,"]")) +
    scale_x_discrete(expand = expansion(mult = c(0.25, -0.02))) + # Start at left edge
    guides(color = guide_legend(ncol = 1))
  
  if(!is.null(file)){
    save(plot.tmp, DFplot, file, file=paste0(file,".Rdata"))
    pdf(file=file,width = 12,height=16)
    print(plot.tmp)
    dev.off()
    #  ggsave(paste0(file,"datPropCmp",scales,".pdf")) # klappt nicht bzw. sieht übel aus
    
  }
  return(plot.tmp)
}

