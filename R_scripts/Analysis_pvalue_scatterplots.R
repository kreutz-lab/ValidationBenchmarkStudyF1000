################################
# Set correct working directory
sys_info <- Sys.info()

if(sys_info[["sysname"]]=="Darwin" && sys_info[["login"]]=="root"){
  setwd("/Users/evakohnert/Documents/PhD/Microbiome/Benchmark/BenchmarkStudy_MicrobiomeSyntheticData/R_scripts")
}
if(sys_info[["sysname"]]=="Windows" && grep("kreutz",sys_info[["login"]],T)){
  setwd("E:/clemens/Repositories/BenchmarkStudy_MicrobiomeSyntheticData/R_scripts")
}
if(sys_info[["sysname"]]=="Linux" && sys_info[["login"]]=="kohnert"){
  setwd("/h/kohnert/BenchmarkStudy_MicrobiomeSyntheticData/R_scripts")
}
if(sys_info[["sysname"]]=="Linux" && sys_info[["login"]]=="ckreutz"){
  if(sys_info[["nodename"]]=="imbip-compute-214")
    setwd("~/BenchmarkStudy_MicrobiomeSyntheticData_25Mar24/R_scripts")
  else
    setwd("~/BenchmarkStudy_MicrobiomeSyntheticData/R_scripts")
}


# Initialization, i.e. sourcing R-functions, loading packages and setting project_path
source("project_init.R")
# project_init()
###############################
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(cowplot)

# Function to create pairs plots for p-values by project
create_pvalue_comparison_plots <- function(data, project_name = NULL) {
  # 
  datasets <- unique(data$dataset)
  if(length(datasets)>1){
    cat("Only plot the last dataset: ",datasets[length(datasets)],"\n")
    data <- data %>% filter(dataset == datasets[length(datasets)])
  }
  
  # Filter by project if specified
  if (!is.null(project_name)) {
    data <- data %>% filter(project == project_name)
    cat("Only project:",project_name,"\n")
  }
  
  # Get all unique tests
  tests <- unique(data$test)
  n_tests <- length(tests)
  
  # If there's only one test, we can't make comparison plots
  if (n_tests <= 1) {
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, 
                      label = paste("Project", project_name, "has only one test:", tests)) + 
             theme_void())
  }
  
  # Create all pairwise combinations of tests
  test_pairs <- expand.grid(test1 = tests, test2 = tests, stringsAsFactors = FALSE) %>%
    filter(test1 != test2) %>%
    # Remove duplicates (A vs B is the same as B vs A for our purposes)
    mutate(pair = pmap_chr(list(test1, test2), ~paste(sort(c(..1, ..2)), collapse = "_"))) %>%
    distinct(pair, .keep_all = TRUE)
  
  # Create a list to store plots
  plot_list <- list()
  
  
  # For each test pair, create a scatterplot
  for (i in 1:nrow(test_pairs)) {
    test1 <- test_pairs$test1[i]
    test2 <- test_pairs$test2[i]
    
    # Prepare data for this pair
    pair_data <- data %>%
      select(feature, test, p_value) %>%
      pivot_wider(names_from = test, values_from = p_value) %>%
      filter(!is.na(!!sym(test1)), !is.na(!!sym(test2)))
    
    if(nrow(pair_data)>500)
      pair_data <- pair_data[round(seq(1,nrow(pair_data),length.out=500)),]
    
    # If there are no matching features between tests, skip this pair
    if (nrow(pair_data) == 0) next
    
    # Create the plot
    p <- ggplot(pair_data, aes(x = !!sym(test1), y = !!sym(test2))) +
      geom_point(alpha = 0.7) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey70") +
      labs(
        x = paste(test1, "p-value"),
        y = paste(test2, "p-value"),
        title = paste(test1, "vs", test2)
      ) +
      theme_bw() +
      theme(plot.title = element_text(size = 10),
            axis.title = element_text(size = 7),
            axis.title.x = element_text(size=10))
    
    plot_list[[length(plot_list) + 1]] <- p
  }
  
  # If no plots were created, return an empty plot with a message
  if (length(plot_list) == 0) {
    return(ggplot() + 
             annotate("text", x = 0.5, y = 0.5, 
                      label = paste("No matching features between tests for project", project_name)) + 
             theme_void())
  }
  
  # Combine all plots into a grid
  combined_plot <- plot_grid(plotlist = plot_list, ncol = ceiling(sqrt(length(plot_list))))
  
  # Add a title for the project
  title <- ggdraw() + 
    draw_label(paste("P-value Comparisons for Project:", 
                     ifelse(is.null(project_name), "All Projects", project_name)),
               fontface = "bold")
  
  # Combine title and plot grid
  final_plot <- plot_grid(title, combined_plot, ncol = 1, rel_heights = c(0.1, 1))
  
  return(final_plot)
}

# Function to create and save plots for all projects
create_all_project_plots <- function(data, output_dir = ".") {
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Get all unique projects
  projects <- unique(data$project)
  
  # For each project, create a plot and save it
  for (proj in projects) {
    plot <- create_pvalue_comparison_plots(data, proj)
    
    # Save the plot
    filename <- file.path(output_dir, paste0("pvalue_comparison_", proj, ".pdf"))
    ggsave(filename, plot, width = 30, height = 25)
    
    cat("Saved plot for project", proj, "to", filename, "\n")
  }
}

# Example usage:
# Create and save plots for all projects
# create_all_project_plots(DF, "output_plots")

folders <- c("../Results_partReg/4.2_metaSPARSim_ZerosAdded_DA_ancom_Nearing",
             "../Results_partReg/4.4_sparseDOSSA_filtered_ZerosAdded_DA_ancom_Nearing",
             "../Results/4.2_metaSPARSim_ZerosAdded_DA_ancom_Nearing",
             "../Results/4.4_metaSPARSim_filtered_ZerosAdded_DA_ancom_Nearing")
for(folder in folders){
  
  DF <- readRDS(paste0(folder,"/DF.RDS"))
  DFtmp <- DF
  DFtmp$feature <- paste(DF$feature,DF$dataset)
  projects <- unique(DFtmp$project)
  #projects <- "ibd_papa"
  
  pdf(file = paste0(folder,"/Compare_DAtests.pdf"), width = 30,height = 25)
  for(project in projects){
    onePlot <- create_pvalue_comparison_plots(DFtmp, project)
    #CairoPNG(filename="Compare_DAtests.PNG",width = 1000,height = 1000)
    print(onePlot)
    #dev.off()
  }
  dev.off()
}

