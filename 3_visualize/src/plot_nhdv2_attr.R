plot_nhdv2_attr <- function(attr_data,file_path){
  #' 
  #' @description This function visualizes each of the downloaded NHDv2 attribute variables across all river segments within the network
  #'
  #' @param attr_data list containing the processed NHDv2 attribute data; data frames must include column "PRMS_segid" 
  #' @param file_path a character string that indicates the location of the saved plot
  #'
  #' @value Returns a png file containing a violin plot showing distribution of each NHDv2 attribute variable
  
  message("Plotting individual NHDv2 attribute variables")
  
  # Bind all columns representing unique NHDv2 attribute variables
  attr_data_df <- attr_data %>%
    Reduce(full_join,.) %>%
    # hide messages that data frames are being joined by column 'PRMS_segid'
    suppressMessages()
  
  plot_names <- c("")
  
  # For each column/attribute variable, plot the distribution of the data across all PRMS segments
  for(i in 2:dim(attr_data_df)[2]){
    
    dat_subset <- attr_data_df[,c(1,i)]
    col_name <- names(dat_subset)[2]
    
    # linear scale
    attr_plot <- dat_subset %>%
      ggplot(aes(x = "", y = .data[[col_name]])) + 
      geom_violin(draw_quantiles = c(0.5)) +
      geom_jitter(height=0,color = "steelblue",alpha=0.5,width=0.2) +
      labs(x="") + 
      theme_bw()
    
    # log scale
    attr_plot_log <- dat_subset %>%
      ggplot(aes(x = "", y = (.data[[col_name]]+0.01))) + 
      geom_violin(draw_quantiles = c(0.5)) +
      geom_jitter(height=0,color = "steelblue",alpha=0.5,width=0.2) +
      scale_y_log10() + 
      labs(x="",y = paste0("log[10]  ",col_name," + 0.01")) + 
      theme_bw()
    
    attr_plot_combined <- attr_plot + attr_plot_log + patchwork::plot_layout(ncol=2)
    
    plot_name <- paste0(file_path,"/",col_name,".png")
    plot_names <- c(plot_names,plot_name)
    
    suppressWarnings(ggsave(plot_name,plot = attr_plot_combined,width=7,height=4,device = "png"))
    
  }
  
  plot_names <- plot_names[-1]
  
  return(plot_names)
  
}


