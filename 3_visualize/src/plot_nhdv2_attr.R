plot_nhdv2_attr <- function(attr_data,network_geometry,file_path){
  #' 
  #' @description This function visualizes each of the downloaded NHDv2 attribute variables across all river segments within the network
  #'
  #' @param attr_data data frame containing the processed NHDv2 attribute data; 
  #' must include column "PRMS_segid"
  #' @param network_geometry sf object containing the network flowline geometry; 
  #' must include columns "subsegid" and "geometry"
  #' @param file_path a character string that indicates the location of the saved plot
  #'
  #' @value Returns a png file containing a violin plot showing distribution of each NHDv2 attribute variable
  
  message("Plotting individual NHDv2 attribute variables")
  
  plot_names <- vector('character', length = 0L)
  
  # For each column/attribute variable, plot the distribution of the data across all PRMS segments
  for(i in 2:dim(attr_data)[2]){
    
    dat_subset <- attr_data[,c(1,i)]
    col_name <- names(dat_subset)[2]
    
    # plot the distribution of attr values on a linear scale
    attr_plot <- dat_subset %>%
      ggplot(aes(x = "", y = .data[[col_name]])) + 
      geom_violin(draw_quantiles = c(0.5)) +
      geom_jitter(height=0,color = "steelblue",alpha=0.5,width=0.2) +
      labs(x="") + 
      theme_bw() + 
      theme(plot.margin = unit(c(0,0,0,0), "cm"))
    
    # plot the spatial variation
    attr_plot_spatial <- dat_subset %>% 
      left_join(.,network_geometry[,c("subsegid","geometry")],by=c("PRMS_segid"="subsegid")) %>%
      sf::st_as_sf() %>%
      ggplot() + 
      geom_sf(aes(color=.data[[col_name]]), size = 0.3) + 
      scale_color_viridis_c(option="plasma") + 
      theme_bw() + 
      theme(plot.margin = unit(c(0,0,0,2), "cm"),
            axis.text.x = element_text(size = 6),
            legend.title = element_text(size = 10))
    
    # create combined plot showing violin plot and spatial distribution
    attr_plot_combined <- attr_plot + attr_plot_spatial + patchwork::plot_layout(ncol=2)

    plot_name <- paste0(file_path,"/",col_name,".png")
    plot_names <- c(plot_names,plot_name)
    
    suppressWarnings(ggsave(plot_name,plot = attr_plot_combined,width=7,height=4,device = "png"))
    
  }
  
  return(plot_names)
  
}


