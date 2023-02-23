plot_nhdv2_attr <- function(attr_data,network_geometry,file_path,
                            filename_end = NULL, boundary = drb_shp){
  #' 
  #' @description This function visualizes each of the downloaded NHDv2 attribute variables across all river segments within the network
  #'
  #' @param attr_data data frame containing the processed NHDv2 attribute data; 
  #' must include column "PRMS_segid"
  #' @param network_geometry sf object containing the network flowline geometry; 
  #' must include columns "subsegid" and "geometry"
  #' @param file_path a character string that indicates the location of the saved plot
  #' @param filename_end optional character string to add to the end of the filename
  #' before the file extension.
  #' @param boundary sf polygon defining the boundary of the DRB to plot
  #'
  #' @value Returns a png file containing a violin plot showing distribution of each NHDv2 attribute variable
  
  message("Plotting individual NHDv2 attribute variables")
  
  plot_names <- vector('character', length = 0L)
  
  attr_names <- names(attr_data)[names(attr_data) != "PRMS_segid"]
  
  # For each column/attribute variable, plot the distribution of the data across all PRMS segments
  for(i in seq_along(attr_names)){
    
    col_name <- attr_names[i]
    dat_subset <- attr_data[,c("PRMS_segid", col_name)]
    
    # plot the distribution of attr values on a linear scale
    attr_plot <- dat_subset %>%
      ggplot(aes(x = "", y = .data[[col_name]])) + 
      geom_violin(draw_quantiles = c(0.5)) +
      geom_jitter(height=0,color = "steelblue",alpha=0.5,width=0.2) +
      labs(x="") + 
      theme_bw() + 
      theme(plot.margin = unit(c(0,0,0,0), "cm"))
    
    # plot the spatial variation
    attr_plot_spatial <- ggplot() + 
      #full network (in case there are reaches without data)
      geom_sf(data = network_geometry, 
              size = 0.3, color = 'gray') +
      #DRB boundary
      geom_sf(data = boundary, fill = NA,
              size = 0.3, color = 'black') +
      #attribute data
      geom_sf(data = dat_subset %>% 
                left_join(.,network_geometry[,c("subsegid","geometry")],
                          by=c("PRMS_segid"="subsegid")) %>%
                sf::st_as_sf(),
              mapping = aes(color=.data[[col_name]]),
              size = 0.3) + 
      scale_color_viridis_c(option="plasma") + 
      theme_bw() + 
      theme(plot.margin = unit(c(0,0,0,2), "cm"),
            axis.text.x = element_text(size = 6),
            legend.title = element_text(size = 10))
    
    # create combined plot showing violin plot and spatial distribution
    attr_plot_combined <- attr_plot + attr_plot_spatial + patchwork::plot_layout(ncol=2)

    plot_name <- paste0(file_path,"/",col_name,filename_end,".png")
    plot_names <- c(plot_names,plot_name)
    
    suppressWarnings(ggsave(plot_name,plot = attr_plot_combined,width=7,height=4,device = "png"))
    
  }
  
  return(plot_names)
  
}


plot_data_splits <- function(split, split_name, full_df, network_geometry, out_dir, boundary = drb_shp){
  #' 
  #' @description This function maps the reaches in the data split as blue
  #' and the outher reaches in gray.
  #'
  #' @param split data frame containing a column for "PRMS_segid"
  #' @param split_name name of the data split for the plot title
  #' @param full_df the full dataframe of reaches that have data. Only needs a column for "PRMS_segid"
  #' @param network_geometry sf object containing the network flowline geometry; 
  #' must include columns "subsegid" and "geometry"
  #' @param out_dir a character string that indicates the location of the saved plot
  #' @param boundary sf polygon defining the boundary of the DRB to plot
  #'
  #' @value Returns a png file containing a map
  
  #Get the split data to only unique PRMS_segid to be plotted
  split <- distinct(split, PRMS_segid)
  full_df <- distinct(full_df, PRMS_segid)
  
  attr_plot_spatial <- ggplot() + 
    #full network (in case there are reaches without data in split or full_df)
    geom_sf(data = network_geometry, 
            aes(color = "no data"),
            size = 0.3) +
    #DRB boundary
    geom_sf(data = boundary, fill = NA,
            size = 0.3, color = 'black') +
    #full data
    geom_sf(data = full_df %>% 
              left_join(.,network_geometry[,c("subsegid","geometry")],
                        by=c("PRMS_segid"="subsegid")) %>%
              sf::st_as_sf(),
            aes(color = "data present in full set"),
            size = 0.3) +
    #split data
    geom_sf(data = split %>% 
              left_join(.,network_geometry[,c("subsegid","geometry")],
                        by=c("PRMS_segid"="subsegid")) %>%
              sf::st_as_sf(),
            aes(color = "data present in split set"),
            size = 0.3) +
    scale_color_manual(name = "",
                       values = c("data present in split set" = "blue", 
                                  "data present in full set" = "lightblue", 
                                  "no data" = "gray")) + 
    theme_bw() + 
    theme(plot.margin = unit(c(0,0,0,2), "cm"),
          axis.text.x = element_text(size = 6),
          legend.title = element_text(size = 10)) +
    ggtitle(split_name)
  
  plot_name <- file.path(out_dir, paste0(split_name,".png"))
  
  suppressWarnings(ggsave(plot_name, plot = attr_plot_spatial, device = "png"))
  
  return(plot_name)
}
