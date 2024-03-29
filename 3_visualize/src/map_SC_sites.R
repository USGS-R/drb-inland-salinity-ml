map_SC_sites <- function(site_list_csv) {
  #' 
  #' @description Function to map the unique site id's within the area of interest.
  #'
  #' @param site_list_csv character string that indicates the path for a csv file containing the list of sites
  #' The site list file should contain the columns "datum", "data_src_combined", and "count_days_total"

  # load the list of sites
  site_list <- read_csv(site_list_csv,show_col_types = FALSE)
  
  # check for required columns in the site list
  req_cols <- c("datum", "data_src_combined","count_days_total")
  flag_cols <- req_cols[which(req_cols %in% names(site_list)=="FALSE")]
  if(length(flag_cols)>0) stop("site_list_csv file is missing one or more required columns: datum, data_src_combined,count_days_total")
  
  # convert data frame to spatial object:
  site_list_sp <- sf::st_as_sf(site_list,coords=c("lon","lat"),crs=site_list$datum[1]) %>%
    sf::st_transform(4326) %>%
    mutate(group = as.factor(case_when(data_src_combined == "NWIS_daily/Harmonized_WQP_data" ~ "daily",
                                       data_src_combined == "NWIS_instantaneous/Harmonized_WQP_data" ~ "instantaneous",
                                       data_src_combined == "Harmonized_WQP_data" ~ "discrete",
                                       data_src_combined == "NWIS_instantaneous" ~ "instantaneous",
                                       data_src_combined == "NWIS_daily" ~ "daily")))

  # define color palette for circle markers:
  pal <- colorFactor(palette = c("blue","orange","green"),domain=site_list_sp$group)
  
  # plot sites:
  map <- leaflet() %>%
    addProviderTiles("CartoDB.DarkMatter", group = "CartoDB") %>%
    addCircleMarkers(data=site_list_sp,radius = ~sqrt(count_days_total/40),color = ~pal(group),
                     stroke = FALSE, fillOpacity = 0.5,popup=paste("Site:", site_list_sp$site_id,"<br>",
                                                                   "n_observation-days:", site_list_sp$count_days_total)) %>%
    addLegend("bottomright", colors = c("orange","green","blue"), labels = c("discrete","instantaneous","daily")) 
  
  return(map)
  
}
