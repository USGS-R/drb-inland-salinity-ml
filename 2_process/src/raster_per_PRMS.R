raster_prop_catchment <- function(catchment_sf, raster_data_path){
  
  raster <- raster(raster_data_path)
  vector <- catchment_sf
  
  ## check vector geometries 
  if(any(st_is_valid(vector) != TRUE){
    vector <- st_make_valid(vector)
  } else if(any(st_is_valid(raster) != TRUE)){
    raster <- st_make_valid(raster)
  }
  
  # # check crs
  # if(!st_crs(raster) == st_crs(vector)){
  #   vector <- st_transform(crs = st_crs(raster))
  # }
  # 
  ## crop raster to catchment_sf 
  list_raster_polygons <- raster::extract(raster, vector)
  
  count_raster_polygons <- lapply(list_raster_polygons, table)
  prop_raster_polygons <- lapply(list_raster_polygons, FUN = function(x) {table(x)/sum(table(x))})
  
  tmp <- raster::extract(drb_1960_raster, hd_catchments_sf)
  
  
  ## handling unequal length of classes
  rbind_list_fill <- function(x){
    ## get list of diff classes - unlist first. using sapply() accesses tmp3[[i]]
    raster_classes <- sapply(x, names)
    columns_names <- unique(unlist(raster_classes))
    
    len <- sapply(x, length)
    out <- vector("list", length(len))
    
    for(i in seq_along(len)){
      out[[i]] <- unname(x[[i]])[match(columns_names, raster_classes[[i]])]
    }
    
    setNames(as.data.frame(do.call(rbind, out), stringsAsFactors=FALSE), columns_names)
  }
  
  final_prop_raster_table <- rbind_list_fill(prop_raster_polygons)
  
  ## add id col for merge
  vector <- vector %>%
    mutate(id = row_number())
  
  final_prop_raster_table <- final_prop_raster_table %>% 
    mutate(id = row_number())
  ## merge
  df <- left_join(x = vector, y = final_prop_raster_table, by = id)
  
  return(df)
  
}


