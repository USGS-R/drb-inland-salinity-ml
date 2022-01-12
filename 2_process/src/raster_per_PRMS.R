raster_to_catchment_polygons <- function(catchment_sf, raster_data_path){
  
  raster <- raster(raster_data_path)
  vector <- catchment_sf %>% mutate(id = row_number())
  
  ## check vector geometries 
  if(any(st_is_valid(vector) != TRUE)){
    vector <- st_make_valid(vector)
  } else if(any(st_is_valid(raster) != TRUE)){
    raster <- st_make_valid(raster)
  }
  
  if(!st_crs(raster) == st_crs(vector)){
    vector <- st_transform(crs = st_crs(raster))
  }
  
  ## crop raster to catchment_sf 
  raster_crop <- terra::crop(raster, vector)
  
  ## Extract rasters pixels in each polygon
  #  raster_per_polygon_list <- raster::extract(raster, vector)
  raster_per_polygon_list <- terra::extract(raster_crop, vector, list = T) %>% 
    #lapply(tmp, table) # to get count
    lapply(tmp, FUN = function(x) {table(x)/sum(table(x))})
  
  #  tmp <- terra::extract(drb_1960, catchments_vect[1:3,], list = T) %>% 
  
  ## handling unequal length of classes
  final_prop_raster_table <- rbind.fill(raster_per_polygon_list)
  
  df <- left_join(x = vector, y = final_prop_raster_table, by = id)
  
  return(df)
  
