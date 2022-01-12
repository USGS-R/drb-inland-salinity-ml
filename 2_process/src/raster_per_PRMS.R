raster_to_catchment_polygons <- function(polygon_sf, raster,
                                         categorical_raster = NULL,
                                         raster_summary_fun = NULL,
                                         new_cols_prefix = NULL){

  #' @description Function extracts raster pixel per catchment polygon and calculated proportion of raster class per total
  #'
  #' @param polygon_sf sf object of polygon(s) in aoi
  #' @param raster path to '.tif' file or raster object of SpatRaster object  
  #' @param categorical_raster logical. If categorical raster, TRUE
  #' @param raster_summary_fun for continuous raster, function to summarize the data by geometry. kept NULL if categorical_raster = TRUE
  #' @param categorical_raster_prefix for categorical raster, prefix added to colnames to label raster classes/values
  #' @value A data frame the same columns as the polygon_sf spatial dataframe with additional columns of the Land Cover classes
  #' @example land cover raster ex: raster_to_catchment_polygons(polygon_sf = p1_catchments_sf, raster = p1_backcasted_LC[1], categorical_raster = TRUE, new_cols_prefix = 'lc_)
  #' @example cont. raster ex: raster_to_catchment_polygons(polygon_sf = p1_catchments_sf, raster = p1_rod_salt, categorical_raster = FALSE, raster_summary_fun = sum, new_cols_prefix = 'cont_raster')
  
  # read in 
  raster <- rast(raster)
  vector_sf <- polygon_sf 
  
  # raster <- rast(path)
  # vector_sf <- p1_catchments_sf
  
  ## check vector geometries 
  if(any(!st_is_valid(vector_sf))){
    vector_sf <- st_make_valid(vector_sf)
    print('shp geometries fixed')
    } 
  
  ## match crs
  if(!st_crs(raster) == st_crs(vector_sf)){
    print('crs are different. Transforming ...')
    vector_sf <- st_transform(vector_sf, crs = st_crs(raster))
    if(st_crs(raster) == st_crs(vector_sf)){
    print('crs now aligned')}
  }else if(st_crs(raster) == st_crs(vector_sf)){
    print('crs are already aligned')
  }
  
  ## convert vector sf object to spatvector compatible to with raster processing with terra
  vector <- vector_sf %>% vect()

  ## crop raster to catchment_sf 
  raster_crop <- terra::crop(raster, vector)
  
  if(categorical_raster == TRUE){
  ## Extract rasters pixels in each polygon
  start_time <- Sys.time()
  raster_per_polygon <- terra::extract(raster_crop, vector, list = T) %>% 
    #lapply(table) # to get frequency of each categorical value
    lapply(FUN = function(x) {table(x)/sum(table(x))})
  end_time <- Sys.time()
  print(end_time - start_time)
  
  print(raster_per_polygon[1:5])
  #  tmp <- terra::extract(drb_1960, catchments_vect[1:3,], list = T) %>% 
  
  ## handling unequal length of classes
  final_raster_table <- rbind.fill(raster_per_polygon) %>%
    setNames(paste0(new_cols_prefix, names(.))) %>% 
    mutate(ID = row_number())
  
  }
  
  if(is.null(categorical_raster) || categorical_raster == FALSE){
    print('extracting cont raster')
    raster_per_polygon <- terra::extract(raster_crop, vector, fun = raster_summary_fun)
    print(head(raster_per_polygon))
    final_raster_table <- data.frame(raster_per_polygon)
    col_len <- length(names(final_raster_table))
    names(final_raster_table)[col_len] <- paste0(new_cols_prefix, names(final_raster_table)[col_len])
  }
  
  vector_sf <- vector_sf %>% mutate(ID = row_number())
  
  df <- left_join(x = vector_sf, y = final_raster_table , by = 'ID')
  
  return(df)
  
}

df_salt <- raster_to_catchment_polygons(polygon_sf = p1_catchments_sf, raster = path,
                                        categorical_raster = FALSE, raster_summary_fun = sum, new_cols_prefix = 'rd_salt_')

df_lc <- raster_to_catchment_polygons(polygon_sf = p1_catchments_sf,
                                      raster = p1_backcasted_LC[1],
                                      categorical_raster = TRUE,
                                      raster_summary_fun = NULL,
                                      new_cols_prefix = 'lcClass_')
