catchment_area_check <- function(PRMS_shapefile, nhd_catchment_areas, area_difference_threshold_km2){
  
  ## group by shapefile from hru to prms scale and convert from m2 to km2
  gpkg_df <- PRMS_shapefile  %>% st_drop_geometry() %>% group_by(PRMS_segid) %>% 
    summarise(prms_gpkg_area_km2 = sum(hru_area_m2)/10^6)
  
  nlcd_area <- nhd_catchment_areas %>%
    select(PRMS_segid, comid, AREASQKM, TOTDASQKM, LENGTHKM) %>% 
    rename(., nhd_cat_area_sqkm = AREASQKM, nhd_tot_area_sqkm = TOTDASQKM, nhd_len_km = LENGTHKM) %>% 
    group_by(PRMS_segid) %>% 
    summarize(nhd_cat_area_sqkm = sum(nhd_cat_area_sqkm),
              nhd_tot_area_sqkm = sum(nhd_tot_area_sqkm),
              nhd_len_km = sum(nhd_len_km)) %>% 
    left_join(gpkg_df, by = 'PRMS_segid') %>% 
    mutate(area_abs_diff = abs(prms_gpkg_area_km2 - nhd_cat_area_sqkm))
  
  
  # Grab the PRMS_segids that have incorrect area
  area_diff <- nlcd_area %>% filter(area_abs_diff > area_difference_threshold_km2)
  
  ## This final list of PRMS_segid that need new polygons for the raster calculation
  
  final_list <- unique(area_diff$PRMS_segid)
  
  return(final_list)

}

## target
extract_nhd_catchments_from_PRMS_segid <- function(selected_PRMS_list, PRMS_comid_df = p2_drb_comids_all_tribs, measure_area = TRUE){
  
  ## Get comid ids for the erroneous PRMS_segid
  comids_PRMS_catchments_to_fix <- PRMS_comid_df %>% filter(PRMS_segid %in% selected_PRMS_list)
  comids_PRMS_catchments_to_fix
  
  ## Get catchments from nhdplustools - this call can take a while 
  shp_comids_PRMS_catchments_to_fix <- get_nhdplus(comid = comids_PRMS_catchments_to_fix$comid, realization = 'catchment') %>% 
    mutate(featureid = as.character(featureid)) %>%
    sf::st_make_valid() 
  
  # join/filter to PRMS_segid from initial list
  nhd_catchments <- comids_PRMS_catchments_to_fix %>% 
    left_join(shp_comids_PRMS_catchments_to_fix, by = c('comid' = 'featureid')) %>% 
    ## turn to sf obj to then be able to dissolve 
    sf::st_as_sf()
  
  ## Creating dissolved shp for PRMS_segid for different area
  nhd_catchments_dissolved <- nhd_catchments %>% 
    # ## 287_1 has no comid. removing this otherwise cannot transform to appropriate sf object
    filter(PRMS_segid != '287_1') %>% 
    dplyr::group_by(PRMS_segid) %>% 
    ## sum the areasqkm. Some comid catchments are NA - those are remove in this aggregation 
    dplyr::summarise(areasqkm = sum(areasqkm, na.rm = TRUE),
      across(geometry, ~ sf::st_union(.)), .groups = "keep") %>% ungroup()
  
  return(nhd_catchments_dissolved)

}

# 
# ## -- 
# 
# 
# final_list <- catchment_area_check(PRMS_shapefile = p1_catchments_edited_sf,
#                                     nhd_catchment_areas = p2_NLCD_LC_w_catchment_area,
#                                     area_difference_threshold = 5)
# 
dissolved_shp <- extract_nhd_catchments_from_PRMS_segid(selected_PRMS_list = final_list,
                                                        PRMS_comid_df = p2_drb_comids_all_tribs)
# 
# # target
# 
# ## Remove PRMS from gpkg
# p1_catchments_edited_sf_filtered <- p1_catchments_edited_sf %>% filter(!PRMS_segid %in% final_list) ## this will be used to calculate proportions 
# ## length: 380
# 
# ## join with PRMS_segid
# 
# ## --> this will be used to run through the raster_extraction function for land cover 
# 
# ## Testing functions
# ## using P
# 
# fixed_polygons <- raster_to_catchment_polygons(polygon_sf = dissolved_shp,
#                              raster = p1_FORESCE_backcasted_LC[1], categorical_raster = TRUE,
#                              raster_summary_fun = NULL,
#                              new_cols_prefix = 'lcClass',
#                              fill = 0)
# 
# gpk_shp <- raster_to_catchment_polygons(polygon_sf = p1_catchments_edited_sf_filtered,
#                                         raster = p1_FORESCE_backcasted_LC[1], categorical_raster = TRUE,
#                                         raster_summary_fun = NULL,
#                                         new_cols_prefix = 'lcClass',
#                                         fill = 0)
# 
# 
# reclassify_land_cover(land_cover_df = fixed_polygons,
#                       reclassify_table_csv_path = '1_fetch/in/Legend_FORESCE_Land_Cover.csv',
#                       reclassify_table_lc_col = 'FORESCE_value',
#                       reclassify_table_reclass_col = 'Reclassify_match',
#                       sep = ',',
#                       pivot_longer_contains = 'lcClass')
