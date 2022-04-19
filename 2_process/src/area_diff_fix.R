catchment_area_check <- function(PRMS_shapefile, nhd_catchment_areas, area_difference_threshold_km2){
  
  #' @description Function to check the area difference between nhd comid aggregation catchments and PRMS_catchments from inputed gpkg file
  #' @param PRMS_shapefile catchment shapefile PRMS_catchments_edited_sf
  #' @param nhd_catchment_areas dataset that includes PRMS_segid, comids and areasqkm cols (for our pipeline, use p2_NLCD_LC_w_catchment_area)
  #' @param area_difference_threshold_km2 int. threshold km2 area difference. If beyond this value, PRMS_segid require nhd  (e.g. 5, 10, 50 )
  #' @value A list of PRMS_segid that need special handling regarding their respective attributed catchment polygon(s) 
  #' @example catchment_area_check(PRMS_shapefile = p1_catchments_edited_sf, nhd_catchment_areas = p2_NLCD_LC_w_catchment_area, area_difference_threshold_km2 = 5)
  
  ## group by shapefile from hru to prms scale and convert from m2 to km2
  gpkg_df <- PRMS_shapefile  %>% st_drop_geometry() %>% group_by(PRMS_segid) %>% 
    summarise(prms_gpkg_area_km2 = sum(hru_area_km2))
  
  ##join gpkg catchments and nhd catchments, and calculate difference
  nlcd_area <- nhd_catchment_areas %>%
    select(PRMS_segid, comid, AREASQKM, LENGTHKM) %>% 
    rename(., nhd_cat_area_sqkm = AREASQKM, nhd_len_km = LENGTHKM) %>% 
    group_by(PRMS_segid) %>% 
    summarize(nhd_cat_area_sqkm = sum(nhd_cat_area_sqkm),
              nhd_len_km = sum(nhd_len_km)) %>% 
    left_join(gpkg_df, by = 'PRMS_segid') %>% 
    mutate(area_abs_diff = abs(prms_gpkg_area_km2 - nhd_cat_area_sqkm))
  
  # Grab the PRMS_segids that have incorrect area
  area_diff <- nlcd_area %>% filter(area_abs_diff > area_difference_threshold_km2)
  
  ## This final list of PRMS_segid that need new polygons for the raster calculation
  final_list <- unique(area_diff$PRMS_segid)
  
  return(final_list)

}

dissolve_nhd_catchments_to_PRMS_segid <- function(selected_PRMS_list, PRMS_comid_df = p2_drb_comids_all_tribs, prms_reaches_sf){
  
  #' @description Function to dissolve nhd catchments polygons to PRMS-scale for PRMS_segids requiring correction. Follows catchment_area_check() function 
  #' @param selected_PRMS_list vector of PRMS_segid (char) that need special handling and new PRMS catchment attribution. Output of catchment_area_check
  #' @param PRMS_comid_df dataframe that maps comids of each PRMS_segid (for our pipeline, this is target p2_drb_comids_all_tribs)
  #' @param prms_reaches_sf PRMS reaches in p1_reaches_sf with columns subsegid and subseglen
  #' @value a multipolygon shapefile  
  #' @example dissolve_nhd_catchments_to_PRMS_segid(selected_PRMS_list = catchment_area_check_output, PRMS_comid_df = p2_drb_comids_all_tribs)
 
  ## Get comid ids for the selected PRMS_segid needing new polygon attribution
  comids_PRMS_catchments_to_fix <- PRMS_comid_df %>% 
      filter(PRMS_segid %in% selected_PRMS_list)
  
  ## Get comid catchments from nhdplustools - this call can takes a little while 
  shp_comids_PRMS_catchments_to_fix <- get_nhdplus(comid = comids_PRMS_catchments_to_fix$comid, realization = 'catchment') %>% 
    mutate(featureid = as.character(featureid)) %>%
    sf::st_make_valid() 
  
  # join/filter comids polygons to PRMS_segid from initial list. Make into sf_object
  nhd_catchments <- comids_PRMS_catchments_to_fix %>% 
    left_join(shp_comids_PRMS_catchments_to_fix, by = c('comid' = 'featureid')) %>% 
    ## turn to sf obj to then be able to dissolve 
    sf::st_as_sf() %>%
    #assign area to reach 287_1 using reach length
    mutate(areasqkm = case_when(PRMS_segid == '287_1' ~ (filter(prms_reaches_sf, 
                                                                subsegid == '287_1') %>% 
                                                           pull(subseglen))^2/10^6,
                                TRUE ~ areasqkm))
  
  #assign geometry to reach 287_1 using a small buffer of 15 m to closely match approx. area
  nhd_catchments$geometry[nhd_catchments$PRMS_segid == '287_1'] <- 
    st_geometry(st_buffer(filter(prms_reaches_sf, subsegid == '287_1'), dist = 15))
  
  ## Creating dissolved shp for PRMS_segid for different area
  nhd_catchments_dissolved <- nhd_catchments %>%
    dplyr::group_by(PRMS_segid) %>% 
    ## sum the areasqkm. Some comid catchments are NA - those are remove in this aggregation 
    dplyr::summarise(total_PRMS_area = sum(areasqkm, na.rm = TRUE),
      across(geometry, ~ sf::st_union(.)), .groups = "keep") %>% ungroup()
  
  return(nhd_catchments_dissolved)

}
