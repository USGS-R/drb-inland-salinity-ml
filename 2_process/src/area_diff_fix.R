catchment_area_check <- function(FORESCE_df, y, area_difference_threshold){
  
  
}

## Get area difference

## Identify PRMS_segid with area difference:
area_difference_threshold <- 5

gpkg_df <- p1_catchments_edited_sf %>% st_drop_geometry() %>% group_by(PRMS_segid) %>% 
  summarise(prms_gpkg_area_km2 = sum(hru_area_m2)/10^6)

nlcd_area <- p2_NLCD_LC_w_catchment_area %>%
  select(PRMS_segid, comid, AREASQKM, TOTDASQKM, LENGTHKM) %>% 
  rename(., nhd_cat_area_sqkm = AREASQKM, nhd_tot_area_sqkm = TOTDASQKM, nhd_len_km = LENGTHKM) %>% 
  group_by(PRMS_segid) %>% 
  summarize(nhd_cat_area_sqkm = sum(nhd_cat_area_sqkm),
            nhd_tot_area_sqkm = sum(nhd_tot_area_sqkm),
            nhd_len_km = sum(nhd_len_km)) %>% 
  left_join(gpkg_df, by = 'PRMS_segid') %>% 
  mutate(area_abs_diff = abs(prms_gpkg_area_km2 - nhd_cat_area_sqkm))

# Grab the PRMS_segids that have incorrect area
area_diff <- nlcd_area %>% filter(area_abs_diff > area_difference_threshold)

## This final list of PRMS_segid that need new polygons for the raster calculation

final_list <- unique(area_diff$PRMS_segid)

## Remove PRMS from gpkg
filtered_gpkg_df <- gpkg_df %>% filter(!PRMS_segid %in% final_list) ## this will be used to calculate proportions 
## length: 380

## Get comid ids for the erroneous PRMS_segid
comids_PRMS_catchments_to_fix <- p2_drb_comids_all_tribs %>% filter(PRMS_segid %in% final_list)

## this call can take a while 
shp_comids_PRMS_catchments_to_fix <- get_nhdplus(comid = comids_PRMS_catchments_to_fix$comid, realization = 'catchment') %>% 
  mutate(featureid = as.character(featureid))
mapview(shp_comids_PRMS_catchments_to_fix)

PRMS_shp_merge <- comids_PRMS_catchments_to_fix %>% left_join(shp_comids_PRMS_catchments_to_fix, by = c('comid' = 'featureid'))

which(any(is.na(PRMS_shp_merge)))
head(rowSums(is.na(PRMS_shp_merge)))

## Creating dissolved shp for PRMS_segid for different area
PRMS_shp_merge <- PRMS_shp_merge %>% 
  ## 287_1 has no comid. removing
  filter(PRMS_segid != '287_1') %>% 
  dplyr::group_by(PRMS_segid) %>% 
  dplyr::summarise(across(geometry, ~ sf::st_combine(.)), .groups = "keep")
## --> this will be used to run through the raster_extraction function for land cover 


