library(sf)
library(dplyr)
library(tidyverse)
library(nhdplusTools)
library(mapview)

catchment_area_check <- function(PRMS_shapefile, nhd_cachment_areas, area_difference_threshold){
  
## Get area difference

## Identify PRMS_segid with area difference:
area_difference_threshold <- 5

## group by shapefile from hru to prms scale and convert from m2 to km2
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

nlcd_area

# Grab the PRMS_segids that have incorrect area
area_diff <- nlcd_area %>% filter(area_abs_diff > area_difference_threshold)

## This final list of PRMS_segid that need new polygons for the raster calculation

final_list <- unique(area_diff$PRMS_segid)


}

## target
extract_nhd_catchments_from_PRMS_segid(selected_PRMS_list, PRMS_comid_df = p2_drb_comids_all_tribs){
  
  ## Get comid ids for the erroneous PRMS_segid
  comids_PRMS_catchments_to_fix <- PRMS_comid_df %>% filter(PRMS_segid %in% final_list)
  
  ## this call can take a while 
  shp_comids_PRMS_catchments_to_fix <- get_nhdplus(comid = comids_PRMS_catchments_to_fix$comid, realization = 'catchment') %>% 
    mutate(featureid = as.character(featureid)) %>%
    sf::st_make_valid()
  
  nhd_catchments <- comids_PRMS_catchments_to_fix %>%
    left_join(shp_comids_PRMS_catchments_to_fix, by = c('comid' = 'featureid')) %>% st_as_sf()
  
  ## Creating dissolved shp for PRMS_segid for different area
  nhd_catchments_dissolved <- nhd_catchments %>% 
    # ## 287_1 has no comid. removing this otherwise cannot transform to appropriate sf object
    filter(PRMS_segid != '287_1') %>% 
    dplyr::group_by(PRMS_segid) %>% 
    dplyr::summarise(across(geometry, ~ sf::st_union(.)), .groups = "keep") %>% ungroup()
  
  return(nhd_catchments_dissolved)

  }
## -- 


# target

## Remove PRMS from gpkg
p1_catchments_edited_sf_filtered <- p1_catchments_edited_sf %>% filter(!PRMS_segid %in% final_list) ## this will be used to calculate proportions 
## length: 380





## join with PRMS_segid


## --> this will be used to run through the raster_extraction function for land cover 


## Testing functions
## using P

fixed_polygons <- raster_to_catchment_polygons(polygon_sf = nhd_catchments_dissolved,
                             raster = p1_FORESCE_backcasted_LC[1], categorical_raster = TRUE,
                             raster_summary_fun = NULL,
                             new_cols_prefix = 'lcClass',
                             fill = 0)

gpk_shp <- raster_to_catchment_polygons(polygon_sf = p1_catchments_edited_sf_filtered,
                                        raster = p1_FORESCE_backcasted_LC[1], categorical_raster = TRUE,
                                        raster_summary_fun = NULL,
                                        new_cols_prefix = 'lcClass',
                                        fill = 0)


reclassify_land_cover(land_cover_df = fixed_polygons,
                      reclassify_table_csv_path = '1_fetch/in/Legend_FORESCE_Land_Cover.csv',
                      reclassify_table_lc_col = 'FORESCE_value',
                      reclassify_table_reclass_col = 'Reclassify_match',
                      sep = ',',
                      pivot_longer_contains = 'lcClass')
