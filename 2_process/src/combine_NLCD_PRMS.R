###-----
## Subset Land to AOI and join with area and PRMS ids
AOI_LC_w_area <- function(NLCD_LC_df, area_att, aoi_comids_df){
  
  #' @description following the comid id as the main key, this function subset NLCD LC data to the area of interest, join with area info and PRMS
  #' Output is a df with PRMS ids, COMIDs, AREA, and NLCD LC percentage per COMID (unique key)
  #' @param area_att df with comid and associated area (AREASQKM)
  #' @param NLCD_LC_df selected LC dataframe organized by COMID 
  #' @param aoi_comids_df df linking comid and prms for the area of interest, already filter to area of interest 
  #' @example AOI_LC_w_area(NLCD_LC_df = p1_NLCD_df$NLCD_LandCover_2011, area_att = p2_nhd_area_att, drb_comids_df = p2_drb_comids)
  
# Filter land cover attributes to aoi comids (drb) and join with area columns
  lc_area <- NLCD_LC_df %>%
    filter(COMID %in% aoi_comids_df$comid) %>%
    left_join(., area_att, by="COMID") %>%
    mutate(COMID = as.character(COMID))

# Create df with PRMS, COMID, and NLCD LC att for the aoi (drb)
  lc_aoi <- aoi_comids_df %>%
    left_join(., lc_area, by = c('comid' = "COMID"))

  return(lc_aoi)

  }

###----
# Estimate proportion of LC coverage at PRMS scale
proportion_lc_by_prms <- function(NLCD_LC_df_w_area,
                          catchment_att = "CAT"){

#' @description This function takes the 
#' @param NLCD_LC_df_w_area df output of AOI_LC_w_area that has LC data per COMID & PRMS id and area in (km2)
#' @param catchment_att Specifies the catchment attribute to subset to. Must be either 'CAT', "ACC", 'TOT'. Default = "CAT"
#' @example proportion_lc_by_prms(NLCD_LC_df_w_area = p2_LC_w_catchment_area, catchment_att = "CAT")

# Try out land cover aggregation for just one NLCD class 
area_df <- NLCD_LC_df_w_area %>%
  # multiply area by percentage for all the cols that start with 'CAT" or "ACC" or "TOT"
  mutate(across(starts_with(catchment_att),~(.x/100)*AREASQKM,.names="AREA_{col}")) %>%
  # aggregate to PRMS scale through group_by()
  group_by(PRMS_segid) %>%
  # summarize group by at PRMS catchment scale
  summarize(
    # Sum to calculate total area of the PRMS  catchment
    across(AREASQKM, sum, .names='AREASQKM_PRMS'),
    # Sum to calculate total LC area at the PRMS catchment scale
    across(starts_with(paste0('AREA_',catchment_att)), sum, .names = 'PRMS_{col}'),
    # calculate proportion of LC in entire PRMS catchment - Call PRMS_PERCENT
    across(starts_with(paste0('PRMS_AREA_',catchment_att)), ~(.x/AREASQKM_PRMS),
           .names="PERCENT_{col}"),
    # round the new PRMS cols 
    across(starts_with('PRMS'), round, 2), 
  .groups="drop")

## Final cleaning - removing LC class 12 (snow/ice) and the empty PRMS (287_1)
area_df <- area_df %>% select(-contains('NLCD11_12')) %>% filter(AREASQKM_PRMS > 0 )

  return(area_df)

}

