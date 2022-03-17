###-----

## Subset Land to AOI and join with PRMS ids and COMID area (sqkm)
AOI_LC_w_area <- function(NLCD_LC_df, area_att, aoi_comids_df){
  
  #' @description following the comid id as the main key, this function subset NLCD LC data to the area of interest, joins with nhd COMID area info and PRMS info
  #' Output is a df with PRMS ids, COMIDs, AREA(SQKM), and NLCD LC percentage per COMID (unique key)
  #' @param area_att df with comids and associated area (AREASQKM)
  #' @param NLCD_LC_df selected LC dataframe structured by COMID 
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

# Estimate prop of LC coverage at PRMS catchment scale
proportion_lc_by_prms <- function(NLCD_LC_df_w_area, catchment_att = 'CAT'){

#' @description This function takes the consolidated df from AOI_LC_w_area() and calculated the proportion share of each LC class at the PRMS segid scale
#' @param NLCD_LC_df_w_area df output of AOI_LC_w_area that has LC data per COMID & PRMS id and area in (km2)
#' @param @param catchment_att Specifies the catchment attribute to subset to. Must be either 'CAT', "ACC", 'TOT'. Default = "CAT"
#' @example proportion_lc_by_prms(NLCD_LC_df_w_area = p2_LC_w_catchment_area)

  area_df <- NLCD_LC_df_w_area %>%
    mutate(AREASQKM_approx = case_when(AREASQKM == 0 ~ LENGTHKM^2, TRUE  ~ AREASQKM)) %>% 
    # multiply COMID area by the LC percent for all the cols that start with 'CAT" - unit km2
    mutate(across(starts_with(catchment_att),~(.x/100)*AREASQKM_approx,.names="AREA_{col}")) %>%
    # aggregate to PRMS scale through group_by()
    group_by(PRMS_segid) %>%
    # summarize group by at PRMS catchment scale
    summarize(
      # Sum to calculate total area of the PRMS  catchment - unit km2
      across(AREASQKM_approx, sum, .names='AREASQKM_PRMS'),
      # Sum to calculate  LC area at the PRMS catchment scale per LC class
      across(starts_with(paste0('AREA_',catchment_att)), sum, .names = 'PRMS_{col}'),
      # calculate proportion coverage of specified LC class in the entire PRMS catchment - proportion
      across(starts_with(paste0('PRMS_AREA_',catchment_att)), ~(.x/AREASQKM_PRMS),
             .names="PROP_{col}"),
      # round the new cols 
      across(starts_with(c('PROP','PRMS')), round, 4), 
    .groups="drop") %>% 
    select(-starts_with(paste0('PRMS_AREA_',catchment_att)))

  return(area_df)

}


