## Gather land cover attributes for PRMS segments
library(readr)
library(sf)
library(targets)
library(tidyverse)
library(data.table)

###-----
LC_w_area <- function(area_att, NLCD_LC_df, drb_comids_df){
  
# Filter land cover attributes for DRB comids that intersect PRMS segments and join with area columns
  lc_aoi <- NLCD_LC_df %>%
    filter(COMID %in% drb_comids_df$comid) %>%
    left_join(., area_att, by="COMID") %>%
    mutate(COMID = as.character(COMID))
  
  return(lc_aoi)
}

###----
proportion_lc_by_prms <- function(NLCD_LC_df_w_area,
                                  drb_comids_df,
                          catchment_att = "CAT"){

#' @description
#' @param NLCD_LC_df_w_area
#' @param catchment_att Must be either 'CAT', "ACC", 'TOT'
#' @example 


# Try out land cover aggregation for just one NLCD class 
area_df <- drb_comids_df %>%
  # Joining PRMS with NLCD LC 
  left_join(.,NLCD_LC_df_w_area,by=c("comid"="COMID")) %>%
  # multiply area by percentage for all the cols that start with 'CAT" or "ACC" or "TOT"
  mutate(across(starts_with(catchment_att),~(.x/100)*AREASQKM,.names="AREA_{col}")) %>%
  # aggregate to PRMS scale through group_by()
  group_by(PRMS_segid) %>%
  # summarize group by at PRMS catchment scale
  summarize(
    # Sum to calculate total area of the PRMS  catchment
    across(AREASQKM, sum, .names='AREASQKM_PRMS'),
    # Sum to calculate total LC area at the PRMS catchment scale
    across(starts_with(paste0('AREA_',catchment_att)), sum, .names = 'PRMS_AREA_{col}'),
    # calculate proportion of LC in entire PRMS catchment - Call PRMS_PERCENT
    across(starts_with(paste0('PRMS_AREA_',catchment_att)), ~(.x/AREASQKM), .names="PRMS_PERCENT_{col}"),
    # round the new PRMS cols 
    across(starts_with('PRMS'), round, 2), 
  .groups="drop")


  return(area_df)

}

