# gather land cover attributes for PRMS segments
library(readr)
library(sf)
library(targets)
library(data.table)

# Read in prms-nhd crosswalk table
tar_load(p2_prms_nhdv2_xwalk)
tar_load(p1_NLCD_df)
tar_load(p1_nhdv2reaches_sf)

###-----
# land cover attributes are given in percentages, so need to gather catchment areas from nhd vaa tables
area_att <- p1_nhdv2reaches_sf %>% st_drop_geometry() %>% select(COMID,AREASQKM,TOTDASQKM)

###----
LC_w_area <- function(area_att = area_att, NLCD_LC_df = p1_NLCD_df$NLCD_LandCover_2011){
  
# Filter land cover attributes for DRB comids that intersect PRMS segments and grab area columns
  lc_aoi <- NLCD_LC_df %>% filter(COMID %in% drb_comids$comid) %>%
    left_join(., area_att, by="COMID") %>%
    mutate(COMID = as.character(COMID))
  
  return(lc_aoi)
}

###----
est_NLCD_prms <- function(NLCD_LC_df_w_area,
                          comid_prms_xwalk = p2_prms_nhdv2_xwalk,
                          catchment_att = "CAT"){

#' @description
#' @param NLCD_LC_df_w_area
#' @param comid_prms_xwalk
#' @param catchment_att Must be either 'CAT', "ACC", 'TOT'
#' @example 

# Create long table with all comids identified for DRB
drb_comids <- comid_prms_xwalk %>%
  # split df by PRMS_segid() -  
  split(.,.$PRMS_segid) %>%
  # loop through each row element of this list and str split such that each new col is a cell
  lapply(.,function(x){
    comids <- data.frame(PRMS_segid = x$PRMS_segid,comid=unlist(strsplit(x$comid_all,split=";")))
  }) %>%
  bind_rows()

# Try out land cover aggregation for just one NLCD class 
area_df <- drb_comids %>%
  # Joining PRMS with NLCD
  left_join(.,lc_drb,by=c("comid"="COMID"))%>%
  # multiply area by percentage for all the cols that start with 'CAT" or "ACC" or "TOT"
  mutate(across(starts_with(catchment_att),~(.x/100)*AREASQKM,.names="AREA_{col}")) %>%
  # group by the PRMS id
  group_by(PRMS_segid) %>%
  # summarize of the group by 
  summarize(
#  across(ends_with(match = '_24', suff)),  sum, .names="PRMS_PERCENT_{col}")
  # sum the area calculation of the comid to agg to the level of PRMS
    across(starts_with(paste0("AREA_",catchment_att)), sum, .names="PRMS_PERCENT_{col}"),
#   # round the new cols PRMS_percent
    across(starts_with(paste0("PRMS_", catchment_att)), round, 2), 
  .groups="keep")

  return(area_df)

}
