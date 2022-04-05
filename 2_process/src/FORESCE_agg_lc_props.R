aggregate_proportions_hrus <- function(df, group_by_segment_colname, proportion_col_prefix, hru_area_colname, new_area_colname){

  #'@description aggregation function to get land cover class proportions for PRMS_catchment_area 
  #'@param df data frame to aggregate
  #'@param group_by_segment_colname colname for segments that will be used for the group by
  #'@param proportion_col_prefix prefix col for lc class proportion value columns
  #'@param hru_area_colname colname for hru area
  #'@param new_area_colname str. New colname for aggregated area
  #'@example aggregate_proportions_hrus(group_by_segment_colname = hru_segment, proportion_col_prefix = 'prop_lcClass', hru_area_colname = hru_area, new_area_colname = total_PRMS_area)
  
  df <- df %>% 
    # Create temp cols of area of lc class per hru - NOTE: simply mutate current cols and no longer have proportion values
    mutate(across(starts_with(proportion_col_prefix),  ~(.x * {{hru_area_colname}}/10^6)))%>% 
    # group by hru segments - droping from 761 row to 416 - to get a single "PRMS" catchment per PRMS segment
    group_by_at(group_by_segment_colname) %>%
    summarise(
      # calc total area of aggregated catchments
      total_area_km2 = sum({{hru_area_colname}}/10^6),
      # calc new proportion with new catchment area
      across(starts_with(proportion_col_prefix), ~(sum(.x)/total_area_km2))) %>% 
    #rename col to name as input
    rename({{new_area_colname}} := total_area_km2)
  
  return(df)
}

