reclassify_land_cover <- function(land_cover_df,
                                  reclassify_table,
                                  reclassify_table_lc_col,
                                  reclassify_table_reclass_col,
                                  pivot_longer_contains,
                                  proportion_col_prefix, hru_area_colname,
                                  remove_NA_cols = TRUE){
  
  #' @description Reclassify land cover df with cols as individual lc classes to pre-defined land cover class
  #' @param land_cover_df dataframe with lc classes as cols 
  #' @param reclassify_table the reclassification table that is used in the function
  #' @param reclassify_table_lc_col col that contains original classes of the land_cover_df dataframe
  #' @param reclassify_table_reclass_col col in reclassify csv that contains the new land cover classes 
  #' @param pivot_longer_contains common colname str found in land_cover_df land cover cols (e.g. 'lcClass' or 'NLCDClass')
  #' @param proportion_col_prefix prefix col for lc class proportion value columns
  #' @param hru_area_colname colname for hru area
  #' @param remove_NA_cols logical for removing the NA land cover class or not. Allocates NAs in the estuary to be water class.
  #' @value output is a updated version of the land_cover_df with new cols representing the new classes 
  
  # Load reclassification csv only taking class values, not description cols
  reclassify_table <- reclassify_table %>% 
    select({{reclassify_table_lc_col}}, {{reclassify_table_reclass_col}})

  ## Pivoted + join 
  # doing as df in case its a list item and needed reseting to df 
  new_classes_df <- as.data.frame(land_cover_df) %>% 
    # pivot longer since that old class is the original land cover class label (1,3,4,...) and land cover class values fall in a prop class in catchment call. (this can be renamed when generalizing)
    pivot_longer(cols = contains(pivot_longer_contains),
               names_to = 'old_class',
               values_to = 'Prop_class_in_catchment') %>% 
    # add col that extracts the original raster class values. This will allow for a merge
    mutate(merge_col = as.numeric(stringr::str_extract(old_class, '\\d+$'))) %>% 
    # join reclassification table
    left_join(reclassify_table, by = c('merge_col' = {{reclassify_table_lc_col}})) %>% 
    # add col that will end up being the name of column (can ultimately be removed if we use `names_prefix` in pivot_wider())
    mutate(new_class = paste0(pivot_longer_contains, '_', .[[reclassify_table_reclass_col]])) %>% 
    # remove cols no longer needed
    select(-c(old_class, merge_col, {{reclassify_table_reclass_col}}))

  ## pivot_wider to return lcClass labels to columns. Summarizing via a sum
  final_df <- pivot_wider(new_classes_df, names_from = new_class, 
                          names_prefix = 'prop_', values_from = Prop_class_in_catchment, 
                          values_fn = sum)
  
  # some lc classes in FORESCE were given NA class
  # for estuary NAs, assign all NA area to "water" area
  # for other NAs subtract the NA area
  # remove the NA lc class
  if(remove_NA_cols == TRUE){
    # Convert the estuary NA proportion to "water" proportion (class 1)
    # most estuary NAs are > 0.15 in proportion. There are 3 others ID'd manually
    final_df <- mutate(.data = final_df,
                 prop_lcClass_1 = case_when(prop_lcClass_NA > 0.15 ~ prop_lcClass_1 + prop_lcClass_NA,
                                            ID == 392 ~ prop_lcClass_1 + prop_lcClass_NA,
                                            ID == 370 ~ prop_lcClass_1 + prop_lcClass_NA,
                                            ID == 413 ~ prop_lcClass_1 + prop_lcClass_NA,
                                            TRUE ~ prop_lcClass_1)) %>%
      #get all proportions to area units
      mutate(across(starts_with(proportion_col_prefix),
                    ~(.x * {{hru_area_colname}}))) %>%
      #drop NA area
      select(-contains('NA'), -{{hru_area_colname}}) %>%
      #compute new area without NA and compute new proportions
      mutate({{hru_area_colname}} := rowSums(select(.,starts_with(proportion_col_prefix)))) %>% 
      mutate(across(starts_with(proportion_col_prefix),
                    ~(.x / {{hru_area_colname}}))) %>%
      #There are 3 HRUs with only NA land cover data (all estuary).
      # these result in NaN values for the proportions. Drop these.
      # Thankfully, there are other HRUs for these PRMS segments that are not NA.
      filter(!is.na(prop_lcClass_1))
  }
  
  return(final_df)
}

## -- Specific function for the NLCD reclassifying NLCD land cover classes

reclassify_LC_for_NLCD <- function(NLCD_lc_proportions_df,
                                   years_suffix,
                                   reclassify_table,
                                   remove_NA_cols = TRUE){

  #' @description placing the process of reclassifying the NLCD (2000 +) land cover df into this tailored function. Works with NLCD catchment attribute ACC, CAT, and TOT)
  #' @param NLCD_lc_proportions_df list of NLCD dataframes with lc classes as cols - output of proportion_lc_by_prms()
  #' @param years_suffix vectors of years (YY) of NLCD data - defined in _targets.R as NLCD_years_suffix
  #' @param reclassify_table reclassification lookup table for NLCD
  #' @param remove_NA_cols logical for removing the NA land cover class or not
  #' @value output is a list of land cover dfs split by year and updated with new cols representing the new classes
  
   df <- purrr::map(
     .x = years_suffix,
     ## first 
     .f = ~{NLCD_lc_proportions_df %>% select(PRMS_segid, AREASQKM_PRMS, contains(glue('NLCD', .x)))}) %>% 
    # Reclassify by individual year df
    purrr::map2(.x = .,
              .y = years_suffix,
              .f = ~{reclassify_land_cover(land_cover_df = .x,
                                           reclassify_table = reclassify_table,
                                           reclassify_table_lc_col = 'NLCD_value',
                                           reclassify_table_reclass_col = 'Reclassify_match',
                                           pivot_longer_contains = glue('NLCD',.y),
                                           remove_NA_cols = FALSE, 
                                           proportion_col_prefix = NULL, 
                                           hru_area_colname = NULL) %>% 
                  # some lc classes in NLCD were given NA ultimately - example: Alaska only shrub - we remove from table
                  {if(remove_NA_cols == TRUE) select(., -contains('NA')) else . } %>% 
                  # adding year column
                  mutate(Year = paste0('20',.y)) %>% 
                  # Renaming col names - removing the col to be consistent across dataframes
                  rename_with(.fn = function(x) sub("NLCD\\d+", "lcClass\\1", x), .cols = starts_with("prop_"))
              }
    )
  
  return(df)
  
}

## -- Specific aggregation steps for the already reclassified FORESCE dataset 
## Function that was previously in FORESCE_agg_lc_props.R
aggregate_proportions_hrus <- function(df, group_by_segment_colname, 
                                       proportion_col_prefix, hru_area_colname, 
                                       new_area_colname){
  #'@description aggregation function to get land cover class proportions for PRMS_catchment_area 
  #'@param df data frame to aggregate
  #'@param group_by_segment_colname colname for segments that will be used for the group by
  #'@param proportion_col_prefix prefix col for lc class proportion value columns
  #'@param hru_area_colname colname for hru area
  #'@param new_area_colname str. New colname for aggregated area
  #'@example aggregate_proportions_hrus(group_by_segment_colname = hru_segment, proportion_col_prefix = 'prop_lcClass', hru_area_colname = hru_area, new_area_colname = total_PRMS_area)

  df <- df %>%
    # Create temp cols of area of lc class per hru
    # NOTE we keep current col name while directly modifying the col values. Therefore, even though the colname stays the same, the values  are no longer proportion values. This is an intermediate step. 
    mutate(across(starts_with(proportion_col_prefix),
                  ~(.x * {{hru_area_colname}}))) %>%
    # group by hru segments - dropping to 390 to get a single "PRMS" catchment per PRMS segment
    group_by_at(group_by_segment_colname) %>%
    summarise(
      # calc total area of aggregated catchments
      total_area = sum({{hru_area_colname}}),
      # calc new proportion with new catchment area
      across(starts_with(proportion_col_prefix),
             ~(sum(.x)/total_area)), .groups = 'drop_last') %>%
    #rename col to name as input
    rename({{new_area_colname}} := total_area) %>% 
    ungroup()
  
  return(df)
}

