reclassify_land_cover <- function(land_cover_df,
                                  reclassify_table_csv_path, sep = ',',
                                  reclassify_table_lc_col,
                                  reclassify_table_reclass_col,
                                  pivot_longer_contains){
  
  #' @description Reclassify land cover df with cols as individual lc classes to pre-defined land cover class
  #' @param land_cover_df dataframe with lc classes as cols 
  #' @param reclassify_table_csv_path path the reclassifilication table that is read in the function
  #' @param reclassify_table_lc_col col that contains original classes of the land_cover_dfdataframe
  #' @param reclassify_table_reclass_col col in reclassify csv that cobtains the new land cover classes 
  #' @param sep separator in reclassify_table
  #' @param pivot_longer_contains common colname str found in land_cover_df land cover cols (e.g. 'lcClass' or 'NLCDClass')
  #' @value output is a updated version of the land_cover_df with new cols representing the new classes 
  
  # Load reclassification csv only taking class values, not description cols
  reclassify_table <- read.csv(reclassify_table_csv_path, sep = sep) %>% 
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
  final_df <- pivot_wider(new_classes_df, names_from = new_class, names_prefix = 'prop_', values_from = Prop_class_in_catchment, values_fn = sum)
  
  return(final_df)
  
}

## -- Specific function for the NLCD reclassifying NLCD land cover classes

reclassify_LC_for_NLCD <- function(NLCD_lc_proportions_df,
                                   years_suffix,
                                   reclassify_table_csv_path = '1_fetch/in/Legend_NLCD_Land_Cover.csv'){

  #' @description placing the process if reclassifying  the NLCD (2000 +) land cover df in a tailored function (works with NLCD catchment att Cat and Tot)
  #' @param NLCD_lc_proportions_df list of NLCD dataframes with lc classes as cols - output of proportion_lc_by_prms()
  #' @param years_suffix vectors of years (YY) of NLCD data - defined in _targets.R as NLCD_years_suffix 
  #' @param reclassify_table_csv_path path to reclassification lookup table for NLCD. Default path from pipeline
  #' @value output is a list of land cover dfs split by year and updated with new cols representing the new classes 
  
   df <- purrr::map(
     .x = years_suffix,
     ## first 
     .f = ~{NLCD_lc_proportions_df %>% select(PRMS_segid, AREASQKM_PRMS, contains(glue('NLCD', .x)))}) %>% 
    # Reclassify by individual year df
    purrr::map2(.x = .,
              .y = years_suffix,
              .f = ~{reclassify_land_cover(land_cover_df = .x,
                                           reclassify_table_csv_path = reclassify_table_csv_path,
                                           reclassify_table_lc_col = 'NLCD_value',
                                           reclassify_table_reclass_col = 'Reclassify_match',
                                           sep = ',',
                                           pivot_longer_contains = glue('NLCD',.y)) %>% 
                  # some lc classes in NLCD were given NA ultimately - example: Alaska only shrub - we remove from table
                  select(-contains('NA')) %>%
                  # adding year column
                  mutate(Year = paste0('20',.y)) %>% 
                  # Renaming col names - removing the col to be consistent across dataframes
                  rename_with(.fn = function(x) sub("NLCD\\d+", "lcClass\\1", x), .cols = starts_with("prop_"))
              }
    )
  return(df)
}

