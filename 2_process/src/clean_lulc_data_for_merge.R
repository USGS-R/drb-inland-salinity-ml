clean_dflist_element <- function(x, area_col, area_unit, cols_to_remove, prefix = NULL){
  
  #' @description clean+munge function for single NLCD or FORESCE df  
  #' @param x df   
  #' @param area_col Col name for area of area column.   
  #' @param area_unit unit of the area col. Default = km2. Options for area metrics (m2, km2, hectares, acres). If other, please edit function and add this case.
  #' @param columns_to_remove  Name of 1 or several columns that should be removed for the final df. Quotes needed. Default Null.
  #' @example clean_dflist_element(lulc_dfs[[1]]) 
  #' @value cleaned df 
  
  x <- x %>%
    # note - renaming before units are change
    rename(., PRMS_area_km2 = all_of(area_col)) %>%
    # area col unit conversion to km2 using case when. Becomes NA if no area unit is provided. 
    mutate(PRMS_area_km2 = case_when(area_unit == 'm2' ~ (PRMS_area_km2 / 1000000),
                                     area_unit == 'km2' ~ PRMS_area_km2,
                                     ## other area unit options added here:
                                     area_unit == 'hectares' | area_unit == 'ha' ~ PRMS_area_km2/100,
                                     area_unit == 'acres' | area_unit == 'ac' ~ PRMS_area_km2/247.11,
                                     TRUE ~ as.numeric(NA))) %>%
    # reorder columns PRMS_segid and Year
    select(PRMS_segid, PRMS_area_km2, Year, everything())
  
  ## adding prefix to prop columns
  if(!is.null(prefix)){
  colnames(x)[startsWith(colnames(x), prefix =  'prop')] <- paste(prefix, colnames(x)[startsWith(colnames(x), prefix =  'prop')], sep = '_') 
  }
  
  if(!is.null(cols_to_remove)){
    # remove unwanted cols if cols_to_remove exists
    x <- x %>% select(-all_of(cols_to_remove))
  }
  
  return(x)
  
}


clean_lulc_data_for_merge <- function(lulc_dfs,  prms_area_col, prms_area_unit = 'km2', columns_to_remove = NULL, prop_prefix = NULL){
  
  #' @description wrapper function for clean_dflist_element() which clean+munge the NLCD and FORESCE lulc datasets before combined together
  #' @param lulc_dfs list or single df. with proportions of land cover class 
  #' @param prms_area_col PRMS column.   
  #' @param prms_area_unit unit of the area col. Default = km2. Options for area metrics (m2, km2, hectares, acres). If other, please edit function and add this case.
  #' @param columns_to_remove  Name of 1 or several columns that should be removed for the final df. Quotes needed. Default Null.
  #' @example clean_lulc_data_for_merge(FORESCE_data_years_list, cols_to_remove = hru_segment, area_col = total_PRMS_area, area_unit = 'm2')
  #' @value Single dataframe combine all years of the original list. Unit of area col should be km2.
  
  # In case lulc_dfs is 1 df, Check if needs to be converted to list or not to apply lapply - Note might want to avoid the lapply + do.call process all together if not list
  if(!is.list(lulc_dfs)){
    print('Input lulc dfs is not list, No list.rbind computed.')
    df_list <- list(lulc_dfs)
  } else {df_list <- lulc_dfs}

  # loop through list and run clean_dflist_element function 
  df_list_cleaned <- lapply(df_list, function(x) clean_dflist_element(x, area_col = prms_area_col, area_unit = prms_area_unit, cols_to_remove = columns_to_remove, prefix = prop_prefix))
  
  # returning rbinded list
  return(do.call('rbind', df_list_cleaned))

}


  
  
  
  
  
