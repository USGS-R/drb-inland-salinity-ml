clean_lulc_data_for_merge <- function(lulc_dfs,  area_col, area_unit = 'km2', cols_to_remove = NULL){
  
  #' @description clean+munge function for NLCD and FORESCE lulc datasets before combined together
  #' @param lulc_dfs list or single df. with proportions of land cover class 
  #' @param area_col Col name for area of PRMS column.   
  #' @param area_unit unit of the area col. Default = km2. Options for area metrics (m2, km2, hectares, acres). If other, please edit function and add this case.
  #' @param cols_to_remove  Name of 1 or several columns that should be removed for the final df. Quotes needed. Default Null.
  #' @example clean_lulc_data_for_merge(FORESCE_data_years_list, cols_to_remove = hru_segment, area_col = total_PRMS_area, area_unit = 'm2')
  #' @value Single dataframe combine all years of the original list. Unit of area col should be km2.
  
  clean_dflist_element <- function(x){
    
    x <- x %>%
      # note - renaming before units are change
      rename(., PRMS_area_km2 = {{area_col}}) %>%
      # area col unit conversion to km2 using case when. Becomes NA if no area unit is provided. 
      mutate(PRMS_area_km2 = case_when(area_unit == 'm2' ~ (PRMS_area_km2 / 1000000),
                                       area_unit == 'km2' ~ PRMS_area_km2,
                                       ## other area unit options added here:
                                       area_unit == 'hectares' | area_unit == 'ha' ~ PRMS_area_km2/100,
                                       area_unit == 'acres' | area_unit == 'ac' ~ PRMS_area_km2/247.11,
                                       TRUE ~ as.numeric(NA))) %>%
      # reorder columns PRMS_segid and Year
      select(PRMS_segid, PRMS_area_km2, Year, everything())
    
    if(!is.null(cols_to_remove)){
      # remove unwanted cols if cols_to_remove exists
      x <- x %>% select(-{{cols_to_remove}})
    }
      
    return(x)
      
  }
  
  # Check if needs to be converted to list or not to apply lapply - Note might want to avoid the lapply + do.call process if not list
  if(!is.list(lulc_dfs)){
    print('Input lulc dfs is not list, No list.rbind computed.')
    df_list <- list(lulc_dfs)
  } else {df_list <- lulc_dfs}

  # loop through list and run clean_dflist_element function 
  df_list_cleaned <- lapply(df_list, function(x) clean_dflist_element(x))
  
  # returning rbinded list
  return(do.call('rbind', df_list_cleaned))

}


  
  
  
  
  
  
  
