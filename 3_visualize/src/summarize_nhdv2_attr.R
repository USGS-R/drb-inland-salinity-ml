summarize_nhdv2_attr <- function(attr_data,fileout){
  #' 
  #' @description This function summarizes each of the downloaded NHDv2 attribute variables across all river segments within the network
  #'
  #' @param attr_data data frame containing the processed NHDv2 attribute data;
  #' must include column "PRMS_segid"
  #' @param fileout a character string that indicates the name of the output file, including path and extension
  #'
  #' @value Returns a csv file containing summary statistics for each NHDv2 attribute variable
  #' 
  
  # Calculate summary statistics for each variable's time series
  attr_summary <- attr_data %>%
    select(where(is.numeric)) %>%
    pivot_longer(everything()) %>%
    group_by(name) %>%
    summarize_at(vars(value), list(Min = min, Mean = mean, Max = max, Sd = sd),na.rm=TRUE) %>%
    mutate_if(is.numeric, round, 3)
  
  # Save data summary
  readr::write_csv(attr_summary,fileout)
  
  return(fileout) 
  
}




summarize_catchment_nhdv2_attr_missing <- function(attr_data_catchment,fileout){
  #'
  #' @description Function to summarize the number of flagged values (-9999) and the 
  #' proportion of catchment area represented by flagged values for each PRMS segment
  #' and NHDv2 attribute variable.
  #' 
  #' @param attr_data_catchment list object containing NHDv2 attributes scaled to the local contributing catchment;
  #' attr_data_catchment should be a nested list such that the first element represents a unique attribute dataset
  #' and the second element represents data frames containing the aggregated datasets and the NA diagnostics for
  #' the attribute dataset. 
  #' @param fileout a character string that indicates the name of the output file, including path and extension
  #' 
  #' @value returns a data frame with one row per PRMS segment and one column for each unique NHDv2 attribute variable
  #' containing flagged values
  
  # For the attribute variables scaled to the PRMS catchment-scale, select the list elements containing the 
  # NA diagnostics information and omit the list elements containing the aggregated data
  attr_data_catchment_missing <- lapply(attr_data_catchment, "[[", 2)
  
  # Return missing data information for NHDv2 attributes with flagged values (-9999) or NA values
  attr_missing_data_summary <- attr_data_catchment_missing %>%
    # bind all columns containing NA diagnostics for NHDv2 attributes into a single data frame
    Reduce(full_join,.) %>%
    # hide messages that data frames are being joined by columns 'PRMS_segid', 'AREASQKM_PRMS', and 'num_NHDv2cats"
    suppressMessages()
  
  # Save data summary
  readr::write_csv(attr_missing_data_summary,fileout)
  
  return(fileout) 
  
}


