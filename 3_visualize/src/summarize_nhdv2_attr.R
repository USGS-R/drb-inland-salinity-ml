summarize_nhdv2_attr <- function(attr_data,fileout){
  #' 
  #' @description This function summarizes each of the downloaded NHDv2 attribute variables across all river segments within the network
  #'
  #' @param attr_data list containing the processed NHDv2 attribute data; data frames must include column "PRMS_segid" 
  #' @param fileout a character string that indicates the name of the output file, including path and extension
  #'
  #' @value Returns a csv file containing summary statistics for each NHDv2 attribute variable
  
  # Bind all columns representing unique NHDv2 attribute variables
  attr_data_df <- attr_data %>%
    Reduce(full_join,.) %>%
    # hide messages that data frames are being joined by column 'PRMS_segid'
    suppressMessages()
  
  # Define function to summarize the number of NA's in numeric vector x
  num_NA <- function(x){
    sum(is.na(x))
  }
  
  # Calculate summary statistics for each variable's time series
  attr_summary <- attr_data_df %>%
    select(where(is.numeric)) %>%
    pivot_longer(everything()) %>%
    group_by(name) %>%
    summarise_at(vars(value), list(Min = min, Mean = mean, Max = max, Sd = sd, num_NA = num_NA)) %>%
    mutate_if(is.numeric, round, 3)
  
  # Save data summary
  readr::write_csv(attr_summary,fileout)
  
  return(fileout) 
  
}
