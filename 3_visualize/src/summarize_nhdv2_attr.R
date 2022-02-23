summarize_nhdv2_attr <- function(attr_data,fileout){
  #' 
  #' @description This function summarizes each of the downloaded NHDv2 attribute variables across all river segments within the network
  #'
  #' @param attr_data data frame containing the processed NHDv2 attribute data;
  #' must include column "PRMS_segid"
  #' @param fileout a character string that indicates the name of the output file, including path and extension
  #'
  #' @value Returns a csv file containing summary statistics for each NHDv2 attribute variable
  

  # Define function to summarize the number of NA's in numeric vector x
  # The argument `na.rm = TRUE` is included so that the `summarize_at` line will run in the code
  # chunk below that generates `attr_summary`; it does not remove NA values in `num_NA()`
  num_NA <- function(x,na.rm=TRUE){
    sum(is.na(x))
  }
  
  # Calculate summary statistics for each variable's time series
  attr_summary <- attr_data %>%
    select(where(is.numeric)) %>%
    pivot_longer(everything()) %>%
    group_by(name) %>%
    summarize_at(vars(value), list(Min = min, Mean = mean, Max = max, Sd = sd, num_NA = num_NA),na.rm=TRUE) %>%
    mutate_if(is.numeric, round, 3)
  
  # Save data summary
  readr::write_csv(attr_summary,fileout)
  
  return(fileout) 
  
}
