calc_monthly_avg_ppt <- function(ppt_data){
  #' 
  #' @description Function to calculate long-term monthly average precipitation for each NHDPlusV2 reach, represented by a unique COMID
  #'
  #' @param ppt_data data frame containing monthly precipitation data for all COMIDs of interest; downloaded from ScienceBase.
  #' Note that we are expecting the years represented in ppt_data to span the 30-year period including 1971-2000, and 
  #' this function will error out if years before 1971 or after 2000 are detected. 
  #'
  #' @value returns a data frame with one row per COMID and one column to contain each of 12 long-term monthly averages
  #'
  
  # Check that years represented in PPT column names include the time period we were expecting
  years <- ppt_data %>%
    names(.) %>%
    str_extract(.,"[[\\d]]+") %>%
    unique(.) %>%
    as.numeric(.) %>%
    range(.,na.rm=TRUE)
  
  if(years[1] < 1971 | years[2] > 2000){
    stop(paste("For PPT data, we expected years 1971-2000 but the downloaded data contains other years: ",
               paste(years,collapse= ' - '),
               ". In 1_fetch/in/NHDVarsOfInterest.csv, check column sb_item_retrieve for Dataset_name PPT_CAT, PPT_TOT, and PPT_ACC."))
  }
  
  # Group columns by month and calculate the long-term monthly average precipitation
  # return a data frame with one row per COMID and one column to contain each of 12 long-term monthly averages
  ppt_data_summarized <- ppt_data %>%
    pivot_longer(!COMID, names_to = "month_yr", values_to = "value") %>%
    mutate(month = str_replace(month_yr,"[[\\d]]+","")) %>%
    group_by(COMID,month) %>%
    summarize(mean_monthly_ppt = mean(value,na.rm = TRUE),
              .groups="drop") %>%
    pivot_wider(names_from = month, values_from = mean_monthly_ppt)
  
  return(ppt_data_summarized)
  
}



process_cumulative_nhdv2_attr <- function(file_path,segs_w_comids,cols){
  #' 
  #' @description Function to read in downloaded NHDv2 attribute data and join with river segment ID's
  #'
  #' @param file_path file path of downloaded NHDv2 attribute data table, including file extension
  #' @param segs_w_comids data frame containing the PRMS segment ids and the comids of interest
  #' segs_w_comids must contain variables PRMS_segid and COMID
  #' @param cols character string indicating which columns to retain from downloaded attribute data; 
  #' cols can take values "ACC" or "TOT"
  #'
  #' @value A data frame containing PRMS_id and columns representing the NHDv2 attribute data referenced to the 
  #' cumulative upstream watershed.
  #' 

  # Read in downloaded data 
  # only specify col_type for COMID since cols will differ for each downloaded data file
  dat <- read_csv(file_path, col_types = cols(COMID = "c"), show_col_types = FALSE)
  
  # For PPT data we want to return the long-term (1971-2000) monthly averages 
  # instead of the monthly values for each year
  if(grepl("PPT_TOT",file_path)|grepl("PPT_ACC",file_path)){
    message("Calculating long-term monthly average precipitation from annual data")
    dat <- calc_monthly_avg_ppt(dat)
  }
    
  # Process downloaded data
  dat_proc <- dat %>%
    # retain desired columns ('ACC' or 'TOT')
    select(c(COMID,starts_with(cols))) %>%
    # join data to {segs_w_comids} data frame by COMID
    right_join(.,segs_w_comids,by=c("COMID"="comid")) %>%
    relocate("PRMS_segid",.before="COMID") %>%
    select(-COMID)
  
  # Flag columns with undesired flag values (e.g. -9999)
  flag_cols <- dat_proc %>%
    select(where(function(x) -9999 %in% x)) %>% 
    names()
  
  # For columns with undesired flag values, replace -9999 with NA, else use existing value
  dat_proc_out <- dat_proc %>%
    mutate(across(all_of(flag_cols), ~case_when(. == -9999 ~ NA_real_, TRUE ~ as.numeric(.))))

  return(dat_proc_out)
  
}

