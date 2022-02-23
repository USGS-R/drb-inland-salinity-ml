create_nhdv2_attr_table <- function(attr_data_upstream,attr_data_catchment){
  #'
  #' @description Function to combine two lists containing cumulative upstream and catchment-scale
  #' NHDv2 attributes
  #' 
  #' @param attr_data_upstream list object containing NHDv2 attributes referenced to the cumulative upstream watershed
  #' @param attr_data_catchment list object containing NHDv2 attributes scaled to the local contributing catchment
  #' 
  #' @value returns a data frame with one row per PRMS segment and one column for each unique NHDv2 attribute variable
  #'
  
  # loop through both lists simultaneously and join data frames by PRMS_segid
  attr_data_df <- purrr::map2(attr_data_catchment, attr_data_upstream, full_join, by = "PRMS_segid") %>%
    # bind all columns containing into a single data frame
    Reduce(full_join,.) %>%
    # hide messages that data frames are being joined by column 'PRMS_segid'
    suppressMessages()
  
  return(attr_data_df)
  
}



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



process_catchment_nhdv2_attr <- function(file_path,vars_table,segs_w_comids,nhd_lines){
  #' 
  #' @description Function to read in downloaded NHDv2 attribute data and aggregate to river segment ID's
  #' based on the CAT aggregation operation defined in the VarsOfInterest table
  #'
  #' @param file_path file path of downloaded NHDv2 attribute data table, including file extension
  #' @param vars_table VarsOfInterest table
  #' @param segs_w_comids data frame containing the PRMS segment ids and the comids of interest
  #' segs_w_comids must contain variables PRMS_segid and COMID
  #' @param nhd_lines sf object containing NHDPlusV2 flowlines for area of interest
  #' nhd_lines must contain variables COMID,AREASQKM, and LENGTHKM
  #'
  #' @value A data frame containing PRMS_id and columns representing the NHDv2 attribute data scaled to the 
  #' local PRMS catchment. 
  #' 
  
  # 1. Parse dataset name from file_path
  data_name <- str_split(basename(file_path),".[[:alnum:]]+$")[[1]][1]
  
  # 2. Format inputs {nhd_lines} and {segs_w_comids} 
  nhd_reaches <- nhd_lines %>%
    sf::st_drop_geometry() %>%
    select(COMID,AREASQKM,LENGTHKM) %>%
    mutate(COMID = as.character(COMID))
  
  segs_w_comids <- segs_w_comids %>%
    rename(COMID = grep("comid",names(segs_w_comids),ignore.case = TRUE,value = TRUE))
  
  # 3. Read in downloaded data file
  # only specify col_type for COMID since cols will differ for each downloaded data file
  dat <- read_csv(file_path, col_types = cols(COMID = "c"), show_col_types = FALSE)
  
  # For PPT data we want to return the long-term (1971-2000) monthly averages 
  # instead of the monthly values for each year
  if(grepl("PPT_CAT",file_path)){
    message("Calculating long-term monthly average precipitation from annual data")
    dat <- calc_monthly_avg_ppt(dat)
  }
  
  # 4. Identify columns of interest
  # Subset VarsOfInterest table to retain the desired dataset {data_name} 
  vars_item <- vars_table %>%
    filter(Dataset_name == data_name) 
  
  col_names <- tibble(col_name = vars_item$Catchment.Item.Name,
                      cat_agg_op = vars_item$CAT_aggregation_operation) %>%
    filter(!is.na(col_name)) 
  
  # Reformat col_names for certain datasets with year suffixes on column names
  # Function `format_col_names_years` sourced from 1_fetch/fetch_nhdv2_attributes_from_sb.R
  # National Inventory of Dams data:
  if(unique(vars_item$sb_id) == "58c301f2e4b0f37a93ed915a"){
    years <- unique(str_extract(names(dat),"\\d{2,}"))
    years <- years[!is.na(years)]
    cols <- format_col_names_years(col_names$col_name,years,yr_pattern = "YYYY")
    cols <- cols[cols != "COMID"]
    col_names <- tibble(col_name = cols,
                        cat_agg_op = rep(vars_item$CAT_aggregation_operation,length(years))) 
  }
  
  # HDENS data:
  if(unique(vars_item$sb_id) == "5910de31e4b0e541a03ac983"){
    years <- unique(str_extract(names(dat),"\\d{2,}"))
    years <- years[!is.na(years)]
    cols <- format_col_names_years(col_names$col_name,years,yr_pattern = "XX")
    cols <- cols[cols != "COMID"]
    col_names <- tibble(col_name = cols,
                        cat_agg_op = rep(vars_item$CAT_aggregation_operation,length(years)))
  }
  
  # monthly average precipitation data:
  if(unique(vars_item$sb_id) %in% c("5734acafe4b0dae0d5de622d")){
    col_names$col_name <- names(dat)[names(dat) != "COMID"]
  }
  
  # Separate columns based on the aggregation operation we need to scale the NHD-catchment-values to PRMS-scale values
  cols_sum <- col_names$col_name[col_names$cat_agg_op=="sum"]
  cols_area_wtd_mean <- col_names$col_name[col_names$cat_agg_op=="area_weighted_mean"]
  cols_min <- col_names$col_name[col_names$cat_agg_op=="min"]
  cols_max <- col_names$col_name[col_names$cat_agg_op=="max"]
  
  # 5. Munge downloaded data
  dat_proc <- dat %>%
    # retain CAT columns from NHDv2 attributes dataset
    select(c(COMID,starts_with("CAT"))) %>%
    # join data to {segs_w_comids} data frame by COMID
    right_join(.,segs_w_comids,by=c("COMID")) %>%
    # join data to {nhd_reaches} by COMID since we need the AREASQKM and LENGTHKM attributes not
    # included in the downloaded NHDv2 attribute datasets
    left_join(.,nhd_reaches,by="COMID") %>%
    relocate("PRMS_segid",.before="COMID") 
  
  # Flag columns with undesired flag values (e.g. -9999)
  flag_cols <- dat_proc %>%
    select(where(function(x) -9999 %in% x)) %>% 
    names()
  
  # For columns with undesired flag values, replace -9999 with NA, else use existing value
  dat_proc_out <- dat_proc %>%
    mutate(across(all_of(flag_cols), ~case_when(. == -9999 ~ NA_real_, TRUE ~ as.numeric(.))))
  
  # 6. Scale NHDv2 attributes to PRMS catchments
  dat_proc_aggregated <- dat_proc_out %>%
    # summarize the data for each unique PRMS_segid
    group_by(PRMS_segid) %>%
    # approximate NHDv2 catchment area for all COMID's where AREASQKM equals zero
    mutate(AREASQKM_approx = case_when(AREASQKM == 0 ~ LENGTHKM^2, TRUE ~ AREASQKM)) %>%
    # apply desired aggregation operations to appropriate columns
    summarize(
      AREASQKM_PRMS = sum(AREASQKM_approx), 
      across(any_of(cols_area_wtd_mean), weighted.mean, w = AREASQKM_approx, na.rm = T, .names = "{col}_area_wtd"),
      across(any_of(cols_sum), sum, na.rm = T, .names = "{col}_sum"),
      across(any_of(cols_min), min, na.rm = T, .names = "{col}_min"),
      across(any_of(cols_max), max, na.rm = T, .names = "{col}_max")) 
  
  
  return(dat_proc_aggregated)
  
}


