create_nhdv2_attr_table <- function(attr_data_upstream,attr_data_catchment){
  #'
  #' @description Function to combine two lists containing cumulative upstream and catchment-scale
  #' NHDv2 attributes
  #' 
  #' @param attr_data_upstream list object containing NHDv2 attributes referenced to the cumulative upstream watershed
  #' @param attr_data_catchment list object containing NHDv2 attributes scaled to the local contributing catchment;
  #' attr_data_catchment should be a nested list such that the first element represents a unique attribute dataset
  #' and the second element represents data frames containing the aggregated datasets and the NA diagnostics for
  #' the attribute dataset. 
  #' 
  #' @value returns a data frame with one row per PRMS segment and one column for each unique NHDv2 attribute variable
  #'
  
  # For the attribute variables scaled to the PRMS catchment-scale, select the list elements containing the data and 
  # omit the list elements containing the NA diagnostics information. 
  attr_data_catchment_dat <- lapply(attr_data_catchment, "[[", 1)
  
  # loop through both lists simultaneously and join data frames by PRMS_segid
  attr_data_df <- purrr::map2(attr_data_catchment_dat, attr_data_upstream, full_join, by = "PRMS_segid") %>%
    # bind all columns containing NHDv2 attributes into a single data frame
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
  
  #Replace -9999 with NA
  if(any(ppt_data == -9999, na.rm = T)){
    ppt_data[ppt_data == -9999] <- NA_real_
  }
  
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


calc_avg_NADP <- function(NADP_data){
  #' 
  #' @description Function to calculate long-term average NADP for each NHDPlusV2 reach, 
  #' represented by a unique COMID
  #'
  #' @param NADP_data data frame containing annual NADP data for all COMIDs of interest; 
  #' downloaded from ScienceBase.Note that we are expecting the years represented 
  #' in NADP_data to span the 30-year period including 1985-2014, and 
  #' this function will error out if years before 1985 or after 2014 are detected. 
  #'
  #' @value returns a data frame with one row per COMID and one column to contain each of 
  #' long-term monthly averages
  #'
  
  #Replace -9999 with NA
  if(any(NADP_data == -9999)){
    NADP_data[NADP_data == -9999] <- NA_real_
  }
  
  # Check that years represented in NADP column names include the time period we were expecting
  years <- NADP_data %>%
    names(.) %>%
    str_extract(.,"[[\\d]]+") %>%
    unique(.) %>%
    as.numeric(.) %>%
    range(.,na.rm=TRUE)
  # Error out if year is not in the 30 year period between 1985 and 2014.  
  if(years[1] < 1985 | years[2] > 2014){
    stop(paste("For NADP data, we expected years 1985-2014 but the downloaded data contains other years: ",
               paste(years,collapse= ' - '),
               ". In 1_fetch/in/NHDVarsOfInterest.csv, check column sb_item_retrieve for Dataset_name NADP."))
  }
  
  # Group columns by constituent and calculate the long-term average deposition
  # return a data frame with one row per COMID and one column to contain each of 12 long-term monthly averages
  NADP_data_summarized <- NADP_data %>%
    pivot_longer(!COMID, names_to = "cons_yr", values_to = "value") %>%
    mutate(cons = str_replace(cons_yr,"_[[\\d]]+","")) %>%
    group_by(COMID,cons) %>%
    summarize(mean_NADP = mean(value, na.rm = TRUE),
              .groups="drop") %>%
    pivot_wider(names_from = cons, values_from = mean_NADP)
  
  return(NADP_data_summarized)
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

  message(file_path)
  # Read in downloaded data 
  # only specify col_type for COMID since cols will differ for each downloaded data file
  dat <- read_csv(file_path, col_types = cols(COMID = "c"), show_col_types = FALSE)
  
  # For PPT data we want to return the long-term (1971-2000) monthly averages 
  # instead of the monthly values for each year
  if(grepl("PPT_TOT",file_path)|grepl("PPT_ACC",file_path)){
    message("Calculating long-term monthly average precipitation from annual data")
    dat <- calc_monthly_avg_ppt(dat)
  }
  
  # For NADP data we want to return the long-term (1984-2014) average 
  # instead of annual values for each consitituent
  if(grepl("NADP",file_path)){
    message("Calculating long-term average NADP from annual data")
    dat <- calc_avg_NADP(dat)
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
  #' @value Returns a list that includes a data table containing the PRMS_id and columns representing the NHDv2 
  #' attribute data scaled to the local PRMS catchment, and a data table containing NA diagnostic information
  #' for each variable and PRMS segment.
  #' 
  
  message(file_path)
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
  
  # For NADP data we want to return the long-term (1985-2014) average 
  # instead of annual values
  if(grepl("NADP",file_path)){
    message("Calculating long-term average NADP from annual data")
    dat <- calc_avg_NADP(dat)
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
  
  # NADP data:
  if(unique(vars_item$sb_id) == "57e2ac2fe4b0908250045981"){
    col_names$col_name <- str_replace(col_names$col_name, pattern = "_YYYY", 
                                      replacement = "")
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
    # approximate NHDv2 catchment area for all COMID's where AREASQKM equals zero
    mutate(AREASQKM_approx = case_when(AREASQKM == 0 ~ LENGTHKM^2, TRUE ~ AREASQKM)) %>%
    # format columns
    relocate("PRMS_segid",.before="COMID") 
  
  # 5b. Handle missing values and flagged values
  # Flag columns with undesired flag values (e.g. -9999)
  flag_cols <- dat_proc %>%
    select(where(function(x) -9999 %in% x | any(is.na(x)))) %>% 
    names()
  
  # Before replacing flagged values, tally the number of -9999's as well as the 
  # proportion of total NHD area where the value is -9999 to use for diagnostics
  flag_tally <- dat_proc %>%
    group_by(PRMS_segid) %>%
    summarize(
      AREASQKM_PRMS = sum(AREASQKM_approx), 
      num_NHDv2cats = length(unique(COMID)),
      across(all_of(flag_cols), ~length(which(. == -9999 | is.na(.))), .names = "{col}_num_NA"),
      across(all_of(flag_cols), ~round(sum(AREASQKM_approx[which(. == -9999 | is.na(.))]/AREASQKM_PRMS), 4), .names = "{col}_propAREA_NA")
    ) 
  
  # 5c. For columns with undesired flag values, replace -9999 with NA, else use existing value
  
  # For STATSGO variables related to HYDGRP, TEXT, and LAYER, the metadata indicate 
  # that -9999 denotes NODATA usually water. For these soils variables only, 
  # change -9999 values to 0.
  if(unique(vars_item$sb_id) %in% c("5728d93be4b0b13d3918a99f","5728decfe4b0b13d3918a9aa","5728dd46e4b0b13d3918a9a7")){
    dat_proc_out <- dat_proc %>%
      mutate(across(all_of(flag_cols), ~case_when(. == -9999 ~ 0, TRUE ~ as.numeric(.))))
  } else {
    dat_proc_out <- dat_proc %>%
      mutate(across(all_of(flag_cols), ~case_when(. == -9999 ~ NA_real_, TRUE ~ as.numeric(.))))
  }
  
  # 6. Scale NHDv2 attributes to PRMS catchments
  dat_proc_aggregated <- dat_proc_out %>%
    # summarize the data for each unique PRMS_segid
    group_by(PRMS_segid) %>%
    # apply desired aggregation operations to appropriate columns
    summarize(
      AREASQKM_PRMS = sum(AREASQKM_approx), 
      across(any_of(cols_area_wtd_mean), weighted.mean, w = AREASQKM_approx, na.rm = T, .names = "{col}_area_wtd"),
      across(any_of(cols_sum), sum, na.rm = T, .names = "{col}_sum"),
      across(any_of(cols_min), min, na.rm = T, .names = "{col}_min"),
      across(any_of(cols_max), max, na.rm = T, .names = "{col}_max")) %>%
    # For segments where all values of a column were NA (thus generating NAN's), replace NAN with NA
    mutate(across(where(is.numeric), ~if_else(is.nan(.),NA_real_,.)))
    
  # 7. Return a list containing the aggregated data table and a table with NA diagnostics for each 
  # attribute and segment ID.
  dat_proc_list <- list(data = dat_proc_aggregated, NA_diagnostics = flag_tally)
  
  
  return(dat_proc_list)
  
}


refine_features <- function(nhdv2_attr, prms_nhdv2_xwalk, 
                            nhdv2_reaches, prms_attribute_df, drop_columns){
  #' 
  #' @description Function to reduce and refine the static attributes for use in models.
  #' It drops features that have the same value for all reaches.
  #' It drops columns specified in drop_columns
  #' It fills in 0 area PRMS areas with NHD areas.
  #' It fills in NA values from immediately neighboring reaches
  #' It computes stream density from the NHD catchments in the PRMS HRU
  #' It computes TOT from CAT variables for 4 attributes.
  #'
  #' @param nhdv2_attr the tbl of static attributes (columns) for each PRMS reach (rows)
  #' @param prms_nhdv2_xwalk the crosswalk tbl from NHD reaches to PRMS reaches
  #' @param nhdv2_reaches the NHD reaches as an sf object
  #' @param prms_attribute_df PRMS attribute table with all_from_segs discovered 
  #' from recursive function (output of p2_prms_attribute_df target)
  #' @param drop_columns character vector of column names to remove from nhdv2_attr
  #' 
  #' @value Returns a refined nhdv2_attr based on the columns to drop
  
  #Detect variables that are all equal across the modeling domain and remove them
  #removes "BEDPERM_4" and "HGAC" for both CAT and TOT scales
  unique_col_vals <- apply(nhdv2_attr, 2, FUN = function(x) length(unique(x)))
  nhdv2_attr_refined <- nhdv2_attr[, which(unique_col_vals > 1)] %>%
    #Remove other columns
    select(!contains(drop_columns)) %>%
    #Modify the CAT Basin Areas that are 0 with PRMS areas
    #These areas are otherwise nearly identical (max difference of 0.1 sq.km)
    mutate(CAT_BASIN_AREA_sum = case_when(CAT_BASIN_AREA_sum == 0 ~ AREASQKM_PRMS,
                                          TRUE ~ CAT_BASIN_AREA_sum)) %>%
    #drop PRMS area column
    select(-AREASQKM_PRMS)
  
  #RECHG
  #Change recharge for NA segment to the average of its neighbors (from_segs and to_seg)
  #index to change
  nhdv2_attr_refined$CAT_RECHG_area_wtd <- refine_from_neighbors(nhdv2_attr_refined,
                                                                 attr_i = 'CAT_RECHG_area_wtd', 
                                                                 prms_attribute_df)
  
  #EWT - water table
  #Change EWT for segment with deep value to the average of its neighbors (from_segs and to_seg)
  nhdv2_attr_refined$CAT_EWT_area_wtd <- refine_from_neighbors(nhdv2_attr_refined,
                                                               attr_i = 'CAT_EWT_area_wtd',
                                                               prms_attribute_df)
  
  #STRM_DENS
  #Compute stream density from the NHD catchment reach length and area
  #only for the 5 NA PRMS segments. These have 1 or 2 NHD catchments.
  # other PRMS segments with some NA stream densities cover areas <3% of total area.
  #Gather the PRMS areas for these reaches
  ind_areas <- filter(nhdv2_attr_refined, is.na(CAT_STRM_DENS_area_wtd)) %>%
    select(PRMS_segid, CAT_BASIN_AREA_sum)
  #Gather the sum of NHD reach lengths in km
  ind_areas$length_km <- 0
  for (i in 1:nrow(ind_areas)){
    #all NHD reaches for this PRMS segment
    nhd_reaches <- filter(prms_nhdv2_xwalk,
                          PRMS_segid %in% ind_areas$PRMS_segid[i]) %>%
      select(comid_cat) %>%
      str_split(., pattern = ';', simplify = T)
    
    ind_areas$length_km[i] <- filter(nhdv2_reaches,
                                    COMID %in% nhd_reaches) %>%
      select(LENGTHKM) %>% 
      st_drop_geometry() %>%
      sum()
  }
  #Compute the reach stream density length (km)/area (sq.km)
  #There must be a typo in the ScienceBase table's units because using m length gives
  #results that are 3 orders of magnitude larger than other values
  ind_areas <- mutate(ind_areas, str_dens = length_km/CAT_BASIN_AREA_sum) %>%
    select(-length_km, -CAT_BASIN_AREA_sum)
  #assign to attribute table
  nhdv2_attr_refined <- mutate(nhdv2_attr_refined,
                          CAT_STRM_DENS_area_wtd = case_when(PRMS_segid %in% ind_areas$PRMS_segid ~
                                                               ind_areas$str_dens[match(PRMS_segid, ind_areas$PRMS_segid)],
                                                             TRUE ~ CAT_STRM_DENS_area_wtd)
  )
  
  #Compute TOT variables from PRMS CAT variables
  # These 3 variables have some segments with 0s that are clearly incorrect:
  # CWD, TAV7100, TMIN7100 
  # and TOT for STRM_DENS should be recomputed due to the edits to CAT variables
  #Add hru segment identifier to match with the recursive function output ID
  nhdv2_attr_refined$hru_segment <- apply(str_split(nhdv2_attr_refined$PRMS_segid, 
                                                    pattern = '_', simplify = T), 
                                          MARGIN = 1, FUN = first)
  #edit identifier for split segments
  nhdv2_attr_refined <- mutate(.data = nhdv2_attr_refined, 
         hru_segment = case_when(PRMS_segid == '3_1' ~ '3_1',
                                 PRMS_segid == '3_2' ~ '3_2',
                                 PRMS_segid == '8_1' ~ '8_1',
                                 PRMS_segid == '8_2' ~ '8_2',
                                 PRMS_segid == '51_1' ~ '51_1',
                                 PRMS_segid == '51_2' ~ '51_2',
                                 TRUE ~ hru_segment))
  
  TOT_frmCAT_cols <- c('TOT_CWD_frmCAT', 'TOT_TAV7100_ANN_frmCAT', 
                       'TOT_TMIN7100_frmCAT', 'TOT_STRM_DENS_frmCAT')
  nhdv2_attr_refined[TOT_frmCAT_cols] <- NA
  #loop over all segments
  for(i in 1:length(unique(nhdv2_attr_refined$hru_segment))){
    #current segment
    subseg <- unique(nhdv2_attr_refined$hru_segment)[i]
    #all upstream segments, including current segment
    ind_segs <- filter(prms_attribute_df, subseg_seg == subseg) %>% 
      pull(all_from_segs)
    #area for each PRMS segment
    areas_segs <- filter(nhdv2_attr_refined, hru_segment %in% ind_segs) %>% 
      pull(CAT_BASIN_AREA_sum)
    #CAT variables for each segment
    CWD_segs <- filter(nhdv2_attr_refined, hru_segment %in% ind_segs) %>% 
      pull(CAT_CWD_area_wtd)
    TAV_segs <- filter(nhdv2_attr_refined, hru_segment %in% ind_segs) %>% 
      pull(CAT_TAV7100_ANN_area_wtd)
    TMN_segs <- filter(nhdv2_attr_refined, hru_segment %in% ind_segs) %>% 
      pull(CAT_TMIN7100_area_wtd)
    STR_DENS_segs <- filter(nhdv2_attr_refined, hru_segment %in% ind_segs) %>% 
      pull(CAT_STRM_DENS_area_wtd)
    
    #Compute TOT values
    #need special handling for PRMS segments with _2
    ind_replace <- which(nhdv2_attr_refined$hru_segment == subseg)
    nhdv2_attr_refined$TOT_CWD_frmCAT[ind_replace] <- weighted.mean(CWD_segs,areas_segs)
    nhdv2_attr_refined$TOT_TAV7100_ANN_frmCAT[ind_replace] <- weighted.mean(TAV_segs,areas_segs)
    nhdv2_attr_refined$TOT_TMIN7100_frmCAT[ind_replace] <- weighted.mean(TMN_segs,areas_segs)
    nhdv2_attr_refined$TOT_STRM_DENS_frmCAT[ind_replace] <- weighted.mean(STR_DENS_segs,areas_segs)
  }
  #Remove original TOT columns
  nhdv2_attr_refined <- select(nhdv2_attr_refined, -c(TOT_CWD, TOT_TAV7100_ANN, TOT_TMIN7100, TOT_STRM_DENS))
  
  return(nhdv2_attr_refined)
}

refine_from_neighbors <- function(nhdv2_attr, attr_i, prms_reach_attr
                                  ){
  #' 
  #' @description Function to fill in a reach's attribute value with a value 
  #' from its neighboring reaches (from_segs and to_seg)
  #'
  #' @param nhdv2_attr the full (or partially refined) attribute tbl
  #' @param attr_i the attribute name as character string
  #' @param prms_reach_attr the PRMS reach attribute tbl with rows for each 
  #' from_segs as discovered from the recursive function 
  #' (output of p2_prms_attribute_df target)
  #'
  #' @value Returns nhdv2_attr with filled in values for the attr_i column.
  
  if(attr_i == 'CAT_EWT_area_wtd'){
    #Search for < -100
    ind_reach <- filter(nhdv2_attr, get(attr_i) < -100) %>%
      pull(PRMS_segid)
  }else{
    #search for NAs
    ind_reach <- filter(nhdv2_attr, is.na(get(attr_i))) %>%
      pull(PRMS_segid)
  }
  #find the from and to segments for this reach
  seg_match <- filter(prms_reach_attr, PRMS_segid_main == ind_reach) %>%
    select(from_segs, to_seg) %>%
    unique() %>%
    mutate(segs = list(c(from_segs[[1]], to_seg))) %>%
    select(-from_segs, -to_seg) %>%
    unlist()
  
  #add _1 and _2 to match PRMS seg ID
  seg_match <- case_when(seg_match == '3_1' ~ '3_1',
                         seg_match == '3_2' ~ '3_2',
                         seg_match == '8_1' ~ '8_1',
                         seg_match == '8_2' ~ '8_2',
                         seg_match == '51_1' ~ '51_1',
                         seg_match == '51_2' ~ '51_2',
                         TRUE ~ paste0(seg_match, '_1'))
  #get the average of the attributes for the matched reaches
  fill_val <- filter(nhdv2_attr, PRMS_segid %in% seg_match) %>%
    select(all_of(attr_i)) %>%
    colMeans() %>%
    as.numeric()
  #assign to attribute table
  nhdv2_attr_refined <- mutate(nhdv2_attr,
                               attr = case_when(PRMS_segid == ind_reach ~ fill_val,
                                                              TRUE ~ get(attr_i))
                               ) %>%
    pull(attr)
  return(nhdv2_attr_refined)
}
