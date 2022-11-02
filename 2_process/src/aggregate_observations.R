combine_wq_data <- function(wqp_data, nwis_data){
  #' 
  #' @description Function to combine WQP datasets with NWIS daily data
  #' 
  #' @param wqp_data data frame containing discrete water quality data 
  #' from the wQP. Must contain cols MonitoringLocationIdentifier, 
  #' OrganizationIdentifier, ActivityStartDate, resultVal2, 
  #' ResultStatusIdentifier
  #' @param nwis_data data frame containing combined instantaneous (uv)
  #' and daily (dv) data; expected cols are agency_cd, site_no, Date, 
  #' Parameter, Value, Value_cd, Value_Max, Value_Max_cd, Value_Min,
  #' Value_Min_cd, dat_src
  #' 
  #' @value returns a data frame containing discrete and continuous 
  #' water quality observations
  #' 
  
  # Reformat WQP data to match NWIS datasets
  wqp_data_formatted <- wqp_data %>%
    # format site names for NWIS sites with prefix "USGS"
    mutate(site_id = if_else(grepl('USGS',MonitoringLocationIdentifier),
                             substr(MonitoringLocationIdentifier, 6, 100),
                             MonitoringLocationIdentifier),
           Parameter = "SpecCond") %>%
    # harmonize column names with cols used in nwis_data
    rename(agency_cd = OrganizationIdentifier,
           site_no = site_id,
           Date = ActivityStartDate) %>%
    # find the min/max and the mean (if multiple observations exist 
    # for a unique site-date)
    group_by(site_no, Date, agency_cd, Parameter) %>%
    summarize(Value = round(mean(resultVal2, na.rm = TRUE), 1),
              Value_cd = unique(ResultStatusIdentifier),
              Value_Max = max(resultVal2, na.rm = TRUE),
              Value_Max_cd = NA_character_,
              Value_Min = min(resultVal2, na.rm = TRUE),
              Value_Min_cd = NA_character_,
              data_type = unique(data_type),
              .groups = "keep") %>%
    ungroup() %>%
    # pare down columns of interest and specify the data source
    select(agency_cd, site_no, Date, Parameter, Value, Value_cd, 
           Value_Max, Value_Max_cd, Value_Min, Value_Min_cd, data_type) %>%
    mutate(dat_src = "WQP")
  
  # Combine wqp_data and nwis_data into a single data frame
  nwis_data$dat_src <- "NWIS"
  
  combined_data <- bind_rows(wqp_data_formatted, nwis_data)
  
  return(combined_data) 
  
}



aggregate_observations <- function(wqp_data, nwis_data, sites_w_segs, 
                                   aggr_method = "reach", prefer_nwis_sites = FALSE){
  #' 
  #' @description Function to aggregate water quality observations to 1 value/day
  #'
  #' @param wqp_data data frame containing discrete water quality data from the wQP. 
  #' Must contain cols MonitoringLocationIdentifier, OrganizationIdentifier, 
  #' ActivityStartDate, resultVal2, ResultStatusIdentifier
  #' @param nwis_data data frame containing combined instantaneous (uv) and daily (dv) data; 
  #' expected cols are agency_cd, site_no, Date, Parameter, Value, Value_cd, Value_Max, 
  #' Value_Max_cd, Value_Min, Value_Min_cd, dat_src
  #' @param sites_w_segs data frame containing site locations and matched segment id's;
  #' must contain column site_id
  #' @param aggr_method character vector indicating how observations should be aggregated;
  #' options include "reach" and "site" (defaults to "reach"). If "reach", then values 
  #' returned represent 1 value/segment/day, if "site", then values will be returned for
  #' each lat/lon location (maintaining multiple sites along a segment) amounting to 
  #' 1 value/site/day. Aggregating by "site" can be useful if a user wishes to keep 
  #' unaggregated, raw data matched to segments. With the "site" option, sites are aggregated 
  #' by geographic location because occasionally there are multiple site names that have 
  #' the same lat/lon location.
  #' @param prefer_nwis_sites logical, only applies when aggr_method = "reach"; indicates
  #' whether segment summaries should defer to NWIS observations when available. Defaults 
  #' to FALSE. If TRUE, any observations coming from a non-NWIS site will be omitted.
  #' 
  #' @value returns a data frame containing 1 mean/min/max value for each unique segment-date
  #' 
  
  # Format water quality data to include segment assignments
  vars <- c("site_id", "lon", "lat", "datum", "subsegid",
            "bird_dist_to_subseg_m", "segidnat")
  sites_w_segs <- sites_w_segs %>%
    select(any_of(vars))
  
  obs_data_w_segs <- combine_wq_data(wqp_data, nwis_data) %>%
    filter(site_no %in% sites_w_segs$site_id) %>%
    left_join(.,sites_w_segs,
              by=c("site_no" = "site_id")) 
  
  # Group observations based on aggregation method 
  if(aggr_method == "reach"){
    
    grouped_obs <- obs_data_w_segs %>%
      group_by(subsegid, Date) %>%
      # If prefer_nwis_sites = TRUE, check whether data for that segment comes from
      # multiple sources (i.e., WQP and NWIS). If multiple sources, filter out any
      # non-NWIS samples for that segment-date; otherwise, retain all samples
      {if(prefer_nwis_sites == "TRUE"){
        filter(.,if(n_distinct(dat_src) > 1) dat_src == "NWIS" else TRUE)
      } else {.}
      }
    
  } else {
    
    # For aggr_method = "site", group by lat/lon since in the WQP database, there
    # may exist different site names that share the same geographic coordinates
    grouped_obs <- obs_data_w_segs %>%
      group_by(lat,lon,Date)
    
  }
  
  # Aggregate observations based on groups defined above
  obs_data_aggr <- grouped_obs %>%
    summarize(mean_value = round(mean(Value, na.rm = TRUE), 1),
              min_value = min(c(mean_value,Value_Min), na.rm = TRUE),
              max_value = max(c(mean_value,Value_Max), na.rm = TRUE),
              sd_value = round(sd(Value, na.rm = TRUE), 2),
              n_value = length(!is.na(Value)),
              site_ids = paste0(unique(site_no), collapse = ","),
              #using max to select 'u', which is greater than 'd'.
              data_type = max(data_type),
              .groups = "keep") %>%
    # suppress any warnings related to taking min/max summaries
    # when no non-NA data exist for a reach-date; in these cases, 
    # Inf will be returned for min_value, max_value
    suppressWarnings() %>%
    mutate(cv_value = (sd_value/mean_value)) %>%
    ungroup()
  
  # Format aggregated data and replace any values of Inf with NA
  vars_to_keep <- c("subsegid","lat","lon","Date","mean_value","min_value",
                    "max_value","n_value","sd_value","cv_value","site_ids",
                    "data_type")
  obs_data_aggr_out <- obs_data_aggr %>%
    select(any_of(vars_to_keep)) %>%
    mutate(across(where(is.numeric), ~na_if(., "Inf"))) 
  
  return(obs_data_aggr_out)
  
}
