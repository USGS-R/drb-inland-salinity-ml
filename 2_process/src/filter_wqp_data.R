filter_wqp_salinity_data <- function(data,
                                     major_ion_names,
                                     wqp_vars_select,
                                     omit_wqp_events,
                                     earliest_date,
                                     latest_date,
                                     exclude_tidal = TRUE){
  #' 
  #' @description Function to filter the DRB multisource surface-water-quality dataset for
  #' desired parameters, units, and variables.
  #'
  #' @param data a data frame containing the downloaded DRB multisource surface-water-quality dataset
  #' @param major_ion_names a character vector containing the desired parameter values
  #' from the data column "param". See Shoda et al. 2019 (https://doi.org/10.5066/P9PX8LZO) 
  #' for more information.  
  #' @param wqp_vars_select a character vector indicating which data columns to
  #' retain from the DRB multisource dataset
  #' @param omit_wqp_events a character vector indicating which values to omit 
  #' from the "HydrologicEvent" column within the DRB multisource dataset
  #' @param earliest_date character string "YY-MM-DD" that indicates the earliest
  #' sampling date that should be retained. 
  #' @param latest_date character string "YY-MM-DD" that indicates the most recent
  #' sampling date that should be retained. 
  #' @param exclude_tidal logical, defaults to TRUE. If TRUE, rows containing "tidal" 
  #' within the "MonitoringLocationTypeName" column of the DRB multisource dataset will
  #' be omitted. 
  #'
  #' @value A data frame containing discrete water quality samples from the
  #' Delaware River Basin for the parameters of interest
  #' 
  
  data_subset <- data %>%
    filter((param_group == "Salinity" | param %in% major_ion_names),
           # Filter out any sediment samples, samples representing hydrologic events 
           # that are not of interest, and samples from LocationType = ditch
           ActivityMediaName != "Sediment",
           !(HydrologicEvent %in% omit_wqp_events),
           MonitoringLocationTypeName != "Stream: Ditch",
           # Keep QA/QC'ed data deemed reliable:
           final == "retain",
           ActivityStartDate >= earliest_date, 
           ActivityStartDate <= latest_date) %>%
    # Filter out any tidal samples if exclude_tidal = TRUE:
    {if(exclude_tidal){
      filter(.,!grepl("tidal", MonitoringLocationTypeName, ignore.case = TRUE))
      } else {.}
    } %>% 
    select(all_of(wqp_vars_select))
  
  if ("LongitudeMeasure" %in% wqp_vars_select){
    #Convert from chr to dbl
    data_subset$LongitudeMeasure <- as.numeric(data_subset$LongitudeMeasure)
  }
  if ("LatitudeMeasure" %in% wqp_vars_select){
    #Convert from chr to dbl
    data_subset$LatitudeMeasure <- as.numeric(data_subset$LatitudeMeasure)
  }
  
  return(data_subset)
}


subset_wqp_SC_data <- function(filtered_data, omit_duplicates = TRUE){
  #' 
  #' @description Function to subset the filtered WQP salinity dataset for specific conductance  
  #' 
  #' @details Duplicates exist in the discrete WQ dataset because records with different
  #' values of "param" (i.e., 'Specific conductance' vs 'Specific conductance, field') were
  #' not considered duplicates in the data harmonization code. 
  #'
  #' @param filtered_data a data frame containing the filtered discrete water quality
  #' dataset. `filtered_data` is the output from filter_wqp_salinity_data().
  #' @param omit_duplicates logical indicating whether to omit duplicated observations for a 
  #' unique site/date-time/lat-lon location/collecting organization; defaults to TRUE
  #'
  #' @value A data frame containing discrete specific conductance samples from the 
  #' Delaware River Basin 
  #' 
  #' @examples 
  #' subset_wqp_SC_data(filtered_data = filtered_wqp_data, omit_dups = TRUE)

  # Filter out specific conductance param values "min" and "max"
  SC_params <- c("Specific conductance, field",
                  "Specific conductance",
                  "Specific conductance, field, mean",
                  "Specific conductance, lab")

  SC_data_subset <- filtered_data %>%
    # Omit samples originally entered as "conductivity" since we can't be sure these reflect 
    # temperature-corrected conductance
    filter(param %in% SC_params, CharacteristicName != "Conductivity") %>%
    # Fill in date-time stamp so that if sampling time is missing, assume some value (12:00:00) 
    # that we can use to look for duplicated date-times
    # Note that "NA" values were handled in the updated data harmonization scripts, so 
    # commenting out this line for now.
    #mutate(ActivityStartDateTime = na_if(ActivityStartDateTime, "NA")) %>%
    mutate(ActivityStartDateTime_filled = if_else(is.na(ActivityStartDateTime),
                                           paste(ActivityStartDate, "12:00:00", sep=" "),
                                           as.character(ActivityStartDateTime)))
  
  if(omit_duplicates){
    # When duplicate observations exist for a unique combination of 
    # [site name & date-time & geographic location & collecting organization], 
    # select one observation based on the `param` attribute
    SC_data_subset_out <- SC_data_subset %>%
      group_by(MonitoringLocationIdentifier,
               ActivityStartDateTime_filled, 
               OrganizationIdentifier, 
               LongitudeMeasure, LatitudeMeasure) %>% 
      # Preferentially retain samples with param equals "Specific conductance, lab" 
      # first and "Specific conductance, field, mean" last.
      arrange(match(param, c("Specific conductance, lab",
                             "Specific conductance",
                             "Specific conductance, field",
                             "Specific conductance, field, mean"))) %>%
      mutate(n_duplicated = n(),
             dup_number = seq(n_duplicated),
             flag_duplicate_drop = n_duplicated > 1 & dup_number != 1) %>% 
      filter(flag_duplicate_drop == FALSE) %>%
      ungroup() %>%
      select(-c(n_duplicated, dup_number, flag_duplicate_drop)) %>%
      # arrange all rows to maintain consistency in row order across users/machines
      arrange(across(everything()))
  
  } else {
    
    # arrange all rows to maintain consistency in row order across users/machines
    SC_data_subset_out <- SC_data_subset %>%
      arrange(across(everything()))
    
  }
  
  return(SC_data_subset_out)
  
}



subset_wqp_SC_dups <- function(filtered_data){
  #' 
  #' @description Function to subset duplicate observations within the filtered 
  #' WQP salinity dataset for specific conductance  
  #'
  #' @param filtered_data a data frame containing the filtered DRB multisource 
  #' surface-water-quality dataset.filtered_data is the output from filter_wqp_salinity_data().
  #'
  #' @value A data frame containing discrete specific conductance samples from the Delaware River Basin 
  #' @examples 
  #' subset_wqp_SC_dups(filtered_data = filtered_wqp_data)
  
  SC_data_subset <- subset_wqp_SC_data(filtered_data, omit_duplicates = FALSE)
  
  # Isolate the duplicated observations
  SC_data_subset_dups <- SC_data_subset %>%
    group_by(MonitoringLocationIdentifier,
             ActivityStartDateTime_filled, 
             OrganizationIdentifier, 
             LongitudeMeasure, LatitudeMeasure) %>% 
    # Preferentially retain samples with param equals "Specific conductance, lab" 
    # first and "Specific conductance, field, mean" last
    arrange(match(param, c("Specific conductance, lab",
                           "Specific conductance",
                           "Specific conductance, field",
                           "Specific conductance, field, mean"))) %>%
    mutate(n_duplicated = n(),
           dup_number = seq(n_duplicated),
           flag_duplicate_drop = n_duplicated > 1 & dup_number != 1) %>% 
    filter(flag_duplicate_drop == TRUE) %>%
    ungroup() %>%
    select(-c(n_duplicated, dup_number, flag_duplicate_drop)) %>%
    # arrange all rows to maintain consistency in row order across users/machines
    arrange(across(everything()))
  
  return(SC_data_subset_dups)
  
}



subset_wqp_nontidal <- function(wqp_data,site_list_w_segs,mainstem_segs){
  #' 
  #' @description Function to filter discrete samples potentially influenced by tides from the WQP specific conductance dataset  
  #'
  #' @param wqp_data a data frame containing the Water Quality Portal (WQP) specific conductance dataset
  #' @param site_list_w_segs data frame containing site locations and matched segment id's
  #' @param mainstem_segs character string containing the mainstem segments that are assumed to be influenced by tides
  #'
  #' @examples 
  #' filter_wqp_nontidal(wqp_data=p2_wqp_SC_data,site_list_w_segs=p2_sites_w_segs,mainstem_segs=c("2771_1","2769_1"))
  #' 

  # Identify discrete sites associated with reaches thought to be tidal:
  sites_tidal <- site_list_w_segs %>%
    # Subset site list to find sites matched to tidal reaches
    filter(subsegid %in% mainstem_segs) %>%
    # Subset site list to include discrete sites only
    filter(grepl("Harmonized_WQP_data",data_src_combined)) %>%
    # "USGS-" was dropped from site_id in create_site_list step. Add back in to match with MonitoringLocationIdentifier in wqp_data
    mutate(site_id_full = case_when(startsWith(site_id,"0") ~ paste0("USGS-",site_id),
                                    TRUE ~ site_id))
  
  # Omit samples from tidal sites:
  wqp_data_filtered <- wqp_data %>%
    filter(!(MonitoringLocationIdentifier %in% sites_tidal$site_id_full))
  
  return(wqp_data_filtered)
  
}

