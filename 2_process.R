source("2_process/src/filter_wqp_data.R")
source("2_process/src/munge_inst_timeseries.R")
source("2_process/src/create_site_list.R")
source("2_process/src/match_sites_reaches.R")

p2_targets_list <- list(
  
  # Filter harmonized WQP data for salinity data
  tar_target(
    p2_filtered_wqp_data,
    filter_wqp_salinity_data(p1_wqp_data,major_ion_names,wqp_vars_select,omit_wqp_events)
  ),
  
  # Subset discrete SC data from harmonized WQP
  tar_target(
    p2_wqp_SC_csv,
    subset_wqp_SC_data(p2_filtered_wqp_data,fileout="2_process/out/DRB_WQdata_SC_data.csv"),
    format="file"),
  
  # Aggregate instantaneous SC data to hourly averages
  tar_target(
    p2_inst_data_hourly,
    aggregate_data_to_hourly(p1_inst_data,output_tz = "UTC"),
    pattern = map(p1_inst_data)),
  
  # Create a list of unique site id's with SC data  
  tar_target(
    p2_site_list_csv,
    {
      wqp_data_subset <- read_csv(p2_wqp_SC_csv,col_types = cols(ResultDetectionConditionText = col_character()))
      create_site_list(wqp_data_subset,p1_nwis_sites,p1_daily_data,p1_inst_data,
                       hucs=drb_huc8s,crs_out="NAD83",fileout = "2_process/out/DRB_SC_sitelist.csv")
    },
    format = "file"),

  tar_target(
     p2_sites_w_segs,
     {
       sites_tbl <- read_csv(p2_site_list_csv)
       get_site_flowlines(p1_reaches_sf, sites_tbl, sites_crs = 4269, max_matches = 1, search_radius = 0.1)
     }),
  
  # Aggregate instantaneous DO data to daily min/mean/max
  tar_target(
    p2_inst_data_daily,
    aggregate_data_to_daily(p1_inst_data,p1_daily_data,"America/New_York")),
  
  # Combine 1) daily DO data and 2) instantaneous DO data that has been aggregated to daily 
  tar_target(
    p2_daily_combined,
    bind_rows(p1_daily_data, p2_inst_data_daily))
)
