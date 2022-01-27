source("2_process/src/filter_wqp_data.R")
source("2_process/src/munge_inst_timeseries.R")
source("2_process/src/create_site_list.R")
source("2_process/src/match_sites_reaches.R")
source("2_process/src/pair_nhd_reaches.R")
source("2_process/src/pair_nhd_catchments.R")
source("2_process/src/create_GFv1_NHDv2_xwalk.R")

p2_targets_list <- list(
  
  # Filter harmonized WQP data for salinity data
  tar_target(
    p2_filtered_wqp_data,
    filter_wqp_salinity_data(p1_wqp_data,major_ion_names,wqp_vars_select,omit_wqp_events)
  ),
  
  # Subset discrete SC data from harmonized WQP
  tar_target(
    p2_wqp_SC_data,
    subset_wqp_SC_data(p2_filtered_wqp_data)),
  
  # Aggregate instantaneous SC data to hourly averages
  tar_target(
    p2_inst_data_hourly,
    aggregate_data_to_hourly(p1_inst_data,output_tz = "UTC"),
    pattern = map(p1_inst_data)),
  
  # Aggregate instantaneous SC data to daily min/mean/max
  tar_target(
    p2_inst_data_daily,
    aggregate_data_to_daily(p1_inst_data,p1_daily_data, min_daily_coverage=0.5, output_tz="America/New_York")),
  
  # Combine 1) daily DO data and 2) instantaneous DO data that has been aggregated to daily 
  tar_target(
    p2_daily_combined,
    bind_rows(p1_daily_data, p2_inst_data_daily)),
  
  # Create a list of unique site id's with SC data  
  tar_target(
    p2_site_list,
    create_site_list(p2_wqp_SC_data,p1_nwis_sites,p1_daily_data,p1_inst_data,
                     hucs=drb_huc8s,crs_out="NAD83")),

  tar_target(
     p2_sites_w_segs,
     get_site_flowlines(p1_reaches_sf, p2_site_list, sites_crs = 4269, max_matches = 1, search_radius = 0.1)),
  
  # Pair PRMS segments with intersecting NHDPlusV2 reaches and contributing NHDPlusV2 catchments
  tar_target(
    p2_prms_nhdv2_xwalk,
    create_GFv1_NHDv2_xwalk(prms_lines = p1_reaches_sf,nhd_lines = p1_nhdv2reaches_sf,prms_hrus = p1_catchments_sf,
                            min_area_overlap = 0.5,drb_segs_spatial = drb_segs_spatial)
  ),
  
  # Filter discrete samples from sites thought to be influenced by tidal extent
  tar_target(
    p2_wqp_SC_filtered,
    subset_wqp_nontidal(p2_wqp_SC_data,p2_sites_w_segs,mainstem_reaches_tidal)),
  
  # Filter SC site list
  tar_target(
    p2_site_list_nontidal_csv,
    create_site_list_nontidal(p2_wqp_SC_filtered,p1_nwis_sites,p1_daily_data,p1_inst_data,
                              hucs=drb_huc8s,crs_out="NAD83",p2_sites_w_segs,"2_process/out/DRB_SC_sitelist_nontidal.csv"),
    format = "file"),
  
  # target for NADP 
  tar_target(p2_NADP_Data, 
             lapply(list.files(path = p1_NADP_data_unzipped, full.names = T), function(x) read.csv(x, sep = ',') %>%
                    # select only cols starting with cat and COMID co
                    select(COMID | starts_with('CAT')) %>%
                    # take only COMIDS in drb  
                    filter(COMID %in% p1_nhd2reaches_sf$COMID) %>% 
                    # add year col to ID each dataset - using regex to extract the year between NADP_ and _CONUS
                    mutate(Year = str_extract_all(x, "(?<=unzipped/NADP_).+(?=_CONUS.txt)")) %>% 
                    # remove year in col name to have all colnames equal across datasets
                    setNames(gsub('_\\d{4}', '', names(.)))) %>% 
               # rbind the list of cleaned dfs 
      do.call(rbind, .)
  )
)






