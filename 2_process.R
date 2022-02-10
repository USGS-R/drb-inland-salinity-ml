source("2_process/src/filter_wqp_data.R")
source("2_process/src/munge_inst_timeseries.R")
source("2_process/src/create_site_list.R")
source("2_process/src/match_sites_reaches.R")
source("2_process/src/pair_nhd_reaches.R")
source("2_process/src/raster_to_catchment_polygons.R")
source("2_process/src/combine_NLCD_PRMS.R")
source("2_process/src/pair_nhd_catchments.R")
source("2_process/src/create_GFv1_NHDv2_xwalk.R")
source("2_process/src/munge_natural_baseflow.R")

p2_targets_list <- list(
  
  # Filter harmonized WQP data for salinity data
  tar_target(
    p2_filtered_wqp_data,
    filter_wqp_salinity_data(p1_wqp_data,major_ion_names,wqp_vars_select,omit_wqp_events)
  ),
  
  # Subset discrete SC data from harmonized WQP
  tar_target(
    p2_wqp_SC_data,
    subset_wqp_SC_data(p2_filtered_wqp_data)
  ),
  
  # Aggregate instantaneous SC data to hourly averages
  tar_target(
    p2_inst_data_hourly,
    aggregate_data_to_hourly(p1_inst_data,output_tz = "UTC"),
    pattern = map(p1_inst_data)
  ),
  
  # Aggregate instantaneous SC data to daily min/mean/max
  tar_target(
    p2_inst_data_daily,
    aggregate_data_to_daily(p1_inst_data,p1_daily_data, min_daily_coverage=0.5, output_tz="America/New_York")
  ),
  
  # Combine 1) daily DO data and 2) instantaneous DO data that has been aggregated to daily 
  tar_target(
    p2_daily_combined,
    bind_rows(p1_daily_data, p2_inst_data_daily)
  ),
  
  # Create a list of unique site id's with SC data  
  tar_target(
    p2_site_list,
    create_site_list(p2_wqp_SC_data,p1_nwis_sites,p1_daily_data,p1_inst_data,
                     hucs=drb_huc8s,crs_out="NAD83")
  ),

  tar_target(
     p2_sites_w_segs,
     get_site_flowlines(p1_reaches_sf, p2_site_list, sites_crs = 4269, max_matches = 1, search_radius = 0.1)
  ),
  
  # Pair PRMS segments with intersecting NHDPlusV2 reaches and contributing NHDPlusV2 catchments
  tar_target(
    p2_prms_nhdv2_xwalk,
    create_GFv1_NHDv2_xwalk(prms_lines = p1_reaches_sf,nhd_lines = p1_nhdv2reaches_sf,
                            prms_hrus = p1_catchments_sf_valid,
                            min_area_overlap = 0.5,drb_segs_spatial = drb_segs_spatial)
  ),
  
  ## Melt PRMS_nhdv2_xwalk to get all cols of comids Ids and PRMS ids filtered to drb 
  tar_target(
    p2_drb_comids_all_tribs, 
    p2_prms_nhdv2_xwalk %>%
      select(PRMS_segid, comid_cat) %>% 
      tidyr::separate_rows(comid_cat,sep=";") %>% 
      rename(comid = comid_cat)
  ),
  
  ## Filter LC data to the AOI : DRB and join with COMIDs area info and PRMS ids
  # returns a df with unique comids for aoi + area of comid and NLCD LC percentage attributes
  tar_target(
    p2_LC_w_catchment_area,
    AOI_LC_w_area(area_att = p1_nhdv2reaches_sf %>% st_drop_geometry() %>% select(COMID,AREASQKM,TOTDASQKM,LENGTHKM),
                  ## NOTE - the NLCD_LC_df selected in the Land Cover 2011 - to be looped across all items of p1_NLCD_data
                  NLCD_LC_df = p1_NLCD_data$NLCD_LandCover_2011,
                  aoi_comids_df = p2_drb_comids_all_tribs)
  ),
  
  ## Estimate LC proportion in PRMS catchment
  # returns df with proportion LC in PRMS catchment in our AOI
  tar_target(
    p2_PRMS_lc_proportions,
    proportion_lc_by_prms(p2_LC_w_catchment_area)
  ),
  
  # Extract baccasted historical LC data raster values catchments polygond FORE-SCE  in the DRB - general function raster_to_catchment_polygons
  tar_target(
    p2_FORESCE_LC_per_catchment, 
    {lapply(p1_FORESCE_backcasted_LC, function(x) raster_to_catchment_polygons(polygon_sf = p1_catchments_sf_valid,
                                                                          raster = x, categorical_raster = TRUE,
                                                                          raster_summary_fun = NULL, new_cols_prefix = 'lcClass'))
      }
  ),
  
  # Extract Road Salt raster values to catchments polygons in the DRB - general function raster_to_catchment_polygons
  tar_target(
    p2_rdsalt_per_catchment,
    {lapply(p1_rdsalt, function(x) raster_to_catchment_polygons(polygon_sf = p1_catchments_sf_valid,
                                                                    raster = x, categorical_raster = FALSE,
                                                                    raster_summary_fun = sum, new_cols_prefix = 'rd_slt', na.rm = T))
    }
  ),
  
  # Aggregate to hru_segment for across each annual road salt df in list of p2_rdsalt_per_catchment
  tar_target(
    p2_rdsalt_per_catchment_grped, 
    lapply(p2_rdsalt_per_catchment, function(x) group_by(x, hru_segment) %>%
             summarise(across(starts_with('rd_sltX'), sum)))
  ),
  
  
  # Combine rd salt targets - from list of dfs to single df with added columns that summarize salt accumulation across all years. 
  tar_target(
    p2_rdsalt_per_catchment_allyrs,
    # Reduce can iterate through elements in a list 1 after another. 
    Reduce(function(...) merge(..., by = 'hru_segment'),
           p2_rdsalt_per_catchment_grped) %>% 
      # Calculate total salt accumulation across all years 
      mutate(rd_salt_all_years = rowSums(across(starts_with('rd_sltX')), na.rm = T)) %>% 
      # Calculate prop of catchment rd salt acc across entire basin
      mutate(rd_salt_all_years_prop_drb = round((rd_salt_all_years/sum(rd_salt_all_years)),8)) %>% 
      # Remove annual rd_saltXYr cols
      select(-starts_with('rd_sltX')) %>% arrange(desc(rd_salt_all_years_prop_drb))
  ),

  # Filter discrete samples from sites thought to be influenced by tidal extent
  tar_target(
    p2_wqp_SC_filtered,
    subset_wqp_nontidal(p2_wqp_SC_data,p2_sites_w_segs,mainstem_reaches_tidal)
  ),
  
  # Filter SC site list
  tar_target(
    p2_site_list_nontidal_csv,
    create_site_list_nontidal(p2_wqp_SC_filtered,p1_nwis_sites,p1_daily_data,p1_inst_data,
                              hucs=drb_huc8s,crs_out="NAD83",p2_sites_w_segs,"2_process/out/DRB_SC_sitelist_nontidal.csv"),
    format = "file"
  ),
  
  # Return natural baseflow estimates for each PRMS segment
  tar_target(
    p2_natural_baseflow,
    munge_natural_baseflow(baseflow_pred_files = p1_natural_baseflow_csv,
                           segs_w_comids = p2_prms_nhdv2_xwalk %>% 
                             select(PRMS_segid,comid_down) %>% 
                             rename('COMID' = 'comid_down'),
                           start_year = as.character(lubridate::year(earliest_date)),
                           end_year = as.character(lubridate::year(dummy_date)),
                           fill_all_years = TRUE)
    
  ),

  # Target for NADP initial Processing  
  tar_target(
    p2_NADP_Data,
    lapply(list.files(path = p1_NADP_data_unzipped, full.names = T), function(x) read.csv(x, sep = ',') %>%
                    # select only cols starting with cat and COMID co
                    select(COMID | starts_with('CAT')) %>%
                    # take only COMIDS in drb
                    filter(COMID %in% p2_drb_comids_all_tribs$comid) %>%
                    # add year col to ID each dataset - using regex to extract the year between NADP_ and _CONUS
                    mutate(Year = as.numeric(str_extract_all(x, "(?<=unzipped/NADP_).+(?=_CONUS.txt)"))) %>%
                    # remove year in col name to have all colnames equal across datasets
                    setNames(gsub('_\\d{4}', '', names(.)))) %>%
               # rbind the list of cleaned dfs
      do.call(rbind, .)
  )
)






