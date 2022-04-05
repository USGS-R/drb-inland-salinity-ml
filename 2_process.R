source("2_process/src/munge_inst_timeseries.R")
source("2_process/src/filter_wqp_data.R")
source("2_process/src/create_site_list.R")
source("2_process/src/match_sites_reaches.R")
source("2_process/src/raster_to_catchment_polygons.R")
source("2_process/src/combine_NLCD_PRMS.R")
source("2_process/src/munge_natural_baseflow.R")
source('2_process/src/reclassify_land_cover.R')
source('2_process/src/FORESCE_agg_lc_props.R')
source("2_process/src/process_nhdv2_attr.R")
source("2_process/src/recursive_fun.R")
source('2_process/src/area_diff_fix.R')

p2_targets_list <- list(
  
  # Filter harmonized WQP data for salinity data
  tar_target(
    p2_filtered_wqp_data,
    filter_wqp_salinity_data(p1_wqp_data,major_ion_names,wqp_vars_select,omit_wqp_events)
  ),
  
  # Subset discrete SC data from harmonized WQP
  tar_target(
    p2_wqp_SC_data,
    subset_wqp_SC_data(p2_filtered_wqp_data, omit_dups = TRUE)
  ),
  
  # Subset duplicated discrete SC observations from the harmonized WQP dataset
  tar_target(
    p2_wqp_SC_dups,
    subset_wqp_SC_dups(p2_filtered_wqp_data)
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

  # Match PRMS stream segments to observation site ids and return subset of sites within 
  # the distance specified by search_radius (in meters)
  tar_target(
     p2_sites_w_segs,
     get_site_flowlines(p1_reaches_sf, p2_site_list, sites_crs = 4269, max_matches = 1, 
                        search_radius = bird_dist_cutoff_m, retain_sites = retain_nwis_sites)
  ),
  
  # Pair PRMS segments with intersecting NHDPlusV2 reaches and contributing NHDPlusV2 catchments
  tar_target(
    p2_prms_nhdv2_xwalk,
    read_csv(GFv1_NHDv2_xwalk_url, col_types = 'cccc')
  ),
  
  # Simple target pairing PRMS_segids with hru_segment:
  tar_target(p2_PRMS_hru_segment,
             p2_prms_nhdv2_xwalk %>% mutate(PRMS_segid_split_col = PRMS_segid) %>%
               separate(col = PRMS_segid_split_col, sep = '_', into =c('hru_segment', "PRMS_segment_suffix")) %>%
               select(PRMS_segid, hru_segment) %>% mutate(hru_segment = as.integer(hru_segment))
  ),
  
  # Melt PRMS-NHDv2 xwalk table to return all COMIDs that drain to each PRMS segment
  tar_target(
    p2_drb_comids_all_tribs, 
    p2_prms_nhdv2_xwalk %>%
      select(PRMS_segid, comid_cat) %>% 
      tidyr::separate_rows(comid_cat,sep=";") %>% 
      rename(comid = comid_cat)
  ),
  
  # Subset PRMS-NHDv2 xwalk table to return the COMID located at the downstream end of each PRMS segment
  tar_target(
    p2_drb_comids_down,
    p2_prms_nhdv2_xwalk %>% 
      select(PRMS_segid,comid_down) %>% 
      rename(comid = comid_down)
  ),
  
  #----
  # NLCD Land Cover Proportions 
  ## Filter LC data to the AOI : DRB and join with COMIDs area info and PRMS ids
  # returns a df with unique comids for aoi + area of comid and NLCD LC percentage attributes
  tar_target(
    p2_NLCD_LC_w_catchment_area,
    AOI_LC_w_area(area_att = p1_nhdv2reaches_sf %>% 
                                                st_drop_geometry() %>% 
                                                select(COMID, AREASQKM,TOTDASQKM, LENGTHKM),
                  NLCD_LC_df = p1_NLCD_LC_data,
                  aoi_comids_df = p2_drb_comids_all_tribs)
  ),
  
  ## Estimate LC proportion in PRMS catchment - CAT, TOT, and ACC
  # returns df with proportion LC in PRMS catchment in our AOI
  tar_target(
    p2_PRMS_NLCD_lc_proportions_cat,
    proportion_lc_by_prms(p2_NLCD_LC_w_catchment_area,
                                   catchment_att = 'CAT') %>%
      select(-contains('NODATA'))
  ),
  
  tar_target(
    p2_PRMS_NLCD_lc_proportions_tot,
    proportion_lc_by_prms(p2_NLCD_LC_w_catchment_area %>%
                            # filtering to only the comid_downs of each PRMS - nrow = ~459
                            filter(comid %in% p2_drb_comids_down$comid),
                          catchment_att = 'TOT') %>%
      select(-contains('NODATA'))
  ),  
  
  tar_target(
    p2_PRMS_NLCD_lc_proportions_acc,
    proportion_lc_by_prms(p2_NLCD_LC_w_catchment_area %>%
                            # filtering to only the comid_downs of each PRMS - nrow = ~459
                            filter(comid %in% p2_drb_comids_down$comid),
                          catchment_att = 'ACC') %>%
      select(-contains('NODATA'))
  ), 
  
  ## Standardize the land cover class names for NLCD to following standardized classes table - ''1_fetch/in/Reclassified_Land_Cover_IS.csv'
  # For NLCD, we use '1_fetch/in/Legend_NLCD_Land_Cover.csv' as vlookup file for the FORESCE targets
  # For Cat
  tar_target(
    p2_PRMS_NLCD_lc_proportions_reclass_cat,
    reclassify_LC_for_NLCD(NLCD_lc_proportions_df = p2_PRMS_NLCD_lc_proportions_cat,
                          years_suffix = NLCD_year_suffix,
                           reclassify_table_csv_path = '1_fetch/in/Legend_NLCD_Land_Cover.csv')
  ),
  
  # For Tot
  tar_target(
    p2_PRMS_NLCD_lc_proportions_reclass_tot,
    reclassify_LC_for_NLCD(p2_PRMS_NLCD_lc_proportions_tot,
                           NLCD_year_suffix,
                           reclassify_table_csv_path = '1_fetch/in/Legend_NLCD_Land_Cover.csv')
  ),

  # For Acc
  tar_target(
    p2_PRMS_NLCD_lc_proportions_reclass_acc,
    reclassify_LC_for_NLCD(p2_PRMS_NLCD_lc_proportions_acc,
                           NLCD_year_suffix,
                           reclassify_table_csv_path = '1_fetch/in/Legend_NLCD_Land_Cover.csv')
  ),
  
  #----
  # PRMS gpkg Area Fix: Fix PRMS polygon area difference between catchments edited sf and nhd comid aggregation

  ## Find list of PRMS_segid requiring new catchment polygon - using 5 km as the threshold
  tar_target(
    p2_PRMS_segid_special_handling_list,
    catchment_area_check(PRMS_shapefile = p1_catchments_edited_sf,
                         nhd_catchment_areas = p2_NLCD_LC_w_catchment_area,
                         area_difference_threshold_km2 = 5)
  ),
  
  ## New PRMS catchment shapefile from comids - used for the PRMS_segid requiring fixed catchments for lc raster extraction : 
  tar_target(
    p2_nhd_catchments_dissolved_sf,
    dissolve_nhd_catchments_to_PRMS_segid(selected_PRMS_list = p2_PRMS_segid_special_handling_list,
                                          PRMS_comid_df = p2_drb_comids_all_tribs,
                                          prms_reaches_sf = p1_reaches_sf)
    ),
  
  ## Filtered PRMS catchments gpkg for the PRMS segid not require dnew nhd-based catchment   
  tar_target(
    p2_filtered_catchments_edited_sf,
    p1_catchments_edited_sf %>% filter(!PRMS_segid %in% p2_PRMS_segid_special_handling_list)
    ),

  # ----
  # FORESCE:Extract historical LC data raster values within catchments polygon + prop calc - general function raster_to_catchment_polygons
  ## This is a subset now. nrow =  702 (vs. 867 before)
  tar_target(
    p2_FORESCE_LC_per_catchment, 
    {lapply(p1_FORESCE_backcasted_LC, function(x) raster_to_catchment_polygons(
      polygon_sf = p2_filtered_catchments_edited_sf,
      raster = x, categorical_raster = TRUE,
      raster_summary_fun = NULL,
      new_cols_prefix = 'lcClass',
      fill = 0))
    }
  ),
  
  ## Proportion calculation for PRMS_segid that use fixed nhd-based catchment polygons
  tar_target(
    p2_FORESCE_LC_per_catchment_fixed,
    {lapply(p1_FORESCE_backcasted_LC, function(x) raster_to_catchment_polygons(
      ## using dissolved catchment polygons here
      polygon_sf = p2_nhd_catchments_dissolved_sf,
      raster = x, categorical_raster = TRUE,
      raster_summary_fun = NULL,
      new_cols_prefix = 'lcClass',
      fill = 0))
    }
  ), 
  
  ## Standardize the land cover class names for NLCD to following standardized classes table - ''1_fetch/in/Reclassified_Land_Cover_IS.csv'
  # For FORESCE '1_fetch/in/Legend_FORESCE_Land_Cover.csv' as vlookup file for the FORESCE targets
  # reclassify FORESCE followed by aggregate to hru_segment scale across all lc classes so that it's ready for x walk - output remains list of dfs for the 5 decade years covered by FORESCE

  ## reclassifying on original subsetted FORESCE LC proportions datasets
  tar_target(
    p2_FORESCE_LC_per_catchment_reclass_correct_cat,
    {purrr::map2(.x = p2_FORESCE_LC_per_catchment,
                 .y = FORESCE_years, 
                 .f = ~{reclassify_land_cover(land_cover_df = .x,reclassify_table_csv_path = '1_fetch/in/Legend_FORESCE_Land_Cover.csv',
                                              reclassify_table_lc_col = 'FORESCE_value',
                                              reclassify_table_reclass_col = 'Reclassify_match',
                                              sep = ',',
                                              pivot_longer_contains = 'lcClass') %>% 
                       # See documentation in function
                     ## group by with both hru and prms because we need hru_segment to run the recursive function for upstream catchments
                     aggregate_proportions_hrus(group_by_segment_colname = vars(PRMS_segid,hru_segment),
                                                proportion_col_prefix = 'prop_lcClass',
                                                hru_area_colname = hru_area_m2,
                                                new_area_colname = total_PRMS_area_km2) %>%
                     ## Adding Year column
                     mutate(Year = .y)}
                 )
    }
  ),
  
  ## Reclassifying on fixed FORESCE LC proportions datasets  + cleaning/munging for rbind 
  tar_target(
    p2_FORESCE_LC_per_catchment_fixed_reclass_cat,
    {purrr::map2(.x = p2_FORESCE_LC_per_catchment_fixed,
                 .y = FORESCE_years, 
                 .f = ~{reclassify_land_cover(land_cover_df = .x,reclassify_table_csv_path = '1_fetch/in/Legend_FORESCE_Land_Cover.csv',
                                              reclassify_table_lc_col = 'FORESCE_value',
                                              reclassify_table_reclass_col = 'Reclassify_match',
                                              sep = ',',
                                              pivot_longer_contains = 'lcClass') %>%
                     ## renaming area col so that it can be rbind-ed with p2_FORESCE_LC_per_catchment_correct_reclass_cat
                     rename(total_PRMS_area_km2 = areasqkm) %>% 
                     ## adding hru_segment col
                     left_join(p2_PRMS_hru_segment, by = 'PRMS_segid') %>% 
                     ## rearranging cols
                     select(PRMS_segid, hru_segment, everything(), -ID) %>% 
                     ## Adding Year column, and coverting hru_segment col to character so that it can be merged appropriately
                     mutate(Year = .y, hru_segment = as.character(hru_segment))}
    )
    }
  ),
  
  ## merge correct and fixed p2_FORESCE_LC_per_catchments_reclass_cat
  tar_target(
    p2_FORESCE_LC_per_catchment_reclass_cat,
    {map2(.x = p2_FORESCE_LC_per_catchment_reclass_correct_cat,
          .y = p2_FORESCE_LC_per_catchment_fixed_reclass_cat,
          .f = ~rbind(.x, .y))}
  ),
  
  ## Produce subset of p1_prms_reach_attr for p2_FORESCE_LC_per_catchment_reclass_tot target via recursively calculating proportions of LC class across all upstream segments for a given segment
  tar_target(
    p2_prms_attribute_df, 
    p1_prms_reach_attr %>% select(subseg_id,subseg_seg,from_segs,to_seg) %>% 
      # renaming so that we can distinguish from p2_FORESCE_LC_per_catchment_reclass_cat$PRMS_segid col
      rename(., PRMS_segid_main = subseg_id) %>% 
      # Update `from_segs` col by splitting the individual segs in a list (can then loop through the list) 
      mutate(from_segs = stringr::str_split(string = from_segs, pattern = ';', simplify = F)) %>% 
      rowwise() %>%
      # Collect all upstream segs per individual seg_id using recursive_fun() (row wise application)
      mutate(all_from_segs = list(recursive_fun(x = subseg_seg,  df = ., col1 = 'subseg_seg', col2 = 'from_segs'))) %>%
      # unest to have new rows for each upstream catchment
      unnest(all_from_segs, keep_empty = TRUE)
    ),
  
  # Produce p2_FORESCE_LC_per_catchment_reclass_tot 
  tar_target(
    p2_FORESCE_LC_per_catchment_reclass_tot,
    {lapply(p2_FORESCE_LC_per_catchment_reclass_cat, function(x)
      p2_prms_attribute_df %>% 
        # join prop calculations - selected inner join because at the moment, p2_prms_attribute_df has more PRMS_segids than p2_FORESCE_LC_per_catchment_reclass_cat due to outdated catchmetns file
        inner_join(x, by = c('all_from_segs' = 'hru_segment')) %>%
        # group by PRMS id
        group_by(PRMS_segid_main, Year) %>% 
        summarise(
          # calc. total area
          total_upstream_PRMS_area = sum(total_PRMS_area),
          # get proportions for the new total area
          across(starts_with('prop'), ~(sum((.x*total_PRMS_area)/total_upstream_PRMS_area))),
          .groups = 'drop_last') %>%
        drop_na() %>% 
        rename(., PRMS_segid = PRMS_segid_main)
      )}
  ),
  
  # Extract Road Salt raster values to catchments polygons in the DRB - general function raster_to_catchment_polygons + Aggregate to hru_segment scale across each annual road salt df in list of p2_rdsalt_per_catchment - can then xwalk
  tar_target(
    p2_rdsalt_per_catchment,
    {lapply(p1_rdsalt, function(x) raster_to_catchment_polygons(polygon_sf = p1_catchments_edited_sf,
                                                                raster = x,
                                                                categorical_raster = FALSE,
                                                                raster_summary_fun = sum,
                                                                new_cols_prefix = 'rd_slt', na.rm = T) %>%
              group_by(PRMS_segid) %>%
              summarise(across(starts_with('rd_sltX'), sum)))
    }
  ),
  
  # Combine rd salt targets - from list of dfs to single df with added columns that summarize salt accumulation across all years. 
  tar_target(
    p2_rdsalt_per_catchment_allyrs,
    # Reduce can iterate through elements in a list 1 after another 
    Reduce(function(...) merge(..., by = 'PRMS_segid'),
           p2_rdsalt_per_catchment) %>% 
      # Calculate total salt accumulation across all years 
      mutate(rd_salt_all_years = rowSums(across(starts_with('rd_sltX')), na.rm = TRUE)) %>% 
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
    lapply(list.files(path = p1_NADP_data_unzipped, full.names = T),
           function(x) read.csv(x, sep = ',') %>%
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
  ),
  
  # Process NHDv2 attributes referenced to cumulative upstream area;
  # returns object target of class "list". List elements for CAT_PPT
  # and ACC_PPT (if TOT is selected below) will only contain the 
  # PRMS_segid column and so will functionally be omitted when  
  # creating the `p2_nhdv2_attr` target below
  tar_target(
    p2_nhdv2_attr_upstream,
    process_cumulative_nhdv2_attr(p1_vars_of_interest_downloaded_csvs,
                                  segs_w_comids = p2_drb_comids_down,
                                  cols = c("TOT")),
    pattern = map(p1_vars_of_interest_downloaded_csvs),
    iteration = "list"
  ),
  
  # Process NHDv2 attributes scaled to the catchment that directly drains to each PRMS segment;
  # returns object target of class "list" that is nested and contains the aggregated data as well 
  # as a separate NA diagnostics data table for each NHDv2 attribute
  tar_target(
    p2_nhdv2_attr_catchment,
    process_catchment_nhdv2_attr(p1_vars_of_interest_downloaded_csvs,
                                 vars_table = p1_vars_of_interest,
                                 segs_w_comids = p2_drb_comids_all_tribs,
                                 nhd_lines = p1_nhdv2reaches_sf),
    pattern = map(p1_vars_of_interest_downloaded_csvs),
    iteration = "list"
  ),
  
  # Create combined NHDv2 attribute data frame that includes both the cumulative upstream and catchment-scale values
  tar_target(
    p2_nhdv2_attr,
    create_nhdv2_attr_table(p2_nhdv2_attr_upstream,p2_nhdv2_attr_catchment)
  )
)







