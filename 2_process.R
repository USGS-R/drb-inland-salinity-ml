source("2_process/src/munge_inst_timeseries.R")
source("2_process/src/filter_wqp_data.R")
source("2_process/src/create_site_list.R")
source("2_process/src/match_sites_reaches.R")
source("2_process/src/raster_to_catchment_polygons.R")
source("2_process/src/combine_NLCD_PRMS.R")
source("2_process/src/munge_natural_baseflow.R")
source('2_process/src/reclassify_land_cover.R')
source("2_process/src/process_nhdv2_attr.R")
source("2_process/src/recursive_fun.R")
source("2_process/src/aggregate_observations.R")
source('2_process/src/area_diff_fix.R')
source('2_process/src/clean_lulc_data_for_merge.R')
source('2_process/src/add_dynamic_attr.R')


p2_targets_list <- list(
  
  # Filter harmonized WQP data for salinity data
  tar_target(
    p2_wqp_salinity_data,
    filter_wqp_salinity_data(p1_wqp_data, major_ion_names, wqp_vars_select, omit_wqp_events, 
                             earliest_date, latest_date, exclude_tidal = TRUE)
  ),
  
  # Subset discrete SC data from harmonized WQP
  # this is a 'local' target so making p3_wqp_ind_csv works
  # see https://github.com/USGS-R/drb-inland-salinity-ml/issues/153
  tar_target(
    p2_wqp_SC_data,
    subset_wqp_SC_data(p2_wqp_salinity_data, omit_duplicates = TRUE)
  ),
  
  # Subset duplicated discrete SC observations from the harmonized WQP dataset;
  # this target provides a record of the duplicate records that were omitted in
  # p2_wqp_SC_data, but is not currently used elsewhere in the pipeline.
  tar_target(
    p2_wqp_SC_dups,
    subset_wqp_SC_dups(p2_wqp_salinity_data, p2_wqp_SC_data)
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
    aggregate_data_to_daily(p1_inst_data,p1_daily_data, min_daily_coverage=0.5, 
                            output_tz="America/New_York") %>%
      #add column to identify the data source as continuous
      mutate(data_type = 'u')
  ),
  
  # Combine 1) daily SC data and 2) instantaneous SC data that has been aggregated to daily 
  tar_target(
    p2_daily_combined,
    {
      #handling p1_daily_data data_type column here because of our
      #current tar_cue = never for p1_daily_data.
      #add column to identify the data source as continuous
      daily_only <- p1_daily_data
      daily_only$data_type <- 'u'
      
      bind_rows(daily_only, p2_inst_data_daily)
    }
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
                          area_col = 'AREASQKM',
                          length_col = 'LENGTHKM',
                          catchment_att = 'CAT',
                          remove_NODATA_cols = TRUE)
  ),
  
  tar_target(
    p2_PRMS_NLCD_lc_proportions_tot,
    proportion_lc_by_prms(p2_NLCD_LC_w_catchment_area %>%
                            # filtering to only the comid_downs of each PRMS - nrow = ~459
                            filter(comid %in% p2_drb_comids_down$comid),
                          area_col = 'TOTDASQKM',
                          length_col = 'LENGTHKM',
                          catchment_att = 'TOT',
                          remove_NODATA_cols = TRUE)

  ),  
  
  tar_target(
    p2_PRMS_NLCD_lc_proportions_acc,
    proportion_lc_by_prms(p2_NLCD_LC_w_catchment_area %>%
                            # filtering to only the comid_downs of each PRMS - nrow = ~459
                            filter(comid %in% p2_drb_comids_down$comid),
                          area_col = 'TOTDASQKM',
                          length_col = 'LENGTHKM',
                          catchment_att = 'ACC', 
                          remove_NODATA_cols = TRUE)
  ), 
  
  ## Standardize the land cover class names for NLCD to following standardized classes table - ''1_fetch/in/Reclassified_Land_Cover_IS.csv'
  # For NLCD, we use '1_fetch/in/Legend_NLCD_Land_Cover.csv' as vlookup file for the FORESCE targets
  # For Cat
  tar_target(
    p2_PRMS_NLCD_lc_proportions_reclass_cat,
    reclassify_LC_for_NLCD(NLCD_lc_proportions_df = p2_PRMS_NLCD_lc_proportions_cat,
                          years_suffix = NLCD_year_suffix,
                           reclassify_table = p1_NLCD_reclass_table)
  ),
  
  # For Tot
  tar_target(
    p2_PRMS_NLCD_lc_proportions_reclass_tot,
    reclassify_LC_for_NLCD(p2_PRMS_NLCD_lc_proportions_tot,
                           NLCD_year_suffix,
                           reclassify_table = p1_NLCD_reclass_table)
  ),

  # For Acc
  tar_target(
    p2_PRMS_NLCD_lc_proportions_reclass_acc,
    reclassify_LC_for_NLCD(p2_PRMS_NLCD_lc_proportions_acc,
                           NLCD_year_suffix,
                           reclassify_table = p1_NLCD_reclass_table)
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
    p2_FORESCE_LC_per_catchment_nhd_dissolve,
    {lapply(p1_FORESCE_backcasted_LC, function(x) raster_to_catchment_polygons(
      ## using dissolved catchment polygons here
      polygon_sf = p2_nhd_catchments_dissolved_sf,
      raster = x, categorical_raster = TRUE,
      raster_summary_fun = NULL,
      new_cols_prefix = 'lcClass',
      fill = 0) %>%
        #Add column of prms_subseg_seg to match p2_FORESCE_LC_per_catchment
        left_join(., 
                  p1_catchments_edited_sf %>% 
                    select(PRMS_segid, prms_subseg_seg) %>% 
                    sf::st_drop_geometry(), 'PRMS_segid')
        )
    }
  ), 
  
  ## Standardize the land cover class names for NLCD to following standardized classes table - ''1_fetch/in/Reclassified_Land_Cover_IS.csv'
  # For FORESCE '1_fetch/in/Legend_FORESCE_Land_Cover.csv' as vlookup file for the FORESCE targets
  # reclassify FORESCE followed by aggregate to hru_segment scale across all lc classes so that it's ready for x walk - output remains list of dfs for the 5 decade years covered by FORESCE

  ## reclassifying on original subsetted FORESCE LC proportions datasets
  tar_target(
    p2_FORESCE_LC_per_catchment_PRMS_reclass_cat,
    {purrr::map2(.x = p2_FORESCE_LC_per_catchment,
                 .y = FORESCE_years, 
                 .f = ~{reclassify_land_cover(land_cover_df = .x,
                                              reclassify_table = p1_FORESCE_reclass_table,
                                              reclassify_table_lc_col = 'FORESCE_value',
                                              reclassify_table_reclass_col = 'Reclassify_match',
                                              pivot_longer_contains = 'lcClass', 
                                              proportion_col_prefix = 'prop_lcClass', 
                                              hru_area_colname = hru_area_km2, 
                                              remove_NA_cols = TRUE) %>% 
                     
                       # See documentation in function
                     ## group by with both hru and prms because we need prms_subseg_seg to run the recursive function for upstream catchments
                     aggregate_proportions_hrus(group_by_segment_colname = vars(PRMS_segid,prms_subseg_seg),
                                                proportion_col_prefix = 'prop_lcClass',
                                                hru_area_colname = hru_area_km2,
                                                new_area_colname = total_PRMS_area) %>%
                     ## Adding Year column
                     mutate(Year = .y) %>% 
                     ## Reorder the land cover columns
                     select(order(colnames(.)))}
                 )
    }
  ),
  
  ## Reclassifying on fixed FORESCE LC proportions datasets  + cleaning/munging for rbind
  tar_target(
    p2_FORESCE_LC_per_catchment_nhd_dissolve_reclass_cat,
    {purrr::map2(.x = p2_FORESCE_LC_per_catchment_nhd_dissolve,
                 .y = FORESCE_years, 
                 .f = ~{reclassify_land_cover(land_cover_df = .x,
                                              reclassify_table = p1_FORESCE_reclass_table,
                                              reclassify_table_lc_col = 'FORESCE_value',
                                              reclassify_table_reclass_col = 'Reclassify_match',
                                              pivot_longer_contains = 'lcClass',
                                              proportion_col_prefix = 'prop_lcClass',
                                              hru_area_colname = total_PRMS_area,
                                              remove_NA_cols = TRUE) %>%
                     ## rearranging cols
                     select(PRMS_segid, everything(), -ID) %>% 
                     ## Adding Year column
                     mutate(Year = .y) %>%
                     ## Reorder the land cover columns
                     select(order(colnames(.)))}
    )
    }
  ),
  
  ## merge correct and fixed p2_FORESCE_LC_per_catchments_reclass_cat
  tar_target(
    p2_FORESCE_LC_per_catchment_reclass_cat,
    {map2(.x = p2_FORESCE_LC_per_catchment_PRMS_reclass_cat,
          .y = p2_FORESCE_LC_per_catchment_nhd_dissolve_reclass_cat,
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
      # un-nest to have new rows for each upstream catchment
      unnest(all_from_segs, keep_empty = TRUE)
  ),
  
  # Produce p2_FORESCE_LC_per_catchment_reclass_tot 
  tar_target(
    p2_FORESCE_LC_per_catchment_reclass_tot,
    {lapply(p2_FORESCE_LC_per_catchment_reclass_cat, function(x)
      p2_prms_attribute_df %>% 
        # join prop calculations
        right_join(x, by = c('all_from_segs' = 'prms_subseg_seg')) %>%
        # group by PRMS id
        group_by(PRMS_segid_main, Year) %>% 
        summarise(
          # calc. total area
          total_upstream_PRMS_area = sum(total_PRMS_area),
          # get proportions for the new total area
          across(starts_with('prop'), ~(sum((.x*total_PRMS_area)/total_upstream_PRMS_area))),
          .groups = 'drop_last') %>%
        drop_na() %>% 
        rename(., PRMS_segid = PRMS_segid_main) %>%
        ungroup() %>%
        # Add prms_subseg_seg
        right_join(x %>% select(PRMS_segid, prms_subseg_seg), 
                   by = c('PRMS_segid' = 'PRMS_segid')) %>%
        # Reorder the columns
        select(order(colnames(.))) %>%
        # Reorder the rows
        arrange(PRMS_segid)
      )}
  ),
  
  # Combine NLCD and FORESCE
  ## Cat
  tar_target(
    p2_all_lulc_data_cat,
    {rbind(
      clean_lulc_data_for_merge(p2_FORESCE_LC_per_catchment_reclass_cat,
                                columns_to_remove = 'prms_subseg_seg', prms_area_col = 'total_PRMS_area', prms_area_unit = 'km2', prop_prefix = 'CAT'),
      clean_lulc_data_for_merge(p2_PRMS_NLCD_lc_proportions_reclass_cat,
                                columns_to_remove = NULL, prms_area_col = 'AREASQKM_PRMS', prms_area_unit = 'km2', prop_prefix = 'CAT'))
      }
  ),
  
  ## Tot
  ## note - rbind only works when same number of cols
  tar_target(
    p2_all_lulc_data_tot,
    {rbind(
      clean_lulc_data_for_merge(p2_FORESCE_LC_per_catchment_reclass_tot,
                                columns_to_remove = 'prms_subseg_seg', prms_area_col = 'total_upstream_PRMS_area', prms_area_unit = 'km2', prop_prefix = 'TOT'),
      clean_lulc_data_for_merge(p2_PRMS_NLCD_lc_proportions_reclass_tot,
                                columns_to_remove = NULL, prms_area_col = 'AREASQKM_PRMS', prms_area_unit = 'km2', prop_prefix = 'TOT'))
    }
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
  
  # Combine rd salt targets - from list of dfs to single df with added columns 
  # that summarize salt accumulation across all years. 
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
      select(-starts_with('rd_sltX')) %>% 
      arrange(desc(rd_salt_all_years_prop_drb))
  ),

  # Filter discrete samples from sites thought to be influenced by tidal extent
  tar_target(
    p2_wqp_SC_filtered,
    subset_wqp_nontidal(p2_wqp_SC_data,p2_sites_w_segs,mainstem_reaches_tidal) %>%
      #add data source type as discrete estimate
      mutate(data_type = 'd') %>%
      #remove bad sample (explained here https://github.com/USGS-R/drb-inland-salinity-ml/issues/190)
      #only remove if equal to the exact value
      filter(!(ActivityStartDate == '1999-03-30' & 
                 MonitoringLocationIdentifier == 'USGS-01421618' & 
                 resultVal2 == 30200))
  ),
  
  # Filter SC site list
  tar_target(
    p2_site_list_nontidal_csv,
    create_site_list_nontidal(p2_wqp_SC_filtered,p1_nwis_sites,p1_daily_data,p1_inst_data,
                              hucs=drb_huc8s,crs_out="NAD83",p2_sites_w_segs,
                              "2_process/out/DRB_SC_sitelist_nontidal.csv"),
    format = "file"
  ),
  
  # Return SC observations aggregated to the PRMS segment
  tar_target(
    p2_SC_observations,
    aggregate_observations(p2_wqp_SC_filtered, p2_daily_combined, p2_sites_w_segs, 
                           aggr_method = "reach", prefer_nwis_sites = FALSE)
  ),
  
  # Return natural baseflow estimates for each PRMS segment
  tar_target(
    p2_natural_baseflow,
    munge_natural_baseflow(baseflow_pred_files = p1_natural_baseflow_csv,
                           segs_w_comids = p2_prms_nhdv2_xwalk %>% 
                             select(PRMS_segid,comid_down) %>% 
                             rename('COMID' = 'comid_down'),
                           start_year = as.character(lubridate::year(earliest_date)),
                           end_year = as.character(lubridate::year(latest_date)),
                           fill_all_years = TRUE) %>%
      #Fill in NA reach from neighbors
      refine_dynamic_from_neighbors(attr_i = 'mean_natl_baseflow_cfs',
                                    prms_reach_attr = p2_prms_attribute_df,
                                    drainage_area = p2_nhdv2_attr_refined %>% 
                                      select(PRMS_segid, TOT_BASIN_AREA))
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
    create_nhdv2_attr_table(p2_nhdv2_attr_upstream, p2_nhdv2_attr_catchment) %>%
      #add CAT road salt to static attributes
      left_join(p2_rdsalt_per_catchment_allyrs %>% 
                  select(PRMS_segid, rd_salt_all_years_prop_drb) %>%
                  rename(CAT_rdsalt_prop = rd_salt_all_years_prop_drb), by = 'PRMS_segid')
  ),
  
  #Refine the attributes that are used for modeling
  tar_target(
    p2_nhdv2_attr_refined,
    refine_features(nhdv2_attr = p2_nhdv2_attr,
                    prms_nhdv2_xwalk = p2_prms_nhdv2_xwalk,
                    nhdv2_reaches = p1_nhdv2reaches_sf,
                    prms_attribute_df = p2_prms_attribute_df,
                    #PHYSIO_AREA says which proportion of catchments are 
                    #covered by physiographic regions
                    #RUN7100 seems like it is by HUC02 instead of reach.
                    #RFACT is perfectly correlated with RF7100
                    drop_columns = c("PHYSIO_AREA", "RUN7100", "RFACT"))    
  ),
  
  #Lags to compute for dynamic attributes
  tar_target(
    p2_dyn_attr_lags,
    #' @param attribute short attribute name used to select columns containing
    #' that name, or 'Land', 'Met', 'Baseflow' as keywords to process those columns. 
    #' @param lags numeric vector stating how many lag_units to lag. 
    #' A column will be added for each element in lags for each attrs column. 
    #' A lag of 0 can be used to extract the value on the Date in the dyn_df tbl.
    #' @param lag_unit character vector containing the unit to use for each lag in lags. 
    #' Accepts any of the lubridate options (e.g., days, months, years). If all
    #' units are the same, can provide a one element vector with that unit.
    data.frame(attribute = c('HDENS', 'MAJOR', 'NDAMS', 'NORM_STORAGE', 'NID_STORAGE', 
                             'Land', 
                             'Met', 
                             'Baseflow'), 
               lags = I(list(c(10,20), c(10,20), c(10,20), c(10,20), c(10,20), 
                             c(5,10,15,20), 
                             c(1,3,7,15,30,90,180,1,5), 
                             c(1,3,6,1,5))), 
               lag_unit = I(list('years', 'years', 'years', 'years', 'years',
                                 'years', 
                                 c(rep('days', 7), 'years', 'years'), 
                                 c(rep('months', 3), 'years', 'years'))),
               lag_fxns = I(list(c('exact','mean'), c('exact','mean'), 
                                 c('exact','mean'), c('exact','mean'), 
                                 c('exact','mean'), 
                                 c('exact','mean'), 
                                 c('mean'),
                                 c('mean')))
    )
  ),
  
  #Dataframe of dynamic attributes
  tar_target(
    p2_dyn_attr,
    add_dyn_attrs_to_reaches(attrs = p2_nhdv2_attr_refined,
                             dyn_cols = c('HDENS', 'MAJOR', 'NDAMS', 'NORM', 
                                          'NID'),
                             start_date = earliest_date,
                             end_date = latest_date,
                             baseflow = p2_natural_baseflow,
                             CAT_Land = p2_all_lulc_data_cat,
                             Upstream_Land = p2_all_lulc_data_tot,
                             gridMET = p1_gridmet,
                             attr_prefix = c('CAT', 'TOT'), 
                             Upstream_Land_prefix = 'TOT',
                             lag_table = p2_dyn_attr_lags)
  ),
  
  #Remove static attributes that were made into dynamic attributes
  tar_target(
    p2_nhdv2_attr_refined_rm_dyn,
    select(p2_nhdv2_attr_refined, -contains('HDENS'), -contains('MAJOR'), 
           -contains('NDAMS'), -contains('NORM'), -contains('NID'), -hru_segment)
  ),
  
  #Join static attributes to dynamic dataframe
  tar_target(
    p2_all_attr,
    left_join(p2_dyn_attr, p2_nhdv2_attr_refined_rm_dyn, by = c('seg' = 'PRMS_segid')) %>%
      rename(PRMS_segid = seg)
  ),
  
  #Join attributes to SC observations and retain only the days with SC observations
  tar_target(
    p2_all_attr_SC_obs,
    left_join(p2_SC_observations, p2_all_attr, by = c('subsegid' = 'PRMS_segid', 'Date')) %>%
      rename(PRMS_segid = subsegid) %>%
      filter(Date >= earliest_date)
  )
)
