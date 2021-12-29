source("2_process/src/filter_wqp_data.R")
source("2_process/src/munge_inst_timeseries.R")
source("2_process/src/create_site_list.R")
source("2_process/src/match_sites_reaches.R")
source("2_process/src/pair_nhd_reaches.R")
source("2_process/src/combine_NLCD_PRMS.R")


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
  
  # Pair PRMS segments with NHDPlusV2 reaches
  tar_target(
    p2_prms_nhdv2_xwalk,
    {
      p1_reaches_sf %>%
        split(.,.$subsegid) %>%
        purrr::map(.,pair_nhd_reaches,nhd_lines=p1_nhdv2reaches_sf) %>%
        purrr::map(.,summarize_paired_comids) %>%
        bind_rows()
    }
  ),
  
  # Extract COMID and respective area for LC estimation below
  tar_target(p2_nhd_area_att, 
             p1_nhdv2reaches_sf %>%
               st_drop_geometry() %>%
               select(COMID,AREASQKM,TOTDASQKM)
  ),
  
  ## Melt PRMS_nhdv2_xwalk to get all cols of comids Ids filtered to drb 
  tar_target(p2_drb_comids, 
            {p2_prms_nhdv2_xwalk %>%
               # split df by PRMS_segid() -  
               split(.,.$PRMS_segid) %>%
               # loop through each row element of this list and str split such that each new col is a cell
               lapply(.,function(x){
                 comids <- data.frame(PRMS_segid = x$PRMS_segid,
                                      comid=unlist(strsplit(x$comid_all,split=";")))
               }) %>%
               bind_rows()
            }
  ),
  ## Filter LC data to the AOI : DRB and join with COMIDs area info and PRMS ids
  # returns a df with unique comids for aoi + area of comid and NLCD LC percentage attributes
  tar_target(p2_LC_w_catchment_area,
             AOI_LC_w_area(area_att = p2_nhd_area_att,
                       NLCD_LC_df = p1_NLCD_df$NLCD_LandCover_2011,
                       drb_comids_df = p2_drb_comids)
             ),
  
  ## Estimate LC proportion in PRMS catchment
  # returns df with percent LC in PRMS catchment in our AOI
  tar_target(p2_PRMS_lc_proportions,
             proportion_lc_by_prms(NLCD_LC_df_w_area = p2_LC_w_catchment_area,
                                   catchment_att = "CAT")
             )
)
