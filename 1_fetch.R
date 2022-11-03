source("1_fetch/src/get_nwis_sites.R")
source("1_fetch/src/get_daily_nwis_data.R")
source("1_fetch/src/get_inst_nwis_data.R")
source("1_fetch/src/find_sites_multipleTS.R")
source('1_fetch/src/get_nlcd_LC.R')
source("1_fetch/src/get_nhdplusv2.R")
source('1_fetch/src/download_tifs_annual.R')
source("1_fetch/src/get_gf.R")
source("1_fetch/src/fetch_sb_data.R")
source("1_fetch/src/fetch_nhdv2_attributes_from_sb.R")
source("1_fetch/src/download_file.R")
source("1_fetch/src/munge_reach_attr_tbl.R")
source("1_fetch/src/generate_credentials.R")

# tar_cue for downloading NWIS sites and data.
# change to 'thorough' to download, and 'never' to prevent downloading.
NWIS_cue = 'never'
NWIS_repository = 'aws'
# Change dummy date to document when NWIS SC sites and data were downloaded
dummy_date <- "2022-06-16"


# Note about 'local' targets:
# The 'local' targets in this file are such b/c the respective fxn
# does not return a single path and therefore targets cannot upload
# to S3 and throws an error.
# See https://github.com/USGS-R/drb-inland-salinity-ml/issues/152

p1_targets_list <- list(
  
  # AWS credentials target
  tar_target(
    p1_aws_credentials_1,
    generate_credentials(),
    deployment = 'main',
    cue = tar_cue('always'),
    priority = 0.99 # default priority (0.8) is set globally in _targets.R
  ),
  # dummy target with high priority. This is here so that no consequential 
  # targets are rebuilt if we forget to renew AWS credentials. 
  tar_target(
    p1_dummy,
    {},
    deployment = 'main',
    cue = tar_cue('always'),
    priority = 0.98 # default priority (0.8) is set globally in _targets.R
  ),
  
  # Load harmonized WQP data product for discrete samples. Note that this data
  # frame was created from an (internal) DRB data harmonization GitLab repository:
  # https://code.usgs.gov/wma/iws/wq-data-harmonization/-/commit/b705a2b91877d5e0bb9386250659b46149295b55
  # The output rds file was downloaded from the inland salinity sharepoint folder
  # and manually placed in 1_fetch/in.
  tar_target(
    p1_wqp_data_rds,
    "1_fetch/in/20220731_DRB_WQdata.rds",
    format = 'file',
    repository = 'local',
    deployment = 'main'
  ),
  tar_target(
    p1_wqp_data,
    readRDS(file = p1_wqp_data_rds),
    deployment = 'main'
  ),
  
  # Identify NWIS sites with SC data 
  tar_target(
    p1_nwis_sites,
    {
      dummy <- dummy_date
      get_nwis_sites(drb_huc8s,pcodes_select,site_tp_select,stat_cd_select)
    },
    deployment = 'main',
    cue = tar_cue(mode = NWIS_cue),
    repository = NWIS_repository
  ),
  
  # Subset daily NWIS sites
  tar_target(
    p1_nwis_sites_daily,
    p1_nwis_sites %>%
      # retain "dv" sites that contain data records after user-specified {earliest_date}
      filter(data_type_cd=="dv",!(site_no %in% omit_nwis_sites), 
      end_date > earliest_date, begin_date < latest_date) %>%
      # for sites with multiple time series (ts_id), retain the most recent time series for site_info
      group_by(site_no) %>% arrange(desc(end_date)) %>% slice(1),
    deployment = 'main',
    cue = tar_cue(mode = NWIS_cue),
    repository = NWIS_repository
  ),
  
  # Download NWIS daily data
  tar_target(
    p1_daily_data,
    get_daily_nwis_data(p1_nwis_sites_daily, parameter, stat_cd_select,
                        start_date = earliest_date, end_date = latest_date),
    pattern = map(p1_nwis_sites_daily),
    deployment = 'main',
    cue = tar_cue(mode = NWIS_cue),
    repository = NWIS_repository
  ),
  
  # Subset NWIS sites with instantaneous (sub-daily) data
  tar_target(
    p1_nwis_sites_inst,
    p1_nwis_sites %>%
      # retain "uv" sites that contain data records after user-specified {earliest_date}
      filter(data_type_cd=="uv",!(site_no %in% omit_nwis_sites), 
      end_date > earliest_date, begin_date < latest_date) %>%
      # for sites with multiple time series (ts_id), retain the most recent time series for site_info
      group_by(site_no) %>% arrange(desc(end_date)) %>% slice(1),
    deployment = 'main',
    cue = tar_cue(mode = NWIS_cue),
    repository = NWIS_repository
  ),
  
  # Create log file to track sites with multiple time series
  tar_target(
    p1_nwis_sites_inst_multipleTS_csv,
    find_sites_multipleTS(p1_nwis_sites, earliest_date, latest_date, omit_nwis_sites,
                          "3_visualize/log/summary_multiple_inst_ts.csv"),
    format = "file",
    deployment = 'main'
  ),

  # Download NWIS instantaneous data
  tar_target(
    p1_inst_data,
    get_inst_nwis_data(p1_nwis_sites_inst,parameter,
                       start_date = earliest_date, end_date = latest_date),
    pattern = map(p1_nwis_sites_inst),
    deployment = 'main',
    cue = tar_cue(mode = NWIS_cue),
    repository = NWIS_repository
  ),

  tar_target(
    p1_reaches_shp_zip,
    # [Jeff] I downloaded this manually from science base: 
    # https://www.sciencebase.gov/catalog/item/5f6a285d82ce38aaa244912e
    # Because it's a shapefile, it's not easily downloaded using sbtools
    # like other files are (see https://github.com/USGS-R/sbtools/issues/277).
    # Because of that and since it's small (<700 Kb) I figured it'd be fine to
    # just include in the repo and have it loosely referenced to the sb item ^
    "1_fetch/in/study_stream_reaches.zip",
    format = "file",
    repository = 'local',
    deployment = 'main'
  ),

  # Unzip zipped shapefile
  tar_target(
    p1_reaches_shp,
    {shapedir = "1_fetch/out/study_stream_reaches"
    # `shp_files` is a vector of all files ('dbf', 'prj', 'shp', 'shx')
    shp_files <- unzip(p1_reaches_shp_zip, exdir = shapedir)
    # return just the .shp file
    grep(".shp", shp_files, value = TRUE)},
    format = "file",
    deployment = 'main'
  ),
  
  # read shapefile into sf object
  tar_target(
    p1_reaches_sf,
    st_read(p1_reaches_shp,quiet=TRUE),
    deployment = 'main'
  ),

  # Download NHDPlusV2 flowlines for DRB
  tar_target(
    p1_nhdv2reaches_sf,
    get_nhdv2_flowlines(drb_huc8s),
    deployment = 'main'
  ),  
  
  # Download edited HRU polygons from https://github.com/USGS-R/drb-network-prep
  tar_target(
    p1_catchments_edited_gpkg,
    download_file(GFv1_HRUs_edited_url,
                  fileout = "1_fetch/out/GFv1_catchments_edited.gpkg", 
                  mode = "wb", quiet = TRUE),
    format = "file",
    deployment = 'main'
  ),
  
  # Read in edited HRU polygons
  tar_target(
    p1_catchments_edited_sf,
    sf::st_read(dsn = p1_catchments_edited_gpkg, layer = "GFv1_catchments_edited", quiet = TRUE) %>%
      mutate(PRMS_segid_split_col = PRMS_segid) %>%
      separate(col = PRMS_segid_split_col, sep = '_', into =c('prms_subseg_seg', "PRMS_segment_suffix")) %>%
      mutate(hru_area_km2 = hru_area_m2/10^6,
             prms_subseg_seg = case_when(PRMS_segid == '3_1' ~ '3_1',
                                          PRMS_segid == '3_2' ~ '3_2',
                                          PRMS_segid == '8_1' ~ '8_1',
                                          PRMS_segid == '8_2' ~ '8_2',
                                          PRMS_segid == '51_1' ~ '51_1',
                                          PRMS_segid == '51_2' ~ '51_2',
                                          TRUE ~ prms_subseg_seg)) %>%
      select(-hru_area_m2, -PRMS_segment_suffix),
    deployment = 'main'
  ),
  
  # Download DRB network attributes
  # Retrieved from: https://www.sciencebase.gov/catalog/item/5f6a289982ce38aaa2449135
  # see note at top of file about 'local' targets
  tar_target(
    p1_prms_reach_attr_csvs,
    download_sb_file(sb_id = "5f6a289982ce38aaa2449135",
                     file_name = c("reach_attributes_drb.csv",
                                   "distance_matrix_drb.csv",
                                   "sntemp_inputs_outputs_drb.zip"),
                     out_dir="1_fetch/out"),
    format="file",
    deployment = 'main',
    repository = 'local'
  ),
  
  # Read DRB reach attributes with all _1 segments for _2 reaches
  tar_target(
    p1_prms_reach_attr,
    munge_reach_attr_table(p1_prms_reach_attr_csvs)
  ),
  
  # Read DRB network adjacency matrix
  tar_target(
    p1_ntw_adj_matrix,
    read_csv(grep("distance_matrix",p1_prms_reach_attr_csvs,value=TRUE),
             show_col_types = FALSE)
  ),
  
  # Unzip DRB SNTemp Inputs-Outputs from temperature project
  tar_target(
    p1_sntemp_inputs_outputs_csv,
    unzip(zipfile = grep("sntemp_inputs_outputs",p1_prms_reach_attr_csvs,value=TRUE), 
          exdir = "1_fetch/out", overwrite = TRUE),
    format = "file"
  ),
  
  # Read DRB SNTemp Inputs-Outputs from temperature project
  tar_target(
    p1_sntemp_inputs_outputs,
    read_csv(p1_sntemp_inputs_outputs_csv,show_col_types = FALSE)
  ),

  # Read in all nlcd data from 2001-2019 
  # Note: NLCD data must already be downloaded locally and manually placed in NLCD_LC_path ('1_fetch/in/NLCD_final/')
  tar_target(
    p1_NLCD_LC_data,
    read_subset_LC_data(LC_data_folder_path = NLCD_LC_path,
                        Comids_in_AOI_df = p1_nhdv2reaches_sf %>% st_drop_geometry() %>% select(COMID),
                        Comid_col = 'COMID', NLCD_type = NULL)
  ),
    
  # Download and unzip other NLCD 2011 datasets 
  ## Note - this returns a string or vector of strings of data path to unzipped datasets
  # see note at top of file about 'local' targets
  tar_target(
    p1_NLCD2011_data_unzipped, 
    {zip_files <- download_NHD_data(sb_id = sb_ids_NLCD2011,
                      out_path = '1_fetch/out',
                      downloaded_data_folder_name = NLCD2011_folders,
                      output_data_parent_folder = 'NLCD_LC_2011_Data')
    unzipped_files <- unzip_NHD_data(downloaded_data_folder_path = zip_files,
                                     create_unzip_subfolder = TRUE)
    rm(zip_files)
    unzipped_files
    },
    format = 'file',
    deployment = 'main',
    repository = 'local'
  ),
  
  # Read in NLCD datasets and subset by comid in DRB
  ## Note that this returns a vector of dfs if more than one NLCD data is in
  ## p1_NLCD_data_unzipped
  tar_target(
    p1_NLCD2011_data,
    read_subset_LC_data(LC_data_folder_path = p1_NLCD2011_data_unzipped, 
                        Comids_in_AOI_df = p1_nhdv2reaches_sf %>% st_drop_geometry() %>% select(COMID), 
                        Comid_col = 'COMID')
  ),

  # Downlaod FORE-SCE backcasted LC tif files and subset to years we want
  ## Retrieved from: https://www.sciencebase.gov/catalog/item/605c987fd34ec5fa65eb6a74
  ## Note - only file #1 DRB_Historical_Reconstruction_1680-2010.zip will be extracted
  # see note at top of file about 'local' targets
  tar_target(
    p1_FORESCE_backcasted_LC, 
    download_tifs(sb_id = '605c987fd34ec5fa65eb6a74',
                  filename = 'DRB_Historical_Reconstruction_1680-2010.zip',
                  download_path = '1_fetch/out',
                  ## Subset downloaded tifs to only process the  years that are relevant model
                  year = FORESCE_years,
                  name_unzip_folder = NULL,
                  overwrite_file = TRUE,
                  name = FORESCE_years), 
    format = 'file',
    deployment = 'main',
    repository = 'local'
  ),
  
  #Targets for the land cover reclassification .csv files
  tar_target(
    p1_NLCD_reclass_table_csv, 
    '1_fetch/in/Legend_NLCD_Land_Cover.csv',
    format = 'file',
    repository = 'local',
    deployment = 'main'
  ),
  tar_target(
    p1_FORESCE_reclass_table_csv, 
    '1_fetch/in/Legend_FORESCE_Land_Cover.csv',
    format = 'file',
    repository = 'local',
    deployment = 'main'
  ),
  tar_target(
    p1_NLCD_reclass_table, 
    read_csv(p1_NLCD_reclass_table_csv, show_col_types = FALSE),
    deployment = 'main'
  ),
  tar_target(
    p1_FORESCE_reclass_table, 
    read_csv(p1_FORESCE_reclass_table_csv, show_col_types = FALSE),
    deployment = 'main'
  ),
  
  # Downlaod Road Salt accumulation data for the drb
  ## Retrieved from: https://www.sciencebase.gov/catalog/item/5b15a50ce4b092d9651e22b9
  ## Note - only zip file named 1992_2015.zip will be extracted
  # see note at top of file about 'local' targets
  tar_target(
    p1_rdsalt, 
    download_tifs(sb_id = '5b15a50ce4b092d9651e22b9',
                  filename = '1992_2015.zip',
                  download_path = '1_fetch/out',
                  overwrite_file = T,
                  ## no year subsetting here as all years with rdsalt data are relevant here
                  year = NULL,
                  name_unzip_folder = 'rd_salt'),
    format = 'file',
    deployment = 'main',
    repository = 'local'
  ),

  # Csv of variables from the Wieczorek dataset that are of interest 
  tar_target(
    p1_vars_of_interest_csv,
    '1_fetch/in/NHDVarsOfInterest.csv',
    format = 'file',
    repository = 'local',
    deployment = 'main'
  ),

  # Variables from the Wieczorek dataset that are of interest 
  # use tar_group to define row groups based on ScienceBase ID; row groups facilitate
  # branching over subsets of the VarsOfInterest table in downstream targets
  tar_target(
    p1_vars_of_interest,
    read_csv(p1_vars_of_interest_csv, show_col_types = FALSE) %>%
      # Parse sb_id from sb link 
      mutate(sb_id = str_extract(Science.Base.Link,"[^/]*$")) %>%
      # Omit LandCover rows since we are loading those separately
      filter(!Theme %in% c('Land Cover')) %>%
      group_by(sb_id) %>%
      tar_group(),
    iteration = "group",
    deployment = 'main'
  ),

  # Map over variables of interest to download NHDv2 attribute data from ScienceBase
  tar_target(
    p1_vars_of_interest_downloaded_csvs,
    fetch_nhdv2_attributes_from_sb(vars_item = p1_vars_of_interest, save_dir = "1_fetch/out", 
                                   comids = p1_nhdv2reaches_sf$COMID, 
                                   delete_local_copies = TRUE),
    pattern = map(p1_vars_of_interest),
    format = "file",
    deployment = 'main'
  ),
  
  # Download and unzip monthly natural baseflow for the DRB
  # from Miller et al. 2021: https://www.sciencebase.gov/catalog/item/6023e628d34e31ed20c874e4
  # see note at top of file about 'local' targets
  tar_target(
    p1_natural_baseflow_csv,
    {zip_file <- download_sb_file(sb_id = "6023e628d34e31ed20c874e4",
                     file_name = "baseflow_partial_model_pred_XX.zip",
                     out_dir="1_fetch/out")
    unzip(zipfile = zip_file,
          exdir = dirname(zip_file), overwrite = TRUE)
    unzipped_files <- file.path(dirname(zip_file), 
                                list.files(path = dirname(zip_file),
                                           pattern = "*baseflow.*.csv"))
    unlink(zip_file, recursive = FALSE)
    rm(zip_file)
    unzipped_files
    },
    format = "file",
    repository = 'local',
    deployment = 'main'
  ),

  # CSV file of gridmet drivers aggregated to PRMS segments
  tar_target(
    p1_gridmet_csv,
    "1_fetch/in/drb_climate_2022_04_06_segments.csv",
    format = "file",
    repository = "local"
  ),
    
  # Read gridmet csv into tibble
  tar_target(
    p1_gridmet,
    read_csv(p1_gridmet_csv, show_col_types = FALSE)
  ),
  
  # Reservoirs shapefile for plotting locations on maps
  # list of dams (points) and reservoirs (polygons).
  # dams includes the reservoir outlet points.
  tar_target(
    p1_reservoirs_rds,
    "1_fetch/in/filtered_dams_reservoirs.rds",
    format = "file",
    repository = "local"
  ),
  tar_target(
    p1_reservoirs_sf,
    readRDS(p1_reservoirs_rds)
  )

)
  
