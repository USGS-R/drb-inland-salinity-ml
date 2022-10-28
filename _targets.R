library(targets)

Sys.setenv(
    AWS_PROFILE = "dev",
    AWS_REGION = 'us-east-1',
    AWS_SHARED_CREDENTIALS_FILE = "./credentials"
)

options(tidyverse.quiet = TRUE,
        #Use multiprocess on Windows, multicore in container (Linux).
        clustermq.scheduler = "multicore")
library(clustermq)
library(tidyverse)

tar_option_set(packages = c("tidyverse", "lubridate",
                            "rmarkdown","dataRetrieval",
                            "knitr","leaflet","sf",
                            "purrr", "sbtools", "terra",
                            "patchwork", "glue", "nhdplusTools",
                            "Boruta", "ranger", "vip", "tidymodels",
                            "doParallel", "fastshap", "pdp"),
               resources = tar_resources(
                 aws = tar_resources_aws(bucket = "drb-inland-salinity")),
               repository = "aws",
               format = "rds",
               priority = 0.8
               )

source("1_fetch.R")
source("2_process.R")
source("3_visualize.R")
source("4_predict.R")
source("4_predict_plots.R")

dir.create("1_fetch/out/", showWarnings = FALSE)
dir.create("2_process/out/", showWarnings = FALSE)
dir.create("3_visualize/out/", showWarnings = FALSE)
dir.create("3_visualize/log/", showWarnings = FALSE)
dir.create("3_visualize/out/daily_timeseries_png/",showWarnings = FALSE)
dir.create("3_visualize/out/hourly_timeseries_png/",showWarnings = FALSE)
dir.create("3_visualize/out/nhdv2_attr_png/",showWarnings = FALSE)
dir.create("3_visualize/out/nhdv2_attr_png/refined",showWarnings = FALSE)
#Predict phase:
# create directories for each of the following combinations of data splits,
# feature sets and results:
predict_dir <- "4_predict/out"
train_test_other <- c("vip","hypopt")
train_test_splits <- c("temporal","random")
train_test_features <- c("RF_static", "RF_min_static", "RF_static_dynamic", 
                         "RF_min_static_dynamic","RF_dynamic")
train_test_res <- c("pred_obs","spatial_res","monthly_res","annual_res","temporal_res")
rf_xai_plot_types <- c("shap","dependence") 
rf_xai_dep_options <- c("pdp","ice")
#directory paths
p4_dirs <- bind_rows(
  expand.grid(predict_dir, train_test_splits, train_test_res, train_test_features),
  expand.grid(predict_dir, train_test_splits, train_test_other),
  expand.grid(predict_dir, train_test_splits, rf_xai_plot_types[rf_xai_plot_types == "shap"], 
              train_test_features),
  expand.grid(predict_dir, train_test_splits, rf_xai_plot_types[rf_xai_plot_types == "dependence"], 
              train_test_features, rf_xai_dep_options)) %>%
  mutate(file_path = case_when(is.na(Var4) ~ file.path(Var1, Var2, Var3),
                               is.na(Var5) ~ file.path(Var1, Var2, Var3, Var4),
                               TRUE ~ file.path(Var1, Var2, Var3, Var4, Var5))) %>%
  pull(file_path)
# create the directories
for(i in seq_along(p4_dirs)){
  dir.create(p4_dirs[i], recursive = TRUE, showWarnings = FALSE)
}


# Define columns of interest for harmonized WQP data
wqp_vars_select <- c("MonitoringLocationIdentifier","MonitoringLocationName","LongitudeMeasure","LatitudeMeasure",
                     "MonitoringLocationTypeName","OrganizationIdentifier","ActivityStartDate","ActivityStartDateTime",
                     "ActivityStartTime.Time","ActivityStartTime.TimeZoneCode","CharacteristicName","param_group","param",
                     "USGSPCode","ActivityMediaName","ResultSampleFractionText","HydrologicCondition","HydrologicEvent",
                     "resultVal2","resultUnits2","ResultDetectionConditionText","ResultTemperatureBasisText",
                     "PrecisionValue","ResultStatusIdentifier","final")

# Define water quality major ions of interest
major_ion_names = c("Chloride","Sodium")

# Define hydrologic event types in harmonized WQP data to exclude
omit_wqp_events <- c("Spill","Volcanic action")

# Define USGS specific conductance parameter codes of interest
pcodes_select <- c("00095","90095","00094","90096") 

# Define USGS specific conductance parameter code to use for continuous data
parameter <- "00095"

# Define minor HUCs (hydrologic unit codes) that make up the DRB to use in calls to dataRetrieval functions
# Lower Delaware: 0204 subregion (for now, exclude New Jersey Coastal (https://water.usgs.gov/GIS/huc_name.html)
drb_huc8s <- c("02040101","02040102","02040103","02040104","02040105","02040106",
               "02040201","02040202","02040203","02040204","02040205","02040206","02040207")

# Define USGS site types for which to download specific conductance data (include "Stream","Stream:Canal", and "Spring" sites)
site_tp_select <- c("ST","ST-CA","SP") 

# Omit undesired sites
# sites 01412350, 01484272, and 01482695 coded as site type "ST" but appear to be tidally-influenced
omit_nwis_sites <- c("01412350","01484272","01482695") 

# Define search radius to match sites to PRMS segments (in meters)
bird_dist_cutoff_m <- 500

# Retain NWIS sites even if nearest PRMS segment is outside of radius set by bird_dist_cutoff_m
retain_nwis_sites <- c("01474703","01477050")

# Define PRMS mainstem segments thought to be tidally-influenced (used for filtering sites used in daily/sub-daily models)
mainstem_reaches_tidal <- c("2771_1","2769_1","2768_1","2767_1","2764_1","2762_1","2763_1","2759_1","2757_1","2752_1",
                            "2753_1","2755_1","2772_1","388_1","389_1","385_1","386_1","383_1","382_1","377_1","378_1",
                            "376_1","351_1","344_1","346_1","333_1")

# Define the url for the NHGFv1 to NHDv2 crosswalk 
# see https://github.com/USGS-R/drb-network-prep/commit/3637931f5a17469a4234eaed3d20ed44ba45958d
GFv1_NHDv2_xwalk_url <- "https://raw.githubusercontent.com/USGS-R/drb-network-prep/3637931f5a17469a4234eaed3d20ed44ba45958d/2_process/out/GFv1_NHDv2_xwalk_omit_zero_area.csv"

# Define the url for the edited HRU polygons
# see https://github.com/USGS-R/drb-network-prep/commit/3637931f5a17469a4234eaed3d20ed44ba45958d
GFv1_HRUs_edited_url <- "https://github.com/USGS-R/drb-network-prep/blob/3637931f5a17469a4234eaed3d20ed44ba45958d/2_process/out/GFv1_catchments_edited.gpkg?raw=true"

# Define USGS stat codes for continuous sites that only report daily statistics (https://help.waterdata.usgs.gov/stat_code) 
stat_cd_select <- c("00001","00003")

# Define earliest startDate and latest endDate for NWIS data retrievals
earliest_date <- "1979-10-01"
latest_date <- "2021-12-31"

# Define dataset of interest for the national geospatial fabric (used to fetch PRMS catchment polygons):
gf_data_select <- 'GeospatialFabricFeatures_02.zip'

## NLCD 
# path to all NLCD LC data 2001-2019 
# Note: this data should be manually downloaded from the project Data folder in sharepoint (`NLCD_final/NLCD_final/`)
NLCD_LC_path <- file.path('1_fetch/in/NLCDs_2001_2019')
dir.create(NLCD_LC_path, showWarnings = F) 
# last two digit suffix of years of NLCD data
NLCD_year_suffix <- c('01','04','06','08','11','13','16','19')

# Define land cover datasets to extract 
sb_ids_NLCD2011 <- c(
  # ImperviousnessPct_2011: 'https://www.sciencebase.gov/catalog/item/57057a9be4b0d4e2b7571fbb',
  '57057a9be4b0d4e2b7571fbb',
  # Imperviousness100mBufferRipZone:'https://www.sciencebase.gov/catalog/item/570577fee4b0d4e2b7571d7b',
  '570577fee4b0d4e2b7571d7b',
  # TreeCanopy_100mBuffered_RipZone: 'https://www.sciencebase.gov/catalog/item/570572e2e4b0d4e2b75718bc'
  '570572e2e4b0d4e2b75718bc',
  # Estimated percent of catchment in 50 meter riparian zone that contains the land-use and land-cover: 'https://www.sciencebase.gov/catalog/item/57855ddee4b0e02680bf37bf'
  '57855ddee4b0e02680bf37bf'
  )

# Define Land Cover dataset download folders:

NLCD2011_folders <- c(
   'ImperviousnessPct_2011',
   'Imperviousness100m_RipZone',
   'TreeCanopy_100mBuffered_RipZone',
   'NLCD_LandCover_50m_RipZone'
)

## NADP_sb_id var
## https://www.sciencebase.gov/catalog/item/57e2ac2fe4b0908250045981
NADP_sb_id <- '57e2ac2fe4b0908250045981'

## FORESCE list of FORESCE years
FORESCE_years <- c('1940', '1950', '1960', '1970', '1980', '1990', '2000')


#Boruta Random Forest Parameters
#maximum number of runs for Boruta feature screening algorithm
Boruta_runs <- 300
#number of trees
Boruta_trees <- 500
#number of cores
Boruta_cores <- 70
RF_cores <- 60
#Cross validation folds
cv_folds <- 10

#SHAP
SHAP_nsim <- 10
SHAP_RAM <- 9
RAM_set <- 180
SHAP_cores <- 35

# Return the complete list of targets
c(p1_targets_list, p2_targets_list, p3_targets_list, p4_targets_list, 
  p4_plot_targets_list)