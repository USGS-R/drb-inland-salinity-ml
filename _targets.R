library(targets)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("tidyverse", "lubridate",
                            "rmarkdown","dataRetrieval",
                            "knitr","leaflet","sf",
                            'purrr', 'sbtools', 'terra')) 

source("1_fetch.R")
source("2_process.R")
source("3_visualize.R")

dir.create("1_fetch/out/", showWarnings = FALSE)
dir.create("2_process/out/", showWarnings = FALSE)
dir.create("3_visualize/out/", showWarnings = FALSE)
dir.create("3_visualize/log/", showWarnings = FALSE)
dir.create("3_visualize/out/daily_timeseries_png/",showWarnings = FALSE)
dir.create("3_visualize/out/hourly_timeseries_png/",showWarnings = FALSE)

# Define columns of interest for harmonized WQP data
wqp_vars_select <- c("MonitoringLocationIdentifier","MonitoringLocationName","LongitudeMeasure","LatitudeMeasure",
                     "MonitoringLocationTypeName","OrganizationIdentifier","ActivityStartDate","ActivityStartTime.Time",
                     "ActivityEndDate","CharacteristicName","param_group","param","USGSPCode","ActivityMediaName",
                     "ResultSampleFractionText","HydrologicCondition","HydrologicEvent","resultVal2","resultUnits2",
                     "ResultDetectionConditionText","ResultTemperatureBasisText","PrecisionValue","ResultStatusIdentifier",
                     "final")

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

# Define PRMS mainstem segments thought to be tidally-influenced (used for filtering sites used in daily/sub-daily models)
mainstem_reaches_tidal <- c("2771_1","2769_1","2768_1","2767_1","2764_1","2762_1","2763_1","2759_1","2757_1","2752_1",
                            "2753_1","2755_1","2772_1","388_1","389_1","385_1","386_1","383_1","382_1","377_1","378_1",
                            "376_1","351_1","344_1","346_1","333_1")

# Define PRMS segments that require special handling for the Geospatial Fabric (GFv1) to NHDplusV2 crosswalk target
drb_segs_spatial <- c("31_1","135_1","233_1","236_1","244_1","249_1","332_1","354_1","355_1","358_1","396_1","593_1","1256_1","2137_1",
                      "2138_1","2757_1","2758_1","2759_1","2765_1","2766_1","2767_1","2772_1","2797_1")

# Define USGS stat codes for continuous sites that only report daily statistics (https://help.waterdata.usgs.gov/stat_code) 
stat_cd_select <- c("00001","00003")

# Define earliest startDate for NWIS data retrievals
earliest_date <- "1979-10-01"

# Change dummy date to force re-build of NWIS SC sites and data download
dummy_date <- "2021-12-31"

# Define dataset of interest for the national geospatial fabric (used to fetch PRMS catchment polygons):
gf_data_select <- 'GeospatialFabricFeatures_02.zip'

# Define land cover datasets to extract 
sb_ids_NLCD <- c(
  # ImperviousnessPct_2011: 'https://www.sciencebase.gov/catalog/item/57057a9be4b0d4e2b7571fbb',
  '57057a9be4b0d4e2b7571fbb',
  # Imperviousness100mBufferRipZone:'https://www.sciencebase.gov/catalog/item/570577fee4b0d4e2b7571d7b',
  '570577fee4b0d4e2b7571d7b',
  # TreeCanopy_100mBuffered_RipZone: 'https://www.sciencebase.gov/catalog/item/570572e2e4b0d4e2b75718bc'
  '570572e2e4b0d4e2b75718bc',
  # NLCD_LandCover_2011: 'https://www.sciencebase.gov/catalog/item/5761bad4e4b04f417c2d30c5',
  '5761bad4e4b04f417c2d30c5',
  # Estimated percent of catchment in 50 meter riparian zone that contains the land-use and land-cover: 'https://www.sciencebase.gov/catalog/item/57855ddee4b0e02680bf37bf'
  '57855ddee4b0e02680bf37bf'
  )

# Define Land Cover dataset download folders:

NLCD_folders <- c(
   'ImperviousnessPct_2011',
   'Imperviousness100m_RipZone',
   'TreeCanopy_100mBuffered_RipZone',
   'NLCD_LandCover_2011',
   'NLCD_LandCover_50m_RipZone'
)

# vars for backcasted LC data
### Retrieved from: https://www.sciencebase.gov/catalog/item/605c987fd34ec5fa65eb6a74
### Note - only file #1 DRB_Historical_Reconstruction_1680-2010.zip will be extracted
sb_id_backcasting_LC <- '605c987fd34ec5fa65eb6a74'
DRB_Historical_Reconstruction_NLCD_file <- 'DRB_Historical_Reconstruction_1680-2010.zip' 

## vars for road salt
### Retrieved from: https://www.sciencebase.gov/catalog/item/5b15a50ce4b092d9651e22b9
### Note - only zip file named 1992_2015.zip will be extracted
rd_salt <- '5b15a50ce4b092d9651e22b9'
rd_salt_zip_file <- '1992_2015.zip'

# Return the complete list of targets
c(p1_targets_list, p2_targets_list, p3_targets_list)


