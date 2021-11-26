library(targets)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("tidyverse", "lubridate","rmarkdown","dataRetrieval","knitr","leaflet","sf")) 

source("1_fetch.R")
source("2_process.R")
source("3_visualize.R")

dir.create("2_process/out/", showWarnings = FALSE)
dir.create("3_visualize/out/", showWarnings = FALSE)

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

# Define all USGS specific conductance parameter codes
pcodes_select <- c("00095","90095","00094","90096") 

# Define USGS specific conductance parameter code to use for continuous data
parameter <- "00095"

# Define minor HUCs (hydrologic unit codes) that make up the DRB to use in calls to dataRetrieval functions
# Lower Delaware: 0204 subregion (for now, exclude New Jersey Coastal (https://water.usgs.gov/GIS/huc_name.html) and Delaware Bay ("02040204")
drb_huc8s <- c("02040101","02040102","02040103","02040104","02040105","02040106",
               "02040201","02040202","02040203","02040205","02040206","02040207")

# Define USGS site types for which to download specific conductance data (include "Stream","Stream:Canal", and "Spring" sites)
site_tp_select <- c("ST","ST-CA","SP") 

# Define USGS stat codes for continuous sites that only report daily statistics (https://help.waterdata.usgs.gov/stat_code) 
stat_cd_select <- c("00001","00003")

# Change dummy date to force re-build of NWIS SC sites and data download
dummy_date <- "2021-11-23"

# Return the complete list of targets
c(p1_targets_list, p2_targets_list, p3_targets_list)


