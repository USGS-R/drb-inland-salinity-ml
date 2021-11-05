library(targets)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("tidyverse", "lubridate","rmarkdown","dataRetrieval","knitr","leaflet","sf")) 

source("1_fetch.R")
source("2_process.R")
source("3_visualize.R")

dir.create("2_process/out/", showWarnings = FALSE)
dir.create("3_visualize/out/", showWarnings = FALSE)

# Define columns of interest for harmonized WQP data
select_wqp_vars <- c("MonitoringLocationIdentifier","MonitoringLocationName","LongitudeMeasure","LatitudeMeasure",
                     "MonitoringLocationTypeName","OrganizationIdentifier","ActivityStartDate","ActivityStartTime.Time",
                     "ActivityEndDate","CharacteristicName","param_group","param","USGSPCode","ActivityMediaName",
                     "ResultSampleFractionText","HydrologicCondition","HydrologicEvent","resultVal2","resultUnits2",
                     "ResultDetectionConditionText","ResultTemperatureBasisText","PrecisionValue","ResultStatusIdentifier",
                     "final")

# Define water quality major ions of interest
major_ion_names = c("Chloride","Sodium")

# Define hydrologic event types in harmonized WQP data to exclude
omit_wqp_events <- c("Spill","Volcanic action")

# Define USGS specific conductance parameter codes
SpC_pcodes <- c("00095","90095","00094","90096") 

# Define minor HUCs (hydrologic unit codes) that make up the DRB (HUC04 = 0204)to use in calls to dataRetrieval functions
drb_huc8s <- c("02040101","02040102","02040103","02040104","02040105","02040106",
               "02040201","02040202","02040203","02040204","02040205","02040206","02040207",
               "02040301","02040302")

# Define USGS site types for which to download specific conductance data (include "Stream","Stream:Canal", and "Spring" sites)
site_tp_select <- c("ST","ST-CA","SP") 


# Return the complete list of targets
c(p1_targets_list, p2_targets_list, p3_targets_list)


