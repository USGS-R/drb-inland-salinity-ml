library(targets)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("tidyverse", "lubridate","rmarkdown","knitr","leaflet","sf")) 

source("./2_process/src/2_process.R")

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

list(
  # Load harmonized WQP data product
  tar_target(
    p1_wqp_data,
    readRDS(file = "1_fetch/in/DRB.WQdata.rds")
  ),
  # Filter harmonized WQP data for salinity data
  tar_target(
    p2_filtered_wqp_data,
    filter_wqp_salinity_data(p1_wqp_data,major_ion_names,select_wqp_vars,omit_wqp_events)
  ),
  # Subset and save discrete specific conductance data
  tar_target(
    p2_wqp_spC_csv,
    subset_wqp_spC_data(p2_filtered_wqp_data,fileout="2_process/out/DRB_WQdata_spC_data.csv")
  ),
  # Render data summary report
  tarchetypes::tar_render(p3_wqp_spC_report, "3_visualize/src/report-wqp-salinity-data.Rmd",output_dir = "3_visualize/out")
)

