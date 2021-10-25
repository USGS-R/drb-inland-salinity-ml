library(targets)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("tidyverse", "lubridate","rmarkdown","knitr","leaflet","sf")) 

source("./2_process/src/2_process.R")

list(
  # Load harmonized WQP data product
  tar_target(
    p1_wqp_data,
    readRDS(file = "1_fetch/in/DRB.WQdata.rds")),
  # Summarize data related to salinity in harmonized WQP data
  tar_target(
    p2_wqp_salinity_records_table,
    summarize_wqp_salinity_data(p1_wqp_data)
  ),
  # Subset discrete major ions data
  tar_target(
    p2_wqp_Cl,
    subset_wqp_major_ions_data(p1_wqp_data,param="Chloride")
  ),
  # Subset and save discrete specific conductance data
  tar_target(
    p2_wqp_spC_csv,
    subset_wqp_spC_data(p1_wqp_data,fileout="2_process/out/DRB_WQdata_spC_data.csv")
  ),
  # Render data summary report
  tarchetypes::tar_render(p3_wqp_spC_report, "3_visualize/src/report-wqp-salinity-data.Rmd",output_dir = "3_visualize/out")
)

