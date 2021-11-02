source("2_process/src/filter_wqp_data.R")
source("2_process/src/munge_nwis_SpC_data.R")

p2_targets_list <- list(
  # Filter harmonized WQP data for salinity data
  tar_target(
    p2_filtered_wqp_data,
    filter_wqp_salinity_data(p1_wqp_data,major_ion_names,select_wqp_vars,omit_wqp_events)
  ),
  # Subset and save discrete specific conductance data
  tar_target(
    p2_wqp_spC_csv,
    subset_wqp_spC_data(p2_filtered_wqp_data,fileout="2_process/out/DRB_WQdata_spC_data.csv")),
  tar_target(
    p2_nwis_spC_csv,
    combine_daily_mean_SpC_data(p1_nwis_daily_SpC_data_ls,fileout="2_process/out/DRB_daily_spC_data.csv")
  )
)
