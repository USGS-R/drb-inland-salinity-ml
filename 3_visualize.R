source("3_visualize/src/plot_daily_data.R")
source("3_visualize/src/plot_inst_data.R")
source("3_visualize/src/map_SC_sites.R")
source("3_visualize/src/summarize_site_list.R")
source("3_visualize/src/summarize_timeseries.R")
source("3_visualize/src/plot_nhdv2_attr.R")
source("3_visualize/src/summarize_nhdv2_attr.R")

p3_targets_list <- list(
  
  # Plot daily data
  tar_target(
    p3_daily_timeseries_png,
    plot_daily_data(sprintf("3_visualize/out/daily_timeseries_png/daily_data_%s.png",unique(p1_daily_data$site_no)),p1_daily_data),
    format = "file",
    pattern = map(p1_daily_data)),
  
  # Plot instantaneous data (hourly averages)
  tar_target(
    p3_hourly_timeseries_png,
    plot_inst_data(sprintf("3_visualize/out/hourly_timeseries_png/hourly_data_%s.png",unique(p2_inst_data_hourly$site_no)),p2_inst_data_hourly),
    format = "file",
    pattern = map(p2_inst_data_hourly)),
  
  # Create and save log file containing data availability summary
  tar_target(
    p3_sitelist_summary_csv,
    summarize_site_list(p2_site_list_nontidal_csv,p1_daily_data,p1_inst_data,fileout = "3_visualize/log/sitelist_summary.csv"),
    format = "file"),
  
  # Create and save indicator file for NWIS daily data
  tar_target(
    p3_daily_timeseries_ind_csv,
    command = save_target_ind_files("3_visualize/log/daily_timeseries_ind.csv",names(p3_daily_timeseries_png)),
    format = "file"),
  
  # Create and save indicator file for NWIS instantaneous data
  tar_target(
    p3_inst_timeseries_ind_csv,
    command = save_target_ind_files("3_visualize/log/inst_timeseries_ind.csv",names(p3_hourly_timeseries_png)),
    format = "file"),
  
  # Create and save indicator file for WQP data
  tar_target(
    p3_wqp_ind_csv,
    command = save_target_ind_files("3_visualize/log/wqp_data_ind.csv","p2_wqp_SC_csv"),
    format = "file"),
  
  # Create and save summary log file for NWIS daily data
  tar_target(
    p3_daily_timeseries_summary_csv,
    command = target_summary_stats(p1_daily_data,"Value","3_visualize/log/daily_timeseries_summary.csv"),
    format = "file"
  ),
  
  # Create and save summary log file for NWIS instantaneous data
  tar_target(
    p3_inst_timeseries_summary_csv,
    command = target_summary_stats(p1_inst_data,"Value_Inst","3_visualize/log/inst_timeseries_summary.csv"),
    format = "file"
  ),
  
  # Render data summary report (note that tar_render returns a target with format="file") 
  tarchetypes::tar_render(p3_SC_report, "3_visualize/src/report-wqp-salinity-data.Rmd",output_dir = "3_visualize/out"),
  
  # Plot distribution of NHDv2 attribute variables across the PRMS network
  tar_target(
    p3_nhdv2_attr_png,
    plot_nhdv2_attr(attr_data = p2_nhdv2_attr,
                    network_geometry = p1_reaches_sf,
                    file_path = "3_visualize/out/nhdv2_attr_png"),
    format = "file"
  ),
  
  # Create and save a summary table that describes variation in the NHDv2 attribute variables across the PRMS network
  tar_target(
    p3_nhdv2_attr_summary_csv,
    summarize_nhdv2_attr(p2_nhdv2_attr,"3_visualize/out/nhdv2_attr_summary.csv"),
    format = "file"
  ),
  
  # Create and save a summary table that indicates the NA's among contributing 
  # NHDv2 catchments for each PRMS segment and attribute variable
  tar_target(
    p3_nhdv2_attr_missing_data_csv,
    summarize_catchment_nhdv2_attr_missing(p2_nhdv2_attr_catchment,"3_visualize/out/nhdv2_attr_missing_data.csv"),
    format = "file"
  )
  
)

