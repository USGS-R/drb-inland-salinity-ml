source("3_visualize/src/plot_daily_data.R")
source("3_visualize/src/plot_inst_data.R")
source("3_visualize/src/map_SC_sites.R")
source("3_visualize/src/summarize_site_list.R")
source("3_visualize/src/summarize_timeseries.R")
source("3_visualize/src/plot_nhdv2_attr.R")
source("3_visualize/src/summarize_nhdv2_attr.R")
source("3_visualize/src/plot_ecdf.R")

p3_targets_list <- list(
  
  # Plot daily data
  # this is a 'local' target so making p3_daily_timeseries_ind_csv works
  # see https://github.com/USGS-R/drb-inland-salinity-ml/issues/153
  tar_target(
    p3_daily_timeseries_png,
    plot_daily_data(sprintf("3_visualize/out/daily_timeseries_png/daily_data_%s.png",
                            unique(p1_daily_data$site_no)),
                    p1_daily_data),
    format = "file",
    pattern = map(p1_daily_data),
    repository = 'local'
  ),
  
  # Plot instantaneous data (hourly averages)
  # this is a 'local' target so making p3_inst_timeseries_ind_csv works
  # see https://github.com/USGS-R/drb-inland-salinity-ml/issues/153
  tar_target(
    p3_hourly_timeseries_png,
    plot_inst_data(sprintf("3_visualize/out/hourly_timeseries_png/hourly_data_%s.png",
                           unique(p2_inst_data_hourly$site_no)),
                   p2_inst_data_hourly),
    format = "file",
    pattern = map(p2_inst_data_hourly),
    repository = 'local'
  ),
  
  # Create and save log file containing data availability summary
  tar_target(
    p3_sitelist_summary_csv,
    summarize_site_list(p2_site_list_nontidal_csv,p1_daily_data,p1_inst_data,
                        fileout = "3_visualize/log/sitelist_summary.csv"),
    format = "file",
    repository = 'local',
    deployment = 'main'
  ),
  
  # Create and save indicator file for NWIS daily data
  tar_target(
    p3_daily_timeseries_ind_csv,
    command = save_target_ind_files("3_visualize/log/daily_timeseries_ind.csv",
                                    names(p3_daily_timeseries_png)),
    format = "file",
    repository = 'local',
    deployment = 'main'
  ),
  
  # Create and save indicator file for NWIS instantaneous data
  tar_target(
    p3_inst_timeseries_ind_csv,
    command = save_target_ind_files("3_visualize/log/inst_timeseries_ind.csv",
                                    names(p3_hourly_timeseries_png)),
    format = "file",
    repository = 'local',
    deployment = 'main'
  ),
  
  # Create and save indicator file for WQP data
  tar_target(
    p3_wqp_ind_csv,
    command = {
      #forcing dependency to the target because the character string 
      #of the target name does not enforce it
      force_dep <- p2_wqp_SC_data
      save_target_ind_files("3_visualize/log/wqp_data_ind.csv", "p2_wqp_SC_data")
      },
    format = "file",
    repository = 'local',
    deployment = "main"
  ),
  
  # Create and save summary log file for NWIS daily data
  tar_target(
    p3_daily_timeseries_summary_csv,
    target_summary_stats(p1_daily_data, "Value",
                         "3_visualize/log/daily_timeseries_summary.csv"),
    format = "file",
    repository = 'local',
    deployment = 'main'
  ),
  
  # Create and save summary log file for NWIS instantaneous data
  tar_target(
    p3_inst_timeseries_summary_csv,
    target_summary_stats(p1_inst_data, "Value_Inst",
                         "3_visualize/log/inst_timeseries_summary.csv"),
    format = "file",
    repository = 'local',
    deployment = 'main'
  ),
  
  # Render data summary report (note that tar_render returns a target with format="file") 
  # note - this is commented out b/c I'm getting an error 
  # see https://github.com/USGS-R/drb-inland-salinity-ml/issues/154
  #tarchetypes::tar_render(p3_SC_report, 
                          #"3_visualize/src/report-wqp-salinity-data.Rmd",
                          #output_dir = "3_visualize/out",
                          #deployment = 'main'
  #),
  
  # Plot distribution of NHDv2 attribute variables across the PRMS network
  # see note at top of 1_fetch.R re why this is a 'local' target
  tar_target(
    p3_nhdv2_attr_png,
    plot_nhdv2_attr(attr_data = p2_nhdv2_attr,
                    network_geometry = p1_reaches_sf,
                    file_path = "3_visualize/out/nhdv2_attr_png"),
    format = "file",
    repository = 'local'
  ),

  # refined
  # see note at top of 1_fetch.R re why this is a 'local' target
  tar_target(
    p3_nhdv2_attr_refined_png,
    plot_nhdv2_attr(attr_data = p2_nhdv2_attr_refined %>% select(-hru_segment),
                    network_geometry = p1_reaches_sf,
                    file_path = "3_visualize/out/nhdv2_attr_png/refined"),
    format = "file",
    repository = 'local'
  ),
  
  # Create and save a summary table that describes variation in the NHDv2 attribute variables across the PRMS network
  tar_target(
    p3_nhdv2_attr_summary_csv,
    summarize_nhdv2_attr(p2_nhdv2_attr,"3_visualize/out/nhdv2_attr_summary.csv"),
    format = "file",
    repository = 'local'
  ),
  # refined
  tar_target(
    p3_nhdv2_attr_summary_refined_csv,
    summarize_nhdv2_attr(p2_nhdv2_attr_refined %>% select(-hru_segment),
                         "3_visualize/out/nhdv2_attr_summary_refined.csv"),
    format = "file",
    repository = 'local'
  ),
  
  # Create and save a summary table that indicates the NA's among contributing 
  # NHDv2 catchments for each PRMS segment and attribute variable
  tar_target(
    p3_nhdv2_attr_missing_data_csv,
    summarize_catchment_nhdv2_attr_missing(p2_nhdv2_attr_catchment,"3_visualize/out/nhdv2_attr_missing_data.csv"),
    format = "file",
    repository = 'local'
  ),
  
  # Create a list of model results that will be passed to model performance
  # plotting functions.
  tar_target(
    p3_model_results,
    {
      model_results_list <- list(static_dynamic = p4_pred_RF_static_dynamic_temporal_test$pred,
                                 min_static_dynamic = p4_pred_RF_min_static_dynamic_temporal_test$pred,
                                 dynamic = p4_pred_RF_dynamic_temporal_test$pred)
      model_results <- purrr::map_df(model_results_list, ~as.data.frame(.x), .id = "model")
      model_results
    },
    repository = "local"
  ),
  
  # Plot empirical CDFs of model performance
  tar_target(
    p3_ecdf_all_reaches_png,
    plot_ecdf(model_results = p3_model_results, 
              plot_type = "all_reaches", 
              fileout = "3_visualize/out/ecdf_all_reaches.png",
              log_x_axis = TRUE,
              plot_points = FALSE,
              plot_width_in = 6, plot_height_in = 4),
    repository = "local",
    format = "file"
  )
  
)

