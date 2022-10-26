source("4_predict/src/select_features.R")
source("4_predict/src/train_models.R")
source("4_predict/src/plot_diagnostics.R")
source("1_fetch/src/generate_credentials.R")

#Predict phase
p4_plot_targets_list <- list(
  # Visualize Model Diagnostics:
  # see note at top of 1_fetch.R re why these are 'local' file targets
  
  # Boruta screening
  # tar_target(p4_Boruta_static_png,
  #            plot_Boruta(p4_Boruta_static$brf_All,
  #                        model_name = 'daily_SC_RF',
  #                        out_dir = '4_predict/out/Boruta'),
  #            deployment = 'main',
  #            format = 'file'
  # ),
  
  # RF variable importance plot 
  #Should add error bars over X random seeds
  tar_target(p4_vip_static_png,
             plot_vip(RF_model = p4_train_RF_static$best_fit,
                      model_name = 'daily_SC_RF_static',
                      num_features = 20,
                      out_dir = '4_predict/out/random/vip'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_vip_static_dynamic_png,
             plot_vip(RF_model = p4_train_RF_static_dynamic$best_fit,
                      model_name = 'daily_SC_RF_static_dynamic',
                      num_features = 20,
                      out_dir = '4_predict/out/random/vip'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_vip_min_static_png,
             plot_vip(RF_model = p4_train_RF_min_static$best_fit,
                      model_name = 'daily_SC_RF_min_static',
                      num_features = 20,
                      out_dir = '4_predict/out/random/vip'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_vip_min_static_dynamic_png,
             plot_vip(RF_model = p4_train_RF_min_static_dynamic$best_fit,
                      model_name = 'daily_SC_RF_min_static_dynamic',
                      num_features = 20,
                      out_dir = '4_predict/out/random/vip'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_vip_dynamic_png,
             plot_vip(RF_model = p4_train_RF_dynamic$best_fit,
                      model_name = 'daily_SC_RF_dynamic',
                      num_features = 20,
                      out_dir = '4_predict/out/random/vip'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_vip_static_dynamic_temporal_png,
             plot_vip(RF_model = p4_train_RF_static_dynamic_temporal$best_fit,
                      model_name = 'daily_SC_RF_static_dynamic_temporal',
                      num_features = 20,
                      out_dir = '4_predict/out/temporal/vip'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_vip_min_static_dynamic_temporal_png,
             plot_vip(RF_model = p4_train_RF_min_static_dynamic_temporal$best_fit,
                      model_name = 'daily_SC_RF_min_static_dynamic_temporal',
                      num_features = 20,
                      out_dir = '4_predict/out/temporal/vip'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_vip_dynamic_temporal_png,
             plot_vip(RF_model = p4_train_RF_dynamic_temporal$best_fit,
                      model_name = 'daily_SC_RF_dynamic_temporal',
                      num_features = 20,
                      out_dir = '4_predict/out/temporal/vip'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  
  # RF hyperparameter optimization
  tar_target(p4_hypopt_static_png,
             plot_hyperparam_opt_results_RF(p4_train_RF_static$grid_params,
                                            model_name = 'daily_SC_RF_static',
                                            out_dir = '4_predict/out/random/hypopt'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_hypopt_marginals_static_png,
             plot_hyperparam_opt_marginals(p4_train_RF_static$grid_params,
                                           model_name = 'daily_SC_RF_static',
                                           plt_type = "marginals",
                                           perf_metric = NULL,
                                           out_dir = '4_predict/out/random/hypopt'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_hypopt_static_dynamic_png,
             plot_hyperparam_opt_results_RF(p4_train_RF_static_dynamic$grid_params,
                                            model_name = 'daily_SC_RF_static_dynamic',
                                            out_dir = '4_predict/out/random/hypopt'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_hypopt_marginals_static_dynamic_png,
             plot_hyperparam_opt_marginals(p4_train_RF_static_dynamic$grid_params,
                                           model_name = 'daily_SC_RF_static_dynamic',
                                           plt_type = "marginals",
                                           perf_metric = NULL,
                                           out_dir = '4_predict/out/random/hypopt'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_hypopt_min_static_png,
             plot_hyperparam_opt_results_RF(p4_train_RF_min_static$grid_params,
                                            model_name = 'daily_SC_RF_min_static',
                                            out_dir = '4_predict/out/random/hypopt'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_hypopt_marginals_min_static_png,
             plot_hyperparam_opt_marginals(p4_train_RF_min_static$grid_params,
                                           model_name = 'daily_SC_RF_min_static',
                                           plt_type = "marginals",
                                           perf_metric = NULL,
                                           out_dir = '4_predict/out/random/hypopt'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_hypopt_min_static_dynamic_png,
             plot_hyperparam_opt_results_RF(p4_train_RF_min_static_dynamic$grid_params,
                                            model_name = 'daily_SC_RF_min_static_dynamic',
                                            out_dir = '4_predict/out/random/hypopt'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_hypopt_marginals_min_static_dynamic_png,
             plot_hyperparam_opt_marginals(p4_train_RF_min_static_dynamic$grid_params,
                                           model_name = 'daily_SC_RF_min_static_dynamic',
                                           plt_type = "marginals",
                                           perf_metric = NULL,
                                           out_dir = '4_predict/out/random/hypopt'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_hypopt_dynamic_png,
             plot_hyperparam_opt_results_RF(p4_train_RF_dynamic$grid_params,
                                            model_name = 'daily_SC_RF_dynamic',
                                            out_dir = '4_predict/out/random/hypopt'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_hypopt_marginals_dynamic_png,
             plot_hyperparam_opt_marginals(p4_train_RF_dynamic$grid_params,
                                           model_name = 'daily_SC_RF_dynamic',
                                           plt_type = "marginals",
                                           perf_metric = NULL,
                                           out_dir = '4_predict/out/random/hypopt'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_hypopt_static_dynamic_temporal_png,
             plot_hyperparam_opt_results_RF(p4_train_RF_static_dynamic_temporal$grid_params,
                                            model_name = 'daily_SC_RF_static_dynamic_temporal',
                                            out_dir = '4_predict/out/temporal/hypopt'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_hypopt_marginals_static_dynamic_temporal_png,
             plot_hyperparam_opt_marginals(p4_train_RF_static_dynamic_temporal$grid_params,
                                           model_name = 'daily_SC_RF_static_dynamic_temporal',
                                           plt_type = "marginals",
                                           perf_metric = NULL,
                                           out_dir = '4_predict/out/temporal/hypopt'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_hypopt_min_static_dynamic_temporal_png,
             plot_hyperparam_opt_results_RF(p4_train_RF_min_static_dynamic_temporal$grid_params,
                                            model_name = 'daily_SC_RF_min_static_dynamic_temporal',
                                            out_dir = '4_predict/out/temporal/hypopt'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_hypopt_marginals_min_static_dynamic_temporal_png,
             plot_hyperparam_opt_marginals(p4_train_RF_min_static_dynamic_temporal$grid_params,
                                           model_name = 'daily_SC_RF_min_static_dynamic_temporal',
                                           plt_type = "marginals",
                                           perf_metric = NULL,
                                           out_dir = '4_predict/out/temporal/hypopt'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_hypopt_dynamic_temporal_png,
             plot_hyperparam_opt_results_RF(p4_train_RF_dynamic_temporal$grid_params,
                                            model_name = 'daily_SC_RF_dynamic_temporal',
                                            out_dir = '4_predict/out/temporal/hypopt'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_hypopt_marginals_dynamic_temporal_png,
             plot_hyperparam_opt_marginals(p4_train_RF_dynamic_temporal$grid_params,
                                           model_name = 'daily_SC_RF_dynamic_temporal',
                                           plt_type = "marginals",
                                           perf_metric = NULL,
                                           out_dir = '4_predict/out/temporal/hypopt'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  
  
  # RF predicted vs. observed y 
  #Should be for the mean over X random seeds
  tar_target(p4_pred_obs_static_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_static$pred,
                           model_name = 'daily_SC_RF_static',
                           out_dir = '4_predict/out/random/pred_obs/RF_static', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_static_test_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_static_test$pred,
                           model_name = 'daily_SC_RF_static_test',
                           out_dir = '4_predict/out/random/pred_obs/RF_static', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_static_dynamic_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_static_dynamic$pred,
                           model_name = 'daily_SC_RF_static_dynamic',
                           out_dir = '4_predict/out/random/pred_obs/RF_static_dynamic', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_static_dynamic_test_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_static_dynamic_test$pred,
                           model_name = 'daily_SC_RF_static_dynamic_test',
                           out_dir = '4_predict/out/random/pred_obs/RF_static_dynamic', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_min_static_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_min_static$pred,
                           model_name = 'daily_SC_RF_min_static',
                           out_dir = '4_predict/out/random/pred_obs/RF_min_static', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_min_static_test_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_min_static_test$pred,
                           model_name = 'daily_SC_RF_min_static_test',
                           out_dir = '4_predict/out/random/pred_obs/RF_min_static', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_min_static_dynamic_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_min_static_dynamic$pred,
                           model_name = 'daily_SC_RF_min_static_dynamic',
                           out_dir = '4_predict/out/random/pred_obs/RF_min_static_dynamic', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_min_static_dynamic_test_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_min_static_dynamic_test$pred,
                           model_name = 'daily_SC_RF_min_static_dynamic_test',
                           out_dir = '4_predict/out/random/pred_obs/RF_min_static_dynamic', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_dynamic_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_dynamic$pred,
                           model_name = 'daily_SC_RF_dynamic',
                           out_dir = '4_predict/out/random/pred_obs/RF_dynamic', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_dynamic_test_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_dynamic_test$pred,
                           model_name = 'daily_SC_RF_dynamic_test',
                           out_dir = '4_predict/out/random/pred_obs/RF_dynamic', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_static_dynamic_temporal_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_static_dynamic_temporal$pred,
                           model_name = 'daily_SC_RF_static_dynamic_temporal',
                           out_dir = '4_predict/out/temporal/pred_obs/RF_static_dynamic', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_static_dynamic_temporal_test_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_static_dynamic_temporal_test$pred,
                           model_name = 'daily_SC_RF_static_dynamic_temporal_test',
                           out_dir = '4_predict/out/temporal/pred_obs/RF_static_dynamic', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_min_static_dynamic_temporal_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_min_static_dynamic_temporal$pred,
                           model_name = 'daily_SC_RF_min_static_dynamic_temporal',
                           out_dir = '4_predict/out/temporal/pred_obs/RF_min_static_dynamic', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_min_static_dynamic_temporal_test_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_min_static_dynamic_temporal_test$pred,
                           model_name = 'daily_SC_RF_min_static_dynamic_temporal_test',
                           out_dir = '4_predict/out/temporal/pred_obs/RF_min_static_dynamic', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_dynamic_temporal_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_dynamic_temporal$pred,
                           model_name = 'daily_SC_RF_dynamic_temporal',
                           out_dir = '4_predict/out/temporal/pred_obs/RF_dynamic', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_dynamic_temporal_test_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_dynamic_temporal_test$pred,
                           model_name = 'daily_SC_RF_dynamic_temporal_test',
                           out_dir = '4_predict/out/temporal/pred_obs/RF_dynamic', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  
  
  #Train test boxplot coverage of SC data
  tar_target(p4_train_test_boxplot_coverage_png,
             plot_metric_boxplot(data_split = p4_Boruta_static$input_data,
                                 model_name = 'daily_SC_RF',
                                 pred_var = 'mean_value',
                                 out_dir = '4_predict/out/random/pred_obs'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_train_test_boxplot_coverage_temporal_png,
             plot_metric_boxplot(data_split = p4_selected_static_dynamic_attrs_temporal$input_data,
                                 model_name = 'daily_SC_RF',
                                 pred_var = 'mean_value',
                                 out_dir = '4_predict/out/temporal/pred_obs'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  
  
  #Cross validation coverage barplots
  tar_target(p4_train_test_CV_static_png,
             barplot_compare_RF(mod = p4_train_RF_static,
                                model_name = 'daily_SC_RF_static',
                                pred_var = 'mean_value',
                                perf_metric = 'rmse',
                                out_dir = '4_predict/out/random/pred_obs/RF_static'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_train_test_CV_static_dynamic_png,
             barplot_compare_RF(mod = p4_train_RF_static_dynamic,
                                model_name = 'daily_SC_RF_static_dynamic',
                                pred_var = 'mean_value',
                                perf_metric = 'rmse',
                                out_dir = '4_predict/out/random/pred_obs/RF_static_dynamic'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_train_test_CV_min_static_png,
             barplot_compare_RF(mod = p4_train_RF_min_static,
                                model_name = 'daily_SC_RF_min_static',
                                pred_var = 'mean_value',
                                perf_metric = 'rmse',
                                out_dir = '4_predict/out/random/pred_obs/RF_min_static'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_train_test_CV_min_static_dynamic_png,
             barplot_compare_RF(mod = p4_train_RF_min_static_dynamic,
                                model_name = 'daily_SC_RF_min_static_dynamic',
                                pred_var = 'mean_value',
                                perf_metric = 'rmse',
                                out_dir = '4_predict/out/random/pred_obs/RF_min_static_dynamic'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_train_test_CV_dynamic_png,
             barplot_compare_RF(mod = p4_train_RF_dynamic,
                                model_name = 'daily_SC_RF_dynamic',
                                pred_var = 'mean_value',
                                perf_metric = 'rmse',
                                out_dir = '4_predict/out/random/pred_obs/RF_dynamic'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_train_test_CV_static_dynamic_temporal_png,
             barplot_compare_RF(mod = p4_train_RF_static_dynamic_temporal,
                                model_name = 'daily_SC_RF_static_dynamic_temporal',
                                pred_var = 'mean_value',
                                perf_metric = 'rmse',
                                out_dir = '4_predict/out/temporal/pred_obs/RF_static_dynamic'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_train_test_CV_min_static_dynamic_temporal_png,
             barplot_compare_RF(mod = p4_train_RF_min_static_dynamic_temporal,
                                model_name = 'daily_SC_RF_min_static_dynamic_temporal',
                                pred_var = 'mean_value',
                                perf_metric = 'rmse',
                                out_dir = '4_predict/out/temporal/pred_obs/RF_min_static_dynamic'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_train_test_CV_dynamic_temporal_png,
             barplot_compare_RF(mod = p4_train_RF_dynamic_temporal,
                                model_name = 'daily_SC_RF_dynamic_temporal',
                                pred_var = 'mean_value',
                                perf_metric = 'rmse',
                                out_dir = '4_predict/out/temporal/pred_obs/RF_dynamic'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  
  
  # Spatial residuals aggregated over time
  tar_target(
    p4_spatial_res_static_png,
    {
      #Average RMSE over all time for each PRMS segment
      PRMS_seg_RMSE <- summarize(group_by(p4_pred_RF_static$pred, PRMS_segid), 
                                 RMSE = sqrt(mean(errsq)),
                                 RMSE_log10 = log10(RMSE))
      plot_nhdv2_attr(attr_data = PRMS_seg_RMSE,
                      network_geometry = p1_reaches_sf,
                      file_path = "4_predict/out/random/spatial_res/RF_static",
                      filename_end = '_full')
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_spatial_res_static_test_png,
    {
      #Average RMSE over all time for each PRMS segment
      PRMS_seg_RMSE <- summarize(group_by(p4_pred_RF_static_test$pred, PRMS_segid), 
                                 RMSE = sqrt(mean(errsq)),
                                 RMSE_log10 = log10(RMSE))
      plot_nhdv2_attr(attr_data = PRMS_seg_RMSE,
                      network_geometry = p1_reaches_sf,
                      file_path = "4_predict/out/random/spatial_res/RF_static",
                      filename_end = '_test')
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_spatial_res_min_static_png,
    {
      #Average RMSE over all time for each PRMS segment
      PRMS_seg_RMSE <- summarize(group_by(p4_pred_RF_min_static$pred, PRMS_segid), 
                                 RMSE = sqrt(mean(errsq)),
                                 RMSE_log10 = log10(RMSE))
      plot_nhdv2_attr(attr_data = PRMS_seg_RMSE,
                      network_geometry = p1_reaches_sf,
                      file_path = "4_predict/out/random/spatial_res/RF_min_static",
                      filename_end = '_full')
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_spatial_res_min_static_test_png,
    {
      #Average RMSE over all time for each PRMS segment
      PRMS_seg_RMSE <- summarize(group_by(p4_pred_RF_min_static_test$pred, PRMS_segid), 
                                 RMSE = sqrt(mean(errsq)),
                                 RMSE_log10 = log10(RMSE))
      plot_nhdv2_attr(attr_data = PRMS_seg_RMSE,
                      network_geometry = p1_reaches_sf,
                      file_path = "4_predict/out/random/spatial_res/RF_min_static",
                      filename_end = '_test')
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_spatial_res_static_dynamic_png,
    {
      #Average RMSE over all time for each PRMS segment
      PRMS_seg_RMSE <- summarize(group_by(p4_pred_RF_static_dynamic$pred, PRMS_segid), 
                                 RMSE = sqrt(mean(errsq)),
                                 RMSE_log10 = log10(RMSE))
      plot_nhdv2_attr(attr_data = PRMS_seg_RMSE,
                      network_geometry = p1_reaches_sf,
                      file_path = "4_predict/out/random/spatial_res/RF_static_dynamic",
                      filename_end = '_full')
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_spatial_res_static_dynamic_test_png,
    {
      #Average RMSE over all time for each PRMS segment
      PRMS_seg_RMSE <- summarize(group_by(p4_pred_RF_static_dynamic_test$pred, PRMS_segid), 
                                 RMSE = sqrt(mean(errsq)),
                                 RMSE_log10 = log10(RMSE))
      plot_nhdv2_attr(attr_data = PRMS_seg_RMSE,
                      network_geometry = p1_reaches_sf,
                      file_path = "4_predict/out/random/spatial_res/RF_static_dynamic",
                      filename_end = '_test')
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_spatial_res_min_static_dynamic_png,
    {
      #Average RMSE over all time for each PRMS segment
      PRMS_seg_RMSE <- summarize(group_by(p4_pred_RF_min_static_dynamic$pred, PRMS_segid), 
                                 RMSE = sqrt(mean(errsq)),
                                 RMSE_log10 = log10(RMSE))
      plot_nhdv2_attr(attr_data = PRMS_seg_RMSE,
                      network_geometry = p1_reaches_sf,
                      file_path = "4_predict/out/random/spatial_res/RF_min_static_dynamic",
                      filename_end = '_full')
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_spatial_res_min_static_dynamic_test_png,
    {
      #Average RMSE over all time for each PRMS segment
      PRMS_seg_RMSE <- summarize(group_by(p4_pred_RF_min_static_dynamic_test$pred, PRMS_segid), 
                                 RMSE = sqrt(mean(errsq)),
                                 RMSE_log10 = log10(RMSE))
      plot_nhdv2_attr(attr_data = PRMS_seg_RMSE,
                      network_geometry = p1_reaches_sf,
                      file_path = "4_predict/out/random/spatial_res/RF_min_static_dynamic",
                      filename_end = '_test')
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_spatial_res_dynamic_png,
    {
      #Average RMSE over all time for each PRMS segment
      PRMS_seg_RMSE <- summarize(group_by(p4_pred_RF_dynamic$pred, PRMS_segid), 
                                 RMSE = sqrt(mean(errsq)),
                                 RMSE_log10 = log10(RMSE))
      plot_nhdv2_attr(attr_data = PRMS_seg_RMSE,
                      network_geometry = p1_reaches_sf,
                      file_path = "4_predict/out/random/spatial_res/RF_dynamic",
                      filename_end = '_full')
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_spatial_res_dynamic_test_png,
    {
      #Average RMSE over all time for each PRMS segment
      PRMS_seg_RMSE <- summarize(group_by(p4_pred_RF_dynamic_test$pred, PRMS_segid), 
                                 RMSE = sqrt(mean(errsq)),
                                 RMSE_log10 = log10(RMSE))
      plot_nhdv2_attr(attr_data = PRMS_seg_RMSE,
                      network_geometry = p1_reaches_sf,
                      file_path = "4_predict/out/random/spatial_res/RF_dynamic",
                      filename_end = '_test')
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_spatial_res_static_dynamic_temporal_png,
    {
      #Average RMSE over all time for each PRMS segment
      PRMS_seg_RMSE <- summarize(group_by(p4_pred_RF_static_dynamic_temporal$pred, PRMS_segid), 
                                 RMSE = sqrt(mean(errsq)),
                                 RMSE_log10 = log10(RMSE))
      plot_nhdv2_attr(attr_data = PRMS_seg_RMSE,
                      network_geometry = p1_reaches_sf,
                      file_path = "4_predict/out/temporal/spatial_res/RF_static_dynamic",
                      filename_end = '_full')
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_spatial_res_static_dynamic_temporal_test_png,
    {
      #Average RMSE over all time for each PRMS segment
      PRMS_seg_RMSE <- summarize(group_by(p4_pred_RF_static_dynamic_temporal_test$pred, PRMS_segid), 
                                 RMSE = sqrt(mean(errsq)),
                                 RMSE_log10 = log10(RMSE))
      plot_nhdv2_attr(attr_data = PRMS_seg_RMSE,
                      network_geometry = p1_reaches_sf,
                      file_path = "4_predict/out/temporal/spatial_res/RF_static_dynamic",
                      filename_end = '_test')
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_spatial_res_min_static_dynamic_temporal_png,
    {
      #Average RMSE over all time for each PRMS segment
      PRMS_seg_RMSE <- summarize(group_by(p4_pred_RF_min_static_dynamic_temporal$pred, PRMS_segid), 
                                 RMSE = sqrt(mean(errsq)),
                                 RMSE_log10 = log10(RMSE))
      plot_nhdv2_attr(attr_data = PRMS_seg_RMSE,
                      network_geometry = p1_reaches_sf,
                      file_path = "4_predict/out/temporal/spatial_res/RF_min_static_dynamic",
                      filename_end = '_full')
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_spatial_res_min_static_dynamic_temporal_test_png,
    {
      #Average RMSE over all time for each PRMS segment
      PRMS_seg_RMSE <- summarize(group_by(p4_pred_RF_min_static_dynamic_temporal_test$pred, PRMS_segid), 
                                 RMSE = sqrt(mean(errsq)),
                                 RMSE_log10 = log10(RMSE))
      plot_nhdv2_attr(attr_data = PRMS_seg_RMSE,
                      network_geometry = p1_reaches_sf,
                      file_path = "4_predict/out/temporal/spatial_res/RF_min_static_dynamic",
                      filename_end = '_test')
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_spatial_res_dynamic_temporal_png,
    {
      #Average RMSE over all time for each PRMS segment
      PRMS_seg_RMSE <- summarize(group_by(p4_pred_RF_dynamic_temporal$pred, PRMS_segid), 
                                 RMSE = sqrt(mean(errsq)),
                                 RMSE_log10 = log10(RMSE))
      plot_nhdv2_attr(attr_data = PRMS_seg_RMSE,
                      network_geometry = p1_reaches_sf,
                      file_path = "4_predict/out/temporal/spatial_res/RF_dynamic",
                      filename_end = '_full')
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_spatial_res_dynamic_temporal_test_png,
    {
      #Average RMSE over all time for each PRMS segment
      PRMS_seg_RMSE <- summarize(group_by(p4_pred_RF_dynamic_temporal_test$pred, PRMS_segid), 
                                 RMSE = sqrt(mean(errsq)),
                                 RMSE_log10 = log10(RMSE))
      plot_nhdv2_attr(attr_data = PRMS_seg_RMSE,
                      network_geometry = p1_reaches_sf,
                      file_path = "4_predict/out/temporal/spatial_res/RF_dynamic",
                      filename_end = '_test')
    },
    format = "file",
    repository = 'local'
  ),
  
  
  #Monthly RMSE and Bias barplots
  #aggregated over all segments and timesteps within the month
  tar_target(
    p4_monthly_res_static_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_month <- summarize(group_by(p4_pred_RF_static$pred, Month),
                                 RMSE = sqrt(mean(errsq)),
                                 Bias = mean(err)) %>%
        arrange(Month)
      plot_barplot(attr_data = PRMS_month,
                   file_path = "4_predict/out/random/monthly_res/RF_static",
                   model_name = 'RF_static_full', 
                   panel = TRUE,
                   plot_month_names = TRUE)
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_monthly_res_static_test_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_month <- summarize(group_by(p4_pred_RF_static_test$pred, Month),
                              RMSE = sqrt(mean(errsq)),
                              Bias = mean(err)) %>%
        arrange(Month)
      plot_barplot(attr_data = PRMS_month,
                   file_path = "4_predict/out/random/monthly_res/RF_static",
                   model_name = 'RF_static_test', 
                   panel = TRUE,
                   plot_month_names = TRUE)
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_monthly_res_min_static_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_month <- summarize(group_by(p4_pred_RF_min_static$pred, Month),
                              RMSE = sqrt(mean(errsq)),
                              Bias = mean(err)) %>%
        arrange(Month)
      plot_barplot(attr_data = PRMS_month,
                   file_path = "4_predict/out/random/monthly_res/RF_min_static",
                   model_name = 'RF_min_static_full', 
                   panel = TRUE,
                   plot_month_names = TRUE)
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_monthly_res_min_static_test_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_month <- summarize(group_by(p4_pred_RF_min_static_test$pred, Month),
                              RMSE = sqrt(mean(errsq)),
                              Bias = mean(err)) %>%
        arrange(Month)
      plot_barplot(attr_data = PRMS_month,
                   file_path = "4_predict/out/random/monthly_res/RF_min_static",
                   model_name = 'RF_min_static_test', 
                   panel = TRUE,
                   plot_month_names = TRUE)
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_monthly_res_static_dynamic_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_month <- summarize(group_by(p4_pred_RF_static_dynamic$pred, Month),
                              RMSE = sqrt(mean(errsq)),
                              Bias = mean(err)) %>%
        arrange(Month)
      plot_barplot(attr_data = PRMS_month,
                   file_path = "4_predict/out/random/monthly_res/RF_static_dynamic",
                   model_name = 'RF_static_dynamic_full', 
                   panel = TRUE,
                   plot_month_names = TRUE)
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_monthly_res_static_dynamic_test_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_month <- summarize(group_by(p4_pred_RF_static_dynamic_test$pred, Month),
                              RMSE = sqrt(mean(errsq)),
                              Bias = mean(err)) %>%
        arrange(Month)
      plot_barplot(attr_data = PRMS_month,
                   file_path = "4_predict/out/random/monthly_res/RF_static_dynamic",
                   model_name = 'RF_static_dynamic_test', 
                   panel = TRUE,
                   plot_month_names = TRUE)
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_monthly_res_min_static_dynamic_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_month <- summarize(group_by(p4_pred_RF_min_static_dynamic$pred, Month),
                              RMSE = sqrt(mean(errsq)),
                              Bias = mean(err)) %>%
        arrange(Month)
      plot_barplot(attr_data = PRMS_month,
                   file_path = "4_predict/out/random/monthly_res/RF_min_static_dynamic",
                   model_name = 'RF_min_static_dynamic_full', 
                   panel = TRUE,
                   plot_month_names = TRUE)
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_monthly_res_min_static_dynamic_test_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_month <- summarize(group_by(p4_pred_RF_min_static_dynamic_test$pred, Month),
                              RMSE = sqrt(mean(errsq)),
                              Bias = mean(err)) %>%
        arrange(Month)
      plot_barplot(attr_data = PRMS_month,
                   file_path = "4_predict/out/random/monthly_res/RF_min_static_dynamic",
                   model_name = 'RF_min_static_dynamic_test', 
                   panel = TRUE,
                   plot_month_names = TRUE)
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_monthly_res_dynamic_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_month <- summarize(group_by(p4_pred_RF_dynamic$pred, Month),
                              RMSE = sqrt(mean(errsq)),
                              Bias = mean(err)) %>%
        arrange(Month)
      plot_barplot(attr_data = PRMS_month,
                   file_path = "4_predict/out/random/monthly_res/RF_dynamic",
                   model_name = 'RF_dynamic_full', 
                   panel = TRUE,
                   plot_month_names = TRUE)
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_monthly_res_dynamic_test_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_month <- summarize(group_by(p4_pred_RF_dynamic_test$pred, Month),
                              RMSE = sqrt(mean(errsq)),
                              Bias = mean(err)) %>%
        arrange(Month)
      plot_barplot(attr_data = PRMS_month,
                   file_path = "4_predict/out/random/monthly_res/RF_dynamic",
                   model_name = 'RF_dynamic_test', 
                   panel = TRUE,
                   plot_month_names = TRUE)
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_monthly_res_static_dynamic_temporal_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_month <- summarize(group_by(p4_pred_RF_static_dynamic_temporal$pred, Month),
                              RMSE = sqrt(mean(errsq)),
                              Bias = mean(err)) %>%
        arrange(Month)
      plot_barplot(attr_data = PRMS_month,
                   file_path = "4_predict/out/temporal/monthly_res/RF_static_dynamic",
                   model_name = 'RF_static_dynamic_temporal_full', 
                   panel = TRUE,
                   plot_month_names = TRUE)
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_monthly_res_static_dynamic_temporal_test_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_month <- summarize(group_by(p4_pred_RF_static_dynamic_temporal_test$pred, Month),
                              RMSE = sqrt(mean(errsq)),
                              Bias = mean(err)) %>%
        arrange(Month)
      plot_barplot(attr_data = PRMS_month,
                   file_path = "4_predict/out/temporal/monthly_res/RF_static_dynamic",
                   model_name = 'RF_static_dynamic_temporal_test', 
                   panel = TRUE,
                   plot_month_names = TRUE)
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_monthly_res_min_static_dynamic_temporal_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_month <- summarize(group_by(p4_pred_RF_min_static_dynamic_temporal$pred, Month),
                              RMSE = sqrt(mean(errsq)),
                              Bias = mean(err)) %>%
        arrange(Month)
      plot_barplot(attr_data = PRMS_month,
                   file_path = "4_predict/out/temporal/monthly_res/RF_min_static_dynamic",
                   model_name = 'RF_min_static_dynamic_temporal_full', 
                   panel = TRUE,
                   plot_month_names = TRUE)
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_monthly_res_min_static_dynamic_temporal_test_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_month <- summarize(group_by(p4_pred_RF_min_static_dynamic_temporal_test$pred, Month),
                              RMSE = sqrt(mean(errsq)),
                              Bias = mean(err)) %>%
        arrange(Month)
      plot_barplot(attr_data = PRMS_month,
                   file_path = "4_predict/out/temporal/monthly_res/RF_min_static_dynamic",
                   model_name = 'RF_min_static_dynamic_temporal_test', 
                   panel = TRUE,
                   plot_month_names = TRUE)
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_monthly_res_dynamic_temporal_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_month <- summarize(group_by(p4_pred_RF_dynamic_temporal$pred, Month),
                              RMSE = sqrt(mean(errsq)),
                              Bias = mean(err)) %>%
        arrange(Month)
      plot_barplot(attr_data = PRMS_month,
                   file_path = "4_predict/out/temporal/monthly_res/RF_dynamic",
                   model_name = 'RF_dynamic_temporal_full', 
                   panel = TRUE,
                   plot_month_names = TRUE)
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_monthly_res_dynamic_temporal_test_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_month <- summarize(group_by(p4_pred_RF_dynamic_temporal_test$pred, Month),
                              RMSE = sqrt(mean(errsq)),
                              Bias = mean(err)) %>%
        arrange(Month)
      plot_barplot(attr_data = PRMS_month,
                   file_path = "4_predict/out/temporal/monthly_res/RF_dynamic",
                   model_name = 'RF_dynamic_temporal_test', 
                   panel = TRUE,
                   plot_month_names = TRUE)
    },
    format = "file",
    repository = 'local'
  ),
  
  #Annual RMSE and Bias over time, aggregated over all reaches
  tar_target(
    p4_annual_res_static_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_ann <- summarize(group_by(p4_pred_RF_static$pred, Year),
                              RMSE = sqrt(mean(errsq)),
                              Bias = mean(err)) %>%
        arrange(Year)
      plot_barplot(attr_data = PRMS_ann,
                   file_path = "4_predict/out/random/annual_res/RF_static",
                   model_name = 'RF_static_full',
                   panel = TRUE,
                   label_sequence = seq(1,length(unique(PRMS_ann$Year)),3))
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_annual_res_static_test_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_ann <- summarize(group_by(p4_pred_RF_static_test$pred, Year),
                            RMSE = sqrt(mean(errsq)),
                            Bias = mean(err)) %>%
        arrange(Year)
      plot_barplot(attr_data = PRMS_ann,
                   file_path = "4_predict/out/random/annual_res/RF_static",
                   model_name = 'RF_static_test',
                   panel = TRUE,
                   label_sequence = seq(1,length(unique(PRMS_ann$Year)),3))
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_annual_res_min_static_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_ann <- summarize(group_by(p4_pred_RF_min_static$pred, Year),
                            RMSE = sqrt(mean(errsq)),
                            Bias = mean(err)) %>%
        arrange(Year)
      plot_barplot(attr_data = PRMS_ann,
                   file_path = "4_predict/out/random/annual_res/RF_min_static",
                   model_name = 'RF_min_static_full',
                   panel = TRUE,
                   label_sequence = seq(1,length(unique(PRMS_ann$Year)),3))
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_annual_res_min_static_test_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_ann <- summarize(group_by(p4_pred_RF_min_static_test$pred, Year),
                            RMSE = sqrt(mean(errsq)),
                            Bias = mean(err)) %>%
        arrange(Year)
      plot_barplot(attr_data = PRMS_ann,
                   file_path = "4_predict/out/random/annual_res/RF_min_static",
                   model_name = 'RF_min_static_test',
                   panel = TRUE,
                   label_sequence = seq(1,length(unique(PRMS_ann$Year)),3))
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_annual_res_static_dynamic_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_ann <- summarize(group_by(p4_pred_RF_static_dynamic$pred, Year),
                            RMSE = sqrt(mean(errsq)),
                            Bias = mean(err)) %>%
        arrange(Year)
      plot_barplot(attr_data = PRMS_ann,
                   file_path = "4_predict/out/random/annual_res/RF_static_dynamic",
                   model_name = 'RF_static_dynamic_full',
                   panel = TRUE,
                   label_sequence = seq(1,length(unique(PRMS_ann$Year)),3))
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_annual_res_static_dynamic_test_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_ann <- summarize(group_by(p4_pred_RF_static_dynamic_test$pred, Year),
                            RMSE = sqrt(mean(errsq)),
                            Bias = mean(err)) %>%
        arrange(Year)
      plot_barplot(attr_data = PRMS_ann,
                   file_path = "4_predict/out/random/annual_res/RF_static_dynamic",
                   model_name = 'RF_static_dynamic_test',
                   panel = TRUE,
                   label_sequence = seq(1,length(unique(PRMS_ann$Year)),3))
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_annual_res_min_static_dynamic_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_ann <- summarize(group_by(p4_pred_RF_min_static_dynamic$pred, Year),
                            RMSE = sqrt(mean(errsq)),
                            Bias = mean(err)) %>%
        arrange(Year)
      plot_barplot(attr_data = PRMS_ann,
                   file_path = "4_predict/out/random/annual_res/RF_min_static_dynamic",
                   model_name = 'RF_min_static_dynamic_full',
                   panel = TRUE,
                   label_sequence = seq(1,length(unique(PRMS_ann$Year)),3))
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_annual_res_min_static_dynamic_test_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_ann <- summarize(group_by(p4_pred_RF_min_static_dynamic_test$pred, Year),
                            RMSE = sqrt(mean(errsq)),
                            Bias = mean(err)) %>%
        arrange(Year)
      plot_barplot(attr_data = PRMS_ann,
                   file_path = "4_predict/out/random/annual_res/RF_min_static_dynamic",
                   model_name = 'RF_min_static_dynamic_test',
                   panel = TRUE,
                   label_sequence = seq(1,length(unique(PRMS_ann$Year)),3))
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_annual_res_dynamic_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_ann <- summarize(group_by(p4_pred_RF_dynamic$pred, Year),
                            RMSE = sqrt(mean(errsq)),
                            Bias = mean(err)) %>%
        arrange(Year)
      plot_barplot(attr_data = PRMS_ann,
                   file_path = "4_predict/out/random/annual_res/RF_dynamic",
                   model_name = 'RF_dynamic_full',
                   panel = TRUE,
                   label_sequence = seq(1,length(unique(PRMS_ann$Year)),3))
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_annual_res_dynamic_test_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_ann <- summarize(group_by(p4_pred_RF_dynamic_test$pred, Year),
                            RMSE = sqrt(mean(errsq)),
                            Bias = mean(err)) %>%
        arrange(Year)
      plot_barplot(attr_data = PRMS_ann,
                   file_path = "4_predict/out/random/annual_res/RF_dynamic",
                   model_name = 'RF_dynamic_test',
                   panel = TRUE,
                   label_sequence = seq(1,length(unique(PRMS_ann$Year)),3))
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_annual_res_static_dynamic_temporal_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_ann <- summarize(group_by(p4_pred_RF_static_dynamic_temporal$pred, Year),
                            RMSE = sqrt(mean(errsq)),
                            Bias = mean(err)) %>%
        arrange(Year)
      plot_barplot(attr_data = PRMS_ann,
                   file_path = "4_predict/out/temporal/annual_res/RF_static_dynamic",
                   model_name = 'RF_static_dynamic_temporal_full',
                   panel = TRUE,
                   label_sequence = seq(1,length(unique(PRMS_ann$Year)),3))
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_annual_res_static_dynamic_temporal_test_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_ann <- summarize(group_by(p4_pred_RF_static_dynamic_temporal_test$pred, Year),
                            RMSE = sqrt(mean(errsq)),
                            Bias = mean(err)) %>%
        arrange(Year)
      plot_barplot(attr_data = PRMS_ann,
                   file_path = "4_predict/out/temporal/annual_res/RF_static_dynamic",
                   model_name = 'RF_static_dynamic_temporal_test',
                   panel = TRUE,
                   label_sequence = seq(1,length(unique(PRMS_ann$Year)),3))
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_annual_res_min_static_dynamic_temporal_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_ann <- summarize(group_by(p4_pred_RF_min_static_dynamic_temporal$pred, Year),
                            RMSE = sqrt(mean(errsq)),
                            Bias = mean(err)) %>%
        arrange(Year)
      plot_barplot(attr_data = PRMS_ann,
                   file_path = "4_predict/out/temporal/annual_res/RF_min_static_dynamic",
                   model_name = 'RF_min_static_dynamic_temporal_full',
                   panel = TRUE,
                   label_sequence = seq(1,length(unique(PRMS_ann$Year)),3))
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_annual_res_min_static_dynamic_temporal_test_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_ann <- summarize(group_by(p4_pred_RF_min_static_dynamic_temporal_test$pred, Year),
                            RMSE = sqrt(mean(errsq)),
                            Bias = mean(err)) %>%
        arrange(Year)
      plot_barplot(attr_data = PRMS_ann,
                   file_path = "4_predict/out/temporal/annual_res/RF_min_static_dynamic",
                   model_name = 'RF_min_static_dynamic_temporal_test',
                   panel = TRUE,
                   label_sequence = seq(1,length(unique(PRMS_ann$Year)),3))
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_annual_res_dynamic_temporal_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_ann <- summarize(group_by(p4_pred_RF_dynamic_temporal$pred, Year),
                            RMSE = sqrt(mean(errsq)),
                            Bias = mean(err)) %>%
        arrange(Year)
      plot_barplot(attr_data = PRMS_ann,
                   file_path = "4_predict/out/temporal/annual_res/RF_dynamic",
                   model_name = 'RF_dynamic_temporal_full',
                   panel = TRUE,
                   label_sequence = seq(1,length(unique(PRMS_ann$Year)),3))
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_annual_res_dynamic_temporal_test_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_ann <- summarize(group_by(p4_pred_RF_dynamic_temporal_test$pred, Year),
                            RMSE = sqrt(mean(errsq)),
                            Bias = mean(err)) %>%
        arrange(Year)
      plot_barplot(attr_data = PRMS_ann,
                   file_path = "4_predict/out/temporal/annual_res/RF_dynamic",
                   model_name = 'RF_dynamic_temporal_test',
                   panel = TRUE,
                   label_sequence = seq(1,length(unique(PRMS_ann$Year)),3))
    },
    format = "file",
    repository = 'local'
  ),
  
  
  #Timeseries of predictions for each reach
  tar_target(
    p4_temporal_res_static_png,
    plot_timeseries(pred_df = p4_pred_RF_static$pred,
                    network_geometry = p1_reaches_sf,
                    model_name = 'RF_static_full',
                    out_dir = "4_predict/out/random/temporal_res/RF_static"),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_temporal_res_static_test_png,
    plot_timeseries(pred_df = p4_pred_RF_static_test$pred,
                    network_geometry = p1_reaches_sf,
                    model_name = 'RF_static_test',
                    out_dir = "4_predict/out/random/temporal_res/RF_static"),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_temporal_res_min_static_png,
    plot_timeseries(pred_df = p4_pred_RF_min_static$pred,
                    network_geometry = p1_reaches_sf,
                    model_name = 'RF_min_static_full',
                    out_dir = "4_predict/out/random/temporal_res/RF_min_static"),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_temporal_res_min_static_test_png,
    plot_timeseries(pred_df = p4_pred_RF_min_static_test$pred,
                    network_geometry = p1_reaches_sf,
                    model_name = 'RF_min_static_test',
                    out_dir = "4_predict/out/random/temporal_res/RF_min_static"),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_temporal_res_static_dynamic_png,
    plot_timeseries(pred_df = p4_pred_RF_static_dynamic$pred,
                    network_geometry = p1_reaches_sf,
                    model_name = 'RF_static_dynamic_full',
                    out_dir = "4_predict/out/random/temporal_res/RF_static_dynamic"),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_temporal_res_static_dynamic_test_png,
    plot_timeseries(pred_df = p4_pred_RF_static_dynamic_test$pred,
                    network_geometry = p1_reaches_sf,
                    model_name = 'RF_static_dynamic_test',
                    out_dir = "4_predict/out/random/temporal_res/RF_static_dynamic"),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_temporal_res_min_static_dynamic_png,
    plot_timeseries(pred_df = p4_pred_RF_min_static_dynamic$pred,
                    network_geometry = p1_reaches_sf,
                    model_name = 'RF_min_static_dynamic_full',
                    out_dir = "4_predict/out/random/temporal_res/RF_min_static_dynamic"),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_temporal_res_min_static_dynamic_test_png,
    plot_timeseries(pred_df = p4_pred_RF_min_static_dynamic_test$pred,
                    network_geometry = p1_reaches_sf,
                    model_name = 'RF_min_static_dynamic_test',
                    out_dir = "4_predict/out/random/temporal_res/RF_min_static_dynamic"),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_temporal_res_dynamic_png,
    plot_timeseries(pred_df = p4_pred_RF_dynamic$pred,
                    network_geometry = p1_reaches_sf,
                    model_name = 'RF_dynamic_full',
                    out_dir = "4_predict/out/random/temporal_res/RF_dynamic"),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_temporal_res_dynamic_test_png,
    plot_timeseries(pred_df = p4_pred_RF_dynamic_test$pred,
                    network_geometry = p1_reaches_sf,
                    model_name = 'RF_dynamic_test',
                    out_dir = "4_predict/out/random/temporal_res/RF_dynamic"),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_temporal_res_static_dynamic_temporal_png,
    plot_timeseries(pred_df = p4_pred_RF_static_dynamic_temporal$pred,
                    network_geometry = p1_reaches_sf,
                    model_name = 'RF_static_dynamic_temporal_full',
                    out_dir = "4_predict/out/temporal/temporal_res/RF_static_dynamic"),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_temporal_res_static_dynamic_temporal_test_png,
    plot_timeseries(pred_df = p4_pred_RF_static_dynamic_temporal_test$pred,
                    network_geometry = p1_reaches_sf,
                    model_name = 'RF_static_dynamic_temporal_test',
                    out_dir = "4_predict/out/temporal/temporal_res/RF_static_dynamic"),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_temporal_res_min_static_dynamic_temporal_png,
    plot_timeseries(pred_df = p4_pred_RF_min_static_dynamic_temporal$pred,
                    network_geometry = p1_reaches_sf,
                    model_name = 'RF_min_static_dynamic_temporal_full',
                    out_dir = "4_predict/out/temporal/temporal_res/RF_min_static_dynamic"),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_temporal_res_min_static_dynamic_temporal_test_png,
    plot_timeseries(pred_df = p4_pred_RF_min_static_dynamic_temporal_test$pred,
                    network_geometry = p1_reaches_sf,
                    model_name = 'RF_min_static_dynamic_temporal_test',
                    out_dir = "4_predict/out/temporal/temporal_res/RF_min_static_dynamic"),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_temporal_res_dynamic_temporal_png,
    plot_timeseries(pred_df = p4_pred_RF_dynamic_temporal$pred,
                    network_geometry = p1_reaches_sf,
                    model_name = 'RF_dynamic_temporal_full',
                    out_dir = "4_predict/out/temporal/temporal_res/RF_dynamic"),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_temporal_res_dynamic_temporal_test_png,
    plot_timeseries(pred_df = p4_pred_RF_dynamic_temporal_test$pred,
                    network_geometry = p1_reaches_sf,
                    model_name = 'RF_dynamic_temporal_test',
                    out_dir = "4_predict/out/temporal/temporal_res/RF_dynamic"),
    format = "file",
    repository = 'local'
  ),


  #SHAP values and plots
  #computation is RAM-limited, so determine number of cores based on available RAM.
  tar_target(
    p4_shap_static,
    {
      #Correct workflow problem
      wf <- extract_workflow(p4_train_RF_static$best_fit)
      data <- p4_train_RF_static$best_fit$splits[[1]]$data %>%
        select(-mean_value) %>%
        as.data.frame()
      data$Date <- p4_Boruta_static$input_data$split$data$Date
      data$PRMS_segid <- p4_Boruta_static$input_data$split$data$PRMS_segid
      
      maxcores <- get_maxcores_by_RAM(SHAP_RAM, RAM_avail = RAM_set)
      compute_shap(model = wf,
                   data = data,
                   ncores = min(maxcores, SHAP_cores),
                   nsim = SHAP_nsim)
    }
  ),
  tar_target(
    p4_shap_min_static,
    {
      maxcores <- get_maxcores_by_RAM(SHAP_RAM, RAM_avail = RAM_set)
    compute_shap(model = p4_train_RF_min_static$workflow,
                 data = p4_train_RF_min_static$best_fit$splits[[1]]$data %>%
                   select(-mean_value) %>%
                   as.data.frame(),
                 ncores = min(maxcores, SHAP_cores),
                 nsim = SHAP_nsim)
    }
  ),
  
  # Refresh AWS credentials
  tar_target(
    p4_aws_credentials_10,
    generate_credentials(dummy_var = p4_shap_min_static),
    deployment = 'main',
    cue = tar_cue('always')
  ),
  
  tar_target(
    p4_shap_static_dynamic,
    {
      maxcores <- get_maxcores_by_RAM(SHAP_RAM, RAM_avail = RAM_set)
    compute_shap(model = p4_train_RF_static_dynamic$workflow,
                 data = p4_train_RF_static_dynamic$best_fit$splits[[1]]$data %>%
                   select(-mean_value) %>%
                   as.data.frame(),
                 ncores = min(maxcores, SHAP_cores),
                 nsim = SHAP_nsim)
    }
  ),
  tar_target(
    p4_shap_min_static_dynamic,
    {
      maxcores <- get_maxcores_by_RAM(SHAP_RAM, RAM_avail = RAM_set)
    compute_shap(model = p4_train_RF_min_static_dynamic$workflow,
                 data = p4_train_RF_min_static_dynamic$best_fit$splits[[1]]$data %>%
                   select(-mean_value) %>%
                   as.data.frame(),
                 ncores = min(maxcores, SHAP_cores),
                 nsim = SHAP_nsim)
    }
  ),
  
  # Refresh AWS credentials
  tar_target(
    p4_aws_credentials_11,
    generate_credentials(dummy_var = p4_shap_min_static_dynamic),
    deployment = 'main',
    cue = tar_cue('always')
  ),
  
  tar_target(
    p4_shap_dynamic,
    {
      maxcores <- get_maxcores_by_RAM(SHAP_RAM, RAM_avail = RAM_set)
    compute_shap(model = p4_train_RF_dynamic$workflow,
                 data = p4_train_RF_dynamic$best_fit$splits[[1]]$data %>%
                   select(-mean_value) %>%
                   as.data.frame(),
                 ncores = min(maxcores, SHAP_cores),
                 nsim = SHAP_nsim)
    }
  ),
  tar_target(
    p4_shap_static_dynamic_temporal,
    {
      maxcores <- get_maxcores_by_RAM(SHAP_RAM, RAM_avail = RAM_set)
    compute_shap(model = p4_train_RF_static_dynamic_temporal$workflow,
                 data = p4_train_RF_static_dynamic_temporal$best_fit$splits[[1]]$data %>%
                   select(-mean_value) %>%
                   as.data.frame(),
                 ncores = min(maxcores, SHAP_cores),
                 nsim = SHAP_nsim)
    }
  ),
  
  # Refresh AWS credentials
  tar_target(
    p4_aws_credentials_12,
    generate_credentials(dummy_var = p4_shap_static_dynamic_temporal),
    deployment = 'main',
    cue = tar_cue('always')
  ),
  
  tar_target(
    p4_shap_min_static_dynamic_temporal,
    {
      maxcores <- get_maxcores_by_RAM(SHAP_RAM, RAM_avail = RAM_set)
      compute_shap(model = p4_train_RF_min_static_dynamic_temporal$workflow,
                   data = p4_train_RF_min_static_dynamic_temporal$best_fit$splits[[1]]$data %>%
                     select(-mean_value) %>%
                     as.data.frame(),
                   ncores = min(maxcores, SHAP_cores),
                   nsim = SHAP_nsim)
    }
  ),
  tar_target(
    p4_shap_dynamic_temporal,
    {
      maxcores <- get_maxcores_by_RAM(SHAP_RAM, RAM_avail = RAM_set)
    compute_shap(model = p4_train_RF_dynamic_temporal$workflow,
                 data = p4_train_RF_dynamic_temporal$best_fit$splits[[1]]$data %>%
                   select(-mean_value) %>%
                   as.data.frame(),
                 ncores = min(maxcores, SHAP_cores),
                 nsim = SHAP_nsim)
    }
  ),
  
  # Refresh AWS credentials
  tar_target(
    p4_aws_credentials_13,
    generate_credentials(dummy_var = p4_shap_dynamic_temporal),
    deployment = 'main',
    cue = tar_cue('always')
  ),
  
  
  #Global shap importance
  tar_target(
    p4_shap_importance_static_png,
    plot_shap_global(shap = p4_shap_static,
                     model_name = 'RF_static_full',
                     out_dir = "4_predict/out/random/shap/RF_static",
                     num_features = 40),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_importance_min_static_png,
    plot_shap_global(shap = p4_shap_min_static,
                    model_name = 'RF_min_static_full',
                    out_dir = "4_predict/out/random/shap/RF_min_static",
                    num_features = 40),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_importance_static_dynamic_png,
    plot_shap_global(shap = p4_shap_static_dynamic,
                    model_name = 'RF_static_dynamic_full',
                    out_dir = "4_predict/out/random/shap/RF_static_dynamic",
                    num_features = 40),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_importance_min_static_dynamic_png,
    plot_shap_global(shap = p4_shap_min_static_dynamic,
                    model_name = 'RF_min_static_dynamic_full',
                    out_dir = "4_predict/out/random/shap/RF_min_static_dynamic",
                    num_features = 40),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_importance_dynamic_png,
    plot_shap_global(shap = p4_shap_dynamic,
                    model_name = 'RF_dynamic_full',
                    out_dir = "4_predict/out/random/shap/RF_dynamic",
                    num_features = 40),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_importance_static_dynamic_temporal_png,
    plot_shap_global(shap = p4_shap_static_dynamic_temporal,
                    model_name = 'RF_static_dynamic_temporal_full',
                    out_dir = "4_predict/out/temporal/shap/RF_static_dynamic",
                    num_features = 40),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_importance_min_static_dynamic_temporal_png,
    plot_shap_global(shap = p4_shap_min_static_dynamic_temporal,
                    model_name = 'RF_min_static_dynamic_temporal_full',
                    out_dir = "4_predict/out/temporal/shap/RF_min_static_dynamic",
                    num_features = 40),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_importance_dynamic_temporal_png,
    plot_shap_global(shap = p4_shap_dynamic_temporal,
                    model_name = 'RF_dynamic_temporal_full',
                    out_dir = "4_predict/out/temporal/shap/RF_dynamic",
                    num_features = 40),
    format = "file",
    repository = 'local'
  ),
  
  
  #shap dependence plots
  tar_target(
    p4_shap_dependence_static_png,
    {
      #Correct workflow problem
      data <- p4_train_RF_static$best_fit$splits[[1]]$data %>%
        select(-mean_value) %>%
        as.data.frame()
      data$Date <- p4_Boruta_static$input_data$split$data$Date
      data$PRMS_segid <- p4_Boruta_static$input_data$split$data$PRMS_segid
      
      plot_shap_dependence(shap = p4_shap_static,
                           data = data,
                           model_name = 'RF_static_full',
                           out_dir = "4_predict/out/random/shap/RF_static",
                           ncores = SHAP_cores)
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_dependence_min_static_png,
    plot_shap_dependence(shap = p4_shap_min_static,
                         data = p4_train_RF_min_static$best_fit$splits[[1]]$data %>%
                           select(-mean_value) %>%
                           as.data.frame(),
                     model_name = 'RF_min_static_full',
                     out_dir = "4_predict/out/random/shap/RF_min_static",
                     ncores = SHAP_cores),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_dependence_static_dynamic_png,
    plot_shap_dependence(shap = p4_shap_static_dynamic,
                         data = p4_train_RF_static_dynamic$best_fit$splits[[1]]$data %>%
                           select(-mean_value) %>%
                           as.data.frame(),
                     model_name = 'RF_static_dynamic_full',
                     out_dir = "4_predict/out/random/shap/RF_static_dynamic",
                     ncores = SHAP_cores),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_dependence_min_static_dynamic_png,
    plot_shap_dependence(shap = p4_shap_min_static_dynamic,
                         data = p4_train_RF_min_static_dynamic$best_fit$splits[[1]]$data %>%
                           select(-mean_value) %>%
                           as.data.frame(),
                     model_name = 'RF_min_static_dynamic_full',
                     out_dir = "4_predict/out/random/shap/RF_min_static_dynamic",
                     ncores = SHAP_cores),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_dependence_dynamic_png,
    plot_shap_dependence(shap = p4_shap_dynamic,
                         data = p4_train_RF_dynamic$best_fit$splits[[1]]$data %>%
                           select(-mean_value) %>%
                           as.data.frame(),
                     model_name = 'RF_dynamic_full',
                     out_dir = "4_predict/out/random/shap/RF_dynamic",
                     ncores = SHAP_cores),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_dependence_static_dynamic_temporal_png,
    plot_shap_dependence(shap = p4_shap_static_dynamic_temporal,
                         data = p4_train_RF_static_dynamic_temporal$best_fit$splits[[1]]$data %>%
                           select(-mean_value) %>%
                           as.data.frame(),
                     model_name = 'RF_static_dynamic_temporal_full',
                     out_dir = "4_predict/out/temporal/shap/RF_static_dynamic",
                     ncores = SHAP_cores),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_dependence_min_static_dynamic_temporal_png,
    plot_shap_dependence(shap = p4_shap_min_static_dynamic_temporal,
                         data = p4_train_RF_min_static_dynamic_temporal$best_fit$splits[[1]]$data %>%
                           select(-mean_value) %>%
                           as.data.frame(),
                     model_name = 'RF_min_static_dynamic_temporal_full',
                     out_dir = "4_predict/out/temporal/shap/RF_min_static_dynamic",
                     ncores = SHAP_cores),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_dependence_dynamic_temporal_png,
    plot_shap_dependence(shap = p4_shap_dynamic_temporal,
                         data = p4_train_RF_dynamic_temporal$best_fit$splits[[1]]$data %>%
                           select(-mean_value) %>%
                           as.data.frame(),
                     model_name = 'RF_dynamic_temporal_full',
                     out_dir = "4_predict/out/temporal/shap/RF_dynamic",
                     ncores = SHAP_cores),
    format = "file",
    repository = 'local'
  )
  
  
  #PDP and ICE plots - not ready yet
  # tar_target(
  #   p4_pdp_static_png,
  #   plot_pdp(shap = p4_shap_static,
  #                        data = p4_train_RF_static$best_fit$splits[[1]]$data %>% 
  #                          select(-mean_value) %>% 
  #                          as.data.frame(),
  #                        model_name = 'RF_static_full',
  #                        out_dir = "4_predict/out/random/dependence/RF_static"),
  #   format = "file",
  #   repository = 'local'
  # ),
  # tar_target(
  #   p4_pdp_min_static_png,
  #   plot_pdp(shap = p4_shap_min_static,
  #                        data = p4_train_RF_min_static$best_fit$splits[[1]]$data %>% 
  #                          select(-mean_value) %>% 
  #                          as.data.frame(),
  #                        model_name = 'RF_min_static_full',
  #                        out_dir = "4_predict/out/random/dependence/RF_min_static"),
  #   format = "file",
  #   repository = 'local'
  # ),
  # tar_target(
  #   p4_pdp_static_dynamic_png,
  #   plot_pdp(shap = p4_shap_static_dynamic,
  #                        data = p4_train_RF_static_dynamic$best_fit$splits[[1]]$data %>% 
  #                          select(-mean_value) %>% 
  #                          as.data.frame(),
  #                        model_name = 'RF_static_dynamic_full',
  #                        out_dir = "4_predict/out/random/dependence/RF_static_dynamic"),
  #   format = "file",
  #   repository = 'local'
  # ),
  # tar_target(
  #   p4_pdp_min_static_dynamic_png,
  #   plot_pdp(shap = p4_shap_min_static_dynamic,
  #                        data = p4_train_RF_min_static_dynamic$best_fit$splits[[1]]$data %>% 
  #                          select(-mean_value) %>% 
  #                          as.data.frame(),
  #                        model_name = 'RF_min_static_dynamic_full',
  #                        out_dir = "4_predict/out/random/dependence/RF_min_static_dynamic"),
  #   format = "file",
  #   repository = 'local'
  # ),
  # tar_target(
  #   p4_pdp_dynamic_png,
  #   plot_pdp(shap = p4_shap_dynamic,
  #                        data = p4_train_RF_dynamic$best_fit$splits[[1]]$data %>% 
  #                          select(-mean_value) %>% 
  #                          as.data.frame(),
  #                        model_name = 'RF_dynamic_full',
  #                        out_dir = "4_predict/out/random/dependence/RF_dynamic"),
  #   format = "file",
  #   repository = 'local'
  # ),
  # tar_target(
  #   p4_pdp_static_dynamic_temporal_png,
  #   plot_pdp(shap = p4_shap_static_dynamic_temporal,
  #                        data = p4_train_RF_static_dynamic_temporal$best_fit$splits[[1]]$data %>% 
  #                          select(-mean_value) %>% 
  #                          as.data.frame(),
  #                        model_name = 'RF_static_dynamic_temporal_full',
  #                        out_dir = "4_predict/out/temporal/dependence/RF_static_dynamic"),
  #   format = "file",
  #   repository = 'local'
  # ),
  # tar_target(
  #   p4_pdp_min_static_dynamic_temporal_png,
  #   plot_pdp(shap = p4_shap_min_static_dynamic_temporal,
  #                        data = p4_train_RF_min_static_dynamic_temporal$best_fit$splits[[1]]$data %>% 
  #                          select(-mean_value) %>% 
  #                          as.data.frame(),
  #                        model_name = 'RF_min_static_dynamic_temporal_full',
  #                        out_dir = "4_predict/out/temporal/dependence/RF_min_static_dynamic"),
  #   format = "file",
  #   repository = 'local'
  # ),
  # tar_target(
  #   p4_pdp_dynamic_temporal_png,
  #   plot_pdp(shap = p4_shap_dynamic_temporal,
  #                        data = p4_train_RF_dynamic_temporal$best_fit$splits[[1]]$data %>% 
  #                          select(-mean_value) %>% 
  #                          as.data.frame(),
  #                        model_name = 'RF_dynamic_temporal_full',
  #                        out_dir = "4_predict/out/temporal/dependence/RF_dynamic"),
  #   format = "file",
  #   repository = 'local'
  # )
)