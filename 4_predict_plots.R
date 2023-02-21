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
  tar_target(p4_vip_static_dynamic_spatial_png,
             plot_vip(RF_model = p4_train_RF_static_dynamic_spatial$best_fit,
                      model_name = 'daily_SC_RF_static_dynamic_spatial',
                      num_features = 20,
                      out_dir = '4_predict/out/spatial/vip'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_vip_min_static_dynamic_spatial_png,
             plot_vip(RF_model = p4_train_RF_min_static_dynamic_spatial$best_fit,
                      model_name = 'daily_SC_RF_min_static_dynamic_spatial',
                      num_features = 20,
                      out_dir = '4_predict/out/spatial/vip'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_vip_dynamic_spatial_png,
             plot_vip(RF_model = p4_train_RF_dynamic_spatial$best_fit,
                      model_name = 'daily_SC_RF_dynamic_spatial',
                      num_features = 20,
                      out_dir = '4_predict/out/spatial/vip'),
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
  tar_target(p4_hypopt_static_dynamic_spatial_png,
             plot_hyperparam_opt_results_RF(p4_train_RF_static_dynamic_spatial$grid_params,
                                            model_name = 'daily_SC_RF_static_dynamic_spatial',
                                            out_dir = '4_predict/out/spatial/hypopt'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_hypopt_marginals_static_dynamic_spatial_png,
             plot_hyperparam_opt_marginals(p4_train_RF_static_dynamic_spatial$grid_params,
                                           model_name = 'daily_SC_RF_static_dynamic_spatial',
                                           plt_type = "marginals",
                                           perf_metric = NULL,
                                           out_dir = '4_predict/out/spatial/hypopt'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_hypopt_min_static_dynamic_spatial_png,
             plot_hyperparam_opt_results_RF(p4_train_RF_min_static_dynamic_spatial$grid_params,
                                            model_name = 'daily_SC_RF_min_static_dynamic_spatial',
                                            out_dir = '4_predict/out/spatial/hypopt'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_hypopt_marginals_min_static_dynamic_spatial_png,
             plot_hyperparam_opt_marginals(p4_train_RF_min_static_dynamic_spatial$grid_params,
                                           model_name = 'daily_SC_RF_min_static_dynamic_spatial',
                                           plt_type = "marginals",
                                           perf_metric = NULL,
                                           out_dir = '4_predict/out/spatial/hypopt'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_hypopt_dynamic_spatial_png,
             plot_hyperparam_opt_results_RF(p4_train_RF_dynamic_spatial$grid_params,
                                            model_name = 'daily_SC_RF_dynamic_spatial',
                                            out_dir = '4_predict/out/spatial/hypopt'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_hypopt_marginals_dynamic_spatial_png,
             plot_hyperparam_opt_marginals(p4_train_RF_dynamic_spatial$grid_params,
                                           model_name = 'daily_SC_RF_dynamic_spatial',
                                           plt_type = "marginals",
                                           perf_metric = NULL,
                                           out_dir = '4_predict/out/spatial/hypopt'),
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
  tar_target(p4_pred_obs_static_dynamic_spatial_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_static_dynamic_spatial$pred,
                           model_name = 'daily_SC_RF_static_dynamic_spatial',
                           out_dir = '4_predict/out/spatial/pred_obs/RF_static_dynamic', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_static_dynamic_spatial_test_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_static_dynamic_spatial_test$pred,
                           model_name = 'daily_SC_RF_static_dynamic_spatial_test',
                           out_dir = '4_predict/out/spatial/pred_obs/RF_static_dynamic', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_min_static_dynamic_spatial_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_min_static_dynamic_spatial$pred,
                           model_name = 'daily_SC_RF_min_static_dynamic_spatial',
                           out_dir = '4_predict/out/spatial/pred_obs/RF_min_static_dynamic', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_min_static_dynamic_spatial_test_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_min_static_dynamic_spatial_test$pred,
                           model_name = 'daily_SC_RF_min_static_dynamic_spatial_test',
                           out_dir = '4_predict/out/spatial/pred_obs/RF_min_static_dynamic', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_dynamic_spatial_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_dynamic_spatial$pred,
                           model_name = 'daily_SC_RF_dynamic_spatial',
                           out_dir = '4_predict/out/spatial/pred_obs/RF_dynamic', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_dynamic_spatial_test_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_dynamic_spatial_test$pred,
                           model_name = 'daily_SC_RF_dynamic_spatial_test',
                           out_dir = '4_predict/out/spatial/pred_obs/RF_dynamic', 
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
  tar_target(p4_train_test_boxplot_coverage_spatial_png,
             plot_metric_boxplot(data_split = p4_selected_static_dynamic_attrs_spatial$input_data,
                                 model_name = 'daily_SC_RF',
                                 pred_var = 'mean_value',
                                 out_dir = '4_predict/out/spatial/pred_obs'),
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
  tar_target(p4_train_test_CV_static_dynamic_spatial_png,
             barplot_compare_RF(mod = p4_train_RF_static_dynamic_spatial,
                                model_name = 'daily_SC_RF_static_dynamic_spatial',
                                pred_var = 'mean_value',
                                perf_metric = 'rmse',
                                out_dir = '4_predict/out/spatial/pred_obs/RF_static_dynamic'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_train_test_CV_min_static_dynamic_spatial_png,
             barplot_compare_RF(mod = p4_train_RF_min_static_dynamic_spatial,
                                model_name = 'daily_SC_RF_min_static_dynamic_spatial',
                                pred_var = 'mean_value',
                                perf_metric = 'rmse',
                                out_dir = '4_predict/out/spatial/pred_obs/RF_min_static_dynamic'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_train_test_CV_dynamic_spatial_png,
             barplot_compare_RF(mod = p4_train_RF_dynamic_spatial,
                                model_name = 'daily_SC_RF_dynamic_spatial',
                                pred_var = 'mean_value',
                                perf_metric = 'rmse',
                                out_dir = '4_predict/out/spatial/pred_obs/RF_dynamic'),
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
  tar_target(
    p4_spatial_res_static_dynamic_spatial_png,
    {
      #Average RMSE over all time for each PRMS segment
      PRMS_seg_RMSE <- summarize(group_by(p4_pred_RF_static_dynamic_spatial$pred, PRMS_segid), 
                                 RMSE = sqrt(mean(errsq)),
                                 RMSE_log10 = log10(RMSE))
      plot_nhdv2_attr(attr_data = PRMS_seg_RMSE,
                      network_geometry = p1_reaches_sf,
                      file_path = "4_predict/out/spatial/spatial_res/RF_static_dynamic",
                      filename_end = '_full')
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_spatial_res_static_dynamic_spatial_test_png,
    {
      #Average RMSE over all time for each PRMS segment
      PRMS_seg_RMSE <- summarize(group_by(p4_pred_RF_static_dynamic_spatial_test$pred, PRMS_segid), 
                                 RMSE = sqrt(mean(errsq)),
                                 RMSE_log10 = log10(RMSE))
      plot_nhdv2_attr(attr_data = PRMS_seg_RMSE,
                      network_geometry = p1_reaches_sf,
                      file_path = "4_predict/out/spatial/spatial_res/RF_static_dynamic",
                      filename_end = '_test')
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_spatial_res_min_static_dynamic_spatial_png,
    {
      #Average RMSE over all time for each PRMS segment
      PRMS_seg_RMSE <- summarize(group_by(p4_pred_RF_min_static_dynamic_spatial$pred, PRMS_segid), 
                                 RMSE = sqrt(mean(errsq)),
                                 RMSE_log10 = log10(RMSE))
      plot_nhdv2_attr(attr_data = PRMS_seg_RMSE,
                      network_geometry = p1_reaches_sf,
                      file_path = "4_predict/out/spatial/spatial_res/RF_min_static_dynamic",
                      filename_end = '_full')
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_spatial_res_min_static_dynamic_spatial_test_png,
    {
      #Average RMSE over all time for each PRMS segment
      PRMS_seg_RMSE <- summarize(group_by(p4_pred_RF_min_static_dynamic_spatial_test$pred, PRMS_segid), 
                                 RMSE = sqrt(mean(errsq)),
                                 RMSE_log10 = log10(RMSE))
      plot_nhdv2_attr(attr_data = PRMS_seg_RMSE,
                      network_geometry = p1_reaches_sf,
                      file_path = "4_predict/out/spatial/spatial_res/RF_min_static_dynamic",
                      filename_end = '_test')
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_spatial_res_dynamic_spatial_png,
    {
      #Average RMSE over all time for each PRMS segment
      PRMS_seg_RMSE <- summarize(group_by(p4_pred_RF_dynamic_spatial$pred, PRMS_segid), 
                                 RMSE = sqrt(mean(errsq)),
                                 RMSE_log10 = log10(RMSE))
      plot_nhdv2_attr(attr_data = PRMS_seg_RMSE,
                      network_geometry = p1_reaches_sf,
                      file_path = "4_predict/out/spatial/spatial_res/RF_dynamic",
                      filename_end = '_full')
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_spatial_res_dynamic_spatial_test_png,
    {
      #Average RMSE over all time for each PRMS segment
      PRMS_seg_RMSE <- summarize(group_by(p4_pred_RF_dynamic_spatial_test$pred, PRMS_segid), 
                                 RMSE = sqrt(mean(errsq)),
                                 RMSE_log10 = log10(RMSE))
      plot_nhdv2_attr(attr_data = PRMS_seg_RMSE,
                      network_geometry = p1_reaches_sf,
                      file_path = "4_predict/out/spatial/spatial_res/RF_dynamic",
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
  tar_target(
    p4_monthly_res_static_dynamic_spatial_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_month <- summarize(group_by(p4_pred_RF_static_dynamic_spatial$pred, Month),
                              RMSE = sqrt(mean(errsq)),
                              Bias = mean(err)) %>%
        arrange(Month)
      plot_barplot(attr_data = PRMS_month,
                   file_path = "4_predict/out/spatial/monthly_res/RF_static_dynamic",
                   model_name = 'RF_static_dynamic_spatial_full', 
                   panel = TRUE,
                   plot_month_names = TRUE)
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_monthly_res_static_dynamic_spatial_test_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_month <- summarize(group_by(p4_pred_RF_static_dynamic_spatial_test$pred, Month),
                              RMSE = sqrt(mean(errsq)),
                              Bias = mean(err)) %>%
        arrange(Month)
      plot_barplot(attr_data = PRMS_month,
                   file_path = "4_predict/out/spatial/monthly_res/RF_static_dynamic",
                   model_name = 'RF_static_dynamic_spatial_test', 
                   panel = TRUE,
                   plot_month_names = TRUE)
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_monthly_res_min_static_dynamic_spatial_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_month <- summarize(group_by(p4_pred_RF_min_static_dynamic_spatial$pred, Month),
                              RMSE = sqrt(mean(errsq)),
                              Bias = mean(err)) %>%
        arrange(Month)
      plot_barplot(attr_data = PRMS_month,
                   file_path = "4_predict/out/spatial/monthly_res/RF_min_static_dynamic",
                   model_name = 'RF_min_static_dynamic_spatial_full', 
                   panel = TRUE,
                   plot_month_names = TRUE)
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_monthly_res_min_static_dynamic_spatial_test_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_month <- summarize(group_by(p4_pred_RF_min_static_dynamic_spatial_test$pred, Month),
                              RMSE = sqrt(mean(errsq)),
                              Bias = mean(err)) %>%
        arrange(Month)
      plot_barplot(attr_data = PRMS_month,
                   file_path = "4_predict/out/spatial/monthly_res/RF_min_static_dynamic",
                   model_name = 'RF_min_static_dynamic_spatial_test', 
                   panel = TRUE,
                   plot_month_names = TRUE)
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_monthly_res_dynamic_spatial_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_month <- summarize(group_by(p4_pred_RF_dynamic_spatial$pred, Month),
                              RMSE = sqrt(mean(errsq)),
                              Bias = mean(err)) %>%
        arrange(Month)
      plot_barplot(attr_data = PRMS_month,
                   file_path = "4_predict/out/spatial/monthly_res/RF_dynamic",
                   model_name = 'RF_dynamic_spatial_full', 
                   panel = TRUE,
                   plot_month_names = TRUE)
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_monthly_res_dynamic_spatial_test_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_month <- summarize(group_by(p4_pred_RF_dynamic_spatial_test$pred, Month),
                              RMSE = sqrt(mean(errsq)),
                              Bias = mean(err)) %>%
        arrange(Month)
      plot_barplot(attr_data = PRMS_month,
                   file_path = "4_predict/out/spatial/monthly_res/RF_dynamic",
                   model_name = 'RF_dynamic_spatial_test', 
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
  tar_target(
    p4_annual_res_static_dynamic_spatial_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_ann <- summarize(group_by(p4_pred_RF_static_dynamic_spatial$pred, Year),
                            RMSE = sqrt(mean(errsq)),
                            Bias = mean(err)) %>%
        arrange(Year)
      plot_barplot(attr_data = PRMS_ann,
                   file_path = "4_predict/out/spatial/annual_res/RF_static_dynamic",
                   model_name = 'RF_static_dynamic_spatial_full',
                   panel = TRUE,
                   label_sequence = seq(1,length(unique(PRMS_ann$Year)),3))
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_annual_res_static_dynamic_spatial_test_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_ann <- summarize(group_by(p4_pred_RF_static_dynamic_spatial_test$pred, Year),
                            RMSE = sqrt(mean(errsq)),
                            Bias = mean(err)) %>%
        arrange(Year)
      plot_barplot(attr_data = PRMS_ann,
                   file_path = "4_predict/out/spatial/annual_res/RF_static_dynamic",
                   model_name = 'RF_static_dynamic_spatial_test',
                   panel = TRUE,
                   label_sequence = seq(1,length(unique(PRMS_ann$Year)),3))
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_annual_res_min_static_dynamic_spatial_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_ann <- summarize(group_by(p4_pred_RF_min_static_dynamic_spatial$pred, Year),
                            RMSE = sqrt(mean(errsq)),
                            Bias = mean(err)) %>%
        arrange(Year)
      plot_barplot(attr_data = PRMS_ann,
                   file_path = "4_predict/out/spatial/annual_res/RF_min_static_dynamic",
                   model_name = 'RF_min_static_dynamic_spatial_full',
                   panel = TRUE,
                   label_sequence = seq(1,length(unique(PRMS_ann$Year)),3))
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_annual_res_min_static_dynamic_spatial_test_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_ann <- summarize(group_by(p4_pred_RF_min_static_dynamic_spatial_test$pred, Year),
                            RMSE = sqrt(mean(errsq)),
                            Bias = mean(err)) %>%
        arrange(Year)
      plot_barplot(attr_data = PRMS_ann,
                   file_path = "4_predict/out/spatial/annual_res/RF_min_static_dynamic",
                   model_name = 'RF_min_static_dynamic_spatial_test',
                   panel = TRUE,
                   label_sequence = seq(1,length(unique(PRMS_ann$Year)),3))
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_annual_res_dynamic_spatial_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_ann <- summarize(group_by(p4_pred_RF_dynamic_spatial$pred, Year),
                            RMSE = sqrt(mean(errsq)),
                            Bias = mean(err)) %>%
        arrange(Year)
      plot_barplot(attr_data = PRMS_ann,
                   file_path = "4_predict/out/spatial/annual_res/RF_dynamic",
                   model_name = 'RF_dynamic_spatial_full',
                   panel = TRUE,
                   label_sequence = seq(1,length(unique(PRMS_ann$Year)),3))
    },
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_annual_res_dynamic_spatial_test_png,
    {
      #Average RMSE over all days of month and all space
      PRMS_ann <- summarize(group_by(p4_pred_RF_dynamic_spatial_test$pred, Year),
                            RMSE = sqrt(mean(errsq)),
                            Bias = mean(err)) %>%
        arrange(Year)
      plot_barplot(attr_data = PRMS_ann,
                   file_path = "4_predict/out/spatial/annual_res/RF_dynamic",
                   model_name = 'RF_dynamic_spatial_test',
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
  tar_target(
    p4_temporal_res_static_dynamic_spatial_png,
    plot_timeseries(pred_df = p4_pred_RF_static_dynamic_spatial$pred,
                    network_geometry = p1_reaches_sf,
                    model_name = 'RF_static_dynamic_spatial_full',
                    out_dir = "4_predict/out/spatial/temporal_res/RF_static_dynamic"),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_temporal_res_static_dynamic_spatial_test_png,
    plot_timeseries(pred_df = p4_pred_RF_static_dynamic_spatial_test$pred,
                    network_geometry = p1_reaches_sf,
                    model_name = 'RF_static_dynamic_spatial_test',
                    out_dir = "4_predict/out/spatial/temporal_res/RF_static_dynamic"),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_temporal_res_min_static_dynamic_spatial_png,
    plot_timeseries(pred_df = p4_pred_RF_min_static_dynamic_spatial$pred,
                    network_geometry = p1_reaches_sf,
                    model_name = 'RF_min_static_dynamic_spatial_full',
                    out_dir = "4_predict/out/spatial/temporal_res/RF_min_static_dynamic"),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_temporal_res_min_static_dynamic_spatial_test_png,
    plot_timeseries(pred_df = p4_pred_RF_min_static_dynamic_spatial_test$pred,
                    network_geometry = p1_reaches_sf,
                    model_name = 'RF_min_static_dynamic_spatial_test',
                    out_dir = "4_predict/out/spatial/temporal_res/RF_min_static_dynamic"),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_temporal_res_dynamic_spatial_png,
    plot_timeseries(pred_df = p4_pred_RF_dynamic_spatial$pred,
                    network_geometry = p1_reaches_sf,
                    model_name = 'RF_dynamic_spatial_full',
                    out_dir = "4_predict/out/spatial/temporal_res/RF_dynamic"),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_temporal_res_dynamic_spatial_test_png,
    plot_timeseries(pred_df = p4_pred_RF_dynamic_spatial_test$pred,
                    network_geometry = p1_reaches_sf,
                    model_name = 'RF_dynamic_spatial_test',
                    out_dir = "4_predict/out/spatial/temporal_res/RF_dynamic"),
    format = "file",
    repository = 'local'
  ),


  #SHAP values and plots
  tar_target(
    p4_shap_data_splits,
    #models may use a different data subset based on the attributes selected.
    #this sets up the full dataset by the season, lulc, and physio region
    setup_shap_data(data = p2_TOT_lc_physio_attrs %>% 
                      select(PRMS_segid, Date),
                    split_by_season = TRUE,
                    split_by_physio = TRUE,
                    split_by_lulc = TRUE,
                    lulc_prop = 0.75,
                    lulc_data = p2_TOT_lc_physio_attrs %>% 
                      select(PRMS_segid, Date, lowurban, midurban, forest),
                    physio_data = p2_TOT_lc_physio_attrs %>% 
                      select(PRMS_segid, Date, AP, BR, VR, CP, PD)
                    )
  ),
  
  #computation is RAM-limited, so determine number of cores based on available RAM.
  tar_target(
    p4_shap_static,
    {
      maxcores <- get_maxcores_by_RAM(SHAP_RAM, RAM_avail = RAM_set)
      
      #sample random subset to reduce computation and RAM demand
      sample_inds <- sample(x = seq(1, nrow(p4_train_RF_static$best_fit$splits[[1]]$data), 1), 
                            size = nrow(p4_train_RF_static$best_fit$splits[[1]]$data)*0.25, 
                            replace = FALSE)
      
      shap <- compute_shap(model = p4_train_RF_static$workflow,
                   data = p4_train_RF_static$best_fit$splits[[1]]$data[sample_inds,] %>%
                     select(-mean_value) %>%
                     as.data.frame(),
                   ncores = min(maxcores, SHAP_cores),
                   nsim = SHAP_nsim) 
      #add PRMS_segid, Date, and data_type columns. Cannot use tidy methods because shap has a strange class
      shap$PRMS_segid <- p4_train_RF_static$best_fit$splits[[1]]$data[sample_inds,]$PRMS_segid
      shap$Date <- p4_train_RF_static$best_fit$splits[[1]]$data[sample_inds,]$Date
      shap$data_type <- p4_train_RF_static$best_fit$splits[[1]]$data[sample_inds,]$data_type
      shap
    }
  ),
  tar_target(
    p4_shap_min_static,
    {
      maxcores <- get_maxcores_by_RAM(SHAP_RAM, RAM_avail = RAM_set)
      
      #sample random subset to reduce computation and RAM demand
      sample_inds <- sample(x = seq(1, nrow(p4_train_RF_min_static$best_fit$splits[[1]]$data), 1), 
                            size = nrow(p4_train_RF_min_static$best_fit$splits[[1]]$data)*0.25, 
                            replace = FALSE)
      
      shap <- compute_shap(model = p4_train_RF_min_static$workflow,
                   data = p4_train_RF_min_static$best_fit$splits[[1]]$data[sample_inds,] %>%
                     select(-mean_value) %>%
                     as.data.frame(),
                   ncores = min(maxcores, SHAP_cores),
                   nsim = SHAP_nsim) 
      #add PRMS_segid, Date, and data_type columns. Cannot use tidy methods because shap has a strange class
      shap$PRMS_segid <- p4_train_RF_min_static$best_fit$splits[[1]]$data[sample_inds,]$PRMS_segid
      shap$Date <- p4_train_RF_min_static$best_fit$splits[[1]]$data[sample_inds,]$Date
      shap$data_type <- p4_train_RF_min_static$best_fit$splits[[1]]$data[sample_inds,]$data_type
      shap
    }
  ),

  # Refresh AWS credentials
  tar_target(
    p4_aws_credentials_10,
    generate_credentials(dummy_var = p4_shap_min_static),
    deployment = 'main',
    cue = tar_cue('always')
  ),
  
  #Doing this calculation differently - using full dataset, manually specifying cores
  tar_target(
    p4_shap_static_dynamic,
    {
      #change the number of threads to 1 for this calculation
      model <- p4_train_RF_static_dynamic$workflow
      model$fit$fit$spec$eng_args$num.threads <- set_args(model$fit$fit$spec, num.threads = 1)$eng_args$num.threads
      model$fit$actions$model$spec$eng_args$num.threads <- set_args(model$fit$actions$model$spec, num.threads = 1)$eng_args$num.threads
      
      shap <- compute_shap(model = model,
                   data = p4_train_RF_static_dynamic$best_fit$splits[[1]]$data %>%
                     select(-mean_value, -PRMS_segid, -Date, -data_type) %>%
                     as.data.frame(),
                   ncores = min(7, SHAP_cores),
                   nsim = SHAP_nsim) 
      #add PRMS_segid, Date, and data_type columns. Cannot use tidy methods because shap has a strange class
      shap$PRMS_segid <- p4_train_RF_static_dynamic$best_fit$splits[[1]]$data$PRMS_segid
      shap$Date <- p4_train_RF_static_dynamic$best_fit$splits[[1]]$data$Date
      shap$data_type <- p4_train_RF_static_dynamic$best_fit$splits[[1]]$data$data_type
      shap
    }
  ),
  
  # Refresh AWS credentials
  tar_target(
    p4_aws_credentials_11,
    generate_credentials(dummy_var = p4_shap_static_dynamic),
    deployment = 'main',
    cue = tar_cue('always')
  ),
  
  tar_target(
    p4_shap_min_static_dynamic,
    {
      maxcores <- get_maxcores_by_RAM(SHAP_RAM, RAM_avail = RAM_set)
      
      #change the number of threads to 1 for this calculation
      model <- p4_train_RF_min_static_dynamic$workflow
      model$fit$fit$spec$eng_args$num.threads <- set_args(model$fit$fit$spec, num.threads = 1)$eng_args$num.threads
      model$fit$actions$model$spec$eng_args$num.threads <- set_args(model$fit$actions$model$spec, num.threads = 1)$eng_args$num.threads
      
      #sample random subset to reduce computation and RAM demand
      sample_inds <- sample(x = seq(1, nrow(p4_train_RF_min_static_dynamic$best_fit$splits[[1]]$data), 1), 
                            size = nrow(p4_train_RF_min_static_dynamic$best_fit$splits[[1]]$data)*0.25, 
                            replace = FALSE)
      
      shap <- compute_shap(model = model,
                   data = p4_train_RF_min_static_dynamic$best_fit$splits[[1]]$data[sample_inds,] %>%
                     select(-mean_value) %>%
                     as.data.frame(),
                   ncores = min(maxcores, SHAP_cores),
                   nsim = SHAP_nsim) 
      #add PRMS_segid, Date, and data_type columns. Cannot use tidy methods because shap has a strange class
      shap$PRMS_segid <- p4_train_RF_min_static_dynamic$best_fit$splits[[1]]$data[sample_inds,]$PRMS_segid
      shap$Date <- p4_train_RF_min_static_dynamic$best_fit$splits[[1]]$data[sample_inds,]$Date
      shap$data_type <- p4_train_RF_min_static_dynamic$best_fit$splits[[1]]$data[sample_inds,]$data_type
      shap
    }
  ),
  tar_target(
    p4_shap_dynamic,
    {
      maxcores <- get_maxcores_by_RAM(SHAP_RAM, RAM_avail = RAM_set)
      
      #sample random subset to reduce computation and RAM demand
      sample_inds <- sample(x = seq(1, nrow(p4_train_RF_dynamic$best_fit$splits[[1]]$data), 1), 
                            size = nrow(p4_train_RF_dynamic$best_fit$splits[[1]]$data)*0.25, 
                            replace = FALSE)
      
      shap <- compute_shap(model = p4_train_RF_dynamic$workflow,
                   data = p4_train_RF_dynamic$best_fit$splits[[1]]$data[sample_inds,] %>%
                     select(-mean_value) %>%
                     as.data.frame(),
                   ncores = min(maxcores, SHAP_cores),
                   nsim = SHAP_nsim) 
      #add PRMS_segid, Date, and data_type columns. Cannot use tidy methods because shap has a strange class
      shap$PRMS_segid <- p4_train_RF_dynamic$best_fit$splits[[1]]$data[sample_inds,]$PRMS_segid
      shap$Date <- p4_train_RF_dynamic$best_fit$splits[[1]]$data[sample_inds,]$Date
      shap$data_type <- p4_train_RF_dynamic$best_fit$splits[[1]]$data[sample_inds,]$data_type
      shap
    }
  ),
  
  # Refresh AWS credentials
  tar_target(
    p4_aws_credentials_11b,
    generate_credentials(dummy_var = p4_shap_dynamic),
    deployment = 'main',
    cue = tar_cue('always')
  ),
  
  #Doing this calculation differently - using full dataset, manually specifying cores
  tar_target(
    p4_shap_static_dynamic_temporal,
    {
      #change the number of threads to 1 for this calculation
      model <- p4_train_RF_static_dynamic_temporal$workflow
      model$fit$fit$spec$eng_args$num.threads <- set_args(model$fit$fit$spec, num.threads = 1)$eng_args$num.threads
      model$fit$actions$model$spec$eng_args$num.threads <- set_args(model$fit$actions$model$spec, num.threads = 1)$eng_args$num.threads
      
      shap <- compute_shap(model = model,
                   data = p4_train_RF_static_dynamic_temporal$best_fit$splits[[1]]$data %>%
                     select(-mean_value, -PRMS_segid, -Date, -data_type, -group) %>%
                     as.data.frame(),
                   ncores = min(7, SHAP_cores),
                   nsim = SHAP_nsim) 
      #add PRMS_segid, Date, and data_type columns. Cannot use tidy methods because shap has a strange class
      shap$PRMS_segid <- p4_train_RF_static_dynamic_temporal$best_fit$splits[[1]]$data$PRMS_segid
      shap$Date <- p4_train_RF_static_dynamic_temporal$best_fit$splits[[1]]$data$Date
      shap$data_type <- p4_train_RF_static_dynamic_temporal$best_fit$splits[[1]]$data$data_type
      shap
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
      
      #change the number of threads to 1 for this calculation
      model <- p4_train_RF_min_static_dynamic_temporal$workflow
      model$fit$fit$spec$eng_args$num.threads <- set_args(model$fit$fit$spec, num.threads = 1)$eng_args$num.threads
      model$fit$actions$model$spec$eng_args$num.threads <- set_args(model$fit$actions$model$spec, num.threads = 1)$eng_args$num.threads
      
      #sample random subset to reduce computation and RAM demand
      sample_inds <- sample(x = seq(1, nrow(p4_train_RF_min_static_dynamic_temporal$best_fit$splits[[1]]$data), 1), 
                            size = nrow(p4_train_RF_min_static_dynamic_temporal$best_fit$splits[[1]]$data)*0.25, 
                            replace = FALSE)
      
      shap <- compute_shap(model = model,
                   data = p4_train_RF_min_static_dynamic_temporal$best_fit$splits[[1]]$data[sample_inds,] %>%
                     select(-mean_value) %>%
                     as.data.frame(),
                   ncores = min(30, SHAP_cores),
                   nsim = SHAP_nsim) 
      #add PRMS_segid, Date, and data_type columns. Cannot use tidy methods because shap has a strange class
      shap$PRMS_segid <- p4_train_RF_min_static_dynamic_temporal$best_fit$splits[[1]]$data[sample_inds,]$PRMS_segid
      shap$Date <- p4_train_RF_min_static_dynamic_temporal$best_fit$splits[[1]]$data[sample_inds,]$Date
      shap$data_type <- p4_train_RF_min_static_dynamic_temporal$best_fit$splits[[1]]$data[sample_inds,]$data_type
      shap
    }
  ),
  tar_target(
    p4_shap_dynamic_temporal,
    {
      maxcores <- get_maxcores_by_RAM(SHAP_RAM, RAM_avail = RAM_set)
      
      #sample random subset to reduce computation and RAM demand
      sample_inds <- sample(x = seq(1, nrow(p4_train_RF_min_static_dynamic_temporal$best_fit$splits[[1]]$data), 1), 
                            size = nrow(p4_train_RF_min_static_dynamic_temporal$best_fit$splits[[1]]$data)*0.25, 
                            replace = FALSE)
      
      shap <- compute_shap(model = p4_train_RF_dynamic_temporal$workflow,
                   data = p4_train_RF_dynamic_temporal$best_fit$splits[[1]]$data[sample_inds,] %>%
                     select(-mean_value) %>%
                     as.data.frame(),
                   ncores = min(maxcores, SHAP_cores),
                   nsim = SHAP_nsim)
      #add PRMS_segid, Date, and data_type columns. Cannot use tidy methods because shap has a strange class
      shap$PRMS_segid <- p4_train_RF_dynamic_temporal$best_fit$splits[[1]]$data[sample_inds,]$PRMS_segid
      shap$Date <- p4_train_RF_dynamic_temporal$best_fit$splits[[1]]$data[sample_inds,]$Date
      shap$data_type <- p4_train_RF_dynamic_temporal$best_fit$splits[[1]]$data[sample_inds,]$data_type
      shap
    }
  ),

  # Refresh AWS credentials
  tar_target(
    p4_aws_credentials_13,
    generate_credentials(dummy_var = p4_shap_dynamic_temporal),
    deployment = 'main',
    cue = tar_cue('always')
  ),
   tar_target(
     p4_shap_static_dynamic_spatial,
     {
       maxcores <- get_maxcores_by_RAM(20, RAM_avail = RAM_set)
       
       #sample random subset to reduce computation and RAM demand
       sample_inds <- sample(x = seq(1, nrow(p4_train_RF_static_dynamic_spatial$best_fit$splits[[1]]$data), 1), 
                             size = nrow(p4_train_RF_static_dynamic_spatial$best_fit$splits[[1]]$data)*0.25, 
                             replace = FALSE)
       
       shap <- compute_shap(model = p4_train_RF_static_dynamic_spatial$workflow,
                            data = p4_train_RF_static_dynamic_spatial$best_fit$splits[[1]]$data[sample_inds,] %>%
                              select(-mean_value) %>%
                              as.data.frame(),
                            ncores = min(maxcores, SHAP_cores),
                            nsim = SHAP_nsim) 
       #add PRMS_segid, Date, and data_type columns. Cannot use tidy methods because shap has a strange class
       shap$PRMS_segid <- p4_train_RF_static_dynamic_spatial$best_fit$splits[[1]]$data[sample_inds,]$PRMS_segid
       shap$Date <- p4_train_RF_static_dynamic_spatial$best_fit$splits[[1]]$data[sample_inds,]$Date
       shap$data_type <- p4_train_RF_static_dynamic_spatial$best_fit$splits[[1]]$data[sample_inds,]$data_type
       shap
     }
   ),
  
   #Refresh AWS credentials
   tar_target(
     p4_aws_credentials_20,
     generate_credentials(dummy_var = p4_shap_static_dynamic_spatial),
     deployment = 'main',
     cue = tar_cue('always')
   ),
  
  #Doing this calculation differently - using full dataset, manually specifying cores
  tar_target(
    p4_shap_min_static_dynamic_spatial,
    {
      #change the number of threads to 1 for this calculation
      model <- p4_train_RF_min_static_dynamic_spatial$workflow
      model$fit$fit$spec$eng_args$num.threads <- set_args(model$fit$fit$spec, num.threads = 1)$eng_args$num.threads
      model$fit$actions$model$spec$eng_args$num.threads <- set_args(model$fit$actions$model$spec, num.threads = 1)$eng_args$num.threads
      
      shap <- compute_shap(model = model,
                           data = p4_train_RF_min_static_dynamic_spatial$best_fit$splits[[1]]$data %>%
                             select(-mean_value, -PRMS_segid, -Date, -data_type, group) %>%
                             as.data.frame(),
                           ncores = min(10, SHAP_cores),
                           nsim = SHAP_nsim) 
      #add PRMS_segid, Date, and data_type columns. Cannot use tidy methods because shap has a strange class
      shap$PRMS_segid <- p4_train_RF_min_static_dynamic_spatial$best_fit$splits[[1]]$data$PRMS_segid
      shap$Date <- p4_train_RF_min_static_dynamic_spatial$best_fit$splits[[1]]$data$Date
      shap$data_type <- p4_train_RF_min_static_dynamic_spatial$best_fit$splits[[1]]$data$data_type
      shap
    }
  ),
  
  # Refresh AWS credentials
  tar_target(
    p4_aws_credentials_20b,
    generate_credentials(dummy_var = p4_shap_min_static_dynamic_spatial),
    deployment = 'main',
    cue = tar_cue('always')
  ),
  
  tar_target(
    p4_shap_dynamic_spatial,
    {
      maxcores <- get_maxcores_by_RAM(10, RAM_avail = RAM_set)
      
      #sample random subset to reduce computation and RAM demand
      sample_inds <- sample(x = seq(1, nrow(p4_train_RF_min_static_dynamic_spatial$best_fit$splits[[1]]$data), 1), 
                            size = nrow(p4_train_RF_min_static_dynamic_spatial$best_fit$splits[[1]]$data)*0.25, 
                            replace = FALSE)
      
      shap <- compute_shap(model = p4_train_RF_dynamic_spatial$workflow,
                           data = p4_train_RF_dynamic_spatial$best_fit$splits[[1]]$data[sample_inds,] %>%
                             select(-mean_value) %>%
                             as.data.frame(),
                           ncores = min(maxcores, SHAP_cores),
                           nsim = SHAP_nsim)
      #add PRMS_segid, Date, and data_type columns. Cannot use tidy methods because shap has a strange class
      shap$PRMS_segid <- p4_train_RF_dynamic_spatial$best_fit$splits[[1]]$data[sample_inds,]$PRMS_segid
      shap$Date <- p4_train_RF_dynamic_spatial$best_fit$splits[[1]]$data[sample_inds,]$Date
      shap$data_type <- p4_train_RF_dynamic_spatial$best_fit$splits[[1]]$data[sample_inds,]$data_type
      shap
    }
  ),
  
  # Refresh AWS credentials
  tar_target(
    p4_aws_credentials_21,
    generate_credentials(dummy_var = p4_shap_dynamic_spatial),
    deployment = 'main',
    cue = tar_cue('always')
  ),
  
  #SHAP values for lulc, physio, season
  #These targets subset the SHAP results into the splits of p4_shap_<model>_subsets
  tar_target(
    p4_shap_static_dynamic_splits,
    get_shap_subset(split = p4_shap_data_splits[[1]], shap = p4_shap_static_dynamic,
                    split_name = names(p4_shap_data_splits)),
    pattern = map(p4_shap_data_splits),
    iteration = 'list'
  ),
  tar_target(
    p4_shap_static_dynamic_temporal_splits,
    get_shap_subset(split = p4_shap_data_splits[[1]], shap = p4_shap_static_dynamic_temporal,
                    split_name = names(p4_shap_data_splits)),
    pattern = map(p4_shap_data_splits),
    iteration = 'list'
  ),
  tar_target(
    p4_shap_min_static_dynamic_spatial_splits,
    get_shap_subset(split = p4_shap_data_splits[[1]], shap = p4_shap_min_static_dynamic_spatial,
                    split_name = names(p4_shap_data_splits)),
    pattern = map(p4_shap_data_splits),
    iteration = 'list'
  ),
  

  #Global shap importance
  tar_target(
    p4_shap_importance_static_png,
    plot_shap_global(shap = p4_shap_static[,
                    -which(colnames(p4_shap_static) %in% 
                             c('PRMS_segid', 'Date', 'data_type', 'group'))],
                     model_name = 'RF_static_full',
                     out_dir = "4_predict/out/random/shap/RF_static",
                     num_features = 40),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_importance_min_static_png,
    plot_shap_global(shap = p4_shap_min_static[,
                    -which(colnames(p4_shap_min_static) %in% 
                             c('PRMS_segid', 'Date', 'data_type', 'group'))],
                    model_name = 'RF_min_static_full',
                    out_dir = "4_predict/out/random/shap/RF_min_static",
                    num_features = 40),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_importance_static_dynamic_png,
    plot_shap_global(shap = p4_shap_static_dynamic[,
                    -which(colnames(p4_shap_static_dynamic) %in% 
                             c('PRMS_segid', 'Date', 'data_type', 'group'))],
                    model_name = 'RF_static_dynamic_full',
                    out_dir = "4_predict/out/random/shap/RF_static_dynamic",
                    num_features = 40),
    format = "file",
    repository = 'local'
  ),
  #mapped over data splits
  tar_target(
    p4_shap_importance_static_dynamic_subsets_png,
    plot_shap_global(shap = p4_shap_static_dynamic_splits$shap[,
                                                   -which(colnames(p4_shap_static_dynamic_splits$shap) %in% 
                                                            c('PRMS_segid', 'Date', 'data_type', 'group'))],
                     model_name = 'RF_static_dynamic_full',
                     out_dir = get_shap_dir(main_dir = "4_predict/out/random/shap/RF_static_dynamic",
                                            subdir_name = p4_shap_static_dynamic_splits$name),
                     num_features = 40),
    pattern = map(p4_shap_static_dynamic_splits),
    iteration = 'list',
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_importance_min_static_dynamic_png,
    plot_shap_global(shap = p4_shap_min_static_dynamic[,
                    -which(colnames(p4_shap_min_static_dynamic) %in% 
                             c('PRMS_segid', 'Date', 'data_type', 'group'))],
                    model_name = 'RF_min_static_dynamic_full',
                    out_dir = "4_predict/out/random/shap/RF_min_static_dynamic",
                    num_features = 40),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_importance_dynamic_png,
    plot_shap_global(shap = p4_shap_dynamic[,
                    -which(colnames(p4_shap_dynamic) %in%
                             c('PRMS_segid', 'Date', 'data_type', 'group'))],
                    model_name = 'RF_dynamic_full',
                    out_dir = "4_predict/out/random/shap/RF_dynamic",
                    num_features = 40),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_importance_static_dynamic_temporal_png,
    plot_shap_global(shap = p4_shap_static_dynamic_temporal[,
                    -which(colnames(p4_shap_static_dynamic_temporal) %in% 
                             c('PRMS_segid', 'Date', 'data_type', 'group'))],
                    model_name = 'RF_static_dynamic_temporal_full',
                    out_dir = "4_predict/out/temporal/shap/RF_static_dynamic",
                    num_features = 40),
    format = "file",
    repository = 'local'
  ),
  #mapped over data splits
  tar_target(
    p4_shap_importance_static_dynamic_temporal_subsets_png,
    plot_shap_global(shap = p4_shap_static_dynamic_temporal_splits$shap[,
                                                                    -which(colnames(p4_shap_static_dynamic_temporal_splits$shap) %in%
                                                                             c('PRMS_segid', 'Date', 'data_type', 'group'))],
                     model_name = 'RF_static_dynamic_temporal_full',
                     out_dir = get_shap_dir(main_dir = "4_predict/out/temporal/shap/RF_static_dynamic",
                                            subdir_name = p4_shap_static_dynamic_temporal_splits$name),
                     num_features = 40),
    pattern = map(p4_shap_static_dynamic_temporal_splits),
    iteration = 'list',
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_importance_min_static_dynamic_temporal_png,
    plot_shap_global(shap = p4_shap_min_static_dynamic_temporal[,
                    -which(colnames(p4_shap_min_static_dynamic_temporal) %in% 
                             c('PRMS_segid', 'Date', 'data_type', 'group'))],
                    model_name = 'RF_min_static_dynamic_temporal_full',
                    out_dir = "4_predict/out/temporal/shap/RF_min_static_dynamic",
                    num_features = 40),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_importance_dynamic_temporal_png,
    plot_shap_global(shap = p4_shap_dynamic_temporal[,
                    -which(colnames(p4_shap_dynamic_temporal) %in% 
                             c('PRMS_segid', 'Date', 'data_type', 'group'))],
                    model_name = 'RF_dynamic_temporal_full',
                    out_dir = "4_predict/out/temporal/shap/RF_dynamic",
                    num_features = 40),
    format = "file",
    repository = 'local'
  ),
   tar_target(
     p4_shap_importance_static_dynamic_spatial_png,
     plot_shap_global(shap = p4_shap_static_dynamic_spatial[,
                                                             -which(colnames(p4_shap_static_dynamic_spatial) %in% 
                                                                      c('PRMS_segid', 'Date', 'data_type', 'group'))],
                      model_name = 'RF_static_dynamic_spatial_full',
                      out_dir = "4_predict/out/spatial/shap/RF_static_dynamic",
                      num_features = 40),
     format = "file",
     repository = 'local'
   ),
  tar_target(
    p4_shap_importance_min_static_dynamic_spatial_png,
    plot_shap_global(shap = p4_shap_min_static_dynamic_spatial[,
                                                                -which(colnames(p4_shap_min_static_dynamic_spatial) %in% 
                                                                         c('PRMS_segid', 'Date', 'data_type', 'group'))],
                     model_name = 'RF_min_static_dynamic_spatial_full',
                     out_dir = "4_predict/out/spatial/shap/RF_min_static_dynamic",
                     num_features = 40),
    format = "file",
    repository = 'local'
  ),
  #mapped over data splits
  tar_target(
    p4_shap_importance_min_static_dynamic_spatial_subsets_png,
    plot_shap_global(shap = p4_shap_min_static_dynamic_spatial_splits$shap[,
                                                                    -which(colnames(p4_shap_min_static_dynamic_spatial_splits$shap) %in% 
                                                                             c('PRMS_segid', 'Date', 'data_type', 'group'))],
                     model_name = 'RF_min_static_dynamic_spatial_full',
                     out_dir = get_shap_dir(main_dir = "4_predict/out/spatial/shap/RF_min_static_dynamic",
                                            subdir_name = p4_shap_min_static_dynamic_spatial_splits$name),
                     num_features = 40),
    pattern = map(p4_shap_min_static_dynamic_spatial_splits),
    iteration = 'list',
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_importance_dynamic_spatial_png,
    plot_shap_global(shap = p4_shap_dynamic_spatial[,
                                                     -which(colnames(p4_shap_dynamic_spatial) %in% 
                                                              c('PRMS_segid', 'Date', 'data_type', 'group'))],
                     model_name = 'RF_dynamic_spatial_full',
                     out_dir = "4_predict/out/spatial/shap/RF_dynamic",
                     num_features = 40),
    format = "file",
    repository = 'local'
  ),


  #shap dependence plots
  tar_target(
    p4_shap_dependence_static_png,
    plot_shap_dependence(shap = p4_shap_static[,
                        -which(colnames(p4_shap_static) %in% 
                                 c('PRMS_segid', 'Date', 'data_type', 'group'))],
                         #This join is needed when a subset of the data are
                         #used to compute SHAP values
                         data = left_join(as.data.frame(p4_shap_static) %>% 
                                            select(PRMS_segid, Date), 
                                          p4_train_RF_static$best_fit$splits[[1]]$data %>% 
                                            select(-mean_value), 
                                          by = c('PRMS_segid', 'Date')) %>%
                           as.data.frame(),
                         model_name = 'RF_static_full',
                         out_dir = "4_predict/out/random/shap/RF_static",
                         ncores = SHAP_cores),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_dependence_min_static_png,
    plot_shap_dependence(shap = p4_shap_min_static[,
                        -which(colnames(p4_shap_min_static) %in% 
                                 c('PRMS_segid', 'Date', 'data_type', 'group'))],
                         data = left_join(as.data.frame(p4_shap_min_static) %>% 
                                            select(PRMS_segid, Date), 
                                          p4_train_RF_min_static$best_fit$splits[[1]]$data %>% 
                                            select(-mean_value), 
                                          by = c('PRMS_segid', 'Date')) %>%
                           as.data.frame(),
                     model_name = 'RF_min_static_full',
                     out_dir = "4_predict/out/random/shap/RF_min_static",
                     ncores = SHAP_cores),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_dependence_static_dynamic_png,
    plot_shap_dependence(shap = p4_shap_static_dynamic[,
                        -which(colnames(p4_shap_static_dynamic) %in% 
                                 c('PRMS_segid', 'Date', 'data_type', 'group'))],
                         data = left_join(as.data.frame(p4_shap_static_dynamic) %>% 
                                            select(PRMS_segid, Date), 
                                          p4_train_RF_static_dynamic$best_fit$splits[[1]]$data %>% 
                                            select(-mean_value), 
                                          by = c('PRMS_segid', 'Date')) %>%
                           as.data.frame(),
                     model_name = 'RF_static_dynamic_full',
                     out_dir = "4_predict/out/random/shap/RF_static_dynamic",
                     ncores = SHAP_cores),
    format = "file",
    repository = 'local'
  ),
  #mapped over data splits
  tar_target(
    p4_shap_dependence_static_dynamic_subsets_png,
    plot_shap_dependence(shap = p4_shap_static_dynamic_splits$shap[,
                                                                        -which(colnames(p4_shap_static_dynamic_splits$shap) %in% 
                                                                                 c('PRMS_segid', 'Date', 'data_type', 'group'))],
                         data = left_join(as.data.frame(p4_shap_static_dynamic_splits$shap) %>% 
                                            select(PRMS_segid, Date), 
                                          p4_train_RF_static_dynamic$best_fit$splits[[1]]$data %>% 
                                            select(-mean_value), 
                                          by = c('PRMS_segid', 'Date')) %>%
                           as.data.frame(),
                     model_name = 'RF_static_dynamic_full',
                     out_dir = get_shap_dir(main_dir = "4_predict/out/random/shap/RF_static_dynamic",
                                            subdir_name = p4_shap_static_dynamic_splits$name),
                     ncores = SHAP_cores),
    pattern = map(p4_shap_static_dynamic_splits),
    iteration = 'list',
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_dependence_min_static_dynamic_png,
    plot_shap_dependence(shap = p4_shap_min_static_dynamic[,
                        -which(colnames(p4_shap_min_static_dynamic) %in% 
                                 c('PRMS_segid', 'Date', 'data_type', 'group'))],
                         data = left_join(as.data.frame(p4_shap_min_static_dynamic) %>% 
                                            select(PRMS_segid, Date), 
                                          p4_train_RF_min_static_dynamic$best_fit$splits[[1]]$data %>% 
                                            select(-mean_value), 
                                          by = c('PRMS_segid', 'Date')) %>%
                           as.data.frame(),
                     model_name = 'RF_min_static_dynamic_full',
                     out_dir = "4_predict/out/random/shap/RF_min_static_dynamic",
                     ncores = SHAP_cores),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_dependence_dynamic_png,
    plot_shap_dependence(shap = p4_shap_dynamic[,
                        -which(colnames(p4_shap_dynamic) %in% 
                                 c('PRMS_segid', 'Date', 'data_type', 'group'))],
                         data = left_join(as.data.frame(p4_shap_dynamic) %>% 
                                            select(PRMS_segid, Date), 
                                          p4_train_RF_dynamic$best_fit$splits[[1]]$data %>% 
                                            select(-mean_value), 
                                          by = c('PRMS_segid', 'Date')) %>%
                           as.data.frame(),
                     model_name = 'RF_dynamic_full',
                     out_dir = "4_predict/out/random/shap/RF_dynamic",
                     ncores = SHAP_cores),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_dependence_static_dynamic_temporal_png,
    plot_shap_dependence(shap = p4_shap_static_dynamic_temporal[,
                        -which(colnames(p4_shap_static_dynamic_temporal) %in%
                                 c('PRMS_segid', 'Date', 'data_type', 'group'))],
                         data = left_join(as.data.frame(p4_shap_static_dynamic_temporal) %>% 
                                            select(PRMS_segid, Date), 
                                          p4_train_RF_static_dynamic_temporal$best_fit$splits[[1]]$data %>% 
                                            select(-mean_value), 
                                          by = c('PRMS_segid', 'Date')) %>%
                           as.data.frame(),
                     model_name = 'RF_static_dynamic_temporal_full',
                     out_dir = "4_predict/out/temporal/shap/RF_static_dynamic",
                     ncores = SHAP_cores),
    format = "file",
    repository = 'local'
  ),
  #mapped over data splits
  tar_target(
    p4_shap_dependence_static_dynamic_temporal_subsets_png,
    plot_shap_dependence(shap = p4_shap_static_dynamic_temporal_splits$shap[,
                                                                        -which(colnames(p4_shap_static_dynamic_temporal_splits$shap) %in% 
                                                                                 c('PRMS_segid', 'Date', 'data_type', 'group'))],
                         data = left_join(as.data.frame(p4_shap_static_dynamic_temporal_splits$shap) %>% 
                                            select(PRMS_segid, Date), 
                                          p4_train_RF_static_dynamic_temporal$best_fit$splits[[1]]$data %>% 
                                            select(-mean_value), 
                                          by = c('PRMS_segid', 'Date')) %>%
                           as.data.frame(),
                         model_name = 'RF_static_dynamic_temporal_full',
                         out_dir = get_shap_dir(main_dir = "4_predict/out/temporal/shap/RF_static_dynamic",
                                                subdir_name = p4_shap_static_dynamic_temporal_splits$name),
                         ncores = SHAP_cores),
    pattern = map(p4_shap_static_dynamic_temporal_splits),
    iteration = 'list',
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_dependence_min_static_dynamic_temporal_png,
    plot_shap_dependence(shap = p4_shap_min_static_dynamic_temporal[,
                        -which(colnames(p4_shap_min_static_dynamic_temporal) %in%
                                 c('PRMS_segid', 'Date', 'data_type', 'group'))],
                         data = left_join(as.data.frame(p4_shap_min_static_dynamic_temporal) %>% 
                                            select(PRMS_segid, Date), 
                                          p4_train_RF_min_static_dynamic_temporal$best_fit$splits[[1]]$data %>% 
                                            select(-mean_value), 
                                          by = c('PRMS_segid', 'Date')) %>%
                           as.data.frame(),
                     model_name = 'RF_min_static_dynamic_temporal_full',
                     out_dir = "4_predict/out/temporal/shap/RF_min_static_dynamic",
                     ncores = SHAP_cores),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_dependence_dynamic_temporal_png,
    plot_shap_dependence(shap = p4_shap_dynamic_temporal[,
                          -which(colnames(p4_shap_dynamic_temporal) %in% 
                            c('PRMS_segid', 'Date', 'data_type', 'group'))],
                         data = left_join(as.data.frame(p4_shap_dynamic_temporal) %>% 
                                            select(PRMS_segid, Date), 
                                          p4_train_RF_dynamic_temporal$best_fit$splits[[1]]$data %>% 
                                            select(-mean_value), 
                                          by = c('PRMS_segid', 'Date')) %>%
                           as.data.frame(),
                     model_name = 'RF_dynamic_temporal_full',
                     out_dir = "4_predict/out/temporal/shap/RF_dynamic",
                     ncores = SHAP_cores),
    format = "file",
    repository = 'local'
  ),
   tar_target(
     p4_shap_dependence_static_dynamic_spatial_png,
     plot_shap_dependence(shap = p4_shap_static_dynamic_spatial[,
                                                                 -which(colnames(p4_shap_static_dynamic_spatial) %in%
                                                                          c('PRMS_segid', 'Date', 'data_type', 'group'))],
                          data = left_join(as.data.frame(p4_shap_static_dynamic_spatial) %>% 
                                             select(PRMS_segid, Date), 
                                           p4_train_RF_static_dynamic_spatial$best_fit$splits[[1]]$data %>% 
                                             select(-mean_value), 
                                           by = c('PRMS_segid', 'Date')) %>%
                            as.data.frame(),
                          model_name = 'RF_static_dynamic_spatial_full',
                          out_dir = "4_predict/out/spatial/shap/RF_static_dynamic",
                          ncores = SHAP_cores),
     format = "file",
     repository = 'local'
   ),
  tar_target(
    p4_shap_dependence_min_static_dynamic_spatial_png,
    plot_shap_dependence(shap = p4_shap_min_static_dynamic_spatial[,
                                                                    -which(colnames(p4_shap_min_static_dynamic_spatial) %in%
                                                                             c('PRMS_segid', 'Date', 'data_type', 'group'))],
                         data = left_join(as.data.frame(p4_shap_min_static_dynamic_spatial) %>% 
                                            select(PRMS_segid, Date), 
                                          p4_train_RF_min_static_dynamic_spatial$best_fit$splits[[1]]$data %>% 
                                            select(-mean_value), 
                                          by = c('PRMS_segid', 'Date')) %>%
                           as.data.frame(),
                         model_name = 'RF_min_static_dynamic_spatial_full',
                         out_dir = "4_predict/out/spatial/shap/RF_min_static_dynamic",
                         ncores = SHAP_cores),
    format = "file",
    repository = 'local'
  ),
  #mapped over data splits
  tar_target(
    p4_shap_dependence_min_static_dynamic_spatial_subsets_png,
    plot_shap_dependence(shap = p4_shap_min_static_dynamic_spatial_splits$shap[,
                                                                                 -which(colnames(p4_shap_min_static_dynamic_spatial_splits$shap) %in% 
                                                                                          c('PRMS_segid', 'Date', 'data_type', 'group'))],
                         data = left_join(as.data.frame(p4_shap_min_static_dynamic_spatial_splits$shap) %>% 
                                            select(PRMS_segid, Date), 
                                          p4_train_RF_min_static_dynamic_spatial$best_fit$splits[[1]]$data %>% 
                                            select(-mean_value), 
                                          by = c('PRMS_segid', 'Date')) %>%
                           as.data.frame(),
                         model_name = 'RF_min_static_dynamic_spatial_full',
                         out_dir = get_shap_dir(main_dir = "4_predict/out/spatial/shap/RF_min_static_dynamic",
                                                subdir_name = p4_shap_min_static_dynamic_spatial_splits$name),
                         ncores = SHAP_cores),
    pattern = map(p4_shap_min_static_dynamic_spatial_splits),
    iteration = 'list',
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_dependence_dynamic_spatial_png,
    plot_shap_dependence(shap = p4_shap_dynamic_spatial[,
                                                         -which(colnames(p4_shap_dynamic_spatial) %in% 
                                                                  c('PRMS_segid', 'Date', 'data_type', 'group'))],
                         data = left_join(as.data.frame(p4_shap_dynamic_spatial) %>% 
                                            select(PRMS_segid, Date), 
                                          p4_train_RF_dynamic_spatial$best_fit$splits[[1]]$data %>% 
                                            select(-mean_value), 
                                          by = c('PRMS_segid', 'Date')) %>%
                           as.data.frame(),
                         model_name = 'RF_dynamic_spatial_full',
                         out_dir = "4_predict/out/spatial/shap/RF_dynamic",
                         ncores = SHAP_cores),
    format = "file",
    repository = 'local'
  ),
  
  #SHAP beeswarm
  tar_target(
    p4_shap_beeswarm_static_png,
    plot_shap_global_sv(shap = p4_shap_static,
                        data = left_join(as.data.frame(p4_shap_static) %>% 
                                           select(PRMS_segid, Date), 
                                         p4_train_RF_static$best_fit$splits[[1]]$data %>% 
                                           select(-mean_value), 
                                         by = c('PRMS_segid', 'Date')) %>%
                          as.data.frame(),
                        model_name = 'RF_static_full',
                        out_dir = '4_predict/out/random/shap/RF_static',
                        num_features = 40,
                        drop_columns = c('PRMS_segid', 'Date', 'data_type'),
                        xlims = c(-1000, 1000)),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_beeswarm_min_static_png,
    plot_shap_global_sv(shap = p4_shap_min_static,
                        data = left_join(as.data.frame(p4_shap_min_static) %>% 
                                           select(PRMS_segid, Date), 
                                         p4_train_RF_min_static$best_fit$splits[[1]]$data %>% 
                                           select(-mean_value), 
                                         by = c('PRMS_segid', 'Date')) %>%
                          as.data.frame(),
                        model_name = 'RF_min_static_full',
                        out_dir = '4_predict/out/random/shap/RF_min_static',
                        num_features = 40,
                        drop_columns = c('PRMS_segid', 'Date', 'data_type'),
                        xlims = c(-1000, 1000)),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_beeswarm_static_dynamic_png,
    plot_shap_global_sv(shap = p4_shap_static_dynamic,
                        data = left_join(as.data.frame(p4_shap_static_dynamic) %>% 
                                           select(PRMS_segid, Date), 
                                         p4_train_RF_static_dynamic$best_fit$splits[[1]]$data %>% 
                                           select(-mean_value), 
                                         by = c('PRMS_segid', 'Date')) %>%
                          as.data.frame(),
                        model_name = 'RF_static_dynamic_full',
                        out_dir = '4_predict/out/random/shap/RF_static_dynamic',
                        num_features = 40,
                        drop_columns = c('PRMS_segid', 'Date', 'data_type'),
                        xlims = c(-1000, 1000)),
    format = "file",
    repository = 'local'
  ),
  #mapped over data splits
  tar_target(
    p4_shap_beeswarm_static_dynamic_subsets_png,
    plot_shap_global_sv(shap = p4_shap_static_dynamic_splits$shap,
                        data = left_join(as.data.frame(p4_shap_static_dynamic_splits$shap) %>% 
                                            select(PRMS_segid, Date), 
                                          p4_train_RF_static_dynamic$best_fit$splits[[1]]$data %>% 
                                            select(-mean_value), 
                                          by = c('PRMS_segid', 'Date')) %>%
                           as.data.frame(),
                         model_name = 'RF_static_dynamic_full',
                         out_dir = get_shap_dir(main_dir = "4_predict/out/random/shap/RF_static_dynamic",
                                                subdir_name = p4_shap_static_dynamic_splits$name),
                        num_features = 40,
                        drop_columns = c('PRMS_segid', 'Date', 'data_type'),
                        xlims = c(-1000, 1000)),
    pattern = map(p4_shap_static_dynamic_splits),
    iteration = 'list',
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_beeswarm_min_static_dynamic_png,
    plot_shap_global_sv(shap = p4_shap_min_static_dynamic,
                        data = left_join(as.data.frame(p4_shap_min_static_dynamic) %>% 
                                           select(PRMS_segid, Date), 
                                         p4_train_RF_min_static_dynamic$best_fit$splits[[1]]$data %>% 
                                           select(-mean_value), 
                                         by = c('PRMS_segid', 'Date')) %>%
                          as.data.frame(),
                        model_name = 'RF_min_static_dynamic_full',
                        out_dir = '4_predict/out/random/shap/RF_min_static_dynamic',
                        num_features = 40,
                        drop_columns = c('PRMS_segid', 'Date', 'data_type'),
                        xlims = c(-1000, 1000)),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_beeswarm_dynamic_png,
    plot_shap_global_sv(shap = p4_shap_dynamic,
                        data = left_join(as.data.frame(p4_shap_dynamic) %>% 
                                           select(PRMS_segid, Date), 
                                         p4_train_RF_dynamic$best_fit$splits[[1]]$data %>% 
                                           select(-mean_value), 
                                         by = c('PRMS_segid', 'Date')) %>%
                          as.data.frame(),
                        model_name = 'RF_dynamic_full',
                        out_dir = '4_predict/out/random/shap/RF_dynamic',
                        num_features = 40,
                        drop_columns = c('PRMS_segid', 'Date', 'data_type'),
                        xlims = c(-1000, 1000)),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_beeswarm_static_dynamic_temporal_png,
    plot_shap_global_sv(shap = p4_shap_static_dynamic_temporal,
                        data = left_join(as.data.frame(p4_shap_static_dynamic_temporal) %>% 
                                           select(PRMS_segid, Date), 
                                         p4_train_RF_static_dynamic_temporal$best_fit$splits[[1]]$data %>% 
                                           select(-mean_value), 
                                         by = c('PRMS_segid', 'Date')) %>%
                          as.data.frame(),
                        model_name = 'RF_static_dynamic_temporal_full',
                        out_dir = '4_predict/out/temporal/shap/RF_static_dynamic',
                        num_features = 40,
                        drop_columns = c('PRMS_segid', 'Date', 'group', 'data_type'),
                        xlims = c(-1000, 1000)),
    format = "file",
    repository = 'local'
  ),
  #mapped over data splits
  tar_target(
    p4_shap_beeswarm_static_dynamic_temporal_subsets_png,
    plot_shap_global_sv(shap = p4_shap_static_dynamic_temporal_splits$shap,
                        data = left_join(as.data.frame(p4_shap_static_dynamic_temporal_splits$shap) %>% 
                                           select(PRMS_segid, Date), 
                                         p4_train_RF_static_dynamic_temporal$best_fit$splits[[1]]$data %>% 
                                           select(-mean_value), 
                                         by = c('PRMS_segid', 'Date')) %>%
                          as.data.frame(),
                        model_name = 'RF_static_dynamic_temporal_full',
                        out_dir = get_shap_dir(main_dir = "4_predict/out/temporal/shap/RF_static_dynamic",
                                               subdir_name = p4_shap_static_dynamic_temporal_splits$name),
                        num_features = 40,
                        drop_columns = c('PRMS_segid', 'Date', 'data_type', 'group'),
                        xlims = c(-1000, 1000)),
    pattern = map(p4_shap_static_dynamic_temporal_splits),
    iteration = 'list',
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_beeswarm_min_static_dynamic_temporal_png,
    plot_shap_global_sv(shap = p4_shap_min_static_dynamic_temporal,
                        data = left_join(as.data.frame(p4_shap_min_static_dynamic_temporal) %>% 
                                           select(PRMS_segid, Date), 
                                         p4_train_RF_min_static_dynamic_temporal$best_fit$splits[[1]]$data %>% 
                                           select(-mean_value), 
                                         by = c('PRMS_segid', 'Date')) %>%
                          as.data.frame(),
                        model_name = 'RF_min_static_dynamic_temporal_full',
                        out_dir = '4_predict/out/temporal/shap/RF_min_static_dynamic',
                        num_features = 40,
                        drop_columns = c('PRMS_segid', 'Date', 'group', 'data_type'),
                        xlims = c(-1000, 1000)),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_beeswarm_dynamic_temporal_png,
    plot_shap_global_sv(shap = p4_shap_dynamic_temporal,
                        data = left_join(as.data.frame(p4_shap_dynamic_temporal) %>% 
                                           select(PRMS_segid, Date), 
                                         p4_train_RF_dynamic_temporal$best_fit$splits[[1]]$data %>% 
                                           select(-mean_value), 
                                         by = c('PRMS_segid', 'Date')) %>%
                          as.data.frame(),
                        model_name = 'RF_dynamic_temporal_full',
                        out_dir = '4_predict/out/temporal/shap/RF_dynamic',
                        num_features = 40,
                        drop_columns = c('PRMS_segid', 'Date', 'group', 'data_type'),
                        xlims = c(-1000, 1000)),
    format = "file",
    repository = 'local'
  ),
   tar_target(
     p4_shap_beeswarm_static_dynamic_spatial_png,
     plot_shap_global_sv(shap = p4_shap_static_dynamic_spatial,
                         data = left_join(as.data.frame(p4_shap_static_dynamic_spatial) %>% 
                                            select(PRMS_segid, Date), 
                                          p4_train_RF_static_dynamic_spatial$best_fit$splits[[1]]$data %>% 
                                            select(-mean_value), 
                                          by = c('PRMS_segid', 'Date')) %>%
                           as.data.frame(),
                         model_name = 'RF_static_dynamic_spatial_full',
                         out_dir = '4_predict/out/spatial/shap/RF_static_dynamic',
                         num_features = 40,
                         drop_columns = c('PRMS_segid', 'Date', 'group', 'data_type'),
                         xlims = c(-1000, 1000)),
     format = "file",
     repository = 'local'
   ),
  tar_target(
    p4_shap_beeswarm_min_static_dynamic_spatial_png,
    plot_shap_global_sv(shap = p4_shap_min_static_dynamic_spatial,
                        data = left_join(as.data.frame(p4_shap_min_static_dynamic_spatial) %>% 
                                           select(PRMS_segid, Date), 
                                         p4_train_RF_min_static_dynamic_spatial$best_fit$splits[[1]]$data %>% 
                                           select(-mean_value), 
                                         by = c('PRMS_segid', 'Date')) %>%
                          as.data.frame(),
                        model_name = 'RF_min_static_dynamic_spatial_full',
                        out_dir = '4_predict/out/spatial/shap/RF_min_static_dynamic',
                        num_features = 40,
                        drop_columns = c('PRMS_segid', 'Date', 'group', 'data_type'),
                        xlims = c(-1000, 1000)),
    format = "file",
    repository = 'local'
  ),
  #mapped over data splits
  tar_target(
    p4_shap_beeswarm_min_static_dynamic_spatial_subsets_png,
    plot_shap_global_sv(shap = p4_shap_min_static_dynamic_spatial_splits$shap,
                        data = left_join(as.data.frame(p4_shap_min_static_dynamic_spatial_splits$shap) %>% 
                                           select(PRMS_segid, Date), 
                                         p4_train_RF_min_static_dynamic_spatial$best_fit$splits[[1]]$data %>% 
                                           select(-mean_value), 
                                         by = c('PRMS_segid', 'Date')) %>%
                          as.data.frame(),
                        model_name = 'RF_min_static_dynamic_spatial_full',
                        out_dir = get_shap_dir(main_dir = "4_predict/out/spatial/shap/RF_min_static_dynamic",
                                               subdir_name = p4_shap_min_static_dynamic_spatial_splits$name),
                        num_features = 40,
                        drop_columns = c('PRMS_segid', 'Date', 'data_type', 'group'),
                        xlims = c(-1000, 1000)),
    pattern = map(p4_shap_min_static_dynamic_spatial_splits),
    iteration = 'list',
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_shap_beeswarm_dynamic_spatial_png,
    plot_shap_global_sv(shap = p4_shap_dynamic_spatial,
                        data = left_join(as.data.frame(p4_shap_dynamic_spatial) %>% 
                                           select(PRMS_segid, Date), 
                                         p4_train_RF_dynamic_spatial$best_fit$splits[[1]]$data %>% 
                                           select(-mean_value), 
                                         by = c('PRMS_segid', 'Date')) %>%
                          as.data.frame(),
                        model_name = 'RF_dynamic_spatial_full',
                        out_dir = '4_predict/out/spatial/shap/RF_dynamic',
                        num_features = 40,
                        drop_columns = c('PRMS_segid', 'Date', 'group', 'data_type'),
                        xlims = c(-1000, 1000)),
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