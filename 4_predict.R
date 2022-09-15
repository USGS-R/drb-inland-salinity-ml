source("4_predict/src/select_features.R")
source("4_predict/src/train_models.R")
source("4_predict/src/plot_diagnostics.R")

#Predict phase
p4_targets_list <- list(
  #Screen out highly correlated attributes
  # remove TOT variables that are highly correlated with other variables (> 0.9)
  tar_target(p4_screened_attrs,
             drop_high_corr_attrs(features = p2_all_attr_SC_obs %>% 
                                    drop_na(mean_value) %>%
                                    #remove unused columns
                                    select(-c("mean_value", "PRMS_segid","Date", "min_value", 
                                              "max_value", "n_value", "sd_value", 
                                              "cv_value", "site_ids")), 
                                  threshold_corr = 0.9,
                                  first_drop_var_prefix = 'TOT',
                                  drop_columns = c(#Keeping shallow and deep soil info. Dropping middle 2.
                                    "SRL35AG", "SRL45AG",
                                    #Elevation is correlated with many attributes
                                    "ELEV",
                                    #NORM storage almost same as NID
                                    "NID_STORAGE",
                                    #The WDANN attribute is similar and does not have a lower bound
                                    "TOT_CWD_frmCAT",
                                    #Correlated with basin area
                                    "CAT_STREAM_LENGTH_sum", "CAT_RDX_sum",
                                    #Almost identical to DEP_CL
                                    "DEP_MG",
                                    #Many 0s and correlated with CAT_OLSON_PERM_area_wtd, 
                                    #which may explain the salinity value
                                    "CAT_SALINAVE_area_wtd",
                                    #correlated with PERMAVE
                                    "SANDAVE",
                                    #remove median, p10, p90 baseflow, keep mean
                                    "med_natl_baseflow", "p10_natl_baseflow", "p90_natl_baseflow",
                                    #correlated with vertical permeability TOT_KFACT_UP
                                    "TOT_AWCAVE",
                                    #PET must be directly derived from avg. annual temperature
                                    "PET",
                                    #Several TOT variables have the same pattern and essentially
                                    #represent drainage area. Removing these variables
                                    #to preferentially retain the basin area
                                    "TOT_MAJOR", "TOT_NDAMS", "TOT_NORM",
                                    #removing basin slope because it's correlated with
                                    #precipitation that falls as snow and temperatures
                                    #So it's like a N/S indicator
                                    "TOT_BASIN_SLOPE", "CAT_BASIN_SLOPE_area_wtd",
                                    #prefer to remove housing density because it's correlated
                                    #with land cover and land cover can be projected
                                    "HDENS",
                                    #Correlated with LC 6 (forest)
                                    "TOT_TWI",
                                    #Correlated with LC 3 (high urban)
                                    "TOT_TOTAL_ROAD_DENS",
                                    #TOT soils correlated with CAT soils and other features
                                    "TOT_HG", "TOT_BEDPERM_3",
                                    #Correlated with water table depth
                                    "TOT_WDANN",
                                    #Correlated with basin area and land cover
                                    "RDX"),
                                  drop_exact = c(#Correlated with density of NPDES sites
                                    "CAT_NPDES_MAJ_sum", 'TOT_NPDES_MAJ', 
                                    #Correlated with basin area
                                    "TOT_SedLOAD_2012", "TOT_rdsalt_prop_frmCAT", 
                                    "TOT_STREAM_LENGTH"),
                                  dynamic_contains = c('_0', '_10year', '_20year'),
                                  cor_method = 'spearman'),
             deployment = 'main'
  ),
  
  
  #Boruta screening - only static attributes
  tar_target(p4_Boruta_static,
             screen_Boruta(input_data = p2_all_attr_SC_obs %>% 
                             drop_na(mean_value) %>%
                             #attributes to retain (identifiers, predictors)
                             select(mean_value, PRMS_segid, Date, min_value, 
                                    max_value, n_value, sd_value, 
                                    cv_value, site_ids, all_of(p4_screened_attrs)),
                           drop_attrs = c("PRMS_segid","Date", "min_value", 
                                          "max_value", "n_value", "sd_value", 
                                          "cv_value", "site_ids",
                                          p2_all_attr_SC_obs %>%
                                            select(all_of(p4_screened_attrs)) %>% 
                                            select(ends_with('_mean'), 
                                                   ends_with('_0')) %>% 
                                            colnames()),
                           pred_var = 'mean_value',
                           ncores = Boruta_cores, 
                           brf_runs = Boruta_runs, 
                           ntrees = Boruta_trees,
                           train_prop = 0.8,
                           by_time = FALSE,
                           num_data_splits = 20
             ),
             deployment = 'worker'
  ),
  
  #Prepare attributes for RF model training
  #only static attributes
  tar_target(p4_selected_static_attrs,
             select_attrs(brf_output = p4_Boruta_static)
  ),
  #static and dynamic
  tar_target(p4_selected_static_dynamic_attrs,
             select_attrs(brf_output = p4_Boruta_static,
                          retain_attrs = p2_all_attr_SC_obs %>% 
                            select(all_of(p4_screened_attrs)) %>%
                            select(ends_with('_mean'), ends_with('_0'))
             ),
             deployment = 'worker'
  ),
  
  #RF train
  #only static attributes
  tar_target(p4_train_RF_static,
             train_models_grid(brf_output = p4_selected_static_attrs,
                               ncores = Boruta_cores,
                               v_folds = cv_folds
             ),
             deployment = 'worker'
  ),
  #static and dynamic
  tar_target(p4_train_RF_static_dynamic,
             train_models_grid(brf_output = p4_selected_static_dynamic_attrs,
                               ncores = Boruta_cores,
                               v_folds = cv_folds
             ),
             deployment = 'worker'
  ),
  
  # Visualize Model Diagnostics:
  
  # Boruta screening
  # tar_target(p4_Boruta_static_png,
  #            plot_Boruta(p4_Boruta_static$brf_All,
  #                        model_name = 'daily_SC_RF',
  #                        out_dir = '6_predict/out/Boruta'),
  #            deployment = 'main',
  #            format = 'file'
  # ),
  
  # RF variable importance plot 
  #Should add error bars over X random seeds
  tar_target(p4_vip_static_png,
             plot_vip(RF_model = p4_train_RF_static$best_fit,
                      model_name = 'daily_SC_RF_static',
                      num_features = 20,
                      out_dir = '6_predict/out/vip'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p4_vip_static_dynamic_png,
             plot_vip(RF_model = p4_train_RF_static_dynamic$best_fit,
                      model_name = 'daily_SC_RF_static_dynamic',
                      num_features = 20,
                      out_dir = '6_predict/out/vip'),
             deployment = 'main',
             format = 'file'
  ),
  
  # RF hyperparameter optimization
  tar_target(p4_hypopt_static_png,
             plot_hyperparam_opt_results_RF(p4_train_RF_static$grid_params,
                                            model_name = 'daily_SC_RF_static',
                                            out_dir = '6_predict/out/hypopt'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p4_hypopt_marginals_static_png,
             plot_hyperparam_opt_marginals(p4_train_RF_static$grid_params,
                                           model_name = 'daily_SC_RF_static',
                                           plt_type = "marginals",
                                           perf_metric = NULL,
                                           out_dir = '6_predict/out/hypopt'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p4_hypopt_static_dynamic_png,
             plot_hyperparam_opt_results_RF(p4_train_RF_static_dynamic$grid_params,
                                            model_name = 'daily_SC_RF_static_dynamic',
                                            out_dir = '6_predict/out/hypopt'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p4_hypopt_marginals_static_dynamic_png,
             plot_hyperparam_opt_marginals(p4_train_RF_static_dynamic$grid_params,
                                           model_name = 'daily_SC_RF_static_dynamic',
                                           plt_type = "marginals",
                                           perf_metric = NULL,
                                           out_dir = '6_predict/out/hypopt'),
             deployment = 'main',
             format = 'file'
  ),
  
  # RF predicted vs. observed y 
  #Should be for the mean over X random seeds
  tar_target(p4_pred_obs_static_png,
             plot_pred_obs(df_pred_obs = p4_train_RF_static$best_fit,
                           model_name = 'daily_SC_RF_static',
                           out_dir = '6_predict/out/pred_obs'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p4_pred_obs_static_dynamic_png,
             plot_pred_obs(df_pred_obs = p4_train_RF_static_dynamic$best_fit,
                           model_name = 'daily_SC_RF_static_dynamic',
                           out_dir = '6_predict/out/pred_obs'),
             deployment = 'main',
             format = 'file'
  ),
  
  #Train test barplot coverage of SC
  tar_target(p4_train_test_boxplot_coverage_static_png,
             boxplot_compare_RF(data_split = p4_train_RF_static$best_fit$input_data,
                           model_name = 'daily_SC_RF_static',
                           pred_var = 'mean_value',
                           out_dir = '6_predict/out/pred_obs'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p4_train_test_boxplot_coverage_static_dynamic_png,
             boxplot_compare_RF(data_split = p4_train_RF_static_dynamic$best_fit$input_data,
                                model_name = 'daily_SC_RF_static_dynamic',
                                pred_var = 'mean_value',
                                out_dir = '6_predict/out/pred_obs'),
             deployment = 'main',
             format = 'file'
  ),
  
  #Cross validation coverage
  tar_target(p4_train_test_CV_static_png,
             boxplot_compare_RF(mod = p4_train_RF_static$wf,
                                pred_var = 'mean_value',
                                perf_metric = 'rmse',
                                out_dir = '6_predict/out/'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p4_train_test_CV_static_dynamic_png,
             boxplot_compare_RF(mod = p4_train_RF_static_dynamic$wf,
                                pred_var = 'mean_value',
                                perf_metric = 'rmse',
                                out_dir = '6_predict/out/'),
             deployment = 'main',
             format = 'file'
  )
  
  # Spatial residuals aggregated over time
  #Should be for the mean over X random seeds
  
  
  # Model RMSE comparison boxplots / barplots
)