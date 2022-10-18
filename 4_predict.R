source("4_predict/src/select_features.R")
source("4_predict/src/train_models.R")
source("4_predict/src/plot_diagnostics.R")
source("1_fetch/src/generate_credentials.R")

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
             deployment = 'worker',
             cue = tar_cue(mode = 'never')
  ),
  
  # Refresh AWS credentials
  tar_target(
    p4_aws_credentials_1,
    generate_credentials(dummy_var = p4_Boruta_static),
    deployment = 'main',
    cue = tar_cue('always')
  ),
  
  #Prepare attributes for RF model training
  #only static attributes
  tar_target(p4_selected_static_attrs,
             select_attrs(brf_output = p4_Boruta_static, 
                          retain_attrs = c('PRMS_segid', 'Date'))
  ),
  #only the minimum static attributes identified by any one data split (20)
  tar_target(p4_min_selected_static_attrs,
             {ret <- p4_selected_static_attrs
             min_selected_attrs <- length(p4_selected_static_attrs$brf[[1]]$finalDecision)
             for(i in 1:length(p4_selected_static_attrs$brf)){
               num_attrs <- length(p4_Boruta_static$brf[[i]]$finalDecision[p4_Boruta_static$brf[[i]]$finalDecision != 'Rejected'])
               if (num_attrs < min_selected_attrs){
                 min_selected_attrs <- num_attrs
                 #record index with the minimum
                 ind = i
               }
             }
             names_select <- names(p4_Boruta_static$brf[[ind]]$finalDecision[p4_Boruta_static$brf[[ind]]$finalDecision != 'Rejected'])
             ret$input_data$split$data <- ret$input_data$split$data %>% 
               select(all_of(names_select), PRMS_segid, Date, mean_value)
             ret$input_data$training <- ret$input_data$training %>% 
               select(all_of(names_select), PRMS_segid, Date, mean_value)
             ret$input_data$testing <- ret$input_data$testing %>% 
               select(all_of(names_select), PRMS_segid, Date, mean_value)
             ret
             }
  ),
  #static and dynamic
  tar_target(p4_selected_static_dynamic_attrs,
             select_attrs(brf_output = p4_Boruta_static,
                          retain_attrs = c(p2_all_attr_SC_obs %>% 
                            select(all_of(p4_screened_attrs)) %>%
                            select(ends_with('_mean'), ends_with('_0')) %>%
                            colnames(),
                            'PRMS_segid', 'Date') 
             ),
             deployment = 'worker'
  ),
  #minimum static and dynamic
  tar_target(p4_selected_min_static_dynamic_attrs,
             {tmp_brf_output <- p4_Boruta_static
             tmp_brf_output$selected_features <- colnames(p4_min_selected_static_attrs$input_data$training)
             select_attrs(brf_output = tmp_brf_output,
                          retain_attrs = c(p2_all_attr_SC_obs %>% 
                            select(all_of(p4_screened_attrs)) %>%
                            select(ends_with('_mean'), ends_with('_0')) %>%
                            colnames(),
                            'PRMS_segid', 'Date')
             )
             },
             deployment = 'worker'
  ),
  #dynamic only
  tar_target(p4_dynamic_attrs,
             {
               brf_output <- p4_Boruta_static
               #using this removal of attributes to allow selecting only the 
               #dynamic attributes
               brf_output$selected_features <- NULL
               select_attrs(brf_output = brf_output,
                            retain_attrs = c(p2_all_attr_SC_obs %>%
                                  select(all_of(p4_screened_attrs)) %>%
                                  select(ends_with('_mean'), ends_with('_0')) %>%
                                  colnames(),
                                'PRMS_segid', 'Date')
                            )
             },
             deployment = 'worker'
  ),
  #Temporal split rule for tests with dynamic attrs
  #static and dynamic
  tar_target(p4_selected_static_dynamic_attrs_temporal,
             make_temporal_split(attrs = p4_selected_static_dynamic_attrs,
                             train_prop = 0.8),
             deployment = 'worker'
  ),
  #minimum static and dynamic
  tar_target(p4_selected_min_static_dynamic_attrs_temporal,
             make_temporal_split(attrs = p4_selected_min_static_dynamic_attrs,
                                 train_prop = 0.8),
             deployment = 'worker'
  ),
  #dynamic only
  tar_target(p4_dynamic_attrs_temporal,
             make_temporal_split(attrs = p4_dynamic_attrs,
                                 train_prop = 0.8),
             deployment = 'worker'
  ),
  
  
  #RF train
  #only static attributes
  tar_target(p4_train_RF_static,
             train_models_grid(brf_output = p4_selected_static_attrs,
                               ncores = RF_cores,
                               v_folds = cv_folds,
                               range_mtry = c(5,30),
                               range_minn = c(2,20),
                               range_trees = c(100,500),
                               gridsize = 50,
                               id_cols = c('PRMS_segid', 'Date')
             ),
             deployment = 'worker',
             cue = tar_cue(mode = 'never')
  ),
  
  # Refresh AWS credentials
  tar_target(
    p4_aws_credentials_2,
    generate_credentials(dummy_var = p4_train_RF_static),
    deployment = 'main',
    cue = tar_cue('always')
  ),
  
  #only the minimum static attributes
  # tar_target(p4_train_RF_min_static,
  #            train_models_grid(brf_output = p4_min_selected_static_attrs,
  #                              ncores = RF_cores,
  #                              v_folds = cv_folds,
  #                              range_mtry = c(2,20),
  #                              range_minn = c(2,20),
  #                              range_trees = c(100,500),
  #                              gridsize = 50,
  #                              id_cols = c('PRMS_segid', 'Date')
  #            ),
  #            deployment = 'worker'
  # ),
  tar_target(p4_train_RF_min_static,
             {#Filter out data before 1984-09-30 for training due to NAs
               filtered_attrs <- filter_rows_date(p4_min_selected_static_attrs,
                                                  '1984-09-30')
             train_models_grid(brf_output = filtered_attrs,
                               ncores = 35,
                               v_folds = 2,
                               range_mtry = c(2,20),
                               range_minn = c(2,20),
                               range_trees = c(100,500),
                               gridsize = 3,
                               id_cols = c('PRMS_segid', 'Date'))
             },
             deployment = 'worker'
  ),
  
  # Refresh AWS credentials
  tar_target(
    p4_aws_credentials_3,
    generate_credentials(dummy_var = p4_train_RF_min_static),
    deployment = 'main',
    cue = tar_cue('always')
  ),
  
  #static and dynamic
  # tar_target(p4_train_RF_static_dynamic,
  #            {#Filter out data before 1984-09-30 for training due to NAs
  #              filtered_attrs <- filter_rows_date(p4_selected_static_dynamic_attrs,
  #                                                 '1984-09-30')
  #              train_models_grid(brf_output = filtered_attrs,
  #                                ncores = RF_cores,
  #                                v_folds = cv_folds,
  #                                range_mtry = c(5,30),
  #                                range_minn = c(2,20),
  #                                range_trees = c(100,500),
  #                                gridsize = 50,
  #                                id_cols = c('PRMS_segid', 'Date'))
  #            },
  #            deployment = 'worker'
  # ),
  tar_target(p4_train_RF_static_dynamic,
             {#Filter out data before 1984-09-30 for training due to NAs
               filtered_attrs <- filter_rows_date(p4_selected_static_dynamic_attrs,
                                                  '1984-09-30')
               train_models_grid(brf_output = filtered_attrs,
                                 ncores = 35,
                                 v_folds = 2,
                                 range_mtry = c(5,30),
                                 range_minn = c(2,20),
                                 range_trees = c(100,500),
                                 gridsize = 3,
                                 id_cols = c('PRMS_segid', 'Date'))
             },
             deployment = 'worker'
  ),
  
  # Refresh AWS credentials
  tar_target(
    p4_aws_credentials_4,
    generate_credentials(dummy_var = p4_train_RF_static_dynamic),
    deployment = 'main',
    cue = tar_cue('always')
  ),
  
  #minimum static and dynamic
  # tar_target(p4_train_RF_min_static_dynamic,
  #            train_models_grid(brf_output = p4_selected_min_static_dynamic_attrs,
  #                              ncores = RF_cores,
  #                              v_folds = cv_folds,
  #                              range_mtry = c(5,30),
  #                              range_minn = c(2,20),
  #                              range_trees = c(100,500),
  #                              gridsize = 50,
  #                              id_cols = c('PRMS_segid', 'Date')
  #            ),
  #            deployment = 'worker'
  # ),
  tar_target(p4_train_RF_min_static_dynamic,
             {#Filter out data before 1984-09-30 for training due to NAs
               filtered_attrs <- filter_rows_date(p4_selected_min_static_dynamic_attrs,
                                                  '1984-09-30')
               train_models_grid(brf_output = filtered_attrs,
                               ncores = 35,
                               v_folds = 2,
                               range_mtry = c(5,30),
                               range_minn = c(2,20),
                               range_trees = c(100,500),
                               gridsize = 3,
                               id_cols = c('PRMS_segid', 'Date'))
             },
             deployment = 'worker'
  ),
  
  # Refresh AWS credentials
  tar_target(
    p4_aws_credentials_5,
    generate_credentials(dummy_var = p4_train_RF_min_static_dynamic),
    deployment = 'main',
    cue = tar_cue('always')
  ),
  
  #dynamic only
  # tar_target(p4_train_RF_dynamic,
  #            train_models_grid(brf_output = p4_dynamic_attrs,
  #                              ncores = RF_cores,
  #                              v_folds = cv_folds,
  #                              range_mtry = c(5,30),
  #                              range_minn = c(2,20),
  #                              range_trees = c(100,500),
  #                              gridsize = 50,
  #                              id_cols = c('PRMS_segid', 'Date')
  #            ),
  #            deployment = 'worker'
  # ),
  tar_target(p4_train_RF_dynamic,
             {#Filter out data before 1984-09-30 for training due to NAs
               filtered_attrs <- filter_rows_date(p4_dynamic_attrs,
                                                  '1984-09-30')
               train_models_grid(brf_output = filtered_attrs,
                                 ncores = 35,
                                 v_folds = 2,
                                 range_mtry = c(5,30),
                                 range_minn = c(2,20),
                                 range_trees = c(100,500),
                                 gridsize = 3,
                                 id_cols = c('PRMS_segid', 'Date'))
             },
             deployment = 'worker'
  ),
  
  # Refresh AWS credentials
  tar_target(
    p4_aws_credentials_6,
    generate_credentials(dummy_var = p4_train_RF_dynamic),
    deployment = 'main',
    cue = tar_cue('always')
  ),
  
  #Temporal train/test split and CV splits
  tar_target(p4_train_RF_static_dynamic_temporal,
             {#Filter out data before 1984-09-30 for training due to NAs
               filtered_attrs <- filter_rows_date(p4_selected_static_dynamic_attrs_temporal,
                                                  '1984-09-30')
               train_models_grid(brf_output = filtered_attrs,
                                 ncores = 35,
                                 v_folds = 2,
                                 range_mtry = c(5,30),
                                 range_minn = c(2,20),
                                 range_trees = c(100,500),
                                 gridsize = 3,
                                 id_cols = c('PRMS_segid', 'Date'))
             },
             deployment = 'worker'
  ),
  
  # Refresh AWS credentials
  tar_target(
    p4_aws_credentials_7,
    generate_credentials(dummy_var = p4_train_RF_static_dynamic_temporal),
    deployment = 'main',
    cue = tar_cue('always')
  ),
  
  tar_target(p4_train_RF_min_static_dynamic_temporal,
             {#Filter out data before 1984-09-30 for training due to NAs
               filtered_attrs <- filter_rows_date(p4_selected_min_static_dynamic_attrs_temporal,
                                                  '1984-09-30')
               train_models_grid(brf_output = filtered_attrs,
                                 ncores = 35,
                                 v_folds = 2,
                                 range_mtry = c(5,30),
                                 range_minn = c(2,20),
                                 range_trees = c(100,500),
                                 gridsize = 3,
                                 id_cols = c('PRMS_segid', 'Date'))
             },
             deployment = 'worker'
  ),
  
  # Refresh AWS credentials
  tar_target(
    p4_aws_credentials_8,
    generate_credentials(dummy_var = p4_train_RF_min_static_dynamic_temporal),
    deployment = 'main',
    cue = tar_cue('always')
  ),
  
  tar_target(p4_train_RF_dynamic_temporal,
             {#Filter out data before 1984-09-30 for training due to NAs
               filtered_attrs <- filter_rows_date(p4_dynamic_attrs_temporal,
                                                  '1984-09-30')
               train_models_grid(brf_output = filtered_attrs,
                                 ncores = 35,
                                 v_folds = 2,
                                 range_mtry = c(5,30),
                                 range_minn = c(2,20),
                                 range_trees = c(100,500),
                                 gridsize = 3,
                                 id_cols = c('PRMS_segid', 'Date'))
             },
             deployment = 'worker'
  ),
  
  # Refresh AWS credentials
  tar_target(
    p4_aws_credentials_9,
    generate_credentials(dummy_var = p4_train_RF_dynamic),
    deployment = 'main',
    cue = tar_cue('always')
  ),
  
  # Visualize Model Diagnostics:
  
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
  
  #RF Predictions
  #Static features, full dataset
  tar_target(p4_pred_RF_static,
             predict_test_data(model_wf = p4_train_RF_static$workflow,
                               test_data = p4_train_RF_static$best_fit$splits[[1]]$data,
                               target_name = 'mean_value',
                               train_ind = p4_train_RF_static$best_fit$splits[[1]]$in_id),
             deployment = 'main'
  ),
  #Static features, test dataset
  tar_target(p4_pred_RF_static_test,
             predict_test_data(model_wf = p4_train_RF_static$workflow,
                               test_data = p4_train_RF_static$best_fit$splits[[1]]$data[-p4_train_RF_static$best_fit$splits[[1]]$in_id,],
                               target_name = 'mean_value'),
             deployment = 'main'
  ),
  #minimum static features, full dataset
  tar_target(p4_pred_RF_min_static,
             predict_test_data(model_wf = p4_train_RF_min_static$workflow,
                               test_data = p4_train_RF_min_static$best_fit$splits[[1]]$data,
                               target_name = 'mean_value',
                               train_ind = p4_train_RF_min_static$best_fit$splits[[1]]$in_id),
             deployment = 'main'
  ),
  #minimum static features, test dataset
  tar_target(p4_pred_RF_min_static_test,
             predict_test_data(model_wf = p4_train_RF_min_static$workflow,
                               test_data = p4_train_RF_min_static$best_fit$splits[[1]]$data[-p4_train_RF_min_static$best_fit$splits[[1]]$in_id,],
                               target_name = 'mean_value'),
             deployment = 'main'
  ),
  #Static and dynamic features, full dataset
  tar_target(p4_pred_RF_static_dynamic,
             predict_test_data(model_wf = p4_train_RF_static_dynamic$workflow,
                               test_data = p4_train_RF_static_dynamic$best_fit$splits[[1]]$data,
                               target_name = 'mean_value',
                               train_ind = p4_train_RF_static_dynamic$best_fit$splits[[1]]$in_id),
             deployment = 'main'
  ),
  #Static and dynamic features, test dataset
  tar_target(p4_pred_RF_static_dynamic_test,
             predict_test_data(model_wf = p4_train_RF_static_dynamic$workflow,
                               test_data = p4_train_RF_static_dynamic$best_fit$splits[[1]]$data[-p4_train_RF_static_dynamic$best_fit$splits[[1]]$in_id,],
                               target_name = 'mean_value'),
             deployment = 'main'
  ),
  #minimum static and dynamic features, full dataset
  tar_target(p4_pred_RF_min_static_dynamic,
             predict_test_data(model_wf = p4_train_RF_min_static_dynamic$workflow,
                               test_data = p4_train_RF_min_static_dynamic$best_fit$splits[[1]]$data,
                               target_name = 'mean_value',
                               train_ind = p4_train_RF_min_static_dynamic$best_fit$splits[[1]]$in_id),
             deployment = 'main'
  ),
  #minimum static and dynamic features, test dataset
  tar_target(p4_pred_RF_min_static_dynamic_test,
             predict_test_data(model_wf = p4_train_RF_min_static_dynamic$workflow,
                               test_data = p4_train_RF_min_static_dynamic$best_fit$splits[[1]]$data[-p4_train_RF_min_static_dynamic$best_fit$splits[[1]]$in_id,],
                               target_name = 'mean_value'),
             deployment = 'main'
  ),
  #dynamic features, full dataset
  tar_target(p4_pred_RF_dynamic,
             predict_test_data(model_wf = p4_train_RF_dynamic$workflow,
                               test_data = p4_train_RF_dynamic$best_fit$splits[[1]]$data,
                               target_name = 'mean_value',
                               train_ind = p4_train_RF_dynamic$best_fit$splits[[1]]$in_id),
             deployment = 'main'
  ),
  #dynamic features, test dataset
  tar_target(p4_pred_RF_dynamic_test,
             predict_test_data(model_wf = p4_train_RF_dynamic$workflow,
                               test_data = p4_train_RF_dynamic$best_fit$splits[[1]]$data[-p4_train_RF_dynamic$best_fit$splits[[1]]$in_id,],
                               target_name = 'mean_value'),
             deployment = 'main'
  ),
  #Temporal train test split
  #Static and dynamic features, full dataset
  tar_target(p4_pred_RF_static_dynamic_temporal,
             predict_test_data(model_wf = p4_train_RF_static_dynamic_temporal$workflow,
                               test_data = p4_train_RF_static_dynamic_temporal$best_fit$splits[[1]]$data,
                               target_name = 'mean_value',
                               train_ind = p4_train_RF_static_dynamic_temporal$best_fit$splits[[1]]$in_id),
             deployment = 'main'
  ),
  #Static and dynamic features, test dataset
  tar_target(p4_pred_RF_static_dynamic_temporal_test,
             predict_test_data(model_wf = p4_train_RF_static_dynamic_temporal$workflow,
                               test_data = p4_train_RF_static_dynamic_temporal$best_fit$splits[[1]]$data[-p4_train_RF_static_dynamic_temporal$best_fit$splits[[1]]$in_id,],
                               target_name = 'mean_value'),
             deployment = 'main'
  ),
  #minimum static and dynamic features, full dataset
  tar_target(p4_pred_RF_min_static_dynamic_temporal,
             predict_test_data(model_wf = p4_train_RF_min_static_dynamic_temporal$workflow,
                               test_data = p4_train_RF_min_static_dynamic_temporal$best_fit$splits[[1]]$data,
                               target_name = 'mean_value',
                               train_ind = p4_train_RF_min_static_dynamic_temporal$best_fit$splits[[1]]$in_id),
             deployment = 'main'
  ),
  #minimum static and dynamic features, test dataset
  tar_target(p4_pred_RF_min_static_dynamic_temporal_test,
             predict_test_data(model_wf = p4_train_RF_min_static_dynamic_temporal$workflow,
                               test_data = p4_train_RF_min_static_dynamic_temporal$best_fit$splits[[1]]$data[-p4_train_RF_min_static_dynamic_temporal$best_fit$splits[[1]]$in_id,],
                               target_name = 'mean_value'),
             deployment = 'main'
  ),
  #dynamic features, full dataset
  tar_target(p4_pred_RF_dynamic_temporal,
             predict_test_data(model_wf = p4_train_RF_dynamic_temporal$workflow,
                               test_data = p4_train_RF_dynamic_temporal$best_fit$splits[[1]]$data,
                               target_name = 'mean_value',
                               train_ind = p4_train_RF_dynamic_temporal$best_fit$splits[[1]]$in_id),
             deployment = 'main'
  ),
  #dynamic features, test dataset
  tar_target(p4_pred_RF_dynamic_temporal_test,
             predict_test_data(model_wf = p4_train_RF_dynamic_temporal$workflow,
                               test_data = p4_train_RF_dynamic_temporal$best_fit$splits[[1]]$data[-p4_train_RF_dynamic_temporal$best_fit$splits[[1]]$in_id,],
                               target_name = 'mean_value'),
             deployment = 'main'
  ),
  
  # RF predicted vs. observed y 
  #Should be for the mean over X random seeds
  tar_target(p4_pred_obs_static_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_static$pred,
                           model_name = 'daily_SC_RF_static',
                           out_dir = '4_predict/out/random/pred_obs', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_static_test_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_static_test$pred,
                           model_name = 'daily_SC_RF_static_testset',
                           out_dir = '4_predict/out/random/pred_obs', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_static_dynamic_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_static_dynamic$pred,
                           model_name = 'daily_SC_RF_static_dynamic',
                           out_dir = '4_predict/out/random/pred_obs', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_static_dynamic_test_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_static_dynamic_test$pred,
                           model_name = 'daily_SC_RF_static_dynamic_testset',
                           out_dir = '4_predict/out/random/pred_obs', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_min_static_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_min_static$pred,
                           model_name = 'daily_SC_RF_min_static',
                           out_dir = '4_predict/out/random/pred_obs', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_min_static_test_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_min_static_test$pred,
                           model_name = 'daily_SC_RF_min_static_testset',
                           out_dir = '4_predict/out/random/pred_obs', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_min_static_dynamic_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_min_static_dynamic$pred,
                           model_name = 'daily_SC_RF_min_static_dynamic',
                           out_dir = '4_predict/out/random/pred_obs', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_min_static_dynamic_test_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_min_static_dynamic_test$pred,
                           model_name = 'daily_SC_RF_min_static_dynamic_testset',
                           out_dir = '4_predict/out/random/pred_obs', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_dynamic_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_dynamic$pred,
                           model_name = 'daily_SC_RF_dynamic',
                           out_dir = '4_predict/out/random/pred_obs', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_dynamic_test_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_dynamic_test$pred,
                           model_name = 'daily_SC_RF_dynamic_testset',
                           out_dir = '4_predict/out/random/pred_obs', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_static_dynamic_temporal_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_static_dynamic_temporal$pred,
                           model_name = 'daily_SC_RF_static_dynamic_temporal',
                           out_dir = '4_predict/out/temporal/pred_obs', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_static_dynamic_temporal_test_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_static_dynamic_temporal_test$pred,
                           model_name = 'daily_SC_RF_static_dynamic_temporal_testset',
                           out_dir = '4_predict/out/temporal/pred_obs', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_min_static_dynamic_temporal_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_min_static_dynamic_temporal$pred,
                           model_name = 'daily_SC_RF_min_static_dynamic_temporal',
                           out_dir = '4_predict/out/temporal/pred_obs', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_min_static_dynamic_temporal_test_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_min_static_dynamic_temporal_test$pred,
                           model_name = 'daily_SC_RF_min_static_dynamic_temporal_testset',
                           out_dir = '4_predict/out/temporal/pred_obs', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_dynamic_temporal_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_dynamic_temporal$pred,
                           model_name = 'daily_SC_RF_dynamic_temporal',
                           out_dir = '4_predict/out/temporal/pred_obs', 
                           count_shade = TRUE),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_pred_obs_dynamic_temporal_test_png,
             plot_pred_obs(df_pred_obs = p4_pred_RF_dynamic_temporal_test$pred,
                           model_name = 'daily_SC_RF_dynamic_temporal_testset',
                           out_dir = '4_predict/out/temporal/pred_obs', 
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
             plot_metric_boxplot(data_split = p4_selected_static_dynamic_attrs$input_data,
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
                                out_dir = '4_predict/out/random/pred_obs'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_train_test_CV_static_dynamic_png,
             barplot_compare_RF(mod = p4_train_RF_static_dynamic,
                                model_name = 'daily_SC_RF_static_dynamic',
                                pred_var = 'mean_value',
                                perf_metric = 'rmse',
                                out_dir = '4_predict/out/random/pred_obs'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_train_test_CV_min_static_png,
             barplot_compare_RF(mod = p4_train_RF_min_static,
                                model_name = 'daily_SC_RF_min_static',
                                pred_var = 'mean_value',
                                perf_metric = 'rmse',
                                out_dir = '4_predict/out/random/pred_obs'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_train_test_CV_min_static_dynamic_png,
             barplot_compare_RF(mod = p4_train_RF_min_static_dynamic,
                                model_name = 'daily_SC_RF_min_static_dynamic',
                                pred_var = 'mean_value',
                                perf_metric = 'rmse',
                                out_dir = '4_predict/out/random/pred_obs'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_train_test_CV_dynamic_png,
             barplot_compare_RF(mod = p4_train_RF_dynamic,
                                model_name = 'daily_SC_RF_dynamic',
                                pred_var = 'mean_value',
                                perf_metric = 'rmse',
                                out_dir = '4_predict/out/random/pred_obs'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_train_test_CV_static_dynamic_temporal_png,
             barplot_compare_RF(mod = p4_train_RF_static_dynamic_temporal,
                                model_name = 'daily_SC_RF_static_dynamic_temporal',
                                pred_var = 'mean_value',
                                perf_metric = 'rmse',
                                out_dir = '4_predict/out/temporal/pred_obs'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_train_test_CV_min_static_dynamic_temporal_png,
             barplot_compare_RF(mod = p4_train_RF_min_static_dynamic_temporal,
                                model_name = 'daily_SC_RF_min_static_dynamic_temporal',
                                pred_var = 'mean_value',
                                perf_metric = 'rmse',
                                out_dir = '4_predict/out/temporal/pred_obs'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  tar_target(p4_train_test_CV_dynamic_temporal_png,
             barplot_compare_RF(mod = p4_train_RF_dynamic_temporal,
                                model_name = 'daily_SC_RF_dynamic_temporal',
                                pred_var = 'mean_value',
                                perf_metric = 'rmse',
                                out_dir = '4_predict/out/temporal/pred_obs'),
             deployment = 'main',
             format = 'file',
             repository = 'local'
  ),
  
  # Spatial residuals aggregated over time
  # see note at top of 1_fetch.R re why this is a 'local' target
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
                      file_path = "4_predict/out/temporal/spatial_res/RF_static_dynamic_temporal",
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
                      file_path = "4_predict/out/temporal/spatial_res/RF_static_dynamic_temporal",
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
                      file_path = "4_predict/out/temporal/spatial_res/RF_min_static_dynamic_temporal",
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
                      file_path = "4_predict/out/temporal/spatial_res/RF_min_static_dynamic_temporal",
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
                      file_path = "4_predict/out/temporal/spatial_res/RF_dynamic_temporal",
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
                      file_path = "4_predict/out/temporal/spatial_res/RF_dynamic_temporal",
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
                   file_path = "4_predict/out/temporal/monthly_res/RF_static_dynamic_temporal",
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
                   file_path = "4_predict/out/temporal/monthly_res/RF_static_dynamic_temporal",
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
                   file_path = "4_predict/out/temporal/monthly_res/RF_min_static_dynamic_temporal",
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
                   file_path = "4_predict/out/temporal/monthly_res/RF_min_static_dynamic_temporal",
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
                   file_path = "4_predict/out/temporal/monthly_res/RF_dynamic_temporal",
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
                   file_path = "4_predict/out/temporal/monthly_res/RF_dynamic_temporal",
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
                   file_path = "4_predict/out/temporal/annual_res/RF_static_dynamic_temporal",
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
                   file_path = "4_predict/out/temporal/annual_res/RF_static_dynamic_temporal",
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
                   file_path = "4_predict/out/temporal/annual_res/RF_min_static_dynamic_temporal",
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
                   file_path = "4_predict/out/temporal/annual_res/RF_min_static_dynamic_temporal",
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
                   file_path = "4_predict/out/temporal/annual_res/RF_dynamic_temporal",
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
                   file_path = "4_predict/out/temporal/annual_res/RF_dynamic_temporal",
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
                    out_dir = "4_predict/out/temporal/temporal_res/RF_static_dynamic_temporal"),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_temporal_res_static_dynamic_temporal_test_png,
    plot_timeseries(pred_df = p4_pred_RF_static_dynamic_temporal_test$pred,
                    network_geometry = p1_reaches_sf,
                    model_name = 'RF_static_dynamic_temporal_test',
                    out_dir = "4_predict/out/temporal/temporal_res/RF_static_dynamic_temporal"),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_temporal_res_min_static_dynamic_temporal_png,
    plot_timeseries(pred_df = p4_pred_RF_min_static_dynamic_temporal$pred,
                    network_geometry = p1_reaches_sf,
                    model_name = 'RF_min_static_dynamic_temporal_full',
                    out_dir = "4_predict/out/temporal/temporal_res/RF_min_static_dynamic_temporal"),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_temporal_res_min_static_dynamic_temporal_test_png,
    plot_timeseries(pred_df = p4_pred_RF_min_static_dynamic_temporal_test$pred,
                    network_geometry = p1_reaches_sf,
                    model_name = 'RF_min_static_dynamic_temporal_test',
                    out_dir = "4_predict/out/temporal/temporal_res/RF_min_static_dynamic_temporal"),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_temporal_res_dynamic_temporal_png,
    plot_timeseries(pred_df = p4_pred_RF_dynamic_temporal$pred,
                    network_geometry = p1_reaches_sf,
                    model_name = 'RF_dynamic_temporal_full',
                    out_dir = "4_predict/out/temporal/temporal_res/RF_dynamic_temporal"),
    format = "file",
    repository = 'local'
  ),
  tar_target(
    p4_temporal_res_dynamic_temporal_test_png,
    plot_timeseries(pred_df = p4_pred_RF_dynamic_temporal_test$pred,
                    network_geometry = p1_reaches_sf,
                    model_name = 'RF_dynamic_temporal_test',
                    out_dir = "4_predict/out/temporal/temporal_res/RF_dynamic_temporal"),
    format = "file",
    repository = 'local'
  )
)
