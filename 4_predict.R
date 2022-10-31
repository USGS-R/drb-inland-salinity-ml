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
                                  drop_columns_contains = c(#Keeping shallow and deep soil info. Dropping middle 2.
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
                                  drop_columns_select = c(#Correlated with density of NPDES sites
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
                                 id_cols = c('PRMS_segid', 'Date'),
                                 temporal = TRUE)
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
                                 id_cols = c('PRMS_segid', 'Date'), 
                                 temporal = TRUE)
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
                                 id_cols = c('PRMS_segid', 'Date'), 
                                 temporal = TRUE)
             },
             deployment = 'worker'
  ),
  
  # Refresh AWS credentials
  tar_target(
    p4_aws_credentials_9,
    generate_credentials(dummy_var = p4_train_RF_dynamic_temporal),
    deployment = 'main',
    cue = tar_cue('always')
  ),
  
  
  #RF Predictions
  #Static features, full dataset
  tar_target(p4_pred_RF_static,
             {
               #Correct workflow problem
               wf <- extract_workflow(p4_train_RF_static$best_fit)
               data <- p4_train_RF_static$best_fit$splits[[1]]$data
               data$Date <- p4_Boruta_static$input_data$split$data$Date
               data$PRMS_segid <- p4_Boruta_static$input_data$split$data$PRMS_segid
               predict_test_data(model_wf = wf,
                                 test_data = data,
                                 target_name = 'mean_value',
                                 train_ind = p4_train_RF_static$best_fit$splits[[1]]$in_id) 
             },
             deployment = 'main'
  ),
  #Static features, test dataset
  tar_target(p4_pred_RF_static_test,
             {
               #Correct workflow problem
               wf <- extract_workflow(p4_train_RF_static$best_fit)
               data <- p4_train_RF_static$best_fit$splits[[1]]$data[-p4_train_RF_static$best_fit$splits[[1]]$in_id,]
               data$Date <- p4_Boruta_static$input_data$testing$Date
               data$PRMS_segid <- p4_Boruta_static$input_data$testing$PRMS_segid
               predict_test_data(model_wf = wf,
                                 test_data = data,
                                 target_name = 'mean_value')
             },
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
  )
)