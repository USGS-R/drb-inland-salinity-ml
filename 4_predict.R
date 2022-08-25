source("4_predict/src/train_models.R")
source("4_predict/src/plot_diagnostics.R")

#Predict phase
p4_targets_list <- list(
  #Boruta screening - 5 year lag attributes
  tar_target(p4_Boruta_5yr,
             screen_Boruta(input_data = p2_all_attr_SC_obs %>% 
                             drop_na(mean_value, contains('5years')) %>%
                             #remove unused columns
                             select(-c("PRMS_segid","Date", "min_value", 
                                       "max_value", "n_value", "sd_value", 
                                       "cv_value", "site_ids")),
                           pred_var = 'mean_value',
                           ncores = Boruta_cores, 
                           brf_runs = Boruta_runs, 
                           ntrees = Boruta_trees,
                           train_prop = 0.8,
                           by_time = FALSE
             ),
             deployment = 'worker'
  ),
  #RF train
  tar_target(p4_train_RF,
             train_models_grid(brf_output = p4_Boruta,
                               ncores = Boruta_cores,
                               v_folds = cv_folds
             ),
             deployment = 'worker'
  ),
  
  # Visualize Model Diagnostics:
  
  # Boruta screening
  tar_target(p4_Boruta_png,
             plot_Boruta(p4_Boruta$brf_All,
                         model_name = 'daily_SC_RF',
                         out_dir = '6_predict/out/Boruta'),
             deployment = 'main',
             format = 'file'
  ),
  
  # RF variable importance plot 
  #Should add error bars over X random seeds
  tar_target(p4_vip_png,
             plot_vip(RF_model = p4_train_RF$best_fit,
                      model_name = 'daily_SC_RF',
                      num_features = 20,
                      out_dir = '6_predict/out/vip'),
             deployment = 'main',
             format = 'file'
  ),
  
  # RF hyperparameter optimization
  tar_target(p4_hypopt_png,
             plot_hyperparam_opt_results_RF(p4_train_RF$grid_params,
                                            model_name = 'daily_SC_RF',
                                            out_dir = '6_predict/out/hypopt'),
             deployment = 'main',
             format = 'file'
  ),
  tar_target(p4_hypopt_marginals_png,
             plot_hyperparam_opt_marginals(p4_train_RF$grid_params,
                                           model_name = 'daily_SC_RF',
                                           plt_type = "marginals",
                                           perf_metric = NULL,
                                           out_dir = '6_predict/out/hypopt'),
             deployment = 'main',
             format = 'file'
  ),
  
  # RF predicted vs. observed y 
  #Should be for the mean over X random seeds
  tar_target(p4_pred_obs_png,
             plot_pred_obs(df_pred_obs = p4_train_RF$best_fit,
                           model_name = 'daily_SC_RF',
                           out_dir = '6_predict/out/pred_obs'),
             deployment = 'main',
             format = 'file'
  ),
  
  #Train test barplot coverage of SC
  tar_target(p4_train_test_boxplot_coverage_png,
             boxplot_compare_RF(data_split = p4_train_RF$best_fit$input_data,
                           model_name = 'daily_SC_RF',
                           pred_var = 'SC',
                           out_dir = '6_predict/out/pred_obs'),
             deployment = 'main',
             format = 'file'
  ),
  
  #Cross validation coverage
  tar_target(p4_train_test_CV_png,
             boxplot_compare_RF(mod = p4_train_RF$wf,
                                pred_var = 'SC',
                                perf_metric = 'rmse',
                                out_dir = '6_predict/out/'),
             deployment = 'main',
             format = 'file'
  )
  
  # Spatial residuals
  #Should be for the mean over X random seeds
  
  
  # Model RMSE comparison boxplots / barplots
)


# brf <- Boruta(x = as.data.frame(input_data[,-1]), 
#        y = input_data$mean_value, 
#        pValue = 0.01, 
#        maxRuns = 11, 
#        doTrace = 0, 
#        holdHistory = TRUE, 
#        getImp = getImpRfZ, 
#        num.trees = 500,
#        oob.error = FALSE, 
#        num.threads = 15)