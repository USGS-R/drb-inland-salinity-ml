split_data <- function(data, train_prop, time_lag = 0, by_time){
  #' 
  #' @description splits the data into training and testing by time
  #'
  #' @param data table of gages (rows) and features (columns). Must include
  #' COMID and GAGES_ID columns.
  #' @param train_prop proportion of the data to use for training
  #' @param time_lag lag used in initial_time_split
  #' @param by_time logical indicating if the split should be temporal or not
  #' 
  #' @return Returns a list of the dataset split, the training dataset and 
  #' the testing dataset
  
  if(by_time){
    split <- rsample::initial_time_split(data, prop = train_prop, lag = time_lag)
  }else{
    split <- rsample::initial_split(data, prop = train_prop)
  }
  training <- rsample::training(split)
  testing <- rsample::testing(split)
  
  return(list(split = split, training = training, testing = testing))
}

screen_Boruta <- function(input_data, pred_var, ncores, brf_runs, ntrees, 
                          train_prop, time_lag = 0, by_time){
  #' 
  #' @description Applies Boruta screening to the features. Makes a train/test
  #' split before applying the screening.
  #'
  #' @param input_data table of gages (rows) and columns with the pred_var and 
  #' features (columns)
  #' @param pred_var column name of the variable to predict
  #' @param ncores number of cores to use
  #' @param brf_runs maximum number of RF runs
  #' @param ntrees number of trees to use
  #' @param train_prop proportion of the data to use for training
  #' @param time_lag lag used in initial_time_split
  #' @param by_time logical indicating if the split should be temporal or not
  #' 
  #' @return Returns a list of all 3 brf models, and the input dataset 
  #' (IDs, features, metric) as a list with train/test splits as the list elements.
  
  #Split into training and testing datasets
  #May want to split reaches with continuous samplers separately
  input_data_split <- split_data(input_data, train_prop = train_prop, 
                                      time_lag = time_lag, by_time = by_time)
  
  #Apply Boruta to down-select features
  #This is parallelized by default
  #applying once to CAT only, then TOT only
  brf_noTOT <- Boruta(x = input_data_split$training %>% 
                        select(-{{pred_var}}, -starts_with('TOT_')) %>%
                        as.data.frame(),
                      y = input_data_split$training %>% 
                        pull({{pred_var}}),
                      pValue = 0.01,
                      mcAdj = TRUE,
                      maxRuns = brf_runs,
                      doTrace = 0,
                      holdHistory = TRUE,
                      getImp = getImpRfZ,
                      num.trees = ntrees,
                      oob.error = TRUE,
                      num.threads = ncores)
  
  brf_noCAT <- Boruta(x = input_data_split$training %>% 
                        select(-{{pred_var}}, -starts_with('CAT_')) %>%
                        as.data.frame(),
                      y = input_data_split$training %>% 
                        pull({{pred_var}}),
                      pValue = 0.01,
                      mcAdj = TRUE,
                      maxRuns = brf_runs,
                      doTrace = 0,
                      holdHistory = TRUE,
                      getImp = getImpRfZ,
                      num.trees = ntrees,
                      oob.error = TRUE,
                      num.threads = ncores)
  
  brf_All <- Boruta(x = input_data_split$training %>%
                      select(-{{pred_var}}) %>%
                      as.data.frame(),
                    y = input_data_split$training %>%
                      pull({{pred_var}}),
                    pValue = 0.01,
                    mcAdj = TRUE,
                    maxRuns = brf_runs,
                    doTrace = 0,
                    holdHistory = TRUE,
                    getImp = getImpRfZ,
                    num.trees = ntrees,
                    oob.error = TRUE,
                    num.threads = ncores)
  
  #Select all features that were not rejected over these 3 screenings
  names_unique = unique(c(names(brf_All$finalDecision[brf_All$finalDecision != 'Rejected']),
                          names(brf_noACC$finalDecision[brf_noACC$finalDecision != 'Rejected']),
                          names(brf_noCAT$finalDecision[brf_noCAT$finalDecision != 'Rejected'])
  ))
  
  #Create modeling dataset
  screened_input_data <- list(split = input_data_split$split,
                              training = input_data_split$training %>% 
                                select(all_of(names_unique), {{pred_var}}),
                              testing = input_data_split$testing %>% 
                                select(all_of(names_unique), {{pred_var}}))
  #correcting the split table separately so that the class of the split object
  #is correct.
  screened_input_data$split$data <- screened_input_data$split$data %>% 
    select(all_of(names_unique), {{pred_var}})
  
  # metric column name all 3 brf models and the input dataset (IDs, features, metric)
  return(list(metric = pred_var, brf_noCAT = brf_noCAT, brf_noTOT = brf_noTOT, 
              brf_All = brf_All, input_data = screened_input_data))
}

train_models_grid <- function(brf_output, v_folds, ncores){
  #' 
  #' @description optimizes hyperparameters using a grid search
  #'
  #' @param brf_output output of the screen_Boruta function
  #' @param v_folds number of cross validation folds to use
  #' @param ncores number of cores to use
  #' 
  #' @return Returns a list of the evaluated grid parameters, the 
  #' best fit parameters, and the workflow for those parameters.
  
  #Set the parameters to be tuned
  #Test with and without write.forest
  tune_spec <- rand_forest(mode = "regression",
                           mtry = tune(), 
                           min_n = tune(), 
                           trees = tune()) %>% 
    set_engine(engine = "ranger", 
               verbose = FALSE, importance = 'permutation', 
               probability = FALSE)
  
  #Set parameter ranges
  params <- parameters(list(mtry = mtry() %>% range_set(c(10,100)), 
                            min_n = min_n() %>% range_set(c(2,10)),
                            trees = trees() %>% range_set(c(500,2000))))
  
  #Space filled grid to search setup
  grid <- grid_max_entropy(params,
                           size = 100,
                           iter = 1000)
  
  #number of cross validation folds (v)
  cv_folds <- vfold_cv(data = brf_output$input_data$training, v = v_folds)
  
  #specify workflow to tune the grid
  wf <- workflow() %>%
    add_model(tune_spec) %>%
    add_variables(outcomes = contains(brf_output$metric),
                  predictors = !(contains(brf_output$metric)))
  
  #Find best model with a grid search over hyperparameters
  cl = parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  #Send variables to worker environments
  parallel::clusterExport(cl = cl, varlist = c('brf_output'), 
                          envir = environment())
  
  grid_result <- tune_grid(wf, 
                           resamples = cv_folds, 
                           grid = grid, 
                           metrics = metric_set(rmse, mae, rsq, rsq_trad),
                           control = control_grid(
                             verbose = TRUE,
                             allow_par = TRUE,
                             extract = NULL,
                             save_pred = FALSE,
                             pkgs = NULL,
                             save_workflow = FALSE,
                             event_level = "first",
                             parallel_over = "everything"
                           )
  )
  #refine hyperparameters with a Bayesian optimizaton
  #parallel::clusterExport(cl = cl, varlist = c('grid_result'), 
  #                        envir = environment())
  # Seems to be an error in the tune_Bayes function when initial is a previous
  # model run (extremely slow to get to step of generating candidates, and also
  # slow on that step). So, not using this method for now.
  # Bayes_result <- tune_bayes(wf,
  #                            resamples = cv_folds,
  #                            iter = 100,
  #                            metrics = metric_set(rmse, mae, rsq, rsq_trad),
  #                            initial = grid_result,
  #                            param_info = params,
  #                            objective = exp_improve(),
  #                            control = control_bayes(
  #                              verbose = TRUE,
  #                              no_improve = 10L,
  #                              uncertain = 5,
  #                              parallel_over = NULL
  #                            )
  # )
    
  best_grid_result <- select_best(grid_result, metric = "rmse")
  
  final_wf <- finalize_workflow(wf, best_grid_result)
  
  #Fit to all training data with best hyperparameters
  #test on testing dataset
  # would help to repeat over X random seeds to get model error
  final_fit <- last_fit(final_wf, 
                        split = brf_output$input_data$split,
                        metrics = metric_set(rmse, mae, rsq, rsq_trad)) 
  
  #test set performance metrics
  #collect_metrics(final_fit)
  
  #test set predictions
  #collect_predictions(final_fit)
  
  parallel::stopCluster(cl)
  
  return(list(grid_params = grid_result, 
              best_fit = final_fit, 
              workflow = final_wf))
}


predict_test_data <- function(model_wf, test_data, perf_metrics){
  #' 
  #' @description uses the provided model to predict on the test dataset and
  #' compute performance metrics
  #'
  #' @param model_wf model workflow containing a single model that will be used
  #' to predict on the test_data.
  #' @param test_data test dataset containing features and the metric to be predicted
  #' @param perf_metrics character vector of the yardstick performance metrics to use
  #' 
  #' @return Returns the predictions and the performance metrics
  
  preds <- predict(model_wf, new_data = test_data, type = 'numeric')
  
  perf_metrics <- metrics(preds, test_data$metric, perf_metrics)
  
  return(list(pred = preds, metrics = perf_metrics))
}