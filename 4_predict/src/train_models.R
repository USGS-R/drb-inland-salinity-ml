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

screen_Boruta <- function(input_data, drop_attrs = NULL, pred_var, ncores, brf_runs, 
                          ntrees, train_prop, time_lag = 0, by_time,
                          num_data_splits){
  #' 
  #' @description Applies Boruta screening to the features. Makes a train/test
  #' split before applying the screening.
  #'
  #' @param input_data table of gages (rows) and columns with the pred_var and 
  #' features (columns)
  #' @param drop_attrs attributes to not use in the Boruta screening (e.g., 
  #' the Date and PRMS segment ID)
  #' @param pred_var column name of the variable to predict
  #' @param ncores number of cores to use
  #' @param brf_runs maximum number of RF runs
  #' @param ntrees number of trees to use
  #' @param train_prop proportion of the data to use for training
  #' @param time_lag lag used in initial_time_split
  #' @param by_time logical indicating if the split should be temporal or not
  #' @param num_data_splits number of splits to make to the data. Boruta screening
  #' is applied to each split and the final set of attributes is the union of non-rejected
  #' attributes. The individual split results are returned in a list.
  #' 
  #' @return Returns a list of all brf models, and the input dataset 
  #' (IDs, features, metric) as a list with train/test splits as the list elements.
  
  if (num_data_splits > 1){
    #split the data into num_data_splits
    input_data$split <- 0
    sample_size <- floor(nrow(input_data)/num_data_splits)
    for (i in 1:num_data_splits){
      sample_split <- sample(x = 1:nrow(input_data[input_data$split == 0,]), 
                             size = sample_size, 
                             replace = FALSE)
      input_data$split[input_data$split == 0][sample_split] <- i
    }
  }else{
    input_data$split <- 1
  }
  
  #Lists and vector to store results
  brf_lst <- list()
  screened_input_data_lst <- list()
  names_unique_all_splits <- vector('character', length = 0L)
  
  #Loop over number of data splits
  for (i in 1:num_data_splits){
    #Split into training and testing datasets
    input_data_split <- split_data(input_data[input_data$split == i,] %>%
                                     select(-split), 
                                   train_prop = train_prop, 
                                   time_lag = time_lag, by_time = by_time)
    
    #Apply Boruta to down-select features
    #This is parallelized by default
    brf_All <- Boruta(x = input_data_split$training %>%
                        select(-{{pred_var}}, -all_of(drop_attrs)) %>%
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
    
    brf_lst <- c(brf_lst, list(brf_All))
    
    #Select all features that were not rejected
    names_unique <- names(brf_All$finalDecision[brf_All$finalDecision != 'Rejected'])
    
    #Create modeling dataset for this iteration
    screened_input_data <- list(split = input_data_split$split)
    
    screened_input_data_lst <- c(screened_input_data_lst, list(screened_input_data))
    
    #vector of the unique features from all data splits
    names_unique_all_splits <- unique(c(names_unique_all_splits, names_unique))
  }
  
  save.image("post-Boruta.RData")
  
  #Aggregate the input data into a single dataframe for model training and testing
  for (i in 1:num_data_splits){
    if(i == 1){
      screened_input_data_aggregated <- screened_input_data_lst[[i]]$split
    }else{
      #Relabel the training data ID
      screened_input_data_aggregated$in_id <- c(screened_input_data_aggregated$in_id,
                                                nrow(screened_input_data_aggregated$data) + 
                                                  screened_input_data_lst[[i]]$split$in_id)
      #Aggregate dataframes
      screened_input_data_aggregated$data <- rbind(screened_input_data_aggregated$data,
                                     screened_input_data_lst[[i]]$split$data)
    }
    if (i == num_data_splits){
      if (nrow(input_data[input_data$split == 0,]) > 0){
        #add these data to the dataset
        screened_input_data_aggregated$data <- rbind(screened_input_data_aggregated$data,
                                                     input_data[input_data$split == 0,])
      }
    }
  }
  
  #Create modeling dataset
  screened_input_data_aggregated <- list(split = screened_input_data_aggregated,
                              training = screened_input_data_aggregated$data[screened_input_data_aggregated$in_id,],
                              testing = screened_input_data_aggregated$data[-screened_input_data_aggregated$in_id,])
  
  # metric column name, all brf models, all input datasets (IDs, features, metric),
  # selected features, and the combined input dataset
  return(list(metric = pred_var, brf = brf_lst, input_data_lst = screened_input_data_lst,
              selected_features = names_unique_all_splits,
              input_data = screened_input_data_aggregated))
}

select_attrs <- function(brf_output, retain_attrs = NULL){
  #' 
  #' @description Applies Boruta screening to the features. Makes a train/test
  #' split before applying the screening.
  #'
  #' @param brf_output output from the screen_Boruta function
  #' @param retain_attrs character vector of additional attributes to retain
  #' beyond those in brf_output$selected_features.
  #' 
  #' @return Returns a list of all brf models, and the input dataset 
  #' (IDs, features, metric) as a list with train/test splits as the list elements.
  #' The training and testing dataframes only contain the selected attributes.
  
  #vector of attributes to retain
  retain_attrs <- unique(c(brf_output$selected_features, retain_attrs, brf_output$metric))
  
  #retaining those attributes in the split, training and testing datasets
  brf_output$input_data$split$data <- brf_output$input_data$split$data %>% 
    select(all_of(retain_attrs))
  brf_output$input_data$training <- brf_output$input_data$training %>% 
    select(all_of(retain_attrs))
  brf_output$input_data$testing <- brf_output$input_data$testing %>% 
    select(all_of(retain_attrs))
  
  return(brf_output)
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
               verbose = FALSE, importance = 'impurity', 
               probability = FALSE)
  
  #Set parameter ranges
  params <- parameters(list(mtry = mtry() %>% range_set(c(10,100)), 
                            min_n = min_n() %>% range_set(c(2,10)),
                            trees = trees() %>% range_set(c(200,2000))))
  
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