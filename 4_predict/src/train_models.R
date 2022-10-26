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
    #ensure that data are arranged by date
    data <- arrange(data, Date)
    split <- rsample::initial_time_split(data, prop = train_prop, lag = time_lag)
    #reallocate samples to training and testing because initial_time_split may miss
    #some reaches that have data on the train_prop-th date.
    max_Date <- max(split$data[split$in_id,]$Date)
    split$in_id <- which(split$data$Date <= max_Date)
    split$out_id <- which(split$data$Date > max_Date)
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
                                                     input_data[input_data$split == 0,] %>%
                                                       select(colnames(screened_input_data_aggregated$data))
                                                     )
      }
    }
  }
  
  #Create modeling dataset
  screened_input_data_aggregated <- list(split = screened_input_data_aggregated,
                              training = screened_input_data_aggregated$data[screened_input_data_aggregated$in_id,],
                              testing = screened_input_data_aggregated$data[-screened_input_data_aggregated$in_id,])
  
  #regenerate AWS credentials
  generate_credentials()
  
  # metric column name, all brf models, all input datasets (IDs, features, metric),
  # selected features, and the combined input dataset
  return(list(metric = pred_var, brf = brf_lst, input_data_lst = screened_input_data_lst,
              selected_features = names_unique_all_splits,
              input_data = screened_input_data_aggregated))
}

select_attrs <- function(brf_output, retain_attrs = NULL){
  #' 
  #' @description Reduces the attributes in the brf_output table to those
  #' that were selected from the Boruta algorithm and specified in the
  #' retain_attrs.
  #'
  #' @param brf_output output from the screen_Boruta function
  #' @param retain_attrs character vector of additional attributes to retain
  #' beyond those in brf_output$selected_features.
  #' 
  #' @return Returns brf_output with only the selected attributes.
  
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

make_temporal_split <- function(attrs_df, train_prop){
  #' 
  #' @description creates temporal splits for the training and testing dataset.
  #'
  #' @param attrs_df output from select_attrs
  #' @param train_prop proportion of the data to use as training.
  #' 
  #' @return Returns attrs_df with an updated training and testing dataset based
  #' on time.
  
  new_split <- split_data(data = attrs_df$input_data$split$data, 
                          train_prop = train_prop,
                          by_time = TRUE)
  
  attrs_df$input_data <- new_split
  
  return(attrs_df)
}

train_models_grid <- function(brf_output, v_folds, ncores,
                              range_mtry, range_minn, range_trees,
                              gridsize, id_cols, temporal = FALSE){
  #' 
  #' @description optimizes hyperparameters using a grid search
  #'
  #' @param brf_output output of the screen_Boruta function
  #' @param v_folds number of cross validation folds to use
  #' @param ncores number of cores to use
  #' @param range_mtry 2-element numeric vector for min and max values of mtry
  #' to use within ranger random forest
  #' @param range_minn 2-element numeric vector for min and max values of min_n
  #' to use within ranger random forest
  #' @param range_trees 2-element numeric vector for min and max values of trees
  #' to use within ranger random forest
  #' @param gridsize numeric number of points to evaluate within the 3D grid
  #' @param id_cols vector of column names that are IDs (e.g., segment ID, Date) 
  #' and not used to predict
  #' @param temporal logical indicating if the cross validation should be completed
  #' using a temporal holdout instead of a random holdout
  #' 
  #' @return Returns a list of the evaluated grid parameters, the 
  #' best fit parameters, and the workflow for those parameters.
  
  #Set the parameters to be tuned
  threads <- floor((ncores - v_folds)/v_folds)
  
  tune_spec <- rand_forest(mode = "regression",
                           mtry = tune(), 
                           min_n = tune(), 
                           trees = tune()) %>% 
    set_engine(engine = "ranger", 
               verbose = FALSE, importance = 'impurity', 
               probability = FALSE, num.threads = threads,
               keep.inbag = TRUE)
  
  #Set parameter ranges
  params <- parameters(list(mtry = mtry() %>% range_set(range_mtry), 
                            min_n = min_n() %>% range_set(range_minn),
                            trees = trees() %>% range_set(range_trees)))
  
  #Space filled grid to search setup
  grid <- grid_max_entropy(params,
                           size = gridsize,
                           #iterations to make optimal allocation of gridsize 
                           #as a space filling approach
                           iter = 1000)
  
  #number of cross validation folds (v)
  if(temporal){
    #add group variable to dataset based on an even temporal split
    brf_output$input_data$training$group <- 0
    brf_output$input_data$testing$group <- NA
    brf_output$input_data$split[[1]]$group <- NA
    for (i in 1:v_folds){
      if(i == v_folds){
        inds_grp <- which(brf_output$input_data$training$group == 0)
      }else{
        inds_grp <- split_data(brf_output$input_data$training, 
                               train_prop = i/v_folds, time_lag = 0, 
                               by_time = TRUE)$split$in_id
      }
      #assign group index only to rows that have not yet been assigned a group index
      brf_output$input_data$training$group[inds_grp][brf_output$input_data$training$group[inds_grp] == 0] <- i
    }
    
    #add group to the id_cols vector so it's not used for prediction
    id_cols <- c(id_cols, 'group')
    
    #make 1 fold per group
    cv_folds <- group_vfold_cv(data = brf_output$input_data$training, group = 'group')
  }else{
    cv_folds <- vfold_cv(data = brf_output$input_data$training, v = v_folds)
  }
  
  #specify workflow to tune the grid
  target_name <- brf_output$metric
  wf <- workflow() %>%
    add_model(tune_spec) %>%
    add_variables(outcomes = target_name,
                  predictors = !(all_of(c(id_cols))))
  
  #Find best model with a grid search over hyperparameters
  cl = parallel::makeCluster(ncores)
  doParallel::registerDoParallel(cl)
  #Send variables to worker environments
  parallel::clusterExport(cl = cl, varlist = c('target_name', 'id_cols', 'threads'), 
                          envir = environment())
  
  grid_result <- tune_grid(wf, 
                           resamples = cv_folds, 
                           grid = grid, 
                           metrics = metric_set(rmse, mae, rsq, rsq_trad),
                           control = control_grid(
                             verbose = FALSE,
                             allow_par = TRUE,
                             extract = NULL,
                             save_pred = FALSE,
                             pkgs = NULL,
                             save_workflow = FALSE,
                             event_level = "first",
                             parallel_over = "resamples"
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
  
  #extract workflow for best hyperparameters
  final_wf_trained <- extract_workflow(final_fit)
  
  parallel::stopCluster(cl)
  
  #remove data from grid_result to reduce file size
  grid_result$splits <- NULL
  
  #regenerate AWS credentials
  generate_credentials()
  
  return(list(grid_params = grid_result, 
              best_fit = final_fit, 
              workflow = final_wf_trained))
}


predict_test_data <- function(model_wf, test_data, target_name,
                              train_ind = NULL){
  #' 
  #' @description uses the provided model to predict on the test dataset and
  #' compute performance metrics
  #'
  #' @param model_wf model workflow containing a single model that will be used
  #' to predict on the test_data.
  #' @param test_data test dataset containing features and the metric to be predicted
  #' @param target_name character string of the column name in test_data to use
  #' as the target variable to be predicted
  #' @param train_ind indicator for which rows in test_data are the training data.
  #' A column with this indicator will be appended to the output file.
  #' 
  #' @return Returns the predictions and the performance metrics
  
  preds <- predict(model_wf, new_data = test_data, type = 'numeric') %>%
    mutate(obs = test_data[[target_name]])
  
  perf_metrics <- metrics(data = preds, truth = 'obs', estimate = '.pred')
  
  #Add PRMS segment and Date to results
  preds <- cbind(test_data[,c('PRMS_segid', 'Date')], preds)
  #Add squared error so that it can later be grouped by PRMS segid mean
  preds$err <- preds$.pred - preds$obs
  preds$errsq <- (preds$err)^2
  #Add month and year for use in other functions
  preds$Month <- month(preds$Date)
  preds$Year <- year(preds$Date)
  
  #Add training data indicator
  if(!is.null(train_ind)){
    preds$training <- 0
    preds$training[train_ind] <- 1
  }
  
  return(list(target = target_name, pred = preds, metrics = perf_metrics))
}


filter_rows_date <- function(attrs, start_date){
  #' 
  #' @description filters attrs for rows >= start_date and corrects the training
  #' and testing datasets to match that filter
  #'
  #' @param attrs dataframe to be filtered
  #' @param start_date desired start date as character string
  #' 
  #' @return Returns filtered attrs
  
  #Remove data before the start_date
  attrs$input_data$training <- filter(attrs$input_data$training, Date >= as.Date(start_date))
  attrs$input_data$testing <- filter(attrs$input_data$testing, Date >= as.Date(start_date))
  attrs$input_data$split$data <- filter(attrs$input_data$split$data, Date >= as.Date(start_date))
  #Correct the in_id (training data IDs)
  attrs$input_data$split$in_id <- which(!is.na(left_join(x = attrs$input_data$split$data, 
                                                         y = attrs$input_data$training, 
                                                         by = c('PRMS_segid', 'Date'))$mean_value.y))
  #Correct the out_id (testing data IDs)
  attrs$input_data$split$out_id <- (1:nrow(attrs$input_data$split$data))[-which(attrs$input_data$split$in_id %in% 1:nrow(attrs$input_data$split$data))]
  
  return(attrs)
}


#SHAP values
compute_shap <- function(model, data, ncores, nsim){
  #' 
  #' @description computes SHAP values
  #'
  #' @param model the model to be used
  #' @param data the dataset with predictor attributes
  #' @param ncores number of parallel cores to use
  #' @param nsim number of replicates to run (more = better SHAP estimates)
  #' 
  #' @return Returns a dataframe of SHAP values

  cl <- parallel::makeCluster(ncores, outfile = "")
  doParallel::registerDoParallel(cl)
  parallel::clusterExport(cl = cl, varlist = c('nsim'), 
                          envir = environment())
  parallel::clusterExport(cl = cl, varlist = c('predict_shap_data'))
  
  shap <- fastshap::explain(object = model, 
                            X = data, 
                            pred_wrapper = predict_shap_data, 
                            nsim = nsim, 
                            .parallel = TRUE, 
                            .inform = TRUE,
                            .paropts = list(.packages = c('tidyverse', 'tidymodels')))
  
  #For some reason, I cannot suppress the warnings of this function. It throws
  #an error with that tacked on. So, I'm allowing this warning to appear for now.
  #suppressing <anonymous>: ... may be used in an incorrect context: ‘.fun(piece, ...)’
  
  parallel::stopCluster(cl)
  
  return(shap)
}


predict_shap_data <- function(object, newdata){
  #' 
  #' @description uses the provided model to predict on the test dataset.
  #'
  #' @param object model workflow containing a single model that will be used
  #' to predict on the test_data.
  #' @param newdata dataset containing features and the metric to be predicted
  #' 
  #' @return Returns a vector of predictions
  
  preds <- predict(object, newdata, type = 'numeric') %>% 
    pull(.pred)
  
  return(preds)
}


get_maxcores_by_RAM <- function(RAM_per_core, RAM_avail = NULL){
  #' 
  #' @description determines the maximum number of cores to use in a parallel
  #' job based on the available RAM. Developed on Linux machines.
  #'
  #' @param RAM_per_core RAM in GB that is needed per core. Estimated from a 
  #' test run.
  #' @param RAM_avail optional specification of available RAM in GB. If not provided,
  #' then an estimate will be made by detecting available RAM.
  #' 
  #' @return Returns the maximum cores as an integer
  
  if(is.null(RAM_avail)){
    RAM_avail <- system2('free', args='-m', stdout=TRUE)
    RAM_avail <- strsplit(RAM_avail[2], " +")[[1]][4] %>%
      as.numeric()
    
    #Convert to GB
    RAM_avail <- RAM_avail/2^10
  }
  
  maxcores <- floor(RAM_avail / RAM_per_core)
  
  return(maxcores)
}
