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

make_spatial_split <- function(attrs, train_prop){
  #' 
  #' @description creates spatial splits for the training and testing dataset.
  #'
  #' @param attrs output from select_attrs. Must have "PRMS_segid" and 
  #' "data_type" columns (characters). Splits are stratified by the data_type
  #' (proportional amount of 'u' and 'd' reaches in each split)
  #' @param train_prop proportion of the reaches to use as training. The number
  #' of observations in repeated runs of this function can differ because each
  #' reach has a different number of observations.
  #' 
  #' @return Returns attrs with an updated training and testing dataset based
  #' on the data_type (spatial split).
  
  #get indicator for if a reach has any continuous data ('u') or not ('d')
  reach_data_type <- summarize(group_by(attrs$input_data$split$data, PRMS_segid), 
                               data_type = max(data_type))
  
  #get a split of the data by reach using strata sampling 
  reach_split <- rsample::initial_split(data = reach_data_type,
                                        prop = train_prop, 
                                        strata = data_type)
  
  #get the attrs into a training and testing split based on the reach_split
  training_reaches <- reach_split$data$PRMS_segid[reach_split$in_id]
  
  #training indices
  ind_train <- which(attrs$input_data$split$data$PRMS_segid %in% training_reaches)
  ind_test <- which(!(attrs$input_data$split$data$PRMS_segid %in% training_reaches))
  
  training <- attrs$input_data$split$data[ind_train,]
  testing <- attrs$input_data$split$data[ind_test,]
  
  #get the split list into expected format
  split <- reach_split
  split$data <- attrs$input_data$split$data
  split$in_id <- ind_train
  split$out_id <- ind_test
  
  attrs$input_data <- list(split = split, training = training, testing = testing)
  
  return(attrs)
}
make_spatial_split_CVtraining <- function(attrs_df, train_prop){
  #' 
  #' @description creates spatial splits for cross validation groups.
  #'
  #' @param attrs_df training dataframe to be split. Must have "PRMS_segid" and 
  #' "data_type" columns (characters). Splits are stratified by the data_type
  #' (proportional amount of 'u' and 'd' reaches in each split)
  #' @param train_prop proportion of the reaches to use as training. The number
  #' of observations in repeated runs of this function can differ because each
  #' reach has a different number of observations.
  #' 
  #' @return Returns the indicies to use for CV group labels
  
  #get indicator for if a reach has any continuous data ('u') or not ('d')
  reach_data_type <- summarize(group_by(attrs_df, PRMS_segid), 
                               data_type = max(data_type))
  
  #get a split of the data by reach using strata sampling 
  reach_split <- rsample::initial_split(data = reach_data_type,
                                        prop = train_prop, 
                                        strata = data_type)
  
  #get the attrs into a training and testing split based on the reach_split
  training_reaches <- reach_split$data$PRMS_segid[reach_split$in_id]
  
  #training indices
  ind_train <- which(attrs_df$PRMS_segid %in% training_reaches)
  
  return(ind_train)
}
assign_spatial_split <- function(attrs, split_template_testing){
  #' 
  #' @description creates spatial splits for the training and testing dataset
  #' based on a provided split_template.
  #'
  #' @param attrs output from select_attrs. Must have "PRMS_segid" and 
  #' "data_type" columns (characters). Splits are stratified by the data_type
  #' (proportional amount of 'u' and 'd' reaches in each split)
  #' @param split_template_testing test set from make_spatial_split. This test
  #' set will also be used for the attrs. Must have PRMS_segid column.
  #' 
  #' @return Returns attrs with an updated training and testing dataset based
  #' on the split_template_testing.
  
  #training indices
  ind_test <- which(attrs$input_data$split$data$PRMS_segid %in% split_template_testing$PRMS_segid)
  ind_train <- which(!(attrs$input_data$split$data$PRMS_segid %in% split_template_testing$PRMS_segid))
  
  training <- attrs$input_data$split$data[ind_train,]
  testing <- attrs$input_data$split$data[ind_test,]
  
  #get the split list into expected format
  split <- attrs$input_data$split
  split$in_id <- ind_train
  split$out_id <- ind_test
  
  attrs$input_data <- list(split = split, training = training, testing = testing)
  
  return(attrs)
}

train_models_grid <- function(brf_output, v_folds, ncores,
                              range_mtry, range_minn, range_trees,
                              gridsize, id_cols, temporal = FALSE,
                              spatial = FALSE){
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
  #' @param spatial logical indicating if the cross validation should be completed
  #' using a spatial holdout instead of a random holdout
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
  }else if (spatial){
    #add group variable to dataset based on spatial splits
    brf_output$input_data$training$group <- 0
    brf_output$input_data$testing$group <- NA
    brf_output$input_data$split[[1]]$group <- NA
    for (i in 1:v_folds){
      if(i == v_folds){
        ind_i <- which(brf_output$input_data$training$group == 0)
        inds_grp <- seq(1,length(ind_i),1)
      }else{
        #only use data that haven't been assigned to a group
        ind_i <- which(brf_output$input_data$training$group == 0)
        #get the correct training proportion to use for a reduced dataset
        train_prop_i <- nrow(brf_output$input_data$training)/v_folds/nrow(brf_output$input_data$training[ind_i,])
        #group row indices
        inds_grp <- make_spatial_split_CVtraining(attrs_df = brf_output$input_data$training[ind_i,], 
                                                  train_prop = train_prop_i)
      }
      #assign group index
      brf_output$input_data$training$group[ind_i][inds_grp] <- i
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
setup_shap_data <- function(data, seasonal = TRUE, physio = TRUE, lulc = TRUE,
                            lulc_prop = 0.75, lulc_data = NULL, physio_data = NULL){
  #' @description prepares data for SHAP computation based on the provided split
  #'
  #' @param data the dataset with predictor attributes
  #' @param seasonal logical for splitting data by water year season
  #' @param physio logical for splitting data by physiographic region
  #' @param lulc logical for splitting data by lulc, as defined into 2 groups:
  #' high urban land cover and high forest land cover
  #' @param lulc_prop threshold defining "high" proportion land cover on [0,1]
  #' @param lulc_data land cover data for total upstream urban and forest. Must
  #' also have the PRMS_segid and Date columns
  #' @param physio_data physiographic region data. 
  #' Must also have the PRMS_segid and Date columns
  #' 
  #' @return Returns a list with the different data splits
  
  data_lst <- list()
  data_lst_names <- c()
  
  if(seasonal){
    #split data by water year season (4 elements in list)
    data$months <- lubridate::month(data$Date)
    OND <- filter(data, months %in% c(10,11,12)) %>%
      select(PRMS_segid, Date)
    JFM <- filter(data, months %in% c(1,2,3)) %>%
      select(PRMS_segid, Date)
    AMJ <- filter(data, months %in% c(4,5,6)) %>%
      select(PRMS_segid, Date)
    JAS <- filter(data, months %in% c(7,8,9)) %>%
      select(PRMS_segid, Date)
    
    data_lst <- c(data_lst, list(OND), list(JFM), list(AMJ), list(JAS))
    #names for list elements
    data_lst_names <- c(data_lst_names, paste0('seas_', c('OND', 'JFM', 'AMJ', 'JAS')))
    
    if(lulc){
      #split each season by lulc
      ind_seasons <- grep('^seas_', data_lst_names)
      for(i in 1:4){
        data_s <- left_join(data_lst[[ind_seasons[i]]], 
                            lulc_data, by = c('PRMS_segid', 'Date'))
        
        high_forest <- filter(data_s, forest >= lulc_prop) %>%
          select(PRMS_segid, Date)
        high_urban <- filter(data_s, midurban + lowurban >= lulc_prop) %>%
          select(PRMS_segid, Date)
        
        data_lst <- c(data_lst, list(high_forest), list(high_urban))
        #name list elements
        data_lst_names <- c(data_lst_names, paste0('seaslc_', data_lst_names[ind_seasons[i]], 
                                                   '_', c('highForest', 'highUrban')))
      }
    }
    
    if(physio){
      #split each season by physiographic region
      ind_seasons <- grep('^seas_', data_lst_names)
      for(i in 1:4){
        data_s <- left_join(data_lst[[ind_seasons[i]]], 
                            physio_data, by = c('PRMS_segid', 'Date'))
        
        #appalachian plateau
        AP <- filter(data_s, AP == 1) %>%
          select(PRMS_segid, Date)
        #coastal plain
        CP <- filter(data_s, CP == 1) %>%
          select(PRMS_segid, Date)
        #all other (interior)
        IN <- filter(data_s, BR == 1 | VR == 1 | PD == 1) %>%
          select(PRMS_segid, Date)
        
        data_lst <- c(data_lst, list(AP), list(CP), list(IN))
        
        #name list elements
        data_lst_names <- c(data_lst_names, paste0('seasphysio_', data_lst_names[ind_seasons[i]], 
                                                   '_', c('AP', 'CP', 'IN')))
      }
    }
    #drop season data
    data <- select(data, -months)
  }
  
  if(lulc){
    #split by lulc
    data <- left_join(data, lulc_data, by = c('PRMS_segid', 'Date'))
    
    high_forest <- filter(data, forest >= lulc_prop) %>%
      select(PRMS_segid, Date)
    high_urban <- filter(data, midurban + lowurban >= lulc_prop) %>%
      select(PRMS_segid, Date)
    
    data_lst <- c(data_lst, list(high_forest), list(high_urban))
    #name list elements
    data_lst_names <- c(data_lst_names, paste0('lc_', c('highForest', 'highUrban')))
    
    #drop lulc data
    data <- select(data, -lowurban, -midurban, -forest)
  }
  
  if(physio){
    #split by physiographic region
    data <- left_join(data, physio_data, by = c('PRMS_segid', 'Date'))
    
    #appalachian plateau
    AP <- filter(data, AP == 1) %>%
      select(PRMS_segid, Date)
    #coastal plain
    CP <- filter(data, CP == 1) %>%
      select(PRMS_segid, Date)
    #all other (interior)
    IN <- filter(data, BR == 1 | VR == 1 | PD == 1) %>%
      select(PRMS_segid, Date)
    
    data_lst <- c(data_lst, list(AP), list(CP), list(IN))
    #name list elements
    data_lst_names <- c(data_lst_names, paste0('physio_', c('AP', 'CP', 'IN')))
    
    #drop physio data
    data <- select(data, -AP, -BR, -VR, -CP, -PD)
  }
  
  #name list elements
  names(data_lst) <- data_lst_names
  
  return(data_lst)
}

get_shap_subset <- function(split, shap, split_name){
  #' 
  #' @description filters the SHAP dataframe based on the split PRMS_segid and Date
  #'
  #' @param split dataframe with 2 columns: PRMS_segid and Date
  #' @param shap SHAP dataframe with class explain. This will be filtered.
  #' @param split_name the name of the split
  #' 
  #' @return Returns a filtered dataframe of SHAP values with the explain class
  
  #get the row indices for this split
  #because shap has a special class, get the row inds, then filter, then drop the ind column
  shap$ind <- seq(1, nrow(shap), 1)
  sample_inds <- left_join(split, shap,
                           by = c('PRMS_segid', 'Date')) %>%
    #the data in the split may not be in the model dataset. Drop NA data
    drop_na()
  
  sample <- shap[shap$ind %in% sample_inds$ind, 
                 -which(colnames(shap) == 'ind')]
  
  #add the name of the list to this and create a list
  lst <- c(list(name = split_name), list(shap = sample))
  
  return(lst)
}

get_shap_dir <- function(main_dir, subdir_name){
  #' 
  #' @description returns the full path to the storage directory based on a main directory
  #' and the name of a data split that will be used to find the corresponding subdirectory
  #'
  #' @param main_dir character string providing the path to the contents that is fixed for
  #' all SHAP computations
  #' @param subdir_name the name of the data split used to compute the SHAP values. The names
  #' will be in the format of seas_, seaslc_, seasphysio_, lc_, or physio_
  #' 
  #' @return Returns a filtered dataframe of SHAP values with the explain class
  
  #get the text before the _
  category <- str_split(pattern = '_', string = subdir_name, simplify = TRUE)[1]

  if(category == 'seas'){
    sub_category <- str_split(pattern = '_', string = subdir_name, simplify = TRUE)[2]
    filepath <- file.path(main_dir, 'seasonal', sub_category)
  }else if(category == 'seaslc'){
    sub_category1 <- str_split(pattern = '_', string = subdir_name, simplify = TRUE)[2]
    sub_category2 <- str_split(pattern = '_', string = subdir_name, simplify = TRUE)[3]
    if(sub_category2 == 'highUrban'){
      filepath <- file.path(main_dir, 'seasonal', sub_category1, 'high_urban')
    }else if(sub_category2 == 'highForest'){
      filepath <- file.path(main_dir, 'seasonal', sub_category1, 'high_forest')
    }else{
      stop('land cover class is not highUrban or highForest')
    }
  }else if(category == 'seasphysio'){
    sub_category1 <- str_split(pattern = '_', string = subdir_name, simplify = TRUE)[2]
    sub_category2 <- str_split(pattern = '_', string = subdir_name, simplify = TRUE)[3]
    if(sub_category2 == 'IN'){
      filepath <- file.path(main_dir, 'seasonal', sub_category1, 'interior')
    }else if(sub_category2 == 'AP'){
      filepath <- file.path(main_dir, 'seasonal', sub_category1, 'appalachian')
    }else if(sub_category2 == 'CP'){
      filepath <- file.path(main_dir, 'seasonal', sub_category1, 'coastal')
    }else{
      stop('physio class is not IN, AP, or CP')
    }
    filepath <- file.path(main_dir, 'seasonal', sub_category1, sub_category2)
  }else if(category == 'lc'){
    sub_category1 <- str_split(pattern = '_', string = subdir_name, simplify = TRUE)[2]
    if(sub_category1 == 'highUrban'){
      filepath <- file.path(main_dir, 'lulc', 'high_urban')
    }else if(sub_category1 == 'highForest'){
      filepath <- file.path(main_dir, 'lulc', 'high_forest')
    }else{
      stop('land cover class is not highUrban or highForest')
    }
  }else if(category == 'physio'){
    sub_category1 <- str_split(pattern = '_', string = subdir_name, simplify = TRUE)[2]
    if(sub_category1 == 'IN'){
      filepath <- file.path(main_dir, 'physio', 'interior')
    }else if(sub_category1 == 'AP'){
      filepath <- file.path(main_dir, 'physio', 'appalachian')
    }else if(sub_category1 == 'CP'){
      filepath <- file.path(main_dir, 'physio', 'coastal')
    }else{
      stop('physio class is not IN, AP, or CP')
    }
  }else{
    stop('category name is not one of seas, seaslc, seasphysio, lc, or physio')
  }
  
  return(filepath)
}

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
