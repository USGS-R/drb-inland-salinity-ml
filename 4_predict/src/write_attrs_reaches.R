write_attrs_files <- function(dataset, drop_cols, filepath){
  #' @description generates a text file of column names
  #' 
  #' @param dataset the dataset whose columns will be written to a txt file
  #' @param drop_cols any columns to not include in the file. 
  #' Passed to select(-all_of(drop_cols))
  #' @param filepath relative filepath to the txt file
  #' 
  #' @return txt file containing the column names of dataset without the drop_columns
  
  col_names <- select(dataset, -all_of(drop_cols)) %>%
    colnames() %>%
    sort()
  
  write_csv(x = as.data.frame(col_names), file = filepath, col_names = FALSE)
  
  return(filepath)
}

write_reaches <- function(reach_ids, filepath){
  #' @description generates a text file of the reach_ids
  #' 
  #' @param reach_ids character string of the IDs to be written to a txt file
  #' @param filepath relative filepath to the txt file
  #' 
  #' @return txt file containing the reach_ids
  
  reach_ids <- sort(unique(reach_ids))
  
  write_csv(x = as.data.frame(reach_ids), file = filepath, col_names = FALSE)
  
  return(filepath)
}

write_reaches_cv <- function(train_dataset, cv_folds, out_dir){
  #' @description generates a text file of the reach_ids within the validation set,
  #' one text file per fold.
  #' 
  #' @param train_dataset tibble with columns for PRMS_segid and data_type.
  #' This will be split into cv_folds partitions based on the data_type proportions
  #' @param cv_folds number of cross validation folds in training. This is used to
  #' write validation files.
  #' @param out_dir directory to save txt files
  #' 
  #' @return txt files containing the reach_ids
  
  #create validation file names
  val_nums <- seq(1,cv_folds,1)
  filepaths <- file.path(out_dir, c(paste0('spatial_val_reaches', val_nums, '.txt')))
  
  #add group variable to dataset based on spatial splits
  train_dataset$group <- 0
  
  #determine validation splits and write to file
  for(i in val_nums){
    if(i == cv_folds){
      ind_i <- which(train_dataset$group == 0)
      inds_grp <- seq(1,length(ind_i),1)
    }else{
      #only use data that haven't been assigned to a group
      ind_i <- which(train_dataset$group == 0)
      #get the correct training proportion to use for a reduced dataset
      train_prop_i <- nrow(train_dataset)/cv_folds/nrow(train_dataset[ind_i,])
      #group row indices
      inds_grp <- make_spatial_split_CVtraining(attrs_df = train_dataset[ind_i,], 
                                                train_prop = train_prop_i)
    }
    #assign group index
    train_dataset$group[ind_i][inds_grp] <- i
    write_csv(x = as.data.frame(sort(unique(train_dataset$PRMS_segid[train_dataset$group == i]))), 
              file = filepaths[i], col_names = FALSE)
  }
  
  return(filepaths)
}

write_dates <- function(dataset, cv_folds = NULL, out_dir){
  #' @description generates a text file for each of the training and testing 
  #' start and end dates for use in Python.
  #' 
  #' @param dataset list of training and testing datasets, each with a Date column
  #' @param cv_folds number of cross validation folds in training. This is used to
  #' write validation files.
  #' @param out_dir directory to save txt files
  #' 
  #' @return vector of paths to txt files containing the start and end dates
  
  if (is.null(cv_folds)){
    filenames <- file.path(out_dir, c('train_start.txt', 'train_end.txt', 'test_start.txt', 'test_end.txt'))
  }else{
    #create validation files from training time period
    val_nums <- seq(1,cv_folds-1,1)
    filenames <- file.path(out_dir, c('train_start.txt', 'train_end.txt', 'test_start.txt', 'test_end.txt',
                                      paste0('val_start', val_nums+1, '.txt'), paste0('val_end', val_nums, '.txt')))
  }
  
  train_start <- min(dataset$training$Date) %>% as.data.frame()
  train_end <- max(dataset$training$Date) %>% as.data.frame()
  test_start <- min(dataset$testing$Date) %>% as.data.frame()
  test_end <- max(dataset$testing$Date) %>% as.data.frame()
  
  write_csv(train_start, file = filenames[1], col_names = FALSE)
  write_csv(train_end, file = filenames[2], col_names = FALSE)
  write_csv(test_start, file = filenames[3], col_names = FALSE)
  write_csv(test_end, file = filenames[4], col_names = FALSE)
  
  if (!is.null(cv_folds)){
    #determine validation start and end dates and write to file
    for(i in val_nums){
      val_start <- train_start + (train_end - train_start)/cv_folds*i
      val_end <- val_start - 1
      write_csv(val_start, file = filenames[4+i], col_names = FALSE)
      write_csv(val_end, file = filenames[8+i], col_names = FALSE)
    }
  }
  
  return(filenames)
}

write_pred_obs <- function(pred_obs, out_dir){
  #' @description generates a text file containing the predictions, observations,
  #' date, and segment columns.
  #' 
  #' @param pred_obs dataframe containing columns "Date", "PRMS_segid", "obs", and ".pred"
  #' @param out_dir directory to save txt file
  #' 
  #' @return vector of paths to txt file
   
  filename <- file.path(out_dir, 'pred_obs.txt')
  
  pred_obs <- select(pred_obs, "Date", "PRMS_segid", "obs", ".pred") %>%
    rename(pred = '.pred')
  
  write_csv(pred_obs, filename)
  
  return(filename)
}

write_pred_obs_splits = function(pred_obs, split, out_dir){
  #' @description generates a text file containing the predictions, observations,
  #' date, and segment columns.
  #' 
  #' @param pred_obs dataframe containing columns "Date", "PRMS_segid", "obs", and ".pred"
  #' @param split data frame with PRMS_segid and Date for the rows to use from pred_obs.
  #' @param out_dir directory to save txt file
  #' 
  #' @return vector of paths to txt file
  
  #get subset of pred_obs to use
  pred_obs_split <- left_join(split, pred_obs, by = c('PRMS_segid', 'Date'))
  
  filename <- write_pred_obs(pred_obs = pred_obs_split, out_dir = out_dir)
  
  return(filename)
}
