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
  #' @return txt file containing the reach_ids
  
  #create validation file names
  val_nums <- seq(1,cv_folds,1)
  filenames <- file.path(out_dir, c(paste0('val_segs', val_nums, '.txt')))
  
  #determine validation splits and write to file
  for(i in val_nums){
    reach_ids_i
    write_csv(x = as.data.frame(reach_ids_i), file = filepath[i], col_names = FALSE)
  }
  
  return(filenames)
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