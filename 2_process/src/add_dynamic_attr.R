add_dyn_attrs_to_reaches <- function(attrs, dyn_cols, start_date, end_date){
  #' @description computes dynamic attributes for each reach based on the provided dates
  #' 
  #' @param attrs table of static attributes (columns) for each reach (rows)
  #' @param dyn_cols character vector of keywords to search for columns that 
  #' should be converted into dynamic attributes.
  #' @param start_date first date to compute attributes
  #' @param end_date last date to compute attributes
  #' 
  #' @return tbl with the added features.
  
  #make a dynamic features dataframe
  dates <- seq(as.Date(start_date), as.Date(end_date), 1)
  segs <- sort(unique(attrs$PRMS_segid))
  df <- tibble(Date = dates, seg = segs[1])
  for(i in 2:length(segs)){
    df_tmp <- tibble(Date = dates, seg = segs[i])
    df <- rbind(df, df_tmp)
  }
  rm(df_tmp, dates, segs, i)
  
  #Special handling for each variable
  if ('HDENS' %in% dyn_cols){
    #Housing density has the format HDENSYY
    tmp_attrs <- select(attrs, PRMS_segid, contains('HDENS'))
    
    #Convert the column names to 4-digit years
    col_years = str_split(colnames(tmp_attrs)[-1], pattern = '_', simplify = T)[,2] %>%
      substr(start = 6, stop = 7) %>%
      get_four_digit_year()
    tmp_colnames <- colnames(tmp_attrs)
    for(i in 2:length(tmp_colnames)){
      tmp_colnames[i] <- gsub(x = tmp_colnames[i], pattern = 'HDENS+[0-9]+[0-9]', 
                              replacement = col_years[i-1])
    }
    colnames(tmp_attrs) <- tmp_colnames
    rm(tmp_colnames, col_years)
    
    #format for new variable is NAME_lag, lag = 0 for these variables
    df$CAT_HDENS_0 = NA_real_
    df$TOT_HDENS_0 = NA_real_
    
    #Join data to all segs by date range
    # Note: using this method instead of a case_when because
    # case_when was very slow
    date_ranges <- seq(as.Date('1965-09-30'), as.Date('2005-09-30'), by = '10 years')
    for (i in 1:(length(date_ranges)+1)){
      if (i == 1){
        df[df$Date <= date_ranges[i], c('CAT_HDENS_0', 'TOT_HDENS_0')] <- 
          left_join(df %>% 
                      filter(Date <= date_ranges[i]) %>% 
                      select(seg), 
                    tmp_attrs %>% 
                      select(PRMS_segid, CAT_1960_area_wtd, TOT_1960),
                    by = c('seg' = 'PRMS_segid')) %>%
          select(CAT_1960_area_wtd, TOT_1960)
      }else if (i == (length(date_ranges)+1)){
        df[df$Date > date_ranges[i-1], c('CAT_HDENS_0', 'TOT_HDENS_0')] <- 
          left_join(df %>% 
                      filter(Date > date_ranges[i-1]) %>% 
                      select(seg),
                    tmp_attrs %>% 
                      select(PRMS_segid, CAT_2010_area_wtd, TOT_2010),
                    by = c('seg' = 'PRMS_segid')) %>%
          select(CAT_2010_area_wtd, TOT_2010)
      }else{
        #Get the year corresponding to the date range
        tmp_yr <- year(mean(c(date_ranges[i], date_ranges[i-1])))
        #Get the column names to be selected
        tmp_colnames <- c(paste0('CAT_', tmp_yr, '_area_wtd'),
                          paste0('TOT_', tmp_yr))
        
        df[df$Date > date_ranges[i-1] & df$Date <= date_ranges[i], 
           c('CAT_HDENS_0', 'TOT_HDENS_0')] <- 
          left_join(df %>% 
                      filter(Date > date_ranges[i-1], Date <= date_ranges[i]) %>% 
                      select(seg),
                    tmp_attrs %>% 
                      select(PRMS_segid, all_of(tmp_colnames)),
                    by = c('seg' = 'PRMS_segid')) %>%
          select(all_of(tmp_colnames))
      }
    }
  }
  
  if ('MAJOR' %in% dyn_cols){
    #major dams has the format MAJORYYYY
    tmp_attrs <- select(attrs, PRMS_segid, contains('MAJOR'))
    
    #format for new variable is NAME_lag, lag = 0 for these variables
    df$CAT_MAJOR_0 = NA_real_
    df$TOT_MAJOR_0 = NA_real_
    
    #Join data to all segs by date range
    date_ranges <- seq(as.Date('1975-09-30'), as.Date('2005-09-30'), by = '10 years')
    for (i in 1:(length(date_ranges)+1)){
      if (i == 1){
        df[df$Date <= date_ranges[i], c('CAT_MAJOR_0', 'TOT_MAJOR_0')] <- 
          left_join(df %>% 
                      filter(Date <= date_ranges[i]) %>% 
                      select(seg), 
                    tmp_attrs %>% 
                      select(PRMS_segid, CAT_MAJOR1970_sum, TOT_MAJOR1970),
                    by = c('seg' = 'PRMS_segid')) %>%
          select(CAT_MAJOR1970_sum, TOT_MAJOR1970)
      }else if (i == (length(date_ranges)+1)){
        df[df$Date > date_ranges[i-1], c('CAT_MAJOR_0', 'TOT_MAJOR_0')] <- 
          left_join(df %>% 
                      filter(Date > date_ranges[i-1]) %>% 
                      select(seg),
                    tmp_attrs %>% 
                      select(PRMS_segid, CAT_MAJOR2010_sum, TOT_MAJOR2010),
                    by = c('seg' = 'PRMS_segid')) %>%
          select(CAT_MAJOR2010_sum, TOT_MAJOR2010)
      }else{
        #Get the year corresponding to the date range
        tmp_yr <- year(mean(c(date_ranges[i], date_ranges[i-1])))
        #Get the column names to be selected
        tmp_colnames <- c(paste0('CAT_MAJOR', tmp_yr, '_sum'),
                          paste0('TOT_MAJOR', tmp_yr))
        
        df[df$Date > date_ranges[i-1] & df$Date <= date_ranges[i], 
           c('CAT_MAJOR_0', 'TOT_MAJOR_0')] <- 
          left_join(df %>% 
                      filter(Date > date_ranges[i-1], Date <= date_ranges[i]) %>% 
                      select(seg),
                    tmp_attrs %>% 
                      select(PRMS_segid, all_of(tmp_colnames)),
                    by = c('seg' = 'PRMS_segid')) %>%
          select(all_of(tmp_colnames))
      }
    }
  }
  
  if ('NDAMS' %in% dyn_cols){
    #number of dams has the format NDMASYYYY
    tmp_attrs <- select(attrs, PRMS_segid, contains('NDAMS'))
    
    #format for new variable is NAME_lag, lag = 0 for these variables
    df$CAT_NDAMS_0 = NA_real_
    df$TOT_NDAMS_0 = NA_real_
    
    #Join data to all segs by date range
    date_ranges <- seq(as.Date('1975-09-30'), as.Date('2005-09-30'), by = '10 years')
    for (i in 1:(length(date_ranges)+1)){
      if (i == 1){
        df[df$Date <= date_ranges[i], c('CAT_NDAMS_0', 'TOT_NDAMS_0')] <- 
          left_join(df %>% 
                      filter(Date <= date_ranges[i]) %>% 
                      select(seg), 
                    tmp_attrs %>% 
                      select(PRMS_segid, CAT_NDAMS1970_sum, TOT_NDAMS1970),
                    by = c('seg' = 'PRMS_segid')) %>%
          select(CAT_NDAMS1970_sum, TOT_NDAMS1970)
      }else if (i == (length(date_ranges)+1)){
        df[df$Date > date_ranges[i-1], c('CAT_NDAMS_0', 'TOT_NDAMS_0')] <- 
          left_join(df %>% 
                      filter(Date > date_ranges[i-1]) %>% 
                      select(seg),
                    tmp_attrs %>% 
                      select(PRMS_segid, CAT_NDAMS2010_sum, TOT_NDAMS2010),
                    by = c('seg' = 'PRMS_segid')) %>%
          select(CAT_NDAMS2010_sum, TOT_NDAMS2010)
      }else{
        #Get the year corresponding to the date range
        tmp_yr <- year(mean(c(date_ranges[i], date_ranges[i-1])))
        #Get the column names to be selected
        tmp_colnames <- c(paste0('CAT_NDAMS', tmp_yr, '_sum'),
                          paste0('TOT_NDAMS', tmp_yr))
        
        df[df$Date > date_ranges[i-1] & df$Date <= date_ranges[i], 
           c('CAT_NDAMS_0', 'TOT_NDAMS_0')] <- 
          left_join(df %>% 
                      filter(Date > date_ranges[i-1], Date <= date_ranges[i]) %>% 
                      select(seg),
                    tmp_attrs %>% 
                      select(PRMS_segid, all_of(tmp_colnames)),
                    by = c('seg' = 'PRMS_segid')) %>%
          select(all_of(tmp_colnames))
      }
    }
  }

  if ('NORM' %in% dyn_cols){
    #dam storage has the formate STORAGEYYYY
    tmp_attrs <- select(attrs, PRMS_segid, contains('NORM'))
    
    #format for new variable is NAME_lag, lag = 0 for these variables
    df$CAT_NORM_0 = NA_real_
    df$TOT_NORM_0 = NA_real_
    
    #Join data to all segs by date range
    date_ranges <- seq(as.Date('1975-09-30'), as.Date('2005-09-30'), by = '10 years')
    for (i in 1:(length(date_ranges)+1)){
      if (i == 1){
        df[df$Date <= date_ranges[i], c('CAT_NORM_0', 'TOT_NORM_0')] <- 
          left_join(df %>% 
                      filter(Date <= date_ranges[i]) %>% 
                      select(seg), 
                    tmp_attrs %>% 
                      select(PRMS_segid, CAT_NORM_STORAGE1970_sum, TOT_NORM_STORAGE1970),
                    by = c('seg' = 'PRMS_segid')) %>%
          select(CAT_NORM_STORAGE1970_sum, TOT_NORM_STORAGE1970)
      }else if (i == (length(date_ranges)+1)){
        df[df$Date > date_ranges[i-1], c('CAT_NORM_0', 'TOT_NORM_0')] <- 
          left_join(df %>% 
                      filter(Date > date_ranges[i-1]) %>% 
                      select(seg),
                    tmp_attrs %>% 
                      select(PRMS_segid, CAT_NORM_STORAGE2010_sum, TOT_NORM_STORAGE2010),
                    by = c('seg' = 'PRMS_segid')) %>%
          select(CAT_NORM_STORAGE2010_sum, TOT_NORM_STORAGE2010)
      }else{
        #Get the year corresponding to the date range
        tmp_yr <- year(mean(c(date_ranges[i], date_ranges[i-1])))
        #Get the column names to be selected
        tmp_colnames <- c(paste0('CAT_NORM_STORAGE', tmp_yr, '_sum'),
                          paste0('TOT_NORM_STORAGE', tmp_yr))
        
        df[df$Date > date_ranges[i-1] & df$Date <= date_ranges[i], 
           c('CAT_NORM_0', 'TOT_NORM_0')] <- 
          left_join(df %>% 
                      filter(Date > date_ranges[i-1], Date <= date_ranges[i]) %>% 
                      select(seg),
                    tmp_attrs %>% 
                      select(PRMS_segid, all_of(tmp_colnames)),
                    by = c('seg' = 'PRMS_segid')) %>%
          select(all_of(tmp_colnames))
      }
    }
  }
  
  if ('NID' %in% dyn_cols){
    #dam storage has the formate STORAGEYYYY
    tmp_attrs <- select(attrs, PRMS_segid, contains('NID'))
    
    #format for new variable is NAME_lag, lag = 0 for these variables
    df$CAT_NID_0 = NA_real_
    df$TOT_NID_0 = NA_real_
    
    #Join data to all segs by date range
    date_ranges <- seq(as.Date('1975-09-30'), as.Date('2005-09-30'), by = '10 years')
    for (i in 1:(length(date_ranges)+1)){
      if (i == 1){
        df[df$Date <= date_ranges[i], c('CAT_NID_0', 'TOT_NID_0')] <- 
          left_join(df %>% 
                      filter(Date <= date_ranges[i]) %>% 
                      select(seg), 
                    tmp_attrs %>% 
                      select(PRMS_segid, CAT_NID_STORAGE1970_sum, TOT_NID_STORAGE1970),
                    by = c('seg' = 'PRMS_segid')) %>%
          select(CAT_NID_STORAGE1970_sum, TOT_NID_STORAGE1970)
      }else if (i == (length(date_ranges)+1)){
        df[df$Date > date_ranges[i-1], c('CAT_NID_0', 'TOT_NID_0')] <- 
          left_join(df %>% 
                      filter(Date > date_ranges[i-1]) %>% 
                      select(seg),
                    tmp_attrs %>% 
                      select(PRMS_segid, CAT_NID_STORAGE2010_sum, TOT_NID_STORAGE2010),
                    by = c('seg' = 'PRMS_segid')) %>%
          select(CAT_NID_STORAGE2010_sum, TOT_NID_STORAGE2010)
      }else{
        #Get the year corresponding to the date range
        tmp_yr <- year(mean(c(date_ranges[i], date_ranges[i-1])))
        #Get the column names to be selected
        tmp_colnames <- c(paste0('CAT_NID_STORAGE', tmp_yr, '_sum'),
                          paste0('TOT_NID_STORAGE', tmp_yr))
        
        df[df$Date > date_ranges[i-1] & df$Date <= date_ranges[i], 
           c('CAT_NID_0', 'TOT_NID_0')] <- 
          left_join(df %>% 
                      filter(Date > date_ranges[i-1], Date <= date_ranges[i]) %>% 
                      select(seg),
                    tmp_attrs %>% 
                      select(PRMS_segid, all_of(tmp_colnames)),
                    by = c('seg' = 'PRMS_segid')) %>%
          select(all_of(tmp_colnames))
      }
    }
  }
  
  if ('LULC' %in% dyn_cols){
    #land cover has a year column
    
  }
  
  if ('gridMET' %in% dyn_cols){
    #gridMET data has a date column
    #several variables to grab
    
  }

  return()
}

get_four_digit_year <- function(two_digit_yr){
  #' @description computes the 4 digit year from two digit year
  #' 
  #' @param two_digit_yr vector of two digit years, YY
  #' 
  #' @return four digit year, YYYY
  
  four_digit_yr <- case_when(two_digit_yr < 40 ~ paste0('20', two_digit_yr),
                               two_digit_yr >= 40 ~ paste0('19', two_digit_yr))
  
  return(four_digit_yr)
}

compute_lagged_attrs <- function(attrs, lags, lag_fxn){
  #' @description computes lagged predictor variables from the provided features
  #' 
  #' @param 
  #' @param lags vector stating how many days to lag. A column will be added for each element.
  #' @param lag_fxn function used to compute the lagged information. 
  #' Example: mean for mean over the last lag days. NULL = exact value.
  #' 
  #' @return tbl with the added lagged features.
  
  #Add a column for each of the elements in lags
  
  #handle cases when the lagged date is not in the dataset
  
  return()
}

#Remove NA observations that result from using lagged predictors

plot_dynamic_attrs <- function(dyn_attrs, sites, out_dir){
  #' @description plots timeseries of dynamic features for the provided sites
  #' 
  #' @param 
  #' @param lags vector stating how many days to lag. A column will be added for each element.
  #' 
  #' @return filepaths to the resulting plots
  
  fileout
  
  return(fileout)
}