add_dyn_attrs_to_reaches <- function(attrs, dyn_cols, start_date, end_date,
                                     baseflow, CAT_Land, TOT_Land, gridMET,
                                     lags, lag_unit){
  #' @description computes dynamic attributes for each reach based on the provided dates
  #' 
  #' @param attrs table of static attributes (columns) for each reach (rows)
  #' @param dyn_cols character vector of keywords to search for columns that 
  #' should be converted into dynamic attributes.
  #' @param start_date first date to compute attributes
  #' @param end_date last date to compute attributes
  #' @param baseflow table of monthly baseflow estimates for each reach
  #' @param CAT_Land table of catchment land cover class proportions
  #' @param TOT_Land table of total upstream land cover class proportions
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
    col_years <- str_split(colnames(tmp_attrs)[-1], pattern = '_', simplify = T)[,2] %>%
      substr(start = 6, stop = 7) %>%
      get_four_digit_year()
    tmp_colnames <- colnames(tmp_attrs)
    for(i in 2:length(tmp_colnames)){
      tmp_colnames[i] <- gsub(x = tmp_colnames[i], pattern = 'HDENS+[0-9]+[0-9]', 
                              replacement = col_years[i-1])
    }
    colnames(tmp_attrs) <- tmp_colnames
    rm(tmp_colnames, col_years)
    
    df <- get_dynamic_HDENS(dyn_df = df, attrs = tmp_attrs, 
                            lags = lags, lag_unit = lag_unit)
  }
  
  if ('MAJOR' %in% dyn_cols){
    #major dams has the format MAJORYYYY
    tmp_attrs <- select(attrs, PRMS_segid, contains('MAJOR'))
    
    #format for new variable is NAME_lag, lag = 0 for these variables
    df$CAT_MAJOR_0 <- NA_real_
    df$TOT_MAJOR_0 <- NA_real_
    
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
    df$CAT_NDAMS_0 <- NA_real_
    df$TOT_NDAMS_0 <- NA_real_
    
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
    df$CAT_NORM_0 <- NA_real_
    df$TOT_NORM_0 <- NA_real_
    
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
    df$CAT_NID_0 <- NA_real_
    df$TOT_NID_0 <- NA_real_
    
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
  
  #Land Cover
  #format for new variable is NAME_lag, lag = 0 for these variables
  df$CAT_LC9_0 <- df$CAT_LC8_0 <- df$CAT_LC7_0 <- df$CAT_LC6_0 <- 
     df$CAT_LC5_0 <- df$CAT_LC4_0 <- df$CAT_LC3_0 <- df$CAT_LC2_0 <- 
     df$CAT_LC1_0 <- NA_real_
  df$TOT_LC9_0 <- df$TOT_LC8_0 <- df$TOT_LC7_0 <- df$TOT_LC6_0 <- 
    df$TOT_LC5_0 <- df$TOT_LC4_0 <- df$TOT_LC3_0 <- df$TOT_LC2_0 <- 
    df$TOT_LC1_0 <- NA_real_
  
  #Join data to all segs by date range
  date_ranges <- c(seq(as.Date('1945-09-30'), as.Date('1995-09-30'), by = '10 years'),
                   '2001-03-31', '2003-03-31', '2005-09-30', '2007-09-30', 
                   '2010-03-31', '2012-09-30', '2015-03-31', '2018-03-31')
  for (i in 1:(length(date_ranges)+1)){
    if (i == 1){
      df[df$Date <= date_ranges[i], grep(colnames(df), 
                                         pattern = 'CAT_LC', fixed = TRUE)] <- 
        left_join(df %>% 
                    filter(Date <= date_ranges[i]) %>% 
                    select(seg), 
                  CAT_Land %>%
                    filter(Year == 1940) %>%
                    select(PRMS_segid, starts_with('CAT')),
                  by = c('seg' = 'PRMS_segid')) %>%
        select(starts_with('CAT'))
      #TOT
      df[df$Date <= date_ranges[i], grep(colnames(df), 
                                         pattern = 'TOT_LC', fixed = TRUE)] <- 
        left_join(df %>% 
                    filter(Date <= date_ranges[i]) %>% 
                    select(seg), 
                  TOT_Land %>%
                    filter(Year == 1940) %>% 
                    select(PRMS_segid, starts_with('TOT')),
                  by = c('seg' = 'PRMS_segid')) %>%
        select(starts_with('TOT'))
    }else if (i == (length(date_ranges)+1)){
      df[df$Date > date_ranges[i-1], grep(colnames(df), 
                                          pattern = 'CAT_LC', fixed = TRUE)] <- 
        left_join(df %>% 
                    filter(Date > date_ranges[i-1]) %>% 
                    select(seg),
                  CAT_Land %>%
                    filter(Year == 2019) %>% 
                    select(PRMS_segid, starts_with('CAT')),
                  by = c('seg' = 'PRMS_segid')) %>%
        select(starts_with('CAT'))
      #TOT
      df[df$Date > date_ranges[i-1], grep(colnames(df), 
                                          pattern = 'TOT_LC', fixed = TRUE)] <- 
        left_join(df %>% 
                    filter(Date > date_ranges[i-1]) %>% 
                    select(seg),
                  TOT_Land %>%
                    filter(Year == 2019) %>% 
                    select(PRMS_segid, starts_with('TOT')),
                  by = c('seg' = 'PRMS_segid')) %>%
        select(starts_with('TOT'))
    }else{
      #Get the land cover year corresponding to the ith date range
      tmp_yr <- sort(unique(CAT_Land$Year))[i]
      
      df[df$Date > date_ranges[i-1] & df$Date <= date_ranges[i], 
         grep(colnames(df), pattern = 'CAT_LC', fixed = TRUE)] <- 
        left_join(df %>% 
                    filter(Date > date_ranges[i-1], Date <= date_ranges[i]) %>% 
                    select(seg),
                  CAT_Land %>%
                    filter(Year == tmp_yr) %>% 
                    select(PRMS_segid, starts_with('CAT')),
                  by = c('seg' = 'PRMS_segid')) %>%
        select(starts_with('CAT'))
      #TOT
      df[df$Date > date_ranges[i-1] & df$Date <= date_ranges[i], 
         grep(colnames(df), pattern = 'TOT_LC', fixed = TRUE)] <- 
        left_join(df %>% 
                    filter(Date > date_ranges[i-1], Date <= date_ranges[i]) %>% 
                    select(seg),
                  TOT_Land %>%
                    filter(Year == tmp_yr) %>% 
                    select(PRMS_segid, starts_with('TOT')),
                  by = c('seg' = 'PRMS_segid')) %>%
        select(starts_with('TOT'))
    }
  }
  
  #gridMET data
  #Reformat columns
  gridMET <- filter(gridMET, time <= end_date, time >= start_date) %>%
    #format for new variable is NAME_lag, lag = 0 for these variables
    rename(Date = time, tmmx_0 = tmmx, tmmn_0 = tmmn, pr_0 = pr, srad_0 = srad, 
           vs_0 = vs, rmax_0 = rmax, rmin_0 = rmin, sph_0 = sph)
  
  #Join to df
  df <- left_join(df, gridMET, by = c('seg' = 'PRMS_segid', "Date"))
  
  #Commented out for now because some reaches do not have baseflow data
  # #Assign monthly baseflow data
  # #Add year and month columns (to be dropped after joining)
  # df$Year <- year(df$Date)
  # df$Month <- month(df$Date)
  # #Remove the leading 0 for Month in baseflow
  # baseflow$Month <- as.numeric(baseflow$Month)
  # 
  # #Join data to all segs by year and month
  # df <- left_join(df, baseflow, by = c('seg' = 'PRMS_segid', 'Year', 'Month')) %>%
  #   select(-Year, -Month)
  

  return(df)
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

get_dynamic_HDENS <- function(dyn_df, attrs, lags, lag_unit){
  #' @description computes dynamic housing density from the provided tbl
  #' 
  #' @param dyn_df tbl containing "Date" and the PRMS "seg" 
  #' @param attrs tbl containing the "PRMS_segid" and the HDENS attribute columns
  #' for CAT_YYYY_area_wtd and TOT_YYYY
  #' @param lags vector stating how many lag_units to lag. 
  #' A column will be added for each element for each attrs column.
  #' @param lag_unit character vector containing the unit to use for each lag in lags. 
  #' Accepts any of the lubridate options (e.g., days, months, years). If all
  #' units are the same, can provide a one element vector with that unit.
  #' 
  #' @return four digit year, YYYY
  
  #Make the length of the lag_unit match the length of lags
  if ((length(lag_unit) == 1) & (length(lags) > 1)){
    lag_unit <- rep(lag_unit, length(lags))
  }
  
  #format for new variable is NAME_lag
  CAT_colnames <- paste0('CAT_HDENS_', lags, lag_unit)
  TOT_colnames <- paste0('TOT_HDENS_', lags, lag_unit)
  dyn_df[CAT_colnames] <- NA_real_
  dyn_df[TOT_colnames] <- NA_real_
  
  #Loop over all of the lags
  for (lag in 1:length(lags)){
    #Create a temporary date column containing the lagged date to use
    if(lags[lag] != 0){
      dyn_df$lagged_Date <- dyn_df$Date %m-% period(paste(lags[lag], lag_unit[lag]))
    }else{
      dyn_df$lagged_Date <- dyn_df$Date
    }
    
    #Join data to all segs by date range
    # Note: using this method instead of a case_when because
    # case_when was very slow
    date_ranges <- seq(as.Date('1965-09-30'), as.Date('2005-09-30'), by = '10 years')
    for (i in 1:(length(date_ranges)+1)){
      if (i == 1){
        dyn_df[dyn_df$lagged_Date <= date_ranges[i], 
              c(paste0('CAT_HDENS_', lags[lag], lag_unit[lag]), 
                paste0('TOT_HDENS_', lags[lag], lag_unit[lag]))] <- 
          left_join(dyn_df %>% 
                      filter(lagged_Date <= date_ranges[i]) %>% 
                      select(seg), 
                    attrs %>% 
                      select(PRMS_segid, CAT_1960_area_wtd, TOT_1960),
                    by = c('seg' = 'PRMS_segid')) %>%
          select(CAT_1960_area_wtd, TOT_1960)
      }else if (i == (length(date_ranges)+1)){
        dyn_df[dyn_df$lagged_Date > date_ranges[i-1], 
              c(paste0('CAT_HDENS_', lags[lag], lag_unit[lag]), 
                paste0('TOT_HDENS_', lags[lag], lag_unit[lag]))] <- 
          left_join(dyn_df %>% 
                      filter(lagged_Date > date_ranges[i-1]) %>% 
                      select(seg),
                    attrs %>% 
                      select(PRMS_segid, CAT_2010_area_wtd, TOT_2010),
                    by = c('seg' = 'PRMS_segid')) %>%
          select(CAT_2010_area_wtd, TOT_2010)
      }else{
        #Get the year corresponding to the date range
        tmp_yr <- year(mean(c(date_ranges[i], date_ranges[i-1])))
        #Get the column names to be selected
        tmp_colnames <- c(paste0('CAT_', tmp_yr, '_area_wtd'),
                          paste0('TOT_', tmp_yr))
        
        dyn_df[dyn_df$lagged_Date > date_ranges[i-1] & dyn_df$lagged_Date <= date_ranges[i],
              c(paste0('CAT_HDENS_', lags[lag], lag_unit[lag]), 
                paste0('TOT_HDENS_', lags[lag], lag_unit[lag]))] <- 
          left_join(dyn_df %>% 
                      filter(lagged_Date > date_ranges[i-1], 
                             lagged_Date <= date_ranges[i]) %>% 
                      select(seg),
                    attrs %>% 
                      select(PRMS_segid, all_of(tmp_colnames)),
                    by = c('seg' = 'PRMS_segid')) %>%
          select(all_of(tmp_colnames))
      }
    }
  }
  #Remove the lagged_Date column
  dyn_df <- select(dyn_df, -lagged_Date)

  return(dyn_df)
}


compute_lagged_attrs <- function(attrs, lags, lag_unit, lag_fxn){
  #' @description computes lagged predictor variables from the provided features
  #' 
  #' @param attrs tbl containing "Date" and the PRMS "seg",
  #' followed by only the columns for which to compute lagged information 
  #' @param lags vector stating how many lag_units to lag. 
  #' A column will be added for each element for each attrs column.
  #' @param lag_unit character vector containing the unit to use for each lag in lags. 
  #' Accepts any of the lubridate options (e.g., days, months, years). If all
  #' units are the same, can provide a one element vector with that unit.
  #' @param lag_fxn function used to compute the lagged information. 
  #' Example: mean for mean over the last lag days. NULL = exact value on that day.
  #' 
  #' @return tbl with the added lagged features.
  
  #Make the length of the lag_unit match the length of lags
  if ((length(lag_unit) == 1) & (length(lags) > 1)){
    lag_unit <- rep(lag_unit, length(lags))
  }
  
  #may have the case that the available attrs span a larger range than the 
  #available data. need a way to specify that
  
  #Trim the date range so that the earliest date = old date + lag
  
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