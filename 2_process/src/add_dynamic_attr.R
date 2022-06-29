add_dyn_attrs_to_reaches <- function(attrs, dyn_cols, start_date, end_date,
                                     baseflow, CAT_Land, Upstream_Land, gridMET,
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
  #' @param Upstream_Land table of total upstream land cover class proportions
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
                              replacement = col_years[i-1]) %>%
        #Format column names to match requirements for the function
        gsub(pattern = '_area_wtd', replacement = '')
    }
    colnames(tmp_attrs) <- tmp_colnames
    rm(tmp_colnames, col_years)
    
    #HDENS data date ranges and dataset years
    tmp_date_ranges <- seq(as.Date('1965-09-30'), as.Date('2005-09-30'),
                           '10 years')
    tmp_years <- tmp_attrs %>% 
      select(starts_with('CAT')) %>% 
      colnames() %>% 
      sort() %>% 
      str_split(pattern = '_', simplify = TRUE) %>% 
      .[,2]
    
    df <- get_dynamic_from_static(dyn_df = df, attrs = tmp_attrs, 
                                  lags = 0, lag_unit = '',
                                  date_ranges = tmp_date_ranges,
                                  attr_years = tmp_years,
                                  attr_name = 'HDENS', 
                                  attr_prefix = c('CAT', 'TOT'))
    
    #add in lagged dynamic information
    #Create df that starts at the earliest date - largest lag
    #Assign the exact value of HDENS on those dates to the df
    #Use the lag function to compute the desired attributes
    #should have an if statement here because if the lag function only has 
    #exact, then do not need to pass to this function.
  }
  
  if ('MAJOR' %in% dyn_cols){
    #major dams has the format MAJORYYYY
    tmp_attrs <- select(attrs, PRMS_segid, contains('MAJOR'))
    tmp_colnames <- colnames(tmp_attrs)
    for(i in 2:length(tmp_colnames)){
      tmp_colnames[i] <- gsub(x = tmp_colnames[i], pattern = '_sum', 
                              replacement = '') %>%
        gsub(pattern = 'MAJOR', 
             replacement = '')
    }
    colnames(tmp_attrs) <- tmp_colnames
    rm(tmp_colnames)
    
    #date ranges and dataset years
    tmp_date_ranges <- seq(as.Date('1975-09-30'), as.Date('2005-09-30'),
                           '10 years')
    tmp_years <- tmp_attrs %>% 
      select(starts_with('CAT')) %>% 
      colnames() %>% 
      sort() %>% 
      str_split(pattern = '_', simplify = TRUE) %>% 
      .[,2]
    
    df <- get_dynamic_from_static(dyn_df = df, attrs = tmp_attrs, 
                                  lags = 0, lag_unit = '',
                                  date_ranges = tmp_date_ranges,
                                  attr_years = tmp_years,
                                  attr_name = 'MAJOR', 
                                  attr_prefix = c('CAT', 'TOT'))
  }
  
  if ('NDAMS' %in% dyn_cols){
    #number of dams has the format NDMASYYYY
    tmp_attrs <- select(attrs, PRMS_segid, contains('NDAMS'))
    tmp_colnames <- colnames(tmp_attrs)
    for(i in 2:length(tmp_colnames)){
      tmp_colnames[i] <- gsub(x = tmp_colnames[i], pattern = '_sum', 
                              replacement = '') %>%
        gsub(pattern = 'NDAMS', 
             replacement = '')
    }
    colnames(tmp_attrs) <- tmp_colnames
    rm(tmp_colnames)
    
    #date ranges and dataset years
    tmp_date_ranges <- seq(as.Date('1975-09-30'), as.Date('2005-09-30'),
                           '10 years')
    tmp_years <- tmp_attrs %>% 
      select(starts_with('CAT')) %>% 
      colnames() %>% 
      sort() %>% 
      str_split(pattern = '_', simplify = TRUE) %>% 
      .[,2]
    
    df <- get_dynamic_from_static(dyn_df = df, attrs = tmp_attrs, 
                                  lags = 0, lag_unit = '',
                                  date_ranges = tmp_date_ranges,
                                  attr_years = tmp_years,
                                  attr_name = 'NDAMS', 
                                  attr_prefix = c('CAT', 'TOT'))
  }

  if ('NORM' %in% dyn_cols){
    #dam storage has the format STORAGEYYYY
    tmp_attrs <- select(attrs, PRMS_segid, contains('NORM'))
    tmp_colnames <- colnames(tmp_attrs)
    for(i in 2:length(tmp_colnames)){
      tmp_colnames[i] <- gsub(x = tmp_colnames[i], pattern = '_sum', 
                              replacement = '') %>%
        gsub(pattern = 'NORM_STORAGE', 
             replacement = '')
    }
    colnames(tmp_attrs) <- tmp_colnames
    rm(tmp_colnames)
    
    #date ranges and dataset years
    tmp_date_ranges <- seq(as.Date('1975-09-30'), as.Date('2005-09-30'),
                           '10 years')
    tmp_years <- tmp_attrs %>% 
      select(starts_with('CAT')) %>% 
      colnames() %>% 
      sort() %>% 
      str_split(pattern = '_', simplify = TRUE) %>% 
      .[,2]
    
    df <- get_dynamic_from_static(dyn_df = df, attrs = tmp_attrs, 
                                  lags = 0, lag_unit = '',
                                  date_ranges = tmp_date_ranges,
                                  attr_years = tmp_years,
                                  attr_name = 'NORM_STORAGE', 
                                  attr_prefix = c('CAT', 'TOT'))
  }
  
  if ('NID' %in% dyn_cols){
    #dam storage has the format STORAGEYYYY
    tmp_attrs <- select(attrs, PRMS_segid, contains('NID'))
    tmp_colnames <- colnames(tmp_attrs)
    for(i in 2:length(tmp_colnames)){
      tmp_colnames[i] <- gsub(x = tmp_colnames[i], pattern = '_sum', 
                              replacement = '') %>%
        gsub(pattern = 'NID_STORAGE', 
             replacement = '')
    }
    colnames(tmp_attrs) <- tmp_colnames
    rm(tmp_colnames)
    
    #date ranges and dataset years
    tmp_date_ranges <- seq(as.Date('1975-09-30'), as.Date('2005-09-30'),
                           '10 years')
    tmp_years <- tmp_attrs %>% 
      select(starts_with('CAT')) %>% 
      colnames() %>% 
      sort() %>% 
      str_split(pattern = '_', simplify = TRUE) %>% 
      .[,2]
    
    df <- get_dynamic_from_static(dyn_df = df, attrs = tmp_attrs, 
                                  lags = 0, lag_unit = '',
                                  date_ranges = tmp_date_ranges,
                                  attr_years = tmp_years,
                                  attr_name = 'NID_STORAGE', 
                                  attr_prefix = c('CAT', 'TOT'))
  }
  
  #Land Cover
  if (!is.null(CAT_Land)){
    #Get data table into format needed for function
    tmp_attrs <- pivot_wider(CAT_Land %>% 
                               select(-PRMS_area_km2), 
                             names_from = Year, 
                             values_from = starts_with('CAT'))
    
    #date ranges and dataset years
    tmp_date_ranges <- c(seq(as.Date('1945-09-30'), as.Date('1995-09-30'), by = '10 years'),
                         '2001-03-31', '2003-03-31', '2005-09-30', '2007-09-30',
                         '2010-03-31', '2012-09-30', '2015-03-31', '2018-03-31')
    tmp_years <- sort(unique(CAT_Land$Year))
    
    #Loop over all land cover classes to assign attributes
    for (lc in 1:9){
      tmp_attrs_lc <- select(tmp_attrs, PRMS_segid, contains(paste0('_',lc,'_')))
      tmp_colnames <- colnames(tmp_attrs_lc)
      for(i in 2:length(tmp_colnames)){
        tmp_colnames[i] <- gsub(x = tmp_colnames[i], 
                                pattern = paste0('_prop_lcClass_', lc), 
                                replacement = '')
      }
      colnames(tmp_attrs_lc) <- tmp_colnames
      rm(tmp_colnames)
      
      df <- get_dynamic_from_static(dyn_df = df, attrs = tmp_attrs_lc, 
                                    lags = 0, lag_unit = '',
                                    date_ranges = tmp_date_ranges,
                                    attr_years = tmp_years,
                                    attr_name = paste0('LC', lc), 
                                    attr_prefix = c('CAT'))
    }
  }
  if (!is.null(Upstream_Land)){
    #Get data table into format needed for function
    tmp_attrs <- pivot_wider(Upstream_Land %>% 
                               select(-PRMS_area_km2), 
                             names_from = Year, 
                             values_from = starts_with('TOT'))
    
    #date ranges and dataset years
    tmp_date_ranges <- c(seq(as.Date('1945-09-30'), as.Date('1995-09-30'), by = '10 years'),
                         '2001-03-31', '2003-03-31', '2005-09-30', '2007-09-30',
                         '2010-03-31', '2012-09-30', '2015-03-31', '2018-03-31')
    tmp_years <- sort(unique(Upstream_Land$Year))
    
    #Loop over all land cover classes to assign attributes
    for (lc in 1:9){
      tmp_attrs_lc <- select(tmp_attrs, PRMS_segid, contains(paste0('_',lc,'_')))
      tmp_colnames <- colnames(tmp_attrs_lc)
      for(i in 2:length(tmp_colnames)){
        tmp_colnames[i] <- gsub(x = tmp_colnames[i], 
                                pattern = paste0('_prop_lcClass_', lc), 
                                replacement = '')
      }
      colnames(tmp_attrs_lc) <- tmp_colnames
      rm(tmp_colnames)
      
      df <- get_dynamic_from_static(dyn_df = df, attrs = tmp_attrs_lc, 
                                    lags = 0, lag_unit = '',
                                    date_ranges = tmp_date_ranges,
                                    attr_years = tmp_years,
                                    attr_name = paste0('LC', lc), 
                                    attr_prefix = c('TOT'))
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
  
  #Assign monthly baseflow data
  #Add year and month columns (to be dropped after joining)
  df$Year <- year(df$Date)
  df$Month <- month(df$Date)
  #Remove the leading 0 for Month in baseflow
  baseflow$Month <- as.numeric(baseflow$Month)

  #Join data to all segs by year and month
  df <- left_join(df, baseflow, by = c('seg' = 'PRMS_segid', 'Year', 'Month')) %>%
    select(-Year, -Month)
  

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

get_dynamic_from_static <- function(dyn_df, attrs, lags, lag_unit, 
                                    attr_years, date_ranges,
                                    attr_name, attr_prefix){
  #' @description computes dynamic attribute from the provided tbl of static
  #' attributes.
  #' 
  #' @param dyn_df tbl containing "Date" and the PRMS "seg". Should have all
  #' dates that you want to use in modeling. 
  #' @param attrs tbl containing the "PRMS_segid" and attribute columns
  #' in the format CAT_YYYY and TOT_YYYY
  #' @param lags numeric vector stating how many lag_units to lag. 
  #' A column will be added for each element for each attrs column. A lag of 0
  #' can be used to extract the value on the Date in the dyn_df tbl.
  #' @param lag_unit character vector containing the unit to use for each lag in lags. 
  #' Accepts any of the lubridate options (e.g., days, months, years). If all
  #' units are the same, can provide a one element vector with that unit.
  #' @param attr_years ordered character vector of the years available for this 
  #' dataset. Should be in ascending order.
  #' In attrs, there should be one CAT and one TOT column for each year.
  #' @param date_ranges Date vector that specifies the transition dates from one
  #' year to another year. Dates in dyn_df less than the first date 
  #' will be assigned to the first year in attr_years. Dates in dyn_df greater 
  #' than the last date will be assigned to the last year in attr_years.
  #' @param attr_name name to use for this attribute. Format of the new name will
  #' be paste0(attr_prefix, '_', attr_name, '_', lags, lag_unit)
  #' @param attr_prefix character vector of prefixes for the attributes 
  #' (e.g., CAT, TOT)
  #' 
  #' @return dyn_df with the new dynamic attribute columns.
  
  #Make the length of the lag_unit match the length of lags
  if ((length(lag_unit) == 1) & (length(lags) > 1)){
    lag_unit <- rep(lag_unit, length(lags))
  }
  
  #format for new variable is NAME_lag
  for (j in 1:length(attr_prefix)){
    tmp_colnames <- paste0(attr_prefix[j], '_', attr_name, '_', lags, lag_unit)
    dyn_df[tmp_colnames] <- NA_real_
  }
  rm(j, tmp_colnames)
  
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
    for (i in 1:(length(date_ranges)+1)){
      for (j in 1:length(attr_prefix)){
        if (i == 1){
          dyn_df[dyn_df$lagged_Date <= date_ranges[i], 
                 c(paste0(attr_prefix[j], '_', attr_name,
                          '_', lags[lag], lag_unit[lag]))] <- 
            left_join(dyn_df %>% 
                        filter(lagged_Date <= date_ranges[i]) %>% 
                        select(seg), 
                      attrs %>% 
                        select(PRMS_segid, 
                               paste0(attr_prefix[j], '_', attr_years[i])),
                      by = c('seg' = 'PRMS_segid')) %>%
            select(paste0(attr_prefix[j], '_', attr_years[i]))
        }else if (i == (length(date_ranges)+1)){
          dyn_df[dyn_df$lagged_Date > date_ranges[i-1], 
                 c(paste0(attr_prefix[j], '_', attr_name,
                          '_', lags[lag], lag_unit[lag]))] <- 
            left_join(dyn_df %>% 
                        filter(lagged_Date > date_ranges[i-1]) %>% 
                        select(seg),
                      attrs %>% 
                        select(PRMS_segid, 
                               paste0(attr_prefix[j], '_', attr_years[i])),
                      by = c('seg' = 'PRMS_segid')) %>%
            select(paste0(attr_prefix[j], '_', attr_years[i]))
        }else{
          dyn_df[dyn_df$lagged_Date > date_ranges[i-1] & dyn_df$lagged_Date <= date_ranges[i],
                 c(paste0(attr_prefix[j], '_', attr_name, '_', lags[lag], lag_unit[lag]))] <- 
            left_join(dyn_df %>% 
                        filter(lagged_Date > date_ranges[i-1], 
                               lagged_Date <= date_ranges[i]) %>% 
                        select(seg),
                      attrs %>% 
                        select(PRMS_segid, 
                               paste0(attr_prefix[j], '_', attr_years[i])),
                      by = c('seg' = 'PRMS_segid')) %>%
            select(paste0(attr_prefix[j], '_', attr_years[i]))
        }
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