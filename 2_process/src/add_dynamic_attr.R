add_dyn_attrs_to_reaches <- function(attrs, dyn_cols, start_date, end_date,
                                     baseflow = NULL, 
                                     CAT_Land = NULL, Upstream_Land = NULL, 
                                     gridMET = NULL,
                                     attr_prefix, Upstream_Land_prefix = NULL,
                                     lag_table){
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
  #' @param gridMET table of gridment data. Row length = dates x PRMS reach
  #' @param attr_prefix character vector of prefixes for the attributes 
  #' (e.g., CAT, TOT)
  #' @param Upstream_Land_prefix character vector of prefixes for the upstream 
  #' land cover attributes (e.g., TOT, ACC)
  #' @param lag_table table with 3 columns: attributes, lags, and lag_unit.
  #' This is used to compute the lagged attributes
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
  rm(df_tmp, dates, i)
  
  #Special handling for each variable
  if ('HDENS' %in% dyn_cols){
    #Housing density has the format HDENSYY
    tmp_attrs <- select(attrs, PRMS_segid, contains('HDENS'))
    
    #Convert the column names to 4-digit years
    col_years <- str_split(colnames(tmp_attrs)[-1], 
                           pattern = '_', simplify = TRUE)[,2] %>%
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
    rm(i, tmp_colnames, col_years)
    
    #HDENS data date ranges and dataset years
    tmp_date_ranges <- seq(as.Date('1965-09-30'), as.Date('2005-09-30'),
                           '10 years')
    tmp_years <- tmp_attrs %>% 
      select(starts_with(attr_prefix[1])) %>% 
      colnames() %>% 
      sort() %>% 
      str_split(pattern = '_', simplify = TRUE) %>% 
      .[,2]
    
    #add lagged dynamic information. Method includes value at lag of 0 days
    #Create df that starts at the (earliest date - largest lag) and goes to
    # earliest date
    df_lag <- get_lagged_dynamic_table(lag_table = lag_table, attr_name = 'HDENS',
                                       start_date = start_date, segs = segs)
    #Join to the full set of dates
    df_lag <- rbind(df_lag, df %>% select(Date, seg)) %>%
      arrange(seg, Date)
    
    #Assign the exact value of HDENS on those dates
    df_lag <- get_dynamic_from_static(dyn_df = df_lag, attrs = tmp_attrs, 
                                      lag_table = NULL,
                                      date_ranges = tmp_date_ranges,
                                      attr_years = tmp_years,
                                      attr_name = 'HDENS', 
                                      attr_prefix = attr_prefix)
    
    #Use the lag function to compute the desired attributes
    df <- cbind(df, compute_lagged_attrs(dyn_df = df_lag, 
                               lag_table = lag_table, 
                               lag_attr_name = 'HDENS', attr_name = 'HDENS',
                               start_date = start_date) %>%
                  select(-Date, -seg)
                )
    
    #Keeping this outside of the compute lagged attrs function because it's
    #faster to compute this way.
    if ('exact' %in% lag_table[lag_table$attribute == 'HDENS',]$lag_fxns[[1]]){
      df <- get_dynamic_from_static(dyn_df = df, attrs = tmp_attrs, 
                                        lag_table = lag_table,
                                        date_ranges = tmp_date_ranges,
                                        attr_years = tmp_years,
                                        attr_name = 'HDENS', 
                                        attr_prefix = attr_prefix)
    }
  }
  
  if ('MAJOR' %in% dyn_cols){
    #major dams has the format MAJORYYYY
    tmp_attrs <- get_attrs(attrs = attrs, attr_name = 'MAJOR',
                           replace_pattern = '_sum')
    
    #date ranges and dataset years
    tmp_date_ranges <- seq(as.Date('1975-09-30'), as.Date('2005-09-30'),
                           '10 years')
    tmp_years <- tmp_attrs %>% 
      select(starts_with(attr_prefix[1])) %>% 
      colnames() %>% 
      sort() %>% 
      str_split(pattern = '_', simplify = TRUE) %>% 
      .[,2]
    
    #add lagged dynamic information. Method includes value at lag of 0 days
    #Create df that starts at the (earliest date - largest lag) and goes to
    # earliest date
    df_lag <- get_lagged_dynamic_table(lag_table = lag_table, attr_name = 'MAJOR',
                                       start_date = start_date, segs = segs)
    #Join to the full set of dates
    df_lag <- rbind(df_lag, df %>% select(Date, seg)) %>%
      arrange(seg, Date)
    
    #Assign the exact value on those dates
    df_lag <- get_dynamic_from_static(dyn_df = df_lag, attrs = tmp_attrs, 
                                      lag_table = NULL,
                                      date_ranges = tmp_date_ranges,
                                      attr_years = tmp_years,
                                      attr_name = 'MAJOR', 
                                      attr_prefix = attr_prefix)
    
    #Use the lag function to compute the desired attributes
    df <- cbind(df, compute_lagged_attrs(dyn_df = df_lag, 
                                         lag_table = lag_table, 
                                         lag_attr_name = 'MAJOR', attr_name = 'MAJOR',
                                         start_date = start_date) %>%
                  select(-Date, -seg)
    )
    
    #Keeping this outside of the compute lagged attrs function because it's
    #faster to compute this way.
    if ('exact' %in% lag_table[lag_table$attribute == 'MAJOR',]$lag_fxns[[1]]){
      df <- get_dynamic_from_static(dyn_df = df, attrs = tmp_attrs, 
                                    lag_table = lag_table,
                                    date_ranges = tmp_date_ranges,
                                    attr_years = tmp_years,
                                    attr_name = 'MAJOR', 
                                    attr_prefix = attr_prefix)
    }
  }
  
  if ('NDAMS' %in% dyn_cols){
    #number of dams has the format NDAMSYYYY
    tmp_attrs <- get_attrs(attrs = attrs, attr_name = 'NDAMS',
                           replace_pattern = '_sum')
    
    #date ranges and dataset years
    tmp_date_ranges <- seq(as.Date('1975-09-30'), as.Date('2005-09-30'),
                           '10 years')
    tmp_years <- tmp_attrs %>% 
      select(starts_with(attr_prefix[1])) %>% 
      colnames() %>% 
      sort() %>% 
      str_split(pattern = '_', simplify = TRUE) %>% 
      .[,2]
    
    #add lagged dynamic information. Method includes value at lag of 0 days
    #Create df that starts at the (earliest date - largest lag) and goes to
    # earliest date
    df_lag <- get_lagged_dynamic_table(lag_table = lag_table, attr_name = 'NDAMS',
                                       start_date = start_date, segs = segs)
    #Join to the full set of dates
    df_lag <- rbind(df_lag, df %>% select(Date, seg)) %>%
      arrange(seg, Date)
    
    #Assign the exact value on those dates
    df_lag <- get_dynamic_from_static(dyn_df = df_lag, attrs = tmp_attrs, 
                                      lag_table = NULL,
                                      date_ranges = tmp_date_ranges,
                                      attr_years = tmp_years,
                                      attr_name = 'NDAMS', 
                                      attr_prefix = attr_prefix)
    
    #Use the lag function to compute the desired attributes
    df <- cbind(df, compute_lagged_attrs(dyn_df = df_lag, 
                                         lag_table = lag_table, 
                                         lag_attr_name = 'NDAMS', attr_name = 'NDAMS',
                                         start_date = start_date) %>%
                  select(-Date, -seg)
    )
    
    #Keeping this outside of the compute lagged attrs function because it's
    #faster to compute this way.
    if ('exact' %in% lag_table[lag_table$attribute == 'NDAMS',]$lag_fxns[[1]]){
      df <- get_dynamic_from_static(dyn_df = df, attrs = tmp_attrs, 
                                    lag_table = lag_table,
                                    date_ranges = tmp_date_ranges,
                                    attr_years = tmp_years,
                                    attr_name = 'NDAMS', 
                                    attr_prefix = attr_prefix)
    }
  }

  if ('NORM' %in% dyn_cols){
    #dam storage has the format STORAGEYYYY
    tmp_attrs <- get_attrs(attrs = attrs, attr_name = 'NORM_STORAGE',
                           replace_pattern = '_sum')
    
    #date ranges and dataset years
    tmp_date_ranges <- seq(as.Date('1975-09-30'), as.Date('2005-09-30'),
                           '10 years')
    tmp_years <- tmp_attrs %>% 
      select(starts_with(attr_prefix[1])) %>% 
      colnames() %>% 
      sort() %>% 
      str_split(pattern = '_', simplify = TRUE) %>% 
      .[,2]
    
    #add lagged dynamic information. Method includes value at lag of 0 days
    #Create df that starts at the (earliest date - largest lag) and goes to
    # earliest date
    df_lag <- get_lagged_dynamic_table(lag_table = lag_table, attr_name = 'NORM_STORAGE',
                                       start_date = start_date, segs = segs)
    #Join to the full set of dates
    df_lag <- rbind(df_lag, df %>% select(Date, seg)) %>%
      arrange(seg, Date)
    
    #Assign the exact value on those dates
    df_lag <- get_dynamic_from_static(dyn_df = df_lag, attrs = tmp_attrs, 
                                      lag_table = NULL,
                                      date_ranges = tmp_date_ranges,
                                      attr_years = tmp_years,
                                      attr_name = 'NORM_STORAGE', 
                                      attr_prefix = attr_prefix)
    
    #Use the lag function to compute the desired attributes
    df <- cbind(df, compute_lagged_attrs(dyn_df = df_lag, 
                                         lag_table = lag_table, 
                                         lag_attr_name = 'NORM_STORAGE', 
                                         attr_name = 'NORM_STORAGE',
                                         start_date = start_date) %>%
                  select(-Date, -seg)
    )
    
    #Keeping this outside of the compute lagged attrs function because it's
    #faster to compute this way.
    if ('exact' %in% lag_table[lag_table$attribute == 'NORM_STORAGE',]$lag_fxns[[1]]){
      df <- get_dynamic_from_static(dyn_df = df, attrs = tmp_attrs, 
                                    lag_table = lag_table,
                                    date_ranges = tmp_date_ranges,
                                    attr_years = tmp_years,
                                    attr_name = 'NORM_STORAGE', 
                                    attr_prefix = attr_prefix)
    }
  }
  
  if ('NID' %in% dyn_cols){
    #dam storage has the format STORAGEYYYY
    tmp_attrs <- get_attrs(attrs = attrs, attr_name = 'NID_STORAGE',
                           replace_pattern = '_sum')
    
    #date ranges and dataset years
    tmp_date_ranges <- seq(as.Date('1975-09-30'), as.Date('2005-09-30'),
                           '10 years')
    tmp_years <- tmp_attrs %>% 
      select(starts_with(attr_prefix[1])) %>% 
      colnames() %>% 
      sort() %>% 
      str_split(pattern = '_', simplify = TRUE) %>% 
      .[,2]
    
    #add lagged dynamic information. Method includes value at lag of 0 days
    #Create df that starts at the (earliest date - largest lag) and goes to
    # earliest date
    df_lag <- get_lagged_dynamic_table(lag_table = lag_table, attr_name = 'NID_STORAGE',
                                       start_date = start_date, segs = segs)
    #Join to the full set of dates
    df_lag <- rbind(df_lag, df %>% select(Date, seg)) %>%
      arrange(seg, Date)
    
    #Assign the exact value on those dates
    df_lag <- get_dynamic_from_static(dyn_df = df_lag, attrs = tmp_attrs, 
                                      lag_table = NULL,
                                      date_ranges = tmp_date_ranges,
                                      attr_years = tmp_years,
                                      attr_name = 'NID_STORAGE', 
                                      attr_prefix = attr_prefix)
    
    #Use the lag function to compute the desired attributes
    df <- cbind(df, compute_lagged_attrs(dyn_df = df_lag, 
                                         lag_table = lag_table, 
                                         lag_attr_name = 'NID_STORAGE', 
                                         attr_name = 'NID_STORAGE',
                                         start_date = start_date) %>%
                  select(-Date, -seg)
    )
    
    #Keeping this outside of the compute lagged attrs function because it's
    #faster to compute this way.
    if ('exact' %in% lag_table[lag_table$attribute == 'NID_STORAGE',]$lag_fxns[[1]]){
      df <- get_dynamic_from_static(dyn_df = df, attrs = tmp_attrs, 
                                    lag_table = lag_table,
                                    date_ranges = tmp_date_ranges,
                                    attr_years = tmp_years,
                                    attr_name = 'NID_STORAGE', 
                                    attr_prefix = attr_prefix)
    }
  }
  
  #Land Cover
  if (!is.null(CAT_Land) | !is.null(Upstream_Land)){
    #date ranges and dataset years
    tmp_date_ranges <- c(seq(as.Date('1945-09-30'), as.Date('1995-09-30'), by = '10 years'),
                         '2001-03-31', '2003-03-31', '2005-09-30', '2007-09-30',
                         '2010-03-31', '2012-09-30', '2015-03-31', '2018-03-31')
    tmp_years <- sort(unique(CAT_Land$Year))
    
    #Create df that starts at the (earliest date - largest lag) and goes to
    # earliest date
    df_lag <- get_lagged_dynamic_table(lag_table = lag_table, attr_name = 'Land',
                                       start_date = start_date, segs = segs)
    #Join to the full set of dates
    df_lag <- rbind(df_lag, df %>% select(Date, seg)) %>%
      arrange(seg, Date)
    
    #Get data table into format needed for function
    if (!is.null(CAT_Land)){
      tmp_attrs_cat <- pivot_wider(CAT_Land %>% 
                                     select(-PRMS_area_km2), 
                                   names_from = Year,
                                   values_from = starts_with('CAT'))
    }
    if (!is.null(Upstream_Land)){
      tmp_attrs_up <- pivot_wider(Upstream_Land %>% 
                                    select(-PRMS_area_km2), 
                                  names_from = Year, 
                                  values_from = starts_with(Upstream_Land_prefix))
    }
    
    #Loop over all land cover classes to assign attributes
    for (lc in 1:9){
      if (!is.null(CAT_Land)){
        tmp_attrs_cat_lc <- get_attrs(attrs = tmp_attrs_cat, 
                                      attr_name = paste0('_',lc,'_'), 
                                      replace_pattern = paste0('_prop_lcClass_', lc))
        
        #add lagged dynamic information. Method includes value at lag of 0 days
        #Assign the exact value on all days
        df_lag <- get_dynamic_from_static(dyn_df = df_lag, attrs = tmp_attrs_cat_lc, 
                                          lag_table = NULL,
                                          date_ranges = tmp_date_ranges,
                                          attr_years = tmp_years,
                                          attr_name = paste0('LC', lc), 
                                          attr_prefix = 'CAT')
      }
      if (!is.null(Upstream_Land)){
        tmp_attrs_up_lc <- get_attrs(attrs = tmp_attrs_up, 
                                     attr_name = paste0('_',lc,'_'), 
                                     replace_pattern = paste0('_prop_lcClass_', lc))
        
        #add lagged dynamic information. Method includes value at lag of 0 days
        #Assign the exact value on all days
        df_lag <- get_dynamic_from_static(dyn_df = df_lag, attrs = tmp_attrs_up_lc, 
                                          lag_table = NULL,
                                          date_ranges = tmp_date_ranges,
                                          attr_years = tmp_years,
                                          attr_name = paste0('LC', lc), 
                                          attr_prefix = Upstream_Land_prefix)
      }
    }
    
    #Use the lag function to compute the desired attributes
    df <- cbind(df, compute_lagged_attrs(dyn_df = df_lag, 
                                         lag_table = lag_table, 
                                         lag_attr_name = 'Land',
                                         attr_name = '',
                                         start_date = start_date) %>%
                  select(-Date, -seg)
    )
  }
    
  
  #gridMET data
  if (!is.null(gridMET)){
    #Reformat columns
    gridMET <- filter(gridMET, time <= end_date, time >= start_date) %>%
      #format for new variable is NAME_lag, lag = 0 for these variables
      rename(Date = time, seg = PRMS_segid, tmmx_0 = tmmx, tmmn_0 = tmmn, 
             pr_0 = pr, srad_0 = srad, 
             vs_0 = vs, rmax_0 = rmax, rmin_0 = rmin, sph_0 = sph)
    
    #add lagged dynamic information
    gridMET <- compute_lagged_attrs_from_dynamic(dyn_df = gridMET, 
                                                 lag_table = lag_table, 
                                                 lag_attr_name = 'Met',
                                                 start_date = start_date)
    
    #Join to df
    df <- left_join(df, gridMET, by = c('seg', 'Date'))
  }
  
  #Monthly baseflow data
  if (!is.null(baseflow)){
    #Add year and month columns (to be dropped after joining)
    df$Year <- year(df$Date)
    df$Month <- month(df$Date)
    #Remove the leading 0 for Month in baseflow
    baseflow$Month <- as.numeric(baseflow$Month)
    #format for new variable is NAME_lag, lag = 0 for these variables
    rename(baseflow, 
           mean_natl_baseflow_cfs_0 = mean_natl_baseflow_cfs,
           med_natl_baseflow_cfs_0 = med_natl_baseflow_cfs,
           p10_natl_baseflow_cfs_0 = p10_natl_baseflow_cfs,
           p90_natl_baseflow_cfs_0 = p90_natl_baseflow_cfs)
    #Join data to all segs by year and month
    df <- left_join(df, baseflow, by = c('seg' = 'PRMS_segid', 'Year', 'Month')) %>%
      select(-Year, -Month)
    
    #add lagged dynamic information
    #for baseflow, the start_date is the earliest available date, so this is 
    #the df_lag.
    df_lag <- df %>% 
      select(Date, seg, contains(baseflow))
    
    #Use the lag function to compute the desired attributes
    df <- cbind(df, compute_lagged_attrs_from_dynamic(dyn_df = df_lag, 
                                                      lag_table = lag_table, 
                                                      lag_attr_name = 'Baseflow',
                                                      start_date = start_date) %>%
                  select(-Date, -seg)
    )
  }

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

get_dynamic_from_static <- function(dyn_df, attrs, lag_table, 
                                    attr_years, date_ranges,
                                    attr_name, attr_prefix){
  #' @description computes dynamic attribute from the provided tbl of static
  #' attributes.
  #' 
  #' @param dyn_df tbl containing "Date" and the PRMS "seg". Should have all
  #' dates that you want to use in modeling. 
  #' @param attrs tbl containing the "PRMS_segid" and attribute columns
  #' in the format <attr_prefix>_YYYY
  #' @param lag_table table with 3 columns: attributes, lags, and lag_unit.
  #' This is used to compute the lagged attributes. Can set to NULL to use a 
  #' lag of 0.
  #' @param attr_years ordered character vector of the years available for this 
  #' dataset. Should be in ascending order.
  #' In attrs, for each attr_prefix, there should be one column for each year.
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
  
  if (is.null(lag_table)){
    lags <- 0
    lag_unit <- ''
  }else{
    lags <- lag_table %>% 
      filter(attribute == attr_name) %>%
      pull(lags) %>%
      unlist()
    lag_unit <- lag_table %>% 
      filter(attribute == attr_name) %>%
      pull(lag_unit) %>%
      unlist()
  }
  
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


get_lagged_dynamic_table <- function(lag_table, attr_name, start_date, segs){
  #' @description Computes the earliest lagged date and returns a tbl that
  #' starts at that date and ends at the start_date - 1 day. 
  #' 
  #' @param lag_table table with 3 columns: attributes, lags, and lag_unit.
  #' This is used to compute the lagged attributes
  #' @param attr_name character string of a name in the attribute column of lag_table
  #' @param start_date first date to compute attributes
  #' @param segs vector of the PRMS reach IDs
  #' 
  #' @return tbl with columns for Date and seg
  
  #Get the lags for this attribute
  tmp_lags <- lag_table %>% 
    filter(attribute == attr_name) %>%
    pull(lags) %>%
    unlist()
  tmp_lag_unit <- lag_table %>% 
    filter(attribute == attr_name) %>%
    pull(lag_unit) %>%
    unlist()
  
  #Make the length of the lag_unit match the length of lags
  if ((length(tmp_lag_unit) == 1) & (length(tmp_lags) > 1)){
    tmp_lag_unit <- rep(tmp_lag_unit, length(tmp_lags))
  }
  
  #Find the earliest date: (start_date - largest lag)
  for (lag in 1:length(tmp_lags)){
    if(lag == 1){
      min_lag_date <- as.Date(start_date) %m-% 
        period(paste(tmp_lags[lag], tmp_lag_unit[lag]))
    }else{
      min_lag_date <- min(min_lag_date, 
                          as.Date(start_date) %m-% 
                            period(paste(tmp_lags[lag], tmp_lag_unit[lag])))
    }
  }
  
  #Create the tbl
  dates <- seq(min_lag_date, as.Date(start_date) %m-% period(1, 'day'), 1)
  df <- tibble(Date = dates, seg = segs[1])
  for(i in 2:length(segs)){
    df_tmp <- tibble(Date = dates, seg = segs[i])
    df <- rbind(df, df_tmp)
  }
  
  return(df)
}


compute_lagged_attrs <- function(dyn_df, lag_table, lag_attr_name, 
                                 attr_name, start_date){
  #' @description computes lagged predictor variables from the provided features
  #' 
  #' @param dyn_df tbl containing "Date" and the PRMS "seg". Should have all
  #' dates that you want to use for lagged computations.
  #' @param lag_table table with 3 columns: attributes, lags, and lag_unit.
  #' This is used to compute the lagged attributes
  #' @param lag_attr_name name in the attribute column of lag_table to use
  #' @param attr_name name to use for this attribute. Format of the new name will
  #' be paste0(attr_prefix, '_', attr_name, '_', lags, lag_unit)
  #' @param start_date first date to compute attributes
  #' 
  #' @return tbl with the added lagged features. Date begins at the start_date
  #' and ends at the latest provided date in dyn_df.
  
  #Get the lag information and lag functions to be used
  tmp_lags <- lag_table %>% 
    filter(attribute == lag_attr_name) %>%
    pull(lags) %>%
    unlist()
  tmp_lag_unit <- lag_table %>% 
    filter(attribute == lag_attr_name) %>%
    pull(lag_unit) %>%
    unlist()
  #Make the length of the lag_unit match the length of lags
  if ((length(tmp_lag_unit) == 1) & (length(tmp_lags) > 1)){
    tmp_lag_unit <- rep(tmp_lag_unit, length(tmp_lags))
  }
  tmp_lag_fxns <- lag_table %>% 
    filter(attribute == lag_attr_name) %>%
    pull(lag_fxns) %>%
    unlist()
  
  if (length(tmp_lag_fxns) > 0){
    #Loop over all functions except exact
    for (f in 1:length(tmp_lag_fxns)){
      if (tmp_lag_fxns[f] != 'exact'){
        #Loop over all lags
        for (lag in 1:length(tmp_lags)){
          #get the number of days to lag
          days_lag <- as.Date(start_date) - 
            (as.Date(start_date) - period(tmp_lags[lag], tmp_lag_unit[lag]))
          
          #Compute the lagged values
          if (tmp_lag_fxns[f] == 'mean'){
            dyn_df <- cbind(dyn_df, 
                            dyn_df %>% 
                              group_by(seg) %>% 
                              summarise(across(.cols = ends_with(paste0(attr_name, '_0')), 
                                               .fns = stats::filter, 
                                               filter = rep(1/as.numeric(days_lag), days_lag), 
                                               sides = 1, 
                                               .names = paste0("{.col}_", tmp_lags[lag], 
                                                               tmp_lag_unit[lag], "_mean")), 
                                        .groups = 'drop') %>%
                              select(-seg) %>%
                              #need to shift the results by 1 day because filter 
                              #includes the current day in the lag
                              add_row(.before = 1) %>%
                              slice(-nrow(.))
            )
          }else{
            stop(print('Error: the function', tmp_lag_fxns[f], 
                       'is not one of the available functions for lags.'))
          }
        }
      }
    }
  }
  
  #Remove the dates before the start_date
  dyn_df <- dyn_df %>% 
    filter(Date >= start_date)
  
  return(dyn_df)
}


compute_lagged_attrs_from_dynamic <- function(dyn_df, lag_table, lag_attr_name,
                                              start_date){
  #' @description computes lagged predictor variables from the provided features
  #' 
  #' @param dyn_df tbl containing "Date" and the PRMS "seg". Should have all
  #' dates that you want to use for lagged computations.
  #' @param lag_table table with 3 columns: attributes, lags, and lag_unit.
  #' This is used to compute the lagged attributes
  #' @param lag_attr_name name in the attribute column of lag_table to use
  #' @param start_date first date to compute attributes
  #' 
  #' @return tbl with the added lagged features. Date begins at the start_date
  #' and ends at the latest provided date in dyn_df.
  
  #Get the lag information and lag functions to be used
  tmp_lags <- lag_table %>% 
    filter(attribute == lag_attr_name) %>%
    pull(lags) %>%
    unlist()
  tmp_lag_unit <- lag_table %>% 
    filter(attribute == lag_attr_name) %>%
    pull(lag_unit) %>%
    unlist()
  #Make the length of the lag_unit match the length of lags
  if ((length(tmp_lag_unit) == 1) & (length(tmp_lags) > 1)){
    tmp_lag_unit <- rep(tmp_lag_unit, length(tmp_lags))
  }
  tmp_lag_fxns <- lag_table %>% 
    filter(attribute == lag_attr_name) %>%
    pull(lag_fxns) %>%
    unlist()
  
  if (length(tmp_lag_fxns) > 0){
    #Loop over all functions except exact
    for (f in 1:length(tmp_lag_fxns)){
      if (tmp_lag_fxns[f] != 'exact'){
        #Loop over all lags
        for (lag in 1:length(tmp_lags)){
          #get the number of days to lag
          days_lag <- as.Date(start_date) - 
            (as.Date(start_date) - period(tmp_lags[lag], tmp_lag_unit[lag]))
          
          #Compute the lagged values
          if (tmp_lag_fxns[f] == 'mean'){
            dyn_df <- cbind(dyn_df, 
                            dyn_df %>% 
                              group_by(seg) %>% 
                              summarise(across(.cols = ends_with('_0'), 
                                               .fns = stats::filter, 
                                               filter = rep(1/as.numeric(days_lag), days_lag), 
                                               sides = 1, 
                                               .names = paste0("{.col}_", tmp_lags[lag], 
                                                               tmp_lag_unit[lag], "_mean")), 
                                        .groups = 'drop') %>%
                              select(-seg) %>%
                              #need to shift the results by 1 day because filter 
                              #includes the current day in the lag
                              add_row(.before = 1) %>%
                              slice(-nrow(.))
            )
          }else{
            stop(print('Error: the function', tmp_lag_fxns[f], 
                       'is not one of the available functions for lags.'))
          }
        }
      }
    }
  }
  
  return(dyn_df)
}

get_attrs <- function(attrs, attr_name, replace_pattern){
  #' @description selects the attr_name attributes and creates column names that are
  #' compatible with other functions.
  #' @param attrs tbl of static attributes (columns) for each reach (rows)
  #' @param attr_name all column names in attrs containing attr_name will be selected
  #' @param replace_pattern gsub pattern to remove from the column names
  #' 
  #' @return tbl of the selected attributes with corrected column names
  
  tmp_attrs <- select(attrs, PRMS_segid, contains(attr_name))
  tmp_colnames <- colnames(tmp_attrs)
  for(i in 2:length(tmp_colnames)){
    tmp_colnames[i] <- gsub(x = tmp_colnames[i], pattern = replace_pattern, 
                            replacement = '') %>%
      gsub(pattern = attr_name, 
           replacement = '')
  }
  colnames(tmp_attrs) <- tmp_colnames
  
  return(tmp_attrs)
}

#' plot_dynamic_attrs <- function(dyn_attrs, sites, out_dir){
#'   #' @description plots timeseries of dynamic features for the provided sites
#'   #' 
#'   #' @param 
#'   #' @param lags vector stating how many days to lag. A column will be added for each element.
#'   #' 
#'   #' @return filepaths to the resulting plots
#'   
#'   # plot(df_lag$Date[df_lag$seg == '157_1'], df_lag$CAT_HDENS_0[df_lag$seg == '157_1'], ylim = c(40,80), type = 'l')
#'   # par(new = T)
#'   # plot(df_lag$Date[df_lag$seg == '157_1'], df_lag$CAT_HDENS_0_10years[df_lag$seg == '157_1'], type = 'l', col = 'red', ylim = c(40,80))
#'   # par(new = T)
#'   # plot(df_lag$Date[df_lag$seg == '157_1'], df_lag$CAT_HDENS_0_20years[df_lag$seg == '157_1'], type = 'l', col = 'blue', ylim = c(40,80))
#'   
#'   fileout
#'   
#'   return(fileout)
#' }