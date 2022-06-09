add_dyn_attrs_to_reaches <- function(attrs, dyn_cols, start_date, end_date){
  #' @description computes dynamic attributes for each reach based on the provided dates
  #' 
  #' @param attrs table of static attributes for each reach
  #' @param dyn_cols vector of character names to use for dynamic columns
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
    #Housing density has the format HDENS00
    tmp_attrs <- select(attrs, PRMS_segid, contains('HDENS'))
    
    #Convert the variable names to years
    col_years = str_split(colnames(tmp_attrs)[-1], pattern = '_', simplify = T)[,2] %>%
      substr(start = 6, stop = 7) %>%
      get_four_digit_year()
    tmp_colnames <- colnames(tmp_attrs)
    for(i in 2:length(tmp_colnames)){
      tmp_colnames[i] <- gsub(x = tmp_colnames[i], pattern = 'HDENS+[0-9]+[0-9]', 
                              replacement = col_years[i-1])
    }
    colnames(tmp_attrs) <- tmp_colnames
    rm(tmp_colnames)
    
    #format for new variable is NAME_lag, lag = 0 for these variables
    df$CAT_HDENS_0 = NA_real_
    df$TOT_HDENS_0 = NA_real_
    
    #This is too slow - let's join all segs by date range
    date_ranges <- seq(as.Date('1965-09-30'), as.Date('2005-09-30'), by = '10 years')
    for (i in 1:length(date_ranges)){
      if (i == 1){
        df$CAT_HDENS_0[df$Date <= date_ranges[i]] <- left_join(df %>% filter(Date <= date_ranges[i]) %>% select(seg), 
                                                               tmp_attrs %>% select(PRMS_segid, CAT_1960_area_wtd),
                                                               by = c('seg' = 'PRMS_segid'))
      }
    }
    for (i in 1:length(unique(df$seg))){
      #Identify the center dates for decadal and annual information
      #this is a decadal timeseries, so 5 yr windows
      #if at the end of a timeseries, assign the most recent observation
      df <- mutate(df, CAT_HDENS_0 = case_when(
        Date <= '1965-09-30' ~ tmp_attrs$CAT_1960_area_wtd[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '1975-09-30' & Date > '1965-09-30' ~ tmp_attrs$CAT_1970_area_wtd[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '1985-09-30' & Date > '1975-09-30' ~ tmp_attrs$CAT_1980_area_wtd[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '1995-09-30' & Date > '1985-09-30' ~ tmp_attrs$CAT_1990_area_wtd[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '2005-09-30' & Date > '1995-09-30' ~ tmp_attrs$CAT_2000_area_wtd[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date > '2005-09-30' ~ tmp_attrs$CAT_2010_area_wtd[tmp_attrs$PRMS_segid == unique(df$seg)[i]],))
      
      df <- mutate(df, TOT_HDENS_0 = case_when(
        Date <= '1965-09-30' ~ tmp_attrs$TOT_1960[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '1975-09-30' & Date > '1965-09-30' ~ tmp_attrs$TOT_1970[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '1985-09-30' & Date > '1975-09-30' ~ tmp_attrs$TOT_1980[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '1995-09-30' & Date > '1985-09-30' ~ tmp_attrs$TOT_1990[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '2005-09-30' & Date > '1995-09-30' ~ tmp_attrs$TOT_2000[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date > '2005-09-30' ~ tmp_attrs$TOT_2010[tmp_attrs$PRMS_segid == unique(df$seg)[i]],))
    }
  }
  
  if ('MAJOR' %in% dyn_cols){
    #major dams has the format MAJORYYYY
    tmp_attrs <- select(attrs, PRMS_segid, contains('MAJOR'))
    
    #format for new variable is NAME_lag, lag = 0 for these variables
    df$CAT_MAJOR_0 = NA_real_
    df$TOT_MAJOR_0 = NA_real_
    
    for (i in 1:length(unique(df$seg))){
      #Identify the center dates for decadal and annual information
      #this is a decadal timeseries, so 5 yr windows
      #if at the end of a timeseries, assign the most recent observation
      df <- mutate(df, CAT_MAJOR_0 = case_when(
        Date <= '1965-09-30' ~ tmp_attrs$CAT_MAJOR1960_area_wtd[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '1975-09-30' & Date > '1965-09-30' ~ tmp_attrs$CAT_MAJOR1970_area_wtd[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '1985-09-30' & Date > '1975-09-30' ~ tmp_attrs$CAT_MAJOR1980_area_wtd[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '1995-09-30' & Date > '1985-09-30' ~ tmp_attrs$CAT_MAJOR1990_area_wtd[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '2005-09-30' & Date > '1995-09-30' ~ tmp_attrs$CAT_MAJOR2000_area_wtd[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date > '2005-09-30' ~ tmp_attrs$CAT_MAJOR2010_area_wtd[tmp_attrs$PRMS_segid == unique(df$seg)[i]],))
      
      df <- mutate(df, TOT_MAJOR_0 = case_when(
        Date <= '1965-09-30' ~ tmp_attrs$TOT_MAJOR1960[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '1975-09-30' & Date > '1965-09-30' ~ tmp_attrs$TOT_MAJOR1970[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '1985-09-30' & Date > '1975-09-30' ~ tmp_attrs$TOT_MAJOR1980[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '1995-09-30' & Date > '1985-09-30' ~ tmp_attrs$TOT_MAJOR1990[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '2005-09-30' & Date > '1995-09-30' ~ tmp_attrs$TOT_MAJOR2000[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date > '2005-09-30' ~ tmp_attrs$TOT_MAJOR2010[tmp_attrs$PRMS_segid == unique(df$seg)[i]],))
    }
  }
  
  if ('NDAMS' %in% dyn_cols){
    #number of dams has the format NDMASYYYY
    tmp_attrs <- select(attrs, PRMS_segid, contains('NDAMS'))
    
    #format for new variable is NAME_lag, lag = 0 for these variables
    df$CAT_NDAMS_0 = NA_real_
    df$TOT_NDAMS_0 = NA_real_
    
    for (i in 1:length(unique(df$seg))){
      #Identify the center dates for decadal and annual information
      #this is a decadal timeseries, so 5 yr windows
      #if at the end of a timeseries, assign the most recent observation
      df <- mutate(df, CAT_NDAMS_0 = case_when(
        Date <= '1965-09-30' ~ tmp_attrs$CAT_NDAMS1960_area_wtd[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '1975-09-30' & Date > '1965-09-30' ~ tmp_attrs$CAT_NDAMS1970_area_wtd[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '1985-09-30' & Date > '1975-09-30' ~ tmp_attrs$CAT_NDAMS1980_area_wtd[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '1995-09-30' & Date > '1985-09-30' ~ tmp_attrs$CAT_NDAMS1990_area_wtd[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '2005-09-30' & Date > '1995-09-30' ~ tmp_attrs$CAT_NDAMS2000_area_wtd[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date > '2005-09-30' ~ tmp_attrs$CAT_NDAMS2010_area_wtd[tmp_attrs$PRMS_segid == unique(df$seg)[i]],))
      
      df <- mutate(df, TOT_NDAMS_0 = case_when(
        Date <= '1965-09-30' ~ tmp_attrs$TOT_NDAMS1960[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '1975-09-30' & Date > '1965-09-30' ~ tmp_attrs$TOT_NDAMS1970[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '1985-09-30' & Date > '1975-09-30' ~ tmp_attrs$TOT_NDAMS1980[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '1995-09-30' & Date > '1985-09-30' ~ tmp_attrs$TOT_NDAMS1990[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '2005-09-30' & Date > '1995-09-30' ~ tmp_attrs$TOT_NDAMS2000[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date > '2005-09-30' ~ tmp_attrs$TOT_NDAMS2010[tmp_attrs$PRMS_segid == unique(df$seg)[i]],))
    }
  }

  if ('NORM' %in% dyn_cols){
    #dam storage has the formate STORAGEYYYY
    tmp_attrs <- select(attrs, PRMS_segid, contains('NORM'))
    
    #format for new variable is NAME_lag, lag = 0 for these variables
    df$CAT_NORM_0 = NA_real_
    df$TOT_NORM_0 = NA_real_
    
    for (i in 1:length(unique(df$seg))){
      #Identify the center dates for decadal and annual information
      #this is a decadal timeseries, so 5 yr windows
      #if at the end of a timeseries, assign the most recent observation
      df <- mutate(df, CAT_NORM_0 = case_when(
        Date <= '1965-09-30' ~ tmp_attrs$CAT_NORM1960_area_wtd[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '1975-09-30' & Date > '1965-09-30' ~ tmp_attrs$CAT_NORM1970_area_wtd[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '1985-09-30' & Date > '1975-09-30' ~ tmp_attrs$CAT_NORM1980_area_wtd[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '1995-09-30' & Date > '1985-09-30' ~ tmp_attrs$CAT_NORM1990_area_wtd[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '2005-09-30' & Date > '1995-09-30' ~ tmp_attrs$CAT_NORM2000_area_wtd[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date > '2005-09-30' ~ tmp_attrs$CAT_NORM2010_area_wtd[tmp_attrs$PRMS_segid == unique(df$seg)[i]],))
      
      df <- mutate(df, TOT_NORM_0 = case_when(
        Date <= '1965-09-30' ~ tmp_attrs$TOT_NORM1960[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '1975-09-30' & Date > '1965-09-30' ~ tmp_attrs$TOT_NORM1970[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '1985-09-30' & Date > '1975-09-30' ~ tmp_attrs$TOT_NORM1980[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '1995-09-30' & Date > '1985-09-30' ~ tmp_attrs$TOT_NORM1990[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '2005-09-30' & Date > '1995-09-30' ~ tmp_attrs$TOT_NORM2000[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date > '2005-09-30' ~ tmp_attrs$TOT_NORM2010[tmp_attrs$PRMS_segid == unique(df$seg)[i]],))
    }
  }
  
  if ('NID' %in% dyn_cols){
    #dam storage has the formate STORAGEYYYY
    tmp_attrs <- select(attrs, PRMS_segid, contains('NID'))
    
    #format for new variable is NAME_lag, lag = 0 for these variables
    df$CAT_NID_0 = NA_real_
    df$TOT_NID_0 = NA_real_
    
    for (i in 1:length(unique(df$seg))){
      #Identify the center dates for decadal and annual information
      #this is a decadal timeseries, so 5 yr windows
      #if at the end of a timeseries, assign the most recent observation
      df <- mutate(df, CAT_NID_0 = case_when(
        Date <= '1965-09-30' ~ tmp_attrs$CAT_NID1960_area_wtd[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '1975-09-30' & Date > '1965-09-30' ~ tmp_attrs$CAT_NID1970_area_wtd[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '1985-09-30' & Date > '1975-09-30' ~ tmp_attrs$CAT_NID1980_area_wtd[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '1995-09-30' & Date > '1985-09-30' ~ tmp_attrs$CAT_NID1990_area_wtd[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '2005-09-30' & Date > '1995-09-30' ~ tmp_attrs$CAT_NID2000_area_wtd[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date > '2005-09-30' ~ tmp_attrs$CAT_NID2010_area_wtd[tmp_attrs$PRMS_segid == unique(df$seg)[i]],))
      
      df <- mutate(df, TOT_NID_0 = case_when(
        Date <= '1965-09-30' ~ tmp_attrs$TOT_NID1960[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '1975-09-30' & Date > '1965-09-30' ~ tmp_attrs$TOT_NID1970[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '1985-09-30' & Date > '1975-09-30' ~ tmp_attrs$TOT_NID1980[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '1995-09-30' & Date > '1985-09-30' ~ tmp_attrs$TOT_NID1990[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date <= '2005-09-30' & Date > '1995-09-30' ~ tmp_attrs$TOT_NID2000[tmp_attrs$PRMS_segid == unique(df$seg)[i]],
        Date > '2005-09-30' ~ tmp_attrs$TOT_NID2010[tmp_attrs$PRMS_segid == unique(df$seg)[i]],))
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