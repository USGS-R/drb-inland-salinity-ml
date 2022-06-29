munge_natural_baseflow <- function(baseflow_pred_files, segs_w_comids, vars=c("mean","med","p10","p90"),
                                   start_year, end_year, fill_all_years=TRUE){
  #' 
  #' @description This function reads in the DRB monthly baseflow tables from Miller et al. 2021 (https://doi.org/10.5066/P9FZG7GZ)
  #' and outputs a data frame containing monthly baseflow estimates for each PRMS segment. The Miller et al. dataset spans
  #' years 1950-2015. If variable {end_year} is later than 2015, monthly values will be filled with long-term monthly averages from
  #' {start_year} to 2015 unless stated otherwise (see param fill_all_years).
  #' 
  #' @param baseflow_pred_files file paths of unzipped monthly baseflow tables, including file extension
  #' @param segs_w_comids data frame containing the DRB PRMS segment ids and the comids of interest
  #' segs_w_comids must contain variables PRMS_segid and COMID
  #' @param vars columns from monthly baseflow tables to retain; can include one or all of "mean","med","p10",or "p90", 
  #' default is to retain all four columns
  #' @param start_year character string indicating the earliest year to return and to include in long-term average monthly baseflows
  #' @param end_year character string indicating the latest year to return and to include in long-term average monthly baseflows
  #' @param fill_all_years logical; indicates whether to fill missing years with long-term monthly baseflow values if {end_year} > 2015; 
  #' defaults to true
  #'                                           
  
  # Read in monthly baseflow files
  monthly_baseflow_ls <- lapply(baseflow_pred_files,function(x){
    
    # Extract the month for each monthly baseflow file
    # this line finds numbers with 2 digits within the file name string
    month <- str_extract(x, "[0-9]{2}")
    
    # Read in monthly tables
    data <- read_csv(x, col_types = 'cidddd', show_col_types = FALSE) %>%
      # reformat column names to include month
      rename_with(.cols = p10.Pred:mean.Pred, function(y){paste0(y,"_", month)}) %>%
      # subset dates
      filter(Year >= start_year, Year <= end_year)
    
    # Set up monthly tables for all PRMS_segid's
    latest_year_w_data <- max(data$Year)
    
    segs_w_comids_complete <- segs_w_comids %>%
      split(.,.$PRMS_segid) %>%
      lapply(.,function(segid){
        segs_w_comids_and_months <- tibble(
          Year = seq(start_year, latest_year_w_data, by = 1),
          PRMS_segid = rep(segid$PRMS_segid, length(Year)),
          COMID = rep(segid$COMID, length(Year))) 
      }) %>%
      bind_rows()
    
    # Filter monthly baseflow tables for desired COMIDs and arrange by year
    data_w_segids <- data %>%
      right_join(y = segs_w_comids_complete, by = c("COMID","Year")) %>%
      arrange(Year)
    
  })
  
  # Combine monthly data frames into one table and join by shared cols
  # "PRMS_segid", "COMID" and "Year"    
  monthly_baseflow <- Reduce(inner_join, monthly_baseflow_ls) %>%
    suppressMessages() %>%
    select(c(PRMS_segid,COMID,Year,contains(vars))) %>%
    # Pivot table to long format so that rows contain data for different months
    pivot_longer(!c(PRMS_segid,COMID,Year), names_to = "var_long", values_to = "Value") %>%
    # Clean up month/variable names
    mutate(Month = str_extract(var_long,"([^_]+$)"),
           Var = str_extract(var_long,".*(?=\\.)")) %>%
    select(PRMS_segid,COMID,Year,Var,Month,Value) %>%
    # Pivot table back to wide format so that desired vars are represented as separate columns
    pivot_wider(names_from = Var, values_from = Value) %>%
    rename_with(.cols = all_of(vars),function(x){paste0(x,"_natl_baseflow_cfs")})
  
  # For each COMID, calculate average monthly natural flow for years {start_year} to {end_year}
  monthly_baseflow_avg <- monthly_baseflow %>%
    group_by(PRMS_segid, COMID, Month) %>%
    summarize(across(contains(vars),mean),
              .groups = "drop")
  
  # If {fill_all_years} = TRUE and {end_year} > last year in monthly baseflow table, 
  # fill in missing years using long-term monthly averages
  if(fill_all_years){
    
    years <- seq(start_year, end_year, 1)
    years_to_fill <- years[which(years %in% unique(monthly_baseflow$Year) == "FALSE")]
    
    if(length(years_to_fill) > 0){
      monthly_baseflow_fill <- years_to_fill %>%
        lapply(.,function(x){
          monthly_baseflow_avg$Year <- x
          return(monthly_baseflow_avg)
          }) %>%
        bind_rows() %>%
        relocate("Year",.after = "COMID")
      
      # Bind dynamic monthly natural baseflow with long-term monthly averages to fill missing years
      monthly_baseflow_out <- bind_rows(monthly_baseflow, monthly_baseflow_fill) %>%
        select(-COMID)
      } else {
        monthly_baseflow_out <- monthly_baseflow %>%
          select(-COMID)
      }
  } else {
    monthly_baseflow_out <- monthly_baseflow %>%
      select(-COMID)
  }
  
  return(monthly_baseflow_out)
  
}
