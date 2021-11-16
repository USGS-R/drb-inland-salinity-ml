munge_daily_mean_SpC_cols <- function(x){
  
  # x is a data frame containing downloaded daily data from NWIS
  
  # function identifies whether multiple columns contain SpC data, for example due to different locations. If so, use the data in the order specified in the daily data frame downloaded from NWIS.
  # returns a data frame of daily NWIS data with harmonized columns "SpecCond", "SpecCond_cd", "SpecCond_Max", and "SpecCond_Max_cd"

  daily_data_rename <- x %>% renameNWISColumns(p00095 = "Value") %>% 
    as_tibble() 
  
  # Munge column names for some sites with different or unusually-named value columns
  if(length(grep("Value_cd",names(daily_data_rename)))>1){
    daily_data_rename <- switch(x$site_no[1],
                 "01463500" = daily_data_rename %>%
                   mutate(Value_merged = coalesce(`Value`,`..2.._Value`),
                          Value_cd_merged = coalesce(`Value_cd`,`..2.._Value_cd`),
                          Value_Max_merged = coalesce(`Value_Max`,`..2.._Value_Max`),
                          Value_Max_cd_merged = coalesce(`Value_Max_cd`,`..2.._Value_Max_cd`)) %>%
                   select(agency_cd,site_no,Date,Value_merged,Value_cd_merged,Value_Max_merged,Value_Max_cd_merged) %>%
                   rename("Value"="Value_merged","Value_cd" = "Value_cd_merged","Value_Max"="Value_Max_merged","Value_Max_cd"="Value_Max_cd_merged"),
                 "01467200" = daily_data_rename %>%
                   mutate(Value_merged = coalesce(`Value`,`ISM.Test.Bed...ISM.barge._Value`),
                          Value_cd_merged = coalesce(`Value_cd`,`ISM.Test.Bed...ISM.barge._Value_cd`)) %>%
                   select(agency_cd,site_no,Date,Value_merged,Value_cd_merged,Value_Max,Value_Max_cd) %>%
                   rename("Value"="Value_merged","Value_cd"="Value_cd_merged"),
                 "01412350" = daily_data_rename %>%
                   mutate(Value = `.Discontinued._Value`,
                          Value_cd = `.Discontinued._Value_cd`,
                          Value_Max = `.Discontinued._Value_Max`,
                          Value_Max_cd = `.Discontinued._Value_Max_cd`) %>%
                   select(agency_cd,site_no,Date,Value,Value_cd,Value_Max,Value_Max_cd))
    }
  
  # If no max value reported, create empty column:
  if(!('Value_Max' %in% names(daily_data_rename))){
    daily_data_rename <- daily_data_rename %>%
      add_column(Value_Max = NA,
                 Value_Max_cd = NA)
    }

  # Rename columns:
  daily_data_out <- daily_data_rename %>% 
    rename("SpecCond"="Value","SpecCond_cd"="Value_cd","SpecCond_Max"="Value_Max","SpecCond_Max_cd"="Value_Max_cd") 
  
  return(daily_data_out)
  
  
}


combine_daily_mean_SpC_data <- function(daily_data_nwis,fileout){
  
  # daily_data_nwis is a list containing the downloaded daily data for each SpC site within the DRB
  
  # returns a data table of daily specific conductance values from NWIS daily sites
  
  # Munge daily site data columns
  daily_data_nwis_munged <- lapply(daily_data_nwis,munge_daily_mean_SpC_cols)
  
  # Combine daily site data 
  daily_data_out <- do.call(rbind,daily_data_nwis_munged) %>%
           # remove rows with no data for either SpecCond or SpecCond_Max:
    filter(!(is.na(SpecCond) & is.na(SpecCond_Max)),
           # remove rows where daily mean SC is greater than daily max SC while keeping rows with NA in just one column:
           (is.na(SpecCond)|is.na(SpecCond_Max)|(!SpecCond > SpecCond_Max)))

  # Save daily site data to fileout
  write_csv(daily_data_out, file = fileout)

  return(fileout)
  
}
