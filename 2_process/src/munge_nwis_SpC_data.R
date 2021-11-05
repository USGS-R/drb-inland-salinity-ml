munge_daily_mean_SpC_cols <- function(x){
  
  # x is a data frame containing downloaded daily data from NWIS
  
  # function identifies whether multiple columns contain SpC data, for example due to different locations. If so, use the data in the order specified in the daily data frame downloaded from NWIS.
  # returns a data frame of daily NWIS data with harmonized columns "SpecCond" and "SpecCond_cd"
  
  daily_data_rename <- renameNWISColumns(x)
  
  # Find which column(s) contain specific conductance data and relevant qualifying codes:
  vector_SpC_vars <- grep("SpecCond$",names(daily_data_rename),value=TRUE)
  vector_SpC_cd_vars <- grep("SpecCond_cd$",names(daily_data_rename),value=TRUE)
  
  # Coalesce multiple specific conductance columns if applicable:
  daily_data_out <- daily_data_rename %>% 
    mutate(SpecCond_out = coalesce(!!!syms(vector_SpC_vars)),
           SpecCond_cd_out = coalesce(!!!syms(vector_SpC_cd_vars))) %>%
    select(agency_cd,site_no,Date,SpecCond_out,SpecCond_cd_out) %>%
    rename("SpecCond"="SpecCond_out","SpecCond_cd"="SpecCond_cd_out") 
  
  return(daily_data_out)
  
  
}


combine_daily_mean_SpC_data <- function(daily_data_nwis,fileout){
  
  # daily_data_nwis is a list containing the downloaded daily data for each SpC site within the DRB
  
  # returns a data table of daily specific conductance values from NWIS daily sites
  
  # Munge daily site data columns
  daily_data_nwis_munged <- lapply(daily_data_nwis,munge_daily_mean_SpC_cols)
  
  # Combine daily site data and save to fileout
  daily_data_out <- do.call(rbind,daily_data_nwis_munged)
  
  write_csv(daily_data_out, file = fileout)

  return(fileout)
  
}
