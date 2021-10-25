summarize_wqp_salinity_data <- function(data){
  
  records_summary <- data %>% 
    filter(final=="retain") %>%
    filter(param_group=="Salinity"|param=="Chloride"|param=="Sodium") %>% 
    group_by(param_group,param) %>% 
    summarize(n_records = n(),n_sites = length(unique(MonitoringLocationIdentifier))) 
  
  return(records_summary)
  
}

subset_wqp_major_ions_data <- function(data,param){
  
  data_subset <- filter(data,param==param & final=="retain") %>%
    select(MonitoringLocationIdentifier,LongitudeMeasure,LatitudeMeasure,CharacteristicName,param,USGSPCode,ActivityStartDate,ActivityEndDate,resultVal2,resultUnits2)
  
  return(data_subset)
  
}


subset_wqp_spC_data <- function(data,fileout){
  
  # Filter out specific conductance param values "min" and "max"
  SpC_params <- c("Specific conductance, field",
                  "Specific conductance",
                  "Specific conductance, field, mean",
                  "Specific conductance, lab")
  
  spC_data_subset <- filter(data,param_group=="Salinity" & param %in% SpC_params & final=="retain") %>%
    select(MonitoringLocationIdentifier,LongitudeMeasure,LatitudeMeasure,CharacteristicName,param,USGSPCode,ActivityStartDate,ActivityEndDate,resultVal2,resultUnits2)
  
  write_csv(spC_data_subset, file = fileout)
  
  return(fileout)
  
}

