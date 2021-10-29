filter_wqp_salinity_data <- function(data,major_ion_names,select_wqp_vars,omit_wqp_events,exclude_tidal=TRUE){
  
  # major_ion_names are values to keep from the param column of the harmonized wqp data
  # select_wqp_vars are columns of data to keep from the harmonized wqp data
  # omit_wqp_events are values to omit from HydrologicEvent column of the harmonized wqp data
  # exclude_tidal boolean; if TRUE, rows containing "tidal" within the MonitoringLocationTypeName column of the harmonized wqp data are omitted
  
  data_subset <- filter(data,(param_group=="Salinity"|param %in% major_ion_names),
                        # Filter out any sediment samples, samples representing hydrologic events that are not of interest, and samples from LocationType = ditch:
                        ActivityMediaName!="Sediment",!(HydrologicEvent %in% omit_wqp_events),MonitoringLocationTypeName != "Stream: Ditch",
                        # Keep QA/QC'ed data deemed reliable:
                        final=="retain") %>%
    # Filter out any tidal samples if exclude_tidal = TRUE:
    {if(exclude_tidal==TRUE){
      filter(.,!grepl("tidal", MonitoringLocationTypeName,ignore.case = TRUE))
      } else {.}
    } %>% 
    select(all_of(select_wqp_vars))
  
  return(data_subset)
  
}


subset_wqp_spC_data <- function(filtered_data,fileout){
  
  # filtered_data is output from filter_wqp_salinity_data function
  # fileout is the name of the file to be saved, including path and file extension
  
  # Filter out specific conductance param values "min" and "max"
  SpC_params <- c("Specific conductance, field",
                  "Specific conductance",
                  "Specific conductance, field, mean",
                  "Specific conductance, lab")

  SpC_data_subset <- filtered_data %>%
    # Omit samples originally entered as "conductivity" since we can't be sure these reflect temperature-corrected conductance
    filter(param %in% SpC_params,CharacteristicName!="Conductivity") 

  write_csv(SpC_data_subset, file = fileout)
  
  return(fileout)
  
}

