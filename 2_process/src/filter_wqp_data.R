filter_wqp_salinity_data <- function(data,major_ion_names,wqp_vars_select,omit_wqp_events,exclude_tidal=TRUE){
  #' 
  #' @description Function to filter the DRB multisource surface-water-quality dataset for desired parameters, units, and variables  
  #'
  #' @param data a data frame containing the downloaded DRB multisource surface-water-quality dataset
  #' @param major_ion_names a character vector containing the desired parameter values from the data column "param". 
  #' See Shoda et al. 2019 (https://doi.org/10.5066/P9PX8LZO) for more information.  
  #' @param wqp_vars_select a character vector indicating which data columns to retain from the DRB multisource dataset
  #' @param omit_wqp_events a character vector indicating which values to omit from the "HydrologicEvent" column within the DRB multisource dataset
  #' @param exclude_tidal logical, defaults to TRUE. If TRUE, rows containing "tidal" within the "MonitoringLocationTypeName" column of the
  #' DRB multisource dataset will be omitted. 
  #'
  #' @value A data frame containing discrete water quality samples from the Delaware River Basin for the parameters of interest
  #' @examples 
  #' filter_wqp_data(data = DRB_WQdata,params_select=c("Dissolved oxygen"),wqp_vars_select=c("MonitoringLocationIdentifier","MonitoringLocationName"),
  #' omit_wqp_events=c("Volcanic action"),fileout="./data/out/filtered_wqp_data.csv")
  
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
    select(all_of(wqp_vars_select))
  
  return(data_subset)
  
}


subset_wqp_SC_data <- function(filtered_data){
  #' 
  #' @description Function to subset the filtered WQP salinity dataset for specific conductance  
  #'
  #' @param filtered_data a data frame containing the filtered DRB multisource surface-water-quality dataset.
  #' Filtered_data is the output from filter_wqp_salinity_data().
  #'
  #' @value A data frame containing discrete specific conductance samples from the Delaware River Basin 
  #' @examples 
  #' subset_wqp_SC_data(filtered_data = filtered_wqp_data)

  # Filter out specific conductance param values "min" and "max"
  SC_params <- c("Specific conductance, field",
                  "Specific conductance",
                  "Specific conductance, field, mean",
                  "Specific conductance, lab")

  SC_data_subset <- filtered_data %>%
    # Omit samples originally entered as "conductivity" since we can't be sure these reflect temperature-corrected conductance
    filter(param %in% SC_params,CharacteristicName!="Conductivity") 

  return(SC_data_subset)
  
}

