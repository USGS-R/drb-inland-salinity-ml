find_sites_multipleTS <- function(site_list,earliest_date,latest_date,omit_sites=NULL,fileout){
#' 
#' @description Function to find NWIS instantaneous sites with multiple time series
#' These sites may require special handling for data download purposes. 
#'
#' @param site_list data frame containing site information for population of NWIS sites
#' @param earliest_date character string indicating the starting date for data retrieval (YYYY-MM-DD)
#' @param latest_date character string indicating the ending date for data retrieval (YYYY-MM-DD).
#' @param omit_sites character string indicating any NWIS site numbers to exclude. Default is NULL.
#' @param fileout file path and name for saved data table, including the file extension
#'
#' @examples 
#' find_sites_multipleTS(site_list = p1_nwis_sites,earliest_date = "1980-10-01",latest_date = "2020-09-30",fileout="1_fetch/summary_multiple_inst_ts.csv")
#'

multTS_table <- site_list %>%
  # retain "uv" sites that contain data records after user-specified {earliest_date}
  filter(data_type_cd=="uv",!(site_no %in% omit_sites),end_date > earliest_date) %>%
  group_by(site_no) %>% 
  # for active sites, set end_date equal to dummy_date and tally unique ts_id's
  mutate(end_date = case_when(end_date > latest_date ~ latest_date, TRUE ~ end_date),
         count_ts = length(unique(ts_id))) %>%
  # find sites with multiple time series
  filter(count_ts > 1) %>% 
  select(-count_nu)

# save record of instantaneous sites with multiple time series
  write_csv(multTS_table,fileout)
  
  return(fileout)

}
