get_nwis_sites <- function(hucs,pcodes_select,site_tp_select){
  
  # hucs is a character vector containing the minor HUCs (hydrologic unit codes) over which to search. A minor HUC is 8 digits in length. 
  # pcodes_select is a character vector containing the USGS parameter codes of interest
  # site_tp_select is a character vector containing the USGS site types to retain
  
  # returns a data frame containing site information for NWIS sites that match given hucs and site types and contain the parameter codes of interest
  
  # Search for sites separately by HUC8 region since calls to readNWISdata allow no more than 10 minor HUCs to be specified:
  nwis_sites_ls <- lapply(hucs,function(x)
    readNWISdata(huc=x,parameterCd=pcodes_select,service="site",seriesCatalogOutput=TRUE))
  
  # Return a data frame of NWIS sites that contain the parameter(s) of interest and the preferred site types:
  nwis_sites <- nwis_sites_ls %>% 
    do.call(rbind,.) %>%
    filter(parm_cd %in% pcodes_select,
           site_tp_cd %in% site_tp_select)
  
  return(nwis_sites)
  
} 
