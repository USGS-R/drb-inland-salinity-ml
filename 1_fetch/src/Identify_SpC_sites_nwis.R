get_drb_SpC_sites <- function(hucs,SpC_pcodes,site_tp_select){
  
  # hucs is a character vector containing the minor HUCs (hydrologic unit codes) over which to search. A minor HUC is 8 digits in length. 
  # SpC_codes is a character vector containing the USGS parameter codes of interest
  # site_tp_select is a character vector containing the USGS site types to retain
  
  # Search for sites separately by HUC8 region since calls to readNWISdata allow no more than 10 minor HUCs to be specified:
  drb_SpC_sites_ls <- lapply(hucs,function(x)
    readNWISdata(huc=x,parameterCd=SpC_pcodes,service="site",seriesCatalogOutput=TRUE))
  
  # Return a data frame of NWIS sites within the DRB that 1) have specific conductance data and 2) match preferred site types:
  drb_SpC_sites <- drb_SpC_sites_ls %>% 
    do.call(rbind,.) %>%
    filter(parm_cd %in% SpC_pcodes,
           site_tp_cd %in% site_tp_select)
  
  return(drb_SpC_sites)
  
} 
