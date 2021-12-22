get_nhdv2_flowlines <- function(huc8){
  #' 
  #' @description Function to download NHDPlusV2 flowlines within individual HUC8 watersheds
  #'
  #' @param huc8 string or character vector containing the HUC8 watersheds of interest
  #'
  #' @value An sf object containing NHDPlusV2 flowlines
  
  huc8_basin <- suppressMessages(nhdplusTools::get_huc8(id=huc8))
  huc8_bbox <- sf::st_as_sfc(st_bbox(huc8_basin))
  
  # Download flowlines within the bounding box of the HUC8 basin
  # Use the bounding box rather than the basin boundary to make sure we're capturing all of the reaches
  huc8_flines <- suppressMessages(nhdplusTools::get_nhdplus(AOI = huc8_bbox,realization="flowline"))
  
  return(huc8_flines)
  
}
