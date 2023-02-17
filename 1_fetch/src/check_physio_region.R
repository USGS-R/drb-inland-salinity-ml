get_physio_regions <- function(reaches, ecoregions){
  #' @description determines if a reach is within the physiographic ecoregion
  #' based on reach intersection. This would ideally be completed by catchment 
  #' areas within each ecoregion instead of reach intersection.
  #'
  #' @param reaches sf dataframe of PRMS reaches (lines)
  #' @param ecoregions sf dataframe of ecoregions (multipolygon)
  #' 
  #' @return Returns reaches dataframe with one binary indicator columns
  #' for each of the unique ecoregions.
  
  #Get a table of TRUE/FALSE for reach intersection with each ecoregion
  physio_intersect <- st_intersects(x = reaches, y = ecoregions,
                                    sparse = FALSE)
  
  #some reaches intersect several regions.
  # assign binary indicator values in a table with columns as the region and row as reach
  physio_mat <- data.frame(matrix(nrow = nrow(reaches), 
                                  ncol = length(unique(ecoregions$Physio))))
  colnames(physio_mat) <- sort(unique(ecoregions$Physio))
  for(i in 1:ncol(physio_mat)){
    #several polygons define each ecoregion, so using a function to check
    # all of those polygons for each reach
    physio_mat[,i] <- apply(physio_intersect, MARGIN = 1, 
                            FUN = check_physio_region, region_name_check = colnames(physio_mat)[i],
                            region_names = ecoregions$Physio) 
  }
  
  #join with original reach dataframe
  reaches <- cbind(reaches, physio_mat)
  
  return(reaches)
}

check_physio_region <- function(region_name_check, region_names, reach_region_indicator){
  #' 
  #' @description determines if a reach is within the physiographic region
  #'
  #' @param region_name_check name of the region to be checked
  #' @param region_names vector containing the region names in order of the columns
  #' within reach_region_indicator
  #' @param reach_region_indicator logical vector of length equal to region_names.
  #' True indicates the reach is within the physio region 
  #' 
  #' @return Returns a 1 (reach within region) or 0 (reach not within region)
  
  #get the column positions to check for TRUE in reach_region_indicator
  cols_check <- which(region_names == region_name_check)
  if(any(reach_region_indicator[cols_check] == TRUE)){
    region <- 1
  }else{
    region <- 0
  }
  
  return(region)
}