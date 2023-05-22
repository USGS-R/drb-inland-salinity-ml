subset_tidal_reaches <- function(reach_sf, segs_w_comids, tidal_elev_m = NULL){
  #' Subset tidally-influenced PRMS segments
  #' 
  #' @details
  #' A PRMS segment may be tidal if 1) at least one overlapping NHDPlusv2 COMID
  #' is labeled as "tidal == 1" in NHDPlus, or 2) the "minelevsmo" and "maxelevsmo"
  #' values from NHDPlus are both less than `tidal_elev_m`*100 for at least one 
  #' overlapping NHDPlusv2 COMID. 
  #' 
  #' @param reach_sf sf object of reach polylines with column "subsegid"
  #' @param segs_w_comids data frame containing the PRMS segment ids and the 
  #' NHDPlusv2 COMIDs of interest. Must contain columns "PRMS_segid" and "comid"
  #' @param tidal_elev_m optional; integer value indicating the cutoff elevation
  #' used to indicate the head-of-tide. If NULL, the default value of 6 meters 
  #' from NHDPlusv2 will be used to define the tidal extent. Defaults to NULL.
  #' 
  
  # Download NHDv2 COMIDs that lie along NHM network, then subset "tidal" COMIDs 
  nhdv2_flines <- nhdplusTools::get_nhdplus(comid = segs_w_comids$comid, realization = "flowline")
  
  if(is.null(tidal_elev_m)){
    comids_tidal <- nhdv2_flines$comid[nhdv2_flines$tidal == 1]
  } else {
    # maxelevsmo and minelevsmo have units of centimeters
    comids_tidal <- filter(nhdv2_flines, 
                           maxelevsmo < (tidal_elev_m*100), 
                           minelevsmo < (tidal_elev_m*100), 
                           qe_ma > 0) %>%
      pull(comid)
  }
  
  # Identify NHM segments that overlap a "tidal" NHDPlus reach
  segs_w_tidal_comids <- filter(segs_w_comids, comid %in% comids_tidal) %>%
    pull(PRMS_segid) %>%
    unique()
  
  # Subset and return NHM segments that overlap a "tidal" NHDPlus reach
  tidal_reaches <- filter(reach_sf, subsegid %in% segs_w_tidal_comids)
  return(tidal_reaches$subsegid)
}
