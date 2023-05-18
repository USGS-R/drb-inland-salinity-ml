subset_tidal_reaches <- function(reach_sf, segs_w_comids){
  #' Subset tidally-influenced PRMS segments
  #' 
  #' @details
  #' A PRMS segment may be tidal if at least one overlapping NHDPlusv2 COMID
  #' is labeled as "tidal == 1" in NHDPlus.
  #' 
  #' @param reach_sf sf object of reach polylines with column "subsegid"
  #' @param segs_w_comids data frame containing the PRMS segment ids and the 
  #' NHDPlusv2 COMIDs of interest. Must contain columns "PRMS_segid" and "comid"
  #' 

  # Download NHDv2 COMIDs that lie along NHM network, then subset "tidal" COMIDs 
  nhdv2_flines <- nhdplusTools::get_nhdplus(comid = segs_w_comids$comid, realization = "flowline")
  comids_tidal <- nhdv2_flines$comid[nhdv2_flines$tidal == 1]
  
  # Identify NHM segments that overlap a "tidal" NHDPlus reach
  segs_w_tidal_comids <- filter(segs_w_comids, comid %in% comids_tidal) %>%
    pull(PRMS_segid) %>%
    unique()
  
  # Subset and return NHM segments that overlap a "tidal" NHDPlus reach
  tidal_reaches <- filter(reach_sf, subsegid %in% segs_w_tidal_comids)
  return(tidal_reaches$subsegid)
}
