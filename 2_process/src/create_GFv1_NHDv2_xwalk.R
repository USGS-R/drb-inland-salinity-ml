create_GFv1_NHDv2_xwalk <- function(prms_lines,nhd_lines,prms_hrus,min_area_overlap,drb_segs_spatial){
  #' 
  #' @description This function outputs a table to facilitate cross-walking between PRMS river segments (from the
  #' Geospatial Fabric, GFv1) and NHDPlusV2 flowlines and catchments for the Delaware River Basin (DRB). 
  #' 
  #' @param prms_lines sf object containing the PRMS river segments for the DRB
  #' @param nhd_lines sf object containing NHDPlusV2 flowlines for area of interest
  #' nhd_lines must contain variables COMID, PATHLENGTH, LENGTHKM, HYDROSEQ, STREAMORDE, STREAMCALC, FROMNODE,and TONODE
  #' @param prms_hrus sf (multi)polygon containing the HRU's from the GFv1
  #' @param min_area_overlap float; value indicating the minimum proportion of NHDPlusV2 catchment area that 
  #' overlaps the PRMS polygon in order to be retained
  #' @param drb_segs_spatial character vector containing the identity of PRMS segments that require special handling
  #' For these segments, the contributing NHD catchments will be determined using a spatial join with the 
  #' PRMS HRU polygons
  #
  
  # find NHDPlusV2 COMID's that intersect PRMS segments
  reach_to_seg_xwalk <- prms_lines %>%
    split(.,.$subsegid) %>%
    purrr::map(.,pair_nhd_reaches,nhd_lines = nhd_lines) %>%
    purrr::map(.,summarize_paired_comids) %>%
    bind_rows()
  
  # find NHDPlusV2 COMID's that drain directly to PRMS segments
  cats_to_seg_xwalk <- prms_lines %>%
    split(.,.$subsegid) %>%
    purrr::map(.,pair_nhd_catchments,
               prms_hrus = prms_hrus,
               min_area_overlap = min_area_overlap,
               nhd_lines = nhd_lines,
               xwalk_table = reach_to_seg_xwalk,
               drb_segs_spatial = drb_segs_spatial) %>%
    bind_rows() %>%
    left_join(reach_to_seg_xwalk,.,by="PRMS_segid")
  
  return(cats_to_seg_xwalk)
}

