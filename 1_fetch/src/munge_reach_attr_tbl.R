munge_reach_attr_table <- function(prms_reach_attr_csvs){
  #'
  #' @description Function to read in the PRMS reach attribute table and add 
  #' _1 PRMS reaches for those reaches with _2 split segments.
  #' 
  #' @param prms_reach_attr_csvs p1_prms_reach_attr_csvs with a csv that 
  #' includes reach_attributes in the filename
  #' 
  #' @value PRMS reach attribute table with added _1 reaches for _2 segments.
  
  # PRMS reach attribute table
  attr_tbl <- read_csv(grep("reach_attributes", prms_reach_attr_csvs, value=TRUE),
                       show_col_types = FALSE)
  
  # Special handling to add _1 split segments to PRMS segments with _2 suffix
  # reach attribute table; values for from_segs and to_seg were
  # derived based on visual inspection and manually entered
  attr_tbl_w_splitsegs <- attr_tbl %>%
    mutate(to_seg = as.character(to_seg),
           subseg_seg = as.character(subseg_seg)) %>%
    tibble::add_row(subseg_id = "3_1", from_segs = "1", subseg_seg = "3_1", 
                    to_seg = "3_2", seg_id_nat = 1437, subseg_length = 14212.94) %>%
    tibble::add_row(subseg_id = "8_1", from_segs = NA, subseg_seg = "8_1", 
                    to_seg = "8_2", seg_id_nat = 1442, subseg_length = 17676.98) %>%
    tibble::add_row(subseg_id = "51_1", from_segs = "43", subseg_seg = "51_1", 
                    to_seg = "51_2", seg_id_nat = 1485, subseg_length = 105.14) %>%
    # now adjust the split segments with _1 suffix
    mutate(
      from_segs = case_when(
        subseg_id == "3_2" ~ "3_1;8_2",
        subseg_id == "4_1" ~ "3_2",
        subseg_id == "8_2" ~ "8_1;2",
        subseg_id == "52_1" ~ "51_2",
        subseg_id == "51_2" ~ "51_1;50",
        TRUE ~ from_segs),
      to_seg = case_when(
        subseg_seg == "1" ~ "3_1",
        subseg_seg == "2" ~ "8_2",
        subseg_id == "8_2" ~ "3_2",
        subseg_id == "43_1" ~ "51_1",
        subseg_id == "50_1" ~ "51_2",
        TRUE ~ to_seg),
      subseg_seg = case_when(
        subseg_seg == "3" ~ "3_2",
        subseg_seg == "8" ~ "8_2",
        subseg_seg == "51" ~ "51_2",
        TRUE ~ subseg_seg
      )
    ) %>%
    arrange(seg_id_nat, subseg_seg)
  
  return(attr_tbl_w_splitsegs)
}