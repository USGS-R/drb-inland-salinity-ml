get_gf <- function(sb_id, sb_name, out_dir) {
  
  #' 
  #' @description Function to download the national geospatial fabric (GF). This function is borrowed/has been modified from 
  #' delaware-model-prep: https://github.com/USGS-R/delaware-model-prep/blob/main/1_network/src/get_national_gf.R
  #'
  #' @param sb_id character string indicating the ScienceBase identifier
  #' @param sb_name string vector of file names attached to the ScienceBase item of interest
  #' @param out_dir directory to save the downloaded GF files 
  #'
  #' @examples 
  #' get_national_gf(out_dir = "1_fetch/out/", sb_id = '5362b683e4b0c409c6289bf6', sb_name = 'GeospatialFabricFeatures_01.gdb')
  #'

  # check against out_dir, if already present, don't download if not needed
  if(grepl(".zip",sb_name)){
    out_file <- tools::file_path_sans_ext(sb_name)
    if(!grepl(".gdb",out_file)){
    out_file <- paste0(out_file,".gdb")
    }
  } else {
    out_file <- sb_name
  }
  out_path <- paste0(out_dir,out_file)

  # if the data don't yet exist, download and unzip
  if(!out_file %in% list.files(out_dir)) {
    temp_loc <- tempfile()
    sbtools::item_file_download(sb_id = sb_id, names = sb_name, destinations=temp_loc,overwrite_file = TRUE)
    if(dir.exists(out_path)) unlink(out_file, recursive=TRUE)
    unzip(temp_loc, exdir = dirname(out_path))
  } else {
    message('GF is already downloaded; doing nothing')
  }
  
  return(out_path)
  
}
