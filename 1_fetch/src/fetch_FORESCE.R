download_backcasted_LC <- function(sb_id, filename, download_path, overwrite_file = T){
  
#' @description Download and unzip specified file from sciencebase
#' @param sb_id str. Sciencebase id. accepts a single str.
#' @param filename str. Name of specific file to download from given sciencebase id. Accepts a single str.
#' @param download_path str.Directory location of download
#' @param overwrite_file binary T/F. Whether to re-download if specific downloaded file exists in directory. Default True
#' @example download_backcasted_LC(sb_id = '605c987fd34ec5fa65eb6a74', filename = 'DRB_Historical_Reconstruction_NLCD.zip', download_path = '1_fetch/out', overwrite_file = T)
  
  path_to_downloaded_file <- file.path(download_path, filename)
  # Download specified file from sciencebase to 1_fetch/out folder
  sbtools::item_file_download(sb_id, names = filename,
                              destinations = path_to_downloaded_file,
                              overwrite_file = overwrite_file)
  
  # Remove '.zip' extension for unzip folder
  unzip_folder_path <- sub('.zip','', path_to_downloaded_file)
  # unzip downloaded zip to subfolder. subfolder is named the filename
  unzip(zipfile = path_to_downloaded_file, exdir = unzip_folder_path)

  # Find and delete years that are not needed - negate enables inverse matching
  Backcasting_files_del <- list.files(unzip_folder_path, full.names = T) %>% str_subset('2000|1990|1980|1970|1960', negate = T)
  lapply(Backcasting_files_del, file.remove)
  rm(path_to_downloaded_file, Backcasting_files_del)
  
  return(list.files(unzip_folder_path, full.names = T))
  
}
