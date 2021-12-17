
download_NHD_NLCD_data <- function(sb_id,
                                 out_path = '1_fetch/out',
                                 downloaded_data_folder_name = NA,
                                 create_LandCover_folder = T,
                                 overwrite_download = T){
  
  #' @description download Land Cover data to the repo's fetch/src folder.
  #' 
  #' @param sb_id: The ScienceBase Id for the nhd NLCD land cover Dataset of interest. Str or vector of str
  #' @param out_path: Default is `1_fetch/out`. Output folder for the downloaded data packages from ScienceBase. If does not exist in directory, will be created. 
  #' @param downloaded_data_folder_name: Default is NA. Folder name(s) for the data in out_path. Str or vector of str. Must be same length as sb_id. When NA, folder names will be labelled by sb_id. 
  #' @param create_LandCover_folder: Default is True. Create a catch all Land Cover Data Folder in 1_fetch/src/ where all downloaded Land Cover Data will reside
  #' @param overwrite_download If true, data will overwrite previous download in same out_path. 
  #' @value 
  #' @example  download_NHD_NLCD_data(sb_id = '57855ddee4b0e02680bf37bf', out_path = '1_fetch/out', downloaded_data_folder_name = 'LandCover_ripbuffer_id_11')
  #' @example  download_NHD_NLCD_data(sb_id = c('57855ddee4b0e02680bf37bf','570577fee4b0d4e2b7571d7b'), out_path = '1_fetch/out', downloaded_data_folder_name = c('LandCover_ripbuffer_id_11', 'pct_imperviousness_ripzone_id_11'))
  
  ## Creating '1_fetch/out' folder in working dir. 
  dir.create(out_path, showWarnings = F)
  
  ## Create Land Cover Data sub folder in base directory if true
  if(create_LandCover_folder == T){
    out_path <- file.path(out_path, "LandCover_Data")
    dir.create(out_path, showWarnings = F)
  }
  
  ## Check lengths of sb_id and downloaded_data_folder_name
  ## first, avoiding error when downloaded_data_folder_name = NA (checking len)
  if(!(is.na(downloaded_data_folder_name) && length(downloaded_data_folder_name) == 1)){
    if(length(downloaded_data_folder_name) != length(sb_id)){
      stop('Downloaded_data_name must be same list length as sb_id')
    }
  }
  
  ## Create data folder for downloaded data if non given
  if(any(is.na(downloaded_data_folder_name))){
    ## if its set at default and downloaded_data_folder_name = NA, folder_name(s) are the sb_ids
    if(length(downloaded_data_folder_name == 1)){
      downloaded_data_folder_name <- sb_id
    } else{
    ## if na present in list of folder names, loop through the names and assign folder_name or sb_id  
      downloaded_data_folder_name <- purrr::map2(.x = downloaded_data_folder_name,
                                                 .y = sb_id,
                                                 .f = ifelse(is.na(.x), .y, .x))
    }
  }

  ## Create labeled list for loop
  sb_id_labeled <- structure(sb_id, names = downloaded_data_folder_name)

  ## Loop through sb_ids and download from ScienceBase
  downloaded_data_folder <- c()

  for(i in 1:length(sb_id_labeled)){
    
    # Create folder for specific sb_id
    file_path <- file.path(out_path, names(sb_id_labeled[i]))
    dir.create(file_path, showWarnings = F)
    
    # Download to specified folder
    sbtools::item_file_download(sb_id = sb_id_labeled[[i]], dest_dir = file_path, overwrite_file = overwrite_download)
    
    message(paste(names(sb_id_labeled[i]), 'data downloaded in:', file_path))
    # append to list 
    downloaded_data_folder <- append(downloaded_data_folder, file_path)
    }
  return(downloaded_data_folder)
}

###-----------------------------------------------------------------------------

unzip_NHD_NLCD_data <- function(downloaded_data_folder_path, 
                                create_unzip_subfolder = T){
  
  #' @description Unzip downloaded Land Cover data 
  #' @param downloaded_data_folder_path: path to folder(s) where zipped data is saved. Input should be output of download_NHD_NLCD_data()
  #' @param create_unzip_subfolder: Default True. Create unzipped sub-folder in downloaded data folder.
  #' @example unzip_NHD_NLCD_data(downloaded_data_folder_path = pct_imperviousness_id_11', create_unzip_subfolder = T)
  #' @example unzip_NHD_NLCD_data(downloaded_data_folder_path = 'pct_imperviousness_id_11', create_unzip_subfolder = T) 
  
  ## Unzip files in subfolders 
  for(i in 1:length(downloaded_data_folder_path)){
    
    data_path <- downloaded_data_folder_path[i]
    
    ## Output path- Create Unzip folder if True
    if(create_unzip_subfolder == T){
      out_path <- file.path(data_path,"unzipped")
      dir.create(out_path, showWarnings = F)
    } else{out_path <- data_path}
  
    ## Unzip files to output path
    list_zipfiles <- list.files(data_path, pattern = ".zip$")
    lapply(list_zipfiles, function(x)
      unzip(file.path(data_path, x), exdir = out_path)
    )
  }
  return(out_path)
}
