
download_NHD_NLCD_data <- function(sb_id,
                                 out_path = '1_fetch/src',
                                 downloaded_data_folder_name = 'NA',
                                 create_LandCover_folder = F){
  
  #' @description download Land Cover data to the repo's fetch/src folder.
  #' 
  #' @param sb_id: sb_id for the ScienceBase Id for land cover dataset of interest
  #' @param out_path: output path for the downloaded data package from ScienceBase  
  #' @param downloaded_data_folder_name: folder name for the new folder for the data in outpath char or list of char. Must be same lenght as sb_id
  #' @param create_LandCover_folder: Create a catch all Land Cover Data Folder in 1_fetch/src/ where all downloaded Land Cover Data will reside
  #' @value 
  #' @example  download_NHD_NLCD_data(sb_id = '57855ddee4b0e02680bf37bf', out_path = '1_fetch/src', downloaded_data_folder_name = 'LandCover_ripbuffer_id_11')
  #' @example  download_NHD_NLCD_data(sb_id = c('57855ddee4b0e02680bf37bf','570577fee4b0d4e2b7571d7b'), out_path = '1_fetch/src', downloaded_data_folder_name = c('LandCover_ripbuffer_id_11', 'pct_imperviousness_ripzone_id_11'))
  
  ## Create Land Cover Data sub folder in base directory if true
  if(create_LandCover_folder == T){
    out_path <- file.path(out_path, "LandCover_Data")
    dir.create(out_path, showWarnings = F)
  }
  
  ## Create data folder for downloaded data if non given
  if(downloaded_data_folder_name == 'NA'){
    downloaded_data_folder_name <- sb_id
  } 
  ## Check lengths of sb_id and downloaded_data_folder_name
  else if(length(downloaded_data_folder_name) != length(sb_id)){
    stop('Downloaded_data_name must be same list length as sb_id')
  }
  
  ## Create labeled list for for loop
  sb_id_labeled <- structure(sb_id, names = downloaded_data_folder_name)
  
  ## Loop through sb_ids and download from ScienceBase
  for(i in 1:length(sb_id_labeled)){
    ## create folder for specific sb_id
    
    file_path <- file.path(out_path, names(sb_id_labeled[i]))
    print(file_path)
    dir.create(file_path, showWarnings = F)
    
    ## Download data but catching error in case of overwrite error in item_file_download
    tryCatch(
      expr = {
        sbtools::item_file_download(sb_id = sb_id_labeled[[i]], dest_dir = file_path)
        },
      error = function(e){
        message('Caught the following error:')
        print(e)
        },
      finally = {
        message(paste(names(sb_id_labeled[i]), 'data downloaded in:', file_path))
        }
      )
    }
  
  return(paste('All downloaded data can be found in:', out_path))
}


###-----------------------------------------------------------------------------

unzip_NHD_NLCD_data <- function(downloaded_data_folder_name, 
                                downloaded_data_path = '1_fetch/src/LandCover',
                                create_unzip_subfolder = T){
  
  #' @description Unzip downloaded Land Cover data 
  #' 
  #' @param downloaded_data_folder_name: sb_id for the ScienceBase Id for land cover dataset of interest
  #' @param downloaded_data_path: output path for the downloaded data package from ScienceBase  
  #' @param create_unzip_subfolder: folder name for the new folder for the data in outpath char or list of char
  #' @value create
  #' @example unzip_NHD_NLCD_data(downloaded_data_folder_name = ''pct_imperviousness_id_11'', downloaded_data_path = '1_fetch/src', create_unzip_subfolder = T)
  #' @example unzip_NHD_NLCD_data(downloaded_data_folder_name = 'pct_imperviousness_id_11', downloaded_data_path = '1_fetch/src/LandCover', create_unzip_subfolder = T) 
  
  ## Unzip files in subfolders 
  for(i in 1:length(downloaded_data_folder_name)){
    
    # Paths to files to unzip based on sb_folder item
    data_path <- file.path(downloaded_data_path, downloaded_data_folder_name[i])
    
    ## create Unzip folder if True
    if(create_unzip_subfolder == T){
      out_path <- file.path(data_path,"unzipped")
      dir.create(out_path, showWarnings = F)
    } else{out_path <- data_path}
    
    ## Unzip files
    for(j in list.files(data_path , pattern = ".zip$")){
        zip_file <- file.path(data_path, j)
        unzip(zip_file, exdir = out_path)
    }  
  }
}



