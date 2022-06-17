###-----------------------------------------------------------------------------
download_NHD_data <- function(sb_id,
                                 out_path = '1_fetch/out',
                                 downloaded_data_folder_name = NULL,
                                 output_data_parent_folder = NULL,
                                 overwrite_download = T){
  
  #' @description download Land Cover data to the repo's fetch/src folder.
  #' 
  #' @param sb_id: The ScienceBase Id for the NHD-NLCD land cover dataset of interest. Str or vector of str.
  #' @param out_path: Output folder for the downloaded data packages from ScienceBase. Default `1_fetch/out`. If path does not exist in directory, path will be created. 
  #' @param downloaded_data_folder_name: Sub-folder name(s) for the data in out_path. Str or vector of str. Default NA, where folder names will be labelled by sb_id. If not = NA, Must be same length as sb_id. 
  #' @param overwrite_download If true, data will overwrite previous download in same out_path. Default TRUE.
  #' @value 
  #' @example  download_NHD_NLCD_data(sb_id = '57855ddee4b0e02680bf37bf', out_path = '1_fetch/out', downloaded_data_folder_name = 'LandCover_ripbuffer_id_11')
  #' @example  download_NHD_NLCD_data(sb_id = c('57855ddee4b0e02680bf37bf','570577fee4b0d4e2b7571d7b'), out_path = '1_fetch/out', downloaded_data_folder_name = c('LandCover_ripbuffer_id_11', 'pct_imperviousness_ripzone_id_11'))
  
  ## Create Land Cover Data sub folder in base directory
  if(!is.null(output_data_parent_folder)){
    out_path <- file.path(out_path, output_data_parent_folder)
    dir.create(out_path, showWarnings = F)
  }
  
  ## Check lengths of sb_id and downloaded_data_folder_name
  # first, avoiding error when downloaded_data_folder_name = NA (checking len)
  if(!(is.null(downloaded_data_folder_name) && length(downloaded_data_folder_name) == 1)){
    if(length(downloaded_data_folder_name) != length(sb_id)){
      stop('Downloaded_data_name must be same length as sb_id')
    }
  }
  
  ## Create data folder for downloaded data if non given
  if(any(is.na(downloaded_data_folder_name))){
    # if its set at default and downloaded_data_folder_name = NA, folder_name(s) are the sb_ids
    if(length(downloaded_data_folder_name == 1)){
      downloaded_data_folder_name <- sb_id
    } else{
    # if na present in list of folder names, loop through the names and assign folder_name or sb_id  
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

unzip_NHD_data <- function(downloaded_data_folder_path, 
                                create_unzip_subfolder = T){
  
  #' @description Unzip downloaded Land Cover data 
  #' @param downloaded_data_folder_path: path to folder(s) where zipped data is saved. Input should be output of download_NHD_NLCD_data()
  #' @param create_unzip_subfolder: Default TRUE. Create unzipped sub-folder in downloaded data folder.
  #' @example unzip_NHD_NLCD_data(downloaded_data_folder_path = pct_imperviousness_id_11', create_unzip_subfolder = T)
  #' @example unzip_NHD_NLCD_data(downloaded_data_folder_path = 'pct_imperviousness_id_11', create_unzip_subfolder = T) 
  
  list_out_paths <- c()
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
      {path <- file.path(data_path, x)
      unzip(path, exdir = out_path)
      ## Remove zipped file
      unlink(path, recursive = FALSE)
      }
    )
    
    list_out_paths <- append(list_out_paths, out_path)
  }
  
  return(list_out_paths)
}

###-----------------------------------------------------------------------------

read_subset_LC_data <- function(LC_data_folder_path,
                                Comids_in_AOI_df,
                                Comid_col,
                                NLCD_type = NULL){
  
  #' @description Read in and subset lc data after data is downloaded and unzipped
  #' @param LC_data_folder_path LC data folder path or vector of LC data folder paths - last subfolder often 'unzipped'
  #' @param Comids_in_AOI_df dataframe of all comid ids
  #' @param Comid_col str. key comid col in Xwalk table. e.g. "comid" | "COMID"
  #' @param NLCD_type str. Default NULL. Options are either CAT, ACC, or TOT. Use NULL if all three are selected
  #' @example read_subset_LC_data(LC_data_folder_path = "1_fetch/out/LandCover_Data/ImperviousnessPct_2011/unzipped",
  #'  Comids_in_AOI_df = PRMSxWalk,  Comid_col = 'comid_down')
  #' @example read_subset_LC_data(LC_data_folder_path = c("1_fetch/out/LandCover_Data/ImperviousnessPct_2011/unzipped",
  #'  "1_fetch/out/LandCover_Data/Imperviousness100m_RipZone/unzipped") , Comids_in_AOI_df = PRMSxWalk,  Comid_col = 'comid_down')

  # Function Vars 
  ## creating list for dfs before for loop
  all_data_subsetted <- list()
  
  # Loop through sub-folders, combine datasets, and subset through Join
  for(LC_data in LC_data_folder_path){
    # Check that downloaded data exists in folder 
    if(length(list.files(LC_data)) == 0){
      stop(paste0('No NLCD LC data for file ', LC_data,'. Please move the NLCD .txt file to this location.'))
    }
    
    LC_data_path <- unlist(LC_data)
    
    if(!is.null(NLCD_type)){
      files <- list.files(path = LC_data_path, pattern = glue('*{NLCD_type}_CONUS.txt|*{NLCD_type}_CONUS.TXT'), full.names = TRUE)
    }
    else{
      files <- list.files(path = LC_data_path, pattern = '*_CONUS.txt|*_CONUS.TXT', full.names = TRUE)
    }
    
    ## Read in and subset by comid_id's from Xwalk  
    data_list <- lapply(files, function(x){ read_csv(x, show_col_types = FALSE) %>% 
        right_join(Comids_in_AOI_df,
                   by = c('COMID' = Comid_col),
                   keep = F)
      })
  
  ## Combine
    cbind_subsetted_df <-data_list %>%
      reduce(inner_join, by = 'COMID') ## possibly add as full_join

    # using str_replace_all to standardize file paths from OS or Windows 
    LC_data <- str_replace_all(LC_data, '\\\\','/')
    
  ## Assign to list - note name of item in list is LC_data (e.g. all_data_subsetted$NLCD_LandCover_2011) 
    if(endsWith(LC_data, 'unzipped') | endsWith(LC_data, '')){
      name <- str_split(LC_data, pattern = '/', simplify = TRUE)
      name <- name[length(name) - 1]
    }
    else{
      name <- str_split(LC_data, pattern = '/', simplify = TRUE)
      name <-name[length(name)]
    }
    
    all_data_subsetted[[name]] <- cbind_subsetted_df
    }
   ## if only one NLCD table loaded, set as a df
    if(length(all_data_subsetted) == 1){
      all_data_subsetted <-  all_data_subsetted[[1]]
    }
      
  return(all_data_subsetted)
  
}
