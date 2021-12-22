
download_NHD_NLCD_data <- function(sb_id,
                                 out_path = '1_fetch/out',
                                 downloaded_data_folder_name = NA,
                                 overwrite_download = T){
  
  #' @description download Land Cover data to the repo's fetch/src folder.
  #' 
  #' @param sb_id: The ScienceBase Id for the NHD-NLCD land cover dataset of interest. Str or vector of str.
  #' @param out_path: Output folder for the downloaded data packages from ScienceBase. Default `1_fetch/out`. If path does not exist in directory, path will be created. 
  #' @param downloaded_data_folder_name: Sub-folder name(s) for the data in out_path. Str or vector of str. Default NA, where folder names will be labelled by sb_id. If not = NA, Must be same length as sb_id. 
  #' @param create_LandCover_folder: Create a catch all Land Cover Data Folder in 1_fetch/out where all downloaded Land Cover Data will reside. Default TRUE.
  #' @param overwrite_download If true, data will overwrite previous download in same out_path. Default TRUE.
  #' @value 
  #' @example  download_NHD_NLCD_data(sb_id = '57855ddee4b0e02680bf37bf', out_path = '1_fetch/out', downloaded_data_folder_name = 'LandCover_ripbuffer_id_11')
  #' @example  download_NHD_NLCD_data(sb_id = c('57855ddee4b0e02680bf37bf','570577fee4b0d4e2b7571d7b'), out_path = '1_fetch/out', downloaded_data_folder_name = c('LandCover_ripbuffer_id_11', 'pct_imperviousness_ripzone_id_11'))
  
  ## Create Land Cover Data sub folder in base directory
    out_path <- file.path(out_path, "LandCover_Data")
    dir.create(out_path, showWarnings = F)
  
  ## Sb_id split
  sb_id <- strsplit(sb_id, "/") %>% sapply(function(x) x[length(x)])
  
  ## Check lengths of sb_id and downloaded_data_folder_name
  ## first, avoiding error when downloaded_data_folder_name = NA (checking len)
  if(!(is.na(downloaded_data_folder_name) && length(downloaded_data_folder_name) == 1)){
    if(length(downloaded_data_folder_name) != length(sb_id)){
      stop('Downloaded_data_name must be same length as sb_id')
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
      unzip(file.path(data_path, x), exdir = out_path)
    )
    
    list_out_paths <- append(list_out_paths, out_path)
  }
  print(list_out_paths)
  return(list_out_paths)
}

###----------------------------------

read_subset_LC_data <- function(LC_data_folder,
                                Comids_in_AOI_df, comid_col = 'comid_down'){
  
  #' @description Read in and subset lc data after data is downloaded and unzipped
  #' @param LC_data_folder_path: LC data folder path or vector of LC data folder paths - last subfolder often 'unzipped'
  #' @param Comids_in_AOI_df: dataframe of all comid ids
  #' @param comid_col: str. key col in Xwalk table 
  #' @example read_subset_LC_data(LC_data_folder = "1_fetch/out/LandCover_Data/ImperviousnessPct_2011/unzipped", PRMSxWalk = PRMSxWalk)
  #' @example read_subset_LC_data(LC_data_folder = c("1_fetch/out/LandCover_Data/ImperviousnessPct_2011/unzipped",
  #'  "1_fetch/out/LandCover_Data/Imperviousness100m_RipZone/unzipped") , PRMSxWalk = PRMSxWalk)

  # Function Vars 
  ## creating list for dfs before for loop
  all_data_subsetted <- list()
  
  # Loop through sub-folders, combine datasets, and subset through Join
  for(LC_data in LC_data_folder){
    
  ## Read in
    LC_data_path <- unlist(LC_data)
    files <- list.files(path = LC_data_path, pattern = '*.txt|*.TXT', full.names = T)
    
    data_list <- lapply(files, read_csv) ## --> Note this will print read_csv() output specs
  
  ## Combine
    cbind_df <-data_list %>%
      reduce(inner_join, by = 'COMID') ## possibly add as full_join

  ## Subset by comid_id's from Xwalk
    data_subsetted <-cbind_df %>%
      right_join(Comids_in_AOI_df,
               by = c('COMID' = comid_col),
               keep = T)

  ## Assign to list - note name of item in list is LC_data (e.g. all_data_subsetted$NLCD_LandCover_2011) 
    if(endsWith(LC_data, 'unzipped')){
      name <- str_split(LC_data, pattern = '/', simplify = T)
      name <- name[length(name)-1]
    }
    else{
      name <- str_split(LC_data, pattern = '/', simplify = T)
      name <-name[length(name)]
    }
    
    all_data_subsetted[[name]] <- data_subsetted
  }

  return(all_data_subsetted)
  
}
