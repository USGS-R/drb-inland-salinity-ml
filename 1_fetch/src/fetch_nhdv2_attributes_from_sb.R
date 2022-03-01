fetch_nhdv2_attributes_from_sb <- function(vars_item,save_dir,comids,delete_local_copies = TRUE){
  #'
  #' @description This function downloads zipped files from ScienceBase. By calling unzip_and_clip_sb_data(), this function
  #' also unzips the downloaded zipped files, reads in the unzipped data table, and filters the CONUS-scale data to 
  #' retain the NHDPlusV2 COMID's of interest.
  #' 
  #' @param vars_item rows from VarsOfInterest csv containing the sb item to download
  #' @param save_dir character string indicting the file path to save the unzipped data
  #' @param comids vector of COMIDs to retain from CONUS-scale datasets
  #' @param delete_local_copies logical; indicates whether to delete CONUS-scale zipped/unzipped data copies from save_dir
  #' delete_local_copies defaults to TRUE
  #' 

  message(sprintf("Downloading %s from ScienceBase",vars_item$Dataset_name[1]))
  
  # 1) Select items associated with ScienceBase ID to download
  item_names <- vars_item %>%
    select(sb_id,SB_item_retrieve) %>%
    tidyr::separate_rows(SB_item_retrieve,sep=";") %>%
    pull(SB_item_retrieve) %>%
    unique()
  
  # Reformat item_names for monthly average precipitation dataset (CAT)
  if(unique(vars_item$sb_id) == "5734acafe4b0dae0d5de622d"){
    years <- as.integer(c(item_names[!is.na(item_names)]))
    years <- c(years[1]:years[2])
    item_names <- paste0("PPT",years,"_CAT_CONUS.zip")
  }
  # Reformat item_names for monthly average precipitation dataset (ACC)
  if(unique(vars_item$sb_id) == "5730f062e4b0dae0d5db1fbe"){
    years <- as.integer(c(item_names[!is.na(item_names)]))
    years <- c(years[1]:years[2])
    item_names <- paste0("PPT",years,"_ACC_CONUS.zip")
  }
  # Reformat item_names for monthly average precipitation dataset (TOT)
  if(unique(vars_item$sb_id) == "573362bce4b0dae0d5dd6193"){
    years <- as.integer(c(item_names[!is.na(item_names)]))
    years <- c(years[1]:years[2])
    item_names <- paste0("PPT",years,"_TOT_CONUS.zip")
  }
  
  # 2) Download data from ScienceBase
  out_file <- download_sb_file(sb_id = unique(vars_item$sb_id),
                               file_name = c(item_names),
                               out_dir = save_dir)
  
  # 3) Select column names to be retained from original downloaded data
  col_names <- c("COMID",
                 vars_item$Catchment.Item.Name,
                 vars_item$Divergence.Item.Name,
                 vars_item$Total.Upstream.Item.Name)
  col_names <- col_names[!is.na(col_names)]
  
  # Reformat column names for certain datasets
  # National Inventory of Dams data:
  if(unique(vars_item$sb_id) == "58c301f2e4b0f37a93ed915a"){
    years <- str_extract(out_file,"\\d{2,}")
    col_names <- format_col_names_years(col_names,years,yr_pattern = "YYYY")
  }
  
  # HDENS data:
  if(unique(vars_item$sb_id) == "5910de31e4b0e541a03ac983"){
    years <- str_extract(out_file,"\\d{2,}")
    col_names <- format_col_names_years(col_names,years,yr_pattern = "XX")
  }
  
  # monthly average precipitation data (CAT,ACC,and TOT have different sb_id's):
  if(unique(vars_item$sb_id) %in% c("5734acafe4b0dae0d5de622d","5730f062e4b0dae0d5db1fbe","573362bce4b0dae0d5dd6193")){
    years <- str_extract(out_file,"\\d{2,}")
    col_names <- format_col_names_years(col_names,years,yr_pattern = "YYYY")
  }
  
  # 4) Unzip out_files, filter to COMIDs of interest, and return combined data frame
  data_out <- lapply(out_file,unzip_and_clip_sb_data,
                     col_names = col_names,
                     comids = comids,
                     save_dir = save_dir,
                     delete_local_copies = delete_local_copies) %>%
    Reduce(full_join,.) %>% 
    suppressMessages() %>%
    suppressWarnings()
  
  # 5) Save file
  data_out_path <- paste0(save_dir,"/",unique(vars_item$Dataset_name),".csv")
  write_csv(data_out,file = data_out_path)
  
  return(data_out_path)
  
}


unzip_and_clip_sb_data <- function(out_file,col_names,comids,save_dir,delete_local_copies = TRUE){
  #'
  #' @description This function unzips zipped files from ScienceBase, reads in the unzipped data table, 
  #' and filters the CONUS-scale data to retain the NHDPlusV2 COMID's of interest.
  #' 
  #' @param out_file character string indicating the file path of the zipped data
  #' @param col_names string vector containing the columns to return
  #' @param comids vector of COMIDs to retain from CONUS-scale datasets
  #' @param save_dir character string indicting the file path to save the unzipped data
  #' @param delete_local_copies logical; indicates whether to delete CONUS-scale zipped/unzipped data copies from save_dir
  #' delete_local_copies defaults to TRUE
  #' 
  
  # Unzip downloaded file
  unzip(zipfile = out_file, exdir = save_dir, overwrite = TRUE)
  
  # Parse name of unzipped file
  file_name <- basename(out_file)
  file_name_sans_ext <- tools::file_path_sans_ext(file_name)
  
  # Special handling 
  # in the future consider replacing with fuzzy string matching to create file_name_new
  if(file_name_sans_ext == "NHDV2_TMEAN7100_ANN_CONUS"){
    file_name_sans_ext <- "TMEAN7100_ANN_CONUS"
  }
  
  # this line finds the files within the desired directory that share file_name but are not zip files
  file_name_new <- grep(file_name_sans_ext,grep(list.files(save_dir), pattern = "\\.zip$", value = TRUE, invert = TRUE),value=TRUE)
  file_path <- paste0(save_dir,"/",file_name_new)
  
  # Read in data and filter to retain COMID's of interest
  dat <- read_delim(file_path,show_col_types = FALSE) %>%
    filter(COMID %in% comids) %>%
    select(any_of(col_names))
  
  # Remove files
  if(delete_local_copies == "TRUE"){
    file.remove(out_file)
    file.remove(file_path)
  }
  
  return(dat)
}



format_col_names_years <- function(col_names,years,yr_pattern){
  #'
  #' @description This function takes column names that are formatted with a generic suffix and returns
  #' column names as they are within the downloaded data from ScienceBase
  #' 
  #' @param col_names string vector containing the column names as given in the VarsOfInterest table
  #' @param years integer; years for which to gather corresponding columns
  #' @param yr_pattern character string pattern used to indicate year placeholder within VarsOfInterest table
  #' Examples include "YYYY" and "XX"
  #' 

  # Identify which col_names have yr_pattern as as a suffix (e.g. colYYYY)
  col_names_to_trim <- which(str_detect(col_names,pattern=yr_pattern)=="TRUE")
  
  # Remove year suffix from column names
  col_names[c(col_names_to_trim)] <- str_sub(col_names[c(col_names_to_trim)],start = 1,end=-(nchar(yr_pattern)+1))
  
  # Rename columns with specific years in place of yr_pattern suffix (e.g. col2015)
  col_names_all_years <- lapply(years,function(x) paste0(col_names[c(col_names_to_trim)],x)) %>% 
    do.call("c",.)
  col_names_out <- c("COMID",col_names_all_years)
  
  return(col_names_out)
  
  
}
