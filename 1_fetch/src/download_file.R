download_file <- function(url,fileout,mode,quiet = FALSE){
  #'
  #' @description Function to download a file from the internet and return a character
  #' string indicating the file path
  #' 
  #' @param url character string indicating the URL of the file to be downloaded
  #' @param fileout character string with the name and file path where the downloaded file
  #' should be saved
  #' @param mode character string indicating the mode used to write the file; see 
  #' ??utils::download.file for details
  #' @param quiet logical (defaults to FALSE); if TRUE, suppresses any status messages
  
  # download the file
  utils::download.file(url, destfile = fileout, mode = mode, quiet = quiet)

  # return the file name including file path and extension
  return(fileout)
  
}

