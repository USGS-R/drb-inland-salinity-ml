write_df_to_zarr <- function(df, index_cols, out_zarr) {
  #' @title Write an R data frame to a zarr data store
  #'
  #' @description This function uses {reticulate} to write an R data frame to a Zarr data
  #' store (the file format river-dl currently takes). write_df_to_zarr()
  #' originally written by J. Sadler in https://github.com/USGS-R/drb-do-ml
  #' and modified for use in drb-inland-salinity-ml.
  #'
  #' @param df a data frame of data. Must contain column "PRMS_segid" and "Date"
  #' @param index vector of strings indicating which column(s) should be the index.
  #' These columns must be in df. 
  #' @param out_zarr character string indicating the name of the saved file, 
  #' including file path and extension.
  #'
  #' @returns Returns the file path of the saved zarr file
  
  # convert to a python (pandas) DataFrame so we have access to 
  # the object methods (set_index and to_xarray)
  py_df <- reticulate::r_to_py(df)
  pd <- reticulate::import("pandas")
  py_df[["Date"]] = pd$to_datetime(py_df$Date)
  py_df[["PRMS_segid"]] = py_df$PRMS_segid$astype("str")
  
  # set the index so that when we convert to an xarray dataset it is indexed properly
  py_df  <- py_df$set_index(index_cols)
  
  # convert to an xarray dataset
  ds <- py_df$to_xarray()
  ds$to_zarr(out_zarr, mode = 'w')
  
  return(out_zarr)
}
