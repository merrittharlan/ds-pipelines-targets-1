library(sbtools)
library(readr)

#' Get the data from ScienceBase
#' 
#' @param fileID ScienceBase file identification
#' @param fileName ScienceBase file name to download
#' @param out_filepath directory and name of saved output file
#' @return a csv of the downloaded ScienceBase file

download_data <- function(fileID = '5d925066e4b0c4f70d0d0599', fileName = 'me_RMSE.csv', out_filepath = '1_fetch/out/model_RMSEs.csv'){
  project_output_dir <- '1_fetch/out'
  if (!dir.exists(project_output_dir)){
    dir.create(project_output_dir)
  }
  item_file_download(fileID, names = fileName, destinations = out_filepath, overwrite_file = TRUE)
  return(out_filepath)
}

