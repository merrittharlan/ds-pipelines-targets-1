library(sbtools)
library(readr)

project_output_dir <- '1_fetch/out'

if (!dir.exists(project_output_dir)){
  dir.create(project_output_dir)
}

#' Get the data from ScienceBase
#' 
#' @param fileID ScienceBase file identification
#' @param fileName ScienceBase file name to download
#' @param outfile directory and name of saved output file
#' @return a csv of the downloaded ScienceBase file

fetch_data <- function(fileID = '5d925066e4b0c4f70d0d0599', fileName = 'me_RMSE.csv', outfile = '1_fetch/out/model_RMSEs.csv'){
  item_file_download(fileID, names = fileName, destinations = outfile, overwrite_file = TRUE)
  return(readr::read_csv(outfile, col_types = 'iccd'))
}

