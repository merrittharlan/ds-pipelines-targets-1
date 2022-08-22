library(sbtools)
library(readr)

# Get the data from ScienceBase
fetch_data <- function(fileID = '5d925066e4b0c4f70d0d0599', fileName = 'me_RMSE.csv', outfile = '1_fetch/out/model_RMSEs.csv'){
  item_file_download(fileID, names = fileName, destinations = outfile, overwrite_file = TRUE)
  return(readr::read_csv(outfile, col_types = 'iccd'))
}

