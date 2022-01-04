
library(sbtools)
library(tidyverse)
# https://www.sciencebase.gov/catalog/item/605c987fd34ec5fa65eb6a74

sb_id_FORESCE <- '605c987fd34ec5fa65eb6a74'
filename <- 'DRB_Historical_Reconstruction_1680-2010.zip'

sbtools::item_file_download(sb_id_FORESCE, names = filename,
                            destinations = file.path(tempdir(),
                                                     filename),
                            overwrite_file = T)

unzip(file.path(tempdir(), filename), exdir = file.path(tempdir(), 'FORESCE'))

fpath <- file.path(tempdir(),'FORESCE')

### keep only relevant years 1960 to 2000
