# ---------------------------------------------------------------------------- #
# Name         : KMZ to GEE Geometry
# Description  : Convert a .kmz file to a Google Earth Engine (GEE) geometry
# Written by   : Rodrigo Lustosa
# Writing date :  1 Apr 2023 17:31 (GMT -03)
# ---------------------------------------------------------------------------- #

# initialization ----------------------------------------------------------

# packages
# library(tidyverse)
library(sf)

# directory and file names
dir_data <- "data"
file_example <- "[SaoPaulo][01042023][RodrigoLustosa].kmz"


# read files --------------------------------------------------------------

file_path <- file.path(dir_data, file_example)
# get layer names
file_layers <- st_layers(file_path)
# number of geometries by layer
n_features <- file_layers$features
n_layers <- length(n_features)
# list of all geometries
geometries <- vector("list",n_layers)
names(geometries) <- file_layers$name
for(l in 1:n_layers){
  # read file and only if there are geometries inside
  if(n_features[l] != 0){
    # list of layer geometries
    geometries[[l]] <- vector("list",n_features[l])
    # read file (layer 'l')
    dados_kmz <- st_read(file_path,layer = file_layers$name[l])
    # get each feature
    for(f in 1:n_features[l]){
      geom_layer <- dados_kmz$geometry[[f]][1][[1]]
      # save geometry only if it is not empty
      if(dim(geom_layer)[1] != 0)
        geometries[[l]][[f]] <- geom_layer
    }
  }
}



