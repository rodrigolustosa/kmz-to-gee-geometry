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
library(stringr)

# directory and file names
dir_data <- "data"
file_example <- "[SaoPaulo][01042023][RodrigoLustosa].kmz"


# read files --------------------------------------------------------------

file_path <- file.path(dir_data, file_example)
# get layer names
file_layers <- st_layers(file_path)
# lengths
n_features <- file_layers$features
n_layers <- length(n_features)
# counters
n_lines <- 0 
# list of all geometries
geometries <- vector("list",n_layers)
names(geometries) <- file_layers$name
for(lay in 1:n_layers){
  # read file and only if there are geometries inside
  if(n_features[lay] != 0){
    any_added <- F
    # read file (layer 'lay')
    dados_kmz <- st_read(file_path,layer = file_layers$name[lay])
    # get each feature
    for(f in 1:n_features[lay]){
      geom_layer <- dados_kmz$geometry[[f]][1][[1]]
      # save geometry only if it is not empty
      if(dim(geom_layer)[1] != 0){
        if(!any_added){
          any_added <- T
          # list of layer geometries
          geometries[[lay]] <- vector("list",n_features[lay])
        }
        geometries[[lay]][[f]] <- geom_layer
        n_lines <- n_lines + dim(geom_layer)[1]
      }
    }
    # if there is geometries in this layer, add one line for the header
    if(any_added)
      n_lines <- n_lines + 1
  }
}


# make GEE code -----------------------------------------------------------

# start code in GEE language
final_lines <- vector("character",n_lines)
# colors for each geometry
geom_colors <- rainbow(sum(n_features != 0))
# start counters
l <- 0 # lines
k <- 0 # colors
for(lay in 1:n_layers){
  if(!is.null(geometries[[lay]])){
    l <- l + 1
    k <- k + 1
    # geometry header
    final_lines[l] <- str_c(ifelse(k == 1,"var", "   ")," geometry",k,
                            " = /* color: ",geom_colors[k],
                            " */ee.Geometry.MultiPolygon(")
    f_index <- which(sapply(geometries[[lay]], function(x) !is.null(x)))
    f_first <- min(f_index)
    f_last  <- max(f_index)
    for(f in f_index){
      if(!is.null(geometries[[lay]][[f]])){
        
        n_points <- nrow(geometries[[lay]][[f]])
        
        for(p in 1:n_points){
          l <- l + 1
          
          is_last_point_geometry <- f == f_last & p == n_points
          prefix <- str_pad(strrep("[",1 + 2*(p == 1) + (f == f_first & p == 1)),
                            12)
          posfix <- strrep("]",1 + 2*(p == n_points) + (is_last_point_geometry))
          posfix <- str_c(posfix, ifelse(is_last_point_geometry,")",""))
          posfix <- str_c(posfix, ifelse(l < n_lines           ,",",";"))
          
          final_lines[l] <- str_c(prefix,
                                  geometries[[lay]][[f]][p,1],", ", # longitude
                                  geometries[[lay]][[f]][p,2],      # latitude
                                  posfix) # close geom
        }
      }
    }
  }
}



# write lines -------------------------------------------------------------

con <- file("output.txt")
writeLines(final_lines, con)
close(con)

