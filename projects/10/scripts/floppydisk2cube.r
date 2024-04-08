# Remove all objects from the workspace (memory) of the R session

rm(list=ls())
gc()

library(sf)
library(dplyr)
library(terra)
library(here)

floppydisk2cube <- function(data_in, target_grid, specieskey){
    
    # define data layer projection
    grid_crs <- st_crs(4326)
    
    # convert data to a vector layer
    occ <- st_as_sf(data_in, coords = c("decimalLongitude", "decimalLatitude"), crs = grid_crs)

    # project vector layer to EEA grid, in this example EPGS:3035
    occ_proj <- st_transform(occ, crs(target_grid))
 
    # intersect spatial occurrences with target grid
    occ_eea <- st_intersection(occ_proj, target_grid)
 
    occ_dat <- as.data.frame(occ_eea)

    # create data frame with the following fields
    colnames_in <- c("eeacellcode", "specieskey", "species", "countrycode", "basisofrecord")
    occ_df <- setNames(data.frame(matrix(ncol = length(colnames_in), nrow = nrow(occ_dat))), colnames_in)

    # fill-in data frame
    occ_df$eeacellcode <- occ_dat$CellCode
    occ_df$specieskey <- specieskey
    occ_df$countrycode <- occ_dat$countryCode
    
    # aggregate occurrences
    occ_agg <- as.data.frame(occ_df %>% group_by(eeacellcode, specieskey, countrycode, basisofrecord) %>% 
    summarise(total_count=n(),
                .groups = 'drop'))
    occ_agg    
    colnames(occ_agg)[which(colnames(occ_agg) == 'total_count')] <- 'count'

    return(occ_agg)
}

# load your species occurrence file 
data_in <- read.csv(here( "./projects/10/data/processed/distribution.csv"))

# assign GBIF species key for your specie e.g., Cakile maritima
specieskey <- "3048831" # automate specieskey extraction from GBIF

# load grid (e.g. EEA grid 10 km)
target_grid <- st_read(here("./projects/10/input/eea_grid/Grid_ETRS89-LAEA_10K.shp"))

floppydatacube <- floppydisk2cube(data_in, target_grid, specieskey)
