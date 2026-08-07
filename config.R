# Shared configuration for pastoralist population analysis
# Last updated: 16/8/2025

# Libraries
library(geodata)
library(terra)
library(tidyterra)
library(sf)
library(dplyr)
library(patchwork) # Generate side-by-side choropleth maps
library(ggplot2)
library(viridis)
library(exactextractr)

# Paths
root       <- "/Users/J.Y.Mutua/Library/CloudStorage/OneDrive-CGIAR/Work/Pastoralist_population"
indir      <- paste0(root, "/SpatialData/Inputs")
outdir     <- indir
resultsdir <- paste0(root, "/Results")

# Regional ISO lists — defined in styles.R; sourced here for scripts that use config.R only
source(file.path(root, "styles.R"))

# Utility functions for efficient raster processing
crop_mask_resample <- function(raster, aoi, ref_raster = NULL, method = "near") {
  result <- crop(raster, aoi) %>% mask(., aoi)
  if (!is.null(ref_raster)) {
    result <- resample(result, ref_raster, method = method)
  }
  return(result)
}

process_raster_stack <- function(file_paths, aoi, ref_raster = NULL) {
  rasters <- rast(file_paths)
  return(crop_mask_resample(rasters, aoi, ref_raster))
}
