# Shared configuration for pastoralist population analysis
# Last updated: 16/8/2025

# Libraries
library(geodata)
library(terra)
library(tidyterra)
library(sf)
library(dplyr)
library(gdalUtils)
library(patchwork) # Generate side-by-side choropleth maps
library(ggplot2)
library(viridis)
library(exactextractr)

# Terra options - optimized for performance
terraOptions(tempdir = "/home/s2255815/scratch/AUTemp")
terraOptions(memfrac = 0.7)
terraOptions(todisk = TRUE)

# Paths
root <- "/home/s2255815/rdrive/Pastoralist_population"
indir <- paste0(root, "/SpatialData/Inputs")
outdir <- indir
resultsdir <- paste0(root, "/Results")
geodata <- "/home/s2255815/rspovertygroup/JameelObs/FeedBaskets/Geodata"

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
