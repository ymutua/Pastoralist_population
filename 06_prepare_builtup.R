# Prepare non-built-up areas mask
# Creates mask excluding urban areas (class 50) with 3x3 buffer
# Last updated: 16/8/2025

source("config.R")

aoi <- vect(paste0(outdir, "/africa_admin_dissolved.shp"))
refRas <- rast(paste0(outdir, "/humanPop_worldPop.tif"))

# Load and process landcover
landCover <- rast(paste0(geodata, "/Landcover/CGLS_100m/PROBAV_LC100_global_v3.0.1_2019-nrt_Discrete-Classification-map_EPSG-4326.tif"))
landCover <- resample(landCover, refRas, method = "near")
landCover <- mask(landCover, aoi)

# Create non-built-up mask
nonBuiltup <- classify(landCover, cbind(50, 1), others = 0)
nonBuiltupBuffered <- focal(nonBuiltup, matrix(1, 3, 3), max, na.policy = "omit", fillvalue = 0)
nonBuiltupBuffered <- ifel(nonBuiltupBuffered == 1, NA, 1)

writeRaster(nonBuiltupBuffered, paste0(outdir, "/nonBuiltup.tif"), overwrite = TRUE)
