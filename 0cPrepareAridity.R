# Prepare aridity zones classification
# Classifies areas into hyperarid (AI<0.05), arid (0.05-0.20), and semi-arid (0.20-0.50)
# Last updated: 16/8/2025

source("config.R")

aoi <- vect(paste0(outdir, "/africa_admin_dissolved.shp"))
refRas <- rast(paste0(outdir, "/humanPop_worldPop.tif"))

# Process aridity index in single pipeline
aridityIndex <- rast(paste0(geodata, "/Climatology/Aridity/Global-AI_ET0_v3_annual/ai_v3_yr.tif")) %>%
  crop_mask_resample(aoi) %>%
  `/`(10000)  # Normalize values

# Efficient reclassification matrix
rcl <- matrix(c(-Inf, 0.05, 1, 0.05, 0.20, 2, 0.20, 0.50, 3, 0.50, Inf, NA), ncol = 3, byrow = TRUE)

aridZones <- classify(aridityIndex, rcl, others = NA) %>%
  crop_mask_resample(aoi, refRas)

# Write results
writeRaster(aridZones, paste0(outdir, "/aridZones.tif"), overwrite=TRUE)
