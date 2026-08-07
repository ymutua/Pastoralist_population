# Prepare livestock production systems
# Extracts LGA (2), MRA (6), and MIA (10) systems from SERE classification
# Last updated: 16/8/2025

source("config.R")

aoi <- vect(paste0(outdir, "/africa_admin_dissolved.shp"))
refRas <- rast(paste0(outdir, "/humanPop_worldPop.tif"))

# Process systems in single pipeline
sereSystems <- rast(paste0(geodata, "/Socioeconomic/Livestock_production_systems/glps_gleam_61113_10km.tif")) %>%
  crop_mask_resample(aoi, refRas) %>%
  ifel(. %in% c(2,6,10), ., NA)

writeRaster(sereSystems, paste0(outdir, "/sereSystems.tif"), overwrite=TRUE)
