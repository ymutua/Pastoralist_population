# Prepare pastoral zones classification
# Delineates agro-pastoral (1) and pastoral (2) zones based on aridity, production systems, livestock density, and population
# Compares with Thornton et al. 2024 classification
# Last updated: 16/8/2025

source("config.R")

options(warn = -1, scipen = 999)

# Load all required rasters
rasters <- list(
  aoi = vect(paste0(outdir, "/africa_admin_dissolved.shp")),
  sereSystems = rast(paste0(outdir, "/sereSystems.tif")),
  aridZones = rast(paste0(outdir, "/aridZones.tif")),
  lvCount = rast(paste0(outdir, "/lvCountAll.tif")),
  refRas = rast(paste0(outdir, "/humanPop_worldPop.tif")),
  nonUrban = rast(paste0(outdir, "/nonBuiltup.tif"))
)

# Process pastoral zones with multiple masks in pipeline
pastoralZones <- ifel(rasters$aridZones %in% c(1,2,3) & rasters$sereSystems %in% c(6,10), 1, 
                      ifel(rasters$aridZones %in% c(1,2,3) & rasters$sereSystems == 2, 2, NA)) %>%
  mask(ifel(rasters$lvCount < 0, NA, rasters$lvCount)) %>%
  mask(ifel(rasters$nonUrban == 0, NA, rasters$nonUrban)) %>%
  mask(max(rast(paste0(outdir, "/humanPop_", c("worldPop", "gpwV4", "landScan"), ".tif"))) %>%
         classify(matrix(c(-Inf, 180, 1, 180, Inf, NA), ncol=3, byrow=TRUE)))

writeRaster(pastoralZones, paste0(outdir, "/pastoralZones.tif"), overwrite=TRUE)

# Process Thornton 2024 zones using classify method
thorton_rcl <- matrix(c(1, 2, 4, 1, NA, NA), ncol = 2, byrow = TRUE)
pastoralZones_Thorton2024 <- rast(paste0(outdir, "/sands-lga-mra-afr-cat5k.asc")) %>%
  classify(thorton_rcl, others = NA) %>%
  crop(rasters$aoi) %>%
  mask(rasters$aoi) %>%
  resample(rasters$refRas, method = "near") %>%
  mask(rasters$nonUrban)

writeRaster(pastoralZones_Thorton2024, paste0(outdir, "/pastoralZones_Thorton2024.tif"), overwrite=TRUE)

par(mfrow=c(1,2))  # Arrange plots side by side
plot(pastoralZones, main="Mutua")
plot(pastoralZones_Thorton2024, main="Thornton")
