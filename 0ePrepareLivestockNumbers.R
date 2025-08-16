# Prepare livestock density data
# Processes GLW4 data for cattle, sheep, goats, and camels
# Converts from 10km density to 1km count data
# Last updated: 16/8/2025

source("config.R")

aoi <- vect(paste0(outdir, "/africa_admin_dissolved.shp"))
refRas <- rast(paste0(outdir, "/humanPop_worldPop.tif"))

# Livestock processing function
process_livestock <- function(species) {
  if(species == "CML") {
    lvDensity <- rast(paste0(geodata, "/Socioeconomic/GLW4/Cm_AF.tif")) %>% 
      `^`(10, .)
  } else {
    lvDensity <- rast(paste0(geodata, "/Socioeconomic/GLW4/2020/GLW4-2020.D-DA.", species, ".tif"))
  }
  
  lvCount <- crop_mask_resample(lvDensity, aoi) %>%
    `*`(cellSize(., unit = "km")) %>%
    disagg(fact = 10, method = "near") %>%
    resample(refRas, method = "near") %>%
    `/`(100)
  
  writeRaster(lvCount, paste0(outdir, "/", species, "_Count.tif"), overwrite = TRUE)
  return(lvCount)
}

# Process all species
lvSpecies <- c("CTL", "SHP", "GTS", "CML")
lvCountList <- lapply(lvSpecies, process_livestock)

# Sum all livestock counts ignoring NA
lvCountStack <- rast(lvCountList)
lvCountAll <- app(lvCountStack, sum, na.rm = TRUE)
writeRaster(lvCountAll, paste0(outdir, "/lvCountAll.tif"), overwrite = TRUE)
