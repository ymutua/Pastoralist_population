# Prepare human population datasets from multiple sources
# Processes WorldPop, GPW v4, and LandScan population data for Africa
# Last updated: 16/8/2025

source("config.R")

aoi <- vect(paste0(outdir, "/africa_admin_dissolved.shp"))

# Population layer mapping
pop_config <- data.frame(
  layer = c("ppp_2020_1km_Aggregated", "gpw_v4_population_count_rev11_2020_30_sec", "landscan-global-2020"),
  source = c("worldPop", "gpwV4", "landScan")
)

# Process all layers with crop and mask only
for(i in 1:nrow(pop_config)) {
  humanPop <- rast(paste0(geodata, "/Socioeconomic/Human_population/", pop_config$layer[i], ".tif")) %>%
    crop(aoi) %>% mask(aoi)
  
  writeRaster(humanPop, paste0(outdir, "/humanPop_", pop_config$source[i], ".tif"), overwrite=TRUE)
}