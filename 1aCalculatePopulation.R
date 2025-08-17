# Calculate pastoralist population statistics
# Estimates population in pastoral zones using WorldPop and GPW v4 datasets
# Generates Africa-wide and country-level statistics for choropleth mapping
# Last updated: 16/8/2025

source("config.R")

options(warn = -1, scipen = 999)
dir.create(resultsdir, FALSE, TRUE)
dir.create(paste0(resultsdir, "/Raster_data"), FALSE, TRUE)
dir.create(paste0(resultsdir, "/Vector_data"), FALSE, TRUE)
dir.create(paste0(resultsdir, "/Tabular_data"), FALSE, TRUE)
dir.create(paste0(resultsdir, "/Plots"), FALSE, TRUE)

# Load data
africaCountries_0 <- st_read(paste0(outdir, "/africa_admin_0.shp"))
africaCountries_1 <- st_read(paste0(outdir, "/africa_admin_1.shp"))
pastoralZones <- rast(paste0(outdir, "/pastoralZones.tif"))
popLayers <- c("worldPop", "gpwV4")

# Process population layers
process_population_layer <- function(popLayer) {
  humanPop <- rast(paste0(outdir, "/humanPop_", popLayer, ".tif"))
  pastoralPopRas <- crop_mask_resample(humanPop, pastoralZones) %>% round()
  
  writeRaster(pastoralPopRas, paste0(resultsdir, "/Raster_data/pastoralPop_", popLayer, ".tif"), overwrite=TRUE)
  
  # Extract agro-pastoral and pastoral separately using ifel
  agropastoralRas <- ifel(pastoralZones == 1, pastoralPopRas, NA)
  pastoralOnlyRas <- ifel(pastoralZones == 2, pastoralPopRas, NA)
  
  agropastoral_0 <- terra::extract(agropastoralRas, africaCountries_0, fun="sum", na.rm=TRUE, bind=TRUE)
  names(agropastoral_0)[ncol(agropastoral_0)] <- paste0(popLayer, "_agropastoral")
  agropastoral_0[[paste0(popLayer, "_agropastoral")]] <- round(agropastoral_0[[paste0(popLayer, "_agropastoral")]], 0)
  
  pastoral_0 <- terra::extract(pastoralOnlyRas, africaCountries_0, fun="sum", na.rm=TRUE, bind=TRUE)
  names(pastoral_0)[ncol(pastoral_0)] <- paste0(popLayer, "_pastoral")
  pastoral_0[[paste0(popLayer, "_pastoral")]] <- round(pastoral_0[[paste0(popLayer, "_pastoral")]], 0)
  
  agropastoral_1 <- terra::extract(agropastoralRas, africaCountries_1, fun="sum", na.rm=TRUE, bind=TRUE)
  names(agropastoral_1)[ncol(agropastoral_1)] <- paste0(popLayer, "_agropastoral")
  agropastoral_1[[paste0(popLayer, "_agropastoral")]] <- round(agropastoral_1[[paste0(popLayer, "_agropastoral")]], 0)
  agropastoral_1 <- agropastoral_1[, c(1,2,3,4,12)]
  
  pastoral_1 <- terra::extract(pastoralOnlyRas, africaCountries_1, fun="sum", na.rm=TRUE, bind=TRUE)
  names(pastoral_1)[ncol(pastoral_1)] <- paste0(popLayer, "_pastoral")
  pastoral_1[[paste0(popLayer, "_pastoral")]] <- round(pastoral_1[[paste0(popLayer, "_pastoral")]], 0)
  pastoral_1 <- pastoral_1[, c(1,2,3,4,12)]
  
  # Zonal statistics
  pastoralPop <- zonal(pastoralPopRas, pastoralZones, "sum", na.rm=TRUE) %>%
    as.data.frame() %>%
    setNames(c("Zone", popLayer))
  
  return(list(africa = pastoralPop, agropastoral_0 = agropastoral_0, pastoral_0 = pastoral_0, agropastoral_1 = agropastoral_1, pastoral_1 = pastoral_1))
}

# Process all layers
results <- lapply(popLayers, process_population_layer)
names(results) <- popLayers

# Merge results
pastoralPopAfrica <- Reduce(function(x,y) merge(x,y,by="Zone"), lapply(results, `[[`, "africa")) %>%
  mutate(Zone = recode(Zone, "1"="Agro-pastoral", "2"="Pastoral"))

# Merge agro-pastoral and pastoral data
agroPoly_0 <- results[[1]]$agropastoral_0
for(i in 2:length(results)) {
  agroPoly_0 <- merge(agroPoly_0, results[[i]]$agropastoral_0[c("GID_0", paste0(names(results)[i], "_agropastoral"))], by="GID_0")
}

pastoralPoly_0 <- results[[1]]$pastoral_0
for(i in 2:length(results)) {
  pastoralPoly_0 <- merge(pastoralPoly_0, results[[i]]$pastoral_0[c("GID_0", paste0(names(results)[i], "_pastoral"))], by="GID_0")
}

agroPoly_1 <- results[[1]]$agropastoral_1
for(i in 2:length(results)) {
  agroPoly_1 <- merge(agroPoly_1, results[[i]]$agropastoral_1[c("GID_1", paste0(names(results)[i], "_agropastoral"))], by="GID_1")
}

pastoralPoly_1 <- results[[1]]$pastoral_1
for(i in 2:length(results)) {
  pastoralPoly_1 <- merge(pastoralPoly_1, results[[i]]$pastoral_1[c("GID_1", paste0(names(results)[i], "_pastoral"))], by="GID_1")
}

# Combine agro-pastoral and pastoral for CSV
combined_0 <- merge(agroPoly_0, pastoralPoly_0[c("GID_0", paste0(popLayers, "_pastoral"))], by="GID_0")
combined_1 <- merge(agroPoly_1, pastoralPoly_1[c("GID_1", paste0(popLayers, "_pastoral"))], by="GID_1")

# Save outputs
write.csv(pastoralPopAfrica, paste0(resultsdir, "/Tabular_data/pastoralPopAfrica.csv"), row.names = FALSE)
write.csv(st_drop_geometry(combined_0), paste0(resultsdir, "/Tabular_data/pastoral_combined_countries_0.csv"), row.names = FALSE)
write.csv(st_drop_geometry(combined_1), paste0(resultsdir, "/Tabular_data/pastoral_combined_countries_1.csv"), row.names = FALSE)

# Save shapefiles for plotting
writeVector(agroPoly_0, paste0(resultsdir, "/Vector_data/agropastoral_countries_0.shp"), overwrite = TRUE)
writeVector(pastoralPoly_0, paste0(resultsdir, "/Vector_data/pastoral_countries_0.shp"), overwrite = TRUE)
writeVector(agroPoly_1, paste0(resultsdir, "/Vector_data/agropastoral_countries_1.shp"), overwrite = TRUE)
writeVector(pastoralPoly_1, paste0(resultsdir, "/Vector_data/pastoral_countries_1.shp"), overwrite = TRUE)

# Greater horn of africa
ghoa_combined_0 <- combined_0 %>% filter(GID_0 %in% c("DJI", "ERI", "ETH", "KEN", "SOM", "SSD", "SDN", "UGA"))
write.csv(st_drop_geometry(ghoa_combined_0), paste0(resultsdir, "/Tabular_data/pastoral_combined_ghoa_0.csv"), row.names = FALSE)
