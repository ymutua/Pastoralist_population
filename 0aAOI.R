# Prepare Area of Interest (AOI) - Africa admin boundaries at multiple levels
# Downloads and processes GADM data for African countries
# Last updated: 16/8/2025

source("config.R")

# Get African country codes from online repository
countryList <- read.csv("https://pkgstore.datahub.io/JohnSnowLabs/country-and-continent-codes-list/country-and-continent-codes-list-csv_csv/data/b7876b7f496677669644f3d1069d3121/country-and-continent-codes-list-csv_csv.csv")
names(countryList)[5] <- "GID_0"
africaCountries <- subset(countryList, Continent_Code=="AF")
africaCountryCodes <- africaCountries$GID_0

# Process multiple admin levels efficiently
process_admin_level <- function(level) {
  gadm(country = africaCountryCodes, level = level, path = outdir, version = "latest") %>%
    st_as_sf() %>% vect() %>% makeValid() %>%
    tidyterra::filter(!GID_0 %in% c("REU", "SHN", "MYT"))
}

# Download and process levels 0 and 1
levels <- 0:1
africa_admin_list <- lapply(levels, process_admin_level)

# Save each level
for(i in seq_along(levels)) {
  writeVector(africa_admin_list[[i]], paste0(outdir, "/africa_admin_", levels[i], ".shp"), overwrite = TRUE)
}

# Create dissolved boundaries from level 0
africa_admin_dissolved <- aggregate(africa_admin_list[[1]])
writeVector(africa_admin_dissolved, paste0(outdir, "/africa_admin_dissolved.shp"), overwrite = TRUE)
