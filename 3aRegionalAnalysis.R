# Regional analysis of pastoralist populations
# Identifies admin 1 units with largest agro-pastoral and pastoral populations by region
# Last updated: 16/8/2025

source("config.R")

# Load combined admin 1 data
combined_1 <- read.csv(paste0(resultsdir, "/Tabular_data/pastoral_combined_countries_1.csv"))

# Define regional country codes
east_africa <- c("DJI", "ERI", "ETH", "KEN", "SOM", "SSD", "SDN", "UGA", "TZA", "RWA", "BDI")
west_africa <- c("BEN", "BFA", "CIV", "CPV", "GHA", "GIN", "GMB", "GNB", "LBR", "MLI", "MRT", "NER", "NGA", "SEN", "SLE", "TGO")
southern_africa <- c("AGO", "BWA", "LSO", "MWI", "MOZ", "NAM", "SWZ", "ZAF", "ZMB", "ZWE")

# Filter by regions
east_data <- combined_1 %>% filter(GID_0 %in% east_africa)
west_data <- combined_1 %>% filter(GID_0 %in% west_africa)
south_data <- combined_1 %>% filter(GID_0 %in% southern_africa)

# Find top admin 1 units by region
# East Africa
east_top_agro <- east_data %>% 
  arrange(desc(worldPop_agropastoral)) %>% 
  slice(1:5) %>%
  select(GID_1, COUNTRY, NAME_1, worldPop_agropastoral, gpwV4_agropastoral)

east_top_pastoral <- east_data %>% 
  arrange(desc(worldPop_pastoral)) %>% 
  slice(1:5) %>%
  select(GID_1, COUNTRY, NAME_1, worldPop_pastoral, gpwV4_pastoral)

# West Africa
west_top_agro <- west_data %>% 
  arrange(desc(worldPop_agropastoral)) %>% 
  slice(1:5) %>%
  select(GID_1, COUNTRY, NAME_1, worldPop_agropastoral, gpwV4_agropastoral)

west_top_pastoral <- west_data %>% 
  arrange(desc(worldPop_pastoral)) %>% 
  slice(1:5) %>%
  select(GID_1, COUNTRY, NAME_1, worldPop_pastoral, gpwV4_pastoral)

# Southern Africa
south_top_agro <- south_data %>% 
  arrange(desc(worldPop_agropastoral)) %>% 
  slice(1:5) %>%
  select(GID_1, COUNTRY, NAME_1, worldPop_agropastoral, gpwV4_agropastoral)

south_top_pastoral <- south_data %>% 
  arrange(desc(worldPop_pastoral)) %>% 
  slice(1:5) %>%
  select(GID_1, COUNTRY, NAME_1, worldPop_pastoral, gpwV4_pastoral)

# Save results
write.csv(east_top_agro, paste0(resultsdir, "/Tabular_data/east_africa_top_agropastoral.csv"), row.names = FALSE)
write.csv(east_top_pastoral, paste0(resultsdir, "/Tabular_data/east_africa_top_pastoral.csv"), row.names = FALSE)
write.csv(west_top_agro, paste0(resultsdir, "/Tabular_data/west_africa_top_agropastoral.csv"), row.names = FALSE)
write.csv(west_top_pastoral, paste0(resultsdir, "/Tabular_data/west_africa_top_pastoral.csv"), row.names = FALSE)
write.csv(south_top_agro, paste0(resultsdir, "/Tabular_data/southern_africa_top_agropastoral.csv"), row.names = FALSE)
write.csv(south_top_pastoral, paste0(resultsdir, "/Tabular_data/southern_africa_top_pastoral.csv"), row.names = FALSE)