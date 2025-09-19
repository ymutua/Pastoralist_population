library(terra)
library(exactextractr)
library(sf)
library(worldhdi)
library(wbstats)

source("config.R")

# Load data
countries <- st_read(paste0(indir, "/africa_admin_0.shp"))
agroPoly_0 <- st_read(paste0(resultsdir, "/Vector_data/agropastoral_countries_0.shp"))
pastoralPoly_0 <- st_read(paste0(resultsdir, "/Vector_data/pastoral_countries_0.shp"))


# Extract total populations
countries$total_worldPop <- exact_extract(rast(paste0(indir, "/humanPop_worldPop.tif")), countries, 'sum')
countries$total_gpwV4 <- exact_extract(rast(paste0(indir, "/humanPop_gpwV4.tif")), countries, 'sum')

# Merge and calculate
result <- merge(merge(st_drop_geometry(countries), 
                      st_drop_geometry(agroPoly_0)[c("COUNTRY", "worldPop_a", "gpwV4_agro")], 
                      by = "COUNTRY", all.x = TRUE),
                st_drop_geometry(pastoralPoly_0)[c("COUNTRY", "worldPop_p", "gpwV4_past")], 
                by = "COUNTRY", all.x = TRUE)

result[is.na(result)] <- 0

# Final table
pop_summary <- data.frame(
  country = result$COUNTRY,
  worldpop_total = result$total_worldPop,
  gpwv4_total = result$total_gpwV4,
  worldpop_agropastoral = result$worldPop_a,
  worldpop_pastoral = result$worldPop_p,
  worldpop_total_pastoralists = result$worldPop_a + result$worldPop_p,
  gpwv4_agropastoral = result$gpwV4_agro,
  gpwv4_pastoral = result$gpwV4_past,
  gpwv4_total_pastoralists = result$gpwV4_agro + result$gpwV4_past,
  worldpop_total_pastoralists_pct = ((result$worldPop_a + result$worldPop_p) / result$total_worldPop) * 100,
  gpwv4_total_pastoralists_pct = ((result$gpwV4_agro + result$gpwV4_past) / result$total_gpwV4) * 100
) %>% 
  select(country, worldpop_total_pastoralists, worldpop_total_pastoralists_pct, gpwv4_total_pastoralists, gpwv4_total_pastoralists_pct)

# Replace names
worldhdi <- worldhdi %>%
  mutate(country = recode(country,
                          "Congo (Democratic Republic of the)" = "Democratic Republic of the Congo",
                          "Congo" = "Republic of the Congo",
                          "Sao Tome and Principe" = "São Tomé and Príncipe",
                          "Somalia" = "Somalia",
                          "Eswatini (Kingdom of)" = "Swaziland",
                          "Tanzania (United Republic of)" = "Tanzania",
                          "Western Sahara" = "Western Sahara"
  )) 
  
  
pastoralist_otherVariables <- pop_summary %>% 
  left_join(worldhdi %>% select(country, hdi_2020), by="country")

# Get GDP per capita 2020
gdp_2020 <- wb_data(indicator = "NY.GDP.PCAP.CD", start_date = 2020, end_date = 2020) %>%
  mutate(country = recode(country,
                          "Congo, Dem. Rep." = "Democratic Republic of the Congo",
                          "Congo, Rep." = "Republic of the Congo",
                          "Sao Tome and Principe" = "São Tomé and Príncipe",
                          "Eswatini" = "Swaziland",
                          "Cote d'Ivoire" = "Côte d'Ivoire",
                          "Egypt, Arab Rep."="Egypt",
                          "Gambia, The" = "Gambia"
  )) %>% 
  select(country, NY.GDP.PCAP.CD) %>% 
  rename(gdp_percap2020 = NY.GDP.PCAP.CD)

pastoralist_otherStats <- pastoralist_otherVariables %>% 
  left_join(gdp_2020, by="country") %>% 
  select(country, worldpop_total_pastoralists_pct, gpwv4_total_pastoralists_pct, hdi_2020, gdp_percap2020) %>% 
  rename(Country = country,
         `Total pastoralists (% of Total country population, Worldpop)`=worldpop_total_pastoralists_pct,
         `Total pastoralists (% of Total country population, gpwV4)`=gpwv4_total_pastoralists_pct,
         HDI=hdi_2020,
         `GDP per capita (US)`=gdp_percap2020) %>% 
  mutate(across(where(is.numeric), ~ round(., 1)))

write.csv(pastoralist_otherStats, paste0(resultsdir, "/Tabular_data/pastoralist_otherStats.csv"), row.names = FALSE)
