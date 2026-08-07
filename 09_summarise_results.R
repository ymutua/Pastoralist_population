# Summarise pastoralist population results
# 1. Regional analysis: top admin-1 units by region
# 2. Percent pastoralist population per country with HDI and GDP
# Last updated: 2025

library(terra)
library(exactextractr)
library(sf)
library(dplyr)
library(wbstats)

source("config.R")

# ── 1. Regional analysis ──────────────────────────────────────────────────────
combined_1 <- read.csv(paste0(resultsdir, "/Tabular_data/pastoral_combined_countries_1.csv"))

east_data  <- combined_1 %>% filter(GID_0 %in% east_africa_iso)
west_data  <- combined_1 %>% filter(GID_0 %in% west_africa_iso)
south_data <- combined_1 %>% filter(GID_0 %in% southern_africa_iso)

top_units <- function(data, col, n = 5) {
  data %>%
    arrange(desc(.data[[col]])) %>%
    slice(1:n) %>%
    select(GID_1, COUNTRY, NAME_1, all_of(col))
}

write.csv(top_units(east_data,  "worldPop_agropastoral"), paste0(resultsdir, "/Tabular_data/east_africa_top_agropastoral.csv"),  row.names = FALSE)
write.csv(top_units(east_data,  "worldPop_pastoral"),     paste0(resultsdir, "/Tabular_data/east_africa_top_pastoral.csv"),       row.names = FALSE)
write.csv(top_units(west_data,  "worldPop_agropastoral"), paste0(resultsdir, "/Tabular_data/west_africa_top_agropastoral.csv"),   row.names = FALSE)
write.csv(top_units(west_data,  "worldPop_pastoral"),     paste0(resultsdir, "/Tabular_data/west_africa_top_pastoral.csv"),       row.names = FALSE)
write.csv(top_units(south_data, "worldPop_agropastoral"), paste0(resultsdir, "/Tabular_data/southern_africa_top_agropastoral.csv"), row.names = FALSE)
write.csv(top_units(south_data, "worldPop_pastoral"),     paste0(resultsdir, "/Tabular_data/southern_africa_top_pastoral.csv"),   row.names = FALSE)

cat("Regional analysis saved\n")

# ── 2. Percent pastoralist population per country ─────────────────────────────
countries    <- st_read(paste0(indir, "/africa_admin_0.shp"), quiet = TRUE)
agroPoly_0   <- st_read(paste0(resultsdir, "/Vector_data/agropastoral_countries_0.shp"), quiet = TRUE)
pastoralPoly_0 <- st_read(paste0(resultsdir, "/Vector_data/pastoral_countries_0.shp"), quiet = TRUE)

countries$total_worldPop <- exact_extract(rast(paste0(indir, "/humanPop_worldPop.tif")), countries, "sum")
countries$total_gpwV4    <- exact_extract(rast(paste0(indir, "/humanPop_gpwV4.tif")),    countries, "sum")

result <- st_drop_geometry(countries) %>%
  merge(st_drop_geometry(agroPoly_0)[c("COUNTRY", "worldPop_a", "gpwV4_agro")],   by = "COUNTRY", all.x = TRUE) %>%
  merge(st_drop_geometry(pastoralPoly_0)[c("COUNTRY", "worldPop_p", "gpwV4_past")], by = "COUNTRY", all.x = TRUE)

result[is.na(result)] <- 0

pop_summary <- result %>%
  mutate(
    worldpop_total_pastoralists     = worldPop_a + worldPop_p,
    gpwv4_total_pastoralists        = gpwV4_agro + gpwV4_past,
    worldpop_total_pastoralists_pct = (worldPop_a + worldPop_p) / total_worldPop * 100,
    gpwv4_total_pastoralists_pct    = (gpwV4_agro + gpwV4_past) / total_gpwV4    * 100
  ) %>%
  select(COUNTRY, worldpop_total_pastoralists, worldpop_total_pastoralists_pct,
         gpwv4_total_pastoralists, gpwv4_total_pastoralists_pct) %>%
  rename(country = COUNTRY)

pop_summaryAveraged <- result %>%
  select(COUNTRY, worldPop_a, worldPop_p, gpwV4_agro, gpwV4_past) %>%
  rename(country = COUNTRY, worldpop_agropastoral = worldPop_a,
         worldpop_pastoral = worldPop_p, gpwv4_agropastoral = gpwV4_agro,
         gpwv4_pastoral = gpwV4_past)

write.csv(pop_summaryAveraged, paste0(resultsdir, "/Tabular_data/pop_summaryAveraged.csv"), row.names = FALSE)

# HDI: UNDP Human Development Index 2022 (UNDP Human Development Report 2023/2024)
# Source: https://hdr.undp.org/sites/default/files/2023-24_HDR/HDR23-24_Composite_indices_complete_time_series.csv
hdi_url  <- "https://hdr.undp.org/sites/default/files/2023-24_HDR/HDR23-24_Composite_indices_complete_time_series.csv"
hdi_dest <- file.path(resultsdir, "Tabular_data/undp_hdi.csv")
download.file(hdi_url, hdi_dest, quiet = TRUE)
worldhdi <- read.csv(hdi_dest) %>%
  select(country, hdi_2022)

# GDP per capita 2023 (World Bank World Development Indicators, NY.GDP.PCAP.CD)
gdp_2020 <- wb_data(indicator = "NY.GDP.PCAP.CD", start_date = 2023, end_date = 2023) %>%
  mutate(country = recode(country,
    "Congo, Dem. Rep."    = "Democratic Republic of the Congo",
    "Congo, Rep."         = "Republic of the Congo",
    "Sao Tome and Principe" = "São Tomé and Príncipe",
    "Eswatini"            = "Swaziland",
    "Cote d'Ivoire"       = "Côte d'Ivoire",
    "Egypt, Arab Rep."    = "Egypt",
    "Gambia, The"         = "Gambia"
  )) %>%
  select(country, NY.GDP.PCAP.CD) %>%
  rename(gdp_percap2023 = NY.GDP.PCAP.CD)

pastoralist_otherStats <- pop_summary %>%
  left_join(worldhdi %>% select(country, hdi_2022), by = "country") %>%
  left_join(gdp_2020, by = "country") %>%
  select(country, worldpop_total_pastoralists_pct, gpwv4_total_pastoralists_pct, hdi_2022, gdp_percap2023) %>%
  rename(
    Country                                                        = country,
    `Total pastoralists (% of Total country population, Worldpop)` = worldpop_total_pastoralists_pct,
    `Total pastoralists (% of Total country population, gpwV4)`    = gpwv4_total_pastoralists_pct,
    HDI                                                            = hdi_2022,
    `GDP per capita (US)`                                          = gdp_percap2023
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 1)))

write.csv(pastoralist_otherStats, paste0(resultsdir, "/Tabular_data/pastoralist_otherStats.csv"), row.names = FALSE)
cat("Percent pastoralist and socioeconomic stats saved\n")
