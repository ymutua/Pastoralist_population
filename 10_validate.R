# Validation of pastoral zone delineation and population estimates
# 1. FEWSNET livelihood zone spatial comparison
# 2. Subnational WorldPop vs GPWv4 comparison across GHOA
# Last updated: 2025

library(terra)
library(sf)
library(dplyr)
library(ggplot2)

source("config.R")
source("styles.R")

indir      <- file.path(root, "SpatialData/Inputs")
resultsdir <- file.path(root, "Results")

# ── 1. FEWSNET zone comparison ────────────────────────────────────────────────
modelled        <- rast(file.path(indir, "pastoralZones.tif"))
fewsnet         <- rast(file.path(indir, "LZ.tif"))
fewsnet_aligned <- resample(fewsnet, modelled, method = "near")

stack        <- c(modelled, fewsnet_aligned)
names(stack) <- c("modelled", "fewsnet")

df_fewsnet <- as.data.frame(stack, na.rm = FALSE) %>%
  filter(!is.na(modelled) | !is.na(fewsnet)) %>%
  mutate(
    agreement = case_when(
      modelled == 1 & fewsnet == 1 ~ "Both agro-pastoral",
      modelled == 2 & fewsnet == 2 ~ "Both pastoral",
      modelled == 1 & fewsnet == 2 ~ "Modelled agro / FEWSNET pastoral",
      modelled == 2 & fewsnet == 1 ~ "Modelled pastoral / FEWSNET agro",
      !is.na(modelled) & is.na(fewsnet) ~ "Modelled only",
      is.na(modelled) & !is.na(fewsnet) ~ "FEWSNET only",
      TRUE ~ "Neither"
    )
  )

total_modelled <- sum(!is.na(df_fewsnet$modelled))
total_fewsnet  <- sum(!is.na(df_fewsnet$fewsnet))
both_pastoral  <- df_fewsnet %>% filter(!is.na(modelled) & !is.na(fewsnet))
n_agree        <- sum(both_pastoral$modelled == both_pastoral$fewsnet)
pct_agree      <- round(n_agree / nrow(both_pastoral) * 100, 1)

zone_agreement <- df_fewsnet %>%
  filter(!is.na(modelled) & !is.na(fewsnet)) %>%
  count(agreement) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  arrange(desc(n))

fewsnet_captured <- df_fewsnet %>%
  filter(!is.na(fewsnet)) %>%
  summarise(
    total_fewsnet_pixels = n(),
    captured_by_model    = sum(!is.na(modelled)),
    pct_captured         = round(captured_by_model / total_fewsnet_pixels * 100, 1)
  )

cat("\n=== FEWSNET vs Modelled Pastoral Zone Comparison ===\n\n")
cat(sprintf("Modelled pastoral pixels:  %s\n", format(total_modelled, big.mark = ",")))
cat(sprintf("FEWSNET pastoral pixels:   %s\n", format(total_fewsnet,  big.mark = ",")))
cat(sprintf("Agreement (same zone):     %s pixels (%s%%)\n\n", format(n_agree, big.mark = ","), pct_agree))
print(as.data.frame(zone_agreement),   row.names = FALSE)
print(as.data.frame(fewsnet_captured), row.names = FALSE)

write.csv(zone_agreement,    file.path(resultsdir, "Tabular_data/fewsnet_zone_agreement.csv"),  row.names = FALSE)
write.csv(fewsnet_captured,  file.path(resultsdir, "Tabular_data/fewsnet_capture_rate.csv"),    row.names = FALSE)
cat("FEWSNET outputs saved\n")

# ── 2. Subnational WorldPop vs GPWv4 comparison (GHOA) ───────────────────────
pastoral     <- st_drop_geometry(st_read(file.path(resultsdir, "Vector_data/pastoral_countries_1.shp"),     quiet = TRUE))
agropastoral <- st_drop_geometry(st_read(file.path(resultsdir, "Vector_data/agropastoral_countries_1.shp"), quiet = TRUE))

pastoral_df <- pastoral %>%
  filter(COUNTRY %in% ghoa_countries) %>%
  filter(!is.na(worldPop_p) & !is.na(gpwV4_past)) %>%
  filter(worldPop_p > 0 | gpwV4_past > 0) %>%
  mutate(zone = "Pastoral", worldPop = worldPop_p, gpwV4 = gpwV4_past,
         pct_diff = (gpwV4 - worldPop) / worldPop * 100) %>%
  select(GID_1, GID_0, COUNTRY, NAME_1, zone, worldPop, gpwV4, pct_diff)

agro_df <- agropastoral %>%
  filter(COUNTRY %in% ghoa_countries) %>%
  filter(!is.na(worldPop_a) & !is.na(gpwV4_agro)) %>%
  filter(worldPop_a > 0 | gpwV4_agro > 0) %>%
  mutate(zone = "Agro-pastoral", worldPop = worldPop_a, gpwV4 = gpwV4_agro,
         pct_diff = (gpwV4 - worldPop) / worldPop * 100) %>%
  select(GID_1, GID_0, COUNTRY, NAME_1, zone, worldPop, gpwV4, pct_diff)

df_sub <- bind_rows(pastoral_df, agro_df) %>%
  mutate(
    COUNTRY = factor(COUNTRY, levels = ghoa_countries),
    zone    = factor(zone, levels = c("Pastoral", "Agro-pastoral"))
  )

summary_tbl <- df_sub %>%
  group_by(COUNTRY, zone) %>%
  summarise(
    n_units    = n(),
    worldPop   = sum(worldPop, na.rm = TRUE),
    gpwV4      = sum(gpwV4,    na.rm = TRUE),
    median_pct = round(median(pct_diff, na.rm = TRUE), 1),
    min_pct    = round(min(pct_diff,    na.rm = TRUE), 1),
    max_pct    = round(max(pct_diff,    na.rm = TRUE), 1),
    .groups    = "drop"
  )

cat("\n=== Subnational WorldPop vs GPWv4 by zone type (GHOA) ===\n\n")
print(as.data.frame(summary_tbl), row.names = FALSE)

max_val <- max(c(df_sub$worldPop, df_sub$gpwV4), na.rm = TRUE)

p_scatter <- ggplot(df_sub, aes(x = worldPop / 1e6, y = gpwV4 / 1e6, colour = COUNTRY)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(size = 3.5, alpha = 0.85) +
  facet_wrap(~zone) +
  scale_colour_manual(values = ghoa_colours) +
  scale_x_continuous(labels = scales::label_comma()) +
  scale_y_continuous(labels = scales::label_comma()) +
  coord_equal(xlim = c(0, max_val / 1e6), ylim = c(0, max_val / 1e6)) +
  labs(x = "WorldPop (million)", y = "GPWv4 (million)", colour = NULL) +
  theme_pastoral()

save_plot(p_scatter, "subnational_worldpop_gpwv4_by_zone.png", width = 10, height = 5)
write.csv(summary_tbl, file.path(resultsdir, "Tabular_data/subnational_comparison_by_zone.csv"), row.names = FALSE)
cat("Subnational comparison saved\n")
