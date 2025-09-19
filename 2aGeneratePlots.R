# Generate choropleth maps for pastoralist populations
# Creates side-by-side comparison maps for agro-pastoral vs pastoral populations
# Last updated: 16/8/2025

source("config.R")

options(warn = -1, scipen = 999)

# Load data
agroPoly_0 <- vect(paste0(resultsdir, "/Vector_data/agropastoral_countries_0.shp"))
pastoralPoly_0 <- vect(paste0(resultsdir, "/Vector_data/pastoral_countries_0.shp"))
agroPoly_1 <- vect(paste0(resultsdir, "/Vector_data/agropastoral_countries_1.shp"))
pastoralPoly_1 <- vect(paste0(resultsdir, "/Vector_data/pastoral_countries_1.shp"))
pastoralZones <- rast(paste0(outdir, "/pastoralZones.tif"))

# Convert to discrete categories
pastoralZones <- as.factor(pastoralZones)
levels(pastoralZones) <- data.frame(ID = c(1, 2), zone = c("Agro-pastoral", "Pastoral"))

pastoralZones <- ggplot(agroPoly_0) +
  geom_spatraster(data=pastoralZones, na.rm = TRUE) +
  geom_sf(data = agroPoly_0, fill = NA, color = "black", size = 0.5) +
  scale_fill_manual(values = c("Pastoral" = "#DEB887", "Agro-pastoral" = "#8FBC8F"),
                    name = "Zone type", na.translate = FALSE) +
  labs(title = "") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 0, size = 12, face = "bold"),
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(10, 10, 10, 10, "pt"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12))

ggsave(paste0(resultsdir, "/Plots/pastoral_zones.png"), pastoralZones, width = 8, height = 8, dpi = 300)

# WorldPop Maps
max_val_0_wp <- max(c(agroPoly_0$worldPop_a, pastoralPoly_0$worldPop_p), na.rm = TRUE)
max_val_1_wp <- max(c(agroPoly_1$worldPop_a, pastoralPoly_1$worldPop_p), na.rm = TRUE)

# WorldPop Country level (0)
p1a_wp <- ggplot(agroPoly_0) +
  geom_sf(aes(fill = worldPop_a), color = "black", size = 0.1) +
  scale_fill_gradient2(low = "white", mid = "orange", high = "darkred", 
                       limits = c(0, max_val_0_wp), midpoint = max_val_0_wp/2, name = "Population") +
  labs(title = "Agro-Pastoral (WorldPop)") +
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

p1b_wp <- ggplot(pastoralPoly_0) +
  geom_sf(aes(fill = worldPop_p), color = "black", size = 0.1) +
  scale_fill_gradient2(low = "white", mid = "orange", high = "darkred", 
                       limits = c(0, max_val_0_wp), midpoint = max_val_0_wp/2, name = "Population") +
  labs(title = "Pastoral (WorldPop)") +
  theme_void() +
  theme(legend.position = "right", legend.key.height = unit(1.5, "cm"), 
        legend.title = element_text(face = "bold", size = 12), legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

p1_wp <- p1a_wp + p1b_wp + plot_layout(guides = "collect") & theme(plot.margin = margin(5, 5, 5, 5))
ggsave(paste0(resultsdir, "/Plots/pastoral_countries_worldpop.png"), p1_wp, width = 16, height = 8, dpi = 300)

# WorldPop Admin level 1
p2a_wp <- ggplot(agroPoly_1) +
  geom_sf(aes(fill = worldPop_a), color = "black", size = 0.1) +
  scale_fill_gradient2(low = "white", mid = "orange", high = "darkred", 
                       limits = c(0, max_val_1_wp), midpoint = max_val_1_wp/2, name = "Population") +
  labs(title = "Agro-Pastoral (WorldPop)") +
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

p2b_wp <- ggplot(pastoralPoly_1) +
  geom_sf(aes(fill = worldPop_p), color = "black", size = 0.1) +
  scale_fill_gradient2(low = "white", mid = "orange", high = "darkred", 
                       limits = c(0, max_val_1_wp), midpoint = max_val_1_wp/2, name = "Population") +
  labs(title = "Pastoral (WorldPop)") +
  theme_void() +
  theme(legend.position = "right", legend.key.height = unit(1.5, "cm"), 
        legend.title = element_text(face = "bold", size = 12), legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

p2_wp <- p2a_wp + p2b_wp + plot_layout(guides = "collect") & theme(plot.margin = margin(5, 5, 5, 5))
ggsave(paste0(resultsdir, "/Plots/pastoral_admin1_worldpop.png"), p2_wp, width = 16, height = 8, dpi = 300)

# GPWv4 Maps
max_val_0_gpw <- max(c(agroPoly_0$gpwV4_agro, pastoralPoly_0$gpwV4_past), na.rm = TRUE)
max_val_1_gpw <- max(c(agroPoly_1$gpwV4_agro, pastoralPoly_1$gpwV4_past), na.rm = TRUE)

# GPWv4 Country level (0)
p1a_gpw <- ggplot(agroPoly_0) +
  geom_sf(aes(fill = gpwV4_agro), color = "black", size = 0.1) +
  scale_fill_gradient2(low = "white", mid = "orange", high = "darkred", 
                       limits = c(0, max_val_0_gpw), midpoint = max_val_0_gpw/2, name = "Population") +
  labs(title = "Agro-Pastoral (GPWv4)") +
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

p1b_gpw <- ggplot(pastoralPoly_0) +
  geom_sf(aes(fill = gpwV4_past), color = "black", size = 0.1) +
  scale_fill_gradient2(low = "white", mid = "orange", high = "darkred", 
                       limits = c(0, max_val_0_gpw), midpoint = max_val_0_gpw/2, name = "Population") +
  labs(title = "Pastoral (GPWv4)") +
  theme_void() +
  theme(legend.position = "right", legend.key.height = unit(1.5, "cm"), 
        legend.title = element_text(face = "bold", size = 12), legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

p1_gpw <- p1a_gpw + p1b_gpw + plot_layout(guides = "collect") & theme(plot.margin = margin(5, 5, 5, 5))
ggsave(paste0(resultsdir, "/Plots/pastoral_countries_gpwv4.png"), p1_gpw, width = 16, height = 8, dpi = 300)

# GPWv4 Admin level 1
p2a_gpw <- ggplot(agroPoly_1) +
  geom_sf(aes(fill = gpwV4_agro), color = "black", size = 0.1) +
  scale_fill_gradient2(low = "white", mid = "orange", high = "darkred", 
                       limits = c(0, max_val_1_gpw), midpoint = max_val_1_gpw/2, name = "Population") +
  labs(title = "Agro-Pastoral (GPWv4)") +
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

p2b_gpw <- ggplot(pastoralPoly_1) +
  geom_sf(aes(fill = gpwV4_past), color = "black", size = 0.1) +
  scale_fill_gradient2(low = "white", mid = "orange", high = "darkred", 
                       limits = c(0, max_val_1_gpw), midpoint = max_val_1_gpw/2, name = "Population") +
  labs(title = "Pastoral (GPWv4)") +
  theme_void() +
  theme(legend.position = "right", legend.key.height = unit(1.5, "cm"), 
        legend.title = element_text(face = "bold", size = 12), legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

p2_gpw <- p2a_gpw + p2b_gpw + plot_layout(guides = "collect") & theme(plot.margin = margin(5, 5, 5, 5))
ggsave(paste0(resultsdir, "/Plots/pastoral_admin1_gpwv4.png"), p2_gpw, width = 16, height = 8, dpi = 300)

# Combined all maps plot with unified scale
max_val_combined <- max(c(agroPoly_0$worldPop_a, pastoralPoly_0$worldPop_p, 
                          agroPoly_0$gpwV4_agro, pastoralPoly_0$gpwV4_past), na.rm = TRUE)

p1a_combined <- ggplot(agroPoly_0) +
  geom_sf(aes(fill = worldPop_a), color = "black", size = 0.1) +
  scale_fill_gradient2(low = "white", mid = "orange", high = "darkred", 
                       limits = c(0, max_val_combined), midpoint = max_val_combined/2, name = "Population") +
  labs(title = "Agro-Pastoral (WorldPop)") +
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

p1b_combined <- ggplot(pastoralPoly_0) +
  geom_sf(aes(fill = worldPop_p), color = "black", size = 0.1) +
  scale_fill_gradient2(low = "white", mid = "orange", high = "darkred", 
                       limits = c(0, max_val_combined), midpoint = max_val_combined/2, name = "Population") +
  labs(title = "Pastoral (WorldPop)") +
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

p2a_combined <- ggplot(agroPoly_0) +
  geom_sf(aes(fill = gpwV4_agro), color = "black", size = 0.1) +
  scale_fill_gradient2(low = "white", mid = "orange", high = "darkred", 
                       limits = c(0, max_val_combined), midpoint = max_val_combined/2, name = "Population") +
  labs(title = "Agro-Pastoral (GPWv4)") +
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

p2b_combined <- ggplot(pastoralPoly_0) +
  geom_sf(aes(fill = gpwV4_past), color = "black", size = 0.1) +
  scale_fill_gradient2(low = "white", mid = "orange", high = "darkred", 
                       limits = c(0, max_val_combined), midpoint = max_val_combined/2, name = "Population") +
  labs(title = "Pastoral (GPWv4)") +
  theme_void() +
  theme(legend.position = "right", legend.key.height = unit(1.5, "cm"), 
        legend.title = element_text(size = 12, face = "bold"), legend.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"))

all_maps <- (p1a_combined + p1b_combined) / (p2a_combined + p2b_combined) + plot_layout(guides = "collect")
ggsave(paste0(resultsdir, "/Plots/pastoral_all_maps_combined.png"), all_maps, width = 16, height = 16, dpi = 300)
