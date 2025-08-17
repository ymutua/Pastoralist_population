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

# Get common scale limits for comparison
max_val_0 <- max(c(agroPoly_0$worldPop_a, pastoralPoly_0$worldPop_p), na.rm = TRUE)
max_val_1 <- max(c(agroPoly_1$worldPop_a, pastoralPoly_1$worldPop_p), na.rm = TRUE)

# Country level (0) plots
p1a <- ggplot(agroPoly_0) +
  geom_sf(aes(fill = worldPop_a), color = "black", size = 0.1) +
  scale_fill_gradient2(low = "white", mid = "orange", high = "darkred", 
                       limits = c(0, max_val_0),
                       midpoint = max_val_0/2,
                       name = "Population") +
  labs(title = "Agro-Pastoral") +
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, vjust = 0, size = 12, face = "bold"))

p1b <- ggplot(pastoralPoly_0) +
  geom_sf(aes(fill = worldPop_p), color = "black", size = 0.1) +
  scale_fill_gradient2(low = "white", mid = "orange", high = "darkred", 
                       limits = c(0, max_val_0),
                       midpoint = max_val_0/2,
                       name = "Population") +
  labs(title = "Pastoral") +
  theme_void() +
  theme(legend.position = "right", legend.key.height = unit(1.5, "cm"), legend.title = element_text(face = "bold", size = 12), legend.text = element_text(size = 12), plot.title = element_text(hjust = 0.5, vjust = 0, size = 12, face = "bold"))

p1_combined <- p1a + p1b + plot_layout(guides = "collect") & theme(plot.margin = margin(5, 5, 5, 5))
ggsave(paste0(resultsdir, "/Plots/pastoral_countries_combined.png"), p1_combined, width = 16, height = 8, dpi = 300)

# Admin level 1 plots
p2a <- ggplot(agroPoly_1) +
  geom_sf(aes(fill = worldPop_a), color = "black", size = 0.1) +
  scale_fill_gradient2(low = "white", mid = "orange", high = "darkred", 
                       limits = c(0, max_val_1),
                       midpoint = max_val_1/2,
                       name = "Population") +
  labs(title = "Agro-Pastoral") +
  theme_void() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5, vjust = 0, size = 12, face = "bold"))

p2b <- ggplot(pastoralPoly_1) +
  geom_sf(aes(fill = worldPop_p), color = "black", size = 0.1) +
  scale_fill_gradient2(low = "white", mid = "orange", high = "darkred", 
                       limits = c(0, max_val_1),
                       midpoint = max_val_1/2,
                       name = "Population") +
  labs(title = "Pastoral") +
  theme_void() +
  theme(legend.position = "right", legend.key.height = unit(1.5, "cm"), legend.title = element_text(face = "bold", size = 12), legend.text = element_text(size = 12), plot.title = element_text(hjust = 0.5, vjust = 0, size = 12, face = "bold"))

p2_combined <- p2a + p2b + plot_layout(guides = "collect") & theme(plot.margin = margin(5, 5, 5, 5))
ggsave(paste0(resultsdir, "/Plots/pastoral_admin1_combined.png"), p2_combined, width = 16, height = 8, dpi = 300)
