# Generate all figures for the paper
# 1. Choropleth maps (pastoral zones, country and admin-1 level)
# 2. Results comparison plot (Figure 4)
# Last updated: 2025

library(ggplot2)
library(stringr)
library(dplyr)
library(tidyr)
library(scales)
library(patchwork)
library(ggspatial)
library(cowplot)

source("config.R")
source("styles.R")

resultsdir <- file.path(root, "Results")
outdir     <- file.path(root, "SpatialData/Inputs")
dir.create(file.path(resultsdir, "Plots"), showWarnings = FALSE)
options(warn = -1, scipen = 999)

# ── Shared helpers ────────────────────────────────────────────────────────────
comma_fmt   <- scales::label_comma(accuracy = 1)
africa_bbox <- c(xmin = -18, xmax = 52, ymin = -35, ymax = 38)

get_legend <- function(p) {
  g <- ggplotGrob(p)
  g$grobs[[which(sapply(g$grobs, function(x) x$name) == "guide-box")]]
}

nodata_label <- ggplot() +
  geom_tile(aes(x = 1, y = 1, fill = "No Data"), width = 0.5, height = 0.5) +
  scale_fill_manual(values = c("No Data" = "grey80"), name = "") +
  theme_void() +
  theme(legend.position = "right", legend.key.size = unit(0.45, "cm"),
        legend.text = element_text(size = 12),
        legend.key  = element_rect(fill = "grey80", color = "black", linewidth = 0.3),
        legend.margin = margin(0,0,0,0), legend.box.margin = margin(0,0,0,0))
nodata_leg <- get_legend(nodata_label)

# ── Load vectors ──────────────────────────────────────────────────────────────
agroPoly_0   <- vect(paste0(resultsdir, "/Vector_data/agropastoral_countries_0.shp"))
pastoralPoly_0 <- vect(paste0(resultsdir, "/Vector_data/pastoral_countries_0.shp"))
agroPoly_1   <- vect(paste0(resultsdir, "/Vector_data/agropastoral_countries_1.shp"))
pastoralPoly_1 <- vect(paste0(resultsdir, "/Vector_data/pastoral_countries_1.shp"))

# ── 1. Pastoral zones map ─────────────────────────────────────────────────────
pz_file <- paste0(outdir, "/pastoralZones.tif")
if (file.exists(pz_file)) {
  pz <- as.factor(rast(pz_file))
  levels(pz) <- data.frame(ID = c(1, 2), zone = c("Agro-pastoral", "Pastoral"))
  p_zones <- ggplot(agroPoly_0) +
    geom_spatraster(data = pz, na.rm = TRUE) +
    geom_sf(data = agroPoly_0, fill = NA, color = "black", size = 0.5) +
    scale_fill_manual(values = c("Pastoral" = col_pastoral, "Agro-pastoral" = col_agropastoral),
                      name = "Zone type", na.translate = FALSE) +
    coord_sf(xlim = c(africa_bbox["xmin"], africa_bbox["xmax"]),
             ylim = c(africa_bbox["ymin"], africa_bbox["ymax"]), expand = FALSE) +
    annotation_scale(width_hint = 0.25, style = "bar", pad_x = unit(0.05, "npc"),
                     pad_y = unit(0.03, "npc"), text_cex = 1.0, height = unit(0.3, "cm"), line_width = 1.5) +
    annotation_north_arrow(location = "tl", which_north = "true",
                           style = north_arrow_fancy_orienteering(text_size = 10)) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", color = NA), plot.margin = margin(10,10,10,10,"pt"),
          legend.title = element_text(size = 12, face = "bold"), legend.text = element_text(size = 12))
  ggsave(paste0(resultsdir, "/Plots/pastoral_zones.png"), p_zones, width = 8, height = 8, dpi = 300)
  cat("Saved: pastoral_zones.png\n")
}

# ── Helper: single choropleth panel ──────────────────────────────────────────
map_panel <- function(poly, fill_col, title, max_val, show_legend = FALSE, show_scale = FALSE) {
  p <- ggplot(poly) +
    geom_sf(aes(fill = .data[[fill_col]]), color = "black", size = 0.1) +
    scale_fill_gradient2(low = "white", mid = "orange", high = "darkred",
                         limits = c(0, max_val), midpoint = max_val / 2,
                         name = "Population count", na.value = "grey80", labels = comma_fmt) +
    coord_sf(xlim = c(africa_bbox["xmin"], africa_bbox["xmax"]),
             ylim = c(africa_bbox["ymin"], africa_bbox["ymax"]), expand = FALSE) +
    labs(title = title) +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
          plot.margin = margin(5,5,5,5))
  if (show_scale) {
    p <- p +
      annotation_scale(width_hint = 0.25, style = "bar", pad_x = unit(0.05, "npc"),
                       pad_y = unit(0.03, "npc"), text_cex = 1.0, height = unit(0.3, "cm"), line_width = 1.5) +
      annotation_north_arrow(location = "tl", which_north = "true",
                             style = north_arrow_fancy_orienteering(text_size = 10))
  }
  if (show_legend) {
    p <- p + theme(legend.position = "right", legend.key.height = unit(1.5, "cm"),
                   legend.title = element_text(face = "bold", size = 12),
                   legend.text  = element_text(size = 12))
  } else {
    p <- p + theme(legend.position = "none")
  }
  p
}

# ── 2. WorldPop country-level maps ───────────────────────────────────────────
mv0_wp <- max(c(agroPoly_0$worldPop_a, pastoralPoly_0$worldPop_p), na.rm = TRUE)
p1_wp  <- map_panel(agroPoly_0,   "worldPop_a", "Agro-Pastoral (WorldPop)", mv0_wp, show_scale = TRUE) +
          map_panel(pastoralPoly_0, "worldPop_p", "Pastoral (WorldPop)",      mv0_wp, show_legend = TRUE, show_scale = TRUE)
p1_wp  <- p1_wp + plot_layout(guides = "collect") & theme(plot.margin = margin(5,5,5,5))
p1_wp  <- p1_wp + inset_element(nodata_leg, left = 0.88, bottom = 0.02, right = 1.0, top = 0.15)
ggsave(paste0(resultsdir, "/Plots/pastoral_countries_worldpop.png"), p1_wp, width = 16, height = 8, dpi = 300)
cat("Saved: pastoral_countries_worldpop.png\n")

# ── 3. WorldPop admin-1 maps ──────────────────────────────────────────────────
mv1_wp <- max(c(agroPoly_1$worldPop_a, pastoralPoly_1$worldPop_p), na.rm = TRUE)
p2_wp  <- map_panel(agroPoly_1,   "worldPop_a", "Agro-Pastoral (WorldPop)", mv1_wp, show_scale = TRUE) +
          map_panel(pastoralPoly_1, "worldPop_p", "Pastoral (WorldPop)",      mv1_wp, show_legend = TRUE, show_scale = TRUE)
p2_wp  <- p2_wp + plot_layout(guides = "collect") & theme(plot.margin = margin(5,5,5,5))
p2_wp  <- p2_wp + inset_element(nodata_leg, left = 0.88, bottom = 0.02, right = 1.0, top = 0.15)
ggsave(paste0(resultsdir, "/Plots/pastoral_admin1_worldpop.png"), p2_wp, width = 16, height = 8, dpi = 300)
cat("Saved: pastoral_admin1_worldpop.png\n")

# ── 4. GPWv4 country-level maps ───────────────────────────────────────────────
mv0_gpw <- max(c(agroPoly_0$gpwV4_agro, pastoralPoly_0$gpwV4_past), na.rm = TRUE)
p1_gpw  <- map_panel(agroPoly_0,   "gpwV4_agro",  "Agro-Pastoral (GPWv4)", mv0_gpw, show_scale = TRUE) +
           map_panel(pastoralPoly_0, "gpwV4_past", "Pastoral (GPWv4)",      mv0_gpw, show_legend = TRUE, show_scale = TRUE)
p1_gpw  <- p1_gpw + plot_layout(guides = "collect") & theme(plot.margin = margin(5,5,5,5))
p1_gpw  <- p1_gpw + inset_element(nodata_leg, left = 0.88, bottom = 0.02, right = 1.0, top = 0.15)
ggsave(paste0(resultsdir, "/Plots/pastoral_countries_gpwv4.png"), p1_gpw, width = 16, height = 8, dpi = 300)
cat("Saved: pastoral_countries_gpwv4.png\n")

# ── 5. GPWv4 admin-1 maps ─────────────────────────────────────────────────────
mv1_gpw <- max(c(agroPoly_1$gpwV4_agro, pastoralPoly_1$gpwV4_past), na.rm = TRUE)
p2_gpw  <- map_panel(agroPoly_1,   "gpwV4_agro",  "Agro-Pastoral (GPWv4)", mv1_gpw, show_scale = TRUE) +
           map_panel(pastoralPoly_1, "gpwV4_past", "Pastoral (GPWv4)",      mv1_gpw, show_legend = TRUE, show_scale = TRUE)
p2_gpw  <- p2_gpw + plot_layout(guides = "collect") & theme(plot.margin = margin(5,5,5,5))
p2_gpw  <- p2_gpw + inset_element(nodata_leg, left = 0.88, bottom = 0.02, right = 1.0, top = 0.15)
ggsave(paste0(resultsdir, "/Plots/pastoral_admin1_gpwv4.png"), p2_gpw, width = 16, height = 8, dpi = 300)
cat("Saved: pastoral_admin1_gpwv4.png\n")

# ── 6. Combined 2x2 map ───────────────────────────────────────────────────────
mv_all <- max(c(agroPoly_0$worldPop_a, pastoralPoly_0$worldPop_p,
                agroPoly_0$gpwV4_agro, pastoralPoly_0$gpwV4_past), na.rm = TRUE)

p_combined_legend <- map_panel(pastoralPoly_0, "gpwV4_past", "Pastoral (GPWv4)", mv_all,
                                show_legend = TRUE, show_scale = FALSE) +
  theme(legend.key.height = unit(5, "cm"),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text  = element_text(size = 12))

nodata_entry <- ggplot() +
  geom_tile(aes(x = 1, y = 1, fill = "No Data"), width = 0.5, height = 0.5) +
  scale_fill_manual(values = c("No Data" = "grey80"), name = "") +
  theme_void() +
  theme(legend.position = "right", legend.key.size = unit(0.5, "cm"),
        legend.text = element_text(size = 12),
        legend.key  = element_rect(fill = "grey80", color = "black", linewidth = 0.3),
        legend.margin = margin(0,0,0,0), legend.box.margin = margin(0,0,0,0))

combined_legend <- plot_grid(get_legend(p_combined_legend), get_legend(nodata_entry),
                             ncol = 1, align = "v", rel_heights = c(0.6, 0.1))

maps_grid <- (map_panel(agroPoly_0,    "worldPop_a",  "Agro-Pastoral (WorldPop)", mv_all) +
              map_panel(pastoralPoly_0, "worldPop_p",  "Pastoral (WorldPop)",      mv_all)) /
             (map_panel(agroPoly_0,    "gpwV4_agro",  "Agro-Pastoral (GPWv4)",    mv_all, show_scale = TRUE) +
              map_panel(pastoralPoly_0, "gpwV4_past",  "Pastoral (GPWv4)",         mv_all))

all_maps <- plot_grid(maps_grid, combined_legend, ncol = 2, rel_widths = c(0.88, 0.12))
ggsave(paste0(resultsdir, "/Plots/pastoral_all_maps_combined.png"), all_maps,
       width = 16, height = 18, dpi = 300, bg = "white")
cat("Saved: pastoral_all_maps_combined.png\n")

# ── 7. Results comparison plot (Figure 4) ────────────────────────────────────
this_study <- data.frame(
  region = c("Africa", "Africa", "West Africa/Sahel", "West Africa/Sahel", "Southern Africa", "Southern Africa",
             "Sahel & HoA", "Sahel & HoA", "Greater HoA", "Greater HoA",
             "Djibouti", "Djibouti", "Eritrea", "Eritrea", "Ethiopia", "Ethiopia",
             "Kenya", "Kenya", "Somalia", "Somalia", "South Sudan", "South Sudan"),
  source = rep(c("This study (WorldPop)", "This study (GPWv4)"), 11),
  population = c(202.3, 253.3, 65.3, 82.7, 23.4, 28.2, 62.0, 73.6, 69.0, 80.4,
                 0.5, 0.5, 2.3, 3.1, 14.7, 19.8, 7.9, 8.5, 8.5, 9.0, 9.3, 9.6)
)

published <- data.frame(
  region = c("Africa", "Africa", "Africa", "Africa", "Africa", "Sahel & HoA",
             "Greater HoA", "Greater HoA", "Djibouti", "Eritrea", "Ethiopia", "Ethiopia",
             "Kenya", "Somalia", "South Sudan"),
  population = c(22.0, 260.0, 250.0, 308.5, 268.0, 58.0, 25.0, 30.0, 0.4, 0.7,
                 14.7, 9.0, 8.8, 10.8, 7.3),
  label = c("Galaty & Bonte, 1982", "Simpkin, 2005", "IIRR & CTA, 2013",
            "Holechek et al., 2017\n(250-367M)", "FAO, 2018", "ECA, 2017",
            "Simpkin, 2005", "Catley et al., 2012", "ECA, 2017", "ECA, 2017",
            "ECA, 2017", "REGLAP, 2009", "Wanyama, 2023",
            "Hassan-Kadle et al., 2024", "ECA, 2017")
)

region_order <- c("Africa", "West Africa/Sahel", "Southern Africa", "Sahel & HoA", "Greater HoA",
                  "Djibouti", "Eritrea", "Ethiopia", "Kenya", "Somalia", "South Sudan")

this_study$region <- factor(this_study$region, levels = region_order)
published$region  <- factor(published$region,  levels = region_order)
this_study$source <- factor(this_study$source, levels = c("This study (WorldPop)", "This study (GPWv4)"))

p_fig4 <- ggplot() +
  geom_col(data = this_study, aes(x = region, y = population, fill = source),
           position = position_dodge(width = 0.7), width = 0.65,
           color = "black", linewidth = 0.2, alpha = 0.85) +
  geom_point(data = published, aes(x = region, y = population, shape = "Published studies"),
             position = position_dodge(width = 0.7),
             size = 3.5, fill = "white", color = "#333333", stroke = 1.2) +
  geom_vline(xintercept = 5.5, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  annotate("text", x = 3,   y = max(this_study$population) * 1.02,
           label = "Continental / Regional", size = 3.5, color = "grey40", fontface = "italic") +
  annotate("text", x = 8.5, y = max(this_study$population) * 1.02,
           label = "Country level", size = 3.5, color = "grey40", fontface = "italic") +
  scale_fill_manual(values = c("This study (WorldPop)" = col_worldpop,
                               "This study (GPWv4)"    = col_gpwv4), name = NULL) +
  scale_shape_manual(values = c("Published studies" = 21), name = NULL) +
  scale_y_continuous(labels = label_comma(suffix = "M", scale = 1),
                     expand = expansion(mult = c(0, 0.08)), limits = c(0, NA)) +
  guides(fill = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  labs(x = NULL, y = "Population (millions)") +
  theme_pastoral() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), plot.margin = margin(5, 20, 70, 20))

save_plot(p_fig4, "results_comparison_plot.png", width = 14, height = 7)
