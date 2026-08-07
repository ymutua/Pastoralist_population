# Shared plot styles and project constants
# Source this file at the top of any analysis or plot script
# Last updated: 2025

library(ggplot2)

base_font <- "sans"
base_size <- 12

# ── Colours ───────────────────────────────────────────────────────────────────
# Population source colours (used across all comparison plots)
col_worldpop <- "#E07B54"
col_gpwv4    <- "#6BAF92"

# Zone colours
col_pastoral     <- "#DEB887"
col_agropastoral <- "#8FBC8F"

# GHOA country colours (consistent across all plots)
ghoa_colours <- c(
  "Djibouti"    = "#E69F00",
  "Eritrea"     = "#56B4E9",
  "Ethiopia"    = "#009E73",
  "Kenya"       = "#F0E442",
  "Somalia"     = "#0072B2",
  "South Sudan" = "#D55E00",
  "Sudan"       = "#CC79A7",
  "Uganda"      = "#999999"
)

# ── GHOA country lists ────────────────────────────────────────────────────────
ghoa_countries  <- c("Djibouti", "Eritrea", "Ethiopia", "Kenya",
                     "Somalia", "South Sudan", "Sudan", "Uganda")

ghoa_iso        <- c("DJI", "ERI", "ETH", "KEN", "SOM", "SSD", "SDN", "UGA")

# ── Regional country lists ────────────────────────────────────────────────────
east_africa_iso    <- c("DJI", "ERI", "ETH", "KEN", "SOM", "SSD", "SDN",
                        "UGA", "TZA", "RWA", "BDI")
west_africa_iso    <- c("BEN", "BFA", "CIV", "CPV", "GHA", "GIN", "GMB",
                        "GNB", "LBR", "MLI", "MRT", "NER", "NGA", "SEN",
                        "SLE", "TGO")
southern_africa_iso <- c("AGO", "BWA", "LSO", "MWI", "MOZ", "NAM", "SWZ",
                         "ZAF", "ZMB", "ZWE")

# ── Base ggplot theme ─────────────────────────────────────────────────────────
theme_pastoral <- function() {
  theme_classic(base_size = base_size, base_family = base_font) %+replace%
    theme(
      axis.text.x        = element_text(size = base_size, family = base_font),
      axis.text.y        = element_text(size = base_size, family = base_font),
      axis.title.x       = element_text(size = base_size, family = base_font),
      axis.title.y       = element_text(size = base_size, family = base_font),
      strip.text         = element_text(size = base_size, family = base_font),
      legend.text        = element_text(size = base_size, family = base_font),
      legend.title       = element_text(size = base_size, family = base_font),
      legend.key.size    = unit(0.5, "cm"),
      legend.position    = "right",
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.4),
      plot.background    = element_rect(fill = "white", color = NA),
      plot.margin        = margin(5, 5, 5, 5)
    )
}

# ── Save helper ───────────────────────────────────────────────────────────────
save_plot <- function(plot, filename, width = 12, height = 7) {
  ggsave(
    filename = file.path(resultsdir, "Plots", filename),
    plot     = plot,
    width    = width,
    height   = height,
    dpi      = 300,
    bg       = "white"
  )
  cat("Saved:", file.path(resultsdir, "Plots", filename), "\n")
}
