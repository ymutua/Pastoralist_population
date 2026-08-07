# Gridded dataset of pastoralist and agro-pastoralist population distribution across Africa

## Overview

This project estimates the population living in pastoral and agro-pastoral zones across Africa by combining aridity, livestock production systems, livestock density, and built-up area constraints. Population totals are derived from two independent gridded datasets (WorldPop and GPWv4) to bracket a plausible range of estimates.

## Workflow

1. **Data preparation** (`01` to `06`): Process boundaries, population, aridity, livestock systems, livestock density, and land cover
2. **Zone delineation** (`07`): Delineate pastoral and agro-pastoral zones using a multi-criteria approach
3. **Population extraction** (`08`): Extract population totals by zone and administrative level
4. **Summarise** (`09`): Regional summaries; fetch HDI (UNDP 2022) and GDP per capita (World Bank 2023)
5. **Validation** (`10`): Compare against FEWSNET livelihood zones; subnational WorldPop vs GPWv4 comparison
6. **Outputs** (`11`): Generate all manuscript figures

## Input datasets

- Administrative boundaries (GADM)
- Global Aridity Index v3
- Livestock production systems (FAO)
- WorldPop unconstrained population (100 m)
- Gridded Population of the World v4 / GPWv4 (1 km)
- Copernicus Global Land Cover (100 m)
- FAO GLW4 livestock density (spatial mask)

## Socioeconomic indicators

- **HDI**: UNDP Human Development Index 2022, downloaded from https://hdr.undp.org
- **GDP per capita**: World Bank World Development Indicators, indicator `NY.GDP.PCAP.CD`, 2023, accessed via the `wbstats` R package

## Key outputs

- `pastoralist_otherStats.csv`: Country-level pastoralist population share, HDI, and GDP
- `subnational_comparison_by_zone.csv`: Admin-1 WorldPop vs GPWv4 differences by zone type (GHOA)
- `results_comparison_plot.png`: Figure 4, comparison against published estimates
- `pastoral_all_maps_combined.png`: Figure 3, combined choropleth maps

## Configuration

Set the `root` path in `config.R` before running:

```r
root <- "/path/to/Pastoralist_population"
```

All other paths are derived from `root` automatically.

## Requirements

R packages: `terra`, `sf`, `dplyr`, `tidyr`, `ggplot2`, `stringr`, `scales`, `patchwork`,
`ggspatial`, `cowplot`, `exactextractr`, `tidyterra`, `viridis`, `wbstats`, `geodata`
