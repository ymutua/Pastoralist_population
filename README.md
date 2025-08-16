# Estimating pastoralist and agro-pastoralist populations across Africa using geospatial data.

## Overview

This project identifies pastoral zones by combining aridity, livestock production systems, livestock density, and population constraints. It separates agro-pastoral (mixed farming) from pastoral (livestock-only) areas and estimates populations in each zone using WorldPop and GPW v4 datasets.

## Workflow

1. **Data preparation** (scripts 0a-0f): Process boundaries, population, aridity, livestock, and land cover data
2. **Zone delineation** (0g): Create pastoral zone classifications using multi-criteria approach
3. **Population extraction** (1a): Extract populations by zone, save statistics and shapefiles
4. **Visualization** (2a): Generate comparative choropleth maps

## Key Outputs

- **Rasters**: Pastoral zone classifications and population distributions
- **Statistics**: Combined CSV files with agro-pastoral and pastoral populations by admin levels
- **Shapefiles**: Spatial data for mapping with preserved attribute names
- **Maps**: High-resolution side-by-side comparison plots (PNG, 300 DPI)

## Usage

1. Configure paths in `config.R`
2. Run scripts sequentially: `0aAOI.R` → `0bPrepareHumanPopulation.R` → ... → `1aCalculatePopulation.R` → `2aGeneratePlots.R`
3. Results saved to `Results/` directory

Last updated: 16/8/2025