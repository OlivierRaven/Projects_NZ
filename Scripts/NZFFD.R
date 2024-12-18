# Title: Data Analysis NZFFD ===================================================
# Author: Olivier Raven
# Date: 23-11-2024
# Description: Script to process, analyze, and visualize data

# Clean and load packages ------------------------------------------------------
cat("\014"); rm(list = ls())#; dev.off()
#sapply(.packages(), unloadNamespace)

#Set working derectory
setwd("~/Documents/R/NZ_Projects")

# Define the list of packages
packages <- c("nzffdr","readr", "readxl", "tidyverse", "dplyr", "ggplot2")

# Load packages if not already installed
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)})


# Import the data sets ---------------------------------------------------------
nzffdms <- read_csv("Data_raw/NZffd/nzffdms.csv")

# Explore the NZFFD data -------------------------------------------------------

# Example 1: Importing data from the NZFFD

# Example 2: Cleaning the imported data
nzffd_clean <- nzffdr_clean(nzffdms)

# Example 3: Adding year, month, and day columns based on "eventDate"
nzffd_clean <- nzffdr_add_dates(nzffd_clean)

# Example 4: Adding taxonomic and threat status information
nzffd_enhanced <- nzffdr_taxon_threat(nzffd_clean)

# Example 5: Converting habitat variables to wide format
nzffd_habitat <- nzffdr_widen_habitat(
  nzffd_enhanced,
  cols_to_expand = c("habitatFlowPercent", "habitatSubstratePercent"))

# Example 6: Visualizing New Zealand map with simple features (sf)
# Requires ggplot2 and sf libraries for mapping
ggplot(data = nzffdr_nzmap) +
    geom_sf(aes(geometry = geometry)) 



# Play some more
names(nzffd_enhanced)
unique(nzffd_enhanced$taxonCommonName)


ggplot(nzffd_enhanced %>%
    filter(taxonCommonName == "Koaro") %>% 
    group_by(year) %>%                    
    summarise(total_count = sum(totalCount, na.rm = TRUE)), 
  aes(x = year, y = total_count)) +
  geom_bar(stat = "identity") 


# Convert nzffd_enhanced into an sf object
fish_data_sf <- st_as_sf(
  nzffd_enhanced,
  coords = c("eastingNZTM", "northingNZTM"), # Coordinates for NZTM
  crs = 2193  # NZTM2000 Coordinate Reference System
)

# Summarize fish abundance and diversity by catchment
fish_summary <- fish_data_sf %>%
  group_by(catchmentNumber) %>%
  summarise(
    total_abundance = sum(totalCount, na.rm = TRUE),
    species_richness = n_distinct(taxonCommonName)) %>% ungroup()

# Plot abundance heatmap
abundance_map <- ggplot() +
  geom_sf(data = fish_summary, aes(color = total_abundance)) +
  scale_color_viridis_c(option = "plasma", na.value = "grey80", name = "Abundance") +
  theme_minimal() +
  labs(
    title = "Fish Abundance by Catchment",
    subtitle = "Heatmap of Total Abundance",
    caption = "Source: NZFFD")

# Plot diversity heatmap
diversity_map <- ggplot() +
  geom_sf(data = fish_summary, aes(col = species_richness)) +
  scale_color_viridis_c(option = "magma", na.value = "grey80", name = "Species Richness") +
  theme_minimal() +
  labs(
    title = "Fish Species Diversity by Catchment",
    subtitle = "Heatmap of Species Richness",
    caption = "Source: NZFFD")

# Highlight significant catchments for biodiversity (e.g., top 10% richest)
significant_catchments <- fish_summary %>%
  mutate(is_significant = species_richness >= quantile(species_richness, 0.9, na.rm = TRUE))

significance_map <- ggplot() +
  geom_sf(data = significant_catchments, aes(col = is_significant)) +
  scale_color_manual(
    values = c("TRUE" = "darkgreen", "FALSE" = "grey80"),
    name = "Significance") +
  theme_minimal() +
  labs(
    title = "Significant Catchments for Biodiversity",
    subtitle = "Top 10% in Species Richness",
    caption = "Source: NZFFD" )

# Display maps
print(abundance_map)
print(diversity_map)
print(significance_map)




# Koura ------------------------------------------------------------------------
nzffd_koura <-nzffd_enhanced %>% filter(taxonCommonName == "Koura")


ggplot(nzffd_koura,aes(eastingNZTM,northingNZTM, col=totalCount)) +
  geom_point()

# Convert to sf object
nzffd_koura <- st_as_sf(nzffd_koura, coords = c("eastingNZTM", "northingNZTM"), crs = 2193) # Replace 2193 with your CRS code

ggplot(nzffd_koura) +
  geom_sf(aes(color = year))

