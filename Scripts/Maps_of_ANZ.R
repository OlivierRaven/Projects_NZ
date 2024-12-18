# Title: Making maps of ANZ ====================================================
# Author: Olivier Raven
# Date: 2024-11-23
# Description: Script to process, analyze, and visualize data

# Import the data sets ---------------------------------------------------------
Coastline <- st_read("Data_raw/Maps/nz-coastlines-topo-150k/nz-coastlines-topo-150k.shp")

Lakes <- st_read("Data_raw/Maps/nz-lake-polygons-topo-150k/nz-lake-polygons-topo-150k.shp")



# Explore the data sets --------------------------------------------------------

# Transform the CRS to WGS84 (EPSG:4326)
Lakes_geo <- st_transform(Lakes, crs = 4326)
lakes_points <- st_centroid(Lakes_geo)


ggplot() +
  geom_sf(data = lakes_points, aes(col=elevation))
  geom_sf(data=Lakes, fill = "blue")+
  geom_sf(data=Coastline, color = "black")



