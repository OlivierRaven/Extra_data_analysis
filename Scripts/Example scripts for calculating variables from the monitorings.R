# Example scripts for calculating variables from the monitorings
# Shear stress based on (Van Leeuwen et al., 2023) -----------------------------
###Required Data:
###Wind Data:
#  Mean daily wind speeds
#  Wind directions
###Geospatial Data:
#  Shapefile of the study location (lake boundaries)
###Lake Characteristics:
#  Mean water depth of the lake

library(rgdal)
library(waver)
library(dplyr)

# Load geospatial data
lake_shapefile <- "path/to/your/lake_shapefile.shp"
lake_data <- readOGR(lake_shapefile)

# Define sampling locations
sampling_locations <- data.frame(
  long = c(...),  # Replace with longitudes
  lat = c(...)    # Replace with latitudes
)

# Calculate fetch lengths
fetch_lengths <- fetch_len_multi(lake_data, sampling_locations)

# Load wind data
wind_data <- read.csv("path/to/your/wind_data.csv")

# Calculate wave energy flux
mean_water_depth <- 4
wave_energy_flux <- wave_energy(wind_data, fetch_lengths, mean_water_depth)

# Log transform and aggregate wave energy flux
wave_energy_flux$log_energy <- log(wave_energy_flux$energy)
mean_log_energy <- wave_energy_flux %>%
  group_by(location) %>%
  summarize(mean_log_energy = mean(log_energy, na.rm = TRUE))

# Print the results
print(mean_log_energy)



# Slope ------------------------------------------------------------------------

#Required Data:
#  Digital Elevation Model (DEM):
#  A raster dataset that represents the elevation of the terrain.
# Shapefile of Shoreline:
#  A polygon or line shapefile representing the shoreline of the lake.

library(raster)
library(rgdal)
library(sf)
library(rgeos)

