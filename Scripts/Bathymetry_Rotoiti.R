# Bathymetry_Rotoiti for modeling
# Clean and load packages ------------------------------------------------------
cat("\014"); rm(list = ls())#; dev.off()
#sapply(.packages(), unloadNamespace)

#Set working derectory
setwd("~/PhD/Data/Extra_data_analysis")

# Define the list of packages
packages <- c("ncdf4","sf","readxl", "readr", "tidyverse", "dplyr", "ggplot2")

# Load packages if not already installed
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)})

# Import the data sets ---------------------------------------------------------
# Import data
Bathymetry_Rotoiti_2004 <- read_excel   ("~/PhD/0. Thesis/1. Reefs Rotoiti/Rotoiti Model/Extra/Bathmetry/Raw_Bathymetry_Rotoiti.xlsx", sheet = "2004")
Bathymetry_Rotoiti_2006_V1 <- read_excel("~/PhD/0. Thesis/1. Reefs Rotoiti/Rotoiti Model/Extra/Bathmetry/Raw_Bathymetry_Rotoiti.xlsx", sheet = "2006_V1")
Bathymetry_Rotoiti_2006_V2 <- read_excel("~/PhD/0. Thesis/1. Reefs Rotoiti/Rotoiti Model/Extra/Bathmetry/Raw_Bathymetry_Rotoiti.xlsx", sheet = "2006_V2")


# Standardize column names for Bathymetry_Rotoiti_2006_V2
Bathymetry_Rotoiti_2006_V2 <- Bathymetry_Rotoiti_2006_V2 %>%
  rename(Easting = X, Northing = y, Elevation = z)

# Combine all data frames with a source column
Bathymetry_Rotoiti <- bind_rows(
  Bathymetry_Rotoiti_2006_V1 %>% mutate(Source = "Bathymetry_Rotoiti_2006_V1"),
  Bathymetry_Rotoiti_2006_V2 %>% mutate(Source = "Bathymetry_Rotoiti_2006_V2"),
  Bathymetry_Rotoiti_2004 %>% mutate(Source = "Bathymetry_Rotoiti_2004"))%>%
  distinct(Easting, Northing, Elevation, .keep_all = TRUE) %>%
  group_by(Easting, Northing) %>% 
  summarize(Elevation = mean(Elevation), Source = first(Source)) %>% 
  select(Easting, Northing, Elevation, Source)

# Create an sf object from the data frame
Bathymetry_Rotoiti_sf <- st_as_sf(Bathymetry_Rotoiti, coords = c("Easting", "Northing"), crs = 27200) # 27200 is the EPSG code for NZMG (New Zealand Map Grid)

library(writexl)
# Save the sf object as a shapefile
setwd("~/PhD/Data/Lakes/Lakes_waterquality/Data_raw")
write_xlsx(Bathymetry_Rotoiti, "Bathymetry_Rotoiti.xlsx")
st_write(Bathymetry_Rotoiti_sf, "Bathymetry_Rotoiti.shp")

# Summarise the data -----------------------------------------------------------
Bat_Rotoiti_sf <- st_read("~/PhD/0. Thesis/1. Reefs Rotoiti/Rotoiti Model/Extra/Bathmetry/Bathymetry_Rotoiti.gpkg")
Rotoiti_Shape <- st_read("~/PhD/0. Thesis/1. Reefs Rotoiti/Rotoiti Model/Bathmetry/Lake Rotoiti shape.gpkg")
TIN <- st_read("~/PhD/0. Thesis/1. Reefs Rotoiti/Rotoiti Model/Bathmetry/TIN12_.gpkg")
RasterTIN12 <- read.csv("~/PhD/0. Thesis/1. Reefs Rotoiti/Rotoiti Model/Extra/Bathmetry/RasterTIN12.csv")
Raster25 <- read_csv("~/PhD/0. Thesis/1. Reefs Rotoiti/Rotoiti Model/Extra/Bathmetry/Raster25.csv")
Raster50 <- read_csv("~/PhD/0. Thesis/1. Reefs Rotoiti/Rotoiti Model/Extra/Bathmetry/Raster50.csv")


RasterTIN12 <- RasterTIN12 %>%
  select(-Z) %>% 
  rename(Z = VALUE)

Raster50 <- Raster50 %>%
  mutate(
    X = (left + right) / 2,  # X centroid
    Y = (top + bottom) / 2,  # Y centroid
    Z = SAMPLE_1             # Z value
  ) %>%
  filter(!is.na(Z)) %>% 
  select(X, Y, Z) # Select only the X, Y, and Z columns

ggplot(RasterTIN12, aes(x = X, y = Y, color = VALUE)) +
  geom_point()

ggplot(Raster25, aes(x = X, y = Y, color = Z)) +
  geom_point() 

ggplot(Raster50, aes(x = X, y = Y, color = Z)) +
  geom_point()







# Define grid resolution
grid_resolution <- 50

# Determine grid extent
x_min <- floor(min(Raster50$X) / grid_resolution) * grid_resolution
x_max <- ceiling(max(Raster50$X) / grid_resolution) * grid_resolution
y_min <- floor(min(Raster50$Y) / grid_resolution) * grid_resolution
y_max <- ceiling(max(Raster50$Y) / grid_resolution) * grid_resolution

# Create empty grid
x_seq <- seq(x_min, x_max, by = grid_resolution)
y_seq <- seq(y_min, y_max, by = grid_resolution)

# Create an empty matrix for the grid
grid_matrix <- matrix(NA, nrow = length(y_seq), ncol = length(x_seq))
rownames(grid_matrix) <- y_seq
colnames(grid_matrix) <- x_seq

# Populate grid with elevation values
for (i in 1:nrow(Raster50)) {
  x_idx <- which.min(abs(x_seq - Raster50$X[i]))
  y_idx <- which.min(abs(y_seq - Raster50$Y[i]))
  grid_matrix[y_idx, x_idx] <- Raster50$Z[i]
}

grid_matrix[is.na(grid_matrix)] <- -9999


# Export the matrix to a text file with tab separators
write.table(grid_matrix, file = "~/PhD/0. Thesis/1. Reefs Rotoiti/Rotoiti Model/Extra/Raster50_grid.txt", sep = "\t", row.names = TRUE, col.names = TRUE, quote = FALSE)


# Export the data frame to a text file
write.table(grid_matrix, file = "Raster25.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

# Export the data frame to a text file
write.table(grid_df, file = "grid_df.txt", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)



grid_df <- as.data.frame(as.table(grid_matrix))

na_count <- sum(is.na(grid_df))

summary(grid_matrix)
grid_df <- grid_df %>%
  rename(X = Var2,Y = Var1, Z = Freq)

ggplot(grid_df, aes(X, Y,  col=Z)) +
  geom_point()



# Define the color ramp
color_ramp <- c("purple", "blue", "green","yellow" ,"red")


# Plot theBathymetry_Rotoiti
ggplot() +
  #geom_sf(data = Rotoiti_Shape) +
  geom_sf(data = TIN, aes(color = VALUE)) +
  scale_color_gradientn(colors = color_ramp) +
  #geom_sf(data = Bat_Rotoiti_sf, aes(color = Elevation)) +
  theme_bw()

