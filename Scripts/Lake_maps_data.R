# Maps and data from LAWA for habitat monitoring
# By Olivier Raven
# Date: 3/04/24


# Clean and load packages ------------------------------------------------------
cat("\014"); rm(list = ls())#; dev.off()
#sapply(.packages(), unloadNamespace)

# set working directory
setwd("~/PhD/Data/R")

# Define the list of packages
packages <- c("reshape2","gganimate", "sfheaders", "ggrepel","readxl", "tidyverse", "dplyr", "ggplot2", "sf", "ggmap", "patchwork", "stringr", "units")

# Load packages if not already installed
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)})


# order the lakes will be mentioned 
Lake_names <- c("Lake Rotorua", "Lake Rotoiti", "Lake Rotoehu", "Lake Rotomā", "Lake Ōkataina", "Lake Ōkāreka", "Lake Tarawera", "Lake Tikitapu", "Lake Rotokākahi", "Lake Rotomāhana", "Lake Ōkaro", "Lake Rerewhakaaitu", "Lake Taupō")

################################################################################
# Lakes PLots ------------------------------------------------------------------

# Get the coastlines
NZ_Coastline <- st_read("~/PhD/Pictures_Figures/Maps/Layers/Coastline/Coastline_line_2.gpkg")
NZ_Coastline <- NZ_Coastline %>% select(geom)
BOPRC <- st_read("~/PhD/Pictures_Figures/Maps/Layers/Regional counsils/BOPRC.gpkg")

Lakes_info <- read_excel("~/PhD/Data/Lakes/All_lakes/Lakes_Monitoring.xlsx")

Lakes <- st_read("~/PhD/Pictures_Figures/Maps/Layers/Lakes/12_lakes.gpkg")
Lakes_BOPRC <- st_read("~/PhD/Pictures_Figures/Maps/Layers/Lakes/Lakes_BOPRC.gpkg")
Lakes_all <- st_read("~/PhD/Pictures_Figures/Maps/Layers/Lakes/Lakes_all.gpkg")

Rivers <- st_read("~/PhD/Pictures_Figures/Maps/Layers/Rivers/Rivers_Lakes.gpkg")
Rivers_BOPRC <- st_read("~/PhD/Pictures_Figures/Maps/Layers/Rivers/Rivers_BOPRC.gpkg")
Rivers_all <- st_read("~/PhD/Pictures_Figures/Maps/Layers/Rivers/Rivers Te Awara Lakes.gpkg")

Taupo <- st_read("~/PhD/Pictures_Figures/Maps/Layers/Lakes/Taupo_perimeter.gpkg")

names(Lakes_info)
Lakes_info <- Lakes_info %>%
  filter(ID != 13,ID != 14)

# Add missing columns to Taupo dataset
missing_cols <- setdiff(colnames(Lakes), colnames(Taupo))
for (col in missing_cols) {
  Taupo[[col]] <- NA
}

# Combine the datasets using rbind
Lakes_per <- rbind(Lakes, Taupo)

# use only te awara lakes
Lakes_per<- Lakes

Lakes_per <- Lakes_per %>%
  select(geom, name) 

# Define the new names
new_names <- c("Lake Ōkataina","Lake Ōkaro","Lake Rotokākahi","Lake Tikitapu", "Lake Rotomāhana", "Lake Ōkāreka", "Lake Taupō")
Lakes_per <- Lakes_per %>% mutate(name = case_when(name == "Lake Ōkataina / Te Moana i kataina ā Te Rangitakaroro" ~ new_names[1],name == "Lake Okaro" ~ new_names[2],name == "Lake Rotokākahi / Green Lake" ~ new_names[3],name == "Lake Tikitapu (Blue Lake)" ~ new_names[4],name == "Lake Rotomahana" ~ new_names[5],name == "Lake Ōkareka" ~ new_names[6], name=="Lake Taupō / Taupōmoana" ~new_names[7] , TRUE ~ name))

# merge them together
Lakes_DATA <- merge(Lakes_per, Lakes_info, by = "name", all.x = TRUE)



ggplot() +
  geom_sf(data = NZ_Coastline) +
  geom_sf(data = Lakes_DATA) +
  coord_sf() +
  labs(x="Longitude", y="Latitude")+
  theme_bw()

# plot TLI/ State
ggplot() +
  geom_sf(data = Lakes_DATA, aes(fill = State)) +
  geom_sf_text(data = Lakes_DATA, aes(label = name), size = 3, color = "black") +
  coord_sf() +
  labs(x="Longitude", y="Latitude")+
  theme_bw()

# make it animated
# Join the datasets by Lake and name, retaining geometry
TLI_3 <- TLI_2 %>%
  left_join(Lakes_DATA %>% select(name, geometry), by = c("Lake" = "name"))

TLI_3 <- st_as_sf(TLI_3)

TLI_4 <- TLI_3 %>% 
  filter(Year != 2009)%>%
  filter(if_else(Lake == "Lake Rotoiti", Site == "Site 3", TRUE))%>%
  filter(if_else(Lake == "Lake Rotorua", Site == "Site 5", TRUE))


# Define colors for trophic status
trophic_colors <- c("Microtrophic" = "blue", "Oligotrophic" = "green", "Mesotrophic" = "yellow", "Eutrophic" = "orange", "Supertrophic" = "red")

ggplot(TLI_4, aes(fill = Trophic_Status, geometry = geometry)) +
  geom_sf() +
  scale_fill_manual(values = trophic_colors, name = "Trophic Status", breaks = c("Microtrophic", "Oligotrophic", "Mesotrophic", "Eutrophic", "Supertrophic"),drop = FALSE) +
  theme_bw() +
  facet_wrap(~ Year)


# Extract boundary lines from the polygons and retain only necessary columns
Lakes_lines <- Lakes_per %>%
  mutate(geom = st_boundary(geom)) %>%
  select(geom, name)

# Perform the merge based on the lake name
Lake_data <- merge(Lakes_lines, Lakes_info, by = "name", all.x = TRUE)

# Plot the lines
ggplot() +
   geom_sf(data=Rivers_BOPRC)+
  geom_sf(data = Lakes_DATA, fill="lightblue") +
  #facet_wrap(~name, ncol = 4)+
  coord_sf()

################################################################################
# Maps of nzffdms data ---------------------------------------------------------
# By Olivier Raven
# Date: 3/04/24

# Get koura data
setwd("~/PhD/Data")
Koura_data_nzffdms <- read_csv("Koura_data_nzffdms.csv")
Koura_sf <- st_as_sf(Koura_data_nzffdms, coords = c("eastingNZTM", "northingNZTM"), crs = 2193)  # Assuming the data is in NZTM projection (EPSG:2193)

# Get catfish data
Catfish_data_nzffdms <- read_csv("Catfish_data_nzffdms.csv")
Catfish_sf <- st_as_sf(Catfish_data_nzffdms, coords = c("eastingNZTM", "northingNZTM"), crs = 2193)  # Assuming the data is in NZTM projection (EPSG:2193)

# Replace NA values in the totalCount column with 1
Koura_sf <- Koura_sf %>%
  mutate(totalCount = ifelse(is.na(totalCount), 1, totalCount))

# Convert the eventDate column to Date type if it is not already
Koura_sf <- Koura_sf %>%
  mutate(eventDate = as.Date(eventDate, format = "%d/%m/%Y"))

ggplot() +
  geom_sf(data = NZ_Coastline) +
  geom_sf(data = Koura_sf, aes(col = eventDate), shape = 21) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(x = "Longitude", y = "Latitude", color = "Total Count") +
  theme_bw()

ggplot(Koura_sf, aes(x = eventDate, y = totalCount)) +
  geom_line(color = "blue") +  # Line plot
  geom_point(color = "red") +  # Points on the line
  labs(x = "Date", y = "Total Count", title = "Total Count Over Time") +
  theme_minimal()

# Plot the Koura data
ggplot() +
  geom_sf(data = NZ_Coastline) +
  geom_sf(data = Koura_sf, col="black", shape=21, fill="blue") +
  labs(x = "Longitude", y = "Latitude")+
  theme_bw()

# Print bounding box values to determine appropriate limits
lake_bbox <- st_bbox(Lakes)
BOPRC_bbox <- st_bbox(BOPRC)

# Plot with adjusted limits
ggplot() +
  geom_sf(data = NZ_Coastline) +
  geom_sf(data = BOPRC) +
  #geom_sf(data = Rivers_BOPRC) +
  geom_sf(data = Lakes_BOPRC, fill = "lightblue") +
  geom_sf(data = Koura_sf, col = "black", shape = 21, fill = "blue") +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw() +
  coord_sf(xlim = c(BOPRC_bbox["xmin"], BOPRC_bbox["xmax"]), ylim = c(BOPRC_bbox["ymin"], BOPRC_bbox["ymax"]))


# Plot the Catfish data
ggplot() +
  geom_sf(data = NZ_Coastline) +
  geom_sf(data = BOPRC) +
  geom_sf(data = Lake_data) +
  geom_sf(data = Catfish_sf, col="black", shape=21, fill="red") +
  labs(x = "Longitude", y = "Latitude")+
  theme_bw()

# Plot with adjusted limits
ggplot() +
  geom_sf(data = Lake_data) +
  geom_sf(data = Catfish_sf, col="black", shape=21, fill="red") +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw() +
  coord_sf(xlim = c(lake_bbox["xmin"], lake_bbox["xmax"]), ylim = c(lake_bbox["ymin"], lake_bbox["ymax"]))

# Plot with adjusted limits
ggplot() +
  geom_sf(data = NZ_Coastline) +
  geom_sf(data = BOPRC) +
  #geom_sf(data = Rivers_BOPRC) +
  geom_sf(data = Lakes_BOPRC, fill = "lightblue") +
  geom_sf(data = Catfish_sf, col="black", shape=21, fill="red") +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw() +
  coord_sf(xlim = c(BOPRC_bbox["xmin"], BOPRC_bbox["xmax"]), ylim = c(BOPRC_bbox["ymin"], BOPRC_bbox["ymax"]))


################################################################################
# Stratified random sampling sites selection For dominant habitat type ###################################
custom_colors <- c("Muddy" = "tan", "Sandy" = "orange","Rocky" = "gray", "Raupo" = "green", "AR"= "red",  "Cliff"= "black", "Geo"= "blue") 

#unique(Ōkāreka_DHT$DHT)

# Rororua DHT ----------------------------------------------------------------------
Rotorua_DHT <- st_read("~/PhD/Pictures_Figures/Maps/Layers/Dominant_habitat_types/Rotorua/DHT_rotorua.gpkg")

# Calculate centroids with correct column names
Rotorua_DHT_centroids <- st_centroid(Rotorua_DHT)
Rotorua_DHT_centroids <- cbind(Rotorua_DHT_centroids, st_coordinates(Rotorua_DHT_centroids))

ggplot() +
  geom_sf(data = Rotorua_DHT, aes(col = DHT), lwd = 1.5) +
  coord_sf() +
  scale_color_manual(values = custom_colors) +
  labs(x = "Longitude", y = "Latitude") + 
  geom_text_repel(data = Rotorua_DHT_centroids, aes(label = id, x = X, y = Y), size = 3, color = "black", max.overlaps = Inf)+
  xlim(176.23, 176.28) +  
  ylim(-38.15, -38.10)  
  

# Rotoiti DHT ----------------------------------------------------------------------
Rotoiti_DHT <- st_read("~/PhD/Pictures_Figures/Maps/Layers/Dominant_habitat_types/Rotoiti/DHT_RotoitiIII.gpkg")

Rotoiti_DHT_centroids <- st_centroid(Rotoiti_DHT)
Rotoiti_DHT_centroids <- cbind(Rotoiti_DHT_centroids, st_coordinates(Rotoiti_DHT_centroids))

ggplot() +
  geom_sf(data = Rotoiti_DHT, aes(col = DHT), lwd = 1.5) +
  coord_sf() +
  scale_color_manual(values = custom_colors) +
  labs(x = "Longitude", y = "Latitude") +
  geom_text_repel(data = Rotoiti_DHT_centroids, aes(label = id, x = X, y = Y), size = 2, color = "black", max.overlaps = Inf)#+
  #xlim(176.33, 176.40) +  
  #ylim(-38.055, -38.01)

DHT_length_Rotoiti <- Rotoiti_DHT %>%
  group_by(DHT) %>%
  summarize(total_length = sum(st_length(geom)))
total_length_Rotoiti <- sum(DHT_length_Rotoiti$total_length)
DHT_length_Rotoiti
total_length_Rotoiti

#### random points
# Filter out the categories "Cliff" and "Geo"
DHT_length_RotoitiI <- DHT_length_Rotoiti %>% 
  filter(!DHT %in% c("Cliff", "Geo", "AR"))

# Transform to UTM Zone 60S for all geometries
DHT_length_Rotoiti_utm <- st_transform(DHT_length_RotoitiI, crs = 32760)  # UTM Zone 60S

# Function to generate random points for each category
set.seed(0356)
generate_random_points <- function(data, n_points = 3) {
  st_sample(data, size = n_points, type = "random")
}

# Generate random points and transform back to WGS 84
random_points_list <- lapply(split(DHT_length_Rotoiti_utm, DHT_length_Rotoiti_utm$DHT), function(cat_data) {
  points_utm <- generate_random_points(cat_data)
  st_transform(points_utm, crs = st_crs(DHT_length_Rotoiti))
})

# Combine random points into one sf object with DHT attributes
all_random_points <- st_sf(
  DHT = rep(names(random_points_list), sapply(random_points_list, length)),
  geometry = st_sfc(do.call(c, random_points_list), crs = st_crs(DHT_length_RotoitiI)))

# Extract coordinates for the random points
coordinates <- st_coordinates(all_random_points)

# Add point number as labels
Rotoiti_DHT$DHT <- factor(Rotoiti_DHT$DHT, levels = names(custom_colors))
label_df <- data.frame(coordinates)
label_df$label <- seq_len(nrow(label_df))

# plot
ggplot() +
  geom_sf(data = Rotoiti_DHT, aes(col = DHT), lwd = 1.5) +
  #geom_sf(data = all_random_points, color = "black", size = 2, shape = 21, fill = "red") +
  #geom_text_repel(data = label_df, aes(x = X, y = Y, label = label), size = 3) +
  coord_sf() +
  scale_color_manual(values = custom_colors) +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw()






#
# Rotoehu DHT ------------------------------------------------------------------
Rotoehu_DHT <- st_read("~/PhD/Pictures_Figures/Maps/Layers/Dominant_habitat_types/Rotoehu/DHT_RotoehuII.gpkg")

Rotoehu_DHT_centroids <- st_centroid(Rotoehu_DHT)
Rotoehu_DHT_centroids <- cbind(Rotoehu_DHT_centroids, st_coordinates(Rotoehu_DHT_centroids))

ggplot() +
  geom_sf(data = Rotoehu_DHT, aes(col = DHT), lwd = 1.5) +
  coord_sf() +
  scale_color_manual(values = custom_colors) +
  labs(x = "Longitude", y = "Latitude") +
  geom_text_repel(data = Rotoehu_DHT_centroids, aes(label = id, x = X, y = Y), size = 3, color = "black", max.overlaps = Inf)

DHT_length_Rotoehu <- Rotoehu_DHT %>%
  group_by(DHT) %>%
  summarize(total_length = sum(st_length(geom)))
total_length_Rotoehu <- sum(DHT_length_Rotoehu$total_length)
DHT_length_Rotoehu
total_length_Rotoehu


# Rotomā DHT -------------------------------------------------------------------
Rotomā_DHT <- st_read("~/PhD/Pictures_Figures/Maps/Layers/Dominant_habitat_types/Rotomā/DHT_RotomaII.gpkg")

Rotomā_DHT_centroids <- st_centroid(Rotomā_DHT)
Rotomā_DHT_centroids <- cbind(Rotomā_DHT_centroids, st_coordinates(Rotomā_DHT_centroids))

ggplot() +
  geom_sf(data = Rotomā_DHT, aes(col = DHT), lwd = 1.5) +
  coord_sf() +
  scale_color_manual(values = custom_colors) +
  labs(x = "Longitude", y = "Latitude") +
  geom_text_repel(data = Rotomā_DHT_centroids, aes(label = id, x = X, y = Y), size = 3, color = "black", max.overlaps = Inf)

DHT_length_Rotomā <- Rotomā_DHT %>%
  group_by(DHT) %>%
  summarize(total_length = sum(st_length(geom)))
total_length_Rotomā <- sum(DHT_length_Rotomā$total_length)
DHT_length_Rotomā
total_length_Rotomā


# Ōkāreka DHT ------------------------------------------------------------------
Ōkāreka_DHT <- st_read("~/PhD/Pictures_Figures/Maps/Layers/Dominant_habitat_types/Ōkāreka/Okaroka_DHT.gpkg")

Ōkāreka_DHT_centroids <- st_centroid(Ōkāreka_DHT)
Ōkāreka_DHT_centroids <- cbind(Ōkāreka_DHT_centroids, st_coordinates(Ōkāreka_DHT_centroids))

# Extract the geometry of the shoreline
shoreline <- st_union(Ōkāreka_DHT$geom)

ggplot() +
  #geom_sf(data = shoreline, lwd = 2.5) +
  geom_sf(data = Ōkāreka_DHT, aes(col = DHT), lwd = 1.5) +
  coord_sf() +
  scale_color_manual(values = custom_colors) +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw()+
  geom_text_repel(data = Ōkāreka_DHT_centroids, aes(label = id, x = X, y = Y), size = 3, color = "black", max.overlaps = Inf)


DHT_length_Ōkāreka <- Ōkāreka_DHT %>%
  group_by(DHT) %>%
  summarize(total_length = sum(st_length(geom)))
total_length_Ōkāreka <- sum(DHT_length_Ōkāreka$total_length)
DHT_length_Ōkāreka
total_length_Ōkāreka

#### random points
# Transform to UTM Zone 60S for all geometries
DHT_length_Ōkāreka_utm <- st_transform(DHT_length_Ōkāreka, crs = 32760)  # UTM Zone 60S

# Function to generate random points for each category
set.seed(17688)
generate_random_points <- function(data, n_points = 3) {
  st_sample(data, size = n_points, type = "random")
}

# Generate random points and transform back to WGS 84
random_points_list <- lapply(split(DHT_length_Ōkāreka_utm, DHT_length_Ōkāreka_utm$DHT), function(cat_data) {
  points_utm <- generate_random_points(cat_data)
  st_transform(points_utm, crs = st_crs(DHT_length_Ōkāreka))
})

# Combine random points into one sf object with DHT attributes
all_random_points <- st_sf(
  DHT = rep(names(random_points_list), sapply(random_points_list, length)),
  geometry = st_sfc(do.call(c, random_points_list), crs = st_crs(DHT_length_Ōkāreka)))

# Extract coordinates for the random points
coordinates <- st_coordinates(all_random_points)

# Add point number as labels
label_df <- data.frame(coordinates)
label_df$label <- seq_len(nrow(label_df))
Ōkāreka_DHT$DHT <- factor(Ōkāreka_DHT$DHT, levels = names(custom_colors))

# plot
ggplot() +
  geom_sf(data = Ōkāreka_DHT, aes(col = DHT), lwd = 1.5) +
  geom_sf(data = all_random_points, color = "black", size = 2, shape = 21, fill = "red") +
  geom_text_repel(data = label_df, aes(x = X, y = Y, label = label), size = 3) +  # Add labels
  coord_sf() +
  scale_color_manual(values = custom_colors) +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw()


################
# Combine the DHT of the 5 lakes ####
combined_DHT <- rbind(Rotorua_DHT,Rotoiti_DHT,Rotoehu_DHT,Rotomā_DHT,Ōkāreka_DHT)

combined_DHT_centroids <- st_centroid(combined_DHT)
combined_DHT_centroids <- cbind(combined_DHT_centroids, st_coordinates(combined_DHT_centroids))


# Combine all data frames into one
combined_DHT_length <- bind_rows(
  DHT_length_Rotoiti %>% mutate(Lake = "Rotoiti"),
  DHT_length_Rotoehu %>% mutate(Lake = "Rotoehu"),
  DHT_length_Rotomā %>% mutate(Lake = "Rotomā"),
  DHT_length_Ōkāreka %>% mutate(Lake = "Ōkāreka"))

# Select and rename columns
combined_DHT_length <- combined_DHT_length %>%
  select(Lake, DHT, total_length_m = total_length)

# Print the combined data frame
print(combined_DHT_length)

#custom_colors <- c("Sandy" = "orange", "Raupo" = "green", "Rocky" = "brown", "Mud/sand" = "tan", "?" = "gray", "? clif"= "black", "AR"= "red", "Geo"= "blue")

ggplot() +
  #geom_sf(data = Lake_data) +
  geom_sf(data = combined_DHT, aes(col=DHT), lwd = 1.5) +
  scale_color_manual(values = custom_colors) +
  coord_sf() +
  labs(x="Longitude", y="Latitude")+
  theme_bw()
  #geom_text_repel(data = combined_DHT_centroids, aes(label = id, x = X, y = Y), size = 2, color = "black", max.overlaps = Inf)



# Calculate the combined total length
combined_DHT_length <- combined_DHT_length %>%
  group_by(Lake) %>%
  mutate(relative_length = total_length_m / sum(total_length_m)) %>%
  ungroup()

# Plot the total length of each DHT
ggplot(combined_DHT_length, aes(x = DHT, y = relative_length, fill = DHT)) +
  scale_fill_manual(values = custom_colors) +
  geom_bar(stat = "identity", col="black") +
  labs(x = "Dominant habitat type", y = "Total length") +
  facet_wrap(~ Lake)+
  theme_bw() 






################################################################################
# Bathymetry_Rotoiti for modeling #############################################
Bathymetry_Rotoiti_2004 <- read_excel("~/PhD/0. Thesis/1. Reefs Rotoiti/Rotoiti Model/Raw_Bathymetry_Rotoiti.xlsx", sheet = "2004")
Bathymetry_Rotoiti_2006_V1 <- read_excel("~/PhD/0. Thesis/1. Reefs Rotoiti/Rotoiti Model/Raw_Bathymetry_Rotoiti.xlsx", sheet = "2006_V1")
Bathymetry_Rotoiti_2006_V2 <- read_excel("~/PhD/0. Thesis/1. Reefs Rotoiti/Rotoiti Model/Raw_Bathymetry_Rotoiti.xlsx", sheet = "2006_V2")


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

# Save the sf object as a shapefile
#setwd("~/PhD/0. Thesis/1. Reefs Rotoiti/Rotoiti Model")
#st_write(Bathymetry_Rotoiti_sf, "Bathymetry_Rotoiti.shp")

Bat_Rotoiti_sf <- st_read("~/PhD/0. Thesis/1. Reefs Rotoiti/Rotoiti Model/Bathymetry_Rotoiti.gpkg")
Rotoiti_Shape <- st_read("~/PhD/0. Thesis/1. Reefs Rotoiti/Rotoiti Model/Lake Rotoiti shape.gpkg")
TIN <- st_read("~/PhD/0. Thesis/1. Reefs Rotoiti/Rotoiti Model/TIN12_.gpkg")
RasterTIN12 <- read.csv("~/PhD/0. Thesis/1. Reefs Rotoiti/Rotoiti Model/RasterTIN12.csv")

RasterTIN12 <- RasterTIN12 %>%
  select(-Z) %>% 
  rename(Z = VALUE)

# Define grid resolution
grid_resolution <- 12.5

# Determine grid extent
x_min <- floor(min(data$X) / grid_resolution) * grid_resolution
x_max <- ceiling(max(data$X) / grid_resolution) * grid_resolution
y_min <- floor(min(data$Y) / grid_resolution) * grid_resolution
y_max <- ceiling(max(data$Y) / grid_resolution) * grid_resolution

# Create empty grid
x_seq <- seq(x_min, x_max, by = grid_resolution)
y_seq <- seq(y_min, y_max, by = grid_resolution)

# Create an empty matrix for the grid
grid_matrix <- matrix(NA, nrow = length(y_seq), ncol = length(x_seq))
rownames(grid_matrix) <- y_seq
colnames(grid_matrix) <- x_seq

# Populate grid with elevation values
for (i in 1:nrow(data)) {
  x_idx <- which.min(abs(x_seq - data$X[i]))
  y_idx <- which.min(abs(y_seq - data$Y[i]))
  grid_matrix[y_idx, x_idx] <- data$Z[i]
}

grid_df <- as.data.frame(as.table(grid_matrix))

summary(grid_df)
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






################################################################################

# Filter for each lake individually and assign them to separate variables ######
Rotorua <- Lake_data %>% filter(name == "Lake Rotorua")
Rotoiti <- Lake_data %>% filter(name == "Lake Rotoiti")
Rotoehu <- Lake_data %>% filter(name == "Lake Rotoehu")
Rotomā <- Lake_data %>% filter(name == "Lake Rotomā")
Ōkataina <- Lake_data %>% filter(name == "Lake Ōkataina")
Ōkāreka <- Lake_data %>% filter(name == "Lake Ōkāreka")
Tarawera <- Lake_data %>% filter(name == "Lake Tarawera")
Tikitapu <- Lake_data %>% filter(name == "Lake Tikitapu")
Rotokākahi <- Lake_data %>% filter(name == "Lake Rotokākahi")
Rotomāhana <- Lake_data %>% filter(name == "Lake Rotomāhana")
Ōkaro <- Lake_data %>% filter(name == "Lake Ōkaro")
Rerewhakaaitu <- Lake_data %>% filter(name == "Lake Rerewhakaaitu")

################################################################################
#Make 10 random points around lake Rotorua -------------------------------------
# Extract the geometry of Lake Rotorua
Rotorua_geometry <- st_geometry(Rotorua)

# Convert the geometry to a simple feature (sf) object
Rotorua_sf <- st_as_sf(Rotorua, crs = st_crs(4326))

# Generate a random point within Lake Rotorua
set.seed(123)  # Set seed for reproducibility
random_point_Rotorua <- st_sample(Rotorua_sf, size = 1)

# Get coordinates of the random point within Lake Rotorua
random_coords_Rotorua <- st_coordinates(random_point_Rotorua)

# Calculate the length of the shoreline
shoreline_length <- st_length(Rotorua_sf)

# Define the number of additional points to place along the shoreline
num_points <- 10

# Calculate the spacing between the points
spacing <- shoreline_length / (num_points + 1)

# Create a sequence of distances along the shoreline
distances <- seq(from = spacing, to = shoreline_length - spacing, by = spacing)

# Sample points along the shoreline
additional_points <- st_sample(Rotorua_sf, size = num_points, type = "regular", as_points = TRUE)

# Combine all points (including the initial random point)
all_points <- (additional_points)

# Convert the list of points to a simple feature geometry column
all_points_geom <- st_sfc(do.call("c", all_points))

# Create a data frame with the geometry column
all_points_df <- st_sf(geometry = all_points_geom)

# Assign a CRS to the all_points_df object
st_crs(all_points_df) <- st_crs(Rotorua_sf)

# Define the coordinates of the reference point
ref_point <- st_point(c(-38.12967144938912, 176.26411771068518))

# Define the CRS for WGS 84 (EPSG:4326)
crs_wgs84 <- st_crs(4326)

# Set the CRS for the reference point
ref_point <- st_sfc(ref_point, crs = crs_wgs84)

# Project the all_points_df to the WGS 84 coordinate system
all_points_df_projected <- st_transform(all_points_df, crs = crs_wgs84)

# Plot Lake Rotorua with all points
ggplot() +
  geom_sf(data = Rotorua_sf) +
  geom_sf(data = all_points_df, color = "red", size = 3) +
  labs(title = "Lake Rotorua", x="Longitude", y="Latitude")+
  theme_bw()

# Print the transformed coordinates of each point
for (i in 1:length(all_points_df_projected$geometry)) {
  cat("Point", i, "coordinates in the new system:", "\n")
  print(st_coordinates(all_points_df_projected$geometry[[i]]))
}

#Make 10 random points around lake Rotoiti ################################################
# Extract the geometry of Lake Rotoiti
Rotoiti_geometry <- st_geometry(Rotoiti)

# Convert the geometry to a simple feature (sf) object
Rotoiti_sf <- st_as_sf(Rotoiti, crs = st_crs(4326))

# Generate a random point within Lake Rotoiti
set.seed(123)  # Set seed for reproducibility
random_point_Rotoiti <- st_sample(Rotoiti_sf, size = 1)

# Get coordinates of the random point within Lake Rotoiti
random_coords_Rotoiti <- st_coordinates(random_point_Rotoiti)

# Calculate the length of the shoreline
shoreline_length <- st_length(Rotoiti_sf)

# Define the number of additional points to place along the shoreline
num_points <- 10

# Calculate the spacing between the points
spacing <- shoreline_length / (num_points + 1)

# Create a sequence of distances along the shoreline
distances <- seq(from = spacing, to = shoreline_length - spacing, by = spacing)

# Sample points along the shoreline
additional_points <- st_sample(Rotoiti_sf, size = num_points, type = "regular", as_points = TRUE)

# Combine all points (including the initial random point)
all_points <- (additional_points)

# Convert the list of points to a simple feature geometry column
all_points_geom <- st_sfc(do.call("c", all_points))

# Create a data frame with the geometry column
all_points_df <- st_sf(geometry = all_points_geom)

# Assign a CRS to the all_points_df object
st_crs(all_points_df) <- st_crs(Rotoiti_sf)

# Define the coordinates of the reference point
ref_point <- st_point(c(-38.12967144938912, 176.26411771068518))

# Define the CRS for WGS 84 (EPSG:4326)
crs_wgs84 <- st_crs(4326)

# Set the CRS for the reference point
ref_point <- st_sfc(ref_point, crs = crs_wgs84)

# Project the all_points_df to the WGS 84 coordinate system
all_points_df_projected <- st_transform(all_points_df, crs = crs_wgs84)

# Plot Lake Rotoiti with all points
ggplot() +
  geom_sf(data = Rotoiti_sf) +
  geom_sf(data = all_points_df, color = "red", size = 3) +
  labs(title = "Lake Rotoiti")+
  theme_bw()

# Print the transformed coordinates of each point
for (i in 1:length(all_points_df_projected$geometry)) {
  cat("Point", i, "coordinates in the new system:", "\n")
  print(st_coordinates(all_points_df_projected$geometry[[i]]))
}

#Make 10 random points around lake Rotoehu ################################################
# Extract the geometry of Lake Rotoehu
Rotoehu_geometry <- st_geometry(Rotoehu)

# Convert the geometry to a simple feature (sf) object
Rotoehu_sf <- st_as_sf(Rotoehu, crs = st_crs(4326))

# Generate a random point within Lake Rotoehu
set.seed(123)  # Set seed for reproducibility
random_point_Rotoehu <- st_sample(Rotoehu_sf, size = 1)

# Get coordinates of the random point within Lake Rotoehu
random_coords_Rotoehu <- st_coordinates(random_point_Rotoehu)

# Calculate the length of the shoreline
shoreline_length <- st_length(Rotoehu_sf)

# Define the number of additional points to place along the shoreline
num_points <- 10

# Calculate the spacing between the points
spacing <- shoreline_length / (num_points + 1)

# Create a sequence of distances along the shoreline
distances <- seq(from = spacing, to = shoreline_length - spacing, by = spacing)

# Sample points along the shoreline
additional_points <- st_sample(Rotoehu_sf, size = num_points, type = "regular", as_points = TRUE)

# Combine all points (including the initial random point)
all_points <- (additional_points)

# Convert the list of points to a simple feature geometry column
all_points_geom <- st_sfc(do.call("c", all_points))

# Create a data frame with the geometry column
all_points_df <- st_sf(geometry = all_points_geom)

# Assign a CRS to the all_points_df object
st_crs(all_points_df) <- st_crs(Rotoehu_sf)

# Define the coordinates of the reference point
ref_point <- st_point(c(-38.12967144938912, 176.26411771068518))

# Define the CRS for WGS 84 (EPSG:4326)
crs_wgs84 <- st_crs(4326)

# Set the CRS for the reference point
ref_point <- st_sfc(ref_point, crs = crs_wgs84)

# Project the all_points_df to the WGS 84 coordinate system
all_points_df_projected <- st_transform(all_points_df, crs = crs_wgs84)

# Plot Lake Rotoehu with all points
ggplot() +
  geom_sf(data = Rotoehu_sf) +
  geom_sf(data = all_points_df, color = "red", size = 3) +
  theme_bw()

# Print the transformed coordinates of each point
for (i in 1:length(all_points_df_projected$geometry)) {
  cat("Point", i, "coordinates in the new system:", "\n")
  print(st_coordinates(all_points_df_projected$geometry[[i]]))
}

#Make 10 random points around lake Rotomā ################################################
# Extract the geometry of Lake Rotomā
Rotoma_geometry <- st_geometry(Rotomā)

# Convert the geometry to a simple feature (sf) object
Rotomā_sf <- st_as_sf(Rotomā, crs = st_crs(4326))

# Generate a random point within Lake Rotomā
set.seed(123)  # Set seed for reproducibility
random_point_Rotomā <- st_sample(Rotomā_sf, size = 1)

# Get coordinates of the random point within Lake Rotoma
random_coords_Rotomā <- st_coordinates(random_point_Rotomā)

# Calculate the length of the shoreline
shoreline_length <- st_length(Rotomā_sf)

# Define the number of additional points to place along the shoreline
num_points <- 10

# Calculate the spacing between the points
spacing <- shoreline_length / (num_points + 1)

# Create a sequence of distances along the shoreline
distances <- seq(from = spacing, to = shoreline_length - spacing, by = spacing)

# Sample points along the shoreline
additional_points <- st_sample(Rotomā_sf, size = num_points, type = "regular", as_points = TRUE)

# Combine all points (including the initial random point)
all_points <- (additional_points)

# Convert the list of points to a simple feature geometry column
all_points_geom <- st_sfc(do.call("c", all_points))

# Create a data frame with the geometry column
all_points_df <- st_sf(geometry = all_points_geom)

# Assign a CRS to the all_points_df object
st_crs(all_points_df) <- st_crs(Rotomā_sf)

# Define the coordinates of the reference point
ref_point <- st_point(c(-38.12967144938912, 176.26411771068518))

# Define the CRS for WGS 84 (EPSG:4326)
crs_wgs84 <- st_crs(4326)

# Set the CRS for the reference point
ref_point <- st_sfc(ref_point, crs = crs_wgs84)

# Project the all_points_df to the WGS 84 coordinate system
all_points_df_projected <- st_transform(all_points_df, crs = crs_wgs84)

# Plot Lake Rotomā with all points
ggplot() +
  geom_sf(data = Rotomā_sf) +
  geom_sf(data = all_points_df, color = "red", size = 3) +
  theme_bw()

# Print the transformed coordinates of each point
for (i in 1:length(all_points_df_projected$geometry)) {
  cat("Point", i, "coordinates in the new system:", "\n")
  print(st_coordinates(all_points_df_projected$geometry[[i]]))
}

#Make 10 random points around lake Ōkataina ################################################
# Extract the geometry of Lake Ōkataina
Ōkataina_geometry <- st_geometry(Ōkataina)

# Convert the geometry to a simple feature (sf) object
Ōkataina_sf <- st_as_sf(Ōkataina, crs = st_crs(4326))

# Generate a random point within Lake Ōkataina
set.seed(123)  # Set seed for reproducibility
random_point_Ōkataina <- st_sample(Ōkataina_sf, size = 1)

# Get coordinates of the random point within Lake Ōkataina
random_coords_Ōkataina <- st_coordinates(random_point_Ōkataina)

# Calculate the length of the shoreline
shoreline_length <- st_length(Ōkataina_sf)

# Define the number of additional points to place along the shoreline
num_points <- 10

# Calculate the spacing between the points
spacing <- shoreline_length / (num_points + 1)

# Create a sequence of distances along the shoreline
distances <- seq(from = spacing, to = shoreline_length - spacing, by = spacing)

# Sample points along the shoreline
additional_points <- st_sample(Ōkataina_sf, size = num_points, type = "regular", as_points = TRUE)

# Combine all points (including the initial random point)
all_points <- (additional_points)

# Convert the list of points to a simple feature geometry column
all_points_geom <- st_sfc(do.call("c", all_points))

# Create a data frame with the geometry column
all_points_df <- st_sf(geometry = all_points_geom)

# Assign a CRS to the all_points_df object
st_crs(all_points_df) <- st_crs(Ōkataina_sf)

# Define the coordinates of the reference point
ref_point <- st_point(c(-38.12967144938912, 176.26411771068518))

# Define the CRS for WGS 84 (EPSG:4326)
crs_wgs84 <- st_crs(4326)

# Set the CRS for the reference point
ref_point <- st_sfc(ref_point, crs = crs_wgs84)

# Project the all_points_df to the WGS 84 coordinate system
all_points_df_projected <- st_transform(all_points_df, crs = crs_wgs84)

# Plot Lake Ōkataina with all points
ggplot() +
  geom_sf(data = Ōkataina_sf) +
  geom_sf(data = all_points_df, color = "red", size = 3) +
  theme_bw()

# Print the transformed coordinates of each point
for (i in 1:length(all_points_df_projected$geometry)) {
  cat("Point", i, "coordinates in the new system:", "\n")
  print(st_coordinates(all_points_df_projected$geometry[[i]]))
}

#Make 10 random points around lake Ōkāreka ################################################
# Extract the geometry of Lake Ōkāreka
Ōkāreka_geometry <- st_geometry(Ōkāreka)

# Convert the geometry to a simple feature (sf) object
Ōkāreka_sf <- st_as_sf(Ōkāreka, crs = st_crs(4326))

# Generate a random point within Lake Ōkāreka
set.seed(123)  # Set seed for reproducibility
random_point_Ōkāreka <- st_sample(Ōkāreka_sf, size = 1)

# Get coordinates of the random point within Lake Ōkareka
random_coords_Ōkāreka <- st_coordinates(random_point_Ōkāreka)

# Calculate the length of the shoreline
shoreline_length <- st_length(Ōkāreka_sf)

# Define the number of additional points to place along the shoreline
num_points <- 15

# Calculate the spacing between the points
spacing <- shoreline_length / (num_points + 1)

# Create a sequence of distances along the shoreline
distances <- seq(from = spacing, to = shoreline_length - spacing, by = spacing)

# Sample points along the shoreline
additional_points <- st_sample(Ōkāreka_sf, size = num_points, type = "regular", as_points = TRUE)

# Combine all points (including the initial random point)
all_points <- (additional_points)

# Convert the list of points to a simple feature geometry column
all_points_geom <- st_sfc(do.call("c", all_points))

# Create a data frame with the geometry column
all_points_df <- st_sf(geometry = all_points_geom)

# Assign a CRS to the all_points_df object
st_crs(all_points_df) <- st_crs(Ōkāreka_sf)

# Define the coordinates of the reference point
ref_point <- st_point(c(-38.12967144938912, 176.26411771068518))

# Define the CRS for WGS 84 (EPSG:4326)
crs_wgs84 <- st_crs(4326)

# Set the CRS for the reference point
ref_point <- st_sfc(ref_point, crs = crs_wgs84)

# Project the all_points_df to the WGS 84 coordinate system
all_points_df_projected <- st_transform(all_points_df, crs = crs_wgs84)

# Plot Lake Ōkāreka with all points
ggplot() +
  #geom_sf(data = Ōkāreka_DHT, aes(col=DHT), lwd = 1.5) +
  geom_sf(data = Ōkāreka_sf) +
  geom_sf(data = all_points_df, color = "red", size = 3) +
  theme_bw()

# Print the transformed coordinates of each point
for (i in 1:length(all_points_df_projected$geometry)) {
  cat("Point", i, "coordinates in the new system:", "\n")
  print(st_coordinates(all_points_df_projected$geometry[[i]]))
}

#Make 10 random points around lake Tarawera ################################################
# Extract the geometry of Lake Tarawera
Tarawera_geometry <- st_geometry(Tarawera)

# Convert the geometry to a simple feature (sf) object
Tarawera_sf <- st_as_sf(Tarawera, crs = st_crs(4326))

# Generate a random point within Lake Tarawera
set.seed(123)  # Set seed for reproducibility
random_point_Tarawera <- st_sample(Tarawera_sf, size = 1)

# Get coordinates of the random point within Lake Tarawera
random_coords_Tarawera <- st_coordinates(random_point_Tarawera)

# Calculate the length of the shoreline
shoreline_length <- st_length(Tarawera_sf)

# Define the number of additional points to place along the shoreline
num_points <- 10

# Calculate the spacing between the points
spacing <- shoreline_length / (num_points + 1)

# Create a sequence of distances along the shoreline
distances <- seq(from = spacing, to = shoreline_length - spacing, by = spacing)

# Sample points along the shoreline
additional_points <- st_sample(Tarawera_sf, size = num_points, type = "regular", as_points = TRUE)

# Combine all points (including the initial random point)
all_points <- (additional_points)

# Convert the list of points to a simple feature geometry column
all_points_geom <- st_sfc(do.call("c", all_points))

# Create a data frame with the geometry column
all_points_df <- st_sf(geometry = all_points_geom)

# Assign a CRS to the all_points_df object
st_crs(all_points_df) <- st_crs(Tarawera_sf)

# Define the coordinates of the reference point
ref_point <- st_point(c(-38.12967144938912, 176.26411771068518))

# Define the CRS for WGS 84 (EPSG:4326)
crs_wgs84 <- st_crs(4326)

# Set the CRS for the reference point
ref_point <- st_sfc(ref_point, crs = crs_wgs84)

# Project the all_points_df to the WGS 84 coordinate system
all_points_df_projected <- st_transform(all_points_df, crs = crs_wgs84)

# Plot Lake Tarawera with all points
ggplot() +
  geom_sf(data = Tarawera_sf) +
  geom_sf(data = all_points_df, color = "red", size = 3) +
  theme_bw()

# Print the transformed coordinates of each point
for (i in 1:length(all_points_df_projected$geometry)) {
  cat("Point", i, "coordinates in the new system:", "\n")
  print(st_coordinates(all_points_df_projected$geometry[[i]]))
}

#Make 10 random points around lake Tikitapu ################################################
# Extract the geometry of Lake Tikitapu
Tikitapu_geometry <- st_geometry(Tikitapu)

# Convert the geometry to a simple feature (sf) object
Tikitapu_sf <- st_as_sf(Tikitapu, crs = st_crs(4326))

# Generate a random point within Lake Tikitapu
set.seed(123)  # Set seed for reproducibility
random_point_Tikitapu <- st_sample(Tikitapu_sf, size = 1)

# Get coordinates of the random point within Lake Tikitapu
random_coords_Tikitapu <- st_coordinates(random_point_Tikitapu)

# Calculate the length of the shoreline
shoreline_length <- st_length(Tikitapu_sf)

# Define the number of additional points to place along the shoreline
num_points <- 10

# Calculate the spacing between the points
spacing <- shoreline_length / (num_points + 1)

# Create a sequence of distances along the shoreline
distances <- seq(from = spacing, to = shoreline_length - spacing, by = spacing)

# Sample points along the shoreline
additional_points <- st_sample(Tikitapu_sf, size = num_points, type = "regular", as_points = TRUE)

# Combine all points (including the initial random point)
all_points <- (additional_points)

# Convert the list of points to a simple feature geometry column
all_points_geom <- st_sfc(do.call("c", all_points))

# Create a data frame with the geometry column
all_points_df <- st_sf(geometry = all_points_geom)

# Assign a CRS to the all_points_df object
st_crs(all_points_df) <- st_crs(Tikitapu_sf)

# Define the coordinates of the reference point
ref_point <- st_point(c(-38.12967144938912, 176.26411771068518))

# Define the CRS for WGS 84 (EPSG:4326)
crs_wgs84 <- st_crs(4326)

# Set the CRS for the reference point
ref_point <- st_sfc(ref_point, crs = crs_wgs84)

# Project the all_points_df to the WGS 84 coordinate system
all_points_df_projected <- st_transform(all_points_df, crs = crs_wgs84)

# Plot Lake Tikitapu with all points
ggplot() +
  geom_sf(data = Tikitapu_sf) +
  geom_sf(data = all_points_df, color = "red", size = 3) +
  theme_bw()

# Print the transformed coordinates of each point
for (i in 1:length(all_points_df_projected$geometry)) {
  cat("Point", i, "coordinates in the new system:", "\n")
  print(st_coordinates(all_points_df_projected$geometry[[i]]))
}

#Make 10 random points around lake Rotokākahi ################################################
# Extract the geometry of Lake Rotokākahi
Rotokākahi_geometry <- st_geometry(Rotokākahi)

# Convert the geometry to a simple feature (sf) object
Rotokākahi_sf <- st_as_sf(Rotokākahi, crs = st_crs(4326))

# Generate a random point within Lake Rotokākahi
set.seed(123)  # Set seed for reproducibility
random_point_Rotokākahi <- st_sample(Rotokākahi_sf, size = 1)

# Get coordinates of the random point within Lake Rotokākahi
random_coords_Rotokākahi <- st_coordinates(random_point_Rotokākahi)

# Calculate the length of the shoreline
shoreline_length <- st_length(Rotokākahi_sf)

# Define the number of additional points to place along the shoreline
num_points <- 10

# Calculate the spacing between the points
spacing <- shoreline_length / (num_points + 1)

# Create a sequence of distances along the shoreline
distances <- seq(from = spacing, to = shoreline_length - spacing, by = spacing)

# Sample points along the shoreline
additional_points <- st_sample(Rotokākahi_sf, size = num_points, type = "regular", as_points = TRUE)

# Combine all points (including the initial random point)
all_points <- (additional_points)

# Convert the list of points to a simple feature geometry column
all_points_geom <- st_sfc(do.call("c", all_points))

# Create a data frame with the geometry column
all_points_df <- st_sf(geometry = all_points_geom)

# Assign a CRS to the all_points_df object
st_crs(all_points_df) <- st_crs(Rotokākahi_sf)

# Define the coordinates of the reference point
ref_point <- st_point(c(-38.12967144938912, 176.26411771068518))

# Define the CRS for WGS 84 (EPSG:4326)
crs_wgs84 <- st_crs(4326)

# Set the CRS for the reference point
ref_point <- st_sfc(ref_point, crs = crs_wgs84)

# Project the all_points_df to the WGS 84 coordinate system
all_points_df_projected <- st_transform(all_points_df, crs = crs_wgs84)

# Plot Lake Rotokākahi with all points
ggplot() +
  geom_sf(data = Rotokākahi_sf) +
  geom_sf(data = all_points_df, color = "red", size = 3) +
  theme_bw()

# Print the transformed coordinates of each point
for (i in 1:length(all_points_df_projected$geometry)) {
  cat("Point", i, "coordinates in the new system:", "\n")
  print(st_coordinates(all_points_df_projected$geometry[[i]]))
}

#Make 10 random points around lake Rotomāhana ################################################
# Extract the geometry of Lake Rotomāhana
Rotomāhana_geometry <- st_geometry(Rotomāhana)

# Convert the geometry to a simple feature (sf) object
Rotomāhana_sf <- st_as_sf(Rotomāhana, crs = st_crs(4326))

# Generate a random point within Lake Rotomāhana
set.seed(123)  # Set seed for reproducibility
random_point_Rotomāhana <- st_sample(Rotomāhana_sf, size = 1)

# Get coordinates of the random point within Lake Rotomāhana
random_coords_Rotomāhana <- st_coordinates(random_point_Rotomāhana)

# Calculate the length of the shoreline
shoreline_length <- st_length(Rotomāhana_sf)

# Define the number of additional points to place along the shoreline
num_points <- 10

# Calculate the spacing between the points
spacing <- shoreline_length / (num_points + 1)

# Create a sequence of distances along the shoreline
distances <- seq(from = spacing, to = shoreline_length - spacing, by = spacing)

# Sample points along the shoreline
additional_points <- st_sample(Rotomāhana_sf, size = num_points, type = "regular", as_points = TRUE)

# Combine all points (including the initial random point)
all_points <- (additional_points)

# Convert the list of points to a simple feature geometry column
all_points_geom <- st_sfc(do.call("c", all_points))

# Create a data frame with the geometry column
all_points_df <- st_sf(geometry = all_points_geom)

# Assign a CRS to the all_points_df object
st_crs(all_points_df) <- st_crs(Rotomāhana_sf)

# Define the coordinates of the reference point
ref_point <- st_point(c(-38.12967144938912, 176.26411771068518))

# Define the CRS for WGS 84 (EPSG:4326)
crs_wgs84 <- st_crs(4326)

# Set the CRS for the reference point
ref_point <- st_sfc(ref_point, crs = crs_wgs84)

# Project the all_points_df to the WGS 84 coordinate system
all_points_df_projected <- st_transform(all_points_df, crs = crs_wgs84)

# Plot Lake Rotomāhana with all points
ggplot() +
  geom_sf(data = Rotomāhana_sf) +
  geom_sf(data = all_points_df, color = "red", size = 3) +
  theme_bw()

# Print the transformed coordinates of each point
for (i in 1:length(all_points_df_projected$geometry)) {
  cat("Point", i, "coordinates in the new system:", "\n")
  print(st_coordinates(all_points_df_projected$geometry[[i]]))
}
#Make 10 random points around lake Ōkaro ################################################
# Extract the geometry of Lake Ōkaro
Ōkaro_geometry <- st_geometry(Ōkaro)

# Convert the geometry to a simple feature (sf) object
Ōkaro_sf <- st_as_sf(Ōkaro, crs = st_crs(4326))

# Generate a random point within Lake Ōkaro
set.seed(123)  # Set seed for reproducibility
random_point_Ōkaro <- st_sample(Ōkaro_sf, size = 1)

# Get coordinates of the random point within Lake Ōkaro
random_coords_Ōkaro <- st_coordinates(random_point_Ōkaro)

# Calculate the length of the shoreline
shoreline_length <- st_length(Ōkaro_sf)

# Define the number of additional points to place along the shoreline
num_points <- 10

# Calculate the spacing between the points
spacing <- shoreline_length / (num_points + 1)

# Create a sequence of distances along the shoreline
distances <- seq(from = spacing, to = shoreline_length - spacing, by = spacing)

# Sample points along the shoreline
additional_points <- st_sample(Ōkaro_sf, size = num_points, type = "regular", as_points = TRUE)

# Combine all points (including the initial random point)
all_points <- (additional_points)

# Convert the list of points to a simple feature geometry column
all_points_geom <- st_sfc(do.call("c", all_points))

# Create a data frame with the geometry column
all_points_df <- st_sf(geometry = all_points_geom)

# Assign a CRS to the all_points_df object
st_crs(all_points_df) <- st_crs(Ōkaro_sf)

# Define the coordinates of the reference point
ref_point <- st_point(c(-38.12967144938912, 176.26411771068518))

# Define the CRS for WGS 84 (EPSG:4326)
crs_wgs84 <- st_crs(4326)

# Set the CRS for the reference point
ref_point <- st_sfc(ref_point, crs = crs_wgs84)

# Project the all_points_df to the WGS 84 coordinate system
all_points_df_projected <- st_transform(all_points_df, crs = crs_wgs84)

# Plot Lake Ōkaro with all points
ggplot() +
  geom_sf(data = Ōkaro_sf) +
  geom_sf(data = all_points_df, color = "red", size = 3) +
  theme_bw()

# Print the transformed coordinates of each point
for (i in 1:length(all_points_df_projected$geometry)) {
  cat("Point", i, "coordinates in the new system:", "\n")
  print(st_coordinates(all_points_df_projected$geometry[[i]]))
}

#Make 10 random points around lake Rerewhakaaitu ################################################
# Extract the geometry of Lake Rerewhakaaitu
Rerewhakaaitu_geometry <- st_geometry(Rerewhakaaitu)

# Convert the geometry to a simple feature (sf) object
Rerewhakaaitu_sf <- st_as_sf(Rerewhakaaitu, crs = st_crs(4326))

# Generate a random point within Lake Rerewhakaaitu
set.seed(123)  # Set seed for reproducibility
random_point_Rerewhakaaitu <- st_sample(Rerewhakaaitu_sf, size = 1)

# Get coordinates of the random point within Lake Rerewhakaaitu
random_coords_Rerewhakaaitu <- st_coordinates(random_point_Rerewhakaaitu)

# Calculate the length of the shoreline
shoreline_length <- st_length(Rerewhakaaitu_sf)

# Define the number of additional points to place along the shoreline
num_points <- 10

# Calculate the spacing between the points
spacing <- shoreline_length / (num_points + 1)

# Create a sequence of distances along the shoreline
distances <- seq(from = spacing, to = shoreline_length - spacing, by = spacing)

# Sample points along the shoreline
additional_points <- st_sample(Rerewhakaaitu_sf, size = num_points, type = "regular", as_points = TRUE)

# Combine all points (including the initial random point)
all_points <- (additional_points)

# Convert the list of points to a simple feature geometry column
all_points_geom <- st_sfc(do.call("c", all_points))

# Create a data frame with the geometry column
all_points_df <- st_sf(geometry = all_points_geom)

# Assign a CRS to the all_points_df object
st_crs(all_points_df) <- st_crs(Rerewhakaaitu_sf)

# Define the coordinates of the reference point
ref_point <- st_point(c(-38.12967144938912, 176.26411771068518))

# Define the CRS for WGS 84 (EPSG:4326)
crs_wgs84 <- st_crs(4326)

# Set the CRS for the reference point
ref_point <- st_sfc(ref_point, crs = crs_wgs84)

# Project the all_points_df to the WGS 84 coordinate system
all_points_df_projected <- st_transform(all_points_df, crs = crs_wgs84)

# Plot Lake Rerewhakaaitu with all points
ggplot() +
  geom_sf(data = Rerewhakaaitu_sf) +
  geom_sf(data = all_points_df, color = "red", size = 3) +
  theme_bw()

# Print the transformed coordinates of each point
for (i in 1:length(all_points_df_projected$geometry)) {
  cat("Point", i, "coordinates in the new system:", "\n")
  print(st_coordinates(all_points_df_projected$geometry[[i]]))
}
################################################################################



