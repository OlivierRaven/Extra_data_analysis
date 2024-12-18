# Work with the NZFFD
# Explanation of this script ------------------------------------------------

# 
# 
#
#


# Clean and load packages ------------------------------------------------------
cat("\014"); rm(list = ls())#; dev.off()
#sapply(.packages(), unloadNamespace)

#Set working derectory
setwd("~/PhD/Data/Extra_data_analysis")

# Define the list of packages
packages <- c("nzffdr", "readr", "tidyverse", "dplyr", "ggplot2")

# Load packages if not already installed
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)})


# Maps of NZFFD data for koura and catfish -----------------------------------
# Date: 3/04/24

# Get the coastlines
NZ_Coastline <- st_read("~/PhD/Pictures_Figures/Maps/Layers/Coastline/Coastline_line_2.gpkg")
NZ_Coastline <- NZ_Coastline %>% select(geom)

# Get koura data
Koura_data_nzffdms <- read_csv("PhD/Data/Koura_data_nzffdms.csv")
Koura_sf <- st_as_sf(Koura_data_nzffdms, coords = c("eastingNZTM", "northingNZTM"), crs = 2193)  # Assuming the data is in NZTM projection (EPSG:2193)

# Get catfish data
Catfish_data_nzffdms <- read_csv("PhD/Data/Catfish_data_nzffdms.csv")
Catfish_sf <- st_as_sf(Catfish_data_nzffdms, coords = c("eastingNZTM", "northingNZTM"), crs = 2193)  # Assuming the data is in NZTM projection (EPSG:2193)


# Plot the Koura data
ggplot() +
  geom_sf(data = NZ_Coastline) +
  geom_sf(data = Koura_sf, col="black", shape=21, fill="blue") +
  labs(x = "Longitude", y = "Latitude")+
  theme_bw()

# Plot the Catfish data
ggplot() +
  geom_sf(data = NZ_Coastline) +
  geom_sf(data = Catfish_sf, col="black", shape=21, fill="red") +
  labs(x = "Longitude", y = "Latitude")+
  theme_bw()


# Import the Data sets ---------------------------------------------------------
nzffdr_data <- nzffdr_import()

# Load data set Sample NZFFD data
data("nzffdr_data", package = "nzffdr")

# Load data set Simple features map of New Zealand
data("nzffdr_nzmap", package = "nzffdr")


nzffdr_get_table(x = c("taxon"))

nzffdr_import(
  institution = "",
  catchment_num = "",
  catchment_name = "",
  water_body = "",
  fish_method = "",
  taxon = "Koura Paranephrops", "Catfish Ameiurus nebulosus",
  starts = "",
  ends = "",
  download_format = "all"
)





# Summarise the data -----------------------------------------------------------
# View the structure of the data
str(nzffdr_data)

# View summary statistics
summary(nzffdr_data)

# View the first few rows
head(nzffdr_data)



# Transform the data -----------------------------------------------------------

# Add dates
nzffdr_data1 <- nzffdr_add_dates(nzffdr_data)

# Clean the data
nzffdr_data1 <- nzffdr_clean(nzffdr_data1)



