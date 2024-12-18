# Maps of the Te Arawa Lakes
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
packages <- c("sf","readxl", "readr", "tidyverse", "dplyr", "ggplot2")

# Load packages if not already installed
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)})


# Import the data sets ---------------------------------------------------------
# Import the coastline
NZ_Coastline <- st_read("~/PhD/Pictures_Figures/Maps/Layers/Coastline/Coastline_line_2.gpkg")
NZ_Coastline <- NZ_Coastline %>% select(geom)

# Import the BOPRC lines
BOPRC <- st_read("~/PhD/Pictures_Figures/Maps/Layers/Regional counsils/BOPRC.gpkg")

# Import the Lakes 
Lakes_Te_Arawa <- st_read("~/PhD/Pictures_Figures/Maps/Layers/Lakes/12_lakes.gpkg")
Lakes_BOPRC <- st_read("~/PhD/Pictures_Figures/Maps/Layers/Lakes/Lakes_BOPRC.gpkg")
Lakes_all <- st_read("~/PhD/Pictures_Figures/Maps/Layers/Lakes/Lakes_all.gpkg")

# Import the Rivers 
Rivers <- st_read("~/PhD/Pictures_Figures/Maps/Layers/Rivers/Rivers_Lakes.gpkg")
Rivers_BOPRC <- st_read("~/PhD/Pictures_Figures/Maps/Layers/Rivers/Rivers_BOPRC.gpkg")
Rivers_all <- st_read("~/PhD/Pictures_Figures/Maps/Layers/Rivers/Rivers Te Awara Lakes.gpkg")


# Import the Lakes_info
Lakes_info <- read_excel("~/PhD/Data/Lakes/All_lakes/Lakes_Monitoring.xlsx")

# Remove 2 lakes from the list
names(Lakes_info)
Lakes_info <- Lakes_info %>%
  filter(ID != 13,ID != 14)

# use only Te Arawa lakes
Lakes<- Lakes_Te_Arawa

#Lakes <- Lakes %>%  select(geom, name) 

# Define the new names
new_names <- c("Lake Ōkataina","Lake Ōkaro","Lake Rotokākahi","Lake Tikitapu", "Lake Rotomāhana", "Lake Ōkāreka")
Lakes <- Lakes %>% mutate(name = case_when(name == "Lake Ōkataina / Te Moana i kataina ā Te Rangitakaroro" ~ new_names[1],name == "Lake Okaro" ~ new_names[2],name == "Lake Rotokākahi / Green Lake" ~ new_names[3],name == "Lake Tikitapu (Blue Lake)" ~ new_names[4],name == "Lake Rotomahana" ~ new_names[5],name == "Lake Ōkareka" ~ new_names[6], name=="Lake Taupō / Taupōmoana" ~new_names[7] , TRUE ~ name))

# merge them together
Lakes_DATA <- merge(Lakes, Lakes_info, by = "name", all.x = TRUE)


# Implort the monitoring sites
All_sample_sites <- read_excel("~/PhD/Data/Lakes/All_lakes/Lakes_Monitoring.xlsx", 
                              sheet = "Monitoring_Sites")
# Convert tibble to sf object
All_sample_sites <- All_sample_sites %>%
  filter(!is.na(Lat) & !is.na(Lon)) %>%
  st_as_sf(coords = c("Lon", "Lat"), crs = 4326, remove = FALSE)


# Safe lakes Rotorua, Rotoiti, Rotoehu, Rotomā, Ōkāreka in separate files ------
# Define lake names
lake_names <- c("Lake Rotorua", "Lake Rotoiti", "Lake Rotoehu", "Lake Rotomā", "Lake Ōkāreka")


setwd("~/PhD/Data/3. Natural habitat monitoring/Data_raw/Lakes_perimeter")
# Save each lake to a separate file
for (lake_name in lake_names) {
  lake_data <- Lakes_DATA %>% filter(name == lake_name) %>% st_make_valid()
  st_write(lake_data, paste0(lake_name, ".gpkg"), append=FALSE)}



# plot all sample sites
ggplot() +
  geom_sf(data=Lakes)+
  geom_sf(data=All_sample_sites, aes(fill=Study),shape=21, col="black",size=3 ) +
  theme_bw()




