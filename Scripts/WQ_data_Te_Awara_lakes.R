# Title: Water quality data of the lakes from different sources combined 
# By:    Olivier Raven
# Date:  18/04/24


# Clean and load packages ------------------------------------------------------
cat("\014"); rm(list = ls())#; dev.off()
#sapply(.packages(), unloadNamespace)

# set working directory
setwd("~/PhD/Data")

# Define the list of packages
packages <- c("readxl", "tidyverse", "dplyr", "ggplot2", "sf", "ggmap", "patchwork", "stringr", "units")

# Load packages if not already installed
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)})


# order the lakes and formats for variables #####
Lake_names <- c("Lake Rotorua", "Lake Rotoiti", "Lake Rotoehu", "Lake Rotomā", "Lake Ōkataina", "Lake Ōkāreka", "Lake Tarawera", "Lake Tikitapu", "Lake Rotokākahi", "Lake Rotomāhana", "Lake Ōkaro", "Lake Rerewhakaaitu", "Lake Taupō")

# temperature
## ylab(expression('Temperature ('*degree*C*')')) +

# light
## ylab(expression('Light ('*mu*mol~s^-1~m^-2*')')) +

# Ambient conductivity
## ylab(expression('Conductivity ('*mu*S~cm^-1*')')) +

# Specific conductivity
## ylab(expression('Conductivity ('*mu*S["20"*degree*"C"]~cm^-1*')')) +

# pH
## ylab(expression('Conductivity ('*mu*S["20"*degree*"C"]~cm^-1*')')) +

# Dissolved oxygen concentrations
## ylab(expression("O"[2]*~'(mg'*~L^-1*')')) +

# Dissolved oxygen saturation
## ylab(expression("O"[2]*~'(%)')) +

# Total invertebrate densities
## ylab(expression('Total density ('*m^-2*')')) +

################################################################################
# All_WQ data frame combination of all the available water quality --------------
setwd("~/PhD/Data")
All_WQ <- read_excel("Lakes/All_lakes/All_WQ.xlsx")
WQ<- All_WQ

# Convert Date to Date format & Extract months and years
WQ$Date <- as.Date(WQ$Date , format = "%d/%m/%Y")
WQ$Month <- month(WQ$Date, label = TRUE)
WQ$Year <- year(WQ$Date)

names(WQ)
unique(WQ$Parameter)

# rename Lakes
WQ <- WQ %>%
  mutate(Lake = case_when(
    Lake == "Lake Rotorua"        ~ "Lake Rotorua",
    Lake == "Lake Rotoiti"        ~ "Lake Rotoiti",
    Lake == "Lake Rotoehu"        ~ "Lake Rotoehu",
    Lake == "Lake Rotoma"         ~ "Lake Rotomā",
    Lake == "Lake Okataina"       ~ "Lake Ōkataina",
    Lake == "Lake Okareka"        ~ "Lake Ōkāreka",
    Lake == "Lake Tikitapu"       ~ "Lake Tikitapu",
    Lake == "Lake Tarawera"       ~ "Lake Tarawera",
    Lake == "Lake Rotokakahi"     ~ "Lake Rotokākahi",
    Lake == "Lake Rotomahana"     ~ "Lake Rotomāhana",
    Lake == "Lake Okaro"          ~ "Lake Ōkaro",
    Lake == "Lake Rerewhakaaitu"  ~ "Lake Rerewhakaaitu",
    TRUE                          ~ Lake))

# rename Site
WQ <- WQ %>%
  mutate(Site = case_when(Site == "Okawa Bay"  ~ "Ōkawa Bay",TRUE~ Site))

# Rename Parameters
WQ <- WQ %>%
  mutate(
    Value = case_when(
      Parameter == "TP (g/m3)" ~ Value * 1000,
      Parameter == "TN (g/m3)" ~ Value * 1000,
      Parameter == "NH4N (mg/L)"  ~ Value * 1000,
      TRUE ~ Value
    ),
    Parameter = case_when(
      Parameter == "NNN (mg/m3)"       ~ "NNN (mg/m³)",
      Parameter == "TP (mg/m3)"        ~ "TP (mg/m³)",
      Parameter == "Chla (mg/m3)"      ~ "Chla (mg/m³)",
      Parameter == "NH4-N (mg/m3)"     ~ "NH4-N (mg/m³)",
      Parameter == "pH"                ~ "pH",
      Parameter == "TN (mg/m3)"        ~ "TN (mg/m³)",
      Parameter == "Turb (NTU)"        ~ "Turb (NTU)",
      Parameter == "DRP (mg/m3)"       ~ "DRP (mg/m³)",
      Parameter == "SecchiDepth (m)"   ~ "Secchi (m)",
      Parameter == "Secchi (m)"        ~ "Secchi (m)",
      Parameter == "pH (-log[H+])"     ~ "pH",
      Parameter == "NH4N (mg/L)"       ~ "NH4-N (mg/m³)",  # After conversion
      Parameter == "ECOLI (#/100 mL)"  ~ "E. coli (#/100 mL)",
      Parameter == "CHLA (mg/m3)"      ~ "Chla (mg/m³)",
      Parameter == "TSS (g/m3)"        ~ "TSS (g/m³)",
      Parameter == "VC - SD (m)"       ~ "Secchi (m)",
      Parameter == "TP (g/m3)"         ~ "TP (mg/m³)",  # After conversion
      Parameter == "Depth (m)"         ~ "Depth (m)",
      Parameter == "TN (g/m3)"         ~ "TN (mg/m³)",  # After conversion
      Parameter == "Turb (NTU)"        ~ "Turb (NTU)",
      Parameter == "Turbidity (NTU)"   ~ "Turb (NTU)",
      TRUE                             ~ Parameter))


# Remove duplicates based on specified columns
WQ_II <- WQ %>%
  distinct(Lake, Site, Date, Parameter, Value, .keep_all = TRUE)


# Define the desired order of lakes
Lake_names <- c("Lake Rotorua", "Lake Rotoiti", "Lake Rotoehu", "Lake Rotomā", "Lake Ōkataina", "Lake Ōkāreka", "Lake Tarawera", "Lake Tikitapu", "Lake Rotokākahi", "Lake Rotomāhana", "Lake Ōkaro", "Lake Rerewhakaaitu", "Lake Taupō")

plot(Value~Date, data = WQ_II)

# Create the plot
ggplot(WQ_II, aes(Date, Value, col=Lake, lty=Data_set)) +
  geom_point()+
  #geom_line(lwd = 1) +
  facet_grid(Parameter ~ Lake , scales = "free_y") +
  scale_linetype_manual(values = c("WQ_master_dataframe_2021" = "dashed", "lawa-lakewq-monitoring-data_2004-2022_statetrend-results_sep2023" = "solid","WQ_Okaro"="dotted" )) +
  theme_bw() +
  theme(strip.text.x = element_text(angle = 0, hjust = 0.5))

ggplot(WQ_II %>% filter(Lake %in% c("Lake Ōkaro")),#Sample %in% c("Integrated")),
       aes(Date, Value, col=Sample, lty=Data_set)) +
  geom_line(lwd = 1) +
  facet_grid(Parameter ~ Lake +Sample, scales = "free_y") +
  scale_linetype_manual(values = c("WQ_master_dataframe_2021" = "dashed", "lawa-lakewq-monitoring-data_2004-2022_statetrend-results_sep2023" = "solid","WQ_Okaro"="dotted" )) +
  theme_bw() +
  theme(strip.text.x = element_text(angle = 0, hjust = 0.5))


# Lake Rotoiti -------------------------------------------------------------------
Rotoiti <- WQ_II %>% filter(Lake == "Lake Rotoiti")
unique(Rotoiti$Parameter)

ggplot(Rotoiti %>% filter(Sample== "Surface" ,Parameter %in% c("pH","NH4-N (mg/m³)" , "Secchi (m)", "Chla (mg/m³)", "TP (mg/m³)", "TN (mg/m³)", "Turb (NTU)" , "TSS (g/m³)" )), aes(Date, Value, col = Parameter, lty=Sample)) +
  geom_line(lwd = 1) +
  facet_grid(Parameter ~ Lake , scales = "free_y") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linetype = "dashed", size = 1.5, col="red") +
  theme_bw() +
  theme(strip.text.x = element_text(angle = 0, hjust = 0.5))


ggplot(Rotoiti %>% filter(Site %in% c("3","4" ), Parameter %in% c("Secchi (m)","TP (mg/m³)","pH", "NH4-N (mg/m³)", "Chla (mg/m³)", "TN (mg/m³)", "Turb (NTU)", "NNN (mg/m³)",  "DRP (mg/m³)" ))
       , aes(x = Date, y = Value, color = as.factor(Sample))) +  # Use color for different samples
  geom_point(size = 2, alpha = 0.6) +  # Enhanced point aesthetics
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linetype = "dashed", size = 1.5, col="red") +  # Improved line aesthetics
  facet_wrap(~ Site+Parameter, scales = "free_y") +  # Facet by Parameter with free y-scales and custom y-axis labels
  theme_bw(base_size = 15) +  # Base theme with larger text size for readability
  labs(
    title = " Lake Rotoiti Water Quality Parameters",
    x = "Date",
    color = "Sample",  # Legend title for the colors
    caption = "Source: BOPRC Rotoiti Data") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Centering and bolding the title
    axis.title.x = element_text(face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(face = "bold"),  # Bold y-axis title
    strip.text = element_text(face = "bold", size = 14),  # Bold and larger facet labels
    panel.grid.major = element_line(color = "grey80"),  # Lighter major grid lines
    panel.grid.minor = element_blank()  # Removing minor grid lines for cleaner look
  )


# Lake Ōkaro -------------------------------------------------------------------
Okaro <- WQ_II %>% filter(Lake == "Lake Ōkaro")

Dosings_Okaro <- read_excel("Lakes/Ōkaro/Waterquality_Okaro.xlsx", sheet = "Dosings")

# Convert Date to Date format & Extract months and years
Dosings_Okaro$Date <- as.Date(Dosings_Okaro$Date , format = "%d/%m/%Y")

unique(Okaro$Parameter)

# Calculate the mean and SD for each parameter
summary_stats <- Okaro %>%
  filter(year(Date) > 2018,Parameter %in% c("Chla (mg/m³)", "TP (mg/m³)"))%>%
  group_by(Parameter, Year, Sample) %>%
  summarise(
    mean_value = mean(Value, na.rm = TRUE),
    sd_value = sd(Value, na.rm = TRUE))

# View the summary statistics
print(summary_stats)


# Create the plot
ggplot(Okaro , aes(Date, Value)) +
  geom_point(col="black", shape=21, size =3, aes (fill=Sample))+
  geom_line(lwd = 1, aes(col=Sample)) +
  facet_grid(Parameter ~ Lake , scales = "free_y") +
  #geom_vline(data = Dosings_Okaro, aes(xintercept = as.Date(Date)), linetype = "longdash", color = "red") +
  theme_bw() +
  theme(strip.text.x = element_text(angle = 0, hjust = 0.5))

ggplot(Okaro %>% filter(year(Date) > 2003,Sample== "Surface" ,Parameter %in% c("pH","NH4-N (mg/m³)" , "Secchi (m)", "Chla (mg/m³)", "TP (mg/m³)", "TN (mg/m³)", "Turb (NTU)" , "TSS (g/m³)" )), aes(Date, Value, col = Parameter, lty=Sample)) +
  geom_line(lwd = 1) +
  facet_grid(Parameter ~ Lake , scales = "free_y") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linetype = "dashed", size = 1.5, col="red") +  # Improved line aesthetics
  geom_vline(data = Dosings_Okaro, aes(xintercept = as.Date(Date)), linetype = "longdash", color = "red") +
  theme_bw() +
  theme(strip.text.x = element_text(angle = 0, hjust = 0.5))

ggplot(Okaro %>% filter(year(Date) > 2018,Parameter %in% c("pH","NH4-N (mg/m³)" , "Secchi (m)", "Chla (mg/m³)", "TP (mg/m³)", "TN (mg/m³)", "Turb (NTU)" , "TSS (g/m³)" ))
                        , aes(Date, Value, col = Sample)) +
         geom_line(lwd = 1) +
         facet_grid(Parameter ~ Lake +Sample , scales = "free_y") +
         geom_vline(data = Dosings_Okaro, aes(xintercept = as.Date(Date)), linetype = "longdash", color = "red") +
         theme_bw() +
         theme(strip.text.x = element_text(angle = 0, hjust = 0.5))

ggplot(Okaro %>% filter(Parameter %in% c("Secchi (m)")) #, "Chla (mg/m³)", "TP (mg/m³)"
       , aes(Date, Value)) +
  geom_point()+
  geom_smooth(method = "lm", formula =  y ~ x) +
  facet_grid( ~ Lake  , scales = "free_y") +
  theme_bw() 


y_labels <- c(
  `Secchi (m)` = "Secchi Depth (m)",
  `TP (mg/m³)` = "Total Phosphorus (mg/m³)",
  `pH` = "pH",
  `NH4-N (mg/m³)` = "Ammonium Nitrogen (mg/m³)",
  `Chla (mg/m³)` = "Chlorophyll-a (mg/m³)",
  `TN (mg/m³)` = "Total Nitrogen (mg/m³)",
  `E. coli (#/100 mL)` = "E. coli (#/100 mL)",
  `NNN (mg/m³)` = "Nitrate Nitrogen (mg/m³)",
  `DRP (mg/m³)` = "Dissolved Reactive Phosphorus (mg/m³)",
  `Turb (NTU)` = "Turbidity (NTU)",
  `Depth (m)` = "Depth (m)",
  `TSS (g/m³)` = "Total Suspended Solids (g/m³)"
)

# Create the plot
ggplot(Okaro %>% filter(Parameter %in% c("Secchi (m)","TP (mg/m³)","pH", "NH4-N (mg/m³)", "Chla (mg/m³)", "TN (mg/m³)", "Turb (NTU)", "NNN (mg/m³)",  "DRP (mg/m³)" ))
       , aes(x = Date, y = Value, color = as.factor(Sample))) +  # Use color for different samples
  geom_point(size = 2, alpha = 0.6) +  # Enhanced point aesthetics
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linetype = "dashed", size = 1.5, col="red") +  # Improved line aesthetics
  facet_wrap(~ Parameter, scales = "free_y", labeller = labeller(Parameter = y_labels)) +  # Facet by Parameter with free y-scales and custom y-axis labels
  theme_bw(base_size = 15) +  # Base theme with larger text size for readability
  labs(
    title = "Lake Ōkaro Water Quality Parameters",
    x = "Date",
    color = "Sample",  # Legend title for the colors
    caption = "Source: BOPRC Ōkaro Data"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Centering and bolding the title
    axis.title.x = element_text(face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(face = "bold"),  # Bold y-axis title
    strip.text = element_text(face = "bold", size = 14),  # Bold and larger facet labels
    panel.grid.major = element_line(color = "grey80"),  # Lighter major grid lines
    panel.grid.minor = element_blank()  # Removing minor grid lines for cleaner look
  )


ggplot(Okaro %>% filter(Parameter %in% c("Secchi (m)")) #, "Chla (mg/m³)", "TP (mg/m³)"
       , aes(Date, Value)) +
  geom_point(color = 'blue', size = 2, alpha = 0.6) +  # Enhanced point aesthetics
  geom_smooth(method = "lm", formula = y ~ x, color = 'red', se = FALSE, linetype = "dashed", size = 1.5) +  # Line aesthetics
  facet_grid(~ Lake, scales = "free_y") +  # Faceting by Lake with free y-scales
  theme_bw(base_size = 15) +  # Base theme with larger text size
  labs(
    title = "Secchi Depth",
    x = "Date",
    y = "Secchi Depth (m)",
    caption = "Source: BOPRC Ōkaro Data") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Centering and bolding the title
    axis.title.x = element_text(face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(face = "bold"),  # Bold y-axis title
    strip.text = element_text(face = "bold", size = 14),  # Bold and larger facet labels
    panel.grid.major = element_line(color = "grey80"),  # Lighter grid lines
    panel.grid.minor = element_blank()  # Removing minor grid lines
 )

ggplot(Okaro %>% filter(Value<=2000,Parameter %in% c("TP (mg/m³)")) #, "Chla (mg/m³)", "TP (mg/m³)"
       , aes(Date, Value)) +
  geom_point(color = 'blue', size = 2, alpha = 0.6) +  # Enhanced point aesthetics
  geom_smooth(method = "lm", formula = y ~ x, color = 'red', se = FALSE, linetype = "dashed", size = 1.5) +  # Line aesthetics
  facet_grid(~ Lake, scales = "free_y") +  # Faceting by Lake with free y-scales
  theme_bw(base_size = 15) +  # Base theme with larger text size
  geom_vline(data = Dosings_Okaro, aes(xintercept = as.Date(Date)), linetype = "longdash", color = "red") +
  labs(
    title = "Total Phosphorus",
    x = "Date",
    y = "Total Phosphorus (mg/m³)",
    caption = "Source: BOPRC Ōkaro Data"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Centering and bolding the title
    axis.title.x = element_text(face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(face = "bold"),  # Bold y-axis title
    strip.text = element_text(face = "bold", size = 14),  # Bold and larger facet labels
    panel.grid.major = element_line(color = "grey80"),  # Lighter grid lines
    panel.grid.minor = element_blank()  # Removing minor grid lines
  )

ggplot(Okaro %>% filter(Value<=2000, year(Date) >= 2020,Parameter %in% c("TP (mg/m³)")) #, "Chla (mg/m³)", "TP (mg/m³)"
       , aes(Date, Value)) +
  geom_point(aes(fill = Sample), shape=21, col="black",size = 3, alpha = 1) +  # Enhanced point aesthetics
  geom_smooth(method = "lm", formula = y ~ x, color = 'black', se = FALSE, linetype = "dashed", size = 1.5) +  # Line aesthetics
  facet_grid(~ Lake, scales = "free_y") +  # Faceting by Lake with free y-scales
  theme_bw(base_size = 15) +  # Base theme with larger text size
  geom_vline(data = Dosings_Okaro, aes(xintercept = as.Date(Date)), linetype = "longdash", color = "red") +
  labs(
    title = "Total Phosphorus",
    x = "Date",
    y = "Total Phosphorus (mg/m³)",
    caption = "Source: BOPRC Ōkaro Data"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Centering and bolding the title
    axis.title.x = element_text(face = "bold"),  # Bold x-axis title
    axis.title.y = element_text(face = "bold"),  # Bold y-axis title
    strip.text = element_text(face = "bold", size = 14),  # Bold and larger facet labels
    panel.grid.major = element_line(color = "grey80"),  # Lighter grid lines
    panel.grid.minor = element_blank()  # Removing minor grid lines
  )


Okaro_TP<- Okaro %>% filter(Value<=2000, year(Date) >= 2020,Parameter %in% c("TP (mg/m³)"))

################################################################################
# WQ_master_dataframe_2021.csv -------------------------------------------------
WQ_df <- read_excel("~/PhD/Data/Lakes/All_lakes/WQ_master_dataframe_2021.xlsx")

names(WQ_df) # "Number", "SiteID","LocationName","Lake","Site","Sample","Date","DepthFrom","Parameter","Value","Sample_Depth"
unique(WQ_df$Parameter)
summary(WQ_df)
# Convert Date to Date format & Extract months and years
WQ_df$Date <- as.Date(WQ_df$Date, format = "%d/%m/%Y")
WQ_df$Month <- month(WQ_df$Date, label = TRUE)
WQ_df$Year <- year(WQ_df$Date)


# Define the desired order of lakes
desired_order <- c("Lake Rotorua", "Lake Rotoiti", "Lake Rotoehu", "Lake Rotomā", "Lake Ōkāreka", "Lake Ōkaro")

# Create a factor column for "Lake" with the desired order
WQ_df <- WQ_df %>%
  mutate(Lake = factor(Lake, levels = desired_order))

ggplot(WQ_df %>% filter(year(Date) > 2018)  %>% filter(Lake %in% c("Lake Rotorua", "Lake Rotoiti", "Lake Rotoehu", "Lake Rotomā", "Lake Ōkāreka", "Lake Ōkaro"), Sample == "Integrated"),
       aes(Date, Value, col = Parameter)) +
  geom_line() +
  #geom_smooth(method = "lm", se = FALSE, formula = 'y ~ x', col = "black") +
  facet_grid(Parameter ~ Lake+Site, scales = "free_y") +
  theme_bw() +
  theme(strip.text.x = element_text(angle = 0, hjust = 0.5))  # Set angle to 0 and hjust to center horizontally


# lawa-monitoring-data.csv -----------------------------------------------------
Monitoring_Data <- read.csv("~/PhD/Data/Lakes/All_lakes/lawa-monitoring-data.csv")



# Filter for Te Arawa lakes
MD <- Monitoring_Data %>% 
  filter(Region == "bay of plenty")
head(MD)
names(MD)
unique(MD$Parameter)

# Convert Date to Date format & Extract months and years
MD$Date <- as.Date(MD$SampleDateTime , format = "%d/%m/%Y")
MD$Month <- month(MD$Date, label = TRUE)
MD$Year <- year(MD$Date)

# make parameters from Indicator and units
MD <- mutate(MD, Parameter = paste(Indicator, " (", Units, ")", sep = ""))

# Split SiteID column into Lake, Site, and Sample columns
MD <- MD %>%
  separate(SiteID, into = c("Lake", "Site", "Sample"), sep = "( at | \\()", convert = TRUE, remove = FALSE)

# make more correct with WQ data set rename parameters and sites
unique(WQ_df$Parameter)
unique(MD$Parameter)

MD <- MD %>%
  mutate(Parameter = case_when(
    Parameter == "Secchi (m)"          ~ "SecchiDepth (m)",
    Parameter == "pH (-log[H+])"       ~ "pH",
    Parameter == "NH4N (mg/L)"         ~ "NH4-N (mg/m3)",
    Parameter == "CHLA (mg/m3)"        ~ "Chla (mg/m3)",
    TRUE                               ~ Parameter))

MD <- MD %>%
  mutate(Site = case_when(
    Site == "Site 5"          ~ "5",
    Site == "Site 4"          ~ "4",
    Site == "Site 3"          ~ "3",
    Site == "Site 2"          ~ "2",
    Site == "Site 1"          ~ "1",
    Site == "Okawa Bay"  ~ "Ōkawa Bay",  # Make sure the character encoding is correct
    TRUE                 ~ Site))

MD <- MD %>%
  mutate(Sample = case_when(
    Sample == "Integrated)" ~ "Integrated",
    TRUE ~ Sample))

# Create a vector of original and renamed lake names
original_names <- c("Lake Rotoma", "Lake Okataina", "Lake Okareka", "Lake Rotokakahi", "Lake Rotomahana", "Lake Okaro")
renamed_names <- c("Lake Rotomā", "Lake Rotomā", "Lake Ōkāreka", "Lake Rotokākahi", "Lake Rotomāhana", "Lake Ōkaro")

# Replace only specific lake names with their renamed counterparts
MD$Lake <- ifelse(MD$Lake %in% original_names, renamed_names[match(MD$Lake, original_names)], MD$Lake)


# Define the desired order of lakes
desired_order <- c("Lake Rotorua", "Lake Rotoiti", "Lake Rotoehu", "Lake Rotomā", "Lake Ōkataina", "Lake Ōkāreka", "Lake Tarawera", "Lake Tikitapu", "Lake Rotokākahi", "Lake Rotomāhana", "Lake Ōkaro", "Lake Rerewhakaaitu", "Lake Taupō")

# Create a factor column for "Lake" with the desired order
MD <- MD %>%
  mutate(Lake = factor(Lake, levels = desired_order))

# Plot the data
ggplot(MD %>% filter(year(Date) > 2018) %>% filter(Lake %in% c("Lake Rotorua", "Lake Rotoiti", "Lake Rotoehu", "Lake Rotomā", "Lake Ōkāreka", "Lake Ōkaro")),
       aes(Date, Value, col = Parameter)) +
  geom_line() +
  #geom_smooth(method = "lm", se = FALSE, formula = 'y ~ x', col = "black") +
  facet_grid(Parameter ~ Lake + Site, scales = "free_y") +
  theme_bw() +
  theme(strip.text.x = element_text(angle = 0, hjust = 0.5))


# Convert MD to sf object
MD_sf <- st_as_sf(MD, coords = c("Longitude", "Latitude"), crs = 4326)

Lakes <- st_read("~/PhD/Pictures_Figures/Maps/Layers/Lakes/12_lakes.gpkg")

Lakes_lines <- Lakes %>%
  mutate(geom = st_boundary(geom)) %>%
  select(geom, name)

# Plot the spatial points
ggplot() +
  geom_sf(data = MD_sf, shape=21, col="black", fill= "red", size=3) +
  geom_sf(data = Lakes_lines) +
  theme_bw()


# ALL data lakes nz ############################################################
# load the data comming from LAWA
Monitoring_Data <- read.csv("~/PhD/Data/Lakes/All_lakes/lawa-monitoring-data.csv")
A_MD<-Monitoring_Data

summary(A_MD)
head(A_MD)
unique(A_MD$Indicator)

# Convert SampleDateTime to Date-Time format
A_MD$SampleDateTime <- as.POSIXct(A_MD$SampleDateTime, format="%d/%m/%Y %H:%M")

na_indicator <- sum(is.na(A_MD$Value))

# plot indicatour and value
plot(A_MD$Indicator, A_MD$Value)

# Scatter plot of two numerical variables
plot(A_MD$Latitude, A_MD$Longitude)

# Box plot of Value by Region
boxplot(Value ~ Region, data=A_MD, main="Value by Region", xlab="Region", ylab="Value")


summary_by_lake <- A_MD %>%
  group_by(LawaSiteID, Indicator) %>%
  summarize(
    count = n(),
    mean_value = mean(as.numeric(Value), na.rm = TRUE),
    median_value = median(as.numeric(Value), na.rm = TRUE),
    min_value = min(as.numeric(Value), na.rm = TRUE),
    max_value = max(as.numeric(Value), na.rm = TRUE)
  )




# Combine them -----------------------------------------------------------------
head(WQ_df)
head(MD)
unique(WQ_df$Site)
unique(MD$Site)


# Combine the datasets and add a new column Dataset
combined_data <- bind_rows(mutate(WQ_df, Dataset = "WQ_df"),mutate(MD, Dataset = "MD"))

# Test for normality
#shapiro.test(combined_data$)
#qqnorm(combined_data$)

# Define the desired order of lakes
desired_order <- c("Lake Rotorua", "Lake Rotoiti", "Lake Rotoehu", "Lake Rotomā", "Lake Ōkāreka", "Lake Ōkaro")

# Create a factor column for "Lake" with the desired order
combined_data <- combined_data %>% mutate(Lake = factor(Lake, levels = desired_order))

unique(combined_data$Parameter)

# Create the plot
ggplot(combined_data %>% 
         filter(year(Date) > 2018, Lake %in% c("Lake Rotorua", "Lake Rotoiti", "Lake Rotoehu", "Lake Rotomā", "Lake Ōkāreka", "Lake Ōkaro"),Sample %in% c("Integrated")),
       aes(Date, Value, lty = Dataset, col=Parameter )) +
  geom_line() +
  facet_grid(Parameter ~ Lake + Site, scales = "free_y") +
  scale_linetype_manual(values = c("MD" = "dashed", "WQ_df" = "solid")) +
  theme_bw() +
  theme(strip.text.x = element_text(angle = 0, hjust = 0.5))


ggplot(combined_data %>% 
         filter(
           #year(Date) > 2018, 
                Lake %in% c("Lake Ōkaro"),
                
                #Sample %in% c("Integrated")
                ),
       aes(Date, Value, col=Sample )) +
  geom_line(lwd=1) +
  #geom_smooth(col="black")+
  facet_grid(Parameter ~ Lake , scales = "free_y") +
  #scale_linetype_manual(values = c("MD" = "dashed", "WQ_df" = "solid")) +
  theme_bw() +
  theme(strip.text.x = element_text(angle = 0, hjust = 0.5))



# lawa-TLI.csv -----------------------------------------------------------------
TLI <- read.csv("~/PhD/Data/Lakes/All_lakes/lawa-TLI.csv")

TLI_1 <- TLI %>% 
  filter(Region == "bay of plenty")%>%
  rename(SiteID = lake, Year = `Hydrological.Year`)

# Split SiteID column into Lake, Site, and Sample columns
TLI_2 <- TLI_1 %>%
  separate(SiteID, into = c("Lake", "Site", "Sample"), sep = "( at | \\()", convert = TRUE, remove = FALSE) %>%
  mutate(Sample = if_else(Sample == "Integrated)", "Integrated", Sample))

# Create a vector of original and renamed lake names
original_names <- c("Lake Rotoma", "Lake Okataina", "Lake Okareka", "Lake Rotokakahi", "Lake Rotomahana", "Lake Okaro")
renamed_names <- c("Lake Rotomā", "Lake Rotomā", "Lake Ōkāreka", "Lake Rotokākahi", "Lake Rotomāhana", "Lake Ōkaro")

# Replace only specific lake names with their renamed counterparts
TLI_2$Lake <- ifelse(TLI_2$Lake %in% original_names, renamed_names[match(TLI_2$Lake, original_names)], TLI_2$Lake)

# Define the desired order of lakes
desired_order <- c("Lake Rotorua", "Lake Rotoiti", "Lake Rotoehu", "Lake Rotomā", "Lake Ōkataina", "Lake Ōkāreka", "Lake Tarawera", "Lake Tikitapu", "Lake Rotokākahi", "Lake Rotomāhana", "Lake Ōkaro", "Lake Rerewhakaaitu", "Lake Taupō")

# Define the trophic status based on TLI values
TLI_2 <- TLI_2 %>%
  mutate(
    Trophic_Status = ifelse(is.na(TLI), "",
                            ifelse(TLI <= 2, "Microtrophic",
                                   ifelse(TLI <= 3, "Oligotrophic",
                                          ifelse(TLI <= 4, "Mesotrophic",
                                                 ifelse(TLI <= 5, "Eutrophic", "Supertrophic")))))
  )

# Define colors for trophic status
trophic_colors <- c("Microtrophic" = "blue", "Oligotrophic" = "green", "Mesotrophic" = "yellow", "Eutrophic" = "orange", "Supertrophic" = "red")

# Ensure data is sorted by Year
TLI_2 <- TLI_2[order(TLI_2$Year), ]

head(TLI_2)

# plot the TLI of all the lakes
Graph<- ggplot(TLI_2,aes(Year, TLI, color = Trophic_Status, fill = Trophic_Status)) +
  geom_line(aes(group = 1),col="black") +  # Connect all points without gaps , 
  geom_point(shape = 21, col = "black", size = 2) +
  scale_fill_manual(values = trophic_colors, name = "Trophic Status", 
                    breaks = c("Microtrophic", "Oligotrophic", "Mesotrophic", "Eutrophic", "Supertrophic")) +  # Define fill colors manually
  facet_wrap(~  factor(Lake, levels = desired_order), ncol = 5) +  # Arrange facets in 5 columns
  theme_bw() +
  guides(color = guide_legend(title = "Trophic Status"))

graph_animation = Graph+
  transition_reveal(Year)

 ggplot(TLI_2 %>% filter(Lake %in% c("Lake Ōkaro")),
       aes(Year, TLI, color = Trophic_Status, fill = Trophic_Status)) +
  geom_line(aes(group = 1), col = "black") +  # Connect all points without gaps
  geom_point(shape = 21, col = "black", size = 2) +
  scale_fill_manual(values = trophic_colors, name = "Trophic Status", 
                    breaks = c("Microtrophic", "Oligotrophic", "Mesotrophic", "Eutrophic", "Supertrophic")) +  # Define fill colors manually
  #scale_color_manual(values = trophic_colors, name = "Trophic Status", 
                     #breaks = c("Microtrophic", "Oligotrophic", "Mesotrophic", "Eutrophic", "Supertrophic")) +  # Define color manually
  facet_wrap(~ factor(Lake, levels = desired_order), ncol = 5) +  # Arrange facets in 5 columns
  theme_bw() +
  guides(color = guide_legend(title = "Trophic Status"), 
         fill = guide_legend(title = "Trophic Status"))




# CTD_profile_Data_BOPRC.csv ---------------------------------------------------
#CTD_profile <- read_csv("~/PhD/Data/Lakes/All_lakes/CTD_profile_Data_BOPRC.csv")
CTD_profile2<- read_excel("~/PhD/Data/Lakes/All_lakes/CTD_profile_Data_BOPRC.xlsx")

CTD <- CTD_profile2

head(CTD)
summary(CTD)
names(CTD)

# Separate the date and time components into two new columns
CTD <- CTD %>%
  mutate(Date = as.Date(Time),
         Time_of_day = format(Time, format = "%H:%M:%S"),
         Month = month(Time, label = TRUE),  
         Year = year(Time),  
         Season = case_when(Month %in% c("Dec", "Jan", "Feb") ~ "Summer", 
                            Month %in% c("Mar", "Apr", "May") ~ "Autumn", 
                            Month %in% c("Jun", "Jul", "Aug") ~ "Winter", 
                            Month %in% c("Sep", "Oct", "Nov") ~ "Spring"))

# make a sample ID for each of the casts
#CTD <- CTD %>% group_by(Time, Lake, Site) %>% mutate(Sample_ID = group_indices())

# Order the lakes will be mentioned 
Lake_names <- c("Lake Rotorua", "Lake Rotoiti", "Lake Rotoehu", "Lake Rotomā", "Lake Ōkataina", "Lake Ōkāreka", "Lake Tarawera", "Lake Tikitapu", "Lake Rotokākahi", "Lake Rotomāhana", "Lake Ōkaro", "Lake Rerewhakaaitu", "Lake Taupō")
CTD$Lake <- factor(CTD$Lake, levels = Lake_names)



# Plot the data
ggplot(CTD %>%
         filter(`Depth (m)` >= 1, 
                year(Date) > 2018, 
                Lake %in% c("Lake Rotorua", "Lake Rotoiti", "Lake Rotoehu", "Lake Rotomā", "Lake Ōkāreka", "Lake Ōkaro")), 
       aes(`Water Temp (degC)`, `Depth (m)`)) +
  geom_line(aes(col = Time)) +
  facet_grid(Month ~ Lake, scales = "free") +
  theme_bw() +
  xlab(expression('Temperature ('*degree*C*')')) +
  scale_y_reverse()

ggplot(CTD %>%
         filter(`Depth (m)` >= 1, 
                #year(Date) == 2021, 
                Lake %in% c("Lake Ōkaro")), 
       aes(`DO (g/m^3)`, `Depth (m)`)) +
  geom_line(aes(col=Season), lwd=1) +
  geom_vline(xintercept = 5, linetype = "dashed", color = "red")+
  facet_grid(Month ~ Lake +Year) +
  theme_bw() +
  xlab(expression("DO" ~ "(g"~m^-3*")"))+ 
  scale_y_reverse()

ggplot(CTD %>%
         filter(`Depth (m)` >= 1, 
                #Site == 4, 
                Lake %in% c("Lake Rotoiti"),
                `DO (g/m^3)` >= 0), 
       aes(`DO (g/m^3)`, `Depth (m)`)) +
  geom_line(aes(col=Site), lwd=1) +
  geom_vline(xintercept = 5, linetype = "dashed", color = "red")+
  facet_grid(Month ~ Lake +Year) +
  theme_bw() +
  xlab(expression("DO" ~ "(g"~m^-3*")"))+
  scale_y_reverse()


ggplot(CTD %>%
         filter(`Depth (m)` >= 1, 
                Lake %in% c("Lake Rotorua"),
                `DO (g/m^3)` >= 0), 
       aes(`DO (g/m^3)`, `Depth (m)`)) +
  geom_line(aes(col=Site), lwd=1) +
  geom_vline(xintercept = 5, linetype = "dashed", color = "red")+
  facet_grid(Month ~ Lake +Year) +
  theme_bw() +
  xlab(expression("DO" ~ "(g"~m^-3*")"))+ 
  scale_y_reverse()

################################################################################

