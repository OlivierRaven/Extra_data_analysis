# Script for PhD to test some old data
#date: 15/03/2024


# Clean and load packages ------------------------------------------------------
cat("\014"); rm(list = ls())#; dev.off()
#sapply(.packages(), unloadNamespace)

# set working directory
setwd("~/PhD/Data")

# Define the list of packages
packages <- c("ggpmisc","readxl", "tidyverse", "dplyr", "ggplot2", "sf", "ggmap", "patchwork", "stringr", "units")

# Load packages if not already installed
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)})

#-------------------------------------------------------------------------------
#file.choose()
Lakes_Monitoring <- read_excel("PhD/Data/Lakes/All_lakes/Lakes_Monitoring.xlsx", sheet = "Old_monitoring_data")


omd<- Old_data
summary(omd)
omd$Lake<- as.factor(omd$Lake)
omd$Year<- as.factor(omd$Year)

plot(omd$Lake, omd$Mean_CPUE_kōura, col=omd$Year)

ggplot(omd, aes(Year,Mean_CPUE_kōura, fill=Site))+
  geom_boxplot()+
  geom_point(aes(fill= Site), col= "black", shape=21)+
  facet_wrap(~Lake)+
  theme_bw()


ggplot(omd %>% filter(Site %in% c("Ōkere Arm", "Te Ākau")),
       aes(Year, Mean_CPUE_kōura, fill= Site)) +
  #geom_boxplot()+
  geom_bar(stat = "identity", color = "black", position = position_dodge(width = 0.8))+
  #geom_point(aes(fill= Site), col= "black", shape=21)+
  #facet_wrap(~Site)+
  theme_bw()


# Calculate mean and standard error for each year and site
mean_se_data <- omd %>%
  filter(Site %in% c("Ōkere Arm", "Te Ākau")) %>%
  group_by(Year, Site) %>%
  summarize(mean_CPUE = mean(Mean_CPUE_kōura),
            se_CPUE = sd(Mean_CPUE_kōura) / sqrt(n()))

# Plot means with standard error bars
ggplot(mean_se_data, aes(Year, mean_CPUE, fill = Site)) +
  geom_bar(stat = "identity", color = "black", position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = mean_CPUE - se_CPUE, ymax = mean_CPUE + se_CPUE),
                position = position_dodge(width = 0.8), width = 0.25, color = "black") +
  labs(x = "Year", y = "Mean CPUE kōura", fill = "Site") +
  theme_bw()


