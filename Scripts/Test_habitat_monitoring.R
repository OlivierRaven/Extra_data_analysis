# habitat monitoring test data collected
# By Olivier Raven
# Date: 20/05/24


# Clean and load packages ------------------------------------------------------
cat("\014"); rm(list = ls())#; dev.off()
#sapply(.packages(), unloadNamespace)

# set working directory
setwd("~/PhD/Data")

# Define the list of packages
packages <- c("mgcv","ggfortify","psych", "ggrepel","readxl", "tidyverse", "dplyr", "ggplot2", "sf", "ggmap", "patchwork", "stringr", "units")

# Load packages if not already installed
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)})

################################################################################
Test_HM <- read_excel("Habitat_monitoring/Test_HM.xlsx")
Test_Catch <- read_excel("Habitat_monitoring/Test_HM.xlsx", sheet = "Test_Catch")
TestHM <- read_excel("Habitat_monitoring/Test_HM.xlsx", sheet = "Test_Habitat_Monitoring")

HM<-Test_HM
TC<-Test_Catch

plot(DO_percent~DO_mgl, data=TestHM)

# Group by Site and Species, and calculate total catch and effort (number of fyke nets) for each combination
TC <- TC %>%
  mutate(Wet_weight_g = ifelse(Species == "Kōura", 0.000648 * (Length_mm ^ 3.0743), NA))

# Calculate CPUE and BPUE
CPUE_BPUE <- TC %>%
  group_by(ID, Species) %>%
  summarise(Total_Catch = sum(Amount, na.rm = TRUE),
    Total_Weight = sum(Wet_weight_g, na.rm = TRUE),
    Average_Length = mean(Length_mm, na.rm = TRUE),
    Total_Effort = 2,  # Assuming this is constant for all groups
    .groups = 'drop') %>%
  mutate(CPUE = Total_Catch / Total_Effort,
    BPUE = Total_Weight / Total_Effort)


species_columns <- unique(CPUE_BPUE$Species)

# Initialize the columns with NA values
for (col in species_columns) {
  TestHM[[col]] <- NA}

# Fill in CPUE values where available
for (i in 1:nrow(CPUE_BPUE)) {
  row_index <- which(TestHM$ID == CPUE_BPUE$ID[i])
  TestHM[row_index, CPUE_BPUE$Species[i]] <- CPUE_BPUE$CPUE[i]}
print(TestHM)

# Replace NA values with zero
TestHM[is.na(TestHM)] <- 0


plot(Kōura~Temperature , data = TestHM)


# Fit a binomial GAM with a logistic link function
gam_presence <- gam(Kōura ~ s(Water_clarity, k=3) + s(D_10m_m, k=3) + s(Rip_veg, k=3) + s(Erosion, k=3) + 
                      s(Boulders, k=3) + s(Cobble, k=3) + s(Gravel, k=3) + s(Sand, k=3) + 
                      s(Mud, k=3) + s(DO_mgl, k=3) + s(DO_percent, k=3) + s(pH, k=3) + 
                      s(Temperature, k=3) + s(Em_Su_macro, k=3) + s(Wood, k=3),
                    data = TestHM, family = binomial(link = "logit"))

summary(gam_presence)

# Plot diagnostics for presence-absence model
plot(gam_presence, pages = 1)
gam.check(gam_presence)

# Plot diagnostics for abundance model
plot(gam_abundance, pages = 1)
gam.check(gam_abundance)

# Predict presence-absence
pred_presence <- predict(gam_presence, newdata = TestHM, type = "response")

# Predict abundance
pred_abundance <- predict(gam_abundance, newdata = TestHM, type = "response")

# Add predictions to your data frame
TestHM <- TestHM %>%
  mutate(pred_presence = pred_presence,
         pred_abundance = pred_abundance)



#######################################
# Load necessary library
library(mgcv)

# Generate synthetic data using gamSim
set.seed(123)  # for reproducibility
data <- gamSim(1, n = 400, dist = "normal", scale = 2)

# Fit a GAM to the synthetic data
gam_model <- gam(y ~ s(x0, k = 3) + s(x1, k = 3) + s(x2, k = 3), data = data)

# Plot the smooth functions
plot(gam_model, select = 1, scheme = 1, col = "blue", main = "Smooth Function for x0")
plot(gam_model, select = 2, scheme = 1, col = "red", main = "Smooth Function for x1")
plot(gam_model, select = 3, scheme = 1, col = "green", main = "Smooth Function for x2")


################################################################################

HM <- read_excel("~/PhD/Data/Lakes/All_lakes/Lakes_Monitoring.xlsx", sheet = "Test_Habitat_Monitoring")
TC <- read_excel("~/PhD/Data/Lakes/All_lakes/Lakes_Monitoring.xlsx", sheet = "Test_Catch")
HM1<-HM
TC1<-TC

summary(HM)
summary(TC)


hist(TC$Length_mm)
#shapiro.test(HM$pH) 
qqnorm(HM$pH)
qqline(HM$pH)


ggplot(TC1, aes(x = Length_mm)) +
  geom_histogram(binwidth = 10) +
  facet_wrap(~Species, scales = "free")


# Group by Site and Species, and calculate total catch and effort (number of fyke nets) for each combination
CPUE_species <- TC1 %>%
  group_by(ID, Species) %>%
  summarise(Total_Catch = sum(Amount, na.rm = TRUE),
            Total_Effort = 2) %>%
  mutate(CPUE = Total_Catch / Total_Effort)


# Selecting only ID and CPUE columns
CPUE_species_select <- CPUE_species[c("ID", "Species", "CPUE")]

# Spread CPUE_species_select data frame
CPUE_species_spread <- spread(CPUE_species_select, key = Species, value = CPUE)

# Merging with the HM dataset
HM2 <- merge(HM1, CPUE_species_spread, by = "ID", all.x = TRUE)
HM2 <- HM2 %>%
  mutate_at(vars(-ID), ~replace_na(., 0))

# Remove non-numeric columns
HM2_numeric <- select(HM2, -c(Lat, Lon, ID, Lake, DHT, Site, Date_Time_in, Weather, Other))


# Test normality with scatterplot matrices
pairs.panels(HM2_numeric, gap = 0, pch = 21)

# Check for columns with zero variance
zero_var_cols <- apply(HM2_numeric, 2, function(x) length(unique(x)) == 1)
zero_var_cols <- names(zero_var_cols)[zero_var_cols]

# Remove columns with zero variance
HM2_numeric_filtered <- HM2_numeric[, !names(HM2_numeric) %in% zero_var_cols]

# Identify columns with zero variance
zero_var_cols <- apply(HM2_numeric_filtered, 2, function(x) all(x == x[1]))

# Remove constant/zero variance columns
HM2_numeric_filtered <- HM2_numeric_filtered[, !zero_var_cols]

# Perform PCA after removing constant/zero variance columns
pc <- prcomp(HM2_numeric_filtered, center = TRUE, scale. = TRUE)

# Extract PC scores and variable loadings
pc_scores <- as.data.frame(pc$x)
pc_loadings <- as.data.frame(pc$rotation)

# Create biplot
ggplot(pc_scores, aes(x = PC1, y = PC2)) +
  geom_point() +
  geom_text_repel(data = pc_loadings, aes(label = rownames(pc_loadings)), color = "blue") 


#
# Create the boxplot
ggplot(TC1, aes(Species, Length_mm, fill = ID)) +
  geom_boxplot() +
  theme_bw()

ggplot(CPUE_species, aes(Species, CPUE, col = ID)) +
  geom_point() +
  theme_bw()

ggplot(HM2, aes(Sand,Catfish , col = ID)) +
  geom_point() +
  theme_bw()

################################################################################
# NEW TEST

library(dplyr)

# Create a mock dataset
set.seed(123)

# Define the number of sites
n_sites <- 60

# Define possible habitat types
habitat_types <- c("Raupō fields", "Cobbles", "Boulders", "Sand", "Gravel", "Mud")

# Create a mock data frame
monitoring_data <- data.frame(
  Lake = rep(c("Rotorua", "Rotoiti", "Rotoehu", "Rotomā", "Ōkāreka"), each = n_sites/5),
  SiteID = 1:n_sites,
  HabitatType = sample(habitat_types, n_sites, replace = TRUE),
  BedrockCover = runif(n_sites, 0, 100),
  BoulderCover = runif(n_sites, 0, 100),
  CobbleCover = runif(n_sites, 0, 100),
  GravelCover = runif(n_sites, 0, 100),
  SandCover = runif(n_sites, 0, 100),
  MudCover = runif(n_sites, 0, 100),
  WoodyDebrisCover = runif(n_sites, 0, 100),
  OrganicMatterCover = runif(n_sites, 0, 100),
  VegetationCover = runif(n_sites, 0, 100),
  KouraCount = sample(0:50, n_sites, replace = TRUE),
  KouraBiomass = runif(n_sites, 0, 500),
  FishCount = sample(0:50, n_sites, replace = TRUE),
  FishBiomass = runif(n_sites, 0, 500)
)

# Ensure cover percentages sum to 100
monitoring_data <- monitoring_data %>%
  rowwise() %>%
  mutate(TotalCover = BedrockCover + BoulderCover + CobbleCover + GravelCover + SandCover + MudCover + WoodyDebrisCover + OrganicMatterCover + VegetationCover,
         BedrockCover = (BedrockCover / TotalCover) * 100,
         BoulderCover = (BoulderCover / TotalCover) * 100,
         CobbleCover = (CobbleCover / TotalCover) * 100,
         GravelCover = (GravelCover / TotalCover) * 100,
         SandCover = (SandCover / TotalCover) * 100,
         MudCover = (MudCover / TotalCover) * 100,
         WoodyDebrisCover = (WoodyDebrisCover / TotalCover) * 100,
         OrganicMatterCover = (OrganicMatterCover / TotalCover) * 100,
         VegetationCover = (VegetationCover / TotalCover) * 100) %>%
  select(-TotalCover)

# Display the first few rows of the dataset
head(monitoring_data)
summary(monitoring_data)

predictors <- monitoring_data[, c("BedrockCover", "BoulderCover", "CobbleCover",
                                  "GravelCover", "SandCover", "MudCover",
                                  "WoodyDebrisCover", "OrganicMatterCover",
                                  "VegetationCover", "KouraCount", "FishCount")]

normalized_data <- as.data.frame(scale(predictors))
head(normalized_data)

par(mfrow = c(3, 3))  # Adjust the layout based on the number of variables

hist(monitoring_data$BedrockCover, main = "Bedrock Cover", xlab = "Cover (%)")
hist(monitoring_data$BoulderCover, main = "Boulder Cover", xlab = "Cover (%)")
hist(monitoring_data$CobbleCover, main = "Cobble Cover", xlab = "Cover (%)")
hist(monitoring_data$GravelCover, main = "Gravel Cover", xlab = "Cover (%)")
hist(monitoring_data$SandCover, main = "Sand Cover", xlab = "Cover (%)")
hist(monitoring_data$MudCover, main = "Mud Cover", xlab = "Cover (%)")
hist(monitoring_data$WoodyDebrisCover, main = "Woody Debris Cover", xlab = "Cover (%)")
hist(monitoring_data$OrganicMatterCover, main = "Organic Matter Cover", xlab = "Cover (%)")
hist(monitoring_data$VegetationCover, main = "Vegetation Cover", xlab = "Cover (%)")

par(mfrow = c(1, 1))
# Example: Correlation matrix
cor_matrix <- cor(predictors)
corrplot::corrplot(cor_matrix, method = "color")
pairs(predictors)


install.packages("mgcv")
install.packages("randomForest")
install.packages("dplyr")

# Load necessary libraries
library(mgcv)
library(dplyr)

# Fit a GAM model
gam_model <- gam(KouraCount ~ s(BedrockCover) + s(BoulderCover) + s(CobbleCover) + 
                   s(GravelCover) + s(SandCover) + s(MudCover) + s(WoodyDebrisCover) + 
                   s(OrganicMatterCover) + s(VegetationCover), data = monitoring_data)

# Summary of the GAM model
summary(gam_model)

# Plot the GAM model
par(mfrow = c(3, 3))
plot(gam_model, se = TRUE)



library(randomForest)

# Fit a Random Forest model
rf_model <- randomForest(KouraCount ~ BedrockCover + BoulderCover + CobbleCover + 
                           GravelCover + SandCover + MudCover + WoodyDebrisCover + 
                           OrganicMatterCover + VegetationCover, data = monitoring_data,
                         importance = TRUE, ntree = 500)

# Summary of the RF model
print(rf_model)

# Variable importance plot
importance(rf_model)
varImpPlot(rf_model)




# Select relevant variables for clustering
cluster_data <- monitoring_data %>%
  select(BedrockCover, BoulderCover, CobbleCover, GravelCover, SandCover, MudCover,
         WoodyDebrisCover, OrganicMatterCover, VegetationCover)

# Perform K-means clustering with 3 clusters (for example)
set.seed(123)
k <- 3  # Number of clusters
kmeans_model <- kmeans(cluster_data, centers = k, nstart = 25)

# Add cluster labels to the original dataset
monitoring_data <- monitoring_data %>%
  mutate(Cluster = as.factor(kmeans_model$cluster))

# Summary of the cluster centroids
cat("Cluster Centroids:\n")
print(kmeans_model$centers)

# Plot clusters (for visualization, you can choose any two variables)
plot(cluster_data[, c("BedrockCover", "BoulderCover")], col = kmeans_model$cluster,
     main = "K-means Clustering of Habitat Characteristics")

# Add centroids to the plot
points(kmeans_model$centers[, c("BedrockCover", "BoulderCover")], col = 1:k, pch = 8, cex = 2)
legend("topright", legend = paste("Cluster", 1:k), col = 1:k, pch = 8, cex = 1.2)

# You can also explore cluster profiles or characteristics using this approach


data(swiss)













# Load necessary library
library(vegan)

# Example data
data <- data.frame(
  Site_ID = rep(1:4, each = 2),
  Time_Period = rep(c("Before", "After"), 4),
  Site_Type = rep(c("Control", "Impact"), each = 4),
  Variable1 = c(10.5, 11.0, 9.8, 12.5, 10.7, 10.9, 9.6, 11.7),
  Variable2 = c(7.8, 8.0, 7.5, 8.5, 7.9, 8.1, 7.6, 8.4),
  Variable3 = c(5.2, 5.4, 5.1, 5.6, 5.3, 5.5, 5.2, 5.7)
)

# Check data structure
str(data)

# Permutational non-parametric BACI test using the adonis function from vegan package
adonis_result <- adonis(cbind(Variable1, Variable2, Variable3) ~ Time_Period * Site_Type, data = data)
summary(adonis_result)








