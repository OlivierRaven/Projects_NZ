# Title: Data Analysis LAWA Data ===============================================
# Author: Olivier Raven
# Date: 2024-11-23
# Description: Script to process, analyze, and visualize data
# Clean and load packages ------------------------------------------------------
cat("\014"); rm(list = ls())#; dev.off()
#sapply(.packages(), unloadNamespace)

#Set working derectory
setwd("~/Documents/R/NZ_Projects")

# Define the list of packages
packages <- c("rnaturalearth","sf","readr", "readxl", "tidyverse", "dplyr", "ggplot2")

# Load packages if not already installed
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)})


# Import the data sets ---------------------------------------------------------
lawa_lake <- read_excel("Data_raw/Lawa/lawa-lake-monitoring-data-2004-2023_statetrendtli-results_sep2024.xlsx", sheet = "Monitoring Dataset (2004-2023)")

lawa_estuary <- read_excel("Data_raw/Lawa/lawa-estuary-health_monitoringdataset_sept2023.xlsx", sheet = "Data")

lawa_land_cover_Broad <- read_excel("Data_raw/Lawa/lawa-land-cover-data_june-2021.xlsx", sheet = "Catchment Broad")
lawa_land_cover_Medium <- read_excel("Data_raw/Lawa/lawa-land-cover-data_june-2021.xlsx", sheet = "Catchment Medium")
lawa_land_cover_Detailed <- read_excel("Data_raw/Lawa/lawa-land-cover-data_june-2021.xlsx", sheet = "Catchment Detailed")

# Explore the Lake data --------------------------------------------------------

# Remove outliers from the dataset
lawa_lake_clean <- lawa_lake %>%
  group_by(LawaSiteID, Indicator) %>%
  mutate(
    Q1 = quantile(Value, 0.25),
    Q3 = quantile(Value, 0.75),
    IQR = Q3 - Q1,
    LowerBound = Q1 - 1.5 * IQR,
    UpperBound = Q3 + 1.5 * IQR
  ) %>%
  filter(Value >= LowerBound & Value <= UpperBound) %>%
  select(-Q1, -Q3, -IQR, -LowerBound, -UpperBound)

# Test normality
# Perform Shapiro-Wilk test for each Indicator
shapiro_results <- lawa_lake_clean %>%
  filter(Indicator %in% c("Chl-a", "pH", "Secchi", "Total Nitrogen", "Total Phosphorus")) %>%
  group_by(Indicator, LawaSiteID) %>%
  filter(n() >= 3) %>%  # Ensure at least 3 observations per group
  summarize(
    p_value = shapiro.test(Value)$p.value,
    is_normal = p_value > 0.05,  # Logical check for normality
    .groups = "drop")


# Plot over time
ggplot(lawa_lake_clean %>% 
         filter(Indicator %in% c("Chl-a", "pH", "Secchi", "Total Nitrogen", "Total Phosphorus"))) +
  geom_point(aes(SampleDateTime, Value, col=Region))+
  facet_wrap(~ Indicator, scales = "free", ncol = 1)

ggplot(lawa_lake_clean %>% 
         filter(Indicator %in% c("Chl-a", "pH", "Secchi", "Total Nitrogen", "Total Phosphorus"))) +
  geom_point(aes(SampleDateTime, Value, col=LawaSiteID))+
  geom_smooth(aes(SampleDateTime, Value, col=LawaSiteID),method=lm )+
  facet_wrap(~ Indicator, scales = "free", ncol = 1)+
  theme(legend.position = "none") 


# Calculate mean, SE, rate of change, and normalize rate in one step ---
lawa_lake_combined <- lawa_lake_clean %>%
  group_by(LawaSiteID, Indicator) %>%
  filter(n_distinct(SampleDateTime) > 1, n_distinct(Value) > 1) %>%
  summarize(
    Mean = mean(Value, na.rm = TRUE),
    SE = sd(Value, na.rm = TRUE) / sqrt(n()),
    Slope = coef(lm(Value ~ as.numeric(SampleDateTime)))[2],
    PValue = summary(lm(Value ~ as.numeric(SampleDateTime)))$coefficients[2, 4],
    Significant = PValue < 0.05,
    RateOfChange = ifelse(Significant, Slope, 0),
    .groups = "drop") %>%
  group_by(Indicator) %>%
  mutate(
    MinRate = min(RateOfChange, na.rm = TRUE),
    MaxRate = max(RateOfChange, na.rm = TRUE),
    NormRate = (RateOfChange - MinRate) / (MaxRate - MinRate)) %>%
  ungroup() %>%
  right_join(lawa_lake_clean %>% distinct(LawaSiteID, Indicator, Longitude, Latitude, Region,CouncilSiteID, GeomorphicLType,Units,LFENZID,LTypeMixingPattern ), 
             by = c("LawaSiteID", "Indicator")) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)


# Plot mean and SE for each indicator and each lake ---
ggplot(lawa_lake_combined %>% filter(Indicator %in% c("Chl-a", "pH", "Secchi", "Total Nitrogen", "Total Phosphorus")),
       aes(LawaSiteID, Mean, col=Region)) +
  geom_point() +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2) +
  facet_wrap(~ Indicator, scales = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(lawa_lake_combined %>% filter(Indicator %in% c("Chl-a", "pH", "Secchi", "Total Nitrogen", "Total Phosphorus")),
       aes(Latitude, Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2) +
  geom_smooth(method =lm)+
  facet_wrap(~ Indicator, scales = "free") 

ggplot(lawa_lake_combined %>% filter(Indicator %in% c("Chl-a", "pH", "Secchi", "Total Nitrogen", "Total Phosphorus")),
       aes(Longitude, Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2) +
  geom_smooth(method =lm)+
  facet_wrap(~ Indicator, scales = "free") 

plot_list <- lawa_lake_combined %>%
  filter(Indicator %in% c("Chl-a", "pH", "Secchi", "Total Nitrogen", "Total Phosphorus")) %>%
  split(.$Indicator) %>%
  lapply(function(data) {
    ggplot(data) +
      geom_sf(aes(fill = Mean), shape = 21, color = "black", size = 3) +
      scale_fill_gradientn(
        colors = c("blue", "green", "yellow", "red"),  # Adjust color scale
        trans = "identity",
        limits = c(min(data$Mean, na.rm = TRUE), max(data$Mean, na.rm = TRUE))) +
      theme_bw() +
      labs(title = unique(data$Indicator))})

grid.arrange(grobs = plot_list, ncol = 3) 


# Plot rate of change ---

ggplot(lawa_lake_combined %>%
         filter(Indicator %in% c("Chl-a", "pH", "Secchi", "Total Nitrogen", "Total Phosphorus"))) +
  geom_point(aes(LawaSiteID, RateOfChange, fill = RateOfChange), shape=21,col="black") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_bw() +
  facet_wrap(~ Indicator, scales = "free") 

ggplot(lawa_lake_combined %>% 
         filter(Indicator %in% c("Chl-a", "pH", "Secchi", "Total Nitrogen", "Total Phosphorus"))) +
  geom_sf(aes(fill = RateOfChange), shape = 21, color = "black", size = 3) + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_bw() +
  facet_wrap(~ Indicator) 

plot_list2 <- lawa_lake_combined %>%
  filter(Indicator %in% c("Chl-a", "pH", "Secchi", "Total Nitrogen", "Total Phosphorus")) %>%
  split(.$Indicator) %>%
  lapply(function(data) {
    ggplot(data) +
      geom_sf(aes(fill = RateOfChange), shape = 21, color = "black", size = 3) +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
      theme_bw() +
      labs(title = unique(data$Indicator))})

grid.arrange(grobs = plot_list2, ncol = 3) 



# LAWA lakedata & LERNZ metadata
unique(lawa_lake$SiteID)
unique(metadata$Name)





# Explore the Estuary data -----------------------------------------------------
unique(lawa_estuary$Indicator)
plot(lawa_estuary$Longitude, lawa_estuary$Latitude)



# Explore the Land cover data --------------------------------------------------
unique(lawa_land_cover$"Detailed Category")

plot(lawa_lake$Longitude, lawa_lake$Latitude)




