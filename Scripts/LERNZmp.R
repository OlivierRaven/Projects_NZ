# Title: Data Analysis LERNZmp =================================================
# Author: Olivier Raven
# Date: 09-12-2024
# Description: Script to process, analyze, and visualize data
# Clean and load packages ------------------------------------------------------
cat("\014"); rm(list = ls())#; dev.off()
#sapply(.packages(), unloadNamespace)

#Set working derectory
setwd("~/Documents/R/NZ_Projects/Data_raw")

# Install from GitHub
pak::pak("limnotrack/AEME")

# Define the list of packages
packages <- c("AEME", "pak", "readr", "readxl", "tidyverse", "dplyr", "ggplot2")

# Load packages if not already installed
lapply(packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE))
    install.packages(pkg, dependencies = TRUE)
  library(pkg, character.only = TRUE)})

# Import the data --------------------------------------------------------------
list.files("lernzmp")

metadata <- read.csv("lernzmp/LERNZmp_lake_metadata.csv")

metadata <- metadata |> 
  dplyr::filter(aeme_file %in% c("LID11133", "LID40102"))
metadata

aeme <- readRDS("lernzmp/LID11133.rds")
class(aeme)

aeme

list.files("lernzmp/aeme", recursive = TRUE)

aeme <- run_aeme(aeme = aeme, model = model, path = "lernzmp/aeme", parallel = TRUE)
aeme


plot_output(aeme = aeme, model = model, var_sim = "HYD_temp")


plot_output(aeme = aeme, model = model, var_sim = "CHM_oxy")


# Check if the path exists
if (!dir.exists(path)) {
  # If the folder doesn't exist, create it
  dir.create(path)
}

