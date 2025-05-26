### ─────────────────────────────────────────────
### Install Required Packages
### ─────────────────────────────────────────────
required_packages <- c(
  "tidyr", "data.table", "dplyr", "ggplot2", "janitor",
  "missForest", "VIM", "zoo", "gender", "stringr", "mice",
  "lubridate", "hms", "readr", "car", "MetBrewer", "patchwork",
  "reshape2", "caret", "randomForest", "xgboost", "pROC",
  "plotly", "dendextend", "viridis", "purrr", "cluster", "factoextra", 
  "NbClust", "fmsb", "vcd", "rcompanion", "corrplot", "gridExtra", 
  "GGally", "waffle", "treemap", "ggridges", "ggalluvial", "ggmosaic",
  "tidyverse", "RColorBrewer", "shiny",
  "shinydashboard", "DT", "shinycssloaders"
)

# Install any packages that are not already installed
installed_packages <- rownames(installed.packages())
packages_to_install <- setdiff(required_packages, installed_packages)
if (length(packages_to_install) > 0) {
  install.packages(packages_to_install)
}

### ─────────────────────────────────────────────
### Load Libraries
### ─────────────────────────────────────────────

# ─── Data Manipulation and Cleaning ───
library(dplyr)        # Data wrangling and pipeline operations
library(tidyr)        # Tidy data reshaping
library(data.table)   # Fast and efficient data processing
library(janitor)      # Clean names and remove empty rows
library(stringr)      # String manipulation and regex
library(zoo)          # Time series and interpolation
library(readr)        # Fast file reading
library(tidyverse)    # Collection of tidy data packages

# ─── Date-Time Handling ───
library(lubridate)    # Parse and manipulate date/time
library(hms)          # Time of day (HH:MM:SS)

# ─── Missing Data Imputation ───
library(missForest)   # Random forest imputation for numeric/mixed data
library(VIM)          # KNN-based and visual imputation
library(mice)         # Multiple Imputation by Chained Equations

# ─── Gender Inference ───
library(gender)       # Predict gender from first names

# ─── Statistical Analysis ───
library(car)          # Companion to Applied Regression (e.g., MANOVA)
library(rcompanion)   # Functions for descriptive statistics and model diagnostics

# ─── Functional Programming ───
library(purrr)        # Apply functions across lists/collections (e.g., map, reduce)

# ─── Machine Learning ───
library(caret)        # Training/testing splits, preprocessing, evaluation
library(randomForest) # Random Forest model
library(xgboost)      # Gradient boosting classifier
library(pROC)         # AUC and ROC curve analysis

# ─── Visualization ───
library(ggplot2)      # Standard plotting library
library(patchwork)    # Combine ggplot2 plots
library(MetBrewer)    # Beautiful color palettes for plots
library(reshape2)     # Reshape data frames (e.g., melt and cast operations)
library(viridis)      # Color palettes optimized for colorblind and grayscale viewing
library(vcd)          # For plotting Mosaic plot
library(corrplot)     # Correlation matrix visualization
library(gridExtra)    # Arrange multiple grid-based plots
library(GGally)       # Extension to ggplot2 for correlation matrices and pair plots
library(waffle)       # Waffle/square pie charts
library(treemap)      # Treemap visualization
library(ggridges)     # Ridge plots for distribution visualization
library(ggalluvial)   # Alluvial diagrams for categorical data flows
library(ggmosaic)     # Mosaic plots with ggplot2 syntax
library(RColorBrewer) # Color palettes for maps and plots

# ─── Interactive / Clustering ───
library(plotly)       # Interactive 2D/3D plots
library(dendextend)   # Enhanced dendrograms (e.g., color branches)
library(cluster)      # Evaluate the quality of clustering
library(factoextra)   # For clustering visualization
library(NbClust)      # Determine optimal number of clusters
library(fmsb)         # Create radar/spider charts

# ─── Web Applications ───
library(shiny)        # Interactive web applications
library(shinydashboard) # Dashboard framework for Shiny apps
library(DT)           # Interactive data tables for Shiny
library(shinycssloaders) # Loading animations for Shiny apps