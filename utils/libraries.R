### ─────────────────────────────────────────────
### Install Required Packages
### ─────────────────────────────────────────────

required_packages <- c(
  "tidyr", "data.table", "dplyr", "ggplot2", "janitor",
  "missForest", "VIM", "zoo", "gender", "stringr", "mice",
  "lubridate", "hms", "readr", "car", "MetBrewer", "patchwork",
  "reshape2", "caret", "randomForest", "xgboost", "pROC",
  "plotly", "dendextend", "viridis", "purrr", "cluster", "factoextra", "NbClust", "fmsb",
  "vcd", "broom", "forcats", "arm", "RColorBrewer", "scales", "nnet",
  "shiny", "shinydashboard", "DT", "shinycssloaders", "tidyverse", "conflicted"
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
library(broom)        # Convert model objects to tidy data
library(arm)          # Bayesian analysis and regression modeling

# ─── Functional Programming ───
library(purrr)        # Apply functions across lists/collections (e.g., map, reduce)
library(forcats)      # Tools for working with factors

# ─── Machine Learning ───
library(caret)        # Training/testing splits, preprocessing, evaluation
library(randomForest) # Random Forest model
library(xgboost)      # Gradient boosting classifier
library(pROC)         # AUC and ROC curve analysis
library(nnet)         # Neural networks and multinomial models

# ─── Visualization ───
library(ggplot2)      # Standard plotting library
library(patchwork)    # Combine ggplot2 plots
library(MetBrewer)    # Beautiful color palettes for plots
library(reshape2)     # Reshape data frames (e.g., melt and cast operations)
library(viridis)      # Color palettes optimized for colorblind and grayscale viewing
library(vcd)          # For plotting Mosaic plot
library(RColorBrewer) # Color palettes for plots and maps
library(scales)       # Scale functions for visualization
library(fmsb)         # Create radar/spider charts

# ─── Interactive / Clustering ───
library(plotly)       # Interactive 2D/3D plots
library(dendextend)   # Enhanced dendrograms (e.g., color branches)
library(cluster)      # Evaluate the quality of clustering
library(factoextra)   # For clustering visualization
library(NbClust)      # Determine optimal number of clusters

# ─── Shiny and Dashboard ───
library(shiny)        # Web application framework
library(shinydashboard) # Dashboard UI components
library(DT)           # Interactive tables
library(shinycssloaders) # Loading spinners for Shiny

# ─── Additional Utilities ───
library(tidyverse)    # Collection of packages for data science
library(conflicted)   # Handle conflict package

# Explicitly declare which package prefer
conflict_prefer("select", "dplyr")
conflicts_prefer(janitor::chisq.test)
conflicts_prefer(pROC::roc)
conflicts_prefer(shinydashboard::box)
conflicts_prefer(shiny::dataTableOutput)
conflicts_prefer(shiny::renderDataTable)
