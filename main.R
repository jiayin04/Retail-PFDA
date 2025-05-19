# main.R
# Main script that consisted of the entire analysis workflow

# Set working directory to the project root
# setwd("~/path/to/retail_analysis")

# Source utility scripts
source("utils/helper_functions.R")
source("preprocessing/data_cleaning.R")

### ─────────────────────────────────────────────
### 1. Install and Load Packages
### ─────────────────────────────────────────────
source("utils/libraries.R")

### ─────────────────────────────────────────────
### 2. Set Working Directory & Load Dataset
### ─────────────────────────────────────────────
# Set working directory
WD <- "~/Custom Office Templates/Academic/Degree/Year 2 Sem 1/PfDA/Assignment/R Source Code" # Change to your WD
if (!dir.exists(WD)) stop("Working directory does not exist!")

# Import dataset
file_path <- file.path(WD, "retail_data.csv")
retail_full_data <- read_csv(file_path)
View(retail_full_data)


### ─────────────────────────────────────────────
### 3. Initial Cleaning & Preprocessing
### ─────────────────────────────────────────────
# Apply preporcessing
retail_data_proc <- preprocess(retail_full_data)

# Structure and summary check
str(retail_data_proc)
summary(retail_data_proc)
View(retail_data_proc)


### ─────────────────────────────────────────────
### 4. Individual Analyses
### ─────────────────────────────────────────────
# KOK JIA YIN (TP071062) - To evaluate the influence of product preferences on purchasing behavior and customer satisfaction. 
source("analysis/product_analysis_jiayin.R")





### ─────────────────────────────────────────────
### 5. Group Analysis
### ─────────────────────────────────────────────
source("analysis/group_predictive_analysis.R")
