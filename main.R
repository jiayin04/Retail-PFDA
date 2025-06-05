# main.R
# Main script that consisted of the entire analysis workflow

### ─────────────────────────────────────────────
### 1. Install and Load Packages
### ─────────────────────────────────────────────
source("utils/libraries.R")


### ─────────────────────────────────────────────
### 2. Source Utility Scripts
### ─────────────────────────────────────────────
source("preprocessing/data_cleaning.R")
source("utils/helper_functions.R")


### ─────────────────────────────────────────────
### 3. Set Working Directory & Load Dataset
### ─────────────────────────────────────────────
# Set working directory
WD <- "~/Custom Office Templates/Academic/Degree/Year 2 Sem 1/PfDA/Assignment/R Source Code" # Change to your WD
if (!dir.exists(WD)) stop("Working directory does not exist!")

# Import dataset
file_path <- file.path(WD, "retail_data.csv")
retail_full_data <- read_csv(file_path)
str(retail_full_data)
View(retail_full_data)


### ─────────────────────────────────────────────
### 4. Initial Cleaning & Preprocessing
### ─────────────────────────────────────────────
# Apply preporcessing
retail_data_proc <- preprocess(retail_full_data)

# Structure and summary check
str(retail_data_proc)
summary(retail_data_proc)
View(retail_data_proc)
colSums(is.na(retail_data_proc))

### ─────────────────────────────────────────────
### 5. Individual Analyses
### ─────────────────────────────────────────────
# Nik Nur Farah Hana Binti Nik Muhd Hayazi Mazlan (TP071995) - To analyze the impact of age on purchasing behavior and customer satisfaction.
source("analysis/age_analysis_hana.R")

# KOK JIA YIN (TP071062) - To evaluate the influence of product preferences on purchasing behavior and customer satisfaction. 
source("analysis/product_analysis_jiayin.R")

# TEH JUN HENG (TP067767) - To investigate the influence of geographical location on purchasing behaviour and customer satisfaction. 
source("analysis/country_analysis_junheng.R")

# LEE AIK YEN (TP070648) - To study the influence of country, age group towards purchasing behaviour and customer satisfaction.
source("analysis/analysis_aikyen.R")


### ─────────────────────────────────────────────
### 6. Group Analysis
### ─────────────────────────────────────────────
source("analysis/group_predictive_analysis.R")


### ─────────────────────────────────────────────
### 7. GUI Display
### ─────────────────────────────────────────────
source("Dashboard.R")
