# Install packages
install.packages(c("tidyr", "data.table", "dplyr", "ggplot2", "janitor", "missForest", "VIM"))

# Load library
library(tidyr)
library(data.table)
library(dplyr)
library(janitor)    
library(missForest) 
library(VIM) 

# Set working directory
WD <- "~/Custom Office Templates/Academic/Degree/Year 2 Sem 1/PfDA/Assignment/R Source Code" # Change to your WD
setwd(WD)

# Import dataset
file_path <- file.path(WD, "retail_data.csv")
retail_data <- read.csv(file_path, stringsAsFactors = FALSE)
View(retail_data)

# Clean up and rename Total_Purchases to Purchase_Quantity for clarity
retail_data <- retail_data |> rename(Purchase_Quantity = Total_Purchases)
retail_data$Purchase_Quantity <- as.numeric(retail_data$Purchase_Quantity)

# Clean Phone Num
retail_data$Phone <- as.character(retail_data$Phone)

# Clean Age [Take the customer age range]
# retail_data$Age_Group <- cut(retail_data$Age, 
#                              breaks = c(18, 25, 35, 50, 70), 
#                              labels = c("18-25", "26-35", "36-50", "51-70"), 
#                              right = TRUE)

# Clean Amount
retail_data$Amount <- round(as.numeric(retail_data$Amount), 2)

# Truncate total amount column and replace with calculated value
retail_data$Total_Amount <- NULL
retail_data$Total_Amount <- retail_data$Purchase_Quantity * retail_data$Amount




## Handling missing values
# Check for missing values
colSums(is.na(retail_data))

# Remove rows without Transaction_ID
retail_data <- retail_data |> drop_na(Transaction_ID)

# MissForest Imputation

# KNN Imputation


# Validate dataset to make sure it had cleaned
str(retail_data)
summary(retail_data)
