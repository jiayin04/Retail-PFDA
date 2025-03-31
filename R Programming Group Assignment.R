# Install packages
install.packages(c("tidyr", "data.table", "dplyr", "ggplot2", "janitor", "missForest", "VIM"))

# Load library
library(tidyr)
library(data.table)
library(dplyr)
library(janitor)    
library(missForest) 
library(VIM) 
library(hms)

# Set working directory
WD <- "~/Custom Office Templates/Academic/Degree/Year 2 Sem 1/PfDA/Assignment/R Source Code" # Change to your WD
setwd(WD)

# Import dataset
file_path <- file.path(WD, "retail_data.csv")
retail_data <- read.csv(file_path, stringsAsFactors = FALSE)
View(retail_data)

# Remove unnecessary columns
retail_data <- retail_data %>% select(-c(Address, Email, Phone, Name, Year, Month, Total_Amount, Zipcode))

# Convert all empty strings to NA across the dataset
retail_data[retail_data == ""] <- NA

# Clean Transaction_ID
retail_data <- retail_data |> mutate(Transaction_ID = as.character(Transaction_ID))
# Keep distinct
retail_data <- retail_data %>% distinct(Transaction_ID, .keep_all = TRUE)

# Clean Customer_ID
retail_data <- retail_data |> mutate(Customer_ID = as.character(Customer_ID))

# Clean ZipCode
retail_data <- retail_data |> mutate(Zipcode = as.character(Zipcode))

# Clean Age
retail_data$Age <- as.integer(retail_data$Age)

# Clean Date
retail_data$Date <- as.Date(retail_data$Date, format="%m/%d/%Y") 

# Clean Time & Convert to HH:MM:SS
retail_data$Time <- as_hms(as.POSIXct(retail_data$Time, format="%H:%M:%S")) 

# Clean up and rename Total_Purchases to Purchase_Quantity for clarity
retail_data <- retail_data |> rename(Purchase_Quantity = Total_Purchases)
retail_data$Purchase_Quantity <- as.numeric(retail_data$Purchase_Quantity)

# Clean Amount
retail_data$Amount <- round(as.numeric(retail_data$Amount), 2)

# Rename products to Products
retail_data <- retail_data  |> rename(Products = products)

# Categorizing


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
