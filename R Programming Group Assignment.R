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
library(readr)
library(lubridate)

# Set working directory
WD <- "~/Custom Office Templates/Academic/Degree/Year 2 Sem 1/PfDA/Assignment/R Source Code" # Change to your WD
if (dir.exists(WD)) {
  setwd(WD)
} else {
  stop("Working directory does not exist!")
}

# Import dataset
file_path <- file.path(WD, "retail_data.csv")
retail_data <- read_csv(file_path)
View(retail_data)

# Remove unnecessary columns
retail_data <- retail_data |> select(-c(Address, Email, Phone, Name, Year, Month, Total_Amount, Zipcode))

# Clean Transaction_ID
retail_data <- retail_data |> mutate(Transaction_ID = as.character(Transaction_ID))

# Clean Customer_ID
retail_data <- retail_data |> mutate(Customer_ID = as.character(Customer_ID))

# Clean City
retail_data$City <- ifelse(trimws(retail_data$City) == "", NA, trimws(retail_data$City))

# Clean State
retail_data$State <- ifelse(trimws(retail_data$State) == "", NA, trimws(retail_data$State))

# Clean Age
retail_data$Age <- as.integer(suppressWarnings(as.numeric(retail_data$Age)))
retail_data$Age[retail_data$Age < 18 | retail_data$Age > 70] <- NA 

# Clean Date
retail_data$Date <- mdy(retail_data$Date)

# Clean Time & Convert to HH:MM:SS
retail_data$Time <- as_hms(as.POSIXct(retail_data$Time, format="%H:%M:%S")) 

# Clean up and rename Total_Purchases to Purchase_Quantity for clarity
retail_data <- retail_data |> rename(Purchase_Quantity = Total_Purchases)
retail_data$Purchase_Quantity <- as.numeric(retail_data$Purchase_Quantity)

# Clean Amount
retail_data$Amount <- round(as.numeric(retail_data$Amount), 2)

# Rename products to Products
retail_data <- retail_data  |> rename(Products = products)
retail_data$Products[retail_data$Products == ""] <- NA

# Convert empty strings to NA and factorize them
factor_cols_to_clean <- c("Gender", "Country", "Income", "Customer_Segment", "Product_Category", "Product_Brand", "Product_Type", "Feedback", "Shipping_Method", "Payment_Method", "Order_Status", "Ratings")

retail_data <- retail_data |> mutate(across(all_of(factor_cols_to_clean), ~ na_if(., ""))) |> 
  mutate(across(all_of(factor_cols_to_clean), as.factor))

# retail_data[factor_cols_to_clean] <- lapply(retail_data[factor_cols_to_clean], function(x) {
#   x[x == ""] <- NA  # Replace empty strings with NA
#   as.factor(x)      # Convert to factor
# })


### Handling missing values
## Check for missing values
na_counts <- colSums(is.na(retail_data))
empty_counts <- colSums(retail_data == "")

## Print missing value summary
print(na_counts)
print(empty_counts)


## Remove rows without Transaction_ID
retail_data <- retail_data |> drop_na(Transaction_ID)
retail_data <- retail_data |> distinct(Transaction_ID, .keep_all = TRUE) # Keep distinct


## MissForest Imputation (Got issues)
# Select numerical columns only
missForest_cols <- c("Age", "Purchase_Quantity", "Amount")

# Subset only numerical columns
retail_data_numeric <- retail_data[, missForest_cols]
na_counts <- colSums(is.na(retail_data_numeric))
print(na_counts)
# For reproducibility
set.seed(123)
retail_data_numeric_imputed <- missForest(retail_data_numeric)

# Replace original dataset with imputed values
clean_retail_data[, missForest_cols] <- retail_data_numeric_imputed$ximp

# Check dataset to make sure no missing rows
plot_missing(clean_retail_data)
summary(clean_retail_data)



## KNN Imputation
# Select category columns
cat_cols <- c("Gender", "Income", "Customer_Segment", "Product_Category", "Product_Brand", "Product_Type", "Shipping_Method", "Payment_Method", "Order_Status", "Products")

# Apply KNN Imputation (k = 5)
retail_data <- kNN(retail_data, variable = cat_cols, k=5)

# Reflect in original dataset
retail_data <- retail_data |> select(-ends_with("_imp"))



## Mode Imputation
mode_impute <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# Apply mode imputation
retail_data$Feedback[is.na(retail_data$Feedback)] <- mode_impute(retail_data$Feedback)
retail_data$Ratings[is.na(retail_data$Ratings)] <- mode_impute(retail_data$Ratings)

## Customer ID, City, State, Country, Date & Time




# Validate dataset to make sure it had cleaned
str(retail_data)
summary(retail_data)
