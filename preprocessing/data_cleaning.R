# preprocessing/data_cleaning.R
# Data preprocessing for retail dataset

# This function handles multiple preprocessing steps including:
# - Removing unnecessary columns
# - Cleaning IDs and categorical variables
# - Handling missing values with multiple imputation methods
# - Converting data types and formatting dates/times
# - Creating a DateTime column
# 
#' @param retail_data The raw retail dataset
#' @return A cleaned and preprocessed dataset

preprocess <- function(retail_data) {
  # Remove unnecessary columns
  retail_data <- retail_data |> select(-c(Address, Email, Phone, Year, Month, Total_Amount, Zipcode))
  
  # Clean Transaction_ID & Customer_ID
  retail_data <- retail_data |> mutate(
    Transaction_ID = as.character(Transaction_ID),
    Customer_ID = as.character(Customer_ID)
  ) |> drop_na(Transaction_ID, Customer_ID) |> distinct(Transaction_ID, .keep_all = TRUE) # Keep distinct
  
  # Clean City & State
  retail_data <- retail_data |> mutate(across(c(City, State), ~ na_if(trimws(.), "")))
  
  # Clean Age
  retail_data$Age <- as.integer(suppressWarnings(as.numeric(retail_data$Age)))
  retail_data$Age[retail_data$Age < 18 | retail_data$Age > 70] <- NA 
  
  # Clean Date
  retail_data$Date <- mdy(retail_data$Date)
  
  # Clean Time & Convert to HH:MM:SS
  retail_data$Time <- as_hms(as.POSIXct(retail_data$Time, format="%H:%M:%S"))
  
  # Clean up and rename Total_Purchases to Purchase_Quantity for clarity
  retail_data <- retail_data |> 
    rename(Purchase_Quantity = Total_Purchases) |> 
    mutate(Purchase_Quantity = as.numeric(Purchase_Quantity))
  
  # Clean Amount
  retail_data$Amount <- round(as.numeric(retail_data$Amount), 2)
  
  # Rename products to Products
  retail_data <- retail_data  |> rename(Products = products)
  retail_data$Products[retail_data$Products == ""] <- NA
  
  # Convert empty strings to NA and factorize them
  factor_cols_to_clean <- c("Gender", "Country", "Income", "Customer_Segment", "Product_Category", "Product_Brand", "Product_Type", "Feedback", "Shipping_Method", "Payment_Method", "Order_Status", "Ratings")
  
  retail_data <- retail_data |> mutate(across(all_of(factor_cols_to_clean), ~ na_if(., ""))) |> 
    mutate(across(all_of(factor_cols_to_clean), as.factor))
  
  # Set ordered levels for factor variables
  retail_data <- retail_data |>
    mutate(
      Income = factor(Income, levels = c("Low", "Medium", "High"), ordered = TRUE),
      Customer_Segment = factor(Customer_Segment, levels = c("New", "Regular", "Premium"), ordered = TRUE),
      Feedback = factor(Feedback, levels = c("Bad", "Average", "Good", "Excellent"), ordered = TRUE),
      Ratings = factor(Ratings, levels = c("Low", "High"), ordered = TRUE)
    )
  
  
  ####### Handling missing values #######
  ## Check for missing values
  na_counts <- colSums(is.na(retail_data))
  empty_counts <- colSums(retail_data == "")
  
  ## Print missing value summary
  print(na_counts)
  print(empty_counts)
  
  ### ─────────────────────────────────────────────
  ### Handle Missing Numerical Values (missForest)
  ### ─────────────────────────────────────────────
  # Set seed for reproducibility
  set.seed(123)
  
  # Select numerical columns only
  missForest_cols <- c("Age", "Purchase_Quantity", "Amount")
  
  # Subset only numerical columns
  retail_data_numeric <- retail_data[, missForest_cols]
  
  # Convert to data frame
  retail_numeric_df <- as.data.frame(retail_data_numeric)
  
  # Apply missForest imputation
  retail_imputed <- missForest(retail_numeric_df)
  
  # Replace imputed values in the original dataset
  retail_data <- retail_data |> 
    mutate(
      Age = as.integer(round(retail_imputed$ximp$Age)),
      Purchase_Quantity = round(retail_imputed$ximp$Purchase_Quantity, 0),
      Amount = round(retail_imputed$ximp$Amount, 2)
    )
  
  
  ### ─────────────────────────────────────────────
  ### Handle Categorical with MICE
  ### ─────────────────────────────────────────────
  # Subset data
  mice_cols <- c("Customer_Segment", "Shipping_Method", "Payment_Method")
  
  # Apply mice
  mice_imputed <- mice(retail_data[, mice_cols], method = 'pmm', m = 1, maxit = 5, seed = 123)
  
  # Complete the data
  retail_data[mice_cols] <- complete(mice_imputed)
  
  # View data
  View(retail_data)
  str(retail_data)
  summary(retail_data)
  
  
  ### ─────────────────────────────────────────────
  ### Gender Imputation via Name (gender package)
  ### ─────────────────────────────────────────────
  retail_data <- retail_data |> mutate(Name = na_if(Name, ""))
  
  # Extract first names from the Name column
  retail_data <- retail_data |> mutate(First_Name = word(Name, 1))
  
  # Perform gender inference using historical data (SSA method)
  gender_data <- gender(unique(retail_data$First_Name), method = "ssa", years = c(1980, 2000))
  
  # Keep relevant columns from gender_data
  gender_data <- gender_data |> select(name, gender) |> rename(First_Name = name, Inferred_Gender = gender)
  
  # Merge inferred gender back into the retail_data
  retail_data <- retail_data |> 
    left_join(gender_data, by = "First_Name") |> 
    mutate(
      # If original Gender is NA, infer using the historical data
      Gender = ifelse(is.na(Gender), str_to_title(Inferred_Gender), as.character(Gender)),
      Gender = as.factor(Gender)  # Ensure Gender is a factor
    ) |> 
    select(-First_Name, -Inferred_Gender, -Name)  # Clean up
  
  
  ### ─────────────────────────────────────────────
  ### KNN Imputation (VIM) for mixed columns
  ### ─────────────────────────────────────────────
  # Select category columns
  retail_data$Date <- as.numeric(as.Date(retail_data$Date))
  cat_cols <- c("Gender","Income", "Product_Category", "Product_Brand", "Product_Type", "Order_Status", "Products", "Date")
  
  # Apply KNN Imputation (k = 5)
  retail_data <- kNN(retail_data, variable = cat_cols, k=5)
  
  # Reflect in original dataset
  retail_data <- retail_data |> select(-ends_with("_imp"))
  
  # After imputation, convert numeric Date back to Date format
  retail_data$Date <- as.Date(retail_data$Date, origin = "1970-01-01")
  
  ### ─────────────────────────────────────────────
  ### Time Imputation (Linear Interpolation)
  ### ─────────────────────────────────────────────
  retail_data <- retail_data |>
    mutate(
      Time_seconds = as.numeric(as_hms(Time)),
      Time_seconds = zoo::na.approx(Time_seconds, na.rm = FALSE),
      Time = hms::as_hms(Time_seconds)
    ) |>
    select(-Time_seconds)  # Remove helper column
  
  
  ### ─────────────────────────────────────────────
  ### Combine Date and Time to DateTime
  ### ─────────────────────────────────────────────
  retail_data <- retail_data |>
    mutate(
      Date = as.Date(Date),
      Time = as_hms(Time),
      DateTime = as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%S", tz="UTC")
    ) |>
    select(-Time, -Date)
  
  
  ### ─────────────────────────────────────────────
  ### Mode Imputation (for Feedback, Ratings, Country, State, City, Gender)
  ### ─────────────────────────────────────────────
  mode_impute <- function(x) {
    ux <- unique(na.omit(x))
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  # Apply mode imputation [Feedback, Ratings, Country, State, City]
  retail_data$Feedback[is.na(retail_data$Feedback)] <- mode_impute(retail_data$Feedback)
  retail_data$Ratings[is.na(retail_data$Ratings)] <- mode_impute(retail_data$Ratings)
  retail_data$Country[is.na(retail_data$Country)] <- mode_impute(retail_data$Country)
  
  retail_data <- retail_data |>
    group_by(Country) |>
    mutate(State = ifelse(is.na(State), mode_impute(State), State)) |>
    ungroup()
  
  retail_data <- retail_data |>
    group_by(State) |>
    mutate(City = ifelse(is.na(City), mode_impute(City), City)) |>
    ungroup()
  
  # Assign the most common gender per CustomerID
  retail_data <- retail_data |> group_by(Customer_ID) |> mutate(Gender = mode_impute(Gender)) |> ungroup()
  
  
  ### ─────────────────────────────────────────────
  ### Final Structure Check
  ### ─────────────────────────────────────────────
  str(retail_data)
  summary(retail_data)
  
  return(retail_data)
}