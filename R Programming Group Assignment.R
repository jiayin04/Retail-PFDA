### ─────────────────────────────────────────────
### 1. Install and Load Packages
### ─────────────────────────────────────────────
# Install packages
install.packages(c("tidyr", "data.table", "dplyr", "ggplot2", "janitor", "missForest", "VIM", "zoo", "gender", "stringr", "mice"))

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
library(zoo)
library(gender)
library(stringr)
library(mice)

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
  ### 4. Handle Missing Numerical Values (missForest)
  ### ─────────────────────────────────────────────
  # Set seed for reproducibility
  set.seed(123)
  
  # Select numerical columns only
  missForest_cols <- c("Age", "Purchase_Quantity", "Amount")
  
  # Subset only numerical columns
  retail_data_numeric <- retail_data[, missForest_cols]
  
  # Check missing values before imputation
  print("Missing values before imputation:")
  print(colSums(is.na(retail_data_numeric)))
  
  # Convert to data frame
  retail_numeric_df <- as.data.frame(retail_data_numeric)
  
  # Apply missForest imputation
  retail_imputed <- missForest(retail_numeric_df)
  
  # Check missing values after imputation
  print("Missing values after imputation:")
  print(colSums(is.na(retail_imputed$ximp)))
  
  # Replace imputed values in the original dataset
  retail_data <- retail_data |> 
    mutate(
      Age = as.integer(round(retail_imputed$ximp$Age)),
      Purchase_Quantity = round(retail_imputed$ximp$Purchase_Quantity, 0),
      Amount = round(retail_imputed$ximp$Amount, 2)
    )
  
  # Verify the structure
  str(retail_data)
  View(retail_data)
  
  
  ### ─────────────────────────────────────────────
  ### 5. Handle Categorical with MICE
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
  ### 6. Gender Imputation via Name (gender package)
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
  
  # View the dataset
  View(retail_data)
  str(retail_data)
  summary(retail_data)
  
  ### ─────────────────────────────────────────────
  ### 7. KNN Imputation (VIM) for mixed columns
  ### ─────────────────────────────────────────────
  # Select category columns
  retail_data$Date <- as.numeric(as.Date(retail_data$Date))
  cat_cols <- c("Gender","Income", "Product_Category", "Product_Brand", "Product_Type", "Order_Status", "Products", "Date")
  
  # retail_data$Date <- as.numeric(as.Date(retail_data$Date, format = "%m-%d-%Y"))
  
  # Apply KNN Imputation (k = 5)
  retail_data <- kNN(retail_data, variable = cat_cols, k=5)
  
  # Reflect in original dataset
  retail_data <- retail_data |> select(-ends_with("_imp"))
  
  # After imputation, convert numeric Date back to Date format
  retail_data$Date <- as.Date(retail_data$Date, origin = "1970-01-01")
  
  
  ### ─────────────────────────────────────────────
  ### 8. Time Imputation (Linear Interpolation)
  ### ─────────────────────────────────────────────
  retail_data <- retail_data |>
    mutate(
      Time_seconds = as.numeric(as_hms(Time)),
      Time_seconds = zoo::na.approx(Time_seconds, na.rm = FALSE),
      Time = hms::as_hms(Time_seconds)
    ) |>
    select(-Time_seconds)  # Remove helper column
  
  
  ### ─────────────────────────────────────────────
  ### 9. Combine Date and Time to DateTime
  ### ─────────────────────────────────────────────
  retail_data <- retail_data |>
    mutate(
      Date = as.Date(Date),
      Time = as_hms(Time),
      DateTime = as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%S", tz="UTC")
    ) |>
    select(-Time, -Date)
  
  
  ### ─────────────────────────────────────────────
  ### 10. Mode Imputation (for Feedback, Ratings, Country, State, City, Gender)
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
  ### 11. Final Structure Check
  ### ─────────────────────────────────────────────
  str(retail_data)
  summary(retail_data)
  
  return(retail_data)
}

retail_data_proc <- preprocess(retail_full_data)

str(retail_data_proc)
summary(retail_data_proc)
View(retail_data_proc)

### ─────────────────────────────────────────────
### 12. How does product preference influence purchasing behavior and customer satisfaction?
### ─────────────────────────────────────────────
# i) Analysis technique - MANOVA
library(dplyr)
## a. Subset data for manova analysis
manova_data <- retail_data_proc |> select(Ratings, Purchase_Quantity, Product_Category) |> mutate(
  ## b. Make ratings numeric
  Ratings_Num = as.numeric(Ratings)
)

## c. Fit Manova model
manova_model <- manova(cbind(Purchase_Quantity, Ratings_Num) ~ Product_Category, data = manova_data)

## d. View Manova summary
summary(manova_model, test = "Pillai")

## e. Individual summary for each dependent variable
summary.aov(manova_model)

## ----------- Visualization ----------------##
# [Pairwise Relationship Visualization - Facet grid]
library(ggplot2)
library(MetBrewer)
library(dplyr)
library(tidyr)
library(patchwork)
library(reshape2)

# [Histogram]
# Style function
styleHistogram <- function() {
  list(
    theme_minimal(base_size = 12),
    theme(
      axis.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
  )
}

# Base histogram of Purchase Quantity
hist1 <- ggplot(manova_data, aes(x = Purchase_Quantity, fill = Product_Category)) +
  geom_histogram(bins = 20, alpha = 0.7, position = "dodge", color = "black") +
  scale_fill_met_d("Cassatt2") +
  labs(x = "Purchase Quantity", y = "Count") +
  styleHistogram() +
  theme(legend.position = "none")

# Base histogram of Ratings histogram
hist2 <- ggplot(manova_data, aes(x = Ratings_Num, fill = Product_Category)) +
  geom_histogram(binwidth = 1, alpha = 0.7, position = "dodge", color = "black") +
  scale_fill_met_d("Cassatt2") +
  labs(x = "Satisfaction Score", y = "Count", fill = "Product Category") +
  styleHistogram()

# Combine plots horizontally
combined_plot_hist <- styled_hist1 | styled_hist2

# Add global title, subtitle, and unified layout
final_hist_plot <- combined_plot_hist +
  plot_annotation(
    title = "Purchasing Behavior and Satisfaction Across Product Categories",
    subtitle = "Left: Purchase Quantity distribution | Right: Customer Ratings variation",
    caption = "Source: Retail Dataset",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 11, hjust = 0)
    )
  )

# Display the final plot
final_hist_plot


# [Violin + Boxplot]
violin_boxplot <- ggplot(manova_data, aes(x = Product_Category, y = Purchase_Quantity, fill = as.factor(Ratings))) +
  geom_violin(trim = FALSE, alpha = 0.5, color = NA, position = position_dodge(width = 0.9)) +
  geom_boxplot(width = 0.1, position = position_dodge(width = 0.9),
               outlier.shape = NA, color = "black", alpha = 0.8) +
  scale_fill_met_d("Cross") +
  labs(
    title = "Purchase Quantity Across Product Categories by Customer Ratings",
    subtitle = "Violin plots show distribution, while overlaid boxplots show quartiles and medians.",
    caption = "Source: Retail Dataset",
    x = "Product Category",
    y = "Purchase Quantity",
    fill = "Ratings"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0),
    plot.subtitle = element_text(size = 11, hjust = 0),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.box = "horizontal"
  )

violin_boxplot

# [Centroid Means Plot with Error Bars]
# Calculate means and standard errors based on Product Category
plot_data <- manova_data |>
  group_by(Product_Category) |>
  summarize(
    Purchase_Mean = mean(Purchase_Quantity),
    Purchase_SE = sd(Purchase_Quantity)/sqrt(n()),
    Rating_Mean = mean(Ratings_Num),
    Rating_SE = sd(Ratings_Num)/sqrt(n())
  ) |>
  tidyr::pivot_longer(
    cols = -Product_Category,
    names_to = c("Variable", ".value"),
    names_sep = "_"
  )

# Create the plot
cm_plot <- ggplot(plot_data, aes(x = Product_Category, y = Mean, fill = Variable)) +
  geom_col(position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = Mean - SE, ymax = Mean + SE),
    position = position_dodge(width = 0.9),
    width = 0.2
  ) +
  geom_text(
    aes(label = round(Mean, 2)),
    position = position_dodge(width = 0.9),
    vjust = -0.5, size = 3
  ) +
  scale_fill_met_d("Thomas") +
  labs(
    title = "Average Purchase Quantity and Ratings by Product Category",
    x = "Product Category",
    y = "Mean Value ± SE",
    fill = "Variables",
    caption = "Source: Retail Dataset"
  ) +
  theme_minimal()

cm_plot

### ─────────────────────────────────────────────
### 13. How does the co-purchasing behaviour across product categories influence customer ratings?
### ─────────────────────────────────────────────
## a. Descriptive Analysis of Customer Ratings by Product Category
library(dplyr)

# Function to create summary for either Product Category or Brand
create_summary <- function(data, group_by_col) {
  summary <- data |>
    group_by(!!sym(group_by_col), Ratings) |>
    summarize(
      rating_count = n(),
      total_purchase = sum(Amount * Purchase_Quantity) / 1000000,  # scaled down for readability
      .groups = 'drop'
    )
  
  # Calculate average age by group
  avg_age_summary <- data |>
    group_by(!!sym(group_by_col)) |>
    summarize(
      avg_age = mean(Age),
      .groups = 'drop'
    )
  
  # Merge the summaries together
  summary <- summary |>
    left_join(avg_age_summary, by = group_by_col)
  
  return(summary)
}

# summary
ratings_summary_cat <- create_summary(retail_data_proc, "Product_Category")
ratings_summary_brand <- create_summary(retail_data_proc, "Product_Brand")

summary(ratings_summary_cat)
summary(ratings_summary_brand)


## ----------- Visualization ----------------##
library(patchwork)
library(ggplot2)
library(MetBrewer)
# [Polar Chart]
# Function to plot the polar chart
plot_polar_chart <- function(summary_data, group_by_col) {
  # Rescale avg_age to match rating_count scale for better point visibility
  max_rating_count <- max(summary_data$rating_count)
  max_avg_age <- max(summary_data$avg_age)
  
  summary_data <- summary_data |>
    mutate(
      scaled_avg_age = avg_age / max_avg_age * max_rating_count  # rescale to same range
    )
  
  # Polar Chart Visualization
  ggplot(summary_data) + 
    # Bars representing rating count, fill color representing total purchase
    geom_col(
      aes(
        x = !!sym(group_by_col),
        y = rating_count,
        fill = total_purchase
      ),
      color = "white"
    ) +
    
    # Points representing average age (rescaled)
    geom_point(
      aes(
        x = !!sym(group_by_col),
        y = scaled_avg_age
      ),
      color = "black",
      size = 3
    ) +
    
    # Dotted lines for visual reference
    geom_segment(
      aes(
        x = !!sym(group_by_col),
        xend = !!sym(group_by_col),
        y = 0,
        yend = max_rating_count
      ),
      linetype = "dotted",
      color = "grey30"
    ) +
    
    # Polar coordinates for circular layout
    coord_polar() 
}

theme_setting <- function(plot, group_by_col) {
  palette_colors <- met.brewer("Cassatt1", 2)
  plot + 
    # Fill scale and plot labels
    scale_fill_gradient("Total Purchase\n (RM, x100k)", 
                        low = palette_colors[1], 
                        high = palette_colors[2],
                        guide = guide_colorbar(
                          title.position = "top",
                          title.hjust = 0.5,
                          barwidth = 10,
                          barheight = 0.6
                        )) +
    labs(
      x=group_by_col
    ) +
    # Minimal theme for cleaner look
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust = 1),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_line(color = "grey90"),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      axis.title = element_blank()
    )
}

# Category
cat_radar <- theme_setting(plot_polar_chart(ratings_summary_cat, "Product_Category"), "Product Category")
# Brand
brand_radar <- theme_setting(plot_polar_chart(ratings_summary_brand, "Product_Brand"), "Product Brand")

# final plot
combined_radar_plot <- (cat_radar | brand_radar) +
  plot_annotation(
    title = "Distribution of Customer Ratings Across Product Segment",
    subtitle = "Visual comparison of rating count, total purchase, and average customer age by category and brand",
    caption = "Source: Retail Dataset | Units scaled for readability"
  )

combined_radar_plot

## b. Market Basket Analysis[Co-purchasing Behavior] 
# Load necessary libraries
library(dplyr)
library(tidyr)
library(purrr)

# Check rating distribution
retail_data_proc |> count(Ratings)

# Create product pairs based only on Product_Category per customer and rating
product_pairs <- retail_data_proc |>
  group_by(Customer_ID, Ratings) |>
  summarise(Categories = list(Product_Category), .groups = "drop") |>
  filter(lengths(Categories) > 1) |>
  mutate(Pairs = map(Categories, ~combn(.x, 2, simplify = FALSE))) |>
  unnest(Pairs) |>
  mutate(pair = map_chr(Pairs, ~paste(sort(.x), collapse = " & ")))

# Count co-purchases separately by rating
pair_by_rating <- product_pairs |>
  group_by(Ratings, pair) |>
  count(name = "n", sort = TRUE) |>
  ungroup()

# Pivot to wide format for easier plotting
pair_by_rating_wide <- pair_by_rating |>
  pivot_wider(names_from = Ratings, values_from = n, values_fill = 0)

# Select top 15 pairs by total count (High + Low)
pair_by_rating_top <- pair_by_rating_wide |>
  mutate(Total = rowSums(across(where(is.numeric)))) |>
  arrange(desc(Total)) |>
  slice_head(n = 15)

print(pair_by_rating_top)

## ----------- Visualization ----------------##
library(ggplot2)
library(MetBrewer)
# [BAR PLOT]
bar_plot <- ggplot(pair_by_rating_top_long, aes(x = pair, y = Count, fill = Rating)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = Count), 
            position = position_dodge(width = 0.7), 
            hjust = -0.1, 
            size = 3.3, 
            color = "black") +
  coord_flip() +
  scale_fill_met_d(name = "Hokusai3") +
  labs(
    title = "Top Co-Purchased Product Category Pairs by Rating",
    subtitle = "Comparison of frequent co-purchases between High and Low rating customers",
    x = "Product Category Pair",
    y = "Co-Purchase Count",
    caption = "Source: Retail Dataset"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 13),
    axis.text.x = element_text(color = "grey20", size = 11),
    axis.text.y = element_text(color = "grey20", size = 10),
    legend.position = "bottom"
  ) +
  expand_limits(y = max(pair_by_rating_top_long$Count) * 1.15)

# [HEATMAP]
# Prepare symmetric heatmap input
pair_matrix <- pair_by_rating_top |>
  separate(pair, into = c("Product1", "Product2"), sep = " & ") |>
  pivot_longer(cols = c(High, Low), names_to = "Rating", values_to = "Count")

heatmap_plot <- ggplot(pair_matrix, aes(x = Product1, y = Product2, fill = Count)) +
  geom_tile(color = "white", linewidth = 0.4) +
  facet_wrap(~Rating) +
  scale_fill_met_c(name = "Monet") +
  labs(
    title = "Heatmap of Co-Purchase Counts by Product Category",
    subtitle = "Top 15 pairs shown, separated by customer rating group",
    x = "Product A",
    y = "Product B",
    fill = "Co-Purchase Count",
    caption = "Source: Retail Dataset"
  ) +
  guides(fill = guide_colorbar(
    title.position = "top",
    title.hjust = 0.5,
    barwidth = 20,
    barheight = 0.6
  )) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 15),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    strip.text = element_text(face = "bold", size = 13),
    legend.position = "bottom"
  )


### ─────────────────────────────────────────────
### 14. Can customer satisfaction be explained by a combination of brand preference and purchasing behavior across different product categories?
### ─────────────────────────────────────────────
# Load necessary libraries
library(randomForest)
library(xgboost)
library(dplyr)
library(caret)
library(pROC)

# Step 1: Prepare model data
model_data <- retail_data_proc |>
  mutate(
    Total_Purchase = Amount * Purchase_Quantity
  ) |>
  select(
    Ratings,
    Customer_ID,
    Product_Brand,
    Product_Category,
    Purchase_Quantity,
    Amount,
    Total_Purchase
  )

# Step 2: Exploratory Data Analysis of key relationships
# Check correlation of brand preferences, Purchase Quantity & Product Category
chisq.test(table(model_data$Product_Brand, model_data$Ratings))
chisq.test(table(model_data$Purchase_Quantity, model_data$Ratings))
chisq.test(table(model_data$Product_Category, model_data$Ratings))

# Step 3: Model Training and Evaluation
train_and_evaluate <- function(data, label = "Ratings") {
  # Split
  set.seed(123)
  train_idx <- createDataPartition(data[[label]], p = 0.8, list = FALSE)
  train_data <- data[train_idx, ]
  test_data <- data[-train_idx, ]
  
  # Calculate ratings distribution
  train_distribution <- table(train_data$Ratings)
  
  # Prepare numeric version for XGBoost
  prepare_numeric_data <- function(df) {
    df$Customer_ID_Num <- as.numeric(factor(df$Customer_ID))
    df$Product_Brand_Num <- as.numeric(factor(df$Product_Brand))
    df$Product_Category_Num <- as.numeric(factor(df$Product_Category))
    df <- df |> select(-Customer_ID, -Product_Brand, -Product_Category)
    return(df)
  }
  
  train_numeric <- prepare_numeric_data(train_data)
  test_numeric <- prepare_numeric_data(test_data)
  
  ###### Random Forest
  rf_model <- randomForest(Ratings ~ ., data = train_data, ntree = 100)
  rf_pred <- predict(rf_model, test_data)
  rf_conf <- confusionMatrix(rf_pred, test_data$Ratings)
  rf_prob <- predict(rf_model, test_data, type = "prob")[, "High"]
  rf_roc <- roc(test_data$Ratings, rf_prob)
  
  ###### XGBoost
  xgb_train <- xgb.DMatrix(data = as.matrix(train_numeric |> select(-Ratings)), label = as.numeric(train_data$Ratings) - 1)
  xgb_test <- xgb.DMatrix(data = as.matrix(test_numeric |> select(-Ratings)), label = as.numeric(test_data$Ratings) - 1)
  
  xgb_model <- xgb.train(
    params = list(
      objective = "binary:logistic", eval_metric = "auc",
      eta = 0.1, max_depth = 6, subsample = 0.8, colsample_bytree = 0.8
    ),
    data = xgb_train,
    nrounds = 100,
    verbose = 0
  )
  
  xgb_prob <- predict(xgb_model, xgb_test)
  xgb_pred <- factor(ifelse(xgb_prob > 0.5, "High", "Low"), levels = c("Low", "High"))
  xgb_conf <- confusionMatrix(xgb_pred, test_data$Ratings)
  xgb_roc <- roc(test_data$Ratings, xgb_prob)
  
  return(list(
    rf = list(model = rf_model, roc = rf_roc, conf = rf_conf, auc = auc(rf_roc), predictions  = rf_pred, confusion_matrix=rf_conf),
    xgb = list(model = xgb_model, roc = xgb_roc, conf = xgb_conf, auc = auc(xgb_roc), predictions = xgb_pred, confusion_matrix = xgb_conf),
    train_distribution = train_distribution
  ))
}


# Imbalanced
imbalanced_results <- train_and_evaluate(model_data)

# Balanced
set.seed(123)
balanced_data <- downSample(x = model_data[, -1], y = model_data$Ratings, yname = "Ratings")
balanced_results <- train_and_evaluate(balanced_data)

# View Ratings Distribution
print(imbalanced_results$train_distribution)
print(balanced_results$train_distribution)

# Step 4: Access Confusion Matrix
print(balanced_results$rf$confusion_matrix)
print(balanced_results$xgb$confusion_matrix)
print(imbalanced_results$rf$confusion_matrix)
print(imbalanced_results$xgb$confusion_matrix)


# --------------------- Visualization --------------------------
library(ggplot2)
# Step 5: Visualize ROC values
# [Random Forest ROC plot]
plot(imbalanced_results$rf$roc, col = "blue", lwd = 2,
     main = "Random Forest ROC Curve (Balanced vs Imbalanced)")
lines(balanced_results$rf$roc, col = "red", lty = 2, lwd = 2)
text(0.7, 0.2, paste("Imbalanced AUC = ", round(auc(imbalanced_results$rf$roc), 4)), col="black")
text(0.65, 0.65, paste("Balanced AUC = ", round(auc(balanced_results$rf$roc), 4)), col="black")
legend("bottomright",
       legend = c("RF Imbalanced", "RF Balanced"),
       col = c("blue", "red"),
       lty = c(1, 2),
       lwd = 2)

# [XGBoost ROC plot]
plot(imbalanced_results$xgb$roc, col = "blue", lwd = 2,
     main = "XGBoost ROC Curve (Balanced vs Imbalanced)")
lines(balanced_results$xgb$roc, col = "red", lty = 2, lwd = 2)
text(0.7, 0.2, paste("Imbalanced AUC = ", round(auc(imbalanced_results$xgb$roc), 4)), col="black")
text(0.65, 0.65, paste("Balanced AUC = ", round(auc(balanced_results$xgb$roc), 4)), col="black")
legend("bottomright",
       legend = c("XGB Imbalanced", "XGB Balanced"),
       col = c("blue", "red"),
       lty = c(1, 2),
       lwd = 2)


# Step 6: Visualize Feature Importance
bar_theme_setting <- function(plot, fill_label = "Data Type", show_legend = TRUE) {
  palette_colors <- met.brewer("Cassatt1", 2)
  
  plot <- plot +
    scale_fill_manual(
      name = fill_label,
      values = c("Imbalanced" = palette_colors[1], "Balanced" = palette_colors[2])
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      axis.ticks = element_blank(),
      panel.grid = element_line(color = "grey90"),
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      axis.title = element_text(size = 11)
    )
  
  if (show_legend) {
    plot <- plot + theme(legend.position = "bottom")
  } else {
    plot <- plot + theme(legend.position = "none")
  }
  
  return(plot)
}


# RANDOM FOREST feature importance
rf_imp_imb <- importance(imbalanced_results$rf$model)
rf_imp_bal <- importance(balanced_results$rf$model)

# Convert to dataframe
rf_imp_df_imb <- data.frame(Feature = rownames(rf_imp_imb),
                            Importance = rf_imp_imb[, "MeanDecreaseGini"],
                            Type = "Imbalanced")

rf_imp_df_bal <- data.frame(Feature = rownames(rf_imp_bal),
                            Importance = rf_imp_bal[, "MeanDecreaseGini"],
                            Type = "Balanced")

rf_imp_df <- rbind(rf_imp_df_imb, rf_imp_df_bal)

# Plot
rf_imp_plot <- bar_theme_setting(
  ggplot(rf_imp_df, aes(x = reorder(Feature, Importance), y = Importance, fill = Type)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    labs(
      x = "Features", 
      y = "Mean Decrease Gini"
      ),
  show_legend = FALSE
)


# XGBoost importance
# Imbalanced importance
xgb_imp_imb <- xgb.importance(model = imbalanced_results$xgb$model)
xgb_imp_imb$Type <- "Imbalanced"

# Balanced importance
xgb_imp_bal <- xgb.importance(model = balanced_results$xgb$model)
xgb_imp_bal$Type <- "Balanced"

# plot
xgb_imp_df <- rbind(xgb_imp_imb, xgb_imp_bal)

xgb_imp_plot <- bar_theme_setting(
  ggplot(xgb_imp_df, aes(x = reorder(Feature, Gain), y = Gain, fill = Type)) +
    geom_bar(stat = "identity", position = "dodge") +
    coord_flip() +
    labs(
      x = "Features",
      y = "Gain"
      ),
  show_legend = TRUE
  )


combined_imp_plot <- (rf_imp_plot | xgb_imp_plot) +
  plot_layout(guides = "collect") + 
  plot_annotation(
    title = "Random Forest Vs XGBoost Feature Importance",
    subtitle = "Left: Random Forest Feature Importance | Right: XGBoost Feature Importance",
    caption = "Source: Retail Dataset | Balanced & Imbalanced Ratings Features"
  ) &
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11)
  )

print(combined_imp_plot)



### ─────────────────────────────────────────────
### 15. Are there clusters of customers who show consistent brand preferences, high purchase behavior, and high satisfaction?
### ─────────────────────────────────────────────
# i) Analysis technique - Unsupervised Learning (Clustering)
# a) Load required libraries
library(dplyr)
library(caret)

## b) Prepare Data
customer_cluster_data <- retail_data_proc |>
  select(Ratings, Product_Brand, Purchase_Quantity, Product_Category)

## c) Data preprocessing
# Convert factors to numeric for clustering
customer_cluster_data$Product_Brand_num <- as.numeric(customer_cluster_data$Product_Brand)
customer_cluster_data$Product_Category_num <- as.numeric(customer_cluster_data$Product_Category)

# Scale the numeric variables for better clustering
customer_cluster_data_scaled <- scale(customer_cluster_data[, c("Product_Brand_num", "Product_Category_num", "Purchase_Quantity")])

## d) K-means Clustering
# Elbow method to find the optimal number of clusters
wss <- (nrow(customer_cluster_data_scaled) - 1) * sum(apply(customer_cluster_data_scaled, 2, var))
for (i in 1:15) {
  kmeans_result <- kmeans(customer_cluster_data_scaled, centers = i)
  wss[i] <- kmeans_result$tot.withinss
}

## e) Perform K-means with the selected number of clusters (K=3) [Based on Elbow Method]
kmeans_result <- kmeans(customer_cluster_data_scaled, centers = 3)
customer_cluster_data$cluster <- as.factor(kmeans_result$cluster)


## f) Cluster Profiling (Summary)
cluster_summary <- customer_cluster_data |>
  group_by(cluster) |>
  summarise(
    avg_purchase = mean(Purchase_Quantity),
    avg_rating = mean(as.numeric(Ratings)),
    most_common_brand = names(sort(table(Product_Brand), decreasing = TRUE))[1]
  )

# Print the cluster summary
print(cluster_summary)


## ----------- Visualization ----------------##
library(ggplot2)
library(plotly)
library(MetBrewer)
library(dendextend)
# [Plot the Elbow Method]
elbow_data <- data.frame(K = 1:15, WSS = wss)
elbow_method_plot <- ggplot(elbow_data, aes(K, WSS)) +
  geom_point(size = 3, color = "#2C3E50") +
  geom_line() +
  labs(title = "Elbow Method for Optimal Number of Clusters",
       x = "Number of Clusters (K)", 
       y = "Total within-cluster sum of squares",
       caption = "Source: Retail Dataset"
       ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
print(elbow_method_plot)


# [3D plot] - see the clusters based on Product Brand, Product Category, Purchase Quantity, and Ratings
## Perform PCA for Dimensional Reduction
pca_result <- prcomp(customer_cluster_data_scaled)
pca_data <- data.frame(pca_result$x)

plot_ly(
  pca_data,
  x = ~PC1, y = ~PC2, z = ~PC3,
  type = "scatter3d",
  mode = "markers",
  color = as.factor(customer_cluster_data$cluster),
  symbol = as.factor(customer_cluster_data$Product_Category),
  colors = c("#F1C40F", "#E74C3C", "#2ECC71"),
  marker = list(opacity = 0.8)
) |>
  layout(
    title = list(
      text = "3D PCA Plot of Customer Clusters<br><sub>3 different clusters segregated in each category</sub>",
      x = 0.05
    ),
    legend = list(
      title = list(text = 'Cluster')
    ),
    scene = list(
      xaxis = list(title = 'PC1'),
      yaxis = list(title = 'PC2'),
      zaxis = list(title = 'PC3')
    ),
    margin = list(l = 0, r = 0, b = 0, t = 40)
  )


# [Barplot]
# Create a numeric version of Ratings for averaging (Low = 1, High = 2)
customer_cluster_data$Rating_Num <- ifelse(customer_cluster_data$Ratings == "High", 2, 1)

# Summarize by cluster and brand
brand_cluster_summary <- customer_cluster_data |>
  group_by(cluster, Product_Brand) |>
  summarise(
    Avg_Purchase = mean(Purchase_Quantity),
    Avg_Rating = mean(Rating_Num),
    .groups = 'drop'
  )

# Average Purchase Quantity colored by Avg Rating (satisfaction)
palette_colors <- met.brewer("Cassatt1")
ggplot(brand_cluster_summary, aes(x = factor(Product_Brand), y = Avg_Purchase, fill = Avg_Rating)) +
  geom_col(color = "black") +
  facet_wrap(~ cluster, scales = "free_y") +
  labs(
    title = "Brand Preferences, Purchase Behavior, and Satisfaction by Cluster",
    subtitle = "Higher bars = more purchases; Deeper fill = higher satisfaction",
    x = "Product Brand",
    y = "Average Purchase Quantity",
    fill = "Avg Rating",
    caption = "Source: Retail dataset"
  ) +
  scale_fill_gradient("Average Rating",
                      low = palette_colors[4],
                      high = palette_colors[6],
                      guide = guide_colorbar(
                        title.position = "top",
                        title.hjust = 0.5,
                        barwidth = 10,
                        barheight = 0.6
                      )) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5),
    legend.position = "bottom"
  )

### [Hierarchical Clustering]
set.seed(123)
# Create a smaller sample
sample_indices <- sample(1:nrow(customer_cluster_data_scaled), 40)
sample_data <- customer_cluster_data_scaled[sample_indices, ]
dist_matrix <- dist(sample_data)
hc <- hclust(dist_matrix)

# Convert to dendrogram
dend <- as.dendrogram(hc)

# Assign labels
labels(dend) <- customer_cluster_data$Product_Brand[sample_indices]

# Color branches by cluster
dend_colored <- color_branches(dend, k = 6)

# Plot horizontally for better readability
plot(dend_colored,
     main = "Clustered Customers by Brand & Cluster (Sampled)",
     horiz = TRUE,
     cex.main = 1.2)


# Add vertical dotted lines and annotate intersection points
for (h in c(0.5, 1, 1.5, 2, 2.5)) {
  abline(v = h, col = "grey60", lty = 2)
}

# Add baseline at y =0
abline(v=0, col="black", lwd= 1)

# ------------------ GROUP --------------------
# Random Forest Prediction
# Load necessary libraries
library(randomForest)
library(caret)
library(ggplot2)
library(dplyr)
library(viridis)

overall_model_data <- retail_data_proc |>
  # Select relevant columns for our model
  select(Ratings, City, State, Country, Age, Gender, Income, Customer_Segment, Purchase_Quantity, Amount, Product_Category, Product_Brand, Product_Type, Feedback, Payment_Method, Shipping_Method, Order_Status, Products, DateTime)

# Split data into training and testing sets (80/20 split)
set.seed(123) # For reproducibility
overall_train_index <- createDataPartition(overall_model_data$Ratings, p = 0.8, list = FALSE)
overall_train_data <- overall_model_data[train_index, ]
overall_test_data <- overall_model_data[-train_index, ]

# Build Random Forest model
overall_rf_model <- randomForest(
  Ratings ~ ., 
  data = overall_train_data,
  ntree = 100,       # Number of trees
  importance = TRUE  # Calculate variable importance
)

# Print model summary
print(overall_rf_model)

# Evaluate model performance
# Make predictions on test data
overall_predictions <- predict(rf_model, overall_test_data)

# Create confusion matrix
overall_conf_matrix <- confusionMatrix(overall_predictions, overall_test_data$Ratings)
print(overall_conf_matrix)

# Variable importance
overall_var_importance <- importance(rf_model)
print(overall_var_importance)

# Plot variable importance
varImpPlot(overall_rf_model, main = "Variable Importance")

# Plot variable importance using ggplot2 for better visualization
overall_importance_df <- as.data.frame(importance(rf_model))
overall_importance_df$Variable <- rownames(overall_importance_df)

# Sort by MeanDecreaseGini
overall_importance_df <- overall_importance_df |>
  arrange(desc(MeanDecreaseGini))

# Plot variable importance
ggplot(overall_importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = viridis(1)) +
  coord_flip() +
  labs(title = "Variable Importance in Random Forest Model",
       x = "Variables",
       y = "Mean Decrease in Gini Index") +
  theme_minimal()
