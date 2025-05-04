### ─────────────────────────────────────────────
### 1. Install and Load Packages
### ─────────────────────────────────────────────
# Install packages
install.packages(c("tidyr", "data.table", "dplyr", "ggplot2", "janitor", "missForest", "VIM", "zoo", "gender", "stringr", "mice", "GGally"))

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
library(GGally)

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
library(factoextra)
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
# Base histogram for Purchase Quantity
hist1 <- ggplot(manova_data, aes(x = Purchase_Quantity, fill = Product_Category)) +
  geom_histogram(bins = 20, alpha = 0.7, position = "dodge", color = "black") +
  scale_fill_met_d("Cassatt2") +
  labs(x = "Purchase Quantity", y = "Count") +
  theme_minimal()

# Base histogram for Customer Ratings
hist2 <- ggplot(manova_data, aes(x = Ratings_Num, fill = Product_Category)) +
  geom_histogram(bins = 5, alpha = 0.7, position = "dodge", color = "black") +
  scale_fill_met_d("Cassatt2") +
  labs(x = "Satisfaction Score", y = "Count", fill = "Product Category") +
  theme_minimal()

# Styling function to add bars
styleHistogram <- function(p) {
  p + 
    theme(
      axis.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
}

# Apply styling and manage legends
styled_hist1 <- styleHistogram(hist1) +
  theme(legend.position = "none")

styled_hist2 <- styleHistogram(hist2)

# Combine plots horizontally
combined_plot_hist <- styled_hist1 | styled_hist2

# Add global title, subtitle, and unified layout
final_hist_plot <- combined_plot_hist +
  plot_annotation(
    title = "Customer Behavior and Satisfaction Across Product Categories",
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

# [Density Plot]
density_plot <- ggplot(manova_data, aes(x = Purchase_Quantity, fill = as.factor(Ratings_Num))) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Product_Category) +
  labs(
    title = "Density Distribution of Purchase Quantity by Product Category",
    caption = "Source: Retail Dataset",
    x = "Purchase Quantity",
    y = "Density",
    fill = "Ratings"
  ) +
  scale_fill_met_d("Cross") + 
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0), 
    plot.subtitle = element_text(size = 11, hjust = 0), 
    axis.title = element_text(face = "bold"), 
    legend.position = "bottom",
    legend.box = "horizontal", 
    strip.text = element_text(face = "bold", size = 13),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.spacing = unit(1, "lines")
  )


# [Centroid Means Plot with Error Bars]
# Compute group means based on Product_Category
manova_plot_data <- manova_data |> 
  group_by(Product_Category) |> 
  summarise(
    Purchase_Quantity = mean(Purchase_Quantity, na.rm = TRUE),
    Ratings = mean(as.numeric(Ratings), na.rm = TRUE)  # Convert ordered factor to numeric
  )

# Melt data to long format
manova_melted <- melt(manova_plot_data, id.vars = "Product_Category")

# Plot
meanPlot_errorBars <- ggplot(manova_melted, aes(x = as.factor(Product_Category), y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_met_d("Thomas") +
  geom_text(aes(label = round(value, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3) +
  labs(
    title = "Mean Purchase Quantity and Ratings by Product Category",
    caption = "Source: Retail Dataset",
    x = "Product Category (Product Preference)",
    y = "Mean Value",
    fill = "Variable"
  ) +
  theme_minimal()


### ─────────────────────────────────────────────
### 13. How does the co-purchasing behaviour across product categories influence customer ratings?
### ─────────────────────────────────────────────
## a. Descriptive Analysis of Customer Ratings by Product Category
library(patchwork)
library(dplyr)
library(ggplot2)
library(MetBrewer)

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
      avg_age = mean(Age, na.rm = TRUE),
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


(cat_radar | brand_radar) +
  plot_annotation(
    title = "Distribution of Customer Ratings Across Product Segment",
    subtitle = "Visual comparison of rating count, total purchase, and average customer age by category and brand",
    caption = "Source: Retail Dataset | Units scaled for readability"
  )


## b. Market Basket Analysis[Co-purchasing Behavior] 
# Load necessary libraries
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

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

## ----------- Visualization ----------------##
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
### 13. Can customer satisfaction be explained by a combination of brand preference and purchasing behavior across different product categories?
### ─────────────────────────────────────────────
# Load required libraries
library(dplyr)
library(tidyr)
library(caret)
library(xgboost)
library(pROC)
library(ggplot2)
library(smotefamily)
library(MetBrewer)

# Setting seed for reproducibility
set.seed(123)

# Step 1: Feature engineering focused on brand preference and purchase behavior
# Create a comprehensive feature set
enhanced_data <- retail_data_proc |>
  # Group by customer to create customer-level features
  group_by(Customer_ID) |>
  mutate(
    # a. Brand loyalty features
    Total_Customer_Purchases = n(),
    Brand_Purchase_Count = sum(Product_Brand == first(Product_Brand)),
    Brand_Loyalty_Score = Brand_Purchase_Count / Total_Customer_Purchases,
    
    # b. Category preference
    Category_Purchase_Count = sum(Product_Category == first(Product_Category)),
    Category_Preference_Score = Category_Purchase_Count / Total_Customer_Purchases,
    
    # c. Price sensitivity
    Avg_Amount_Per_Purchase = mean(Amount, na.rm = TRUE),
    Purchase_Amount_Ratio = ifelse(Avg_Amount_Per_Purchase > 0, 
                                   Amount / Avg_Amount_Per_Purchase, 
                                   NA),
    
    # d. Purchase quantity patterns
    Quantity_Mod2 = Purchase_Quantity %% 2,  # Capture even/odd pattern
    Quantity_Mod5 = Purchase_Quantity %% 5,  # Capture 5-unit pattern
    Quantity_Sin = sin(Purchase_Quantity * pi/2.5),  # Wave pattern
    Quantity_Cos = cos(Purchase_Quantity * pi/2.5)   # Wave pattern
  ) |>
  ungroup() |>
  
  # e. Brand-Category interaction
  mutate(
    Brand_Category_Combo = paste(Product_Brand, Product_Category, sep="_")
  ) 

# Select final features for modeling
model_data <- enhanced_data %>%
  select(
    # Target variable
    Ratings,
    
    # Brand preference features
    Product_Brand,
    Brand_Loyalty_Score,
    
    # Purchase behavior features
    Purchase_Quantity,
    Quantity_Mod2,
    Quantity_Mod5,
    Quantity_Sin,
    Quantity_Cos,
    
    # Category features
    Product_Category,
    Category_Preference_Score,
    
    # Interaction features
    Brand_Category_Combo,
    
    # Customer features
    Purchase_Amount_Ratio
  )

# Convert categorical variables to factors
model_data$Ratings <- factor(model_data$Ratings, levels = c("Low", "High"))
model_data$Product_Brand <- as.factor(model_data$Product_Brand)
model_data$Product_Category <- as.factor(model_data$Product_Category)
model_data$Brand_Category_Combo <- as.factor(model_data$Brand_Category_Combo)


# Step 2: Exploratory Data Analysis of key relationships

# a. Brand preference vs. Ratings
brand_ratings <- model_data |>
  group_by(Product_Brand, Ratings) |>
  summarise(count = n(), .groups = 'drop') |>
  group_by(Product_Brand) |>
  mutate(percentage = count / sum(count) * 100)

# [Bar chart] Plot brand ratings
brand_ratings_plot <- ggplot(brand_ratings, aes(x = Product_Brand, y = percentage, fill = Ratings)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Customer Satisfaction by Product Brand", 
       subtitle = "Ratings distribution by brand (in %)",
       x = "Product Brand", 
       y = "Percentage (%)", 
       fill = "Customer Rating") +
  theme_minimal(base_size = 13) +
  scale_fill_met_d("Derain") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

# Check correlation of brand preferences
chisq.test(table(model_data$Product_Brand, model_data$Ratings))

# b. Purchase quantity pattern vs. Ratings
quantity_ratings <- model_data |>
  group_by(Purchase_Quantity, Ratings) |>
  summarise(count = n(), .groups = 'drop')

# [Line Graph] Plot quantity vs ratings
quantity_ratings_plot <- ggplot(quantity_ratings, aes(x = Purchase_Quantity, y = count, color = Ratings)) +
  geom_line(linewidth=1.2) +
  scale_color_met_d("Derain") +
  labs(
    title = "Purchase Quantity Patterns by Customer Rating",
    subtitle = "Line chart of rating frequency per quantity ordered",
    x = "Purchase Quantity",
    y = "Number of Orders",
    color = "Customer Rating"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

# c. [Density Plot] Brand loyalty vs. Ratings
brand_loyalty_plot <- ggplot(model_data, aes(x = Brand_Loyalty_Score, fill = Ratings)) +
  geom_density(alpha = 0.5) +
  scale_fill_met_d("Java") +
  labs(
    title = "Distribution of Brand Loyalty Scores by Rating",
    subtitle = "Smoothed density plots grouped by customer rating",
    x = "Brand Loyalty Score",
    y = "Density",
    fill = "Customer Rating"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

# Check correlation of brand loyalty
t.test(Brand_Loyalty_Score ~ Ratings, data = model_data)

# Step 3: Train-Test Split
train_index <- createDataPartition(model_data$Ratings, p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Check class balance
table(train_data$Ratings)

# Step 4: Prepare Data for XGBoost
# Function to convert categorical variables to numeric for XGBoost
prepare_numeric_data <- function(data) {
  result <- data
  # Create copies of factors as numerics
  if("Product_Brand" %in% colnames(result) && is.factor(result$Product_Brand)) {
    result$Product_Brand_Num <- as.numeric(result$Product_Brand)
  }
  if("Product_Category" %in% colnames(result) && is.factor(result$Product_Category)) {
    result$Product_Category_Num <- as.numeric(result$Product_Category)
  }
  if("Brand_Category_Combo" %in% colnames(result) && is.factor(result$Brand_Category_Combo)) {
    result$Brand_Category_Combo_Num <- as.numeric(result$Brand_Category_Combo)
  }
  
  # Remove original factor columns for numeric processing
  result <- result |>
    select(-Product_Brand, -Product_Category, -Brand_Category_Combo)
  
  return(result)
}

# Prepare numeric version of the training data
train_numeric <- prepare_numeric_data(train_data)

# Apply SMOTE to handle class imbalance
smote_result <- SMOTE(
  X = train_numeric |> select(-Ratings),
  target = train_numeric$Ratings,
  K = 5,
  dup_size = 1
)

# Create balanced dataset
balanced_data <- smote_result$data
balanced_data$class <- as.factor(balanced_data$class)
names(balanced_data)[names(balanced_data) == "class"] <- "Ratings"

# Check class balance
table(balanced_data$Ratings)

# Prepare XGBoost matrices
xgb_train <- xgb.DMatrix(
  data = as.matrix(balanced_data |> select(-Ratings)),
  label = ifelse(balanced_data$Ratings == "High", 1, 0)
)

# Prepare test data
test_numeric <- prepare_numeric_data(test_data)
xgb_test <- xgb.DMatrix(
  data = as.matrix(test_numeric |> select(-Ratings)),
  label = ifelse(test_numeric$Ratings == "High", 1, 0)
)

# Step 5: Train XGBoost Model
# Set XGBoost parameters
xgb_params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  eta = 0.1,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8
)

# Train the model
xgb_model <- xgb.train(
  params = xgb_params,
  data = xgb_train,
  nrounds = 100,
  watchlist = list(train = xgb_train, test = xgb_test),
  verbose = 1,
  early_stopping_rounds = 10
)

# Step 6: Model Evaluation
# Make predictions
xgb_prob <- predict(xgb_model, xgb_test)
xgb_pred <- ifelse(xgb_prob > 0.5, "High", "Low") |> factor(levels = c("Low", "High"))

# Calculate confusion matrix
conf <- confusionMatrix(xgb_pred, test_data$Ratings)
print(conf)

# Calculate ROC and AUC
roc_obj <- roc(test_data$Ratings, xgb_prob)
auc_val <- auc(roc_obj)
cat("AUC:", round(auc_val, 4), "\n\n")

# Plot [ROC curve]
plot(roc_obj, main = "XGBoost ROC Curve", col = "blue")
abline(a = 0, b = 1, lty = 2, col = "gray")

# Step 7: Feature Importance Analysis
# Get feature importance from XGBoost
xgb_importance <- xgb.importance(model = xgb_model)
print(xgb_importance)

# [Bar Chart] Customized feature importance plot
importance_df <- as.data.frame(xgb_importance)
ggplot(importance_df[1:10, ], aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_bar(stat = "identity", fill = "steelblue", width = 0.7) +
  coord_flip() +
  labs(
    title = "Top 10 Features by Importance (XGBoost)",
    x = "Feature",
    y = "Importance (Gain)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.major.y = element_blank(),  # Remove horizontal grid lines
    panel.grid.minor = element_blank()
  )


# Step 8: Direct Analysis of Brand Preference and Purchase Behavior
# Calculate correlation between brand loyalty and rating
brand_loyalty_effect <- train_data |>
  group_by(Product_Brand) |>
  summarize(
    Avg_Loyalty_Score = mean(Brand_Loyalty_Score, na.rm = TRUE),
    High_Rating_Pct = sum(Ratings == "High", na.rm = TRUE) / n() * 100,
    Count = n()
  ) |>
  arrange(desc(High_Rating_Pct))

# Analysis of purchase quantity patterns across categories and ratings
quantity_pattern <- train_data |>
  group_by(Product_Category, Purchase_Quantity, Ratings) |>
  summarize(Count = n(), .groups = 'drop') |>
  group_by(Product_Category, Purchase_Quantity) |>
  mutate(High_Rating_Pct = sum(Ratings == "High" & Count > 0) / sum(Count) * 100)

# [Bubble Chart] Visualize brand loyalty vs. satisfaction
brand_loyalty_plot <- ggplot(brand_loyalty_effect, 
                             aes(x = Avg_Loyalty_Score, 
                                 y = High_Rating_Pct, 
                                 size = Count, 
                                 color = Product_Brand)) +
  geom_point(alpha = 0.7) +
  scale_size_continuous(range = c(3, 12)) +  # Adjust bubble sizes
  labs(
    title = "Brand Loyalty vs Customer Satisfaction",
    x = "Average Brand Loyalty Score", 
    y = "percentage of High Ratings(%)",
    size = "Customer Count",
    color = "Brand"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )


### ─────────────────────────────────────────────
### 15. Are there clusters of customers who show consistent brand preferences, high purchase behavior, and high satisfaction?
### ─────────────────────────────────────────────
# i) Analysis technique - Unsupervised Learning (Clustering)
# a) Load required libraries
library(dplyr)
library(caret)
library(ggplot2)
library(plotly)
library(dendextend)

## b) Prepare Data
customer_cluster_data <- retail_data_proc %>%
  select(Ratings, Product_Brand, Purchase_Quantity, Product_Category) %>%
  mutate(Ratings = factor(Ratings, levels = c("Low", "High")))

## c) Store original labels
product_brand_labels <- levels(factor(customer_cluster_data$Product_Brand))
product_category_labels <- levels(factor(customer_cluster_data$Product_Category))

## d) Data preprocessing
# Convert factors to numeric for clustering but store original labels separately
customer_cluster_data$Product_Brand_num <- as.numeric(as.factor(customer_cluster_data$Product_Brand))
customer_cluster_data$Product_Category_num <- as.numeric(as.factor(customer_cluster_data$Product_Category))

# Scale the numeric variables for better clustering
customer_cluster_data_scaled <- scale(customer_cluster_data[, c("Product_Brand_num", "Product_Category_num", "Purchase_Quantity")])

## e) K-means Clustering
# Elbow method to find the optimal number of clusters
wss <- (nrow(customer_cluster_data_scaled) - 1) * sum(apply(customer_cluster_data_scaled, 2, var))
for (i in 2:15) {
  kmeans_result <- kmeans(customer_cluster_data_scaled, centers = i)
  wss[i] <- kmeans_result$tot.withinss
}

## f) Perform K-means with the selected number of clusters (K=3)
kmeans_result <- kmeans(customer_cluster_data_scaled, centers = 3)
customer_cluster_data$cluster <- as.factor(kmeans_result$cluster)


## g) Cluster Profiling (Summary)
cluster_summary <- customer_cluster_data %>%
  group_by(cluster) %>%
  summarise(
    avg_purchase = mean(Purchase_Quantity),
    avg_rating = mean(as.numeric(Ratings)),
    most_common_brand = names(sort(table(Product_Brand), decreasing = TRUE))[1]
  )

# Print the cluster summary
print(cluster_summary)


## ----------- Visualization ----------------##
# [Plot the Elbow Method]
elbow_data <- data.frame(K = 1:15, WSS = wss)
elbow_method_plot <- ggplot(elbow_data, aes(K, WSS)) +
  geom_point(size = 3, color = "#2C3E50") +
  geom_line(color = "#2980B9") +
  labs(title = "Elbow Method for Optimal Number of Clusters",
       x = "Number of Clusters (K)", y = "Total within-cluster sum of squares", subtitle = "") +
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
) %>%
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
brand_cluster_summary <- customer_cluster_data %>%
  group_by(cluster, Product_Brand) %>%
  summarise(
    Avg_Purchase = mean(Purchase_Quantity),
    Avg_Rating = mean(Rating_Num),
    Count = n(),
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

# Convert to dendrogram
dend <- as.dendrogram(hc)

# Assign labels (e.g., original Product_Brand from sampled rows)
labels(dend) <- customer_cluster_data$Product_Brand[sample_indices]

# Color branches by cluster
dend_colored <- color_branches(dend, k = 3)

# Plot horizontally for better readability
plot(dend_colored,
     main = "Clustered Customers by Brand & Cluster (Sampled)",
     horiz = TRUE,
     cex.main = 0.9)

