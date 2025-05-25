# Load libraries
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
library(ggplot2)
library(vcd)
library(rcompanion)
library(viridis)
library(corrplot)
library(gridExtra)
library(GGally)
library(waffle)
library(treemap)
library(plotly)
library(ggridges)
library(ggalluvial)
library(ggmosaic)

# View the structure of your dataset  
data <- retail_data_proc
str(data)

#==================================================================================
# Analysis 1: Is there a significant difference in customer satisfaction ratings 
# among countries, based on rating distribution and average scores?
#==================================================================================


#---Descriptive Analysis---#

# a. Convert 'Ratings' and 'Country' to character columns
data$Ratings <- as.character(data$Ratings)
data$Country <- as.character(data$Country)

# b. Create the contingency table
table_ratings <- table(data$Country, data$Ratings)

# c. View the table to ensure no empty categories
print(table_ratings)

# Descriptive Statistics by Ratings
# Convert Ratings to numeric for average rating analysis
data$Ratings_numeric <- ifelse(data$Ratings == "Low", 1, 5)

rating_stats_by_country <- data %>%
  group_by(Country) %>%
  summarise(
    Count = n(),
    Mean = mean(Ratings_numeric, na.rm = TRUE),
    Median = median(Ratings_numeric, na.rm = TRUE),
    SD = sd(Ratings_numeric, na.rm = TRUE),
    Min = min(Ratings_numeric, na.rm = TRUE),
    Max = max(Ratings_numeric, na.rm = TRUE)
  ) %>%
  arrange(desc(Mean))

print(rating_stats_by_country)

# d. Proportional bar chart of ratings by country
ggplot(data, aes(x = Country, fill = Ratings)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Proportional Customer Ratings by Country", y = "Ratings") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Low" = "red", "High" = "blue"))

# e. Heatmap of ratings by country
ggplot(as.data.frame(as.table(table_ratings)), aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  labs(title = "Heatmap of Ratings by Country", x = "Country", y = "Rating") +
  theme_minimal() +
  scale_fill_gradient(low = "white", high = "blue")

# f. Stacked bar chart (absolute counts)
ggplot(data, aes(x = Country, fill = Ratings)) +
  geom_bar(position = "stack") +
  labs(title = "Absolute Customer Ratings by Country", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Low" = "red", "High" = "blue"))

# g. Total number of ratings per country
rating_count_per_country <- data %>%
  group_by(Country) %>%
  summarise(Total_Ratings = n())

ggplot(rating_count_per_country, aes(x = Country, y = Total_Ratings, fill = Country)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Number of Ratings per Country", y = "Total Ratings") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d()

# i. Calculate average rating per country
avg_ratings <- data %>%
  group_by(Country) %>%
  summarise(Average_Rating = mean(Ratings_numeric, na.rm = TRUE))

print(avg_ratings)


# j. Violin plot of ratings by country
ggplot(data, aes(x = Country, y = Ratings_numeric, fill = Country)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +
  labs(title = "Distribution of Ratings by Country", y = "Rating Score") +
  theme_minimal() +
  scale_fill_viridis_d()

# m. Visualize average rating per country
ggplot(avg_ratings, aes(x = Country, y = Average_Rating, fill = Country)) +
  geom_col() +
  labs(title = "Average Customer Rating by Country", y = "Average Rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d()


#---Diagnostic Analysis---#

# a. Perform Chi-square test if no zero counts
if (all(table_ratings > 0)) {
  chisq_result <- chisq.test(table_ratings)
  print(chisq_result)
} else {
  # Perform Fisher's Exact Test if there are zero counts
  fisher_result <- fisher.test(table_ratings)
  print(fisher_result)
}

# b. Perform ANOVA test to compare mean ratings
anova_rating <- aov(Ratings_numeric ~ Country, data = data)
summary(anova_rating)

anova_model <- aov(Ratings_numeric ~ Country, data = data)

# c. Run Tukey HSD post-hoc test
tukey_result <- TukeyHSD(anova_model)

# View results
print(tukey_result)

#---Prediction analysis---#

#1. Random Forest Model

# Load necessary libraries
library(randomForest)
library(caret)
library(ggplot2)
library(viridis)
library(dplyr)

# Step 1: Select only Country and Ratings columns
raw_data <- data %>%
  select(Ratings, Country) %>%
  na.omit()

# Step 2: Convert variables to factors
raw_data$Ratings <- as.factor(raw_data$Ratings)
raw_data$Country <- as.factor(raw_data$Country)

# Step 3: Check the initial distribution of data by country
country_counts <- table(raw_data$Country)
print("Original country distribution:")
print(country_counts)

# Step 4: Create a balanced dataset with equal representation from each country

# Find the count of the second most frequent country (not USA)
usa_count <- sum(raw_data$Country == "USA")
other_countries <- raw_data$Country != "USA"
if(sum(other_countries) > 0) {
  second_most_freq <- names(sort(table(raw_data$Country[other_countries]), decreasing = TRUE)[1])
  second_most_count <- sum(raw_data$Country == second_most_freq)
} else {
  second_most_count <- 0
}

# Set sample size per country (use second most frequent country as guide)
sample_size <- min(second_most_count, 500)  # Cap at 500 samples per country
if(sample_size < 50) sample_size <- 50  # Ensure at least 50 samples per country

# Create balanced dataset
set.seed(123)  # For reproducibility
balanced_data <- data.frame()  # Empty dataframe to hold our balanced dataset

for(country in levels(raw_data$Country)) {
  country_data <- raw_data[raw_data$Country == country, ]
  
  # If country has fewer samples than our target, use all of them
  if(nrow(country_data) <= sample_size) {
    sampled_data <- country_data
  } else {
    # Otherwise, take a random sample
    sampled_data <- country_data[sample(1:nrow(country_data), sample_size), ]
  }
  
  balanced_data <- rbind(balanced_data, sampled_data)
}

# Shuffle the balanced dataset
balanced_data <- balanced_data[sample(1:nrow(balanced_data)), ]

# Step 5: Check the new distribution
balanced_counts <- table(balanced_data$Country)
print("Balanced country distribution:")
print(balanced_counts)

# Step 6: Visualize the balanced distribution
par(mfrow = c(1, 2))
# Original distribution
barplot(country_counts, main = "Original Country Distribution", 
        las = 2, cex.names = 0.7)
# Balanced distribution
barplot(balanced_counts, main = "Balanced Country Distribution", 
        las = 2, cex.names = 0.7)
par(mfrow = c(1, 1))

# Step 7: Check rating distribution in balanced dataset
rating_by_country <- table(balanced_data$Country, balanced_data$Ratings)
print("Rating distribution by country in balanced dataset:")
print(rating_by_country)

# Step 8: Split balanced data into training and testing sets (80/20 split)
set.seed(123)
train_index <- createDataPartition(balanced_data$Ratings, p = 0.8, list = FALSE)
train_data <- balanced_data[train_index, ]
test_data <- balanced_data[-train_index, ]

# Step 9: Build Random Forest model
rf_model <- randomForest(
  Ratings ~ Country,
  data = train_data,
  ntree = 50,
  importance = TRUE
)

# Step 10: Print model summary
print(rf_model)

# Step 11: Make predictions on test data
predictions <- predict(rf_model, test_data)

# Step 12: Create confusion matrix for evaluation
conf_matrix <- confusionMatrix(predictions, test_data$Ratings)
print(conf_matrix)

# Step 13: Variable importance (only Country in this case)
var_importance <- importance(rf_model)
print(var_importance)

# Step 14: Calculate rating distribution by country in our balanced dataset
country_ratings <- balanced_data %>%
  group_by(Country) %>%
  summarize(
    Total = n(),
    High_Count = sum(Ratings == "High"),
    Low_Count = sum(Ratings == "Low"),
    High_Percentage = round(High_Count / Total * 100, 2)
  ) %>%
  arrange(desc(High_Percentage))

print("Rating percentages by country (balanced data):")
print(country_ratings)

# Step 15: Visualize rating distribution by country
ggplot(balanced_data, aes(x = reorder(Country, -as.numeric(Ratings == "High")), fill = Ratings)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Rating Distribution by Country (Balanced Dataset)", 
       y = "Percentage",
       x = "Country") +
  theme_minimal() +
  scale_fill_manual(values = c("Low" = "#E69F00", "High" = "#56B4E9")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Step 16: Calculate accuracy by country
train_predictions <- predict(rf_model, train_data)
train_results <- data.frame(
  Country = train_data$Country,
  Actual = train_data$Ratings,
  Predicted = train_predictions
)

country_accuracy <- train_results %>%
  group_by(Country) %>%
  summarize(
    Total = n(),
    Correct = sum(Actual == Predicted),
    Accuracy = round(Correct / Total * 100, 2)
  ) %>%
  arrange(desc(Accuracy))

print("Prediction accuracy by country:")
print(country_accuracy)

# Step 17: Check model performance on original unbalanced data
# This shows how well the model generalizes to the real-world distribution
orig_predictions <- predict(rf_model, raw_data)
orig_accuracy <- mean(orig_predictions == raw_data$Ratings)
print(paste0("Model accuracy on original unbalanced data: ", round(orig_accuracy * 100, 2), "%"))

# Step 18: Predict high rating percentages for each country
for(country in levels(balanced_data$Country)) {
  # Create test data for this country
  country_test <- data.frame(Country = factor(country, levels = levels(balanced_data$Country)))
  
  # Get predictions for this country
  country_pred <- predict(rf_model, country_test, type = "prob")
  
  # Print results
  cat(paste0("Predicted high rating percentage for ", country, ": ", 
             round(country_pred[, "High"] * 100, 2), "%\n"))
}

# Step 19: Final performance metrics
final_metrics <- data.frame(
  Accuracy = conf_matrix$overall["Accuracy"],
  Sensitivity = conf_matrix$byClass["Sensitivity"],
  Specificity = conf_matrix$byClass["Specificity"],
  Precision = conf_matrix$byClass["Pos Pred Value"]
)
print("Model performance metrics (on balanced test data):")
print(final_metrics)

#2. Logistic regression model

library(caret)  
library(dplyr)     

# Make sure Ratings is a factor with "Low" as baseline and "High" as event
data$Ratings <- factor(data$Ratings, levels = c("Low", "High"))

# Check the structure
str(data)

# Fit logistic regression model predicting Ratings by Country
logistic_model <- glm(Ratings ~ Country, data = data, family = binomial)

# View summary of the model
summary(logistic_model)

# Predict probabilities of "High" rating
data$predicted_prob <- predict(logistic_model, type = "response")

# Convert predicted probabilities to class labels with threshold 0.5
data$predicted_class <- ifelse(data$predicted_prob > 0.5, "High", "Low")

# Convert to factor for confusion matrix
data$predicted_class <- factor(data$predicted_class, levels = c("Low", "High"))

# Generate confusion matrix to evaluate the model
conf_matrix <- confusionMatrix(data$predicted_class, data$Ratings)

# Print confusion matrix and statistics
print(conf_matrix)

#==================================================================================
# Extra feature: Geospatial analysis
#==================================================================================

# Load libraries
library(tidyverse)
library(tidygeocoder)  # For OpenStreetMap geocoding
library(leaflet)       # For interactive map
library(rgdal)         # For additional map functionalities
library(RColorBrewer)  # For color palettes

# Step 1: Get unique cities
unique_cities <- data %>%
  select(City, State, Country) %>%
  distinct() %>%
  mutate(full_address = paste(City, State, Country, sep = ", "))

# Step 2: Geocode using Nominatim (OpenStreetMap)
geocoded_cities <- unique_cities %>%
  geocode(address = full_address, method = "osm", lat = latitude, long = longitude)

# Step 3: Merge coordinates back into the main data
data_with_coords <- data %>%
  left_join(geocoded_cities, by = c("City", "State", "Country"))

# Step 4: Check any failed geocoding
failed <- data_with_coords %>% filter(is.na(latitude) | is.na(longitude))
print(failed)

# Step 5: Basic map visualization
leaflet(data_with_coords) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    lng = ~longitude, lat = ~latitude,
    color = ~ifelse(Ratings == "High", "blue", "red"),
    popup = ~paste0(City, ", ", Country, "<br>Rating: ", Ratings),
    radius = 4,
    fillOpacity = 0.7
  ) %>%
  addLegend(position = "bottomright", colors = c("blue", "red"),
            labels = c("High Rating", "Low Rating"), title = "Customer Ratings")
#==================================================================================
# Analysis 2: Are certain product categories, brands, and types 
# more popular in specific countries?
#==================================================================================

# Load required libraries
library(dplyr)
library(ggplot2)
library(viridis)
library(treemap)
library(ggalluvial)
library(vcd)
library(RColorBrewer)

# a. Frequency Tables

# Product Category by Country
table_category <- table(data$Country, data$Product_Category)
print(table_category)

# Product Brand by Country
table_brand <- table(data$Country, data$Product_Brand)
print(table_brand)

# Product Type by Country
table_type <- table(data$Country, data$Product_Type)
print(table_type)

#---Diagnostic Analysis---#

# b. Chi-square Tests

# Chi-square test for Product Category vs Country
chi_category <- chisq.test(table_category)
print(chi_category)

# Chi-square test for Product Brand vs Country
chi_brand <- chisq.test(table_brand)
print(chi_brand)

# Chi-square test for Product Type vs Country
chi_type <- chisq.test(table_type)
print(chi_type)

# c. Proportional Bar Plots for Visualization

# Product Category by Country
ggplot(data, aes(x = Country, fill = Product_Category)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Product Category Proportions by Country", y = "Percentage") +
  theme_minimal() +
  scale_fill_viridis(discrete = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Product Brand by Country
ggplot(data, aes(x = Country, fill = Product_Brand)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Product Brand Proportions by Country", y = "Percentage") +
  theme_minimal() +
  scale_fill_viridis(discrete = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Product Type by Country
ggplot(data, aes(x = Country, fill = Product_Type)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Product Type Proportions by Country", y = "Percentage") +
  theme_minimal() +
  scale_fill_viridis(discrete = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# 5. Heat map of Product Categories by Country with percentage annotations
# Calculate percentages within each country
heatmap_data <- data %>%
  group_by(Country, Product_Category) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(Country) %>%
  mutate(percentage = count / sum(count) * 100)

ggplot(heatmap_data, aes(x = Country, y = Product_Category, fill = percentage)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), color = "white", size = 3) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Percentage of Product Categories by Country",
       fill = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 7. Circular bar plot for product brands by country
# Prepare data
brand_country_counts <- data %>%
  count(Country, Product_Brand) %>%
  group_by(Country) %>%
  arrange(desc(n)) %>%
  slice_head(n = 3) %>%  # Top 3 brands per country
  ungroup()

# Add angle for circular plot
brand_country_counts <- brand_country_counts %>%
  arrange(Country, desc(n)) %>%
  mutate(id = row_number()) %>%
  mutate(angle = 90 - 360 * (id - 0.5) / nrow(.))

# Create circular bar plot
ggplot(brand_country_counts, aes(x = as.factor(id), y = n, fill = Country)) +
  geom_bar(stat = "identity") +
  coord_polar() +
  geom_text(aes(label = Product_Brand, angle = angle), 
            hjust = ifelse(brand_country_counts$angle > 90 & brand_country_counts$angle < 270, 1.1, -0.1),
            size = 2.5) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(title = "Top 3 Product Brands by Country (Circular Bar Plot)")

# 8. Paired bar chart comparing product types across countries
# Get counts for comparison
paired_data <- data %>%
  count(Country, Product_Type) %>%
  group_by(Product_Type) %>%
  filter(n() > 1) %>%  # Only include product types present in multiple countries
  ungroup()

ggplot(paired_data, aes(x = Product_Type, y = n, fill = Country)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Comparison of Product Types Across Countries",
       x = "Product Type", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Paired")

#==================================================================================
# Predictive Analysis: Random Forest to predict product category from country
#==================================================================================

# Load required libraries
library(caret)         # For data splitting and evaluation
library(nnet)          # For multinomial logistic regression
library(ggplot2)       # For visualization
library(dplyr)         # For data manipulation

# Convert relevant columns to factors
data$Country <- as.factor(data$Country)
data$Product_Category <- as.factor(data$Product_Category)
data$Product_Brand <- as.factor(data$Product_Brand)
data$Product_Type <- as.factor(data$Product_Type)

# Split data into training and testing sets (80% train, 20% test)
set.seed(123)
data_shuffled <- data[sample(nrow(data)), ]
train_index_product <- createDataPartition(data_shuffled$Product_Category, p = 0.8, list = FALSE)
train_data_product <- data_shuffled[train_index_product, ]
test_data_product <- data_shuffled[-train_index_product, ]

# Train multinomial logistic regression model to predict Product_Category
logit_model <- multinom(Product_Category ~ Country + Product_Brand + Product_Type, data = train_data_product)

# Summary of the logistic regression model
cat("Multinomial Logistic Regression Model for Product Category Prediction:\n")
print(summary(logit_model))

# Make predictions on test data
predictions <- predict(logit_model, newdata = test_data_product)

# Evaluate model performance using confusion matrix
conf_matrix <- confusionMatrix(predictions, test_data_product$Product_Category)
print(conf_matrix)

# Model accuracy
accuracy <- conf_matrix$overall["Accuracy"]
cat("Model Accuracy:", round(accuracy * 100, 2), "%\n")

# Visualization of the confusion matrix
conf_data <- as.data.frame(conf_matrix$table)
colnames(conf_data) <- c("Actual", "Predicted", "Frequency")

ggplot(conf_data, aes(x = Actual, y = Predicted, fill = Frequency)) +
  geom_tile() +
  geom_text(aes(label = Frequency), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Confusion Matrix: Product Category Predictions") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Add predicted categories to the test set
test_data_product$Predicted_Category <- predictions

# Find the most common predicted product category for each country
country_predictions <- test_data_product %>%
  group_by(Country) %>%
  count(Predicted_Category, sort = TRUE) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  arrange(desc(n))

cat("\nMost Common Predicted Product Category by Country:\n")
print(country_predictions)

# Plot top predicted product category by country
ggplot(country_predictions, aes(x = reorder(Country, n), y = n, fill = Predicted_Category)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top Predicted Product Category by Country",
       x = "Country",
       y = "Number of Predictions",
       fill = "Predicted Category") +
  theme_minimal()

#==================================================================================
# Analysis 3: Do payment method and shipping method preferences 
# vary across countries?
#==================================================================================

# Load required libraries
library(dplyr)
library(ggplot2)
library(viridis)
library(vcd)
library(forcats)
library(gganimate)

#---Descriptive Analysis---#

# a. Frequency Tables for Payment Method and Shipping Method by Country

# Payment Method by Country
table_payment <- table(data$Country, data$Payment_Method)
print(table_payment)

# Shipping Method by Country
table_shipping <- table(data$Country, data$Shipping_Method)
print(table_shipping)

# c. Visualizations for Payment Method and Shipping Method

# 1. Payment Method by Country (Proportional Bar Plot)
ggplot(data, aes(x = Country, fill = Payment_Method)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Payment Method Preferences by Country", y = "Payment Method (Percentage)") +
  theme_minimal() +
  scale_fill_viridis(discrete = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. Shipping Method by Country (Proportional Bar Plot)
ggplot(data, aes(x = Country, fill = Shipping_Method)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Shipping Method Preferences by Country", y = "Shipping Method (Percentage)") +
  theme_minimal() +
  scale_fill_viridis(discrete = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. Absolute Payment Method Counts by Country
ggplot(data, aes(x = Country, fill = Payment_Method)) +
  geom_bar(position = "stack") +
  labs(title = "Absolute Payment Method Counts by Country", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis(discrete = TRUE)

# 4. Absolute Shipping Method Counts by Country
ggplot(data, aes(x = Country, fill = Shipping_Method)) +
  geom_bar(position = "stack") +
  labs(title = "Absolute Shipping Method Counts by Country", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis(discrete = TRUE)


# 5. Dot plot comparing payment method preferences
payment_data <- data %>%
  group_by(Country, Payment_Method) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(Country) %>%
  mutate(percentage = count / sum(count) * 100)

ggplot(payment_data, aes(x = percentage, y = reorder(Country, percentage), color = Payment_Method)) +
  geom_point(size = 4) +
  facet_wrap(~ Payment_Method) +
  labs(title = "Payment Method Preferences by Country",
       x = "Percentage", y = "Country") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")


#---Diagnostic Analysis---#

# b. Chi-square Tests

# Chi-square test for Payment Method vs Country
chi_payment <- chisq.test(table_payment)
print(chi_payment)

# Chi-square test for Shipping Method vs Country
chi_shipping <- chisq.test(table_shipping)
print(chi_shipping)

#---Predictive Analysis---#

# Load required libraries
library(randomForest)
library(caret)
library(ggplot2)
library(dplyr)

data$Country <- as.factor(data$Country)
data$Payment_Method <- as.factor(data$Payment_Method)


set.seed(123)
data_shuffled <- data[sample(nrow(data)), ]
train_index_payment <- createDataPartition(data_shuffled$Payment_Method, p = 0.8, list = FALSE)
train_data_payment <- data_shuffled[train_index_payment, ]
test_data_payment <- data_shuffled[-train_index_payment, ]

table(test_data_payment$Payment_Method)

# Train Random Forest model for Payment Method
rf_payment <- randomForest(
  Payment_Method ~ Country,
  data = train_data_payment,
  ntree = 200,
  importance = TRUE
)

# Model summary for Payment Method
cat("Random Forest Model for Payment Method Prediction\n")
print(rf_payment)

# Make predictions on test data
payment_predictions <- predict(rf_payment, test_data_payment)

# Create confusion matrix
payment_conf_matrix <- confusionMatrix(payment_predictions, test_data_payment$Payment_Method)
print(payment_conf_matrix)

# Calculate accuracy
payment_accuracy <- payment_conf_matrix$overall["Accuracy"]
cat("Payment Method Prediction Accuracy:", round(payment_accuracy * 100, 2), "%\n\n")

payment_conf_data <- as.data.frame(payment_conf_matrix$table)
colnames(payment_conf_data) <- c("Actual", "Predicted", "Frequency")

ggplot(payment_conf_data, aes(x = Actual, y = Predicted, fill = Frequency)) +
  geom_tile() +
  geom_text(aes(label = Frequency), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Confusion Matrix: Payment Method Predictions") +
  theme_minimal()

#  Top predicted payment method per country
test_data_payment$Predicted_Payment_Method <- payment_predictions

predicted_by_country <- test_data_payment %>%
  group_by(Country, Predicted_Payment_Method) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  arrange(Country, desc(Count)) %>%
  group_by(Country) %>%
  slice_max(order_by = Count, n = 1)  

print(predicted_by_country)

# Plot Visualization for Top Predicted Payement Method by Country
ggplot(predicted_by_country, aes(x = Country, y = Count, fill = Predicted_Payment_Method)) +
  geom_bar(stat = "identity") +
  labs(title = "Most Predicted Payment Method by Country", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 2. Second Random Forest model: Predicting Shipping Method
data$Shipping_Method <- as.factor(data$Shipping_Method)

# Split data
set.seed(123)
data_shuffled <- data[sample(nrow(data)), ]
train_index_shipping <- createDataPartition(data_shuffled$Shipping_Method, p = 0.8, list = FALSE)
train_data_shipping <- data_shuffled[train_index_shipping, ]
test_data_shipping <- data_shuffled[-train_index_shipping, ]

# Train Random Forest model for Shipping Method
rf_shipping <- randomForest(
  Shipping_Method ~ Country,
  data = train_data_shipping,
  ntree = 200,
  importance = TRUE
)

# Model summary for Shipping Method
cat("Random Forest Model for Shipping Method Prediction\n")
print(rf_shipping)

# Make predictions on test data
shipping_predictions <- predict(rf_shipping, test_data_shipping)

# Create confusion matrix
shipping_conf_matrix <- confusionMatrix(shipping_predictions, test_data_shipping$Shipping_Method)
print(shipping_conf_matrix)

# Calculate accuracy
shipping_accuracy <- shipping_conf_matrix$overall["Accuracy"]
cat("Shipping Method Prediction Accuracy:", round(shipping_accuracy * 100, 2), "%\n\n")

# Simple visualization of shipping prediction
shipping_conf_data <- as.data.frame(shipping_conf_matrix$table)
colnames(shipping_conf_data) <- c("Actual", "Predicted", "Frequency")

ggplot(shipping_conf_data, aes(x = Actual, y = Predicted, fill = Frequency)) +
  geom_tile() +
  geom_text(aes(label = Frequency), color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Confusion Matrix: Shipping Method Predictions") +
  theme_minimal()

#  Top predicted shipping method per country
test_data_shipping$Predicted_Shipping_Method <- shipping_predictions

predicted_shipping_by_country <- test_data_shipping %>%
  group_by(Country, Predicted_Shipping_Method) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  arrange(Country, desc(Count)) %>%
  group_by(Country) %>%
  slice_max(order_by = Count, n = 1)  

print(predicted_shipping_by_country)

# Plot Visualization for Top Predicted Shipping Method by Country
ggplot(predicted_shipping_by_country, aes(x = Country, y = Count, fill = Predicted_Shipping_Method)) +
  geom_bar(stat = "identity") +
  labs(title = "Most Predicted Shipping Method by Country", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))








