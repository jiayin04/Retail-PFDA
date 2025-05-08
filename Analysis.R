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


# View the structure of your dataset  
data <- retail_data_proc
str(data)

# Analysis 1: Is there a significant difference in customer satisfaction ratings 
#among countries, based on rating distribution and average scores?

# Load necessary libraries
library(ggplot2)
library(vcd)
library(rcompanion)
library(dplyr)
library(viridis)

# a. Convert 'Ratings' and 'Country' to character columns
data$Ratings <- as.character(data$Ratings)
data$Country <- as.character(data$Country)

# b. Create the contingency table
table_ratings <- table(data$Country, data$Ratings)

# c. View the table to ensure no empty categories
print(table_ratings)

# d. Perform Chi-square test if no zero counts
if (all(table_ratings > 0)) {
  chisq_result <- chisq.test(table_ratings)
  print(chisq_result)
} else {
  # e. Perform Fisher's Exact Test if there are zero counts
  fisher_result <- fisher.test(table_ratings)
  print(fisher_result)
}

# f. Proportional bar chart of ratings by country
ggplot(data, aes(x = Country, fill = Ratings)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Proportional Customer Ratings by Country", y = "Ratings") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Low" = "red", "High" = "blue"))

# g. Heatmap of ratings by country
ggplot(as.data.frame(as.table(table_ratings)), aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  labs(title = "Heatmap of Ratings by Country", x = "Country", y = "Rating") +
  theme_minimal() +
  scale_fill_gradient(low = "white", high = "blue")

# h. Stacked bar chart (absolute counts)
ggplot(data, aes(x = Country, fill = Ratings)) +
  geom_bar(position = "stack") +
  labs(title = "Absolute Customer Ratings by Country", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Low" = "red", "High" = "blue"))

# i. Total number of ratings per country
rating_count_per_country <- data %>%
  group_by(Country) %>%
  summarise(Total_Ratings = n())

ggplot(rating_count_per_country, aes(x = Country, y = Total_Ratings, fill = Country)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Number of Ratings per Country", y = "Total Ratings") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d()

# j. Convert Ratings to numeric for average rating analysis
data$Ratings_numeric <- ifelse(data$Ratings == "Low", 1, 5)


# k. Calculate average rating per country
avg_ratings <- data %>%
  group_by(Country) %>%
  summarise(Average_Rating = mean(Ratings_numeric, na.rm = TRUE))

print(avg_ratings)

# l. Visualize average rating per country
ggplot(avg_ratings, aes(x = Country, y = Average_Rating, fill = Country)) +
  geom_col() +
  labs(title = "Average Customer Rating by Country", y = "Average Rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d()

# m. Perform ANOVA test to compare mean ratings
anova_rating <- aov(Ratings_numeric ~ Country, data = data)
summary(anova_rating)


# Geospatial analysis
# Load libraries
library(tidyverse)
library(tidygeocoder)  # For OpenStreetMap geocoding
library(leaflet)       # For interactive map

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

# Step 5: Visualize on a map with Ratings
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



# Analysis 2: Are certain product categories, brands, and types 
#more popular in specific countries?

# Load required libraries
library(dplyr)
library(ggplot2)
library(viridis)

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

#Analysis 3: Do payment method and shipping method preferences 
#vary across countries?

# Load required libraries
library(dplyr)
library(ggplot2)
library(viridis)

# a. Frequency Tables for Payment Method and Shipping Method by Country

# Payment Method by Country
table_payment <- table(data$Country, data$Payment_Method)
print(table_payment)

# Shipping Method by Country
table_shipping <- table(data$Country, data$Shipping_Method)
print(table_shipping)

# b. Chi-square Tests

# Chi-square test for Payment Method vs Country
chi_payment <- chisq.test(table_payment)
print(chi_payment)

# Chi-square test for Shipping Method vs Country
chi_shipping <- chisq.test(table_shipping)
print(chi_shipping)

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

# Load necessary libraries
library(randomForest)
library(caret)
library(ggplot2)
library(viridis)
library(dplyr)
library(pdp)

# Assuming 'data' is the retail_data_proc dataset from your code
# Let's prepare the data for modeling

# a. Select relevant features for predicting customer ratings
# We'll use product details, payment method, shipping method, and country

model_data <- data %>%
  # Select relevant columns for our model
  select(Ratings, Country, Product_Category, Product_Brand, Product_Type, 
         Payment_Method, Shipping_Method) %>%
  # Remove any rows with missing values
  na.omit()

# b. Convert the target variable to a factor (for classification)
model_data$Ratings <- as.factor(model_data$Ratings)

# c. Convert all categorical variables to factors
model_data <- model_data %>%
  mutate_if(is.character, as.factor)

# d. Split data into training and testing sets (80/20 split)
set.seed(123) # For reproducibility
train_index <- createDataPartition(model_data$Ratings, p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# e. Build Random Forest model
rf_model <- randomForest(
  Ratings ~ ., 
  data = train_data,
  ntree = 100,       # Number of trees
  importance = TRUE  # Calculate variable importance
)

# f. Print model summary
print(rf_model)

# g. Evaluate model performance
# Make predictions on test data
predictions <- predict(rf_model, test_data)

# Create confusion matrix
conf_matrix <- confusionMatrix(predictions, test_data$Ratings)
print(conf_matrix)

# h. Variable importance
var_importance <- importance(rf_model)
print(var_importance)

# i. Plot variable importance
varImpPlot(rf_model, main = "Variable Importance")

# j. Plot variable importance using ggplot2 for better visualization
importance_df <- as.data.frame(importance(rf_model))
importance_df$Variable <- rownames(importance_df)

# Sort by MeanDecreaseGini
importance_df <- importance_df %>%
  arrange(desc(MeanDecreaseGini))

# Plot variable importance
ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = viridis(1)) +
  coord_flip() +
  labs(title = "Variable Importance in Random Forest Model",
       x = "Variables",
       y = "Mean Decrease in Gini Index") +
  theme_minimal()

# k. Analyze feature interactions (partial dependence plots)
# For the most important categorical variable
most_important_var <- importance_df$Variable[1]
if(most_important_var %in% names(train_data)) {
  pdp_plot <- partial(rf_model, pred.var = most_important_var, plot = TRUE)
  print(pdp_plot)
}

# l. Predict ratings for different countries
country_ratings <- data.frame(Country = unique(model_data$Country))
for(country in unique(model_data$Country)) {
  country_data <- model_data[model_data$Country == country, ]
  high_rating_pct <- mean(predict(rf_model, country_data) == "High") * 100
  cat(paste0("Predicted high rating percentage for ", country, ": ", 
             round(high_rating_pct, 2), "%\n"))
}


# m. Final performance metrics
final_metrics <- data.frame(
  Accuracy = conf_matrix$overall["Accuracy"],
  Sensitivity = conf_matrix$byClass["Sensitivity"],
  Specificity = conf_matrix$byClass["Specificity"],
  Precision = conf_matrix$byClass["Pos Pred Value"]
)
print(final_metrics)


