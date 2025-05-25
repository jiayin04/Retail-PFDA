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
library(randomForest)
library(caret)
library(tidyverse)
library(tidygeocoder)
library(leaflet)
library(rgdal)
library(RColorBrewer)

# View the structure of your dataset  
data <- retail_data_proc
str(data)

#==================================================================================
# Analysis 1: What are the patterns and distributions of customer satisfaction 
# ratings across different countries, and how do the rating profiles vary geographically?
#==================================================================================

#---Descriptive Analysis---#

# a. Convert 'Ratings' and 'Country' to character columns
data$Ratings <- as.character(data$Ratings)
data$Country <- as.character(data$Country)

# b. Create the contingency table
table_ratings <- table(data$Country, data$Ratings)

# c. View the table to ensure no empty categories
print(table_ratings)

# d. Convert Ratings to numeric for average rating analysis
data$Ratings_numeric <- ifelse(data$Ratings == "Low", 1, 5)

# e. Calculate descriptive statistics by ratings
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

# j. Calculate average rating per country
avg_ratings <- data %>%
  group_by(Country) %>%
  summarise(Average_Rating = mean(Ratings_numeric, na.rm = TRUE))

print(avg_ratings)

# k. Violin plot of ratings by country
ggplot(data, aes(x = Country, y = Ratings_numeric, fill = Country)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +
  labs(title = "Distribution of Ratings by Country", y = "Rating Score") +
  theme_minimal() +
  scale_fill_viridis_d()

# l. Visualize average rating per country
ggplot(avg_ratings, aes(x = Country, y = Average_Rating, fill = Country)) +
  geom_col() +
  labs(title = "Average Customer Rating by Country", y = "Average Rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d()

#==================================================================================
# Analysis 2: Is there a statistically significant difference in customer 
# satisfaction ratings among countries, and which specific country pairs show 
# meaningful differences in their rating distributions?
#==================================================================================

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

# d. View results
print(tukey_result)

#==================================================================================
# Analysis 3: Can we accurately predict customer satisfaction ratings based on 
# country location, and which countries are most likely to generate high customer 
# satisfaction scores?
#==================================================================================

#---Predictive Analysis---#

# Random Forest Model

# a. Select only Country and Ratings columns
raw_data <- data %>%
  select(Ratings, Country) %>%
  na.omit()

# b. Convert variables to factors
raw_data$Ratings <- as.factor(raw_data$Ratings)
raw_data$Country <- as.factor(raw_data$Country)

# c. Check the initial distribution of data by country
country_counts <- table(raw_data$Country)
print("Original country distribution:")
print(country_counts)

# d. Create a balanced dataset with equal representation from each country
# Find the count of the second most frequent country (not USA)
usa_count <- sum(raw_data$Country == "USA")
other_countries <- raw_data$Country != "USA"
if(sum(other_countries) > 0) {
  second_most_freq <- names(sort(table(raw_data$Country[other_countries]), decreasing = TRUE)[1])
  second_most_count <- sum(raw_data$Country == second_most_freq)
} else {
  second_most_count <- 0
}

# e. Set sample size per country (use second most frequent country as guide)
sample_size <- min(second_most_count, 500)  # Cap at 500 samples per country
if(sample_size < 50) sample_size <- 50  # Ensure at least 50 samples per country

# f. Create balanced dataset
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

# g. Shuffle the balanced dataset
balanced_data <- balanced_data[sample(1:nrow(balanced_data)), ]

# h. Check the new distribution
balanced_counts <- table(balanced_data$Country)
print("Balanced country distribution:")
print(balanced_counts)

# i. Visualize the balanced distribution
par(mfrow = c(1, 2))
# Original distribution
barplot(country_counts, main = "Original Country Distribution", 
        las = 2, cex.names = 0.7)
# Balanced distribution
barplot(balanced_counts, main = "Balanced Country Distribution", 
        las = 2, cex.names = 0.7)
par(mfrow = c(1, 1))

# j. Check rating distribution in balanced dataset
rating_by_country <- table(balanced_data$Country, balanced_data$Ratings)
print("Rating distribution by country in balanced dataset:")
print(rating_by_country)

# k. Split balanced data into training and testing sets (80/20 split)
set.seed(123)
train_index <- createDataPartition(balanced_data$Ratings, p = 0.8, list = FALSE)
train_data <- balanced_data[train_index, ]
test_data <- balanced_data[-train_index, ]

# l. Build Random Forest model
rf_model <- randomForest(
  Ratings ~ Country,
  data = train_data,
  ntree = 50,
  importance = TRUE
)

# m. Print model summary
print(rf_model)

# n. Make predictions on test data
predictions <- predict(rf_model, test_data)

# o. Create confusion matrix for evaluation
conf_matrix <- confusionMatrix(predictions, test_data$Ratings)
print(conf_matrix)

# p. Variable importance (only Country in this case)
var_importance <- importance(rf_model)
print(var_importance)

# q. Calculate rating distribution by country in our balanced dataset
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

# r. Visualize rating distribution by country
ggplot(balanced_data, aes(x = reorder(Country, -as.numeric(Ratings == "High")), fill = Ratings)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Rating Distribution by Country (Balanced Dataset)", 
       y = "Percentage",
       x = "Country") +
  theme_minimal() +
  scale_fill_manual(values = c("Low" = "#E69F00", "High" = "#56B4E9")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# s. Calculate accuracy by country
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

# t. Check model performance on original unbalanced data
orig_predictions <- predict(rf_model, raw_data)
orig_accuracy <- mean(orig_predictions == raw_data$Ratings)
print(paste0("Model accuracy on original unbalanced data: ", round(orig_accuracy * 100, 2), "%"))

# u. Predict high rating percentages for each country
for(country in levels(balanced_data$Country)) {
  # Create test data for this country
  country_test <- data.frame(Country = factor(country, levels = levels(balanced_data$Country)))
  
  # Get predictions for this country
  country_pred <- predict(rf_model, country_test, type = "prob")
  
  # Print results
  cat(paste0("Predicted high rating percentage for ", country, ": ", 
             round(country_pred[, "High"] * 100, 2), "%\n"))
}

# v. Final performance metrics
final_metrics <- data.frame(
  Accuracy = conf_matrix$overall["Accuracy"],
  Sensitivity = conf_matrix$byClass["Sensitivity"],
  Specificity = conf_matrix$byClass["Specificity"],
  Precision = conf_matrix$byClass["Pos Pred Value"]
)
print("Model performance metrics (on balanced test data):")
print(final_metrics)

#==================================================================================
# Analysis 4: What specific actions should be taken to improve customer 
# satisfaction ratings in each country?
#==================================================================================

#---Prescriptive Analysis---#

# a. Calculate satisfaction rate by country
country_summary <- data %>%
  group_by(Country) %>%
  summarise(
    Total_Customers = n(),
    High_Ratings = sum(Ratings == "High"),
    Satisfaction_Rate = round((High_Ratings / Total_Customers) * 100, 1)
  ) %>%
  arrange(Satisfaction_Rate)

print("Country Satisfaction Rates:")
print(country_summary)

# b. Categorize countries by performance
country_summary <- country_summary %>%
  mutate(
    Performance_Level = case_when(
      Satisfaction_Rate >= 70 ~ "Good",
      Satisfaction_Rate >= 50 ~ "Average",
      TRUE ~ "Needs Improvement"
    )
  )

# c. Create action recommendations
recommendations <- country_summary %>%
  mutate(
    Recommended_Action = case_when(
      Performance_Level == "Needs Improvement" ~ "Priority: Immediate service improvement needed",
      Performance_Level == "Average" ~ "Focus: Targeted improvements to reach 70%+",
      TRUE ~ "Maintain: Continue current good practices"
    ),
    Target_Satisfaction = case_when(
      Performance_Level == "Needs Improvement" ~ Satisfaction_Rate + 30,
      Performance_Level == "Average" ~ 75,
      TRUE ~ Satisfaction_Rate + 5
    )
  )

print("Country Recommendations:")
print(recommendations %>% select(Country, Satisfaction_Rate, Performance_Level, Recommended_Action))

# d. Visualize current vs target satisfaction
ggplot(recommendations, aes(x = reorder(Country, Satisfaction_Rate))) +
  geom_col(aes(y = Satisfaction_Rate, fill = Performance_Level), alpha = 0.7) +
  geom_point(aes(y = Target_Satisfaction), color = "red", size = 3) +
  geom_segment(aes(xend = Country, y = Satisfaction_Rate, yend = Target_Satisfaction), 
               color = "red", linetype = "dashed") +
  coord_flip() +
  labs(title = "Current vs Target Satisfaction Rates",
       subtitle = "Red dots show improvement targets",
       x = "Country", y = "Satisfaction Rate (%)",
       fill = "Performance Level") +
  theme_minimal()

# e. Simple action plan summary
action_summary <- recommendations %>%
  count(Performance_Level, Recommended_Action) %>%
  rename(Countries_Count = n)

print("Action Plan Summary:")
print(action_summary)

# f. Priority countries (lowest satisfaction rates)
priority_countries <- head(recommendations, 3)

cat("\nTOP 3 PRIORITY COUNTRIES FOR IMPROVEMENT:\n")
for(i in 1:3) {
  cat(paste0(i, ". ", priority_countries$Country[i], 
             " (", priority_countries$Satisfaction_Rate[i], "% satisfaction)\n"))
  cat("   Action:", priority_countries$Recommended_Action[i], "\n\n")
}

#==================================================================================
# Extra Feature: Geospatial Analysis
#==================================================================================

# a. Get unique cities
unique_cities <- data %>%
  select(City, State, Country) %>%
  distinct() %>%
  mutate(full_address = paste(City, State, Country, sep = ", "))

# b. Geocode using Nominatim (OpenStreetMap)
geocoded_cities <- unique_cities %>%
  geocode(address = full_address, method = "osm", lat = latitude, long = longitude)

# c. Merge coordinates back into the main data
data_with_coords <- data %>%
  left_join(geocoded_cities, by = c("City", "State", "Country"))

# d. Check any failed geocoding
failed <- data_with_coords %>% filter(is.na(latitude) | is.na(longitude))
print(failed)

# e. Basic map visualization
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