#==================================================================================
# Analysis 3: Can we accurately predict customer satisfaction ratings based on 
# country location, and which countries are most likely to generate high customer 
# satisfaction scores?
#==================================================================================

#---Predictive Analysis---#

# Data Preparation for Both Models

# a. Select only Country and Ratings columns
raw_data_country <- data %>%
  select(Ratings, Country) %>%
  na.omit()

# b. Convert variables to factors
raw_data_country$Ratings <- as.factor(raw_data_country$Ratings)
raw_data_country$Country <- as.factor(raw_data_country$Country)

# c. Check the initial distribution of data by country
country_counts <- table(raw_data_country$Country)
print("Original country distribution:")
print(country_counts)

# d. Create a balanced dataset with equal representation from each country
# Find the count of the second most frequent country (not USA)
usa_count <- sum(raw_data_country$Country == "USA")
other_countries <- raw_data_country$Country != "USA"
if(sum(other_countries) > 0) {
  second_most_freq_country <- names(sort(table(raw_data_country$Country[other_countries]), decreasing = TRUE)[1])
  second_most_count_country <- sum(raw_data_country$Country == second_most_freq_country)
} else {
  second_most_count_country <- 0
}

# e. Set sample size per country (use second most frequent country as guide)
sample_size_country <- min(second_most_count_country, 500)  # Cap at 500 samples per country
if(sample_size_country < 50) sample_size_country <- 50  # Ensure at least 50 samples per country

# f. Create balanced dataset
set.seed(123)  # For reproducibility
balanced_data_country <- data.frame()  # Empty dataframe to hold our balanced dataset

for(country in levels(raw_data_country$Country)) {
  country_data <- raw_data_country[raw_data_country$Country == country, ]
  
  # If country has fewer samples than our target, use all of them
  if(nrow(country_data) <= sample_size_country) {
    sampled_data_country <- country_data
  } else {
    # Otherwise, take a random sample
    sampled_data_country <- country_data[sample(1:nrow(country_data), sample_size_country), ]
  }
  
  balanced_data_country <- rbind(balanced_data_country, sampled_data_country)
}

# g. Shuffle the balanced dataset
balanced_data_country <- balanced_data_country[sample(1:nrow(balanced_data_country)), ]

# h. Check the new distribution
balanced_counts_country <- table(balanced_data_country$Country)
print("Balanced country distribution:")
print(balanced_counts_country)

# i. Visualize the balanced distribution
par(mfrow = c(1, 2))
# Original distribution
barplot(country_counts, main = "Original Country Distribution", 
        las = 2, cex.names = 0.7)
# Balanced distribution
barplot(balanced_counts_country, main = "Balanced Country Distribution", 
        las = 2, cex.names = 0.7)
par(mfrow = c(1, 1))

# j. Check rating distribution in balanced dataset
rating_by_country <- table(balanced_data_country$Country, balanced_data_country$Ratings)
print("Rating distribution by country in balanced dataset:")
print(rating_by_country)

# k. Split balanced data into training and testing sets (80/20 split)
set.seed(123)
train_index_country <- createDataPartition(balanced_data_country$Ratings, p = 0.8, list = FALSE)
train_data_country <- balanced_data_country[train_index_country, ]
test_data_country <- balanced_data_country[-train_index_country, ]

#==================================================================================
#  Logistic Regression Model
#==================================================================================

print("\n=== LOGISTIC REGRESSION MODEL ===\n")

# a. Convert Ratings to binary for logistic regression (1 = High, 0 = Low)
balanced_data_country$Ratings_binary <- ifelse(balanced_data_country$Ratings == "High", 1, 0)
train_data_country$Ratings_binary <- ifelse(train_data_country$Ratings == "High", 1, 0)
test_data_country$Ratings_binary <- ifelse(test_data_country$Ratings == "High", 1, 0)

# b. Build logistic regression model
logistic_model_country <- glm(Ratings_binary ~ Country, 
                              data = train_data_country, 
                              family = binomial)

# c. Print model summary
print("=== LOGISTIC REGRESSION MODEL SUMMARY ===")
print(summary(logistic_model_country))

# d. Make predictions on test data
predicted_probs_country <- predict(logistic_model_country, test_data_country, type = "response")
predicted_ratings_country <- ifelse(predicted_probs_country >= 0.5, "High", "Low")
predicted_ratings_country <- factor(predicted_ratings_country, levels = c("Low", "High"))

# e. Create confusion matrix
conf_matrix_logistic_country <- confusionMatrix(predicted_ratings_country, test_data_country$Ratings)
print("=== LOGISTIC REGRESSION PERFORMANCE ===")
print(conf_matrix_logistic_country)

# f. Get predicted probabilities for each country
country_predictions_logistic <- data.frame()
for(country in levels(balanced_data_country$Country)) {
  country_test_logistic <- data.frame(Country = factor(country, levels = levels(balanced_data_country$Country)))
  prob_high_country_logistic <- predict(logistic_model_country, country_test_logistic, type = "response")
  country_predictions_logistic <- rbind(country_predictions_logistic, 
                                        data.frame(Country = country, 
                                                   Predicted_High_Probability = round(prob_high_country_logistic * 100, 2)))
}

# g. Sort by probability and display
country_predictions_logistic <- country_predictions_logistic[order(-country_predictions_logistic$Predicted_High_Probability), ]
print("=== PREDICTED HIGH RATING PROBABILITIES BY COUNTRY (LOGISTIC REGRESSION) ===")
print(country_predictions_logistic)

# h. Visualize predicted probabilities from logistic regression
plot_logistic_country <- ggplot(country_predictions_logistic, 
                                aes(x = reorder(Country, Predicted_High_Probability), 
                                    y = Predicted_High_Probability, 
                                    fill = Predicted_High_Probability)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = paste0(Predicted_High_Probability, "%")), 
            hjust = -0.1, size = 3.5, fontface = "bold") +
  coord_flip() +
  scale_fill_viridis_c(name = "Probability (%)", 
                       option = "plasma",
                       begin = 0.2, end = 0.9) +
  labs(title = "Predicted High Rating Probabilities by Country",
       subtitle = "Logistic Regression Model",
       x = "Country", 
       y = "Predicted Probability of High Rating (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    legend.position = "right",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ylim(0, max(country_predictions_logistic$Predicted_High_Probability) * 1.1)

print(plot_logistic_country)

# Final performance metrics for Logistic Regression Model
final_metrics_country_logistic <- data.frame(
  Accuracy = conf_matrix_logistic_country$overall["Accuracy"],
  Sensitivity = conf_matrix_logistic_country$byClass["Sensitivity"],
  Specificity = conf_matrix_logistic_country$byClass["Specificity"],
  Precision = conf_matrix_logistic_country$byClass["Pos Pred Value"]
)
print("=== LOGISTIC REGRESSION DETAILED METRICS ===")
print(final_metrics_country_logistic)

#==================================================================================
# Additional Model: Random Forest Model
#==================================================================================

print("\n=== RANDOM FOREST MODEL ===\n")

# a. Build Random Forest model
rf_country_ratings_model <- randomForest(
  Ratings ~ Country,
  data = train_data_country,
  ntree = 50,
  importance = TRUE
)

# b. Print model summary
print("=== RANDOM FOREST MODEL SUMMARY ===")
print(rf_country_ratings_model)

# c. Make predictions on test data
predictions_country_rf <- predict(rf_country_ratings_model, test_data_country)

# d. Create confusion matrix for evaluation
conf_matrix_rf_country <- confusionMatrix(predictions_country_rf, test_data_country$Ratings)
print("=== RANDOM FOREST PERFORMANCE ===")
print(conf_matrix_rf_country)

# e. Variable importance (only Country in this case)
var_importance_country <- importance(rf_country_ratings_model)
print("=== VARIABLE IMPORTANCE ===")
print(var_importance_country)

# f. Calculate rating distribution by country in our balanced dataset
country_ratings <- balanced_data_country %>%
  group_by(Country) %>%
  summarize(
    Total = n(),
    High_Count = sum(Ratings == "High"),
    Low_Count = sum(Ratings == "Low"),
    High_Percentage = round(High_Count / Total * 100, 2)
  ) %>%
  arrange(desc(High_Percentage))

print("=== RATING PERCENTAGES BY COUNTRY (BALANCED DATA) ===")
print(country_ratings)

# g. Visualize rating distribution by country
ggplot(balanced_data_country, aes(x = reorder(Country, -as.numeric(Ratings == "High")), fill = Ratings)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Rating Distribution by Country (Balanced Dataset)", 
       y = "Percentage",
       x = "Country") +
  theme_minimal() +
  scale_fill_manual(values = c("Low" = "#E69F00", "High" = "#56B4E9")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# h. Calculate accuracy by country
train_predictions_country <- predict(rf_country_ratings_model, train_data_country)
train_results_country <- data.frame(
  Country = train_data_country$Country,
  Actual = train_data_country$Ratings,
  Predicted = train_predictions_country
)

country_accuracy <- train_results_country %>%
  group_by(Country) %>%
  summarize(
    Total = n(),
    Correct = sum(Actual == Predicted),
    Accuracy = round(Correct / Total * 100, 2)
  ) %>%
  arrange(desc(Accuracy))

print("=== PREDICTION ACCURACY BY COUNTRY ===")
print(country_accuracy)

# i. Predict high rating percentages for each country using Random Forest
all_country_predictions <- data.frame(
  Country = character(),
  Predicted_High_Probability = numeric(),
  stringsAsFactors = FALSE
)

cat("=== PREDICTED HIGH RATING PROBABILITIES BY COUNTRY (RANDOM FOREST) ===\n")
for (country in levels(balanced_data_country$Country)) {
  # Create test data for this country
  country_test <- data.frame(Country = factor(country, levels = levels(balanced_data_country$Country)))
  
  # Get predictions for this country
  prediction <- predict(rf_country_ratings_model, country_test, type = "prob")
  predicted_prob <- round(prediction[, "High"] * 100, 2)
  
  # Print result
  cat(paste0("Predicted high rating percentage for ", country, ": ", predicted_prob, "%\n"))
  
  # Append to results
  all_country_predictions <- rbind(all_country_predictions, 
                                   data.frame(Country = country, 
                                              Predicted_High_Probability = predicted_prob))
}

#j. Visualization for high ratings predictions
plot_rf_country <- ggplot(all_country_predictions, 
                          aes(x = reorder(Country, Predicted_High_Probability), 
                              y = Predicted_High_Probability, 
                              fill = Predicted_High_Probability)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = paste0(Predicted_High_Probability, "%")), 
            hjust = -0.1, size = 3.5, fontface = "bold") +
  coord_flip() +
  scale_fill_viridis_c(name = "Probability (%)", 
                       option = "viridis",
                       begin = 0.2, end = 0.9) +
  labs(title = "Predicted High Rating Probabilities by Country",
       subtitle = "Random Forest Model",
       x = "Country", 
       y = "Predicted Probability of High Rating (%)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11, face = "bold"),
    legend.position = "right",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  ylim(0, max(all_country_predictions$Predicted_High_Probability) * 1.1)

print(plot_rf_country)

#k. Final performance metrics for Random Forest
final_metrics_country_rf <- data.frame(
  Accuracy = conf_matrix_rf_country$overall["Accuracy"],
  Sensitivity = conf_matrix_rf_country$byClass["Sensitivity"],
  Specificity = conf_matrix_rf_country$byClass["Specificity"],
  Precision = conf_matrix_rf_country$byClass["Pos Pred Value"]
)
print("=== RANDOM FOREST DETAILED METRICS ===")
print(final_metrics_country_rf)

#==================================================================================
# Model Comparison
#==================================================================================

print("\n=== MODEL COMPARISON ===\n")

# Compare model performance
performance_comparison_country <- data.frame(
  Model = c("Logistic Regression", "Random Forest"),
  Accuracy = c(
    round(conf_matrix_logistic_country$overall["Accuracy"], 3),
    round(conf_matrix_country$overall["Accuracy"], 3)
  ),
  Sensitivity = c(
    round(conf_matrix_logistic_country$byClass["Sensitivity"], 3),
    round(conf_matrix_country$byClass["Sensitivity"], 3)
  ),
  Specificity = c(
    round(conf_matrix_logistic_country$byClass["Specificity"], 3),
    round(conf_matrix_country$byClass["Specificity"], 3)
  ),
  Precision = c(
    round(conf_matrix_logistic_country$byClass["Pos Pred Value"], 3),
    round(conf_matrix_country$byClass["Pos Pred Value"], 3)
  )
)

print("=== FINAL MODEL PERFORMANCE COMPARISON ===")
print(performance_comparison_country)