# Load libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(viridis)
library(randomForest)
library(caret)
library(tidyverse)
library(patchwork)

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
# Create summary data for labels
data_summary_country <- data %>%
  group_by(Country, Ratings) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Country) %>%
  mutate(total = sum(count),
         prop = count / total) %>%
  arrange(Country, desc(Ratings)) %>%  # Arrange so High comes first (top)
  mutate(pos = cumsum(prop) - 0.5 * prop)

plot_proportional_ratings_country <- ggplot(data, aes(x = Country, fill = Ratings)) +
  geom_bar(position = "fill") +
  geom_text(data = data_summary_country, 
            aes(x = Country, y = pos, label = count),
            position = position_identity(),
            color = "white", 
            fontface = "bold",
            size = 3.5) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Proportional Customer Ratings by Country", 
       y = "Proportion of Ratings",
       subtitle = "Numbers show actual counts for each rating category") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Low" = "red", "High" = "green"))

print(plot_proportional_ratings_country)

# g. Heatmap of ratings by country 
plot_heatmap_ratings_country <- ggplot(as.data.frame(as.table(table_ratings)), aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  labs(title = "Heatmap of Ratings by Country", x = "Country", y = "Rating") +
  theme_minimal() +
  scale_fill_gradient(low = "white", high = "purple")
print(plot_heatmap_ratings_country)

# h.Pie chart showing distribution of ratings across all countries
rating_totals <- data %>%
  group_by(Ratings) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

plot_pie_ratings <- ggplot(rating_totals, aes(x = "", y = Count, fill = Ratings)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Overall Distribution of Customer Ratings") +
  theme_void() +
  scale_fill_manual(values = c("Low" = "red", "High" = "green")) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))
print(plot_pie_ratings)


# plotly-friendly version
plot_pie_ratings_plotly <- plot_ly(rating_totals, 
                                  labels = ~Ratings, 
                                  values = ~Count, 
                                  type = 'pie',
                                  textinfo = 'label+percent',
                                  insidetextorientation = 'radial',
                                  marker = list(colors = c('green', 'red'))) %>%
  layout(title = list(text = "Overall Distribution of Customer Ratings",
                     x = 0.5,
                     y = 0.95,
                     font = list(size = 16, face = "bold")))

# i.Dot plot for total ratings per country
rating_count_per_country <- data %>%
  group_by(Country) %>%
  summarise(Total_Ratings = n()) %>%
  arrange(desc(Total_Ratings))

plot_dot_total_ratings <- ggplot(rating_count_per_country, aes(x = Total_Ratings, y = reorder(Country, Total_Ratings))) +
  geom_point(size = 4, color = "steelblue") +
  geom_segment(aes(x = 0, xend = Total_Ratings, y = Country, yend = Country), 
               color = "lightblue", size = 1) +
  labs(title = "Total Number of Ratings per Country", 
       x = "Total Ratings", y = "Country") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank())
print(plot_dot_total_ratings)

# j. Calculate average rating per country
avg_country_ratings <- data %>%
  group_by(Country) %>%
  summarise(Average_Rating = mean(Ratings_numeric, na.rm = TRUE))
print(avg_country_ratings)

# k. Violin plot of ratings by country 
plot_violin_ratings_country <- ggplot(data, aes(x = Country, y = Ratings_numeric, fill = Country)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", alpha = 0.5) +
  labs(title = "Distribution of Ratings by Country", y = "Rating Score") +
  theme_minimal() +
  scale_fill_viridis_d()
print(plot_violin_ratings_country)

# l.Lollipop chart for average ratings by country
avg_country_ratings_sorted <- avg_country_ratings %>%
  arrange(desc(Average_Rating))

plot_lollipop_avg_ratings <- ggplot(avg_country_ratings_sorted, aes(x = Average_Rating, y = reorder(Country, Average_Rating))) +
  geom_segment(aes(x = 0, xend = Average_Rating, y = Country, yend = Country), 
               color = "grey", size = 1) +
  geom_point(size = 4, color = "orange") +
  labs(title = "Average Customer Rating by Country", 
       x = "Average Rating", y = "Country") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank()) +
  xlim(0, 5)
print(plot_lollipop_avg_ratings)

# m. Scatter plot showing relationship between total ratings and average ratings
plot_scatter_count_vs_avg <- ggplot(merge(rating_count_per_country, avg_country_ratings), 
                                    aes(x = Total_Ratings, y = Average_Rating)) +
  geom_point(size = 4, alpha = 0.7, color = "darkgreen") +
  geom_text(aes(label = Country), hjust = -0.1, vjust = 0.5, size = 3) +
  labs(title = "Relationship between Total Ratings and Average Rating by Country",
       x = "Total Number of Ratings", y = "Average Rating") +
  theme_minimal() +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed")
print(plot_scatter_count_vs_avg)

#==================================================================================
# Analysis 2: Is there a statistically significant difference in customer 
# satisfaction ratings among countries, and which specific country pairs show 
# meaningful differences in their rating distributions?
#==================================================================================

#---Diagnostic Analysis---#

# a. Perform Chi-square test

chisq_country_result <- chisq.test(table_ratings)
print(chisq_country_result)

# b. Perform ANOVA test to compare mean ratings
anova_country_rating <- aov(Ratings_numeric ~ Country, data = data)
summary(anova_country_rating)

anova_country_model <- aov(Ratings_numeric ~ Country, data = data)

# c. Run Tukey HSD post-hoc test
tukey_country_result <- TukeyHSD(anova_country_model)

# d. View results
print(tukey_country_result)

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
plot_rating_distribution_country <- ggplot(balanced_data_country, aes(x = reorder(Country, -as.numeric(Ratings == "High")), fill = Ratings)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Rating Distribution by Country (Balanced Dataset)", 
       y = "Percentage",
       x = "Country") +
  theme_minimal() +
  scale_fill_manual(values = c("Low" = "#E69F00", "High" = "#56B4E9")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot_rating_distribution_country)

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
# SIDE-BY-SIDE COMPARISON
#==================================================================================

# Create side-by-side plots
side_by_side_plots_country <- plot_logistic_country + plot_rf_country + 
  plot_layout(ncol = 2) +
  plot_annotation(
    title = "Model Predictions Comparison: High Rating Probabilities by Country",
    theme = theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
  )

print(side_by_side_plots_country)

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
    Country_Satisfaction_Rate = round((High_Ratings / Total_Customers) * 100, 1)
  ) %>%
  arrange(Country_Satisfaction_Rate)

print("Country Satisfaction Rates:")
print(country_summary)

# b. Categorize countries by performance
country_summary <- country_summary %>%
  mutate(
    Country_Performance_Level = case_when(
      Country_Satisfaction_Rate >= 70 ~ "Good",
      Country_Satisfaction_Rate >= 50 ~ "Average",
      TRUE ~ "Needs Improvement"
    )
  )

# c. Create action recommendations
recommendations_country <- country_summary %>%
  mutate(
    Country_Recommended_Action = case_when(
      Country_Performance_Level == "Needs Improvement" ~ "Priority: Immediate service improvement needed",
      Country_Performance_Level == "Average" ~ "Focus: Targeted improvements to reach 70%+",
      TRUE ~ "Maintain: Continue current good practices"
    ),
    Country_Target_Satisfaction = case_when(
      Country_Performance_Level == "Needs Improvement" ~ Country_Satisfaction_Rate + 30,
      Country_Performance_Level == "Average" ~ 75,
      TRUE ~ Country_Satisfaction_Rate + 5
    )
  )

print("Country Recommendations:")
print(recommendations_country %>% select(Country, Country_Satisfaction_Rate, Country_Performance_Level, Country_Recommended_Action))

# d. Visualize current vs target satisfaction
plot_satisfaction_targets_country <- ggplot(recommendations_country, aes(x = reorder(Country, Country_Satisfaction_Rate))) +
  geom_col(aes(y = Country_Satisfaction_Rate, fill = Country_Performance_Level), alpha = 0.7) +
  geom_point(aes(y = Country_Target_Satisfaction), color = "red", size = 3) +
  geom_segment(aes(xend = Country, y = Country_Satisfaction_Rate, yend = Country_Target_Satisfaction), 
               color = "red", linetype = "dashed") +
  coord_flip() +
  labs(title = "Current vs Target Satisfaction Rates",
       subtitle = "Red dots show improvement targets",
       x = "Country", y = "Satisfaction Rate (%)",
       fill = "Performance Level") +
  theme_minimal()

print(plot_satisfaction_targets_country)

# e. Simple action plan summary
action_summary <- recommendations_country %>%
  count(Country_Performance_Level, Country_Recommended_Action) %>%
  rename(Countries_Count = n)

print("Action Plan Summary:")
print(action_summary)

# f. Priority countries (lowest satisfaction rates)
priority_countries <- head(recommendations_country, 3)

cat("\nTOP 3 PRIORITY COUNTRIES FOR IMPROVEMENT:\n")
for(i in 1:3) {
  cat(paste0(i, ". ", priority_countries$Country[i], 
             " (", priority_countries$Country_Satisfaction_Rate[i], "% satisfaction)\n"))
  cat("   Action:", priority_countries$Country_Recommended_Action[i], "\n\n")
}