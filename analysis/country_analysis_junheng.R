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

# c. Run Tukey HSD post-hoc test
anova_country_model <- aov(Ratings_numeric ~ Country, data = data)
tukey_country_result <- TukeyHSD(anova_country_model)
print(tukey_country_result)

#==================================================================================
# Analysis 3: Can we accurately predict customer satisfaction ratings based on 
# country location, and which countries are most likely to generate high customer 
# satisfaction scores?
#==================================================================================

#---Predictive Analysis---#

# Data Preparation for Both Models

raw_data_country <- data %>%
  select(Ratings, Country) %>%
  na.omit()

# a. Convert variables to factors
raw_data_country$Ratings <- as.factor(raw_data_country$Ratings)
raw_data_country$Country <- as.factor(raw_data_country$Country)

# b. Check the initial distribution of data by country
country_counts <- table(raw_data_country$Country)
print("Original country distribution:")
print(country_counts)

# c. Check rating distribution before balancing
print("Original rating distribution:")
print(table(raw_data_country$Ratings))

# d. Create a balanced dataset with equal representation from each country
usa_count <- sum(raw_data_country$Country == "USA")
other_countries <- raw_data_country$Country != "USA"
if(sum(other_countries) > 0) {
  second_most_freq_country <- names(sort(table(raw_data_country$Country[other_countries]), decreasing = TRUE)[1])
  second_most_count_country <- sum(raw_data_country$Country == second_most_freq_country)
} else {
  second_most_count_country <- 0
}

# e. Set sample size per country
sample_size_country <- min(second_most_count_country, 50000)
if(sample_size_country < 50) sample_size_country <- 50

# f. Create balanced dataset with stratified sampling to preserve rating distribution
set.seed(123)
balanced_data_country <- data.frame()

for(country in levels(raw_data_country$Country)) {
  country_data <- raw_data_country[raw_data_country$Country == country, ]
  
  if(nrow(country_data) <= sample_size_country) {
    sampled_data_country <- country_data
  } else {
    # Stratified sampling to maintain rating distribution within each country
    high_ratings <- country_data[country_data$Ratings == "High", ]
    low_ratings <- country_data[country_data$Ratings == "Low", ]
    
    # Calculate proportional sample sizes
    high_prop <- nrow(high_ratings) / nrow(country_data)
    low_prop <- nrow(low_ratings) / nrow(country_data)
    
    high_sample_size <- round(sample_size_country * high_prop)
    low_sample_size <- sample_size_country - high_sample_size
    
    # Sample from each rating category
    if(nrow(high_ratings) > 0 && high_sample_size > 0) {
      high_sampled <- high_ratings[sample(1:nrow(high_ratings), min(high_sample_size, nrow(high_ratings))), ]
    } else {
      high_sampled <- data.frame()
    }
    
    if(nrow(low_ratings) > 0 && low_sample_size > 0) {
      low_sampled <- low_ratings[sample(1:nrow(low_ratings), min(low_sample_size, nrow(low_ratings))), ]
    } else {
      low_sampled <- data.frame()
    }
    
    sampled_data_country <- rbind(high_sampled, low_sampled)
  }
  
  balanced_data_country <- rbind(balanced_data_country, sampled_data_country)
}

# g. Shuffle the balanced dataset
balanced_data_country <- balanced_data_country[sample(1:nrow(balanced_data_country)), ]

# h.  Check the new distributions
balanced_counts_country <- table(balanced_data_country$Country)
print("Balanced country distribution:")
print(balanced_counts_country)

print("Balanced rating distribution:")
print(table(balanced_data_country$Ratings))

# i. Visualize the balanced distribution
par(mfrow = c(1, 2))
barplot(country_counts, main = "Original Country Distribution", las = 2, cex.names = 0.7)
barplot(balanced_counts_country, main = "Balanced Country Distribution", las = 2, cex.names = 0.7)
par(mfrow = c(1, 1))

# Check rating distribution by country in balanced dataset
rating_by_country <- table(balanced_data_country$Country, balanced_data_country$Ratings)
print("Rating distribution by country in balanced dataset:")
print(rating_by_country)

# Split balanced data into training and testing sets (80/20 split)
set.seed(123)
train_index_country <- createDataPartition(balanced_data_country$Ratings, p = 0.8, list = FALSE)
train_data_country <- balanced_data_country[train_index_country, ]
test_data_country <- balanced_data_country[-train_index_country, ]

print("Training data rating distribution:")
print(table(train_data_country$Ratings))
print("Test data rating distribution:")
print(table(test_data_country$Ratings))

#==================================================================================
# Logistic Regression Model
#==================================================================================

balanced_data_country$Ratings_binary <- ifelse(balanced_data_country$Ratings == "High", 1, 0)
train_data_country$Ratings_binary <- ifelse(train_data_country$Ratings == "High", 1, 0)
test_data_country$Ratings_binary <- ifelse(test_data_country$Ratings == "High", 1, 0)

# a. Build logistic regression model
logistic_model_country <- glm(Ratings_binary ~ Country, 
                              data = train_data_country, 
                              family = binomial)

# b. Print model summary
print("=== LOGISTIC REGRESSION MODEL SUMMARY ===")
print(summary(logistic_model_country))

# c. Make predictions on test data
predicted_probs_country <- predict(logistic_model_country, test_data_country, type = "response")

print("Distribution of predicted probabilities:")
print(summary(predicted_probs_country))
print("Quantiles of predicted probabilities:")
print(quantiles <- quantile(predicted_probs_country, probs = c(0.1, 0.25, 0.5, 0.75, 0.9)))

threshold_balanced <- quantile(predicted_probs_country, 1 - mean(test_data_country$Ratings == "High"))
print(paste("Balanced threshold:", round(threshold_balanced, 3)))

# d. Use the balanced threshold for final predictions
final_threshold <- threshold_balanced
predicted_ratings_country <- ifelse(predicted_probs_country >= final_threshold, "High", "Low")
predicted_ratings_country <- factor(predicted_ratings_country, levels = c("Low", "High"))

# e. Confusion matrix
conf_matrix_logistic_country <- confusionMatrix(predicted_ratings_country, test_data_country$Ratings)
print(conf_matrix_logistic_country)

# f. Get predicted probabilities for each country
country_predictions_logistic <- data.frame()
for(country in levels(balanced_data_country$Country)) {
  country_test_logistic <- data.frame(Country = factor(country, levels = levels(balanced_data_country$Country)))
  prob_high_country_logistic <- predict(logistic_model_country, country_test_logistic, type = "response")
  country_predictions_logistic <- rbind(country_predictions_logistic, 
                                        data.frame(Country = country, 
                                                   Predicted_High_Probability = round(prob_high_country_logistic * 100, 2),
                                                   Predicted_Low_Probability = round((1 - prob_high_country_logistic) * 100, 2)))
}

# g. Sort by probability and display
country_predictions_logistic <- country_predictions_logistic[order(-country_predictions_logistic$Predicted_High_Probability), ]
print("=== PREDICTED PROBABILITIES BY COUNTRY (LOGISTIC REGRESSION) ===")
print(country_predictions_logistic)

# h. Visualize predicted probabilities from logistic regression
country_pred_long <- reshape2::melt(country_predictions_logistic, 
                                    id.vars = "Country",
                                    measure.vars = c("Predicted_High_Probability", "Predicted_Low_Probability"),
                                    variable.name = "Rating_Type", 
                                    value.name = "Probability")

country_pred_long$Rating_Type <- gsub("Predicted_", "", country_pred_long$Rating_Type)
country_pred_long$Rating_Type <- gsub("_Probability", "", country_pred_long$Rating_Type)

# i.  Stacked bar plot showing both high and low probabilities
plot_logistic_country <- ggplot(country_pred_long, 
                                aes(x = reorder(Country, ifelse(Rating_Type == "High", Probability, -Probability)), 
                                    y = Probability, 
                                    fill = Rating_Type)) +
  geom_col(position = "dodge", alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0(Probability, "%")), 
            position = position_dodge(width = 0.7), 
            hjust = -0.1, size = 3.5, fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = c("High" = "#440154FF", "Low" = "#FDE725FF"),
                    name = "Rating Prediction") +
  labs(title = "Predicted Rating Probabilities by Country",
       subtitle = "Logistic Regression Model - Both High and Low Predictions",
       x = "Country", 
       y = "Predicted Probability (%)") +
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
  ylim(0, 105)

print(plot_logistic_country)


# j. Final performance metrics for Logistic Regression Model
final_metrics_country_logistic <- data.frame(
  Accuracy = conf_matrix_logistic_country$overall["Accuracy"],
  Sensitivity = conf_matrix_logistic_country$byClass["Sensitivity"],
  Specificity = conf_matrix_logistic_country$byClass["Specificity"],
  Precision = conf_matrix_logistic_country$byClass["Pos Pred Value"]
)
print("=== LOGISTIC REGRESSION DETAILED METRICS ===")
print(final_metrics_country_logistic)

# k. Visualize ROC Plot
country_roc_obj <- roc(test_data_country$Ratings_binary, predicted_probs_country)

# l. Calculate AUC
country_auc_value <- auc(country_roc_obj)

# m. Extract ROC curve data for plotting
country_roc_data <- data.frame(
  sensitivity = country_roc_obj$sensitivities,
  specificity = country_roc_obj$specificities,
  threshold = country_roc_obj$thresholds
)

# n. Calculate 1 - specificity for plotting
country_roc_data$fpr <- 1 - country_roc_data$specificity

# o. Create the ROC curve plot
country_roc_plot <- ggplot(country_roc_data, aes(x = fpr, y = sensitivity)) +
  geom_line(color = "#440154FF", size = 1.2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", 
              color = "red", alpha = 0.7, size = 0.8) +
  geom_point(data = data.frame(
    fpr = 1 - threshold_balanced,  # Using the balanced threshold from your code
    sensitivity = country_roc_obj$sensitivities[which.min(abs(country_roc_obj$thresholds - threshold_balanced))]
  ), aes(x = fpr, y = sensitivity), 
  color = "#FDE725FF", size = 3, shape = 16) +
  annotate("text", 
           x = 0.7, y = 0.3, 
           label = paste("AUC =", round(country_auc_value, 3)), 
           size = 5, fontface = "bold",
           color = "#440154FF") +
  labs(
    title = "ROC Curve - Logistic Regression Model",
    subtitle = "Country-based Rating Prediction",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  ) +
  coord_equal() +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2))

# p. Display the plot
print(country_roc_plot)

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

# e. Action plan summary
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
