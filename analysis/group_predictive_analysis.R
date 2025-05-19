# analysis/group_predictive_analysis.R
#' @description ML model for entire retail dataset 
#' @data Source: retail_data_proc (processed retail dataset)

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
overall_predictions <- predict(overall_rf_model, overall_test_data)

# Create confusion matrix
overall_conf_matrix <- confusionMatrix(overall_predictions, overall_test_data$Ratings)
print(overall_conf_matrix)

# Variable importance
overall_var_importance <- importance(overall_rf_model)
print(overall_var_importance)

# Plot variable importance
varImpPlot(overall_rf_model, main = "Variable Importance")

# Plot variable importance using ggplot2 for better visualization
overall_importance_df <- as.data.frame(importance(overall_rf_model))
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