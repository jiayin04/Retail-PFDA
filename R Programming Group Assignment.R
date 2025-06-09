library(tidyr)
library(dplyr)
library(scales)
library(ggplot2)
library(caret)
library(pROC)
library(xgboost)
library(arules)
library(purrr)
library(reshape2)
library(cluster)
library(dendextend)
library(factoextra)

### Analysis 2 - 1: How does customer Payment Method and Shipping Method affect customer satisfaction levels and total amount?
## Section 1: IVs x Ratings
# Create plot data
payment_shipping_interception_plot_data <- retail_data_proc %>%
  group_by(Payment_Method, Shipping_Method, Ratings) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(Payment_Method, Shipping_Method) %>%
  mutate(prop_high = count / sum(count)) %>%
  filter(Ratings == "High")

# Print the data
print(payment_shipping_interception_plot_data)

# Plot
payment_shipping_interception_plot <- ggplot(payment_shipping_interception_plot_data, aes(x = Payment_Method, y = prop_high, group = Shipping_Method, color = Shipping_Method)) +
  geom_point(size = 4) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Customer Satisfaction by Payment and Shipping Method",
    x = "Payment Method",
    y = "% of High Ratings",
    color = "Shipping Method"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal()
print(payment_shipping_interception_plot)


## Section 2: IVs x Total Amount (Purchasing Behavior)
# Create plot-specific Total_Amount without mutating retail_data_proc
payment_shipping_dot_plot_data <- retail_data_proc %>%
  mutate(Total_Amount = Purchase_Quantity * Amount) %>%
  group_by(Payment_Method, Shipping_Method) %>%
  summarise(mean_total = mean(Total_Amount, na.rm = TRUE), .groups = 'drop')

# Print the summarized plot data
print(payment_shipping_dot_plot_data)

# Plot with red–green color gradient
payment_shipping_dot_plot <- ggplot(payment_shipping_dot_plot_data, aes(x = Payment_Method, y = Shipping_Method)) +
  geom_point(aes(size = mean_total, color = mean_total)) +
  # geom_text(aes(label = round(mean_total, 1)), vjust = -0.5, size = 3.5) +
  scale_color_gradient(low = "red", high = "green") +
  labs(
    title = "Average Total Amount by Payment and Shipping Method",
    x = "Payment Method",
    y = "Shipping Method",
    size = "Mean Total Amount",
    color = "Mean Total Amount"
  ) +
  theme_minimal()
print(payment_shipping_dot_plot)

## Section 3: Both IVs & Both DVs
# Create and categorize Total Amount
payment_shipping_facet_heatmap_data <- retail_data_proc %>%
  mutate(Total_Amount = Purchase_Quantity * Amount) %>%
  mutate(Total_Amount_Category = case_when(
    Total_Amount < 100 ~ "<100",
    Total_Amount >= 100 & Total_Amount <= 4000 ~ "100-4000",
    Total_Amount > 4000 ~ ">4000"
  ))

# Create plot data
payment_shipping_facet_heatmap_data <- payment_shipping_facet_heatmap_data %>%
  group_by(Payment_Method, Shipping_Method, Total_Amount_Category, Ratings) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(Payment_Method, Shipping_Method, Total_Amount_Category) %>%
  mutate(prop_high = count / sum(count)) %>%
  filter(Ratings == "High")

# Print the data
print(payment_shipping_facet_heatmap_data)

# Plot
payment_shipping_facet_heatmap_plot <- ggplot(payment_shipping_facet_heatmap_data, aes(x = Payment_Method, y = Total_Amount_Category, fill = prop_high)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#deebf7", high = "#08519c", labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~Shipping_Method) +
  labs(
    title = "Satisfaction vs. Purchasing Behavior by Shipping and Payment",
    x = "Payment Method",
    y = "Total Amount Category",
    fill = "% of High Ratings"
  ) +
  theme_minimal()
print(payment_shipping_facet_heatmap_plot)

### Analysis 2 - 2: Is there a statistically significant relationship between customer satisfaction with payment and shipping method? 
## Calculate p value with Chi Square Test
# Collapse into a 2D table by combining Shipping_Method & Payment_Method into one interaction
retail_data_proc$Combo <- interaction(retail_data_proc$Payment_Method, retail_data_proc$Shipping_Method)
payment_shipping_chi_table <- table(retail_data_proc$Ratings, retail_data_proc$Combo)

## Perform Chi-square test
payment_shipping_chi_test <- chisq.test(payment_shipping_chi_table)

# Print results
print(payment_shipping_chi_test)

## With logistic regression test
# Convert Ratings to binary (1 = High, 0 = Low)
retail_data_proc$Ratings_Binary <- ifelse(retail_data_proc$Ratings == "High", 1, 0)

# Fit logistic regression model
payment_shipping_logit_model <- glm(Ratings_Binary ~ Payment_Method + Shipping_Method,
                   data = retail_data_proc, family = "binomial")

# Summary and p-values
summary(payment_shipping_logit_model)


## Visualization
# Run the logistic regression model
payment_shipping_lr_model <- glm(Ratings_Binary ~ Payment_Method + Shipping_Method, data = retail_data_proc, family = binomial)

# Extract coefficients and p-values
payment_shipping_lr_summary_model <- summary(payment_shipping_lr_model)
payment_shipping_coefficients_df <- as.data.frame(coef(payment_shipping_lr_summary_model))
payment_shipping_coefficients_df$Variable <- rownames(payment_shipping_coefficients_df)
colnames(payment_shipping_coefficients_df) <- c("Estimate", "Std_Error", "Z_Value", "P_Value", "Variable")

# Remove intercept if you want to focus only on predictors
payment_shipping_coefficients_df <- payment_shipping_coefficients_df[payment_shipping_coefficients_df$Variable != "(Intercept)", ]

# Plot
payment_shipping_coefficient_plot <- ggplot(payment_shipping_coefficients_df, aes(x = reorder(Variable, Estimate), y = Estimate, fill = P_Value < 0.05)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "gray")) +
  geom_text(aes(label = paste0("p=", signif(P_Value, 3))), hjust = 0.1, size = 3) +
  labs(
    title = "Logistic Regression Coefficients & Significance",
    x = "Predictor",
    y = "Estimate (Effect on Log Odds)",
    fill = "Significant (p < 0.05)"
  ) +
  theme_minimal()
print(payment_shipping_coefficient_plot)


## Second visualization
payment_shipping_coefficients_df$log_pval <- -log10(payment_shipping_coefficients_df$P_Value)

payment_shipping_log10_coefficient_plot <- ggplot(payment_shipping_coefficients_df, aes(x = reorder(Variable, log_pval), y = log_pval, fill = P_Value < 0.05)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") +
  scale_fill_manual(values = c("TRUE" = "#08519c", "FALSE" = "gray")) +
  labs(
    title = "-log10(p-value) by Predictor",
    x = "Predictor",
    y = "-log10(p-value)",
    fill = "Significant"
  ) +
  theme_minimal()
print(payment_shipping_log10_coefficient_plot)


### Analysis 2 – 3: How accurately can payment and shipping method predict customer satisfaction ratings?
# Make sure Ratings_bin is a labeled factor
retail_data_bin <- retail_data_proc %>%
  mutate(Ratings_bin = ifelse(Ratings == "High", "High", "Low")) %>%
  mutate(Ratings_bin = factor(Ratings_bin, levels = c("Low", "High")))


# Down sampling
set.seed(123)
payment_shipping_downsampled_data <- caret::downSample(
  x = dplyr::select(retail_data_bin, Payment_Method, Shipping_Method),
  y = retail_data_bin$Ratings_bin,
  yname = "Ratings_bin"
)

# Compare original data vs downsampled data high/low rating count
print(table(factor(retail_data_bin$Ratings_bin, levels = c("Low", "High"))))
print(table(payment_shipping_downsampled_data$Ratings_bin))

# Split into training and testing sets
set.seed(123)
payment_shipping_trainIndex <- createDataPartition(payment_shipping_downsampled_data$Ratings_bin, p = 0.8, list = FALSE)
payment_shipping_train_data <- payment_shipping_downsampled_data[payment_shipping_trainIndex, ]
payment_shipping_test_data <- payment_shipping_downsampled_data[-payment_shipping_trainIndex, ]

# Logistic Regression
payment_shipping_logit_model <- glm(Ratings_bin ~ Payment_Method + Shipping_Method,
                   data = payment_shipping_train_data,
                   family = "binomial")

# Ensure payment_shipping_test_data has same factor levels as payment_shipping_train_data
payment_shipping_test_data$Payment_Method <- factor(payment_shipping_test_data$Payment_Method, levels = levels(payment_shipping_train_data$Payment_Method))
payment_shipping_test_data$Shipping_Method <- factor(payment_shipping_test_data$Shipping_Method, levels = levels(payment_shipping_train_data$Shipping_Method))

# Predict probabilities and classes
payment_shipping_logit_probs <- predict(payment_shipping_logit_model, newdata = payment_shipping_test_data[, c("Payment_Method", "Shipping_Method")], type = "response")
payment_shipping_logit_preds <- ifelse(payment_shipping_logit_probs > 0.5, "High", "Low")
payment_shipping_logit_preds <- factor(payment_shipping_logit_preds, levels = c("Low", "High"))

# Step 6: XGBoost Prediction
payment_shipping_dummies <- dummyVars(Ratings_bin ~ ., data = payment_shipping_train_data)
train_matrix <- predict(payment_shipping_dummies, newdata = payment_shipping_train_data)
test_matrix <- predict(payment_shipping_dummies, newdata = payment_shipping_test_data)

# Convert target to numeric 0 and 1
payment_shipping_dtrain <- xgb.DMatrix(data = train_matrix, label = as.numeric(payment_shipping_train_data$Ratings_bin) - 1)
payment_shipping_dtest <- xgb.DMatrix(data = test_matrix, label = as.numeric(payment_shipping_test_data$Ratings_bin) - 1)

# Train the XGBoost model
payment_shipping_xgb_model <- xgboost(data = payment_shipping_dtrain, nrounds = 50, objective = "binary:logistic", verbose = 0)

# Predict probabilities and classes
payment_shipping_xgb_probs <- predict(payment_shipping_xgb_model, payment_shipping_dtest)
payment_shipping_xgb_preds <- ifelse(payment_shipping_xgb_probs > 0.5, "High", "Low")
payment_shipping_xgb_preds <- factor(payment_shipping_xgb_preds, levels = c("Low", "High"))

# Output predictions for review
print("Logistic Regression Predictions Summary:")
print(summary(payment_shipping_logit_preds))

print("XGBoost Predictions Summary:")
print(summary(payment_shipping_xgb_preds))


### Prediction Evaluation
## Confusion Matrix Evaluation (CM)
# Logistic Regression CM
# Ensure both are factors with the same levels
payment_shipping_logit_preds <- factor(payment_shipping_logit_preds, levels = c("Low", "High"))
payment_shipping_test_data$Ratings_bin <- factor(payment_shipping_test_data$Ratings_bin, levels = c("Low", "High"))

payment_shipping_cm_logit <- confusionMatrix(payment_shipping_logit_preds, payment_shipping_test_data$Ratings_bin)
print(payment_shipping_cm_logit)

# XGBoost CM
payment_shipping_cm_xgb <- confusionMatrix(payment_shipping_xgb_preds, payment_shipping_test_data$Ratings_bin)
print(payment_shipping_cm_xgb)

## ROC Curve Evaluation (ROC)
# Logistic Regression
payment_shipping_roc_logit <- roc(as.numeric(payment_shipping_test_data$Ratings_bin), as.numeric(payment_shipping_logit_probs))
payment_shipping_auc_logit <- auc(payment_shipping_roc_logit)
# Plot
payment_shipping_lr_roc <- plot(payment_shipping_roc_logit, main = paste("Logistic Regression ROC Curve (AUC =", round(payment_shipping_auc_logit, 3), ")"))

# XGBoost
payment_shipping_roc_xgb <- roc(as.numeric(payment_shipping_test_data$Ratings_bin), as.numeric(payment_shipping_xgb_probs))
payment_shipping_auc_xgb <- auc(payment_shipping_roc_xgb)
# Plot
payment_shipping_xgb_roc <- plot(payment_shipping_roc_xgb, main = paste("XGBoost ROC Curve (AUC =", round(payment_shipping_auc_xgb, 3), ")"))

  
### Analysis 2 – 4: How Does Payment And Shipping Methods Affect Customer Satisfaction Across Clusters?
# Prepare the cluster data (add Total_Amount and convert Ratings)
transaction_cluster_data <- retail_data_proc %>%
  mutate(
    Total_Amount = Purchase_Quantity * Amount,
    Rating_Numeric = ifelse(Ratings == "High", 1, 0)  # Convert to numeric
  ) %>%
  dplyr::select(Payment_Method, Shipping_Method, Total_Amount, Rating_Numeric) %>%
  drop_na()

# Down sampling the cluster data to 12k records
set.seed(123)  # For reproducibility
transaction_cluster_data_sampled <- transaction_cluster_data %>%
  sample_n(12000)

# Calculate Gower distance 
gower_dist <- cluster::daisy(transaction_cluster_data_sampled, metric = "gower")

# Hierarchical clustering
transaction_hclust_model <- hclust(gower_dist, method = "complete")

# Cut into 3 clusters
transaction_cluster_cut <- cutree(transaction_hclust_model, k = 3)

# Add cluster results back to sampled data
transaction_cluster_data_sampled$Cluster <- as.factor(transaction_cluster_cut)

# Encode categoricals to model matrix
transaction_cluster_data_encoded <- model.matrix(~ Payment_Method + Shipping_Method + Total_Amount + Rating_Numeric - 1, data = transaction_cluster_data_sampled)
transaction_cluster_data_encoded <- as.data.frame(transaction_cluster_data_encoded)

# Add back cluster info for use in PCA or visualization
transaction_cluster_data_encoded$Cluster <- transaction_cluster_data_sampled$Cluster

# Plot
transaction_cluster_plot <- factoextra::fviz_cluster(
  list(data = transaction_cluster_data_encoded[, !names(transaction_cluster_data_encoded) %in% "Cluster"], 
       cluster = transaction_cluster_data_encoded$Cluster),
  geom = "point", ellipse.type = "convex", palette = "jco",
  main = "Cluster Visualization"
)
print(transaction_cluster_plot)

transaction_cluster_data_sampled$Cluster <- as.factor(transaction_cluster_cut)


transaction_summary_table <- transaction_cluster_data_sampled %>%
  group_by(Cluster) %>%
  summarise(
    Cluster_Size = n(),
    Avg_Amount = round(mean(Total_Amount, na.rm = TRUE), 2),
    Avg_Rating = round(mean(Rating_Numeric, na.rm = TRUE), 3),
    Top_Shipping = names(sort(table(Shipping_Method), decreasing = TRUE))[1],
    Top_Payment = names(sort(table(Payment_Method), decreasing = TRUE))[1],
    .groups = "drop"
  )

print(transaction_summary_table)


## Top 3 Payment x Shipping Combo in each cluster
# Preparing Data
transaction_cluster_data_sampled_data <- transaction_cluster_data_sampled %>%
  mutate(Combo = paste(Payment_Method, "+", Shipping_Method))

transaction_summary_table <- transaction_cluster_data_sampled_data %>%
  group_by(Cluster, Combo) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Cluster) %>%
  mutate(Percent = Count / sum(Count) * 100) %>%
  arrange(Cluster, desc(Count)) %>%
  slice_head(n = 1)  # Top combo per cluster

transaction_summary_table


# Create combo column
transaction_cluster_data_sampled_data$Combo <- paste(transaction_cluster_data_sampled_data$Payment_Method, transaction_cluster_data_sampled_data$Shipping_Method, sep = " + ")

# Get top 3 combos per cluster
payment_shipping_top_combos <- transaction_cluster_data_sampled_data %>%
  count(Cluster, Combo) %>%
  group_by(Cluster) %>%
  slice_max(n, n = 3) %>%
  mutate(Proportion = n / sum(n)) %>%
  arrange(Cluster, desc(n)) %>%
  mutate(Combo = factor(Combo, levels = rev(unique(Combo)))) %>%  # this ensures proper stacking order within each cluster
  mutate(LabelPos = cumsum(Proportion) - Proportion / 2) %>%
  ungroup() %>%
  mutate(Label = ifelse(Proportion > 0.05, paste0(Combo, "\n", percent(Proportion)), ""))

# Plot
cluster_combo_plot <- ggplot(payment_shipping_top_combos, aes(x = factor(1), y = Proportion, fill = Combo)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(
    aes(y = LabelPos, label = Label),
    color = "black", size = 3, fontface = "italic", angle =30, lineheight = 0.9
  ) +
  facet_wrap(~ Cluster) +
  labs(
    title = "Proportional Distribution of Top 3 Payment + Shipping Combos per Cluster",
    x = NULL,
    y = "Proportion"
  ) +
  scale_y_continuous(labels = percent_format()) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank()
  )

print(cluster_combo_plot)
