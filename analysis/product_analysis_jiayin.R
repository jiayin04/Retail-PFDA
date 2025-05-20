# analysis/product_analysis_jiayin.R
#' @description Objectives: To evaluate the influence of product preferences on purchasing behavior and customer satisfaction. 
#' @author Kok Jia Yin
#' @details This script analyzes retail customer data examining:
#'   1. How product preference influences purchasing behavior and satisfaction
#'   2. Co-purchasing behavior across product categories
#'   3. Relationships between brand preference and customer satisfaction
#'   4. Customer clustering based on brand preferences and purchase behaviors
#' @data Source: retail_data_proc (processed retail dataset)


### ─────────────────────────────────────────────
### 1. How does product preference influence purchasing behavior and customer satisfaction?
### ─────────────────────────────────────────────
#' @details
#' - Uses styleHistogram function in helper_functions

# i) Analysis technique - MANOVA
library(dplyr)
library(car) 

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
# Base histogram of Purchase Quantity
hist1 <- ggplot(manova_data, aes(x = Purchase_Quantity, fill = Product_Category)) +
  geom_histogram(bins = 20, alpha = 0.7, position = "dodge", color = "black") +
  scale_fill_met_d("Cassatt2") +
  labs(x = "Purchase Quantity", y = "Count") +
  styleHistogram() +
  theme(legend.position = "none")

# Base histogram of Ratings histogram
hist2 <- ggplot(manova_data, aes(x = Ratings_Num, fill = Product_Category)) +
  geom_histogram(bins = 5, alpha = 0.7, position = "dodge", color = "black") +
  scale_fill_met_d("Cassatt2") +
  labs(x = "Satisfaction Score", y = "Count", fill = "Product Category") +
  styleHistogram()

# Combine plots horizontally
combined_plot_hist <- hist1 | hist2

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

# Display final plot
cm_plot


### ─────────────────────────────────────────────
### 2. How does the co-purchasing behaviour across product categories influence customer ratings?
### ─────────────────────────────────────────────
#' @description
#' - Uses create_summary, plot_polar_chart, theme_setting functions in helper_functions

## a. Descriptive Analysis of Customer Ratings by Product Category
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
### 3. Can customer satisfaction be explained by a combination of brand preference and purchasing behavior across different product categories?
### ─────────────────────────────────────────────
#' @description
#' - Uses train_and_evaluate and bar_theme_setting functions in helper_functions

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
### 4. Are there clusters of customers who show consistent brand preferences, high purchase behavior, and high satisfaction?
### ─────────────────────────────────────────────
# i) Analysis technique - Unsupervised Learning (Clustering)
# a) Load required libraries
library(dplyr)
library(caret)
library(cluster)

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

### e) Evaluates K-means clustering quality using the average silhouette score
set.seed(123)
silhouette_sample_data <- customer_cluster_data_scaled[sample(nrow(customer_cluster_data_scaled), 10000), ]

sil_score <- function(k) {
  km <- kmeans(silhouette_sample_data, centers = k)
  mean(silhouette(km$cluster, dist(silhouette_sample_data))[, 3])
}

cat(sprintf("Avg Silhouette \n K=3: %.4f \n K=4: %.4f\n", sil_score(3), sil_score(4)))


## f) Perform K-means with the selected number of clusters (K=3) [Based on Elbow Method & average silhouette score]
kmeans_result <- kmeans(customer_cluster_data_scaled, centers = 3)
customer_cluster_data$cluster <- as.factor(kmeans_result$cluster)


## g) Cluster Profiling (Summary)
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