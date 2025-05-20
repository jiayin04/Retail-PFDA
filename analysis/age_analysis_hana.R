# Analysis 1 : What are the behavioral & satisfaction patterns of diff cust age

library(dplyr)
library(ggplot2)
library(tidyr)


# retail_data_proc <- readr::read_csv("retail_data_proc.csv")

#Grouping age
retail_data_proc <- retail_data_proc %>% 
  mutate(Age_Group = cut(Age,
                         breaks = c(0, 25, 35, 45, 60, 100),
                         labels = c("Under 25", "25–34", "35–44", "45–59", "60+")))

#Calculate total amount
retail_data_proc <- retail_data_proc %>% 
  mutate(Total_Amount = Amount * Purchase_Quantity)

#Summarization 
summary_table <- retail_data_proc %>%
  group_by(Age_Group, Gender, Income, Customer_Segment, Ratings) %>%
  summarise(
    avg_total_amount = mean(Total_Amount, na.rm = TRUE),
    count = n()
  ) %>%
  arrange(desc(avg_total_amount))

print(summary_table)


#Faceted scatter plot for continuous age & total amount, ratings

# Convert Ratings 
retail_data_proc <- retail_data_proc %>%
  mutate(Ratings = ifelse(Ratings == "High", 1, 0))

# Convert data format
age_long <- retail_data_proc %>%
  pivot_longer(cols = c(Total_Amount, Ratings), names_to = "Measure", values_to = "Value")

# Plot (Faceted scatter)
ggplot(age_long, aes(x = Age, y = Value)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  facet_wrap(~ Measure, scales = "free_y") +
  labs(title = "Relationship Between Age and Total Amount / Ratings",
       x = "Age", y = "Value")


#Faceted Boxplot for age group & total amount, ratings

# Convert data format
age_bin_long <- retail_data_proc %>%
  pivot_longer(cols = c(Total_Amount, Ratings), names_to = "Measure", values_to = "Value")

# Plot (faceted boxplot)
ggplot(age_bin_long, aes(x = Age_Group, y = Value, fill = Measure)) +
  geom_boxplot() +
  facet_wrap(~ Measure, scales = "free_y") +
  labs(title = "Total Amount and Ratings by Age Group",
       x = "Age Group", y = "Value")


################## Extra feature for Analysis 1

#Heatmap with ratings, income, gender n age grp

heatmap_data <- retail_data_proc %>%
  group_by(Gender, Income, Ratings, Age_Group) %>%
  summarise(avg_total = mean(Total_Amount, na.rm = TRUE))

ggplot(heatmap_data, aes(x = Income, y = Gender, fill = avg_total)) +
  geom_tile() +
  facet_grid(Ratings ~ Age_Group)+
  #facet_wrap(~ Ratings) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Avg Total Amount spent by Gender, Income, Ratings, and Age Group",
       fill = "Avg Amount (RM)")


###################################################################################################

#Analysis 2 : Predicting satisfaction with age & purchasing behavior

library(randomForest)
library(caret)
library(pROC)
library(dplyr)

retail_data_proc <- readr::read_csv("retail_data_proc.csv")


# Use age only to predict ratings

# Convert Ratings to factor
retail_data_proc$Ratings <- as.factor(retail_data_proc$Ratings)

# Train-test split
set.seed(123)
train_index <- sample(1:nrow(retail_data_proc), 0.7 * nrow(retail_data_proc))
train_data <- retail_data_proc[train_index, ]
test_data <- retail_data_proc[-train_index, ]

#Select age, ratings
train_data <- train_data %>% select(Age, Ratings)
test_data <- test_data %>% select(Age, Ratings)

# Logistic Regression Model
model_log_age <- glm(Ratings ~ Age, data = train_data, family = binomial)
summary(model_log_age)

# Predict on test set
pred_log <- predict(model_log_age, newdata = test_data, type = "response")
pred_class <- ifelse(pred_log > 0.3613, "High", "Low")

# Confusion Matrix
confusionMatrix(factor(pred_class, levels = c("Low", "High")), test_data$Ratings)

# ROC for logistic regression
roc_log <- roc(test_data$Ratings, pred_log)
plot(roc_log, main = "ROC Curve - Logistic Regression (Age Only)")
auc(roc_log)



#Random Forest Model

#Random Forest using balanced data
balanced_train <- downSample(x = train_data[, "Age", drop = FALSE],
                             y = train_data$Ratings,
                             yname = "Ratings")

rf_model_age <- randomForest(Ratings ~ Age, data = balanced_train, ntree = 100)
print(rf_model_age)
varImpPlot(rf_model_age)

# Predict probabilities and classes 
rf_pred_prob <- predict(rf_model_age, newdata = test_data, type = "prob")[, 2]
rf_pred_class <- predict(rf_model_age, newdata = test_data)

# Confusion Matrix for Random Forest
confusionMatrix(rf_pred_class, test_data$Ratings)

# ROC Curve for Random Forest
test_labels_numeric <- ifelse(test_data$Ratings == "High", 1, 0)
roc_rf <- roc(test_labels_numeric, rf_pred_prob)
plot(roc_rf, main = "ROC Curve - Random Forest (Age Only)")
auc(roc_rf)

###################### Extra features for Analysis 2

#Factor conversion
retail_data_proc$Ratings <- as.factor(retail_data_proc$Ratings)
retail_data_proc$Gender <- as.factor(retail_data_proc$Gender)
retail_data_proc$Income <- as.factor(retail_data_proc$Income)
retail_data_proc$Customer_Segment <- as.factor(retail_data_proc$Customer_Segment)

#Train test
set.seed(123)  # for reproducibility
train_index <- sample(1:nrow(retail_data_proc), 0.7 * nrow(retail_data_proc))
train_data <- retail_data_proc[train_index, ]
test_data <- retail_data_proc[-train_index, ]

train_data <- train_data %>% select(Age, Gender, Income, Customer_Segment, Total_Amount, Ratings)
test_data <- test_data %>% select(Age, Gender, Income, Customer_Segment, Total_Amount, Ratings)


# Logistic regression model
model_log <- glm(Ratings ~ ., data = train_data, family = binomial)

summary(model_log)  # See model coefficients and significance

# Confusion matrix for Logistic regression
pred_log <- predict(model_log, newdata=test_data, type="response")
pred_class <- ifelse(pred_log > 0.3538, 1, 0)

confusionMatrix(factor(pred_class), 
                factor(ifelse(test_data$Ratings=="High", 1, 0)))

#ROC for Logistics regression model
roc_obj <- roc(test_data$Ratings, as.numeric(pred_log))
plot(roc_obj, main = "ROC Curve - Logistic Regression")
auc(roc_obj)


# Random Forest model

#Random Forest using balanced dataset
balanced_train <- upSample(x = train_data[, -which(names(train_data) == "Ratings")],
                           y = train_data$Ratings,
                           yname = "Ratings")

# Train model in balanced data
rf_model <- randomForest(Ratings ~ ., data = balanced_train, ntree = 100)

print(rf_model)
varImpPlot(rf_model)

# Predict on test data
predictions_rf <- predict(rf_model, newdata = test_data)

# Confusion matrix
confusionMatrix(predictions_rf, test_data$Ratings)

# ROC curve for Random Forest
# Convert High/Low to numeric (High = 1, Low = 0)
test_labels_numeric <- ifelse(test_data$Ratings == "High", 1, 0)
predictions_rf_prob <- predict(rf_model, newdata = test_data, type = "prob")[, 2]

roc_rf <- roc(test_labels_numeric, predictions_rf_prob)
plot(roc_rf, main = "ROC Curve - Random Forest")
auc(roc_rf)


################################################################################################

#Analysis 3 : Customer profiling based on behavior, satisfaction & age

library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)  # for clustering visualization
library(NbClust)     
library(fmsb)

retail_data_proc <- readr::read_csv("retail_data_proc.csv")

#Select only Age and Ratings
profile_data <- retail_data_proc %>%
  select(Age, Ratings)

# Convert Ratings to numeric
profile_data$Ratings <- ifelse(profile_data$Ratings == "High", 1, 0)

#Sample smaller set
set.seed(123)
profile_data <- profile_data %>% sample_n(2000)

#Scale numeric data
profile_data_scaled <- scale(profile_data)

#Determine optimal number of clusters (elbow method)
fviz_nbclust(profile_data_scaled, kmeans, method = "wss")

#KMeans clustering
set.seed(123)
kmeans_model <- kmeans(profile_data_scaled, centers = 3, nstart = 25)

#Add cluster labels
profile_data$Cluster <- factor(kmeans_model$cluster)

#Visualize clusters
fviz_cluster(kmeans_model, data = profile_data_scaled, geom = "point", ellipse.type = "norm",
             main = "KMeans Clustering - Age & Ratings Only (Sampled Data - 2000")

#Summarize clusters
summary_clusters <- profile_data %>%
  group_by(Cluster) %>%
  summarise(
    Avg_Age = mean(Age),
    Avg_Rating = mean(Ratings)
  )

print(summary_clusters)

# Visualisation: Age vs Ratings with centroids
# Calculate centroids
centroids <- profile_data %>%
  group_by(Cluster) %>%
  summarise(Age = mean(Age), Ratings = mean(Ratings))

# Plot
ggplot(profile_data, aes(x = Age, y = Ratings, color = Cluster)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_point(data = centroids, aes(x = Age, y = Ratings),
             color = "black", shape = 4, size = 4, stroke = 2) +  # centroids as X
  geom_text(data = centroids, aes(label = paste("Cluster", Cluster)),
            vjust = -1, color = "black", size = 4) +
  labs(title = "Customer Profilling Based on Age and Ratings (Sampled Data - 2000)",
       x = "Age",
       y = "Ratings (High = 1, Low = 0)",
       color = "Cluster") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")



############################ Extra features for Analysis 3

#Prepare Data
profile_data <- retail_data_proc %>%
  select(Age, Gender, Income, Customer_Segment, Total_Amount, Ratings)

#Convert Ratings to numeric
profile_data$Ratings <- ifelse(profile_data$Ratings == "High", 1, 0)

#Sample smaller data
set.seed(123)
profile_data <- profile_data %>% sample_n(2000)

#Creating dummy variables using model.matrix
dummy_vars <- model.matrix(~ Gender + Income + Customer_Segment - 1, data = profile_data) %>% as.data.frame()

#Combine into a single dataframe for clustering
profile_data_all <- bind_cols(
  profile_data %>% select(Age, Total_Amount, Ratings),
  dummy_vars
)

#Scale numeric data
profile_data_scaled <- scale(profile_data_all)

#Find optimal number of clusters
fviz_nbclust(profile_data_scaled, kmeans, method = "wss")

#K-Means clustering
set.seed(123)
kmeans_model <- kmeans(profile_data_scaled, centers = 3, nstart = 25)

#Add cluster labels to original data
profile_data_all$Cluster <- factor(kmeans_model$cluster)

# Visualize clusters
fviz_cluster(kmeans_model, data = profile_data_scaled, geom = "point", ellipse.type = "norm",
             main = "KMeans Clustering (Sampled Data - 2000)")

#Summarize profile by cluster
profile_summary <- profile_data_all %>%
  group_by(Cluster) %>%
  summarise(across(c(Age, Total_Amount, Ratings), mean),
            Gender_Male = mean(GenderMale),
            Income_Low = mean(IncomeLow),
            Income_Medium = mean(IncomeMedium),
            Customer_Segment_Premium = mean(Customer_SegmentPremium),
            Customer_Segment_Regular = mean(Customer_SegmentRegular))

print(profile_summary)

#Radar chart
profile_summary_scaled <- as.data.frame(scale(profile_summary[,-1]))
radar_data <- rbind(apply(profile_summary_scaled, 2, max),
                    apply(profile_summary_scaled, 2, min),
                    profile_summary_scaled)

colors_border <- c("red", "blue", "green")

radar_data

radarchart(radar_data,
           axistype = 1,
           pcol = colors_border,
           plwd = 2,
           plty = 1,
           cglcol = "grey", cglty = 1, axislabcol = "grey", caxislabels = seq(-2,2,1), cglwd = 0.8,
           vlcex = 0.8,
           title = "Customer Profile per Cluster (Sampled Data - 2000)")

legend(x = "topright", legend = c("Cluster 1", "Cluster 2", "Cluster 3"),
       bty = "n", pch=20, col=colors_border, text.col = "black", cex=0.8)



###############################################################################################

#Analysis 4 : Wwhat strategies can improve satisfaction in low rated but high value customer (age only)

library(dplyr)         
library(ggplot2)      
library(vcd) # Mosaic plot

retail_data_proc <- read_csv("retail_data_proc.csv")

# Violin Plot: Age vs Total Amount by Ratings
ggplot(retail_data_proc, aes(x = Age_Group, y = Total_Amount, fill = Ratings)) +
  geom_violin(position = "dodge", trim = FALSE) +
  labs(
    title = "Spending Distribution by Ratings across Age Groups",
    x = "Age Group",
    y = "Total Amount Spent"
  ) +
  theme_minimal()

# Density Plot: Total Amount Distribution by Ratings per Age Group
ggplot(retail_data_proc, aes(x = Total_Amount, fill = Ratings)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Age_Group) +
  labs(
    title = "Density Plot: Total Amount by Ratings and Age Group",
    x = "Total Amount Spent"
  ) +
  theme_minimal()


################################# Extra features for Analysis 4

retail_data_proc$Ratings <- as.factor(retail_data_proc$Ratings)
retail_data_proc$Gender <- as.factor(retail_data_proc$Gender)
retail_data_proc$Income <- as.factor(retail_data_proc$Income)
retail_data_proc$Customer_Segment <- as.factor(retail_data_proc$Customer_Segment)
retail_data_proc$Age_Group <- as.factor(retail_data_proc$Age_Group)

# Define high-value threshold and subset
high_value_cutoff <- median(retail_data_proc$Total_Amount)
low_rate_high_value <- retail_data_proc %>%
  filter(Ratings == "Low" & Total_Amount >= high_value_cutoff)

# Chi-square: Gender, Income, Customer Segment vs Ratings
chisq.test(table(retail_data_proc$Gender, retail_data_proc$Ratings))
chisq.test(table(retail_data_proc$Income, retail_data_proc$Ratings))
chisq.test(table(retail_data_proc$Customer_Segment, retail_data_proc$Ratings))

#Mosaic plots Gender, Income, Customer Segment
mosaic(~ Gender + Ratings, data = retail_data_proc, shade = TRUE, main = "Gender vs Ratings")
mosaic(~ Income + Ratings, data = retail_data_proc, shade = TRUE, main = "Income vs Ratings")
mosaic(~ Customer_Segment + Ratings, data = retail_data_proc, shade = TRUE, main = "Customer Segment vs Ratings")

#Bar chart profiling low-rated high-value customers
ggplot(low_rate_high_value, aes(x = Customer_Segment, fill = Income)) +
  geom_bar(position = "dodge") +
  #geom_bar(position = "fill") +  #see which ver u want to use
  facet_wrap(~ Gender) +
  labs(title = "Low-Rated High-Value Customers by Segment, Income, and Gender",
       x = "Customer Segment", y = "Count") +
  theme_minimal()

#The boxplot all look very similar
# Boxplot: Total Amount vs Ratings across Age Group and Income
ggplot(retail_data_proc, aes(x = Ratings, y = Total_Amount, fill = Ratings)) +
  geom_boxplot() +
  facet_grid(Age_Group ~ Income) +
  labs(title = "Total Amount by Ratings across Age and Income",
       y = "Total Amount Spent") +
  theme_minimal()


















