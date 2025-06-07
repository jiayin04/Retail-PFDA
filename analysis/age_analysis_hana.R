# Analysis 1 : What are the behavioral & satisfaction patterns of diff cust age

library(dplyr) #Data manipulation (summary, grouping, selecting)
library(ggplot2) #Data visualisation (Make nice looking plots)
library(tidyr) #Tidy up data (turn multiple column into long format w one column)
library(readr) #To read file

# retail_data_proc <- read_csv("retail_data_proc.csv")

#Creating age group and total amt
retail_data_proc_temp <- retail_data_proc %>% 
  mutate(
    Age_Group   = cut(Age, 
                      breaks = c(0, 25, 35, 45, 60, 100),
                      labels = c("Under 25", "25–34", "35–44", "45–59", "60+")),
    Total_Amount = Amount * Purchase_Quantity
  )

#Saving the csv
write.csv(retail_data_proc_temp, "retail_data_proc_temp.csv")

#Summarization 
summary_table <- retail_data_proc_temp %>%
  group_by(Age_Group, Gender, Income, Customer_Segment, Ratings) %>%
  summarise(
    avg_total_amount = mean(Total_Amount, na.rm = TRUE),
    count            = n(),
    .groups          = "drop" #Ungroup data back after summary
  ) %>%
  arrange(desc(avg_total_amount))

print(summary_table)


# Convert ratings (scatter and boxplot)
age_data <- retail_data_proc_temp %>%
  mutate(Ratings_Num = ifelse(Ratings == "High", 1, 0)) %>%
  #Combining total amt and rating into single column 
  pivot_longer( 
    cols      = c(Total_Amount, Ratings_Num),
    names_to  = "Measure",
    values_to = "Value"
  )

#Faceted scatter plot for continuous age & total amount, ratings
age_scatter_plot <- ggplot(age_data, aes(x = Age, y = Value)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  #Splits the plot by measure column (ttl amt, rating)
  facet_wrap(~ Measure, scales = "free_y") +
  labs(
    title = "Relationship Between Age and Total Amount / Ratings",
    x     = "Age",
    y     = "Value"
  )

age_scatter_plot

#Faceted Boxplot for age group & total amount, ratings
ggplot(age_data, aes(x = Age_Group, y = Value, fill = Measure)) +
  geom_boxplot() +
  facet_wrap(~ Measure, scales = "free_y") +
  labs(
    title = "Total Amount and Ratings by Age Group",
    x     = "Age Group",
    y     = "Value"
  )

################## Extra feature for Analysis 1

#Heatmap with ratings, income, gender n age grp
heatmap_data <- retail_data_proc_temp %>%
  group_by(Gender, Income, Ratings, Age_Group) %>%
  summarise(
    avg_total = mean(Total_Amount, na.rm = TRUE),
    .groups   = "drop"
  )

ggplot(heatmap_data, aes(x = Income, y = Gender, fill = avg_total)) +
  geom_tile() +
  facet_grid(Ratings ~ Age_Group) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Avg Total Amount spent by Gender, Income, Ratings, and Age Group",
    fill  = "Avg Amount (RM)"
  ) +
  theme_minimal()

#########################################################################################

# Analysis 2 : Predicting satisfaction with age & purchasing behavior
library(randomForest)
library(caret) #Splitting data, evaluate model performance, sampling
library(pROC) #ROC curve, calculate AUC
library(dplyr)
library(readr)

retail_data_proc_temp <- read_csv("retail_data_proc_temp.csv")

# Use age only to predict ratings

age_mod_data <- retail_data_proc_temp %>%
  select(Age, Ratings) %>%
  mutate(Ratings = factor(Ratings, levels = c("Low","High")))

# Train-test split (0.7 training, 0.3 testing)
set.seed(123)
train_idx_age <- sample(nrow(age_mod_data), 0.7 * nrow(age_mod_data))
age_train <- age_mod_data[train_idx_age, ]
age_test  <- age_mod_data[-train_idx_age, ]

# Logistic Regression Model
model_log_age <- glm(Ratings ~ Age, data = age_train, family = binomial)
summary(model_log_age)

# Predict on test set
pred_log_age_prob  <- predict(model_log_age, newdata = age_test, type = "response")
pred_log_age_class <- ifelse(pred_log_age_prob > 0.6256, "High", "Low")

# Confusion Matrix
confusionMatrix(factor(pred_log_age_class, levels = c("Low","High")),age_test$Ratings)

# ROC for logistic regression
roc_age_log <- pROC::roc(age_test$Ratings, pred_log_age_prob)
plot(roc_age_log, main = "ROC Curve – Logistic (Age Only)")
auc(roc_age_log)


#Random Forest Model

#Random Forest using balanced data
balanced_age_train <- downSample(x = age_train["Age"],
                                 y = age_train$Ratings,
                                 yname = "Ratings")

rf_age_model <- randomForest(Ratings ~ Age, data  = balanced_age_train, ntree = 100)
print(rf_age_model)
varImpPlot(rf_age_model)

# Predict probabilities and classes 
rf_age_prob  <- predict(rf_age_model, newdata = age_test, type = "prob")[,2]
rf_age_class <- predict(rf_age_model, newdata = age_test)

# Confusion Matrix for Random Forest
confusionMatrix(rf_age_class, age_test$Ratings)

# ROC Curve for Random Forest
roc_age_rf <- pROC::roc(response = ifelse(age_test$Ratings == "High", 1, 0),
                  predictor = rf_age_prob)
plot(roc_age_rf, main = "ROC Curve – Random Forest (Age Only)")
auc(roc_age_rf)


###################### Extra features for Analysis 2
#Factor conversion
full_mod_data <- retail_data_proc_temp %>%
  select(Age, Gender, Income, Customer_Segment, Total_Amount, Ratings) %>%
  mutate(
    Ratings          = factor(Ratings, levels = c("Low","High")),
    Gender           = factor(Gender),
    Income           = factor(Income),
    Customer_Segment = factor(Customer_Segment)
  )

# Train-test split
set.seed(123)
train_idx_full <- sample(nrow(full_mod_data), 0.7 * nrow(full_mod_data))
full_train <- full_mod_data[train_idx_full, ]
full_test  <- full_mod_data[-train_idx_full, ]

# Logistic regression model
model_log_full <- glm(Ratings ~ .,data = full_train, family = binomial)
summary(model_log_full)

pred_log_full_prob  <- predict(model_log_full,newdata = full_test,type= "response")
pred_log_full_class <- ifelse(pred_log_full_prob > 0.5755, "High", "Low")

# Confusion matrix for Logistic regression
confusionMatrix(
  factor(pred_log_full_class, levels = c("Low","High")),
  full_test$Ratings
)

#ROC for Logistics regression model
roc_full_log <- pROC::roc(full_test$Ratings, pred_log_full_prob)
plot(roc_full_log, main = "ROC Curve – Logistic Regression")
auc(roc_full_log)


# Random Forest model

#Random Forest using balanced dataset
balanced_full_train <- upSample(x = full_train %>% select(-Ratings),
                                y = full_train$Ratings,
                                yname = "Ratings")

# Train model in balanced data
rf_full_model <- randomForest(Ratings ~ .,data = balanced_full_train, ntree = 100)

print(rf_full_model)
varImpPlot(rf_full_model)

rf_full_prob  <- predict(rf_full_model, newdata = full_test,type = "prob")[,2]
rf_full_class <- predict(rf_full_model,newdata = full_test)

# Confusion matrix
confusionMatrix(rf_full_class, full_test$Ratings)

# ROC curve for Random Forest
roc_full_rf <- pROC::roc(as.numeric(full_test$Ratings) - 1, rf_full_prob)
plot(roc_full_rf, main = "ROC Curve – Random Forest")
auc(roc_full_rf)

################################################################################################

#Analysis 3 : Customer profiling based on behavior, satisfaction & age
library(dplyr)
library(ggplot2)
library(factoextra)  # for clustering visualization
library(fmsb) #Radar chart

retail_data_proc_temp <- readr::read_csv("retail_data_proc_temp.csv")

#Select only Age and Ratings
profile_data <- retail_data_proc_temp %>%
  select(Age, Ratings)

# Convert Ratings to numeric
profile_data$Ratings <- ifelse(profile_data$Ratings == "High", 1, 0)

#Sample smaller set
set.seed(123)
profile_data <- profile_data %>% sample_n(10000)

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
             main = "KMeans Clustering - Age & Ratings Only (Sampled Data - 10000")

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
plot_customer_profiling <- ggplot(profile_data, aes(x = Age, y = Ratings, color = Cluster)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_point(data = centroids, aes(x = Age, y = Ratings),
             color = "black", shape = 4, size = 4, stroke = 2) +  # centroids as X
  geom_text(data = centroids, aes(label = paste("Cluster", Cluster)),
            vjust = -1, color = "black", size = 4) +
  labs(title = "Customer Profilling Based on Age and Ratings (Sampled Data - 10000)",
       x = "Age",
       y = "Ratings (High = 1, Low = 0)",
       color = "Cluster") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

print(plot_customer_profiling)

############################ Extra features for Analysis 3

#Prepare Data
profile_data <- retail_data_proc_temp %>%
  select(Age, Gender, Income, Customer_Segment, Total_Amount, Ratings)

#Convert Ratings to numeric
profile_data$Ratings <- ifelse(profile_data$Ratings == "High", 1, 0)

#Sample smaller data
set.seed(123)
profile_data <- profile_data %>% sample_n(10000)

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
             main = "KMeans Clustering (Sampled Data - 10000)")

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
           title = "Customer Profile per Cluster (Sampled Data - 10000)")

legend(x = "topright", legend = c("Cluster 1", "Cluster 2", "Cluster 3"),
       bty = "n", pch=20, col=colors_border, text.col = "black", cex=0.8)



###############################################################################################


#Analysis 4 : Wwhat strategies can improve satisfaction in low rated but high value customer (age only)

library(dplyr)         
library(ggplot2)      
library(vcd) # Mosaic plot
library(readr)

retail_data_proc_temp <- read_csv("retail_data_proc_temp.csv")

# Violin Plot: Age vs Total Amount by Ratings
ggplot(retail_data_proc_temp, aes(x = Age_Group, y = Total_Amount, fill = Ratings)) +
  geom_violin(position = "dodge", trim = FALSE) +
  labs(
    title = "Spending Distribution by Ratings across Age Groups",
    x = "Age Group",
    y = "Total Amount Spent"
  ) +
  theme_minimal()

# Density Plot: Total Amount Distribution by Ratings per Age Group
plot_density_age_group <- ggplot(retail_data_proc_temp, aes(x = Total_Amount, fill = Ratings)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~Age_Group) +
  labs(
    title = "Density Plot: Total Amount by Ratings and Age Group",
    x = "Total Amount Spent"
  ) +
  theme_minimal()

print(plot_density_age_group)

################################# Extra features for Analysis 4

# Create a copy of the dataset
retail_data_factor <- retail_data_proc_temp

retail_data_factor$Ratings <- as.factor(retail_data_factor$Ratings)
retail_data_factor$Gender <- as.factor(retail_data_factor$Gender)
retail_data_factor$Income <- as.factor(retail_data_factor$Income)
retail_data_factor$Customer_Segment <- as.factor(retail_data_factor$Customer_Segment)
retail_data_factor$Age_Group <- as.factor(retail_data_factor$Age_Group)

# Define high-value threshold and subset
high_value_cutoff <- median(retail_data_factor$Total_Amount)
low_rate_high_value <- retail_data_factor %>%
  filter(Ratings == "Low" & Total_Amount >= high_value_cutoff)

# Chi-square: Gender, Income, Customer Segment vs Ratings
chisq.test(table(retail_data_factor$Gender, retail_data_factor$Ratings))
chisq.test(table(retail_data_factor$Income, retail_data_factor$Ratings))
chisq.test(table(retail_data_factor$Customer_Segment, retail_data_factor$Ratings))

# Mosaic plots
mosaic(~ Gender + Ratings, data = retail_data_factor, shade = TRUE, main = "Gender vs Ratings")
mosaic(~ Income + Ratings, data = retail_data_factor, shade = TRUE, main = "Income vs Ratings")
mosaic(~ Customer_Segment + Ratings, data = retail_data_factor, shade = TRUE, main = "Customer Segment vs Ratings")

# Bar chart profiling low-rated high-value customers
ggplot(low_rate_high_value, aes(x = Customer_Segment, fill = Income)) +
  geom_bar(position = "fill") +
  #geom_bar(position = "dodge") +  #diff ver of visualisation
  facet_wrap(~ Gender) +
  labs(title = "Low-Rated High-Value Customers by Segment, Income, and Gender",
       x = "Customer Segment", y = "Count") +
  theme_minimal()

# Boxplot: Total Amount vs Ratings across Age Group and Income
ggplot(retail_data_factor, aes(x = Ratings, y = Total_Amount, fill = Ratings)) +
  geom_boxplot() +
  facet_grid(Age_Group ~ Income) +
  labs(title = "Total Amount by Ratings across Age and Income",
       y = "Total Amount Spent") +
  theme_minimal()
