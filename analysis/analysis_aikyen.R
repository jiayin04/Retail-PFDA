#### ANALYSIS STARTS HERE!!!

library(broom)        
library(forcats)     
library(arm)         
library(RColorBrewer)
library(scales)  
library(nnet) 

### Analysis 2 - 1: How does customer Age and Country affect customer satisfaction levels and product category choice?

## Show relationship between Age_Group, Country, Product Category
## Data preparation
# Step 1: Convert Age into ordered categorical variable
plot_data <- retail_data_proc %>%
  mutate(Age_Group = case_when(
    Age >= 18 & Age <= 25 ~ "18-25",
    Age >= 26 & Age <= 35 ~ "26-35",
    Age >= 36 & Age <= 45 ~ "36-45",
    Age >= 46 & Age <= 55 ~ "46-55",
    Age >= 56             ~ "56+",
    TRUE                  ~ NA_character_  # for missing or out-of-range values
  )) %>%
  mutate(Age_Group = factor(Age_Group, levels = c("18-25", "26-35", "36-45", "46-55", "56+"), ordered = TRUE))

# Step 2: Add a new Column that contains 'Rating' in binary value
plot_data <- plot_data %>%
  mutate(Ratings_Num = ifelse(Ratings == "High", 1, 0))

## Plotting for raw amount (Country age)
# Step 1: Summarize user counts
heatmap_data <- plot_data %>%
  filter(!is.na(Country) & !is.na(Age_Group)) %>%
  group_by(Country, Age_Group) %>%
  summarise(User_Count = n(), .groups = "drop")


# Step 2: Plot the heatmap
ggplot(heatmap_data, aes(x = Country, y = Age_Group, fill = User_Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = User_Count), color = "black", size = 3) +
  scale_fill_gradient(low = "#d1e5f0", high = "#2166ac") +
  labs(
    title = "Heatmap of User Count by Country and Age Group",
    x = "Country",
    y = "Age Group",
    fill = "User Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Plotting for (Country age ratings)
ratings_summary <- plot_data %>%
  group_by(Country, Age_Group) %>%
  summarise(
    Total = n(),
    High_Count = sum(Ratings == "High"),
    High_Proportion = High_Count / Total,
    .groups = "drop"
  )

ggplot(ratings_summary, aes(x = Country, y = High_Proportion, group = Age_Group, color = Age_Group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3.5) +
  labs(
    title = "Proportion of 'High' Ratings by Country and Age Group",
    x = "Country",
    y = "Proportion of High Ratings",
    color = "Age Group"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent)


## Plotting raw amount data (Country Age Product)
# Step 1: Create a new dataset with Total_Amount
plot_data_raw <- plot_data %>% mutate(Total_Amount = Purchase_Quantity * Amount)
plot_data_summary <- plot_data_raw %>%
  group_by(Product_Category, Age_Group, Country) %>%
  summarise(Total_Sales = sum(Total_Amount), .groups = "drop")

# Step 2: Create the dot and line plot
ggplot(plot_data_summary, aes(x = Product_Category, y = Total_Sales, color = Age_Group, group = Age_Group)) +
  geom_line(size = 0.7) +
  geom_point(size = 2.5) +
  facet_wrap(~ Country) +
  scale_y_continuous(labels = scales::comma) + 
  labs(
    title = "Sales by Product Category and Age Group, Faceted by Country",
    x = "Product Category",
    y = "Total Sales Amount",
    color = "Age Group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Data visualization with Age Group, Product Category, and Country
# Step 1: Summarize data (proportion of High ratings)
overall_plot_data <- plot_data %>%
  group_by(Age_Group, Product_Category, Ratings, Country) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Age_Group, Product_Category, Country) %>%
  mutate(Proportion = Count / sum(Count)) %>%
  filter(Ratings == "High")  # Plot only "High" ratings

# Step 2: Define custom color palette
age_colors <- c(
  "18-25" = "#E41A1C",
  "26-35" = "#377EB8",
  "36-45" = "#4DAF4A",
  "46-55" = "#984EA3",
  "56+"   = "#FF7F00"
)

# Step 3: Plot
ggplot(overall_plot_data, aes(x = Product_Category, y = Proportion, group = Age_Group, color = Age_Group)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 2.5) +
  scale_color_manual(values = age_colors) +
  labs(
    title = "Proportion of High Ratings by Product Category, Age Group, and Country",
    x = "Product Category",
    y = "Proportion of High Ratings",
    color = "Age Group"
  ) +
  facet_wrap(~ Country) +  # Add Country as facet
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



### Analysis 2 - 2: Is there a statistically significant relationship between customer satisfaction, product category, age, and country? 
## Calculate p value
## with Binary Logistic Regression
bayesglm_data <- downSample(
  x = plot_data[, c("Age_Group", "Product_Category", "Country")],
  y = plot_data$Ratings,
  yname = "Ratings"
)

# Convert to factor (optional if already factors)
bayesglm_data <- bayesglm_data %>%
  mutate(across(c(Age_Group, Product_Category, Country), as.factor))

## Bayesian
bayes_model <- bayesglm(Ratings ~ Age_Group + Country,
                        data = bayesglm_data,
                        family = binomial)
summary(bayes_model)

confint(bayes_model)  # Confidence intervals for the coefficients
exp(coef(bayes_model))           # Odds ratios
exp(confint(bayes_model))        # Confidence intervals on odds ratios

## Visualization
# Tidy the model output with confidence intervals and odds ratios
tidy_bayes <- tidy(bayes_model, conf.int = TRUE, exponentiate = TRUE)

# Plot horizontally
ggplot(tidy_bayes, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point(color = "steelblue", size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "gray40") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  labs(title = "Odds Ratios from Bayesian Logistic Regression",
       x = "Odds Ratio (with 95% CI)",
       y = "Predictor") +
  theme_minimal(base_size = 14)

## Multinom 
# Downsample for multinom (multiclass outcome: Product_Category)
multinom_data <- downSample(
  x = plot_data[, c("Age_Group", "Country")],
  y = plot_data$Product_Category,
  yname = "Product_Category"
)

# Convert to factor
multinom_data <- multinom_data %>%
  mutate(across(c(Age_Group, Country, Product_Category), as.factor))

# Fit the multinomial logistic regression model
multinom_model <- multinom(Product_Category ~ Age_Group + Country, data = multinom_data)
summary(multinom_model)

# Tidy up the model with exponentiated estimates and confidence intervals
tidy_multinom <- tidy(multinom_model, conf.int = TRUE, exponentiate = TRUE)

# Plot odds ratios with confidence intervals
ggplot(tidy_multinom, aes(x = estimate, y = reorder(term, estimate))) +
  geom_point(color = "darkorange", size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2, color = "gray50") +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Odds Ratios from Multinomial Logistic Regression",
    x = "Odds Ratio (95% CI)",
    y = "Predictor Term"
  ) +
  theme_minimal(base_size = 14)

z <- summary(multinom_model)$coefficients / summary(multinom_model)$standard.errors
p <- 2 * (1 - pnorm(abs(z)))  # two-tailed test
print(p)


### Analysis 2 – 3: How accurately can a customer age, country and product category predict customer satisfaction ratings? 
## A prediction when total amount is constant (fixed value), only uses shipping method
# Generate new data frame to predict
# Create new hypothetical data to predict on
new_data <- expand.grid(
  Age_Group = unique(balanced_data$Age_Group),
  Product_Category = unique(balanced_data$Product_Category),
  Country = unique(balanced_data$Country)
)

# Predict probabilities
new_data$Predicted_Prob <- predict(model, newdata = new_data, type = "response")

# Visualize predictions
ggplot(new_data, aes(x = Product_Category, y = Predicted_Prob, color = Age_Group)) +
  geom_point() +
  facet_wrap(~Country) + 
  labs(
    title = "Predicted Probability of High Rating",
    x = "Product Category",
    y = "Predicted Probability"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Confusion Matrix to check accuracy
# Step 1: Make predictions (probabilities)
pred_probs <- predict(model, type = "response")

# Step 2: Convert probabilities into class labels
# Assume threshold 0.5 for binary classification
pred_class <- ifelse(pred_probs >= 0.5, "High", "Low")

# Step 3: Create confusion matrix
# Convert both actual and predicted values to factors with same levels
actual <- factor(balanced_data$Ratings, levels = c("Low", "High"))
predicted <- factor(pred_class, levels = c("Low", "High"))

# Use caret package for nicer output (optional)
confusionMatrix(predicted, actual)

## ROC Curve
# Step 1: Convert actual labels to numeric (Low = 0, High = 1)
actual_numeric <- ifelse(actual == "High", 1, 0)

# Step 2: Compute ROC curve
roc_obj <- roc(actual_numeric, pred_probs)

# Step 3: Plot the ROC curve
plot(roc_obj, 
     main = "ROC Curve for Customer Satisfaction Prediction",
     col = "#2c7fb8", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray")  # diagonal line for random guessing

# Step 4: Print AUC (Area Under Curve)
auc_value <- auc(roc_obj)
cat("AUC:", auc_value, "\n")

### Analysis 2 – 4: What actionable improvements in shipping or pricing strategies could lead to better customer satisfaction? 
## Heat Map (face wrap via age group)
plot_data %>%
  mutate(Ratings_Num = ifelse(Ratings == "High", 1, 0)) %>%
  group_by(Country, Age_Group, Product_Category) %>%
  summarise(Avg_High = mean(Ratings_Num, na.rm = TRUE)) %>%
  ggplot(aes(x = Product_Category, y = Country, fill = Avg_High)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightgray", high = "darkblue") +
  facet_wrap(~ Age_Group) +
  labs(title = "Proportion of High Ratings by Age, Product Category, and Country",
       fill = "High Rating %",
       x = "Product Category", y = "Age Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## Dot Plot (Faceet wrap via country)
plot_data %>%
  mutate(Ratings_Num = ifelse(Ratings == "High", 1, 0)) %>%
  group_by(Country, Age_Group, Product_Category) %>%
  summarise(High_Prop = mean(Ratings_Num, na.rm = TRUE)) %>%
  ggplot(aes(x = Product_Category, y = Age_Group, size = High_Prop, color = High_Prop)) +
  geom_point(alpha = 0.8) +
  geom_text(aes(label = round(High_Prop * 100, 1)), 
            color = "black", size = 3, vjust = -1.5) +  # adjust vjust as needed
  facet_wrap(~ Country) +
  scale_size_continuous(range = c(2, 10)) +
  scale_color_gradient(low = "red", high = "green") +
  labs(
    title = "High Ratings by Product Category, Age Group, and Country",
    x = "Product Category",
    y = "Age Group",
    size = "High Rating %",
    color = "High Rating %"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))