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

# 1. What is the distribution of payment and shipping methods among 
# customers, and how does purchase quantity vary across them?

# Load required libraries
library(ggplot2)
library(dplyr)

# View the structure of your dataset
data <- retail_data_proc

str(data)

# a. Frequency of Payment and Shipping Methods
table(data$Payment_Method)
table(data$Shipping_Method)

#b. Show proportions
prop.table(table(data$Payment_Method)) * 100
prop.table(table(data$Shipping_Method)) * 100

# c. Summary of Purchase Quantity
summary(data$Purchase_Quantity)

# d. Purchase Quantity by Payment Method
data %>%
  group_by(Payment_Method) %>%
  summarise(
    count = n(),
    mean_purchase = mean(Purchase_Quantity, na.rm = TRUE),
    sd_purchase = sd(Purchase_Quantity, na.rm = TRUE),
    median_purchase = median(Purchase_Quantity, na.rm = TRUE)
  )

# e. Purchase Quantity by Shipping Method
data %>%
  group_by(Shipping_Method) %>%
  summarise(
    count = n(),
    mean_purchase = mean(Purchase_Quantity, na.rm = TRUE),
    sd_purchase = sd(Purchase_Quantity, na.rm = TRUE),
    median_purchase = median(Purchase_Quantity, na.rm = TRUE)
  )

# f. Bar Plot: Distribution of Payment Methods
ggplot(data, aes(x = Payment_Method)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Payment Methods", x = "Payment Method", y = "Count") +
  theme_minimal()

# g. Bar Plot: Distribution of Shipping Methods
ggplot(data, aes(x = Shipping_Method)) +
  geom_bar(fill = "seagreen") +
  labs(title = "Distribution of Shipping Methods", x = "Shipping Method", y = "Count") +
  theme_minimal()

# h. Boxplot: Purchase Quantity by Payment Method
ggplot(data, aes(x = Payment_Method, y = Purchase_Quantity)) +
  geom_boxplot(fill = "darkorange") +
  labs(title = "Purchase Quantity by Payment Method", x = "Payment Method", y = "Purchase Quantity") +
  theme_minimal()

# i. Boxplot: Purchase Quantity by Shipping Method
ggplot(data, aes(x = Shipping_Method, y = Purchase_Quantity)) +
  geom_boxplot(fill = "mediumpurple") +
  labs(title = "Purchase Quantity by Shipping Method", x = "Shipping Method", y = "Purchase Quantity") +
  theme_minimal()

#j. Density plot: Purchase Quantity by Payment Method
ggplot(data, aes(x = Purchase_Quantity, fill = Payment_Method)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Purchase Quantity by Payment Method",
       x = "Purchase Quantity", y = "Density") +
  theme_minimal()

#k. Density Plot: Purchase Quantity by Shipping Method
ggplot(data, aes(x = Purchase_Quantity, fill = Shipping_Method)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Purchase Quantity by Shipping Method",
       x = "Purchase Quantity", y = "Density") +
  theme_minimal()

#2. Do different payment methods lead to significantly 
#different purchasing behaviors?

library(ggplot2)
library(car)

#a. Boxplot helps spot variance visually
ggplot(data, aes(x = Payment_Method, y = Purchase_Quantity)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Boxplot: Purchase Quantity by Payment Method") +
  theme_minimal()

#b. One-way Anova model
anova_result <- aov(Purchase_Quantity ~ Payment_Method, data = data)
summary(anova_result)

#c. Levene's Test to check homogeneity of variance
leveneTest(Purchase_Quantity ~ Payment_Method, data = data)

#d. Post-hoc test
TukeyHSD(anova_result)


#3. Do payment and shipping methods, and their interaction,
# significantly influence purchase quantity?

#Load libraries
library(ggplot2)
library(dplyr)
library(car)  # For Levene's Test

#a. Boxplot
ggplot(data, aes(x = interaction(Payment_Method, Shipping_Method), y = Purchase_Quantity)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Purchase Quantity by Payment & Shipping Methods",
       x = "Payment x Shipping Method", y = "Purchase Quantity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#b. Two-way Anova model 
anova2 <- aov(Purchase_Quantity ~ Payment_Method * Shipping_Method, data = data)
summary(anova2)

#c. Levene's Test for Equal Variance 
leveneTest(Purchase_Quantity ~ Payment_Method * Shipping_Method, data = data)

#d. Interaction Plot for visualizing 
interaction.plot(x.factor =  data$Payment_Method,
                 trace.factor = data$Shipping_Method,
                 response = data$Purchase_Quantity,
                 fun = mean,
                 type = "b",
                 col = c("red", "blue", "green", "purple"),
                 pch = 19,
                 xlab = "Payment Method",
                 ylab = "Mean Purchase Quantity",
                 trace.label = "Shipping Method",
                 main = "Interaction Plot: Payment x Shipping Method")


#4. Is there a significant association between the choice of 
#payment method and shipping method?

#a. Create a contingency table
table_4 <- table(data$Payment_Method, data$Shipping_Method)

#b. View the table
print(table_4)

#c. Run chi-square test of independence
chisq_result <- chisq.test(table_4)

#d. View results
print(chisq_result)

#Load libaries
library(ggplot2)
library(dplyr)

#e. Create a frequency table
heatmap_data <- as.data.frame(table(data$Payment_Method, data$Shipping_Method))
colnames(heatmap_data) <- c("Payment_Method", "Shipping_Method", "Frequency")

#f. Plot heatmap
ggplot(heatmap_data, aes(x = Payment_Method, y = Shipping_Method, fill = Frequency)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Heatmap of Payment vs Shipping Methods", x = "Payment Method", y = "Shipping Method") +
  theme_minimal()




