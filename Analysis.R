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
