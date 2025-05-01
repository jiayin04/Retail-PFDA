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

# 1. Frequency of Payment and Shipping Methods
table(data$Payment_Method)
table(data$Shipping_Method)

# Optional: Show proportions
prop.table(table(data$Payment_Method)) * 100
prop.table(table(data$Shipping_Method)) * 100

# 2. Summary of Purchase Quantity
summary(data$Purchase_Quantity)

# 3. Purchase Quantity by Payment Method
data %>%
  group_by(Payment_Method) %>%
  summarise(
    count = n(),
    mean_purchase = mean(Purchase_Quantity, na.rm = TRUE),
    sd_purchase = sd(Purchase_Quantity, na.rm = TRUE),
    median_purchase = median(Purchase_Quantity, na.rm = TRUE)
  )

# 4. Purchase Quantity by Shipping Method
data %>%
  group_by(Shipping_Method) %>%
  summarise(
    count = n(),
    mean_purchase = mean(Purchase_Quantity, na.rm = TRUE),
    sd_purchase = sd(Purchase_Quantity, na.rm = TRUE),
    median_purchase = median(Purchase_Quantity, na.rm = TRUE)
  )

# 5. Bar Plot: Distribution of Payment Methods
ggplot(data, aes(x = Payment_Method)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Payment Methods", x = "Payment Method", y = "Count") +
  theme_minimal()

# 6. Bar Plot: Distribution of Shipping Methods
ggplot(data, aes(x = Shipping_Method)) +
  geom_bar(fill = "seagreen") +
  labs(title = "Distribution of Shipping Methods", x = "Shipping Method", y = "Count") +
  theme_minimal()

# 7. Boxplot: Purchase Quantity by Payment Method
ggplot(data, aes(x = Payment_Method, y = Purchase_Quantity)) +
  geom_boxplot(fill = "darkorange") +
  labs(title = "Purchase Quantity by Payment Method", x = "Payment Method", y = "Purchase Quantity") +
  theme_minimal()

# 8. Boxplot: Purchase Quantity by Shipping Method
ggplot(data, aes(x = Shipping_Method, y = Purchase_Quantity)) +
  geom_boxplot(fill = "mediumpurple") +
  labs(title = "Purchase Quantity by Shipping Method", x = "Shipping Method", y = "Purchase Quantity") +
  theme_minimal()
