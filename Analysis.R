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


#Analysis 1: Do customers in different countries rate their experiences 
#differently?


# View the structure of your dataset
data <- retail_data_proc
str(data)

# Load necessary libraries
library(ggplot2)
library(vcd)
library(rcompanion)

# Step 1: Convert 'Ratings' and 'Country' to character columns
data$Ratings <- as.character(data$Ratings)
data$Country <- as.character(data$Country)

# Step 2: Create the contingency table
table_ratings <- table(data$Country, data$Ratings)

# View the table to ensure no empty categories
print(table_ratings)

# Step 3: Check for zero counts in the table
# If you see zero counts, you can perform Fisher's Exact Test.
# Otherwise, you can use the Chi-square test.

# Step 4: Perform Chi-square test if no zero counts
if (all(table_ratings > 0)) {
  chisq_result <- chisq.test(table_ratings)
  # Output the Chi-square test result
  print(chisq_result)
} else {
  # Step 5: Perform Fisher's Exact Test if there are zero counts
  fisher_result <- fisher.test(table_ratings)
  print(fisher_result)
}

# Step 6: Visualize the distribution of ratings across countries
ggplot(data, aes(x = Country, fill = Ratings)) +
  geom_bar(position = "fill") +  # Proportional (relative) counts
  scale_y_continuous(labels = scales::percent) +  # Show percentages on y-axis
  labs(title = "Proportional Customer Ratings by Country", y = "Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Low" = "red", "High" = "blue"))


# Step 7: Visualize the contingency table as a heatmap
ggplot(as.data.frame(as.table(table_ratings)), aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  labs(title = "Heatmap of Ratings by Country") +
  theme_minimal() +
  scale_fill_gradient(low = "white", high = "blue")

# Step 8: Visualize the absolute counts of ratings across countries
ggplot(data, aes(x = Country, fill = Ratings)) +
  geom_bar(position = "stack") +  # Stacked bars for absolute counts
  labs(title = "Absolute Customer Ratings by Country", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Low" = "red", "High" = "blue"))

# Step 9: Visualize the total number of ratings per country
rating_count_per_country <- data %>%
  group_by(Country) %>%
  summarise(Total_Ratings = n())

ggplot(rating_count_per_country, aes(x = Country, y = Total_Ratings, fill = Country)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Number of Ratings per Country", y = "Total Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_viridis_d()






