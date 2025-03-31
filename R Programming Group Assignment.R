# Load library
require(dplyr)
require(tidyr)
require(readr)

# Set working directory
WD <- "~/Custom Office Templates/Academic/Degree/Year 2 Sem 1/PfDA/Assignment" # Change to your WD
setwd(WD)

# Import dataset
file_path <- file.path(WD, "retail_data.csv")
retail_data <- read.csv(file_path)
View(retail_data)




# Validate dataset to make sure it had cleaned
str(retail_data)
summary(retail_data)
