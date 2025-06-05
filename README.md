# 🛒 Programming for Data Analysis – Retail Customer Insights

## 📌 Project Overview

This project focuses on the preprocessing, exploration, and analysis of a retail dataset. It includes cleaning steps, feature engineering, statistical analysis, visualizations, and predictive modeling to uncover customer behavior and purchase patterns.

------------------------------------------------------------------------

## ✅ Requirements

To run this project, ensure the following are installed:

-   **R** (version ≥ 4.0)
-   **RStudio**
-   Required R packages (automatically handled in `libraries.R`)

------------------------------------------------------------------------

## 🚀 Setup Instructions

1.  **Clone the Repository**

    ``` bash
    git clone https://github.com/jiayin04/Retail-PFDA.git
    ```

2.  Open RStudio, navigate to the project folder.

3.  Run the **main** file

``` bash
source("main.R")
```

## Project File Structure

``` bash
Retail-PFDA/
├── main.R                   # Main script that sources other files and consist of the analysis 
├── Dashboard.R              # GUI dashboard that display part of the analysis 
├── utils/ 
│   ├── libraries.R          # Package installation and loading 
│   └── helper_functions.R   # Reusable helper functions 
├── preprocessing/ 
│   └── data_cleaning.R      # Data preprocessing and cleaning functions 
├── analysis/ 
│   ├── product_analysis_jiayin.R    # Analysis for Product 
│   ├── age_analysis_hana.R          # Analysis for Age
│   ├── country_analysis_junheng.R   # Analysis for Country
│   ├── analysis_aikyen.R            # Analysis for Transaction Logistics
│   ├── group_predictive_analysis.R  # Analysis for Group ML 
```
