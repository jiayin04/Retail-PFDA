# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(viridis)
library(shinycssloaders)
library(randomForest)
library(caret)
library(pROC)
library(MetBrewer)
library(tidyr)
library(patchwork)
library(cluster)
library(factoextra)
library(NbClust)
library(fmsb)
library(purrr)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Customer Analytics Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Customer Satisfaction Analysis", 
               tabName = "satisfaction", 
               icon = icon("star")),
      menuItem("Age Analysis", 
               tabName = "age_analysis", 
               icon = icon("user")),
      menuItem("Product Analysis", 
               tabName = "product_analysis", 
               icon = icon("shopping-cart")),
      menuItem("Teammate Analysis 3", 
               tabName = "analysis3", 
               icon = icon("chart-pie"))
    )
  ),
  
  dashboardBody(
    # Custom CSS for better styling
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 5px;
        }
        .nav-tabs-custom > .nav-tabs > li.active {
          border-top-color: #3c8dbc;
        }
      "))
    ),
    
    tabItems(
      # Customer Satisfaction Analysis Tab
      tabItem(tabName = "satisfaction",
              fluidRow(
                box(
                  title = "Customer Satisfaction Analysis by Country", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  p("This analysis examines customer satisfaction ratings patterns across different countries, 
              identifies statistical differences, and provides actionable recommendations for improvement.")
                )
              ),
              
              # Navigation tabs within the satisfaction analysis
              fluidRow(
                box(
                  width = 12,
                  tabsetPanel(
                    id = "satisfaction_tabs",
                    
                    # Descriptive Analysis Tab
                    tabPanel("Descriptive Analysis",
                             fluidRow(
                               column(6,
                                      box(
                                        title = "Rating Distribution by Country",
                                        status = "info",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotlyOutput("rating_distribution_plot"))
                                      )
                               ),
                               column(6,
                                      box(
                                        title = "Average Ratings by Country",
                                        status = "info", 
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotlyOutput("avg_rating_plot"))
                                      )
                               )
                             ),
                             fluidRow(
                               column(6,
                                      box(
                                        title = "Rating Heatmap",
                                        status = "success",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotlyOutput("heatmap_plot"))
                                      )
                               ),
                               column(6,
                                      box(
                                        title = "Country Statistics Summary",
                                        status = "success",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(DT::dataTableOutput("country_stats_table"))
                                      )
                               )
                             )
                    ),
                    
                    # Statistical Analysis Tab
                    tabPanel("Statistical Analysis",
                             fluidRow(
                               column(6,
                                      box(
                                        title = "Chi-Square Test Results",
                                        status = "warning",
                                        solidHeader = TRUE,
                                        width = 12,
                                        h4("Test for Independence of Ratings and Country"),
                                        verbatimTextOutput("chisq_results"),
                                        br(),
                                        p("The Chi-square test examines whether customer satisfaction ratings 
                        are independent of country location.")
                                      )
                               ),
                               column(6,
                                      box(
                                        title = "ANOVA Results",
                                        status = "warning",
                                        solidHeader = TRUE,
                                        width = 12,
                                        h4("Analysis of Variance"),
                                        verbatimTextOutput("anova_results"),
                                        br(),
                                        p("ANOVA tests if there are significant differences in mean ratings 
                        between countries.")
                                      )
                               )
                             ),
                             fluidRow(
                               box(
                                 title = "Tukey HSD Post-Hoc Analysis",
                                 status = "primary",
                                 solidHeader = TRUE,
                                 width = 12,
                                 h4("Pairwise Country Comparisons"),
                                 verbatimTextOutput("tukey_results"),
                                 br(),
                                 p("Tukey HSD identifies which specific country pairs have 
                      significantly different rating distributions.")
                               )
                             )
                    ),
                    
                    # Predictive Analysis Tab
                    tabPanel("Predictive Analysis",
                             fluidRow(
                               column(6,
                                      box(
                                        title = "Random Forest Model Performance",
                                        status = "success",
                                        solidHeader = TRUE,
                                        width = 12,
                                        h4("Model Accuracy Metrics"),
                                        withSpinner(DT::dataTableOutput("model_performance_table")),
                                        br(),
                                        verbatimTextOutput("confusion_matrix")
                                      )
                               ),
                               column(6,
                                      box(
                                        title = "Country Rating Predictions",
                                        status = "success",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotlyOutput("prediction_plot"))
                                      )
                               )
                             ),
                             fluidRow(
                               box(
                                 title = "Balanced Dataset Distribution",
                                 status = "info",
                                 solidHeader = TRUE,
                                 width = 12,
                                 withSpinner(plotlyOutput("balanced_data_plot"))
                               )
                             )
                    ),
                    
                    # Recommendations Tab
                    tabPanel("Recommendations",
                             fluidRow(
                               column(12,
                                      box(
                                        title = "Current vs Target Satisfaction Rates",
                                        status = "primary",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotlyOutput("recommendations_plot"))
                                      )
                               ),
                               column(6,
                                      box(
                                        title = "Action Plan Summary",
                                        status = "primary",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(DT::dataTableOutput("action_summary_table"))
                                      )
                               )
                             ),
                             fluidRow(
                               column(5,
                                      box(
                                        title = "Country Performance Levels",
                                        status = "warning",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(DT::dataTableOutput("recommendations_table"))
                                      )
                               ),
                               column(7,
                                      box(
                                        title = "Priority Countries",
                                        status = "danger",
                                        solidHeader = TRUE,
                                        width = 20,
                                        h4("Top 3 Countries Needing Immediate Attention"),
                                        withSpinner(DT::dataTableOutput("priority_countries_table"))
                                      )
                               )
                             )
                    )
                  )
                )
              )
      ),
      
      # Age Analysis Tab
      tabItem(tabName = "age_analysis",
              fluidRow(
                box(
                  title = "Age-Based Customer Analysis", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  p("This analysis examines customer behavior and satisfaction patterns across different age groups.")
                )
              ),
              
              fluidRow(
                box(
                  width = 12,
                  tabsetPanel(
                    id = "age_tabs",
                    
                    # Age Distribution Analysis
                    tabPanel("Age Distribution",
                             fluidRow(
                               column(6,
                                      box(
                                        title = "Age vs Total Amount",
                                        status = "info",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotlyOutput("age_amount_plot"))
                                      )
                               ),
                               column(6,
                                      box(
                                        title = "Age vs Ratings",
                                        status = "info",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotlyOutput("age_ratings_plot"))
                                      )
                               )
                             ),
                             fluidRow(
                               column(12,
                                      box(
                                        title = "Age Group Analysis",
                                        status = "success",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotlyOutput("age_group_plot"))
                                      )
                               )
                             ),
                        
                    ),
                    
                    # Age-Based Customer Profiling
                    tabPanel("Customer Profiling",
                             fluidRow(
                               column(12,
                                      box(
                                        title = "Age-Based Clusters",
                                        status = "warning",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotlyOutput("age_cluster_plot"))
                                      )
                               ),
                               column(7,
                                      box(
                                        title = "Cluster Summary",
                                        status = "warning",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(DT::dataTableOutput("age_cluster_summary"))
                                      )
                               )
                             )
                    )
                  )
                )
              )
      ),
      
      # Product Analysis Tab
      tabItem(tabName = "product_analysis",
              fluidRow(
                box(
                  title = "Product Preference Analysis", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  p("This analysis examines product preferences, co-purchasing behavior, and brand preferences.")
                )
              ),
              
              fluidRow(
                box(
                  width = 12,
                  tabsetPanel(
                    id = "product_tabs",
                    
                    # Product Preference Analysis
                    tabPanel("Product Preferences",
                             fluidRow(
                               column(6,
                                      box(
                                        title = "Product Category Analysis",
                                        status = "info",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotlyOutput("product_category_plot"))
                                      )
                               ),
                               column(6,
                                      box(
                                        title = "Brand Preference Analysis",
                                        status = "info",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotlyOutput("brand_preference_plot"))
                                      )
                               )
                             ),
                             fluidRow(
                               column(12,
                                      box(
                                        title = "Customer Ratings Across Product Segment (Radar Chart)",
                                        status = "primary",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotlyOutput("product_radar_plot"))
                                      )
                               )
                             )
                    ),
                    
                    # Co-Purchasing Analysis
                    tabPanel("Co-Purchasing",
                             fluidRow(
                               column(12,
                                      box(
                                        title = "Co-Purchase Heatmap",
                                        status = "success",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotlyOutput("co_purchase_heatmap"))
                                      )
                               )
                             ),
                             fluidRow(
                               column(12,
                                      box(
                                        title = "Top Co-Purchases (Bar Plot)",
                                        status = "success",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotlyOutput("co_purchase_bar_plot"))
                                      )
                               )
                             )
                    ),
                    
                    # Product-Based Clustering
                    tabPanel("Customer Clustering",
                             fluidRow(
                               column(5,
                                      box(
                                        title = "Product-Based Clusters",
                                        status = "warning",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotlyOutput("product_cluster_plot"))
                                      )
                               ),
                               column(7,
                                      box(
                                        title = "Cluster Characteristics",
                                        status = "warning",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(DT::dataTableOutput("product_cluster_summary"))
                                      )
                               )
                             )
                    ),
                    tabPanel("Random Forest Prediction",
                             fluidRow(
                               column(12,
                                      box(
                                        title = "Random Forest ROC Curve (Balanced vs Imbalanced)",
                                        status = "success",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotlyOutput("product_rf_roc_plot"))
                                      )
                               )
                             )
                    )
                  )
                )
              )
      ),
      
      # Teammate Analysis 3 Tab
      tabItem(tabName = "analysis3",
              fluidRow(
                box(
                  title = "Teammate Analysis 3", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  h3("Analysis Title: [To be filled by teammate]"),
                  p("This section will contain the third teammate's analysis."),
                  br(),
                  div(
                    style = "text-align: center; padding: 50px;",
                    h4("Content will be added by Teammate 3", style = "color: #999;"),
                    icon("chart-pie", style = "font-size: 48px; color: #ddd;")
                  )
                )
              ),
              fluidRow(
                column(4,
                       box(
                         title = "Metric 1",
                         status = "warning",
                         solidHeader = TRUE,
                         width = 12,
                         div(
                           style = "height: 200px; background-color: #f9f9f9; border: 2px dashed #ddd; 
                         display: flex; align-items: center; justify-content: center;",
                           p("Metric 1", style = "color: #999; font-size: 16px;")
                         )
                       )
                ),
                column(4,
                       box(
                         title = "Metric 2",
                         status = "warning",
                         solidHeader = TRUE,
                         width = 12,
                         div(
                           style = "height: 200px; background-color: #f9f9f9; border: 2px dashed #ddd; 
                         display: flex; align-items: center; justify-content: center;",
                           p("Metric 2", style = "color: #999; font-size: 16px;")
                         )
                       )
                ),
                column(4,
                       box(
                         title = "Metric 3",
                         status = "warning",
                         solidHeader = TRUE,
                         width = 12,
                         div(
                           style = "height: 200px; background-color: #f9f9f9; border: 2px dashed #ddd; 
                         display: flex; align-items: center; justify-content: center;",
                           p("Metric 3", style = "color: #999; font-size: 16px;")
                         )
                       )
                )
              )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Load and prepare data
  data <- retail_data_proc
  data$Ratings <- as.character(data$Ratings)
  data$Country <- as.character(data$Country)
  data$Ratings_numeric <- ifelse(data$Ratings == "High", 1, 5)
  
  # Create Age_Group variable
  data <- data %>% 
    mutate(
      Age_Group = cut(Age,
                     breaks = c(0, 25, 35, 45, 60, 100),
                     labels = c("Under 25", "25–34", "35–44", "45–59", "60+")),
      Total_Amount = Amount * Purchase_Quantity
    )
  
  # Create contingency table for statistical tests
  table_ratings <- table(data$Country, data$Ratings)
  
  # Descriptive Analysis Outputs
  output$rating_distribution_plot <- renderPlotly({
    p <- ggplot(data, aes(x = Country, fill = Ratings)) +
      geom_bar(position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Proportional Customer Ratings by Country", 
           y = "Percentage", x = "Country") +
      theme_minimal() +
      scale_fill_manual(values = c("Low" = "#E69F00", "High" = "#56B4E9")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  output$avg_rating_plot <- renderPlotly({
    avg_ratings <- data %>%
      group_by(Country) %>%
      summarise(Average_Rating = mean(Ratings_numeric, na.rm = TRUE))
    
    p <- ggplot(avg_ratings, aes(x = reorder(Country, Average_Rating), 
                                 y = Average_Rating, fill = Country)) +
      geom_col() +
      labs(title = "Average Customer Rating by Country", 
           y = "Average Rating", x = "Country") +
      theme_minimal() +
      scale_fill_viridis_d() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  output$heatmap_plot <- renderPlotly({
    heatmap_data <- as.data.frame(as.table(table_ratings))
    
    p <- ggplot(heatmap_data, aes(Var1, Var2, fill = Freq)) +
      geom_tile() +
      labs(title = "Rating Frequency Heatmap", 
           x = "Country", y = "Rating", fill = "Count") +
      theme_minimal() +
      scale_fill_gradient(low = "white", high = "blue") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  output$country_stats_table <- DT::renderDataTable({
    rating_stats <- data %>%
      group_by(Country) %>%
      summarise(
        Count = n(),
        Mean = round(mean(Ratings_numeric, na.rm = TRUE), 2),
        Median = median(Ratings_numeric, na.rm = TRUE),
        SD = round(sd(Ratings_numeric, na.rm = TRUE), 2),
        Min = min(Ratings_numeric, na.rm = TRUE),
        Max = max(Ratings_numeric, na.rm = TRUE),
        High_Rating_Pct = round(sum(Ratings == "High")/n() * 100, 1)
      ) %>%
      arrange(desc(Mean))
    
    DT::datatable(rating_stats, 
                  options = list(pageLength = 10, scrollX = TRUE),
                  caption = "Statistical Summary by Country")
  })
  
  # Statistical Analysis Outputs
  output$chisq_results <- renderText({
    if (all(table_ratings > 0)) {
      chisq_country_result <- chisq.test(table_ratings)
      paste(
        "Chi-squared Test Results:\n",
        "Chi-squared statistic:", round(chisq_country_result$statistic, 4), "\n",
        "Degrees of freedom:", chisq_country_result$parameter, "\n",
        "P-value:", format(chisq_country_result$p.value, scientific = TRUE), "\n\n",
        "Interpretation:", 
        ifelse(chisq_country_result$p.value < 0.05, 
               "There is a significant association between country and ratings (p < 0.05)",
               "No significant association between country and ratings (p >= 0.05)")
      )
    } else {
      fisher_country_result <- fisher.test(table_ratings)
      paste(
        "Fisher's Exact Test Results:\n",
        "P-value:", format(fisher_country_result$p.value, scientific = TRUE), "\n\n",
        "Interpretation:", 
        ifelse(fisher_country_result$p.value < 0.05, 
               "There is a significant association between country and ratings (p < 0.05)",
               "No significant association between country and ratings (p >= 0.05)")
      )
    }
  })
  
  output$anova_results <- renderText({
    anova_country_rating <- aov(Ratings_numeric ~ Country, data = data)
    anova_summary <- summary(anova_country_rating)
    
    f_stat <- anova_summary[[1]][["F value"]][1]
    p_value <- anova_summary[[1]][["Pr(>F)"]][1]
    
    paste(
      "ANOVA Results:\n",
      "F-statistic:", round(f_stat, 4), "\n",
      "P-value:", format(p_value, scientific = TRUE), "\n\n",
      "Interpretation:",
      ifelse(p_value < 0.05,
             "Significant differences exist between countries (p < 0.05)",
             "No significant differences between countries (p >= 0.05)")
    )
  })
  
  output$tukey_results <- renderText({
    anova_model <- aov(Ratings_numeric ~ Country, data = data)
    tukey_result <- TukeyHSD(anova_model)
    
    # Extract significant pairs
    tukey_df <- as.data.frame(tukey_result$Country)
    significant_pairs <- tukey_df[tukey_df$`p adj` < 0.05, ]
    
    if(nrow(significant_pairs) > 0) {
      paste(
        "Significant country pairs (p < 0.05):\n",
        paste(rownames(significant_pairs), 
              " (p-adj = ", round(significant_pairs$`p adj`, 4), ")",
              collapse = "\n")
      )
    } else {
      "No significant pairwise differences found between countries."
    }
  })
  
  # Predictive Analysis Outputs
  output$model_performance_table <- DT::renderDataTable({
    # Prepare data for Random Forest
    raw_data_country <- data %>%
      select(Ratings, Country) %>%
      na.omit()
    
    raw_data_country$Ratings <- as.factor(raw_data_country$Ratings)
    raw_data_country$Country <- as.factor(raw_data_country$Country)
    
    # Create balanced dataset
    set.seed(123)
    sample_size_country <- min(500, min(table(raw_data_country$Country)))
    balanced_data_country <- data.frame()
    
    for(country in levels(raw_data_country$Country)) {
      country_data <- raw_data_country[raw_data_country$Country == country, ]
      if(nrow(country_data) <= sample_size_country) {
        sampled_data_country <- country_data
      } else {
        sampled_data_country <- country_data[sample(1:nrow(country_data), sample_size_country), ]
      }
      balanced_data_country <- rbind(balanced_data_country, sampled_data_country)
    }
    
    # Split data and train model
    train_index_country <- createDataPartition(balanced_data_country$Ratings, p = 0.8, list = FALSE)
    train_data_country <- balanced_data_country[train_index_country, ]
    test_data_country <- balanced_data_country[-train_index_country, ]
    
    rf_country_ratings_model <- randomForest(
      Ratings ~ Country,
      data = train_data_country,
      ntree = 50,
      importance = TRUE
    )
    
    predictions_country <- predict(rf_country_ratings_model, test_data_country)
    conf_matrix_country <- confusionMatrix(predictions_country, test_data_country$Ratings)
    
    # Create performance metrics table
    performance_metrics <- data.frame(
      Metric = c("Accuracy", "Sensitivity", "Specificity", "Precision"),
      Value = c(
        round(conf_matrix_country$overall["Accuracy"], 3),
        round(conf_matrix_country$byClass["Sensitivity"], 3),
        round(conf_matrix_country$byClass["Specificity"], 3),
        round(conf_matrix_country$byClass["Pos Pred Value"], 3)
      ),
      Description = c(
        "Overall prediction accuracy",
        "True positive rate (High ratings correctly identified)",
        "True negative rate (Low ratings correctly identified)", 
        "Precision of High rating predictions"
      )
    )
    
    DT::datatable(performance_metrics,
                  options = list(pageLength = 5, dom = 't'),
                  caption = "Random Forest Model Performance")
  })
  
  output$confusion_matrix <- renderText({
    # Prepare data for Random Forest
    raw_data_country <- data %>%
      select(Ratings, Country) %>%
      na.omit()
    
    raw_data_country$Ratings <- as.factor(raw_data_country$Ratings)
    raw_data_country$Country <- as.factor(raw_data_country$Country)
    
    # Create balanced dataset and train model
    set.seed(123)
    sample_size_country <- min(500, min(table(raw_data_country$Country)))
    balanced_data_country <- data.frame()
    
    for(country in levels(raw_data_country$Country)) {
      country_data <- raw_data_country[raw_data_country$Country == country, ]
      if(nrow(country_data) <= sample_size_country) {
        sampled_data_country <- country_data
      } else {
        sampled_data_country <- country_data[sample(1:nrow(country_data), sample_size_country), ]
      }
      balanced_data_country <- rbind(balanced_data_country, sampled_data_country)
    }
    
    train_index_country <- createDataPartition(balanced_data_country$Ratings, p = 0.8, list = FALSE)
    train_data_country <- balanced_data_country[train_index_country, ]
    test_data_country <- balanced_data_country[-train_index_country, ]
    
    rf_country_ratings_model <- randomForest(
      Ratings ~ Country,
      data = train_data_country,
      ntree = 50,
      importance = TRUE
    )
    
    predictions_country <- predict(rf_country_ratings_model, test_data_country)
    conf_matrix_country <- confusionMatrix(predictions_country, test_data_country$Ratings)
    
    paste(
      "Confusion Matrix:\n",
      "           Predicted\n",
      "Actual     High  Low\n",
      "High       ", conf_matrix_country$table[1,1], "  ", conf_matrix_country$table[1,2], "\n",
      "Low        ", conf_matrix_country$table[2,1], "  ", conf_matrix_country$table[2,2], "\n\n",
      "Model trained on balanced dataset with equal representation from each country."
    )
  })
  
  output$prediction_plot <- renderPlotly({
    # Prepare data for Random Forest
    raw_data_country <- data %>%
      select(Ratings, Country) %>%
      na.omit()
    
    raw_data_country$Ratings <- as.factor(raw_data_country$Ratings)
    raw_data_country$Country <- as.factor(raw_data_country$Country)
    
    # Create balanced dataset and train model
    set.seed(123)
    sample_size_country <- min(500, min(table(raw_data_country$Country)))
    balanced_data_country <- data.frame()
    
    for(country in levels(raw_data_country$Country)) {
      country_data <- raw_data_country[raw_data_country$Country == country, ]
      if(nrow(country_data) <= sample_size_country) {
        sampled_data_country <- country_data
      } else {
        sampled_data_country <- country_data[sample(1:nrow(country_data), sample_size_country), ]
      }
      balanced_data_country <- rbind(balanced_data_country, sampled_data_country)
    }
    
    rf_country_ratings_model <- randomForest(
      Ratings ~ Country,
      data = balanced_data_country,
      ntree = 50,
      importance = TRUE
    )
    
    # Calculate prediction probabilities for each country
    country_pred <- data.frame()
    for(country in levels(balanced_data_country$Country)) {
      country_test <- data.frame(Country = factor(country, levels = levels(balanced_data_country$Country)))
      pred <- predict(rf_country_ratings_model, country_test, type = "prob")
      country_pred <- rbind(country_pred, 
                           data.frame(Country = country, 
                                    High_Rating_Probability = pred[, "High"]))
    }
    
    p <- ggplot(country_pred, aes(x = reorder(Country, High_Rating_Probability),
                                  y = High_Rating_Probability, fill = Country)) +
      geom_col() +
      scale_y_continuous(labels = scales::percent) +
      labs(title = "Predicted High Rating Probability by Country",
           x = "Country", y = "Probability of High Rating") +
      theme_minimal() +
      scale_fill_viridis_d() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  output$balanced_data_plot <- renderPlotly({
    # Show the concept of balanced dataset
    sample_sizes <- data %>%
      group_by(Country) %>%
      summarise(Original_Count = n()) %>%
      mutate(Balanced_Count = min(500, min(Original_Count)))  # Use actual balanced count
    
    sample_long <- sample_sizes %>%
      tidyr::pivot_longer(cols = c(Original_Count, Balanced_Count),
                          names_to = "Dataset", values_to = "Count")
    
    p <- ggplot(sample_long, aes(x = Country, y = Count, fill = Dataset)) +
      geom_col(position = "dodge") +
      labs(title = "Original vs Balanced Dataset Sample Sizes",
           x = "Country", y = "Number of Records") +
      theme_minimal() +
      scale_fill_manual(values = c("Original_Count" = "#FF6B6B", "Balanced_Count" = "#4ECDC4")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Recommendations Outputs
  output$recommendations_plot <- renderPlotly({
    country_summary <- data %>%
      group_by(Country) %>%
      summarise(
        Satisfaction_Rate = round(mean(Ratings == "High") * 100, 1)
      ) %>%
      mutate(
        Performance_Level = case_when(
          Satisfaction_Rate >= 70 ~ "Good",
          Satisfaction_Rate >= 50 ~ "Average", 
          TRUE ~ "Needs Improvement"
        ),
        Target_Satisfaction = case_when(
          Performance_Level == "Needs Improvement" ~ pmin(Satisfaction_Rate + 30, 85),
          Performance_Level == "Average" ~ 75,
          TRUE ~ pmin(Satisfaction_Rate + 5, 90)
        )
      )
    
    p <- ggplot(country_summary, aes(x = reorder(Country, Satisfaction_Rate))) +
      geom_col(aes(y = Satisfaction_Rate, fill = Performance_Level), alpha = 0.7) +
      geom_point(aes(y = Target_Satisfaction), color = "red", size = 3) +
      geom_segment(aes(xend = Country, y = Satisfaction_Rate, yend = Target_Satisfaction),
                   color = "red", linetype = "dashed") +
      coord_flip() +
      labs(title = "Current vs Target Satisfaction Rates",
           subtitle = "Red dots show improvement targets",
           x = "Country", y = "Satisfaction Rate (%)",
           fill = "Performance Level") +
      theme_minimal() +
      scale_fill_manual(values = c("Good" = "#2ECC71", "Average" = "#F39C12", "Needs Improvement" = "#E74C3C"))
    
    ggplotly(p)
  })
  
  output$recommendations_table <- DT::renderDataTable({
    country_summary <- data %>%
      group_by(Country) %>%
      summarise(
        Total_Customers = n(),
        High_Ratings = sum(Ratings == "High"),
        Satisfaction_Rate = round((High_Ratings / Total_Customers) * 100, 1)
      ) %>%
      mutate(
        Performance_Level = case_when(
          Satisfaction_Rate >= 70 ~ "Good",
          Satisfaction_Rate >= 50 ~ "Average",
          TRUE ~ "Needs Improvement"
        ),
        Recommended_Action = case_when(
          Performance_Level == "Needs Improvement" ~ "Priority: Immediate service improvement needed",
          Performance_Level == "Average" ~ "Focus: Targeted improvements to reach 70%+",
          TRUE ~ "Maintain: Continue current good practices"
        )
      ) %>%
      select(Country, Satisfaction_Rate, Performance_Level, Recommended_Action)
    
    DT::datatable(country_summary,
                  options = list(pageLength = 10, scrollX = TRUE),
                  caption = "Country Performance and Recommendations") %>%
      DT::formatStyle("Performance_Level",
                      backgroundColor = DT::styleEqual(
                        c("Good", "Average", "Needs Improvement"),
                        c("#d4edda", "#fff3cd", "#f8d7da")
                      ))
  })
  
  output$action_summary_table <- DT::renderDataTable({
    action_summary <- data %>%
      group_by(Country) %>%
      summarise(
        Satisfaction_Rate = round(mean(Ratings == "High") * 100, 1)
      ) %>%
      mutate(
        Performance_Level = case_when(
          Satisfaction_Rate >= 70 ~ "Good",
          Satisfaction_Rate >= 50 ~ "Average",
          TRUE ~ "Needs Improvement"
        ),
        Recommended_Action = case_when(
          Performance_Level == "Needs Improvement" ~ "Priority: Immediate service improvement needed",
          Performance_Level == "Average" ~ "Focus: Targeted improvements to reach 70%+",
          TRUE ~ "Maintain: Continue current good practices"
        )
      ) %>%
      count(Performance_Level, Recommended_Action) %>%
      rename(Countries_Count = n)
    
    DT::datatable(action_summary,
                  options = list(pageLength = 5, dom = 't'),
                  caption = "Action Plan Summary")
  })
  
  output$priority_countries_table <- DT::renderDataTable({
    priority_countries <- data %>%
      group_by(Country) %>%
      summarise(
        Satisfaction_Rate = round(mean(Ratings == "High") * 100, 1),
        Total_Customers = n()
      ) %>%
      arrange(Satisfaction_Rate) %>%
      head(3) %>%
      mutate(
        Priority = paste("Priority", 1:3),
        Action_Needed = "Immediate service improvement program"
      ) %>%
      select(Priority, Country, Satisfaction_Rate, Total_Customers, Action_Needed)
    
    DT::datatable(priority_countries,
                  options = list(pageLength = 5, dom = 't'),
                  caption = "Top 3 Priority Countries") %>%
      DT::formatStyle("Satisfaction_Rate",
                      backgroundColor = "#f8d7da",
                      color = "#721c24")
  })
  
  # Age Analysis Outputs
  output$age_amount_plot <- renderPlotly({
    age_data <- data %>%
      group_by(Age_Group) %>%
      summarise(
        Avg_Amount = mean(Total_Amount),
        Count = n()
      )
    
    p <- ggplot(age_data, aes(x = Age_Group, y = Avg_Amount, fill = Age_Group)) +
      geom_col() +
      labs(
        title = "Average Spending by Age Group",
        x = "Age Group",
        y = "Average Total Amount"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  output$age_ratings_plot <- renderPlotly({
    p <- ggplot(data, aes(x = Age_Group, y = Ratings_numeric, fill = Ratings)) +
      geom_boxplot() +
      labs(
        title = "Ratings Distribution by Age Group",
        x = "Age Group",
        y = "Rating"
      )
    
    ggplotly(p)
  })
  
  output$age_group_plot <- renderPlotly({
    p <- ggplot(data, aes(x = Age_Group, y = Total_Amount, fill = Ratings)) +
      geom_violin(position = "dodge", trim = FALSE) +
      labs(
        title = "Spending Distribution by Ratings across Age Groups",
        x = "Age Group",
        y = "Total Amount Spent"
      )
    
    ggplotly(p)
  })
  
  output$age_cluster_plot <- renderPlotly({
    # Prepare data for clustering and plotting
    data_with_ratings <- data %>%
      mutate(Ratings_numeric = ifelse(Ratings == "High", 1, 0))
    
    # Prepare matrix for clustering
    cluster_matrix <- as.matrix(dplyr::select(as.data.frame(data_with_ratings), Age, Ratings_numeric))
    cluster_matrix <- scale(cluster_matrix)
    
    # Perform k-means clustering
    set.seed(123)
    kmeans_result <- kmeans(cluster_matrix, centers = 3)
    
    # Add cluster labels to the data frame
    data_with_ratings <- data_with_ratings %>%
      mutate(Cluster = factor(kmeans_result$cluster))
    
    # Calculate centroids
    centroids <- data_with_ratings %>%
      group_by(Cluster) %>%
      summarise(Age = mean(Age), Ratings = mean(Ratings_numeric))
    
    p <- ggplot(data_with_ratings, aes(x = Age, y = Ratings_numeric, color = Cluster)) +
      geom_point(alpha = 0.6, size = 2) +
      geom_point(data = centroids, aes(x = Age, y = Ratings),
                 color = "black", shape = 4, size = 4, stroke = 2, inherit.aes = FALSE) +
      geom_text(data = centroids, aes(x = Age, y = Ratings, label = paste("Cluster", Cluster)),
                vjust = -1, color = "black", size = 4, inherit.aes = FALSE) +
      labs(title = "Customer Profiling Based on Age and Ratings",
           x = "Age",
           y = "Ratings (High = 1, Low = 0)",
           color = "Cluster") +
      theme_minimal() +
      scale_color_brewer(palette = "Set1")
    
    ggplotly(p)
  })
  
  output$age_cluster_summary <- DT::renderDataTable({
    # Prepare data for clustering and summary
    data_with_ratings <- data %>%
      mutate(Ratings_numeric = ifelse(Ratings == "High", 1, 0))
    
    cluster_matrix <- as.matrix(dplyr::select(as.data.frame(data_with_ratings), Age, Ratings_numeric, Total_Amount))
    cluster_matrix <- scale(cluster_matrix)
    
    # Perform k-means clustering
    set.seed(123)
    kmeans_result <- kmeans(cluster_matrix, centers = 3)
    
    # Add cluster labels to a new data frame
    data_with_ratings <- data_with_ratings %>%
      mutate(Cluster = factor(kmeans_result$cluster))
    
    # Create cluster summary
    cluster_summary <- data_with_ratings %>%
      group_by(Cluster) %>%
      summarise(
        Avg_Age = mean(Age),
        Avg_Amount = mean(Total_Amount),
        Avg_Rating = mean(Ratings_numeric),
        Count = n(),
        .groups = "drop"
      )
    
    DT::datatable(cluster_summary,
                  options = list(pageLength = 5, dom = 't'),
                  caption = "Age-Based Cluster Summary")
  })
  
  # Product Analysis Outputs
  output$product_category_plot <- renderPlotly({
    p <- ggplot(data, aes(x = Product_Category, y = Purchase_Quantity, fill = Ratings)) +
      geom_violin(trim = FALSE, alpha = 0.5) +
      geom_boxplot(width = 0.1, position = position_dodge(width = 0.9),
                  outlier.shape = NA, color = "black", alpha = 0.8) +
      labs(
        title = "Purchase Quantity Across Product Categories by Customer Ratings",
        x = "Product Category",
        y = "Purchase Quantity"
      )
    
    ggplotly(p)
  })
  
  output$brand_preference_plot <- renderPlotly({
    brand_summary <- data %>%
      group_by(Product_Brand, Ratings) %>%
      summarise(
        Avg_Purchase = mean(Purchase_Quantity),
        Count = n(),
        .groups = "drop"
      )
    
    p <- ggplot(brand_summary, aes(x = Product_Brand, y = Avg_Purchase, fill = Ratings)) +
      geom_col(position = "dodge") +
      labs(
        title = "Brand Preferences by Ratings",
        x = "Product Brand",
        y = "Average Purchase Quantity"
      )
    
    ggplotly(p)
  })

  output$product_radar_plot <- renderPlotly({

    ratings_summary_cat <- create_summary(data, "Product_Category")
    ratings_summary_brand <- create_summary(data, "Product_Brand")

    # [Polar Chart]
    # Category
    cat_radar <- theme_setting(plot_polar_chart(ratings_summary_cat, "Product_Category"), "Product Category")
    # Brand
    brand_radar <- theme_setting(plot_polar_chart(ratings_summary_brand, "Product_Brand"), "Product Brand")

    # final plot
    combined_radar_plot <- (cat_radar | brand_radar) + # Use patchwork for combining
      plot_annotation(
        title = "Distribution of Customer Ratings Across Product Segment",
        subtitle = "Visual comparison of rating count, total purchase, and average customer age by category and brand",
        caption = "Source: Retail Dataset | Units scaled for readability",
        theme = theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5), # Center title
                      plot.subtitle = element_text(size = 11, hjust = 0.5)) # Center subtitle
      )

    ggplotly(combined_radar_plot) # Wrap in ggplotly
  })
  
  output$co_purchase_heatmap <- renderPlotly({
    # Create product pairs based on Product_Category per customer and rating
    product_pairs <- data %>%
      group_by(Customer_ID, Ratings) %>%
      summarise(Categories = list(unique(Product_Category)), .groups = "drop") %>%
      filter(lengths(Categories) > 1) %>%
      mutate(Pairs = map(Categories, ~combn(.x, 2, simplify = FALSE))) %>%
      unnest(Pairs) %>%
      mutate(pair = map_chr(Pairs, ~paste(sort(.x), collapse = " & ")))

    # Count co-purchases by rating
    pair_by_rating <- product_pairs %>%
      group_by(Ratings, pair) %>%
      count(name = "n", sort = TRUE) %>%
      ungroup()

    # Pivot to wide format to select top pairs by total count
    pair_by_rating_wide <- pair_by_rating %>%
      pivot_wider(names_from = Ratings, values_from = n, values_fill = 0)

    # Select top 15 pairs by total count (High + Low)
    pair_by_rating_top <- pair_by_rating_wide %>%
      mutate(Total = rowSums(across(where(is.numeric)))) %>%
      arrange(desc(Total)) %>%
      slice_head(n = 15)

    # Prepare symmetric heatmap input
    pair_matrix <- pair_by_rating_top %>%
      separate(pair, into = c("Product1", "Product2"), sep = " & ") %>%
      pivot_longer(cols = c(High, Low), names_to = "Rating", values_to = "Count")

    # Create the heatmap plot
    heatmap_plot <- ggplot(pair_matrix, aes(x = Product1, y = Product2, fill = Count)) +
      geom_tile(color = "white", linewidth = 0.4) +
      facet_wrap(~Rating) +
      scale_fill_met_c(name = "Monet") + # Using MetBrewer Monet palette
      labs(
        title = "Heatmap of Co-Purchase Counts by Product Category",
        subtitle = "Top 15 pairs shown, separated by customer rating group",
        x = "Product A",
        y = "Product B",
        fill = "Co-Purchase Count"
      ) +
      guides(fill = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = 10,
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

    ggplotly(heatmap_plot)
  })

  output$co_purchase_bar_plot <- renderPlotly({
     # Create product pairs based on Product_Category per customer and rating
    product_pairs <- data %>%
      group_by(Customer_ID, Ratings) %>%
      summarise(Categories = list(unique(Product_Category)), .groups = "drop") %>%
      filter(lengths(Categories) > 1) %>%
      mutate(Pairs = map(Categories, ~combn(.x, 2, simplify = FALSE))) %>%
      unnest(Pairs) %>%
      mutate(pair = map_chr(Pairs, ~paste(sort(.x), collapse = " & ")))

    # Count co-purchases by rating
    pair_by_rating <- product_pairs %>%
      group_by(Ratings, pair) %>%
      count(name = "n", sort = TRUE) %>%
      ungroup()

     # Pivot to wide format to select top pairs by total count
    pair_by_rating_wide <- pair_by_rating %>%
      pivot_wider(names_from = Ratings, values_from = n, values_fill = 0)

    # Select top 15 pairs by total count (High + Low)
    pair_by_rating_top <- pair_by_rating_wide %>%
      mutate(Total = rowSums(across(where(is.numeric)))) %>%
      arrange(desc(Total)) %>%
      slice_head(n = 15)

    # Pivot to long format for bar plot
    pair_by_rating_top_long <- pair_by_rating_top %>%
      pivot_longer(cols = c(High, Low), names_to = "Rating", values_to = "Count")

    # Create the bar plot
    bar_plot <- ggplot(pair_by_rating_top_long, aes(x = reorder(pair, Count), y = Count, fill = Rating)) + # Reorder by Count
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      geom_text(aes(label = Count), 
                position = position_dodge(width = 0.7), 
                hjust = -0.1, 
                size = 3.3, 
                color = "black") +
      coord_flip() +
      scale_fill_met_d(name = "Hokusai3") + # Using MetBrewer Hokusai3 palette
      labs(
        title = "Top Co-Purchased Product Category Pairs by Rating",
        subtitle = "Comparison of frequent co-purchases between High and Low rating customers",
        x = "Product Category Pair",
        y = "Co-Purchase Count"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 13),
        axis.text.x = element_text(color = "grey20", size = 11),
        axis.text.y = element_text(color = "grey20", size = 10),
        legend.position = "bottom"
      ) +
      expand_limits(y = max(pair_by_rating_top_long$Count, na.rm = TRUE) * 1.2) # Adjust expand_limits

    ggplotly(bar_plot)
  })

  # Product-based clustering
  output$product_cluster_plot <- renderPlotly({
    # Prepare data for clustering and plotting
    data_for_clustering <- data %>%
      mutate(Ratings_numeric = ifelse(Ratings == "High", 1, 0)) %>%
      select(Ratings, Product_Brand, Purchase_Quantity, Product_Category) # Select only relevant columns for clustering/plotting

    # Convert factors to numeric for clustering
    data_for_clustering$Product_Brand_num <- as.numeric(data_for_clustering$Product_Brand)
    data_for_clustering$Product_Category_num <- as.numeric(data_for_clustering$Product_Category)
    # Ratings_numeric is not used for clustering input, but needed for potential future cluster analysis/summary

    # Remove rows with NAs introduced by factor conversion or existing NAs
    data_for_clustering <- na.omit(data_for_clustering)

    # Select only the numeric variables for scaling and clustering (3 variables as in product_analysis_jiayin.R)
    data_scaled <- data_for_clustering %>%
      select(Product_Brand_num, Product_Category_num, Purchase_Quantity) %>% # Use the 3 variables
      scale()

    # Perform k-means clustering (using scaled data)
    set.seed(123)
    # Check if there are enough rows for clustering after na.omit
    if (nrow(data_scaled) < 3) {
      # Handle case where there are not enough data points for clustering
      return(plotly() %>% layout(title = "Not enough data to perform clustering"))
    }
    kmeans_result <- kmeans(data_scaled, centers = 3)

    # Add cluster labels back to the original-like data frame
    data_for_clustering$cluster <- factor(kmeans_result$cluster)

    # Perform PCA for Dimensional Reduction (using the same 3 scaled variables)
    pca_result <- prcomp(data_scaled)
    pca_data <- data.frame(pca_result$x)

    # Ensure row names match for joining/plotting by index later
    rownames(pca_data) <- rownames(data_for_clustering)

    # Create the 3D plot using pca_data for coordinates and data_for_clustering for color/symbol
    plotly_3d <- plot_ly(
      x = pca_data$PC1, y = pca_data$PC2, z = pca_data$PC3,
      type = "scatter3d",
      mode = "markers",
      color = data_for_clustering$cluster, # Use cluster from the data_for_clustering
      symbol = data_for_clustering$Product_Category, # Use Product_Category from the data_for_clustering
      colors = c("#F1C40F", "#E74C3C", "#2ECC71"),
      marker = list(opacity = 0.8)
    ) %>%
      layout(
        title = list(
          text = "3D PCA Plot of Customer Clusters<br><sub>Based on Product Attributes and Purchase Quantity</sub>", # Updated title
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

    plotly_3d
  })

  output$product_cluster_summary <- DT::renderDataTable({
    # Prepare data for clustering and summary
    data_for_summary <- data %>%
      mutate(Ratings_numeric = ifelse(Ratings == "High", 1, 0)) %>%
      select(Ratings, Product_Brand, Purchase_Quantity, Product_Category, Ratings_numeric)

    # Convert factors to numeric for clustering
    data_for_summary$Product_Brand_num <- as.numeric(data_for_summary$Product_Brand)
    data_for_summary$Product_Category_num <- as.numeric(data_for_summary$Product_Category)

    # Remove rows with NAs introduced by factor conversion or existing NAs
    data_for_summary <- na.omit(data_for_summary)

    # Select only the numeric variables for scaling and clustering (3 variables + Ratings_numeric for summary)
    data_scaled <- data_for_summary %>%
      select(Product_Brand_num, Product_Category_num, Purchase_Quantity) %>% # Use the 3 variables for scaling
      scale()

    # Perform k-means clustering
    set.seed(123)
    # Check if there are enough rows for clustering after na.omit
    if (nrow(data_scaled) < 3) {
       return(DT::datatable(data.frame("Summary" = "Not enough data to perform clustering")))
    }
    kmeans_result <- kmeans(data_scaled, centers = 3)

    # Add cluster labels to original data frame for summary
    data_for_summary$cluster <- factor(kmeans_result$cluster)

    # Create cluster summary
    cluster_summary <- data_for_summary %>% # Use data_for_summary with cluster labels
      group_by(cluster) %>%
      summarise(
        Avg_Purchase = mean(Purchase_Quantity, na.rm = TRUE),
        Avg_Rating = mean(Ratings_numeric, na.rm = TRUE),
        # Find the most common brand within each cluster
        Most_Common_Brand = names(sort(table(Product_Brand), decreasing = TRUE))[1],
        Count = n(),
        .groups = "drop"
      )

    DT::datatable(cluster_summary,
                  options = list(pageLength = 5, dom = 't', autoWidth = TRUE),
                  class = 'cell-border stripe hover nowrap',
                  caption = "Product-Based Cluster Summary")
  })
  
  output$product_rf_roc_plot <- renderPlotly({
    # Prepare model data
    model_data <- data %>%
      mutate(Total_Purchase = Amount * Purchase_Quantity) %>%
      select(Ratings, Customer_ID, Product_Brand, Product_Category, Purchase_Quantity, Amount, Total_Purchase)

    # Imbalanced
    imbalanced_results <- train_and_evaluate(model_data)

    # Balanced
    set.seed(123)
    balanced_data <- downSample(x = model_data[, -1], y = model_data$Ratings, yname = "Ratings")
    balanced_results <- train_and_evaluate(balanced_data)

    # Prepare ROC data for plotly
    imbalanced_roc <- imbalanced_results$rf$roc
    balanced_roc <- balanced_results$rf$roc

    roc_df <- data.frame(
      FPR = c(1 - imbalanced_roc$specificities, 1 - balanced_roc$specificities),
      TPR = c(imbalanced_roc$sensitivities, balanced_roc$sensitivities),
      Type = rep(c("Imbalanced", "Balanced"),
                 c(length(imbalanced_roc$specificities), length(balanced_roc$specificities)))
    )

    auc_imb <- round(auc(imbalanced_roc), 4)
    auc_bal <- round(auc(balanced_roc), 4)

    p <- ggplot(roc_df, aes(x = FPR, y = TPR, color = Type)) +
      geom_line(size = 1.2) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
      scale_color_manual(values = met.brewer("Cassatt1", 2)) + # Using MetBrewer Cassatt1 palette
      labs(
        title = "Random Forest ROC Curve (Balanced vs Imbalanced)",
        subtitle = paste("AUC Imbalanced =", auc_imb, "| AUC Balanced =", auc_bal),
        x = "False Positive Rate (1 - Specificity)",
        y = "True Positive Rate (Sensitivity)",
        color = "Data Type"
      ) +
      theme_minimal()

    ggplotly(p)
  })

}

# Run the application
shinyApp(ui = ui, server = server)
