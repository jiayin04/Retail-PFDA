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
  dashboardHeader(title = "Retail Data Analytics Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Geographical Analysis", 
               tabName = "satisfaction", 
               icon = icon("star")),
      menuItem("Age Analysis", 
               tabName = "age_analysis", 
               icon = icon("user")),
      menuItem("Product Analysis", 
               tabName = "product_analysis", 
               icon = icon("shopping-cart")),
      menuItem("Transaction Logistics Analysis", 
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
                                        title = "Proportional Ratings by Country",
                                        status = "info",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotlyOutput("proportional_ratings_plot"))
                                      )
                               ),
                               column(6,
                                      box(
                                        title = "Average Ratings by Country",
                                        status = "info", 
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotlyOutput("avg_ratings_plot"))
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
                                        title = "Stacked Ratings by Country",
                                        status = "success",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotlyOutput("stacked_ratings_plot"))
                                      )
                               )
                             ),
                             fluidRow(
                               column(6,
                                      box(
                                        title = "Total Ratings per Country",
                                        status = "warning",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotlyOutput("total_ratings_plot"))
                                      )
                               ),
                               column(6,
                                      box(
                                        title = "Violin Plot of Ratings",
                                        status = "warning",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotlyOutput("violin_ratings_plot"))
                                      )
                               )
                             ),
                             fluidRow(
                               column(12,
                                      box(
                                        title = "Country Statistics Summary",
                                        status = "primary",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(DT::dataTableOutput("country_stats_table"))
                                      )
                               )
                             )
                    ),
                    
                    # Statistical Analysis Tab
                    tabPanel("Diagnostic Analysis",
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
                               column(12,
                                      box(
                                        title = "Model Performance Comparison",
                                        status = "success",
                                        solidHeader = TRUE,
                                        width = 12,
                                        h4("Model Accuracy Metrics"),
                                        withSpinner(DT::dataTableOutput("model_performance_table")),
                                        br(),
                                        verbatimTextOutput("confusion_matrix")
                                      )
                               )
                             ),
                             fluidRow(
                               column(6,
                                      box(
                                        title = "Logistic Regression Predictions",
                                        status = "info",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotlyOutput("logistic_prediction_plot"))
                                      )
                               ),
                               column(6,
                                      box(
                                        title = "Random Forest Predictions",
                                        status = "info",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotlyOutput("rf_prediction_plot"))
                                      )
                               )
                             ),
                             fluidRow(
                               box(
                                 title = "Balanced Dataset Distribution",
                                 status = "primary",
                                 solidHeader = TRUE,
                                 width = 12,
                                 withSpinner(plotlyOutput("balanced_data_plot"))
                               )
                             )
                    ),
                    
                    # Recommendations Tab
                    tabPanel("Prescriptive Analysis",
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
                               column(12,
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
                  p("This analysis examines product preferences, co-purchasing behavior, brand preferences and customer profiling.")
                )
              ),
              
              fluidRow(
                box(
                  width = 12,
                  tabsetPanel(
                    id = "product_tabs",
                    
                    # Product Preference Analysis
                    tabPanel("Diagnostic Analysis",
                             fluidRow(
                               column(12,
                                      box(
                                        title = "Pairwise Comparison Plot (Histogram) ",
                                        status = "success",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotOutput("product_category_plot"))
                                      )
                               ),
                             ),
                             fluidRow(
                               column(8,
                                        box(
                                          title = "Centroid Means Plot with Error Bars",
                                                     status = "success",
                                                     solidHeader = TRUE,
                                                     width = 12,
                                                     withSpinner(plotOutput("centroid_mean_plot"))
                                        )
                                      ),
                               column(4,
                                      box(
                                        title = "Standard Error Formula",
                                        status = "info",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withMathJax(
                                          p("The standard error of the centroid mean is calculated as:"),
                                          p("$$SE = \\frac{Standard Deviation (SD)}{\\sqrt{Sample Size(n)}}$$")
                                        )
                                      )
                                      ),
                             )
                            
                    ),
                    
                    # Co-Purchasing Analysis
                    tabPanel("Descriptive and Exploratory Data Analysis",
                             fluidRow(
                               column(6,
                                      box(
                                        title = "Distribution of Customer Ratings Across Product Segment (Polar Chart)",
                                        status = "success",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotOutput("product_radar_plot"))
                                      )
                               ),
                               column(6,
                                      box(
                                        title = "Top 15 Product Pairs by Customer Ratings",
                                        status = "success",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(DTOutput("top_pairs_table"))
                                      ))
                             ),
                             fluidRow(
                               column(12,
                                      box(
                                        title = "Co-Purchase Heatmap",
                                        status = "warning",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotOutput("co_purchase_heatmap"))
                                      )
                               )
                             ),
                    ),
                    tabPanel("Predictive Analysis",
                             fluidRow(
                               column(8,
                                      box(
                                        title = "Model Evaluation Metrics (Confusion Matrix Summary)",
                                        status = "primary",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(dataTableOutput("model_metrics"))
                                      )
                               ),
                               column(4,
                                      box(
                                        title = "Ratings Distribution in Training Set",
                                        status = "info",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(tableOutput("ratings_distribution_table"))
                                      )
                               ),
                             ),
                             
                             fluidRow(
                               column(6,
                                      box(
                                        title = "Random Forest ROC Curve (Balanced vs Imbalanced)",
                                        status = "success",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotOutput("rf_roc_plot"))
                                      )
                               ),
                               column(6,
                                      box(
                                        title = "XGBoost ROC Curve (Balanced vs Imbalanced)",
                                        status = "success",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotOutput("xgb_roc_plot"))
                                      )
                               )
                             ),
                             
                             fluidRow(
                               column(12,
                                      box(
                                        title = "Feature Importance",
                                        status = "success",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotOutput("product_feature_importance"))
                                        )
                                      )
                             ),
                    ),
                    # Product-Based Clustering
                    tabPanel("Clustering Analysis",
                             fluidRow(
                               column(5,
                                      box(
                                        title = "Avg Silhouette Score",
                                        status = "info",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(verbatimTextOutput("avg_sil_score"))
                                        ),
                                      
                                      box(
                                        title = "Product-Based Cluster Summary",
                                        status = "info",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(tableOutput("product_cluster_summary"))
                                      )
                                      ),
                               column(7,
                                      box(
                                        title = "Elbow Method for Optimal K",
                                        status = "primary",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotOutput(("product_elbow_method")))
                                        )
                                      )
                             ),
                             fluidRow(
                               column(5,
                                      box(
                                        title = "3D PCA Product-Based Clusters",
                                        status = "success",
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
                                        withSpinner(plotOutput(("dendrogram_plot"))
                                      )
                               )
                             )
                    ),
                    
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
  
  ################# Country ################
  
  # Create contingency table for statistical tests
  table_ratings <- table(data$Country, data$Ratings)
  
  # Descriptive Analysis Outputs
  output$proportional_ratings_plot <- renderPlotly({
    ggplotly(plot_proportional_ratings_country)
  })
  
  output$avg_ratings_plot <- renderPlotly({
    ggplotly(plot_avg_ratings_country)
  })
  
  output$heatmap_plot <- renderPlotly({
    ggplotly(plot_heatmap_ratings_country)
  })
  
  output$stacked_ratings_plot <- renderPlotly({
    ggplotly(plot_stacked_ratings_country)
  })
  
  output$total_ratings_plot <- renderPlotly({
    ggplotly(plot_total_ratings_country)
  })
  
  output$violin_ratings_plot <- renderPlotly({
    ggplotly(plot_violin_ratings_country)
  })
  
  output$country_stats_table <- DT::renderDataTable({
    rating_stats_by_country
  })
  
  # Diagnostic Analysis Outputs
  output$chisq_results <- renderText({
    if (all(table_ratings > 0)) {
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
    # Extract significant pairs
    tukey_df <- as.data.frame(tukey_country_result$Country)
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
    # Create performance metrics table
    performance_metrics_country <- data.frame(
      Model = c("Logistic Regression", "Random Forest"),
      Accuracy = c(
        round(conf_matrix_logistic_country$overall["Accuracy"], 3),
        round(conf_matrix_rf_country$overall["Accuracy"], 3)
      ),
      Sensitivity = c(
        round(conf_matrix_logistic_country$byClass["Sensitivity"], 3),
        round(conf_matrix_rf_country$byClass["Sensitivity"], 3)
      ),
      Specificity = c(
        round(conf_matrix_logistic_country$byClass["Specificity"], 3),
        round(conf_matrix_rf_country$byClass["Specificity"], 3)
      ),
      Precision = c(
        round(conf_matrix_logistic_country$byClass["Pos Pred Value"], 3),
        round(conf_matrix_rf_country$byClass["Pos Pred Value"], 3)
      )
    )
    
    DT::datatable(performance_metrics_country,
                  options = list(pageLength = 5, dom = 't'),
                  caption = "Model Performance Comparison")
  })
  
  output$confusion_matrix <- renderText({
    paste(
      "Logistic Regression Confusion Matrix:\n",
      "           Predicted\n",
      "Actual     High  Low\n",
      "High       ", conf_matrix_logistic_country$table[1,1], "  ", conf_matrix_logistic_country$table[1,2], "\n",
      "Low        ", conf_matrix_logistic_country$table[2,1], "  ", conf_matrix_logistic_country$table[2,2], "\n\n",
      "Random Forest Confusion Matrix:\n",
      "           Predicted\n",
      "Actual     High  Low\n",
      "High       ", conf_matrix_rf_country$table[1,1], "  ", conf_matrix_rf_country$table[1,2], "\n",
      "Low        ", conf_matrix_rf_country$table[2,1], "  ", conf_matrix_rf_country$table[2,2], "\n\n",
      "Models trained on balanced dataset with equal representation from each country."
    )
  })
  
  output$side_by_side_plots <- renderPlotly({
    ggplotly(side_by_side_plots_country)
  })
  
  output$logistic_prediction_plot <- renderPlotly({
    ggplotly(plot_logistic_country)
  })
  
  output$rf_prediction_plot <- renderPlotly({
    ggplotly(plot_rf_country)
  })
  
  output$balanced_data_plot <- renderPlotly({
    ggplotly(plot_rating_distribution_country)
  })
  
  # Prescriptive Analysis Outputs
  output$recommendations_plot <- renderPlotly({
    ggplotly(plot_satisfaction_targets_country)
  })
  
  output$recommendations_table <- DT::renderDataTable({
    DT::datatable(recommendations_country %>% 
                    select(Country, Country_Satisfaction_Rate, Country_Performance_Level, Country_Recommended_Action),
                  options = list(pageLength = 10, scrollX = TRUE),
                  caption = "Country Performance and Recommendations") %>%
      DT::formatStyle("Country_Performance_Level",
                      backgroundColor = DT::styleEqual(
                        c("Good", "Average", "Needs Improvement"),
                        c("#d4edda", "#fff3cd", "#f8d7da")
                      ))
  })
  
  output$action_summary_table <- DT::renderDataTable({
    DT::datatable(action_summary,
                  options = list(pageLength = 5, dom = 't'),
                  caption = "Action Plan Summary")
  })
  
  output$priority_countries_table <- DT::renderDataTable({
    priority_table <- priority_countries %>%
      select(Country, Country_Satisfaction_Rate, Total_Customers, Country_Recommended_Action) %>%
      rename(
        "Satisfaction Rate (%)" = Country_Satisfaction_Rate,
        "Total Customers" = Total_Customers,
        "Recommended Action" = Country_Recommended_Action
      )
    
    DT::datatable(priority_table,
                  options = list(pageLength = 5, dom = 't'),
                  caption = "Top 3 Priority Countries") %>%
      DT::formatStyle("Satisfaction Rate (%)",
                      backgroundColor = "#f8d7da",
                      color = "#721c24")
  })
  
  ################# Age ################
  
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
  

  ################PRODUCT########################  
  
  ##### Product Analysis Outputs ####
  # Product Category Histogram
  output$product_category_plot <- renderPlot({
    final_hist_plot
  })
  
  # Centroid Mean Plot
  output$centroid_mean_plot <- renderPlot({
    cm_plot
  })
  
  # Radar Plot/ Polar Chart
  output$product_radar_plot <- renderPlot({
    combined_radar_plot
  })
  
  # Top Pairs Table
  output$top_pairs_table <- DT::renderDataTable({
    pair_by_rating_top
  }, options = list(pageLength = 8, scrollX = TRUE))
  
  # Heatmap
  output$co_purchase_heatmap <- renderPlot({
    heatmap_plot
  })
  
  # Confusion matrix for balanced and imbalanced dataset
  output$model_metrics <- renderDataTable({
    make_metric_row <- function(cm, model_name) {
      data.frame(
        Model = model_name,
        Accuracy = round(cm$overall["Accuracy"], 3),
        Kappa = round(cm$overall["Kappa"], 3),
        Sensitivity = round(cm$byClass["Sensitivity"], 3),
        Specificity = round(cm$byClass["Specificity"], 3)
      )
    }
    
    df <- do.call(rbind, list(
      make_metric_row(imbalanced_results$rf$confusion_matrix, "RF - Imbalanced"),
      make_metric_row(balanced_results$rf$confusion_matrix, "RF - Balanced"),
      make_metric_row(imbalanced_results$xgb$confusion_matrix, "XGB - Imbalanced"),
      make_metric_row(balanced_results$xgb$confusion_matrix, "XGB - Balanced")
    ))
    
    datatable(df, rownames = FALSE)
  })
  
  # Ratings Distribution [Balanced Vs Imbalanced]
  output$ratings_distribution_table <- renderTable({
    im <- imbalanced_results$train_distribution
    bl <- balanced_results$train_distribution
    
    combined <- rbind(
      data.frame(Rating = names(im), Count = as.numeric(im), Type = "Imbalanced"),
      data.frame(Rating = names(bl), Count = as.numeric(bl), Type = "Balanced")
    )
    
    combined
  })
  
  # ROC Plot
  output$rf_roc_plot <- renderPlot({
    plot(imbalanced_results$rf$roc, col = "blue",
         main = "Random Forest ROC Curve (Balanced vs Imbalanced)")
    lines(balanced_results$rf$roc, col = "red", lty = 2, lwd = 2)
    text(0.7, 0.2, paste("Imbalanced AUC = ", round(auc(imbalanced_results$rf$roc), 4)), col="black")
    text(0.65, 0.65, paste("Balanced AUC = ", round(auc(balanced_results$rf$roc), 4)), col="black")
    legend("bottomright",
           legend = c("RF Imbalanced", "RF Balanced"),
           col = c("blue", "red"),
           lty = c(1, 2),
           lwd = 2)
    
  })
  
  output$product_feature_importance <- renderPlot({
    combined_imp_plot
  })
  
  output$xgb_roc_plot <- renderPlot({
    plot(imbalanced_results$xgb$roc, col = "blue",
         main = "XGBoost ROC Curve (Balanced vs Imbalanced)")
    lines(balanced_results$xgb$roc, col = "red", lty = 2, lwd = 2)
    text(0.7, 0.2, paste("Imbalanced AUC =", round(auc(imbalanced_results$xgb$roc), 4)), col="black")
    text(0.65, 0.65, paste("Balanced AUC =", round(auc(balanced_results$xgb$roc), 4)), col="black")
    legend("bottomright", legend = c("XGB Imbalanced", "XGB Balanced"),
           col = c("blue", "red"), lty = c(1, 2), lwd = 2)
  })
  
  
  
  # Product-based clustering
  output$product_cluster_plot <- renderPlotly({
    plotly_3d
  })
  
  output$avg_sil_score <- renderPrint({
    cat(sprintf("Avg Silhouette \n K=3: %.4f \n K=4: %.4f\n", sil_score(3), sil_score(4)))
  })
  
  output$product_cluster_summary <- renderTable({
    cluster_summary
  }, 
  striped = TRUE, 
  hover = TRUE, 
  bordered = TRUE, 
  caption = "Product-Based Cluster Summary")
  
  output$product_elbow_method <- renderPlot({
    elbow_method_plot
  })
  
  output$dendrogram_plot <- renderPlot({
    plot(dend_colored,
         main = "Clustered Customers by Brand & Cluster (Sampled)",
         horiz = TRUE,
         cex.main = 1.2)
    
    for (h in c(0.5, 1, 1.5, 2, 2.5)) {
      abline(v = h, col = "grey60", lty = 2)
    }
    
    abline(v=0, col="black", lwd= 1)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)