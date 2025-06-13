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
               tabName = "transaction_logistics_analysis", 
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
                                        title = "Pie Chart for Overall Distribution Ratings",
                                        status = "success",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotlyOutput("pie_ratings_plot"))
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
                                        title = "Scatter Plot for Relationship between Total Ratings and Average Ratings",
                                        status = "primary",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotlyOutput("scatter_ratings_plot"))
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
                                        verbatimTextOutput("confusion_matrix")
                                      )
                               )
                             ),
                             fluidRow(
                               column(12,
                                      box(
                                        title = "Logistic Regression Predictions",
                                        status = "info",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotlyOutput("logistic_prediction_plot"))
                                      )
                               )
                             ),
                             fluidRow(
                               column(12,
                                      box(
                                        title = "ROC Curve Analysis",
                                        status = "warning",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotlyOutput("country_roc_plot"))
                                      )
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
                    
                    # Age Descriptive Analysis
                    tabPanel("Descriptive Analysis",
                             fluidRow(
                               column(12,
                                      box(
                                        title = "Total Amount and Ratings by Age Group",
                                        status = "info",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotOutput("faceted_boxplot_age"))
                                      )
                               ),
                             ),
                             fluidRow(
                               column(12,
                                      box(
                                        title = "Avg Total Amount Spent by Gender, Income, Ratings and Age Group",
                                        status = "success",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotOutput("age_adds_heatmap"))
                                      )
                               )
                             ),
                             
                    ),
                    
                    # Age Predictive Profiling
                    tabPanel("Predictive Analysis",
                             fluidRow(
                               column(6,
                                      box(
                                        title = "ROC Curve - Logistic (Age Only)",
                                        status = "warning",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotOutput("roc_log_age"))
                                      )
                               ),
                               column(6,
                                      box(
                                        title = "ROC Curve – Random Forest (Age Only)",
                                        status = "success",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotOutput("roc_rf_age"))
                                      )
                               ),
                             ),
                             fluidRow(
                               column(6,
                                      box(
                                        title = "ROC Curve – Logistic Regression",
                                        status = "info",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotOutput("roc_log_age_add"))
                                      )
                               ),
                               column(6,
                                      box(
                                        title = "Random Forest Variable Importance",
                                        status = "primary",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotOutput("var_imp_rf_age"))
                                      )
                               )
                             )
                    ),
                    
                    # Unsupervised Analysis Tab
                    tabPanel("Unsupervised Analysis",
                             fluidRow(
                               column(12,
                                      box(
                                        title = "Customer Profiling Based on Age and Ratings",
                                        status = "primary",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotOutput("plot_customer_profiling"))
                                      )
                               )
                             ),
                             fluidRow(
                               column(12,
                                      box(
                                        title = "Customer Profile Radar Chart",
                                        status = "success",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotOutput("radar_plot"))
                                      )
                               )
                             )
                    ),
                    
                    # Prescriptive Analysis Tab
                    tabPanel("Prescriptive Analysis",
                             fluidRow(
                               column(12,
                                      box(
                                        title = "Density Plot: Total Amount by Ratings and Age Group",
                                        status = "primary",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotOutput("age_density_plot"))
                                      )
                               )
                             ),
                             fluidRow(
                               column(6,
                                      box(
                                        title = "Income vs Ratings Mosaic Plot",
                                        status = "success",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotOutput("income_ratings_mosaic"))
                                      )
                               ),
                               column(6,
                                      box(
                                        title = "Customer Segment vs Ratings Mosaic Plot",
                                        status = "success",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotOutput("segment_ratings_mosaic"))
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
      
      # Transaction Logistics Analysis
      tabItem(tabName = "transaction_logistics_analysis",
              fluidRow(
                box(
                  title = "Transaction Logistics Analysis", 
                  status = "primary", 
                  solidHeader = TRUE,
                  width = 12,
                  p("This analysis examines how payment and shipping methods affect customer satisfaction and purchasing behavior.")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  tabsetPanel(
                    id = "logistics_tabs",
                    
                    # Descriptive Analysis Tab
                    tabPanel("Descriptive Analysis",
                             fluidRow(
                               column(12,
                                      box(
                                        title = "Satisfaction vs. Purchasing Behavior by Shipping and Payment",
                                        status = "primary",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotOutput("payment_shipping_facet_heatmap"))
                                      )
                               )
                             )
                    ),
                    
                    # Statistical Analysis Tab
                    tabPanel("Statistical Analysis",
                             fluidRow(
                               column(12,
                                      box(
                                        title = "Logistic Regression Coefficients & Significance",
                                        status = "warning",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotOutput("payment_shipping_coefficient"))
                                      )
                               )
                             )
                    ),
                    
                    # Predictive Analysis Tab
                    tabPanel("Predictive Analysis",
                             fluidRow(
                               column(12,
                                      box(
                                        title = "XGBoost ROC Curve",
                                        status = "success",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotOutput("payment_shipping_xgb_roc"))
                                      )
                               )
                             )
                    ),
                    
                    # Clustering Analysis Tab
                    tabPanel("Clustering Analysis",
                             fluidRow(
                               column(12,
                                      box(
                                        title = "Top Payment + Shipping Combos per Cluster",
                                        status = "info",
                                        solidHeader = TRUE,
                                        width = 12,
                                        withSpinner(plotOutput("cluster_combo"))
                                      )
                               )
                             )
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
    ggplotly(plot_lollipop_avg_ratings)
  })
  
  output$heatmap_plot <- renderPlotly({
    ggplotly(plot_heatmap_ratings_country)
  })
  
  output$pie_ratings_plot <- renderPlotly({
    plot_pie_ratings_plotly
  })
  
  output$total_ratings_plot <- renderPlotly({
    ggplotly(plot_dot_total_ratings)
  })
  
  output$violin_ratings_plot <- renderPlotly({
    ggplotly(plot_violin_ratings_country)
  })
  
  output$scatter_ratings_plot <- renderPlotly({
    ggplotly(plot_scatter_count_vs_avg)
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
  
  output$confusion_matrix <- renderText({
    paste(
      "Logistic Regression Confusion Matrix:\n",
      "           Predicted\n",
      "Actual     High  Low\n",
      "High       ", conf_matrix_logistic_country$table[1,1], "  ", conf_matrix_logistic_country$table[1,2], "\n",
      "Low        ", conf_matrix_logistic_country$table[2,1], "  ", conf_matrix_logistic_country$table[2,2], "\n\n",
      "Model trained on balanced dataset with equal representation from each country."
    )
  })
  
  output$logistic_prediction_plot <- renderPlotly({
    ggplotly(plot_logistic_country)
  })
  
  # Add ROC curve plot output
  output$country_roc_plot <- renderPlotly({
    ggplotly(country_roc_plot)
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
  
  # Faceted Boxplot
  output$faceted_boxplot_age <- renderPlot({
    ggplot(age_data, aes(x = Age_Group, y = Value, fill = Measure)) +
      geom_boxplot() +
      facet_wrap(~ Measure, scales = "free_y") +
      labs(
        title = "Total Amount and Ratings by Age Group",
        x     = "Age Group",
        y     = "Value"
      )
  })
  
  output$age_adds_heatmap <- renderPlot({
    ggplot(heatmap_data, aes(x = Income, y = Gender, fill = avg_total)) +
      geom_tile() +
      facet_grid(Ratings ~ Age_Group) +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(
        title = "Avg Total Amount spent by Gender, Income, Ratings, and Age Group",
        fill  = "Avg Amount (RM)"
      ) +
      theme_minimal()
  }) 
  
  output$roc_log_age <- renderPlot({
    pROC::plot.roc(roc_age_log, main = "ROC Curve – Logistic (Age Only)")
  })
  
  output$roc_rf_age <- renderPlot({
    pROC::plot.roc(roc_age_rf, main = "ROC Curve – Random Forest (Age Only)")
  })
  
  output$roc_log_age_add <- renderPlot({
    pROC::plot.roc(roc_full_log, main = "ROC Curve – Logistic Regression")
  })
  
  output$var_imp_rf_age <- renderPlot({
    varImpPlot(rf_full_model)
  })
  
    
  # Unsupervised Analysis Outputs
  output$plot_customer_profiling <- renderPlot({
    plot_customer_profiling
  })
  
  output$radar_plot <- renderPlot({
    radarchart(radar_data,
               axistype = 1,
               pcol = colors_border,
               plwd = 2,
               plty = 1,
               cglcol = "grey", 
               cglty = 1, 
               axislabcol = "grey", 
               caxislabels = seq(-2,2,1), 
               cglwd = 0.8,
               vlcex = 0.8,
               title = "Customer Profile per Cluster (Sampled Data - 10000)")
    
    # Add legend
    legend(x = "topright", 
           legend = c("Cluster 1", "Cluster 2", "Cluster 3"),
           bty = "n", 
           pch=20, 
           col=colors_border, 
           text.col = "black", 
           cex=0.8)
  })
  
  # Prescriptive Analysis Outputs
  output$age_density_plot <- renderPlot({
    plot_density_age_group
  })
  
  output$income_ratings_mosaic <- renderPlot({
    mosaic(~ Income + Ratings, data = data, shade = TRUE, 
           main = "Income vs Ratings")
  })
  
  output$segment_ratings_mosaic <- renderPlot({
    mosaic(~ Customer_Segment + Ratings, data = data, shade = TRUE, 
           main = "Customer Segment vs Ratings")
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
    pROC::plot.roc(imbalanced_results$rf$roc, col = "blue",
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

  output$xgb_roc_plot <- renderPlot({
    pROC::plot.roc(imbalanced_results$xgb$roc, col = "blue",
                   main = "XGBoost ROC Curve (Balanced vs Imbalanced)")
    lines(balanced_results$xgb$roc, col = "red", lty = 2, lwd = 2)
    text(0.7, 0.2, paste("Imbalanced AUC = ", round(auc(imbalanced_results$xgb$roc), 4)), col="black")
    text(0.65, 0.65, paste("Balanced AUC = ", round(auc(balanced_results$xgb$roc), 4)), col="black")
    legend("bottomright",
           legend = c("XGB Imbalanced", "XGB Balanced"),
           col = c("blue", "red"),
           lty = c(1, 2),
           lwd = 2)
  })
  
  # Var Importance plot
  output$product_feature_importance <- renderPlot({
    combined_imp_plot
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

  
  ################# Transaction Logistics ##############
  output$payment_shipping_facet_heatmap <- renderPlot({
    payment_shipping_facet_heatmap_plot
  })
  
  output$payment_shipping_coefficient <- renderPlot({
    payment_shipping_coefficient_plot
  })
  
  output$payment_shipping_xgb_roc <- renderPlot({
    plot(payment_shipping_roc_xgb, main = paste("XGBoost ROC Curve (AUC =", round(payment_shipping_auc_xgb, 3), ")"))
  })
  
  output$cluster_combo <- renderPlot({
    cluster_combo_plot
  })
}

# Run the application
shinyApp(ui = ui, server = server)