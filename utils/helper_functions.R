# utils/helper_functions.R
#' @description Reusable helper functions for data analysis


################# KOK JIA YIN (TP071062)  ############### 
### To evaluate the influence of product preferences on purchasing behavior and customer satisfaction. 
#' Style function for histograms
#'
#' @return A list of ggplot theme elements
styleHistogram <- function() {
  list(
    theme_minimal(base_size = 12),
    theme(
      axis.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
  )
}

#' Create summary statistics by group for ratings analysis
#'
#' @param data The dataset to summarize
#' @param group_by_col Column to group by
#' @return Summary statistics by group (Category or Brand)
library(dplyr)
create_summary <- function(data, group_by_col) {
  summary <- data |>
    group_by(!!sym(group_by_col), Ratings) |>
    summarize(
      rating_count = n(),
      total_purchase = sum(Amount * Purchase_Quantity) / 1000000,  # scaled down for readability
      .groups = 'drop'
    )
  
  # Calculate average age by group
  avg_age_summary <- data |>
    group_by(!!sym(group_by_col)) |>
    summarize(
      avg_age = mean(Age),
      .groups = 'drop'
    )
  
  # Merge the summaries together
  summary <- summary |>
    left_join(avg_age_summary, by = group_by_col)
  
  return(summary)
}

#' Plot a polar chart for ratings analysis
#'
#' @param summary_data Summary data from create_summary function
#' @param group_by_col Column name used for grouping
#' @return A ggplot object with polar chart
plot_polar_chart <- function(summary_data, group_by_col) {
  # Rescale avg_age to match rating_count scale for better point visibility
  max_rating_count <- max(summary_data$rating_count)
  max_avg_age <- max(summary_data$avg_age)
  
  summary_data <- summary_data |>
    mutate(
      scaled_avg_age = avg_age / max_avg_age * max_rating_count  # rescale to same range
    )
  
  # Polar Chart Visualization
  ggplot(summary_data) + 
    # Bars representing rating count, fill color representing total purchase
    geom_col(
      aes(
        x = !!sym(group_by_col),
        y = rating_count,
        fill = total_purchase
      ),
      color = "white"
    ) +
    
    # Points representing average age (rescaled)
    geom_point(
      aes(
        x = !!sym(group_by_col),
        y = scaled_avg_age
      ),
      color = "black",
      size = 3
    ) +
    
    # Dotted lines for visual reference
    geom_segment(
      aes(
        x = !!sym(group_by_col),
        xend = !!sym(group_by_col),
        y = 0,
        yend = max_rating_count
      ),
      linetype = "dotted",
      color = "grey30"
    ) +
    
    # Polar coordinates for circular layout
    coord_polar() 
}

#' Apply theme settings to a polar chart
#'
#' @param plot The ggplot object to style
#' @param group_by_col Column name used for grouping (for axis label)
#' @return A styled ggplot object
theme_setting <- function(plot, group_by_col) {
  palette_colors <- met.brewer("Cassatt1", 2)
  plot + 
    # Fill scale and plot labels
    scale_fill_gradient("Total Purchase\n (RM, x100k)", 
                        low = palette_colors[1], 
                        high = palette_colors[2],
                        guide = guide_colorbar(
                          title.position = "top",
                          title.hjust = 0.5,
                          barwidth = 10,
                          barheight = 0.6
                        )) +
    labs(
      x=group_by_col
    ) +
    # Minimal theme for cleaner look
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust = 1),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_line(color = "grey90"),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      axis.title = element_blank()
    )
}

#' Train and evaluate Machine Learning models for ratings prediction
#'
#' @param data The dataset to use for model training
#' @param label The target variable column name
#' @return A list with model results
train_and_evaluate <- function(data, label = "Ratings") {
  # Split
  set.seed(123)
  train_idx <- createDataPartition(data[[label]], p = 0.8, list = FALSE)
  train_data <- data[train_idx, ]
  test_data <- data[-train_idx, ]
  
  # Calculate ratings distribution
  train_distribution <- table(train_data$Ratings)
  
  # Prepare numeric version for XGBoost
  prepare_numeric_data <- function(df) {
    df$Customer_ID_Num <- as.numeric(factor(df$Customer_ID))
    df$Product_Brand_Num <- as.numeric(factor(df$Product_Brand))
    df$Product_Category_Num <- as.numeric(factor(df$Product_Category))
    df <- df |> select(-Customer_ID, -Product_Brand, -Product_Category)
    return(df)
  }
  
  train_numeric <- prepare_numeric_data(train_data)
  test_numeric <- prepare_numeric_data(test_data)
  
  ###### Random Forest
  rf_model <- randomForest(Ratings ~ ., data = train_data, ntree = 100)
  rf_pred <- predict(rf_model, test_data)
  rf_conf <- confusionMatrix(rf_pred, test_data$Ratings)
  rf_prob <- predict(rf_model, test_data, type = "prob")[, "High"]
  rf_roc <- roc(test_data$Ratings, rf_prob)
  
  ###### XGBoost
  xgb_train <- xgb.DMatrix(data = as.matrix(train_numeric |> select(-Ratings)), label = as.numeric(train_data$Ratings) - 1)
  xgb_test <- xgb.DMatrix(data = as.matrix(test_numeric |> select(-Ratings)), label = as.numeric(test_data$Ratings) - 1)
  
  xgb_model <- xgb.train(
    params = list(
      objective = "binary:logistic", eval_metric = "auc",
      eta = 0.1, max_depth = 6, subsample = 0.8, colsample_bytree = 0.8
    ),
    data = xgb_train,
    nrounds = 100,
    verbose = 0
  )
  
  xgb_prob <- predict(xgb_model, xgb_test)
  xgb_pred <- factor(ifelse(xgb_prob > 0.5, "High", "Low"), levels = c("Low", "High"))
  xgb_conf <- confusionMatrix(xgb_pred, test_data$Ratings)
  xgb_roc <- roc(test_data$Ratings, xgb_prob)
  
  return(list(
    rf = list(model = rf_model, roc = rf_roc, conf = rf_conf, auc = auc(rf_roc), predictions  = rf_pred, confusion_matrix=rf_conf),
    xgb = list(model = xgb_model, roc = xgb_roc, conf = xgb_conf, auc = auc(xgb_roc), predictions = xgb_pred, confusion_matrix = xgb_conf),
    train_distribution = train_distribution
  ))
}


#' Apply bar chart theme settings
#'
#' @param plot The ggplot object to style
#' @param fill_label Label for the fill legend
#' @param show_legend Whether to show the legend
#' @return A styled ggplot object
bar_theme_setting <- function(plot, fill_label = "Data Type", show_legend = TRUE) {
  palette_colors <- met.brewer("Cassatt1", 2)
  
  plot <- plot +
    scale_fill_manual(
      name = fill_label,
      values = c("Imbalanced" = palette_colors[1], "Balanced" = palette_colors[2])
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      axis.ticks = element_blank(),
      panel.grid = element_line(color = "grey90"),
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 9),
      axis.title = element_text(size = 11)
    )
  
  if (show_legend) {
    plot <- plot + theme(legend.position = "bottom")
  } else {
    plot <- plot + theme(legend.position = "none")
  }
  
  return(plot)
}





