#' @title Advanced Data Visualization with ggplot2
#' @description Professional plotting techniques and custom visualizations
#' @author Portfolio Developer
#' @date 2025

library(ggplot2)
library(dplyr)
library(scales)
library(RColorBrewer)
library(viridis)
library(plotly)
library(patchwork)
library(ggthemes)

# =============================================================================
# CUSTOM THEMES AND STYLING
# =============================================================================

#' Professional Custom Theme
#'
#' @description Creates a professional, publication-ready ggplot theme
#' @param base_size Base font size
#' @param base_family Font family
#' @param grid_color Grid line color
#' @return ggplot2 theme object
#' @export
theme_professional <- function(base_size = 12, base_family = "Arial", grid_color = "grey90") {
  
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # Plot background and panel
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = grid_color, size = 0.5, linetype = "solid"),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      
      # Axes
      axis.line = element_line(color = "black", size = 0.5),
      axis.ticks = element_line(color = "black", size = 0.5),
      axis.text = element_text(color = "black", size = rel(0.9)),
      axis.title = element_text(color = "black", size = rel(1.0), face = "bold"),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      
      # Legend
      legend.background = element_rect(fill = "white", color = "grey80", size = 0.5),
      legend.key = element_rect(fill = "white", color = NA),
      legend.title = element_text(face = "bold", size = rel(1.0)),
      legend.text = element_text(size = rel(0.9)),
      legend.position = "right",
      legend.margin = margin(l = 10),
      
      # Plot titles and labels
      plot.title = element_text(
        face = "bold", 
        size = rel(1.3), 
        hjust = 0, 
        margin = margin(b = 10)
      ),
      plot.subtitle = element_text(
        color = "grey40", 
        size = rel(1.1), 
        hjust = 0,
        margin = margin(b = 15)
      ),
      plot.caption = element_text(
        color = "grey60", 
        size = rel(0.8), 
        hjust = 1,
        margin = margin(t = 15)
      ),
      
      # Facets
      strip.background = element_rect(fill = "grey95", color = "grey80", size = 0.5),
      strip.text = element_text(face = "bold", size = rel(1.0), margin = margin(5, 5, 5, 5)),
      
      # Spacing
      plot.margin = margin(20, 20, 20, 20)
    )
}

#' Dark Theme for Modern Visualizations
#'
#' @description Modern dark theme for contemporary visualizations
#' @param base_size Base font size
#' @return ggplot2 theme object
#' @export
theme_dark_modern <- function(base_size = 12) {
  
  theme_minimal(base_size = base_size) +
    theme(
      # Background colors
      plot.background = element_rect(fill = "#1a1a1a", color = NA),
      panel.background = element_rect(fill = "#1a1a1a", color = NA),
      panel.grid.major = element_line(color = "#333333", size = 0.3),
      panel.grid.minor = element_blank(),
      
      # Text colors
      text = element_text(color = "#ffffff"),
      axis.text = element_text(color = "#cccccc"),
      axis.title = element_text(color = "#ffffff", face = "bold"),
      plot.title = element_text(color = "#ffffff", face = "bold", size = rel(1.3)),
      plot.subtitle = element_text(color = "#cccccc", size = rel(1.1)),
      plot.caption = element_text(color = "#999999", size = rel(0.8)),
      
      # Legend
      legend.background = element_rect(fill = "#2a2a2a", color = "#444444"),
      legend.key = element_rect(fill = "#1a1a1a", color = NA),
      legend.text = element_text(color = "#cccccc"),
      legend.title = element_text(color = "#ffffff", face = "bold"),
      
      # Strips for faceting
      strip.background = element_rect(fill = "#333333", color = "#555555"),
      strip.text = element_text(color = "#ffffff", face = "bold")
    )
}

#' Custom Color Palettes
#'
#' @description Professional color palettes for different visualization needs
#' @param palette_name Name of the palette
#' @param n Number of colors needed
#' @return Vector of hex colors
#' @export
get_custom_palette <- function(palette_name = "professional", n = 5) {
  
  palettes <- list(
    professional = c("#2C3E50", "#E74C3C", "#3498DB", "#2ECC71", "#F39C12", "#9B59B6", "#1ABC9C"),
    business = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2"),
    modern = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FFEAA7", "#DDA0DD", "#98D8C8"),
    muted = c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", "#44AA99"),
    vibrant = c("#E64B35", "#4DBBD5", "#00A087", "#3C5488", "#F39B7F", "#8491B4", "#91D1C2")
  )
  
  if (!palette_name %in% names(palettes)) {
    palette_name <- "professional"
  }
  
  palette <- palettes[[palette_name]]
  
  if (n <= length(palette)) {
    return(palette[1:n])
  } else {
    # Interpolate colors if more colors needed
    return(colorRampPalette(palette)(n))
  }
}

# =============================================================================
# ADVANCED PLOTTING FUNCTIONS
# =============================================================================

#' Create Advanced Scatter Plot with Statistical Overlays
#'
#' @description Professional scatter plot with regression lines, confidence intervals, and annotations
#' @param data Data frame containing the data
#' @param x_var X-axis variable name
#' @param y_var Y-axis variable name
#' @param color_var Variable for color mapping (optional)
#' @param size_var Variable for size mapping (optional)
#' @param add_regression Logical, whether to add regression line
#' @param add_correlation Logical, whether to add correlation annotation
#' @return ggplot object
#' @export
create_advanced_scatter <- function(data, x_var, y_var, color_var = NULL, size_var = NULL, 
                                   add_regression = TRUE, add_correlation = TRUE) {
  
  # Base plot
  p <- ggplot(data, aes_string(x = x_var, y = y_var))
  
  # Add confidence interval first (so it's behind points)
  if (add_regression) {
    p <- p + geom_smooth(
      method = "lm", 
      se = TRUE, 
      alpha = 0.2, 
      color = "#2C3E50",
      fill = "#3498DB"
    )
  }
  
  # Add points with conditional aesthetics
  if (!is.null(color_var) && !is.null(size_var)) {
    p <- p + geom_point(aes_string(color = color_var, size = size_var), alpha = 0.7)
  } else if (!is.null(color_var)) {
    p <- p + geom_point(aes_string(color = color_var), size = 3, alpha = 0.7)
  } else if (!is.null(size_var)) {
    p <- p + geom_point(aes_string(size = size_var), color = "#2C3E50", alpha = 0.7)
  } else {
    p <- p + geom_point(size = 3, color = "#2C3E50", alpha = 0.7)
  }
  
  # Add correlation annotation
  if (add_correlation) {
    correlation <- cor(data[[x_var]], data[[y_var]], use = "complete.obs")
    p <- p + annotate(
      "text",
      x = Inf, y = Inf,
      label = paste("r =", round(correlation, 3)),
      hjust = 1.1, vjust = 1.1,
      size = 4, fontface = "bold",
      color = "#E74C3C"
    )
  }
  
  # Styling
  p <- p +
    scale_x_continuous(labels = comma_format()) +
    scale_y_continuous(labels = comma_format()) +
    labs(
      title = paste("Relationship between", str_to_title(gsub("_", " ", x_var)), 
                   "and", str_to_title(gsub("_", " ", y_var))),
      subtitle = "Professional scatter plot with statistical overlays",
      x = str_to_title(gsub("_", " ", x_var)),
      y = str_to_title(gsub("_", " ", y_var)),
      caption = paste("Data points:", nrow(data), "| Generated:", Sys.Date())
    ) +
    theme_professional()
  
  # Conditional color scales
  if (!is.null(color_var)) {
    if (is.numeric(data[[color_var]])) {
      p <- p + scale_color_viridis_c(name = str_to_title(gsub("_", " ", color_var)))
    } else {
      p <- p + scale_color_manual(
        values = get_custom_palette("professional", n_distinct(data[[color_var]])),
        name = str_to_title(gsub("_", " ", color_var))
      )
    }
  }
  
  # Size scale
  if (!is.null(size_var)) {
    p <- p + scale_size_continuous(
      range = c(1, 6),
      name = str_to_title(gsub("_", " ", size_var)),
      guide = guide_legend(override.aes = list(alpha = 1))
    )
  }
  
  return(p)
}

#' Advanced Distribution Visualization
#'
#' @description Creates comprehensive distribution plots with multiple display options
#' @param data Data frame
#' @param var Variable to plot
#' @param group_var Grouping variable (optional)
#' @param plot_type Type of plot ("histogram", "density", "boxplot", "violin")
#' @return ggplot object
#' @export
create_distribution_plot <- function(data, var, group_var = NULL, plot_type = "histogram") {
  
  # Base aesthetics
  if (!is.null(group_var)) {
    base_aes <- aes_string(x = var, fill = group_var)
  } else {
    base_aes <- aes_string(x = var)
  }
  
  p <- ggplot(data, base_aes)
  
  # Plot type specific layers
  if (plot_type == "histogram") {
    p <- p + geom_histogram(
      bins = 30, 
      alpha = 0.8, 
      color = "white", 
      size = 0.3,
      position = ifelse(!is.null(group_var), "dodge", "identity")
    )
    
    # Add density curve overlay
    p <- p + geom_density(
      aes(y = after_stat(density * nrow(data) * (max(data[[var]], na.rm = TRUE) - 
                                                min(data[[var]], na.rm = TRUE)) / 30)),
      alpha = 0.3, size = 1
    )
    
  } else if (plot_type == "density") {
    p <- p + geom_density(alpha = 0.7, size = 1)
    
    # Add rug plot
    p <- p + geom_rug(alpha = 0.5, size = 0.5)
    
  } else if (plot_type == "boxplot") {
    if (!is.null(group_var)) {
      p <- ggplot(data, aes_string(x = group_var, y = var, fill = group_var))
    } else {
      p <- ggplot(data, aes_string(x = "''", y = var))
    }
    
    p <- p + 
      geom_boxplot(alpha = 0.7, outlier.color = "#E74C3C", outlier.size = 2) +
      geom_jitter(width = 0.2, alpha = 0.3, size = 1)
    
  } else if (plot_type == "violin") {
    if (!is.null(group_var)) {
      p <- ggplot(data, aes_string(x = group_var, y = var, fill = group_var))
    } else {
      p <- ggplot(data, aes_string(x = "''", y = var))
    }
    
    p <- p + 
      geom_violin(alpha = 0.7, scale = "width") +
      geom_boxplot(width = 0.1, alpha = 0.9, outlier.shape = NA) +
      stat_summary(fun = median, geom = "point", size = 2, color = "red")
  }
  
  # Statistical annotations
  mean_val <- mean(data[[var]], na.rm = TRUE)
  median_val <- median(data[[var]], na.rm = TRUE)
  
  if (plot_type %in% c("histogram", "density")) {
    p <- p + 
      geom_vline(xintercept = mean_val, color = "#E74C3C", linetype = "dashed", size = 1) +
      geom_vline(xintercept = median_val, color = "#2ECC71", linetype = "dashed", size = 1) +
      annotate("text", x = mean_val, y = Inf, label = paste("Mean:", round(mean_val, 2)),
               hjust = -0.1, vjust = 1.1, color = "#E74C3C", fontface = "bold") +
      annotate("text", x = median_val, y = Inf, label = paste("Median:", round(median_val, 2)),
               hjust = -0.1, vjust = 2.5, color = "#2ECC71", fontface = "bold")
  }
  
  # Styling and labels
  p <- p +
    scale_x_continuous(labels = comma_format()) +
    scale_y_continuous(labels = comma_format()) +
    labs(
      title = paste("Distribution of", str_to_title(gsub("_", " ", var))),
      subtitle = paste("Statistical summary using", plot_type, "visualization"),
      x = str_to_title(gsub("_", " ", var)),
      y = ifelse(plot_type %in% c("boxplot", "violin"), "Value", "Frequency"),
      caption = paste("n =", nrow(data), "| Generated:", Sys.Date())
    ) +
    theme_professional()
  
  # Color scaling
  if (!is.null(group_var)) {
    n_groups <- n_distinct(data[[group_var]])
    p <- p + scale_fill_manual(
      values = get_custom_palette("professional", n_groups),
      name = str_to_title(gsub("_", " ", group_var))
    )
  } else {
    p <- p + scale_fill_manual(values = "#3498DB")
  }
  
  return(p)
}

#' Advanced Time Series Visualization
#'
#' @description Comprehensive time series plotting with trend analysis
#' @param data Data frame with time series data
#' @param date_var Date variable name
#' @param value_var Value variable name
#' @param group_var Grouping variable (optional)
#' @param add_trend Logical, add trend line
#' @param add_seasonality Logical, add seasonal decomposition
#' @return ggplot object
#' @export
create_timeseries_plot <- function(data, date_var, value_var, group_var = NULL, 
                                  add_trend = TRUE, add_seasonality = FALSE) {
  
  # Ensure date column is properly formatted
  data[[date_var]] <- as.Date(data[[date_var]])
  
  # Base plot
  if (!is.null(group_var)) {
    p <- ggplot(data, aes_string(x = date_var, y = value_var, color = group_var))
  } else {
    p <- ggplot(data, aes_string(x = date_var, y = value_var))
  }
  
  # Add the main line
  p <- p + geom_line(size = 1, alpha = 0.8)
  
  # Add points for emphasis
  p <- p + geom_point(size = 2, alpha = 0.6)
  
  # Add trend line
  if (add_trend) {
    p <- p + geom_smooth(
      method = "loess", 
      se = TRUE, 
      alpha = 0.2, 
      size = 1.2,
      color = "#E74C3C"
    )
  }
  
  # Seasonal highlighting (if requested and data spans multiple years)
  if (add_seasonality && length(unique(year(data[[date_var]]))) > 1) {
    # Add background shading for quarters
    date_range <- range(data[[date_var]], na.rm = TRUE)
    quarters <- seq(floor_date(date_range[1], "quarter"), 
                   ceiling_date(date_range[2], "quarter"), 
                   by = "quarter")
    
    quarter_data <- data.frame(
      start = quarters[-length(quarters)],
      end = quarters[-1],
      quarter = rep(c("Q1", "Q2", "Q3", "Q4"), length.out = length(quarters) - 1)
    )
    
    p <- p + geom_rect(
      data = quarter_data,
      aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = quarter),
      alpha = 0.1, inherit.aes = FALSE
    )
  }
  
  # Statistical annotations
  latest_value <- data[[value_var]][which.max(data[[date_var]])]
  max_value <- max(data[[value_var]], na.rm = TRUE)
  min_value <- min(data[[value_var]], na.rm = TRUE)
  
  p <- p + annotate(
    "text",
    x = max(data[[date_var]], na.rm = TRUE),
    y = latest_value,
    label = paste("Latest:", comma_format()(latest_value)),
    hjust = 1.1, vjust = -0.5,
    fontface = "bold", color = "#2C3E50"
  )
  
  # Styling
  p <- p +
    scale_x_date(
      date_labels = "%b %Y",
      date_breaks = "3 months",
      expand = expansion(mult = c(0.02, 0.02))
    ) +
    scale_y_continuous(
      labels = comma_format(),
      expand = expansion(mult = c(0.05, 0.05))
    ) +
    labs(
      title = paste("Time Series Analysis:", str_to_title(gsub("_", " ", value_var))),
      subtitle = paste("Trend analysis from", min(data[[date_var]]), "to", max(data[[date_var]])),
      x = "Date",
      y = str_to_title(gsub("_", " ", value_var)),
      caption = paste("Data points:", nrow(data), "| Range:", 
                     comma_format()(max_value - min_value))
    ) +
    theme_professional() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor.x = element_blank()
    )
  
  # Color scaling
  if (!is.null(group_var)) {
    n_groups <- n_distinct(data[[group_var]])
    p <- p + scale_color_manual(
      values = get_custom_palette("professional", n_groups),
      name = str_to_title(gsub("_", " ", group_var))
    )
  } else {
    p <- p + scale_color_manual(values = "#3498DB")
  }
  
  return(p)
}

#' Advanced Heatmap Visualization
#'
#' @description Professional correlation heatmap with annotations
#' @param data Data frame (will compute correlations for numeric variables)
#' @param title Plot title
#' @param cluster_rows Logical, whether to cluster rows
#' @param show_values Logical, whether to show correlation values
#' @return ggplot object
#' @export
create_correlation_heatmap <- function(data, title = "Correlation Matrix", 
                                      cluster_rows = TRUE, show_values = TRUE) {
  
  # Select only numeric variables
  numeric_data <- data %>% select_if(is.numeric)
  
  if (ncol(numeric_data) < 2) {
    stop("Need at least 2 numeric variables for correlation analysis")
  }
  
  # Calculate correlation matrix
  cor_matrix <- cor(numeric_data, use = "complete.obs")
  
  # Convert to long format for ggplot
  cor_long <- expand.grid(
    Var1 = rownames(cor_matrix),
    Var2 = colnames(cor_matrix),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      correlation = as.vector(cor_matrix),
      abs_correlation = abs(correlation)
    )
  
  # Clustering (optional)
  if (cluster_rows) {
    # Hierarchical clustering
    hc <- hclust(dist(cor_matrix))
    cor_long$Var1 <- factor(cor_long$Var1, levels = rownames(cor_matrix)[hc$order])
    cor_long$Var2 <- factor(cor_long$Var2, levels = colnames(cor_matrix)[hc$order])
  } else {
    cor_long$Var1 <- factor(cor_long$Var1, levels = rownames(cor_matrix))
    cor_long$Var2 <- factor(cor_long$Var2, levels = rev(colnames(cor_matrix)))
  }
  
  # Create the heatmap
  p <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = correlation)) +
    geom_tile(color = "white", size = 0.5) +
    scale_fill_gradient2(
      low = "#E74C3C", 
      mid = "white", 
      high = "#3498DB",
      midpoint = 0,
      limits = c(-1, 1),
      name = "Correlation"
    )
  
  # Add correlation values if requested
  if (show_values) {
    p <- p + geom_text(
      aes(label = round(correlation, 2)),
      color = ifelse(abs(cor_long$correlation) > 0.5, "white", "black"),
      size = 3, fontface = "bold"
    )
  }
  
  # Styling
  p <- p +
    labs(
      title = title,
      subtitle = paste("Correlation analysis of", ncol(numeric_data), "numeric variables"),
      x = NULL, y = NULL,
      caption = paste("Method: Pearson correlation | n =", nrow(numeric_data))
    ) +
    theme_professional() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      axis.text.y = element_text(hjust = 1),
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "right"
    ) +
    coord_fixed()  # Equal aspect ratio
  
  return(p)
}

#' Advanced Bar Chart with Statistical Annotations
#'
#' @description Professional bar chart with error bars and statistical tests
#' @param data Data frame
#' @param x_var X-axis variable (categorical)
#' @param y_var Y-axis variable (numeric)
#' @param fill_var Fill variable (optional)
#' @param stat_function Statistical function ("mean", "median", "sum")
#' @param add_error_bars Logical, add error bars
#' @param horizontal Logical, create horizontal bars
#' @return ggplot object
#' @export
create_advanced_barplot <- function(data, x_var, y_var, fill_var = NULL, 
                                   stat_function = "mean", add_error_bars = TRUE,
                                   horizontal = FALSE) {
  
  # Calculate statistics
  if (!is.null(fill_var)) {
    stat_data <- data %>%
      group_by(!!sym(x_var), !!sym(fill_var)) %>%
      summarise(
        stat_value = case_when(
          stat_function == "mean" ~ mean(!!sym(y_var), na.rm = TRUE),
          stat_function == "median" ~ median(!!sym(y_var), na.rm = TRUE),
          stat_function == "sum" ~ sum(!!sym(y_var), na.rm = TRUE),
          TRUE ~ mean(!!sym(y_var), na.rm = TRUE)
        ),
        se = sd(!!sym(y_var), na.rm = TRUE) / sqrt(n()),
        n = n(),
        .groups = "drop"
      )
  } else {
    stat_data <- data %>%
      group_by(!!sym(x_var)) %>%
      summarise(
        stat_value = case_when(
          stat_function == "mean" ~ mean(!!sym(y_var), na.rm = TRUE),
          stat_function == "median" ~ median(!!sym(y_var), na.rm = TRUE),
          stat_function == "sum" ~ sum(!!sym(y_var), na.rm = TRUE),
          TRUE ~ mean(!!sym(y_var), na.rm = TRUE)
        ),
        se = sd(!!sym(y_var), na.rm = TRUE) / sqrt(n()),
        n = n(),
        .groups = "drop"
      )
  }
  
  # Create base plot
  if (!is.null(fill_var)) {
    p <- ggplot(stat_data, aes_string(x = x_var, y = "stat_value", fill = fill_var))
    position <- position_dodge(width = 0.8)
  } else {
    p <- ggplot(stat_data, aes_string(x = x_var, y = "stat_value"))
    position <- "identity"
  }
  
  # Add bars
  if (horizontal) {
    p <- p + geom_col(position = position, alpha = 0.8, width = 0.7) +
      coord_flip()
  } else {
    p <- p + geom_col(position = position, alpha = 0.8, width = 0.7)
  }
  
  # Add error bars
  if (add_error_bars && stat_function == "mean") {
    if (horizontal) {
      p <- p + geom_errorbarh(
        aes(xmin = stat_value - se, xmax = stat_value + se),
        position = position, width = 0.2, color = "black", size = 0.5
      )
    } else {
      p <- p + geom_errorbar(
        aes(ymin = stat_value - se, ymax = stat_value + se),
        position = position, width = 0.2, color = "black", size = 0.5
      )
    }
  }
  
  # Add value labels
  p <- p + geom_text(
    aes(label = comma_format(accuracy = 0.1)(stat_value)),
    position = position_dodge(width = 0.8),
    vjust = ifelse(horizontal, 0.5, -0.5),
    hjust = ifelse(horizontal, -0.1, 0.5),
    fontface = "bold", size = 3
  )
  
  # Statistical significance testing (if fill_var is provided)
  if (!is.null(fill_var) && n_distinct(data[[fill_var]]) == 2) {
    # Add significance indicators
    max_y <- max(stat_data$stat_value, na.rm = TRUE)
    p <- p + annotate(
      "text",
      x = 1:n_distinct(stat_data[[x_var]]),
      y = max_y * 1.1,
      label = "*",
      size = 6, fontface = "bold"
    )
  }
  
  # Styling
  p <- p +
    scale_y_continuous(
      labels = comma_format(),
      expand = expansion(mult = c(0, 0.15))
    ) +
    labs(
      title = paste(str_to_title(stat_function), "of", str_to_title(gsub("_", " ", y_var)), 
                   "by", str_to_title(gsub("_", " ", x_var))),
      subtitle = paste("Statistical analysis with", 
                      ifelse(add_error_bars, "standard errors", "summary statistics")),
      x = str_to_title(gsub("_", " ", x_var)),
      y = paste(str_to_title(stat_function), "of", str_to_title(gsub("_", " ", y_var))),
      caption = paste("n =", nrow(data), "| Method:", stat_function)
    ) +
    theme_professional() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Color scaling
  if (!is.null(fill_var)) {
    n_groups <- n_distinct(stat_data[[fill_var]])
    p <- p + scale_fill_manual(
      values = get_custom_palette("professional", n_groups),
      name = str_to_title(gsub("_", " ", fill_var))
    )
  } else {
    p <- p + scale_fill_manual(values = "#3498DB")
  }
  
  return(p)
}

#' Interactive Plotly Conversion
#'
#' @description Converts ggplot objects to interactive plotly visualizations
#' @param ggplot_obj ggplot object to convert
#' @param tooltip_vars Variables to show in tooltip
#' @return plotly object
#' @export
make_interactive <- function(ggplot_obj, tooltip_vars = c("x", "y")) {
  
  plotly_obj <- ggplotly(
    ggplot_obj,
    tooltip = tooltip_vars
  ) %>%
    layout(
      hovermode = "closest",
      showlegend = TRUE
    ) %>%
    config(
      displayModeBar = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
      displaylogo = FALSE
    )
  
  return(plotly_obj)
}

# =============================================================================
# COMPREHENSIVE VISUALIZATION DASHBOARD
# =============================================================================

#' Create Comprehensive Data Visualization Dashboard
#'
#' @description Generates a multi-panel dashboard with various visualization types
#' @param data Data frame for analysis
#' @param save_plots Logical, whether to save plots to files
#' @param output_path Directory to save plots
#' @return List of ggplot objects
#' @export
create_visualization_dashboard <- function(data, save_plots = FALSE, output_path = "outputs/plots/") {
  
  # Ensure output directory exists
  if (save_plots && !dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
  }
  
  # Generate sample data if none provided
  if (missing(data)) {
    set.seed(42)
    data <- tibble(
      id = 1:1000,
      category = sample(c("A", "B", "C", "D"), 1000, replace = TRUE),
      segment = sample(c("Premium", "Standard", "Basic"), 1000, replace = TRUE, prob = c(0.2, 0.5, 0.3)),
      value = rnorm(1000, 100, 25),
      revenue = abs(rnorm(1000, 1500, 500)),
      date = sample(seq(as.Date("2023-01-01"), as.Date("2024-12-31"), by = "day"), 1000, replace = TRUE),
      score = runif(1000, 0, 100),
      region = sample(c("North", "South", "East", "West"), 1000, replace = TRUE)
    ) %>%
    arrange(date)
  }
  
  plots <- list()
  
  # 1. Advanced Scatter Plot
  plots$scatter <- create_advanced_scatter(
    data = data,
    x_var = "value",
    y_var = "revenue",
    color_var = "segment",
    size_var = "score",
    add_regression = TRUE,
    add_correlation = TRUE
  )
  
  # 2. Distribution Analysis
  plots$distribution <- create_distribution_plot(
    data = data,
    var = "revenue",
    group_var = "segment",
    plot_type = "histogram"
  )
  
  # 3. Time Series Analysis
  if ("date" %in% names(data)) {
    time_data <- data %>%
      group_by(date, segment) %>%
      summarise(daily_revenue = sum(revenue, na.rm = TRUE), .groups = "drop")
    
    plots$timeseries <- create_timeseries_plot(
      data = time_data,
      date_var = "date",
      value_var = "daily_revenue",
      group_var = "segment",
      add_trend = TRUE
    )
  }
  
  # 4. Correlation Heatmap
  plots$correlation <- create_correlation_heatmap(
    data = data,
    title = "Variable Correlation Analysis",
    cluster_rows = TRUE,
    show_values = TRUE
  )
  
  # 5. Advanced Bar Chart
  plots$barplot <- create_advanced_barplot(
    data = data,
    x_var = "category",
    y_var = "revenue",
    fill_var = "segment",
    stat_function = "mean",
    add_error_bars = TRUE
  )
  
  # 6. Box Plot Comparison
  plots$boxplot <- create_distribution_plot(
    data = data,
    var = "value",
    group_var = "segment",
    plot_type = "boxplot"
  )
  
  # 7. Violin Plot
  plots$violin <- create_distribution_plot(
    data = data,
    var = "score",
    group_var = "category",
    plot_type = "violin"
  )
  
  # 8. Regional Analysis (Faceted Plot)
  plots$regional <- ggplot(data, aes(x = value, y = revenue, color = segment)) +
    geom_point(alpha = 0.6, size = 2) +
    geom_smooth(method = "lm", se = FALSE, size = 1) +
    facet_wrap(~ region, scales = "free") +
    scale_color_manual(values = get_custom_palette("professional", 3)) +
    labs(
      title = "Revenue vs Value Analysis by Region",
      subtitle = "Regional patterns in customer value relationships",
      x = "Customer Value Score",
      y = "Revenue ($)",
      color = "Segment"
    ) +
    theme_professional() +
    theme(
      strip.background = element_rect(fill = "#f8f9fa", color = "#dee2e6"),
      strip.text = element_text(face = "bold")
    )
  
  # 9. Density Ridge Plot (Alternative distribution visualization)
  if (require(ggridges, quietly = TRUE)) {
    plots$ridges <- ggplot(data, aes(x = revenue, y = segment, fill = segment)) +
      ggridges::geom_density_ridges(alpha = 0.8, scale = 1.2) +
      scale_fill_manual(values = get_custom_palette("professional", 3)) +
      scale_x_continuous(labels = comma_format()) +
      labs(
        title = "Revenue Distribution by Customer Segment",
        subtitle = "Density ridge plot showing distribution shapes",
        x = "Revenue ($)",
        y = "Customer Segment"
      ) +
      theme_professional() +
      theme(legend.position = "none")
  }
  
  # 10. Composite Dashboard Plot
  if (require(patchwork, quietly = TRUE)) {
    plots$dashboard <- (plots$scatter + plots$distribution) / 
                      (plots$barplot + plots$correlation) +
      plot_annotation(
        title = "Comprehensive Data Analysis Dashboard",
        subtitle = "Multi-panel visualization for business intelligence",
        caption = paste("Generated:", Sys.Date(), "| n =", nrow(data))
      )
  }
  
  # Save plots if requested
  if (save_plots) {
    for (plot_name in names(plots)) {
      if (plot_name != "dashboard") {  # Save dashboard separately due to size
        ggsave(
          filename = file.path(output_path, paste0(plot_name, "_analysis.png")),
          plot = plots[[plot_name]],
          width = 12, height = 8, dpi = 300, type = "cairo"
        )
      }
    }
    
    # Save dashboard with different dimensions
    if ("dashboard" %in% names(plots)) {
      ggsave(
        filename = file.path(output_path, "comprehensive_dashboard.png"),
        plot = plots$dashboard,
        width = 16, height = 12, dpi = 300, type = "cairo"
      )
    }
    
    cat("Plots saved to:", output_path, "\n")
  }
  
  return(plots)
}

#' Generate Interactive Visualization Report
#'
#' @description Creates interactive HTML report with multiple visualizations
#' @param data Data frame for analysis
#' @param output_file Output HTML file name
#' @return List of interactive plotly objects
#' @export
create_interactive_report <- function(data, output_file = "interactive_report.html") {
  
  # Generate base plots
  base_plots <- create_visualization_dashboard(data, save_plots = FALSE)
  
  # Convert to interactive
  interactive_plots <- list()
  
  # Scatter plot
  interactive_plots$scatter <- make_interactive(
    base_plots$scatter,
    tooltip_vars = c("x", "y", "colour", "size")
  )
  
  # Distribution plot
  interactive_plots$distribution <- make_interactive(
    base_plots$distribution,
    tooltip_vars = c("x", "y", "fill")
  )
  
  # Time series
  if ("timeseries" %in% names(base_plots)) {
    interactive_plots$timeseries <- make_interactive(
      base_plots$timeseries,
      tooltip_vars = c("x", "y", "colour")
    )
  }
  
  # Bar plot
  interactive_plots$barplot <- make_interactive(
    base_plots$barplot,
    tooltip_vars = c("x", "y", "fill")
  )
  
  return(interactive_plots)
}

# =============================================================================
# SPECIALIZED VISUALIZATION FUNCTIONS
# =============================================================================

#' Create Network-Style Correlation Plot
#'
#' @description Creates a network visualization of correlations
#' @param data Data frame with numeric variables
#' @param threshold Minimum correlation threshold to display
#' @return ggplot object
#' @export
create_correlation_network <- function(data, threshold = 0.5) {
  
  # Select numeric variables
  numeric_data <- data %>% select_if(is.numeric)
  cor_matrix <- cor(numeric_data, use = "complete.obs")
  
  # Convert to edge list
  cor_edges <- expand.grid(
    from = rownames(cor_matrix),
    to = colnames(cor_matrix),
    stringsAsFactors = FALSE
  ) %>%
    mutate(correlation = as.vector(cor_matrix)) %>%
    filter(
      from != to,  # Remove self-correlations
      abs(correlation) >= threshold  # Apply threshold
    )
  
  # Create node positions (circular layout)
  n_vars <- nrow(cor_matrix)
  angles <- seq(0, 2 * pi, length.out = n_vars + 1)[1:n_vars]
  
  nodes <- tibble(
    variable = rownames(cor_matrix),
    x = cos(angles),
    y = sin(angles)
  )
  
  # Add node positions to edges
  edge_coords <- cor_edges %>%
    left_join(nodes, by = c("from" = "variable")) %>%
    rename(x_from = x, y_from = y) %>%
    left_join(nodes, by = c("to" = "variable")) %>%
    rename(x_to = x, y_to = y)
  
  # Create the network plot
  p <- ggplot() +
    # Draw edges
    geom_segment(
      data = edge_coords,
      aes(x = x_from, y = y_from, xend = x_to, yend = y_to, 
          color = correlation, size = abs(correlation)),
      alpha = 0.7
    ) +
    # Draw nodes
    geom_point(
      data = nodes,
      aes(x = x, y = y),
      size = 8, color = "#2C3E50", alpha = 0.8
    ) +
    # Add variable labels
    geom_text(
      data = nodes,
      aes(x = x * 1.2, y = y * 1.2, label = variable),
      size = 3, fontface = "bold"
    ) +
    scale_color_gradient2(
      low = "#E74C3C", mid = "grey80", high = "#3498DB",
      midpoint = 0, name = "Correlation"
    ) +
    scale_size_continuous(
      range = c(0.5, 3), guide = "none"
    ) +
    labs(
      title = "Correlation Network Analysis",
      subtitle = paste("Showing correlations above threshold:", threshold),
      caption = paste("Variables:", n_vars, "| Edges:", nrow(edge_coords))
    ) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      plot.caption = element_text(hjust = 0.5, size = 10),
      legend.position = "bottom"
    ) +
    coord_fixed()
  
  return(p)
}

#' Create Animated Plot (requires gganimate)
#'
#' @description Creates an animated visualization showing changes over time
#' @param data Data frame with time variable
#' @param time_var Time variable name
#' @param x_var X-axis variable
#' @param y_var Y-axis variable
#' @param group_var Grouping variable
#' @return gganimate object (if package available)
#' @export
create_animated_plot <- function(data, time_var, x_var, y_var, group_var = NULL) {
  
  if (!require(gganimate, quietly = TRUE)) {
    warning("gganimate package not available. Returning static plot.")
    return(create_advanced_scatter(data, x_var, y_var, group_var))
  }
  
  # Ensure time variable is properly formatted
  if (is.character(data[[time_var]]) || is.factor(data[[time_var]])) {
    data[[time_var]] <- as.Date(data[[time_var]])
  }
  
  # Create base animated plot
  if (!is.null(group_var)) {
    p <- ggplot(data, aes_string(x = x_var, y = y_var, color = group_var)) +
      scale_color_manual(values = get_custom_palette("professional", n_distinct(data[[group_var]])))
  } else {
    p <- ggplot(data, aes_string(x = x_var, y = y_var)) +
      scale_color_manual(values = "#3498DB")
  }
  
  p <- p +
    geom_point(size = 3, alpha = 0.8) +
    scale_x_continuous(labels = comma_format()) +
    scale_y_continuous(labels = comma_format()) +
    labs(
      title = paste("Animated Analysis:", str_to_title(gsub("_", " ", y_var)), 
                   "vs", str_to_title(gsub("_", " ", x_var))),
      subtitle = "Date: {closest_state}",
      x = str_to_title(gsub("_", " ", x_var)),
      y = str_to_title(gsub("_", " ", y_var))
    ) +
    theme_professional() +
    transition_states(!!sym(time_var),
                     transition_length = 1,
                     state_length = 1) +
    ease_aes("linear")
  
  return(p)
}

#' Run Complete Visualization Demonstration
#'
#' @description Executes comprehensive visualization portfolio
#' @param generate_sample_data Logical, whether to generate sample data
#' @param save_outputs Logical, whether to save plots
#' @param verbose Logical, whether to show progress
#' @return List of all visualization objects
#' @export
run_visualization_demo <- function(generate_sample_data = TRUE, save_outputs = FALSE, verbose = TRUE) {
  
  if (verbose) {
    cat("\n", "="*60, "\n")
    cat("       ADVANCED VISUALIZATION PORTFOLIO\n")
    cat("="*60, "\n\n")
  }
  
  # Generate or use provided data
  if (generate_sample_data) {
    if (verbose) cat("1. Generating sample business data...\n")
    
    set.seed(42)
    demo_data <- tibble(
      customer_id = 1:500,
      segment = sample(c("Premium", "Standard", "Basic"), 500, replace = TRUE, prob = c(0.2, 0.5, 0.3)),
      region = sample(c("North", "South", "East", "West"), 500, replace = TRUE),
      age = sample(25:65, 500, replace = TRUE),
      income = rnorm(500, 50000, 15000),
      satisfaction = runif(500, 1, 10),
      purchase_amount = abs(rnorm(500, 500, 200)),
      purchase_frequency = rpois(500, 3),
      loyalty_score = runif(500, 0, 100),
      date = sample(seq(as.Date("2023-01-01"), as.Date("2024-12-31"), by = "day"), 500, replace = TRUE)
    ) %>%
    mutate(
      customer_value = purchase_amount * purchase_frequency,
      lifetime_value = customer_value * (loyalty_score / 10)
    ) %>%
    arrange(date)
  }
  
  results <- list()
  
  # Create comprehensive dashboard
  if (verbose) cat("2. Creating comprehensive visualization dashboard...\n")
  results$dashboard_plots <- create_visualization_dashboard(
    data = demo_data,
    save_plots = save_outputs,
    output_path = "outputs/plots/"
  )
  
  # Create interactive visualizations
  if (verbose) cat("3. Generating interactive plots...\n")
  results$interactive_plots <- create_interactive_report(demo_data)
  
  # Create specialized visualizations
  if (verbose) cat("4. Creating specialized visualizations...\n")
  
  # Correlation network
  results$correlation_network <- create_correlation_network(
    demo_data %>% select_if(is.numeric),
    threshold = 0.3
  )
  
  # Custom theme demonstrations
  results$theme_comparisons <- list(
    professional = create_advanced_scatter(demo_data, "age", "income", "segment") + 
      theme_professional() + labs(title = "Professional Theme"),
    
    dark_modern = create_advanced_scatter(demo_data, "age", "income", "segment") + 
      theme_dark_modern() + labs(title = "Dark Modern Theme"),
    
    minimal = create_advanced_scatter(demo_data, "age", "income", "segment") + 
      theme_minimal() + labs(title = "Minimal Theme")
  )
  
  # Portfolio summary statistics
  results$portfolio_stats <- list(
    total_plots_created = length(unlist(results, recursive = FALSE)),
    data_points_visualized = nrow(demo_data),
    plot_types = c("scatter", "distribution", "timeseries", "heatmap", "barplot", 
                  "boxplot", "violin", "network", "interactive"),
    themes_demonstrated = c("professional", "dark_modern", "minimal"),
    color_palettes = c("professional", "business", "modern", "muted", "vibrant")
  )
  
  if (verbose) {
    cat("\n", "="*60, "\n")
    cat("    VISUALIZATION PORTFOLIO COMPLETE\n")
    cat("="*60, "\n")
    cat("Total visualizations created:", results$portfolio_stats$total_plots_created, "\n")
    cat("Data points visualized:", results$portfolio_stats$data_points_visualized, "\n")
    cat("Plot types demonstrated:", length(results$portfolio_stats$plot_types), "\n")
    cat("Custom themes available:", length(results$portfolio_stats$themes_demonstrated), "\n")
    
    if (save_outputs) {
      cat("Outputs saved to: outputs/plots/\n")
    }
  }
  
  invisible(results)
}

# =============================================================================
# EXAMPLE USAGE (Run only in interactive mode)
# =============================================================================

if (interactive()) {
  # Run the complete visualization demonstration
  viz_results <- run_visualization_demo(
    generate_sample_data = TRUE,
    save_outputs = TRUE,
    verbose = TRUE
  )
  
  # Display specific plots
  # print(viz_results$dashboard_plots$scatter)
  # print(viz_results$correlation_network)
  
  # View interactive plot
  # viz_results$interactive_plots$scatter
  
  # Compare themes
  # viz_results$theme_comparisons$professional
  # viz_results$theme_comparisons$dark_modern
}