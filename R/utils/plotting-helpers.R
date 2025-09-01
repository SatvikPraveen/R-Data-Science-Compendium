# @title Plotting Helpers and Utilities
# @description Comprehensive plotting utilities and helper functions
# @author R Data Science Portfolio Developer
# @date 2025

#' ========================================
#' PLOTTING HELPERS AND UTILITIES
#' ========================================

# Load required libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  ggplot2, scales, RColorBrewer, viridis, gridExtra, 
  grid, gtable, cowplot, patchwork, ggrepel, ggridges
)

#' ========================================
#' 1. COLOR PALETTES AND THEMES
#' ========================================

#' Get Custom Color Palette
#' @param palette_name Name of palette ("professional", "business", "scientific", "vibrant")
#' @param n Number of colors needed
#' @param reverse Should palette be reversed?
#' @return Vector of color codes
get_custom_palette <- function(palette_name = "professional", n = 8, reverse = FALSE) {
  
  palettes <- list(
    "professional" = c("#2C3E50", "#3498DB", "#E74C3C", "#F39C12", 
                      "#27AE60", "#9B59B6", "#1ABC9C", "#34495E"),
    
    "business" = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", 
                  "#9467bd", "#8c564b", "#e377c2", "#7f7f7f"),
    
    "scientific" = c("#000000", "#E69F00", "#56B4E9", "#009E73", 
                    "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
    
    "vibrant" = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", 
                 "#FFEAA7", "#DDA0DD", "#FFB74D", "#81C784"),
    
    "earth" = c("#8D4004", "#B85450", "#C7956D", "#F2CC8F", 
               "#81B366", "#3A6B35", "#2F4F4F", "#8FBC8F"),
    
    "ocean" = c("#003f5c", "#2f4b7c", "#665191", "#a05195", 
               "#d45087", "#f95d6a", "#ff7c43", "#ffa600")
  )
  
  if (!palette_name %in% names(palettes)) {
    stop("Unknown palette name. Available: ", paste(names(palettes), collapse = ", "))
  }
  
  colors <- palettes[[palette_name]]
  
  if (reverse) {
    colors <- rev(colors)
  }
  
  # Repeat colors if more needed
  if (n > length(colors)) {
    colors <- rep(colors, ceiling(n / length(colors)))
  }
  
  return(colors[1:n])
}

#' Create Custom ggplot2 Theme
#' @param base_size Base font size
#' @param base_family Base font family
#' @param grid Should grid lines be shown?
#' @param border Should plot border be shown?
#' @return ggplot2 theme object
theme_professional <- function(base_size = 12, base_family = "", 
                              grid = TRUE, border = FALSE) {
  
  theme_base <- theme_minimal(base_size = base_size, base_family = base_family)
  
  theme_custom <- theme_base +
    theme(
      # Text elements
      plot.title = element_text(size = rel(1.2), face = "bold", 
                               margin = margin(b = 20), hjust = 0),
      plot.subtitle = element_text(size = rel(1), color = "grey40", 
                                  margin = margin(b = 20)),
      plot.caption = element_text(size = rel(0.8), color = "grey50", 
                                 hjust = 1, margin = margin(t = 15)),
      
      # Axis elements
      axis.title = element_text(size = rel(1), face = "bold"),
      axis.text = element_text(size = rel(0.9)),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      
      # Legend elements
      legend.title = element_text(size = rel(1), face = "bold"),
      legend.text = element_text(size = rel(0.9)),
      legend.position = "bottom",
      legend.box = "horizontal",
      
      # Panel elements
      panel.spacing = unit(1, "lines"),
      
      # Strip elements (for facets)
      strip.text = element_text(size = rel(1), face = "bold", 
                               margin = margin(b = 5, t = 5)),
      strip.background = element_rect(fill = "grey90", color = NA)
    )
  
  # Grid lines
  if (!grid) {
    theme_custom <- theme_custom +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  }
  
  # Border
  if (border) {
    theme_custom <- theme_custom +
      theme(
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)
      )
  }
  
  return(theme_custom)
}

#' Create Presentation Theme
#' @param base_size Base font size (larger for presentations)
#' @return ggplot2 theme for presentations
theme_presentation <- function(base_size = 16) {
  
  theme_professional(base_size = base_size) +
    theme(
      plot.title = element_text(size = rel(1.4), face = "bold"),
      plot.subtitle = element_text(size = rel(1.1)),
      axis.title = element_text(size = rel(1.2)),
      axis.text = element_text(size = rel(1)),
      legend.text = element_text(size = rel(1)),
      strip.text = element_text(size = rel(1.1))
    )
}

#' ========================================
#' 2. PLOT LAYOUT AND ARRANGEMENT
#' ========================================

#' Arrange Multiple Plots with Titles
#' @param plot_list List of ggplot objects
#' @param ncol Number of columns
#' @param nrow Number of rows
#' @param main_title Main title for the arrangement
#' @param subtitle Subtitle for the arrangement
#' @return Grid arrangement of plots
arrange_plots <- function(plot_list, ncol = 2, nrow = NULL, 
                         main_title = NULL, subtitle = NULL) {
  
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("gridExtra package required for plot arrangement")
  }
  
  # Calculate rows if not specified
  if (is.null(nrow)) {
    nrow <- ceiling(length(plot_list) / ncol)
  }
  
  # Create the arrangement
  arranged_plot <- gridExtra::grid.arrange(grobs = plot_list, ncol = ncol, nrow = nrow)
  
  # Add titles if specified
  if (!is.null(main_title) || !is.null(subtitle)) {
    
    title_grob <- grid::textGrob(
      label = main_title %||% "",
      gp = grid::gpar(fontsize = 16, fontface = "bold")
    )
    
    subtitle_grob <- grid::textGrob(
      label = subtitle %||% "",
      gp = grid::gpar(fontsize = 12, col = "grey40")
    )
    
    # Combine title, subtitle, and plots
    if (!is.null(main_title) && !is.null(subtitle)) {
      final_plot <- gridExtra::grid.arrange(
        title_grob, subtitle_grob, arranged_plot,
        heights = c(0.1, 0.05, 0.85)
      )
    } else if (!is.null(main_title)) {
      final_plot <- gridExtra::grid.arrange(
        title_grob, arranged_plot,
        heights = c(0.1, 0.9)
      )
    }
    
    return(final_plot)
  }
  
  return(arranged_plot)
}

#' Create Figure with Multiple Panels
#' @param plot_list List of ggplot objects
#' @param labels Vector of panel labels (A, B, C, etc.)
#' @param label_size Size of panel labels
#' @return Combined figure with labeled panels
create_figure_panels <- function(plot_list, labels = LETTERS[1:length(plot_list)], 
                                label_size = 14) {
  
  if (!requireNamespace("cowplot", quietly = TRUE)) {
    stop("cowplot package required for panel creation")
  }
  
  # Add labels to plots
  labeled_plots <- map2(plot_list, labels, function(plot, label) {
    cowplot::plot_grid(
      plot,
      labels = label,
      label_size = label_size,
      hjust = -0.1,
      vjust = 1.1
    )
  })
  
  # Arrange labeled plots
  cowplot::plot_grid(plotlist = labeled_plots)
}

#' ========================================
#' 3. STATISTICAL PLOT HELPERS
#' ========================================

#' Add Statistical Annotations to Plot
#' @param plot ggplot object
#' @param x_var X variable name
#' @param y_var Y variable name
#' @param group_var Grouping variable (optional)
#' @param test_type Type of test ("t.test", "wilcox.test", "anova")
#' @return Plot with statistical annotations
add_statistical_annotations <- function(plot, x_var, y_var, group_var = NULL, 
                                      test_type = "t.test") {
  
  if (!requireNamespace("ggpubr", quietly = TRUE)) {
    warning("ggpubr package not available, returning original plot")
    return(plot)
  }
  
  # Add statistical comparisons based on test type
  if (test_type == "t.test" && !is.null(group_var)) {
    plot + ggpubr::stat_compare_means(method = "t.test")
  } else if (test_type == "wilcox.test" && !is.null(group_var)) {
    plot + ggpubr::stat_compare_means(method = "wilcox.test")
  } else if (test_type == "anova") {
    plot + ggpubr::stat_compare_means(method = "anova")
  } else {
    plot
  }
}

#' Create Correlation Matrix Plot
#' @param data Dataset with numeric variables
#' @param method Correlation method ("pearson", "spearman", "kendall")
#' @param show_values Should correlation values be displayed?
#' @param title Plot title
#' @return Correlation matrix plot
plot_correlation_matrix <- function(data, method = "pearson", show_values = TRUE, 
                                  title = "Correlation Matrix") {
  
  # Select only numeric columns
  numeric_data <- data[sapply(data, is.numeric)]
  
  if (ncol(numeric_data) < 2) {
    stop("Need at least 2 numeric columns for correlation matrix")
  }
  
  # Calculate correlation matrix
  cor_matrix <- cor(numeric_data, use = "complete.obs", method = method)
  
  # Convert to long format
  cor_data <- expand.grid(Var1 = rownames(cor_matrix), Var2 = colnames(cor_matrix))
  cor_data$value <- as.vector(cor_matrix)
  
  # Create plot
  p <- ggplot(cor_data, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white", size = 0.5) +
    scale_fill_gradient2(
      low = "#E74C3C", mid = "white", high = "#3498DB",
      midpoint = 0, limits = c(-1, 1),
      name = "Correlation"
    ) +
    labs(title = title, x = "", y = "") +
    theme_professional() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right"
    ) +
    coord_fixed()
  
  # Add correlation values if requested
  if (show_values) {
    p <- p + geom_text(aes(label = round(value, 2)), color = "black", size = 3)
  }
  
  return(p)
}

#' Create Distribution Comparison Plot
#' @param data Dataset
#' @param value_var Variable to plot distribution for
#' @param group_var Grouping variable
#' @param plot_type Type of plot ("density", "histogram", "boxplot", "violin")
#' @return Distribution comparison plot
plot_distribution_comparison <- function(data, value_var, group_var, 
                                       plot_type = "density") {
  
  base_plot <- ggplot(data, aes_string(x = value_var, fill = group_var))
  
  if (plot_type == "density") {
    p <- base_plot + 
      geom_density(alpha = 0.7) +
      labs(title = paste("Distribution of", value_var, "by", group_var),
           x = value_var, y = "Density")
  
  } else if (plot_type == "histogram") {
    p <- base_plot + 
      geom_histogram(alpha = 0.7, bins = 30, position = "identity") +
      labs(title = paste("Histogram of", value_var, "by", group_var),
           x = value_var, y = "Count")
  
  } else if (plot_type == "boxplot") {
    p <- ggplot(data, aes_string(x = group_var, y = value_var, fill = group_var)) +
      geom_boxplot(alpha = 0.7) +
      labs(title = paste("Boxplot of", value_var, "by", group_var),
           x = group_var, y = value_var)
  
  } else if (plot_type == "violin") {
    p <- ggplot(data, aes_string(x = group_var, y = value_var, fill = group_var)) +
      geom_violin(alpha = 0.7) +
      geom_boxplot(width = 0.1, alpha = 0.8) +
      labs(title = paste("Violin Plot of", value_var, "by", group_var),
           x = group_var, y = value_var)
  }
  
  p + theme_professional() + 
    scale_fill_manual(values = get_custom_palette("professional", 
                                                 length(unique(data[[group_var]]))))
}

#' ========================================
#' 4. SPECIALIZED PLOT FUNCTIONS
#' ========================================

#' Create Waterfall Chart
#' @param data Data frame with categories and values
#' @param category_col Category column name
#' @param value_col Value column name
#' @param title Plot title
#' @return Waterfall chart
create_waterfall_chart <- function(data, category_col, value_col, 
                                  title = "Waterfall Chart") {
  
  # Prepare data for waterfall
  data <- data[order(match(data[[category_col]], data[[category_col]])), ]
  data$cumulative <- cumsum(data[[value_col]])
  data$end <- data$cumulative
  data$start <- data$end - data[[value_col]]
  
  # Determine colors based on positive/negative values
  data$color <- ifelse(data[[value_col]] >= 0, "Positive", "Negative")
  
  ggplot(data) +
    geom_rect(aes_string(
      xmin = paste0("as.numeric(factor(", category_col, ")) - 0.4"),
      xmax = paste0("as.numeric(factor(", category_col, ")) + 0.4"),
      ymin = "start", ymax = "end", fill = "color"
    ), alpha = 0.8) +
    geom_text(aes_string(
      x = paste0("as.numeric(factor(", category_col, "))"),
      y = "end + 0.1 * max(abs(end))",
      label = value_col
    ), size = 3) +
    scale_x_continuous(
      breaks = 1:nrow(data),
      labels = data[[category_col]]
    ) +
    scale_fill_manual(values = c("Positive" = "#27AE60", "Negative" = "#E74C3C")) +
    labs(title = title, x = category_col, y = "Value", fill = "") +
    theme_professional() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#' Create Bullet Chart
#' @param actual Actual values
#' @param target Target values
#' @param labels Category labels
#' @param title Plot title
#' @return Bullet chart
create_bullet_chart <- function(actual, target, labels, title = "Performance vs Target") {
  
  chart_data <- data.frame(
    category = factor(labels, levels = rev(labels)),
    actual = actual,
    target = target,
    performance = actual / target
  )
  
  ggplot(chart_data) +
    # Target bars (background)
    geom_col(aes(x = category, y = target), 
             fill = "grey80", alpha = 0.7, width = 0.6) +
    # Actual bars
    geom_col(aes(x = category, y = actual, fill = performance), 
             width = 0.4, alpha = 0.9) +
    # Target line
    geom_point(aes(x = category, y = target), 
               color = "black", size = 3, shape = "|") +
    scale_fill_gradient2(
      low = "#E74C3C", mid = "#F39C12", high = "#27AE60",
      midpoint = 1, name = "Performance"
    ) +
    coord_flip() +
    labs(title = title, x = "", y = "Value") +
    theme_professional()
}

#' Create Slope Graph
#' @param data Data frame with categories and two time periods
#' @param category_col Category column
#' @param time1_col First time period column
#' @param time2_col Second time period column
#' @param time1_label Label for first time period
#' @param time2_label Label for second time period
#' @return Slope graph
create_slope_graph <- function(data, category_col, time1_col, time2_col,
                              time1_label = "Time 1", time2_label = "Time 2") {
  
  # Reshape data for slope graph
  slope_data <- data.frame(
    category = rep(data[[category_col]], 2),
    time = rep(c(time1_label, time2_label), each = nrow(data)),
    value = c(data[[time1_col]], data[[time2_col]])
  )
  
  # Calculate change
  change_data <- data.frame(
    category = data[[category_col]],
    change = data[[time2_col]] - data[[time1_col]]
  )
  
  ggplot(slope_data, aes(x = time, y = value, group = category)) +
    geom_line(aes(color = category), size = 1, alpha = 0.8) +
    geom_point(aes(color = category), size = 3) +
    geom_text_repel(
      data = slope_data[slope_data$time == time1_label, ],
      aes(label = category), hjust = 1, direction = "y"
    ) +
    scale_color_manual(values = get_custom_palette("professional", nrow(data))) +
    labs(title = paste("Change from", time1_label, "to", time2_label),
         x = "", y = "Value") +
    theme_professional() +
    theme(legend.position = "none")
}

#' ========================================
#' 5. DATA QUALITY VISUALIZATION
#' ========================================

#' Plot Missing Data Pattern
#' @param data Dataset to analyze
#' @param title Plot title
#' @return Missing data visualization
plot_missing_data <- function(data, title = "Missing Data Pattern") {
  
  # Calculate missing data
  missing_data <- data.frame(
    variable = names(data),
    missing_count = sapply(data, function(x) sum(is.na(x))),
    missing_pct = sapply(data, function(x) sum(is.na(x)) / length(x) * 100)
  )
  
  missing_data <- missing_data[order(missing_data$missing_pct, decreasing = TRUE), ]
  missing_data$variable <- factor(missing_data$variable, levels = missing_data$variable)
  
  # Create plot
  p1 <- ggplot(missing_data, aes(x = variable, y = missing_pct)) +
    geom_col(fill = "#E74C3C", alpha = 0.8) +
    geom_text(aes(label = paste0(round(missing_pct, 1), "%")), 
              vjust = -0.5, size = 3) +
    labs(title = title, x = "Variables", y = "Missing Data (%)") +
    theme_professional() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Missing data heatmap
  missing_matrix <- is.na(data)
  missing_long <- expand.grid(
    row = 1:nrow(data),
    col = names(data)
  )
  missing_long$missing <- as.vector(missing_matrix)
  
  p2 <- ggplot(missing_long, aes(x = col, y = row, fill = missing)) +
    geom_tile() +
    scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "#E74C3C"),
                     name = "Missing") +
    labs(title = "Missing Data Heatmap", x = "Variables", y = "Observations") +
    theme_professional() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_blank())
  
  # Combine plots
  gridExtra::grid.arrange(p1, p2, ncol = 1, heights = c(1, 2))
}

#' Plot Data Distribution Summary
#' @param data Dataset to analyze
#' @param max_cats Maximum categories to show for categorical variables
#' @return Data distribution plots
plot_data_distribution <- function(data, max_cats = 10) {
  
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  categorical_vars <- names(data)[sapply(data, function(x) is.character(x) | is.factor(x))]
  
  plots <- list()
  
  # Numeric variable distributions
  if (length(numeric_vars) > 0) {
    numeric_plots <- map(numeric_vars[1:min(4, length(numeric_vars))], function(var) {
      ggplot(data, aes_string(x = var)) +
        geom_histogram(bins = 30, fill = "#3498DB", alpha = 0.8) +
        labs(title = paste("Distribution of", var), x = var, y = "Count") +
        theme_professional()
    })
    plots <- c(plots, numeric_plots)
  }
  
  # Categorical variable distributions
  if (length(categorical_vars) > 0) {
    categorical_plots <- map(categorical_vars[1:min(2, length(categorical_vars))], function(var) {
      
      # Limit categories if too many
      var_data <- data[[var]]
      if (length(unique(var_data)) > max_cats) {
        top_cats <- names(sort(table(var_data), decreasing = TRUE))[1:max_cats]
        var_data[!var_data %in% top_cats] <- "Other"
      }
      
      plot_data <- data.frame(category = var_data)
      
      ggplot(plot_data, aes(x = category)) +
        geom_bar(fill = "#27AE60", alpha = 0.8) +
        labs(title = paste("Distribution of", var), x = var, y = "Count") +
        theme_professional() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    plots <- c(plots, categorical_plots)
  }
  
  # Arrange plots
  if (length(plots) > 0) {
    arrange_plots(plots, ncol = 2, main_title = "Data Distribution Summary")
  } else {
    cat("No suitable variables found for distribution plotting\n")
  }
}

#' ========================================
#' 6. EXPORT AND SAVING UTILITIES
#' ========================================

#' Save Plot with Multiple Formats
#' @param plot ggplot object to save
#' @param filename Base filename (without extension)
#' @param formats Vector of formats to save ("png", "pdf", "svg", "jpg")
#' @param width Plot width in inches
#' @param height Plot height in inches
#' @param dpi Resolution for raster formats
save_plot_multiple_formats <- function(plot, filename, 
                                     formats = c("png", "pdf"), 
                                     width = 10, height = 8, dpi = 300) {
  
  saved_files <- character()
  
  for (format in formats) {
    full_filename <- paste0(filename, ".", format)
    
    if (format == "png") {
      ggsave(full_filename, plot, width = width, height = height, dpi = dpi)
    } else if (format == "pdf") {
      ggsave(full_filename, plot, width = width, height = height, device = "pdf")
    } else if (format == "svg") {
      ggsave(full_filename, plot, width = width, height = height, device = "svg")
    } else if (format == "jpg") {
      ggsave(full_filename, plot, width = width, height = height, dpi = dpi, device = "jpg")
    }
    
    saved_files <- c(saved_files, full_filename)
    cat("Saved:", full_filename, "\n")
  }
  
  return(saved_files)
}

#' Create Plot Gallery
#' @param plot_list Named list of plots
#' @param output_dir Directory to save plots
#' @param formats Formats to save
#' @return List of saved file paths
create_plot_gallery <- function(plot_list, output_dir = "plots", 
                               formats = c("png", "pdf")) {
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  saved_files <- list()
  
  for (plot_name in names(plot_list)) {
    plot <- plot_list[[plot_name]]
    filename <- file.path(output_dir, plot_name)
    
    files <- save_plot_multiple_formats(plot, filename, formats)
    saved_files[[plot_name]] <- files
  }
  
  cat("Plot gallery created in:", output_dir, "\n")
  cat("Total plots saved:", length(plot_list), "\n")
  
  return(saved_files)
}

#' ========================================
#' 7. DEMONSTRATION FUNCTIONS
#' ========================================

#' Generate Demo Data for Plotting
generate_plotting_demo_data <- function() {
  
  set.seed(42)
  n <- 200
  
  # Main dataset
  demo_data <- data.frame(
    id = 1:n,
    category = sample(c("A", "B", "C", "D"), n, replace = TRUE),
    value1 = rnorm(n, 50, 15),
    value2 = rnorm(n, 100, 25),
    group = sample(c("Group 1", "Group 2", "Group 3"), n, replace = TRUE),
    time1 = rnorm(n, 80, 20),
    time2 = rnorm(n, 85, 22),
    stringsAsFactors = FALSE
  )
  
  # Add some missing values
  demo_data$value1[sample(1:n, 10)] <- NA
  demo_data$value2[sample(1:n, 15)] <- NA
  
  # Correlation data
  cor_data <- data.frame(
    var1 = rnorm(100),
    var2 = rnorm(100),
    var3 = rnorm(100)
  )
  cor_data$var4 <- 0.7 * cor_data$var1 + 0.3 * rnorm(100)
  cor_data$var5 <- -0.5 * cor_data$var2 + 0.5 * rnorm(100)
  
  # Waterfall data
  waterfall_data <- data.frame(
    category = c("Starting", "Increase 1", "Decrease 1", "Increase 2", "Final"),
    value = c(100, 20, -15, 30, 135)
  )
  
  return(list(
    main_data = demo_data,
    correlation_data = cor_data,
    waterfall_data = waterfall_data
  ))
}

#' Demonstrate Plotting Utilities
demo_plotting_helpers <- function() {
  cat("=== PLOTTING HELPERS DEMONSTRATION ===\n\n")
  
  # Generate demo data
  demo_data <- generate_plotting_demo_data()
  
  cat("1. COLOR PALETTES AND THEMES\n")
  cat("="*40, "\n")
  
  # Show available palettes
  palette_names <- c("professional", "business", "scientific", "vibrant")
  
  for (palette in palette_names) {
    colors <- get_custom_palette(palette, 5)
    cat(palette, "palette:", paste(colors, collapse = ", "), "\n")
  }
  
  cat("\n2. STATISTICAL PLOTS\n")
  cat("="*40, "\n")
  
  # Correlation matrix
  cor_plot <- plot_correlation_matrix(demo_data$correlation_data, 
                                     title = "Demo Correlation Matrix")
  cat("Created correlation matrix plot\n")
  
  # Distribution comparison
  dist_plot <- plot_distribution_comparison(demo_data$main_data, "value1", "group", 
                                          plot_type = "density")
  cat("Created distribution comparison plot\n")
  
  cat("\n3. SPECIALIZED CHARTS\n")
  cat("="*40, "\n")
  
  # Waterfall chart
  waterfall_plot <- create_waterfall_chart(demo_data$waterfall_data, 
                                          "category", "value")
  cat("Created waterfall chart\n")
  
  # Bullet chart
  bullet_plot <- create_bullet_chart(
    actual = c(85, 92, 78, 95),
    target = c(90, 85, 80, 100),
    labels = c("Sales", "Marketing", "Support", "Development"),
    title = "Department Performance"
  )
  cat("Created bullet chart\n")
  
  # Slope graph
  slope_plot <- create_slope_graph(demo_data$main_data[1:10, ], 
                                  "category", "time1", "time2",
                                  "Q1", "Q2")
  cat("Created slope graph\n")
  
  cat("\n4. DATA QUALITY VISUALIZATION\n")
  cat("="*40, "\n")
  
  # Missing data plot
  missing_plot <- plot_missing_data(demo_data$main_data)
  cat("Created missing data visualization\n")
  
  # Data distribution summary
  dist_summary <- plot_data_distribution(demo_data$main_data)
  cat("Created data distribution summary\n")
  
  cat("\n5. PLOT ARRANGEMENT\n")
  cat("="*40, "\n")
  
  # Create multiple plots for arrangement
  plots_list <- list(
    cor_plot = cor_plot,
    dist_plot = dist_plot,
    waterfall_plot = waterfall_plot,
    bullet_plot = bullet_plot
  )
  
  # Arrange plots
  arranged_plot <- arrange_plots(plots_list, ncol = 2, 
                                main_title = "Demo Plot Collection",
                                subtitle = "Various plot types demonstration")
  cat("Arranged multiple plots with titles\n")
  
  cat("\nPlotting Helpers Demo Complete!\n")
  cat("All plotting utilities demonstrated successfully.\n")
  
  return(list(
    demo_data = demo_data,
    individual_plots = plots_list,
    arranged_plot = arranged_plot
  ))
}

# Export key functions
plotting_helpers_exports <- list(
  get_custom_palette = get_custom_palette,
  theme_professional = theme_professional,
  theme_presentation = theme_presentation,
  arrange_plots = arrange_plots,
  create_figure_panels = create_figure_panels,
  plot_correlation_matrix = plot_correlation_matrix,
  plot_distribution_comparison = plot_distribution_comparison,
  create_waterfall_chart = create_waterfall_chart,
  create_bullet_chart = create_bullet_chart,
  create_slope_graph = create_slope_graph,
  plot_missing_data = plot_missing_data,
  plot_data_distribution = plot_data_distribution,
  save_plot_multiple_formats = save_plot_multiple_formats,
  create_plot_gallery = create_plot_gallery,
  generate_plotting_demo_data = generate_plotting_demo_data,
  demo_plotting_helpers = demo_plotting_helpers
)

# Run demo if script is executed directly
if (sys.nframe() == 0) {
  demo_results <- demo_plotting_helpers()
}