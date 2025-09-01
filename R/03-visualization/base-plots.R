# @title Base R Plotting System
# @description Comprehensive guide to base R graphics and plotting functions
# @author R Data Science Portfolio
# @date 2025

#' ========================================
#' BASE R PLOTTING SYSTEM
#' ========================================

# Comprehensive framework for base R graphics including plots, customization, and advanced techniques

#' ========================================
#' 1. BASIC PLOT TYPES
#' ========================================

#' Generate Sample Data for Plotting Demonstrations
#' @param n Number of observations
#' @return List of datasets for different plot types
generate_plot_data <- function(n = 500) {
  set.seed(42)
  
  list(
    continuous = data.frame(
      x = rnorm(n),
      y = 2 * rnorm(n) + 0.5 * rnorm(n),
      group = sample(c("A", "B", "C"), n, replace = TRUE),
      size = abs(rnorm(n, 10, 3))
    ),
    
    categorical = data.frame(
      category = sample(c("Sales", "Marketing", "Engineering", "HR", "Finance"), n, replace = TRUE),
      values = abs(rnorm(n, 100, 30)),
      region = sample(c("North", "South", "East", "West"), n, replace = TRUE)
    ),
    
    time_series = data.frame(
      date = seq(as.Date("2020-01-01"), length.out = 365, by = "day"),
      sales = 1000 + 200 * sin(2 * pi * (1:365) / 365) + rnorm(365, 0, 50),
      temperature = 20 + 15 * sin(2 * pi * (1:365 - 80) / 365) + rnorm(365, 0, 3)
    ),
    
    distribution = rnorm(1000, 100, 15)
  )
}

#' Demonstrate Basic Plot Types
demonstrate_basic_plots <- function() {
  
  cat("BASE R BASIC PLOTS\n")
  cat("==================\n\n")
  
  # Generate sample data
  plot_data <- generate_plot_data()
  
  # Set up plotting area for multiple plots
  par(mfrow = c(2, 3), mar = c(4, 4, 3, 2))
  
  cat("1. SCATTER PLOT\n")
  cat("---------------\n")
  
  # Basic scatter plot
  plot(plot_data$continuous$x, plot_data$continuous$y,
       main = "Basic Scatter Plot",
       xlab = "X Variable", ylab = "Y Variable",
       pch = 19, col = "steelblue", cex = 0.8)
  
  # Add trend line
  abline(lm(y ~ x, data = plot_data$continuous), col = "red", lwd = 2)
  
  cat("2. LINE PLOT\n")
  cat("------------\n")
  
  # Time series line plot
  plot(plot_data$time_series$date, plot_data$time_series$sales,
       type = "l", main = "Time Series Line Plot",
       xlab = "Date", ylab = "Sales",
       col = "darkgreen", lwd = 2)
  
  cat("3. BAR PLOT\n")
  cat("-----------\n")
  
  # Bar plot
  category_counts <- table(plot_data$categorical$category)
  barplot(category_counts,
          main = "Bar Plot - Category Counts",
          xlab = "Category", ylab = "Count",
          col = rainbow(length(category_counts)),
          las = 2)  # Rotate labels
  
  cat("4. HISTOGRAM\n")
  cat("------------\n")
  
  # Histogram
  hist(plot_data$distribution,
       main = "Histogram - Distribution",
       xlab = "Value", ylab = "Frequency",
       col = "lightblue", border = "white",
       breaks = 30)
  
  cat("5. BOX PLOT\n")
  cat("-----------\n")
  
  # Box plot
  boxplot(values ~ category, data = plot_data$categorical,
          main = "Box Plot by Category",
          xlab = "Category", ylab = "Values",
          col = heat.colors(5),
          las = 2)
  
  cat("6. PIE CHART\n")
  cat("------------\n")
  
  # Pie chart
  pie(category_counts,
      main = "Pie Chart - Category Distribution",
      col = rainbow(length(category_counts)))
  
  # Reset plotting parameters
  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))
  
  cat("\nBasic plots demonstration complete!\n")
}

#' ========================================
#' 2. ADVANCED PLOT CUSTOMIZATION
#' ========================================

#' Demonstrate Advanced Plot Customization
demonstrate_advanced_customization <- function() {
  
  cat("ADVANCED PLOT CUSTOMIZATION\n")
  cat("===========================\n\n")
  
  plot_data <- generate_plot_data()
  
  cat("1. CUSTOMIZED SCATTER PLOT\n")
  cat("---------------------------\n")
  
  # Advanced scatter plot with customization
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))
  
  # Color by group
  colors <- c("red", "blue", "green")
  group_colors <- colors[as.numeric(as.factor(plot_data$continuous$group))]
  
  plot(plot_data$continuous$x, plot_data$continuous$y,
       main = "Customized Scatter Plot",
       xlab = "X Variable", ylab = "Y Variable",
       pch = 21,  # Filled circles
       bg = group_colors,  # Fill color
       col = "black",      # Border color
       cex = 1.2,         # Point size
       lwd = 1.5)         # Line width
  
  # Add legend
  legend("topright", 
         legend = c("Group A", "Group B", "Group C"),
         fill = colors,
         bty = "n")  # No box around legend
  
  # Add grid
  grid(col = "gray90", lty = 2)
  
  cat("2. ENHANCED LINE PLOT\n")
  cat("---------------------\n")
  
  # Multiple line plot
  plot(plot_data$time_series$date, plot_data$time_series$sales,
       type = "l", main = "Multi-line Plot",
       xlab = "Date", ylab = "Value",
       col = "blue", lwd = 2, ylim = c(-20, 1400))
  
  # Add second line (scaled temperature)
  lines(plot_data$time_series$date, plot_data$time_series$temperature * 20,
        col = "red", lwd = 2, lty = 2)
  
  # Add legend
  legend("topright",
         legend = c("Sales", "Temperature (x20)"),
         col = c("blue", "red"),
         lty = c(1, 2),
         lwd = 2,
         bty = "n")
  
  cat("3. ENHANCED BAR PLOT\n")
  cat("--------------------\n")
  
  # Grouped bar plot
  category_region <- table(plot_data$categorical$category, plot_data$categorical$region)
  
  barplot(category_region,
          main = "Grouped Bar Plot",
          xlab = "Region", ylab = "Count",
          col = rainbow(nrow(category_region)),
          legend.text = TRUE,
          args.legend = list(x = "topright", bty = "n"),
          beside = TRUE)  # Side-by-side bars
  
  cat("4. SOPHISTICATED HISTOGRAM\n")
  cat("---------------------------\n")
  
  # Enhanced histogram with density overlay
  hist(plot_data$distribution,
       main = "Enhanced Histogram with Density",
       xlab = "Value", ylab = "Density",
       col = "lightblue", border = "white",
       breaks = 30, freq = FALSE)
  
  # Add density curve
  lines(density(plot_data$distribution), col = "red", lwd = 3)
  
  # Add normal distribution overlay
  x_seq <- seq(min(plot_data$distribution), max(plot_data$distribution), length.out = 100)
  normal_curve <- dnorm(x_seq, mean = mean(plot_data$distribution), sd = sd(plot_data$distribution))
  lines(x_seq, normal_curve, col = "blue", lwd = 2, lty = 2)
  
  legend("topright",
         legend = c("Observed Density", "Normal Distribution"),
         col = c("red", "blue"),
         lty = c(1, 2),
         lwd = c(3, 2),
         bty = "n")
  
  # Reset plotting parameters
  par(mfrow = c(1, 1), mar = c(5, 4, 4, 2))
  
  cat("\nAdvanced customization demonstration complete!\n")
}

#' ========================================
#' 3. SPECIALIZED PLOT TYPES
#' ========================================

#' Create Correlation Matrix Plot
create_correlation_plot <- function(data) {
  
  cat("CORRELATION MATRIX PLOT\n")
  cat("=======================\n")
  
  # Select numeric columns
  numeric_data <- data[sapply(data, is.numeric)]
  
  if (ncol(numeric_data) < 2) {
    stop("Need at least 2 numeric columns for correlation plot")
  }
  
  # Calculate correlation matrix
  cor_matrix <- cor(numeric_data, use = "complete.obs")
  
  # Create color palette
  colors <- colorRampPalette(c("blue", "white", "red"))(100)
  
  # Create the plot
  par(mar = c(2, 2, 2, 2))
  image(1:ncol(cor_matrix), 1:nrow(cor_matrix), 
        as.matrix(cor_matrix),
        col = colors,
        xlab = "", ylab = "",
        main = "Correlation Matrix Heatmap",
        axes = FALSE)
  
  # Add text annotations
  for (i in 1:nrow(cor_matrix)) {
    for (j in 1:ncol(cor_matrix)) {
      text(j, i, round(cor_matrix[i, j], 2), 
           col = ifelse(abs(cor_matrix[i, j]) > 0.5, "white", "black"))
    }
  }
  
  # Add axes
  axis(1, at = 1:ncol(cor_matrix), labels = colnames(cor_matrix), las = 2)
  axis(2, at = 1:nrow(cor_matrix), labels = rownames(cor_matrix), las = 2)
  
  # Add color legend
  legend("right", 
         legend = c("1", "0.5", "0", "-0.5", "-1"),
         fill = colors[c(100, 75, 50, 25, 1)],
         title = "Correlation",
         xpd = TRUE)
  
  par(mar = c(5, 4, 4, 2))
  
  return(cor_matrix)
}

#' Create Density Plot with Multiple Groups
create_density_plot <- function(data, variable, group_var) {
  
  cat("MULTI-GROUP DENSITY PLOT\n")
  cat("========================\n")
  
  # Get unique groups
  groups <- unique(data[[group_var]])
  colors <- rainbow(length(groups))
  
  # Calculate density for each group
  densities <- lapply(groups, function(g) {
    subset_data <- data[data[[group_var]] == g, variable]
    density(subset_data, na.rm = TRUE)
  })
  
  # Find plot limits
  x_range <- range(sapply(densities, function(d) range(d$x)))
  y_range <- range(sapply(densities, function(d) range(d$y)))
  
  # Create base plot
  plot(NULL, xlim = x_range, ylim = y_range,
       xlab = variable, ylab = "Density",
       main = paste("Density Plot by", group_var))
  
  # Add density curves
  for (i in seq_along(densities)) {
    lines(densities[[i]], col = colors[i], lwd = 2)
    polygon(densities[[i]], col = adjustcolor(colors[i], alpha.f = 0.3), border = NA)
  }
  
  # Add legend
  legend("topright",
         legend = groups,
         col = colors,
         lwd = 2,
         fill = adjustcolor(colors, alpha.f = 0.3),
         bty = "n")
  
  # Add grid
  grid(col = "gray90", lty = 2)
}

#' Create Q-Q Plot for Normality Assessment
create_qq_plot <- function(data, variable) {
  
  cat("Q-Q PLOT FOR NORMALITY\n")
  cat("======================\n")
  
  values <- data[[variable]]
  values <- values[!is.na(values)]
  
  # Create Q-Q plot
  qqnorm(values, main = paste("Q-Q Plot:", variable),
         pch = 19, col = "steelblue", cex = 0.8)
  qqline(values, col = "red", lwd = 2)
  
  # Add grid
  grid(col = "gray90", lty = 2)
  
  # Calculate and display Shapiro-Wilk test
  if (length(values) <= 5000) {  # Shapiro test limit
    shapiro_test <- shapiro.test(values)
    text(x = par("usr")[1] + 0.05 * diff(par("usr")[1:2]),
         y = par("usr")[4] - 0.05 * diff(par("usr")[3:4]),
         paste("Shapiro-Wilk p-value:", round(shapiro_test$p.value, 4)),
         adj = 0, cex = 0.9)
  }
}

#' ========================================
#' 4. MULTI-PANEL LAYOUTS
#' ========================================

#' Create Comprehensive Data Overview
create_data_overview <- function(data) {
  
  cat("COMPREHENSIVE DATA OVERVIEW\n")
  cat("===========================\n")
  
  # Set up multi-panel layout
  layout(matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE))
  par(mar = c(4, 4, 3, 2))
  
  # Find numeric and categorical variables
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  categorical_vars <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
  
  # Panel 1: Distribution of first numeric variable
  if (length(numeric_vars) >= 1) {
    hist(data[[numeric_vars[1]]], 
         main = paste("Distribution:", numeric_vars[1]),
         col = "lightblue", border = "white",
         xlab = numeric_vars[1])
  }
  
  # Panel 2: Box plot of first numeric by first categorical
  if (length(numeric_vars) >= 1 && length(categorical_vars) >= 1) {
    boxplot(data[[numeric_vars[1]]] ~ data[[categorical_vars[1]]],
            main = paste(numeric_vars[1], "by", categorical_vars[1]),
            col = rainbow(length(unique(data[[categorical_vars[1]]]))),
            xlab = categorical_vars[1],
            ylab = numeric_vars[1],
            las = 2)
  }
  
  # Panel 3: Scatter plot of first two numeric variables
  if (length(numeric_vars) >= 2) {
    plot(data[[numeric_vars[1]]], data[[numeric_vars[2]]],
         main = paste(numeric_vars[1], "vs", numeric_vars[2]),
         xlab = numeric_vars[1], ylab = numeric_vars[2],
         pch = 19, col = "steelblue", cex = 0.8)
    abline(lm(data[[numeric_vars[2]]] ~ data[[numeric_vars[1]]]), 
           col = "red", lwd = 2)
  }
  
  # Panel 4: Bar plot of first categorical variable
  if (length(categorical_vars) >= 1) {
    category_counts <- table(data[[categorical_vars[1]]])
    barplot(category_counts,
            main = paste("Counts:", categorical_vars[1]),
            col = rainbow(length(category_counts)),
            las = 2)
  }
  
  # Panel 5: Q-Q plot of first numeric variable
  if (length(numeric_vars) >= 1) {
    qqnorm(data[[numeric_vars[1]]], 
           main = paste("Q-Q Plot:", numeric_vars[1]),
           pch = 19, col = "steelblue", cex = 0.8)
    qqline(data[[numeric_vars[1]]], col = "red", lwd = 2)
  }
  
  # Panel 6: Summary statistics text
  plot.new()
  text(0.5, 0.9, "Summary Statistics", cex = 1.5, font = 2, adj = 0.5)
  
  # Basic summary
  summary_text <- capture.output({
    cat("Observations:", nrow(data), "\n")
    cat("Variables:", ncol(data), "\n")
    cat("Numeric:", length(numeric_vars), "\n")
    cat("Categorical:", length(categorical_vars), "\n")
    if (length(numeric_vars) > 0) {
      cat("Missing values:", sum(is.na(data[numeric_vars])), "\n")
    }
  })
  
  for (i in seq_along(summary_text)) {
    text(0.1, 0.8 - (i-1) * 0.1, summary_text[i], adj = 0, cex = 1)
  }
  
  # Reset layout
  layout(1)
  par(mar = c(5, 4, 4, 2))
  
  cat("\nData overview complete!\n")
}

#' ========================================
#' 5. PLOT SAVING AND EXPORT
#' ========================================

#' Save Plot to File
#' @param plot_function Function that creates the plot
#' @param filename Output filename
#' @param width Plot width in inches
#' @param height Plot height in inches
#' @param resolution Plot resolution in DPI
save_plot <- function(plot_function, filename, width = 8, height = 6, resolution = 300) {
  
  cat("SAVING PLOT TO FILE\n")
  cat("===================\n")
  
  # Determine file type from extension
  file_ext <- tools::file_ext(filename)
  
  if (file_ext == "png") {
    png(filename, width = width * resolution, height = height * resolution, res = resolution)
  } else if (file_ext == "pdf") {
    pdf(filename, width = width, height = height)
  } else if (file_ext == "jpg" || file_ext == "jpeg") {
    jpeg(filename, width = width * resolution, height = height * resolution, res = resolution)
  } else {
    stop("Unsupported file format. Use png, pdf, or jpg.")
  }
  
  # Execute the plot function
  plot_function()
  
  # Close the device
  dev.off()
  
  cat("Plot saved to:", filename, "\n")
}

#' ========================================
#' 6. DEMONSTRATION FUNCTIONS
#' ========================================

#' Run Base R Plotting Demonstrations
run_base_plots_demo <- function() {
  
  cat("===========================================\n")
  cat("BASE R PLOTTING SYSTEM DEMONSTRATION\n")
  cat("===========================================\n\n")
  
  # Generate sample data
  plot_data <- generate_plot_data()
  
  # Basic plots demonstration
  demonstrate_basic_plots()
  
  cat("\n" , rep("=", 50), "\n")
  
  # Advanced customization
  demonstrate_advanced_customization()
  
  cat("\n" , rep("=", 50), "\n")
  
  # Specialized plots
  cat("SPECIALIZED PLOTS\n")
  cat("=================\n\n")
  
  # Correlation plot
  cor_matrix <- create_correlation_plot(plot_data$continuous)
  
  cat("\n")
  
  # Density plot by group
  create_density_plot(plot_data$continuous, "y", "group")
  
  cat("\n")
  
  # Q-Q plot
  create_qq_plot(plot_data$continuous, "y")
  
  cat("\n" , rep("=", 50), "\n")
  
  # Data overview
  create_data_overview(plot_data$continuous)
  
  cat("\n===========================================\n")
  cat("BASE R PLOTTING DEMONSTRATION COMPLETE!\n")
  cat("===========================================\n")
}

#' ========================================
#' 7. UTILITY FUNCTIONS
#' ========================================

#' Set Standard Plot Parameters
set_plot_defaults <- function() {
  par(
    mfrow = c(1, 1),        # Single plot
    mar = c(5, 4, 4, 2),    # Margins
    oma = c(0, 0, 0, 0),    # Outer margins
    mgp = c(3, 1, 0),       # Axis label positions
    las = 0,                # Label orientation
    bty = "o",              # Box type
    xaxs = "r",             # Axis style
    yaxs = "r",
    font.main = 2,          # Title font
    font.lab = 1,           # Label font
    font.axis = 1,          # Axis font
    cex.main = 1.2,         # Title size
    cex.lab = 1,            # Label size
    cex.axis = 0.9          # Axis size
  )
}

#' Get Color Palettes
get_color_palettes <- function() {
  list(
    default = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
                "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"),
    
    qualitative = RColorBrewer::brewer.pal(8, "Set2"),
    
    sequential_blue = RColorBrewer::brewer.pal(9, "Blues"),
    
    diverging = RColorBrewer::brewer.pal(11, "RdBu"),
    
    viridis = viridis::viridis(10),
    
    custom_corporate = c("#003f5c", "#2f4b7c", "#665191", "#a05195", 
                        "#d45087", "#f95d6a", "#ff7c43", "#ffa600")
  )
}

# Run demonstration if script is executed directly
if (sys.nframe() == 0) {
  run_base_plots_demo()
}