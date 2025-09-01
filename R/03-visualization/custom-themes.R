# @title Custom ggplot2 Themes and Styling
# @description Professional themes, color palettes, and styling functions for ggplot2
# @author R Data Science Portfolio
# @date 2025

#' ========================================
#' CUSTOM GGPLOT2 THEMES AND STYLING
#' ========================================

# Comprehensive collection of custom themes, color palettes, and styling functions

# Required libraries
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(viridis)
library(extrafont)

#' ========================================
#' 1. CUSTOM THEMES
#' ========================================

#' Corporate Theme
#' @description Clean, professional theme suitable for business presentations
#' @param base_size Base font size
#' @param base_family Font family
#' @return ggplot2 theme object
theme_corporate <- function(base_size = 12, base_family = "Arial") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # Text elements
      plot.title = element_text(
        size = base_size * 1.4,
        face = "bold",
        color = "#2c3e50",
        margin = margin(b = 20)
      ),
      plot.subtitle = element_text(
        size = base_size * 1.1,
        color = "#7f8c8d",
        margin = margin(b = 15)
      ),
      plot.caption = element_text(
        size = base_size * 0.8,
        color = "#95a5a6",
        hjust = 0,
        margin = margin(t = 15)
      ),
      
      # Axis elements
      axis.title = element_text(
        size = base_size * 1.1,
        color = "#2c3e50",
        face = "bold"
      ),
      axis.text = element_text(
        size = base_size * 0.9,
        color = "#34495e"
      ),
      axis.ticks = element_line(color = "#bdc3c7", size = 0.5),
      axis.line = element_line(color = "#bdc3c7", size = 0.5),
      
      # Panel elements
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "#ecf0f1", size = 0.5, linetype = "dotted"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "#bdc3c7", fill = NA, size = 0.5),
      
      # Legend elements
      legend.position = "bottom",
      legend.title = element_text(
        size = base_size,
        color = "#2c3e50",
        face = "bold"
      ),
      legend.text = element_text(
        size = base_size * 0.9,
        color = "#34495e"
      ),
      legend.key = element_rect(fill = "white", color = NA),
      legend.spacing.x = unit(0.5, "cm"),
      
      # Strip elements (for faceting)
      strip.background = element_rect(fill = "#ecf0f1", color = "#bdc3c7"),
      strip.text = element_text(
        size = base_size,
        color = "#2c3e50",
        face = "bold",
        margin = margin(t = 5, b = 5)
      ),
      
      # Plot background
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    )
}

#' Academic Theme
#' @description Clean, minimalist theme for academic publications
#' @param base_size Base font size
#' @param base_family Font family
#' @return ggplot2 theme object
theme_academic <- function(base_size = 11, base_family = "Times") {
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      # Text elements
      plot.title = element_text(
        size = base_size * 1.3,
        face = "bold",
        hjust = 0.5,
        margin = margin(b = 15)
      ),
      plot.subtitle = element_text(
        size = base_size * 1.1,
        hjust = 0.5,
        margin = margin(b = 10)
      ),
      plot.caption = element_text(
        size = base_size * 0.8,
        hjust = 0,
        margin = margin(t = 10)
      ),
      
      # Axis elements
      axis.title = element_text(size = base_size),
      axis.text = element_text(size = base_size * 0.9),
      axis.ticks = element_line(color = "black", size = 0.3),
      
      # Panel elements
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(color = "grey90", size = 0.3),
      panel.grid.minor = element_line(color = "grey95", size = 0.2),
      panel.border = element_rect(color = "black", fill = NA, size = 0.5),
      
      # Legend elements
      legend.background = element_rect(fill = "white", color = "black", size = 0.3),
      legend.key = element_rect(fill = "white", color = NA),
      legend.title = element_text(size = base_size, face = "bold"),
      legend.text = element_text(size = base_size * 0.9),
      
      # Strip elements
      strip.background = element_rect(fill = "grey90", color = "black"),
      strip.text = element_text(size = base_size, face = "bold"),
      
      # Plot elements
      plot.background = element_rect(fill = "white"),
      plot.margin = margin(t = 15, r = 15, b = 15, l = 15)
    )
}

#' Dark Theme
#' @description Modern dark theme for presentations and dashboards
#' @param base_size Base font size
#' @param base_family Font family
#' @return ggplot2 theme object
theme_dark_modern <- function(base_size = 12, base_family = "Arial") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # Text elements
      plot.title = element_text(
        size = base_size * 1.4,
        face = "bold",
        color = "#ffffff",
        margin = margin(b = 20)
      ),
      plot.subtitle = element_text(
        size = base_size * 1.1,
        color = "#cccccc",
        margin = margin(b = 15)
      ),
      plot.caption = element_text(
        size = base_size * 0.8,
        color = "#999999",
        hjust = 0,
        margin = margin(t = 15)
      ),
      
      # Axis elements
      axis.title = element_text(
        size = base_size * 1.1,
        color = "#ffffff",
        face = "bold"
      ),
      axis.text = element_text(
        size = base_size * 0.9,
        color = "#cccccc"
      ),
      axis.ticks = element_line(color = "#555555", size = 0.5),
      axis.line = element_line(color = "#555555", size = 0.5),
      
      # Panel elements
      panel.background = element_rect(fill = "#1e1e1e", color = NA),
      panel.grid.major = element_line(color = "#333333", size = 0.5, linetype = "dotted"),
      panel.grid.minor = element_blank(),
      
      # Legend elements
      legend.background = element_rect(fill = "#1e1e1e", color = NA),
      legend.key = element_rect(fill = "#1e1e1e", color = NA),
      legend.title = element_text(
        size = base_size,
        color = "#ffffff",
        face = "bold"
      ),
      legend.text = element_text(
        size = base_size * 0.9,
        color = "#cccccc"
      ),
      
      # Strip elements
      strip.background = element_rect(fill = "#333333", color = "#555555"),
      strip.text = element_text(
        size = base_size,
        color = "#ffffff",
        face = "bold"
      ),
      
      # Plot background
      plot.background = element_rect(fill = "#1e1e1e", color = NA),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    )
}

#' Minimal Clean Theme
#' @description Ultra-clean theme with minimal visual elements
#' @param base_size Base font size
#' @param base_family Font family
#' @return ggplot2 theme object
theme_minimal_clean <- function(base_size = 12, base_family = "Helvetica") {
  theme_void(base_size = base_size, base_family = base_family) +
    theme(
      # Text elements
      plot.title = element_text(
        size = base_size * 1.5,
        face = "bold",
        color = "#2c3e50",
        hjust = 0.5,
        margin = margin(b = 30)
      ),
      plot.subtitle = element_text(
        size = base_size * 1.1,
        color = "#7f8c8d",
        hjust = 0.5,
        margin = margin(b = 20)
      ),
      
      # Axis elements
      axis.title.x = element_text(
        size = base_size,
        color = "#2c3e50",
        margin = margin(t = 10)
      ),
      axis.title.y = element_text(
        size = base_size,
        color = "#2c3e50",
        angle = 90,
        margin = margin(r = 10)
      ),
      axis.text = element_text(
        size = base_size * 0.9,
        color = "#34495e"
      ),
      
      # Panel elements
      panel.background = element_rect(fill = "white", color = NA),
      
      # Legend elements
      legend.position = "bottom",
      legend.title = element_text(
        size = base_size,
        color = "#2c3e50",
        face = "bold"
      ),
      legend.text = element_text(
        size = base_size * 0.9,
        color = "#34495e"
      ),
      
      # Plot elements
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
    )
}

#' ========================================
#' 2. COLOR PALETTES
#' ========================================

#' Corporate Color Palette
#' @description Professional color palette for business visualizations
#' @param n Number of colors to return
#' @return Vector of hex color codes
corporate_colors <- function(n = NULL) {
  colors <- c(
    "#2c3e50", "#3498db", "#e74c3c", "#f39c12", 
    "#27ae60", "#9b59b6", "#34495e", "#16a085",
    "#e67e22", "#95a5a6", "#d35400", "#8e44ad"
  )
  
  if (is.null(n)) {
    return(colors)
  } else if (n <= length(colors)) {
    return(colors[1:n])
  } else {
    return(colorRampPalette(colors)(n))
  }
}

#' Vibrant Color Palette
#' @description Bright, vibrant colors for engaging visualizations
#' @param n Number of colors to return
#' @return Vector of hex color codes
vibrant_colors <- function(n = NULL) {
  colors <- c(
    "#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", 
    "#FECA57", "#FF9FF3", "#54A0FF", "#5F27CD",
    "#00D2D3", "#FF9F43", "#EE5A24", "#0ABDE3"
  )
  
  if (is.null(n)) {
    return(colors)
  } else if (n <= length(colors)) {
    return(colors[1:n])
  } else {
    return(colorRampPalette(colors)(n))
  }
}

#' Pastel Color Palette
#' @description Soft, pastel colors for subtle visualizations
#' @param n Number of colors to return
#' @return Vector of hex color codes
pastel_colors <- function(n = NULL) {
  colors <- c(
    "#FFB3BA", "#FFDFBA", "#FFFFBA", "#BAFFC9", 
    "#BAE1FF", "#FFBAF3", "#D4BAFF", "#FFD1DC",
    "#E6E6FA", "#F0FFFF", "#FFF8DC", "#F5DEB3"
  )
  
  if (is.null(n)) {
    return(colors)
  } else if (n <= length(colors)) {
    return(colors[1:n])
  } else {
    return(colorRampPalette(colors)(n))
  }
}

#' Monochromatic Blue Palette
#' @description Various shades of blue for cohesive visualizations
#' @param n Number of colors to return
#' @return Vector of hex color codes
blue_monochrome <- function(n = NULL) {
  colors <- c(
    "#08306B", "#2171B5", "#4292C6", "#6BAED6", 
    "#9ECAE1", "#C6DBEF", "#DEEBF7", "#F7FBFF"
  )
  
  if (is.null(n)) {
    return(colors)
  } else if (n <= length(colors)) {
    return(colors[1:n])
  } else {
    return(colorRampPalette(colors)(n))
  }
}

#' ========================================
#' 3. SCALE FUNCTIONS
#' ========================================

#' Corporate Color Scale (Discrete)
scale_color_corporate <- function(...) {
  scale_color_manual(values = corporate_colors(), ...)
}

#' Corporate Fill Scale (Discrete)
scale_fill_corporate <- function(...) {
  scale_fill_manual(values = corporate_colors(), ...)
}

#' Vibrant Color Scale (Discrete)
scale_color_vibrant <- function(...) {
  scale_color_manual(values = vibrant_colors(), ...)
}

#' Vibrant Fill Scale (Discrete)
scale_fill_vibrant <- function(...) {
  scale_fill_manual(values = vibrant_colors(), ...)
}

#' Pastel Color Scale (Discrete)
scale_color_pastel <- function(...) {
  scale_color_manual(values = pastel_colors(), ...)
}

#' Pastel Fill Scale (Discrete)
scale_fill_pastel <- function(...) {
  scale_fill_manual(values = pastel_colors(), ...)
}

#' Blue Monochrome Color Scale (Continuous)
scale_color_blue_continuous <- function(...) {
  scale_color_gradientn(colors = blue_monochrome(), ...)
}

#' Blue Monochrome Fill Scale (Continuous)
scale_fill_blue_continuous <- function(...) {
  scale_fill_gradientn(colors = blue_monochrome(), ...)
}

#' ========================================
#' 4. UTILITY FUNCTIONS
#' ========================================

#' Apply Corporate Styling to ggplot
#' @param plot ggplot object
#' @param palette Color palette to use
#' @return Styled ggplot object
apply_corporate_style <- function(plot, palette = "corporate") {
  
  # Select color scale based on palette
  color_scale <- switch(palette,
    "corporate" = scale_color_corporate(),
    "vibrant" = scale_color_vibrant(),
    "pastel" = scale_color_pastel(),
    scale_color_corporate()  # default
  )
  
  fill_scale <- switch(palette,
    "corporate" = scale_fill_corporate(),
    "vibrant" = scale_fill_vibrant(),
    "pastel" = scale_fill_pastel(),
    scale_fill_corporate()  # default
  )
  
  plot +
    theme_corporate() +
    color_scale +
    fill_scale
}

#' Create Color Palette Preview
#' @param palette_function Function that returns color palette
#' @param palette_name Name of the palette
#' @param n Number of colors to display
preview_palette <- function(palette_function, palette_name, n = 8) {
  colors <- palette_function(n)
  
  ggplot(data.frame(x = 1:n, y = 1, colors = colors), 
         aes(x = x, y = y, fill = colors)) +
    geom_col(width = 0.8) +
    scale_fill_identity() +
    theme_void() +
    labs(title = paste("Color Palette:", palette_name)) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.margin = margin(t = 20, b = 20)
    ) +
    coord_fixed(ratio = 0.8)
}

#' ========================================
#' 5. PLOT ENHANCERS
#' ========================================

#' Add Corporate Logo Watermark
#' @param plot ggplot object
#' @param logo_path Path to logo image
#' @param position Position of logo (e.g., "bottom.right")
#' @param size Size of logo
add_logo_watermark <- function(plot, logo_path, position = "bottom.right", size = 0.1) {
  if (file.exists(logo_path)) {
    logo <- png::readPNG(logo_path)
    plot +
      annotation_raster(logo, 
                       xmin = -Inf, xmax = Inf, 
                       ymin = -Inf, ymax = Inf,
                       alpha = 0.1)
  } else {
    warning("Logo file not found")
    plot
  }
}

#' Add Professional Caption
#' @param plot ggplot object
#' @param source Data source
#' @param date Date of analysis
#' @param author Author name
add_professional_caption <- function(plot, source = NULL, date = NULL, author = NULL) {
  
  caption_parts <- c()
  
  if (!is.null(source)) {
    caption_parts <- c(caption_parts, paste("Source:", source))
  }
  
  if (!is.null(date)) {
    caption_parts <- c(caption_parts, paste("Date:", date))
  }
  
  if (!is.null(author)) {
    caption_parts <- c(caption_parts, paste("Analysis:", author))
  }
  
  caption_text <- paste(caption_parts, collapse = " | ")
  
  plot +
    labs(caption = caption_text)
}

#' Format Numbers for Labels
#' @param x Numeric vector
#' @param type Type of formatting ("comma", "percent", "currency", "scientific")
#' @param digits Number of decimal places
format_numbers <- function(x, type = "comma", digits = 1) {
  switch(type,
    "comma" = scales::comma(x, accuracy = 10^(-digits)),
    "percent" = scales::percent(x, accuracy = 10^(-digits)),
    "currency" = scales::dollar(x, accuracy = 10^(-digits)),
    "scientific" = scales::scientific(x, digits = digits),
    scales::comma(x, accuracy = 10^(-digits))  # default
  )
}

#' ========================================
#' 6. THEME COMBINATIONS
#' ========================================

#' Get Complete Theme Package
#' @param style Theme style ("corporate", "academic", "dark", "minimal")
#' @param palette Color palette to use
#' @return List containing theme and color scales
get_theme_package <- function(style = "corporate", palette = "corporate") {
  
  # Select theme
  theme_obj <- switch(style,
    "corporate" = theme_corporate(),
    "academic" = theme_academic(),
    "dark" = theme_dark_modern(),
    "minimal" = theme_minimal_clean(),
    theme_corporate()  # default
  )
  
  # Select color scales
  color_scale <- switch(palette,
    "corporate" = scale_color_corporate(),
    "vibrant" = scale_color_vibrant(),
    "pastel" = scale_color_pastel(),
    scale_color_corporate()  # default
  )
  
  fill_scale <- switch(palette,
    "corporate" = scale_fill_corporate(),
    "vibrant" = scale_fill_vibrant(),
    "pastel" = scale_fill_pastel(),
    scale_fill_corporate()  # default
  )
  
  list(
    theme = theme_obj,
    color_scale = color_scale,
    fill_scale = fill_scale
  )
}

#' ========================================
#' 7. DEMONSTRATION FUNCTIONS
#' ========================================

#' Demonstrate All Themes
demonstrate_themes <- function() {
  
  cat("CUSTOM THEMES DEMONSTRATION\n")
  cat("===========================\n\n")
  
  # Sample data
  df <- data.frame(
    x = 1:10,
    y = rnorm(10, 5, 2),
    category = rep(c("A", "B"), 5)
  )
  
  # Base plot
  base_plot <- ggplot(df, aes(x = x, y = y, color = category)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      title = "Theme Demonstration",
      subtitle = "Comparing different custom themes",
      x = "X Variable",
      y = "Y Variable",
      color = "Category"
    )
  
  # Corporate theme
  p1 <- base_plot + theme_corporate() + scale_color_corporate() +
    labs(title = "Corporate Theme")
  
  # Academic theme
  p2 <- base_plot + theme_academic() + scale_color_manual(values = c("#000000", "#666666")) +
    labs(title = "Academic Theme")
  
  # Dark theme
  p3 <- base_plot + theme_dark_modern() + scale_color_vibrant() +
    labs(title = "Dark Modern Theme")
  
  # Minimal theme
  p4 <- base_plot + theme_minimal_clean() + scale_color_pastel() +
    labs(title = "Minimal Clean Theme")
  
  # Display plots
  print(p1)
  print(p2)
  print(p3)
  print(p4)
  
  cat("Themes demonstration complete!\n")
}

#' Demonstrate Color Palettes
demonstrate_palettes <- function() {
  
  cat("COLOR PALETTES DEMONSTRATION\n")
  cat("============================\n\n")
  
  # Create palette previews
  p1 <- preview_palette(corporate_colors, "Corporate")
  p2 <- preview_palette(vibrant_colors, "Vibrant")
  p3 <- preview_palette(pastel_colors, "Pastel")
  p4 <- preview_palette(blue_monochrome, "Blue Monochrome")
  
  # Display previews
  print(p1)
  print(p2)
  print(p3)
  print(p4)
  
  cat("Color palettes demonstration complete!\n")
}

#' Run Complete Themes Demo
run_themes_demo <- function() {
  
  cat("===========================================\n")
  cat("CUSTOM THEMES AND STYLING DEMONSTRATION\n")
  cat("===========================================\n\n")
  
  # Demonstrate themes
  demonstrate_themes()
  
  cat("\n", rep("=", 50), "\n")
  
  # Demonstrate palettes
  demonstrate_palettes()
  
  cat("\n===========================================\n")
  cat("CUSTOM THEMES DEMONSTRATION COMPLETE!\n")
  cat("===========================================\n")
}

# Example usage function
example_styled_plots <- function() {
  
  # Sample data
  sample_data <- data.frame(
    x = rep(1:5, 3),
    y = c(rnorm(5, 10, 2), rnorm(5, 15, 2), rnorm(5, 12, 2)),
    group = rep(c("Group A", "Group B", "Group C"), each = 5),
    size = abs(rnorm(15, 100, 30))
  )
  
  # Example 1: Corporate styled scatter plot
  plot1 <- ggplot(sample_data, aes(x = x, y = y, color = group, size = size)) +
    geom_point(alpha = 0.7) +
    labs(
      title = "Corporate Styled Visualization",
      subtitle = "Professional appearance with corporate colors",
      x = "X Variable",
      y = "Y Variable",
      color = "Group",
      size = "Size"
    ) +
    theme_corporate() +
    scale_color_corporate() +
    scale_size_continuous(range = c(2, 8)) +
    guides(
      color = guide_legend(override.aes = list(size = 4)),
      size = guide_legend(override.aes = list(color = "black"))
    )
  
  # Example 2: Dark theme with vibrant colors
  plot2 <- ggplot(sample_data, aes(x = factor(x), y = y, fill = group)) +
    geom_boxplot(alpha = 0.8) +
    labs(
      title = "Dark Theme Visualization",
      subtitle = "Modern dark appearance with vibrant colors",
      x = "Category",
      y = "Value",
      fill = "Group"
    ) +
    theme_dark_modern() +
    scale_fill_vibrant()
  
  return(list(corporate = plot1, dark = plot2))
}

# Run demonstration if script is executed directly
if (sys.nframe() == 0) {
  run_themes_demo()
}