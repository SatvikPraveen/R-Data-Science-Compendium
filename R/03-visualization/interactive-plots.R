# @title Interactive Plotting and Visualization Framework
# @description Advanced interactive visualizations using plotly, DT, and other packages
# @author R Data Science Portfolio Developer
# @date 2025

#' ========================================
#' INTERACTIVE PLOTTING FRAMEWORK
#' ========================================

# Load required libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr, ggplot2, plotly, DT, crosstalk, htmlwidgets,
  leaflet, visNetwork, networkD3, highcharter, 
  ggiraph, echarts4r, reactable, flexdashboard,
  shiny, shinydashboard, htmltools, viridis,
  scales, tidyr, tibble
)

#' ========================================
#' 1. INTERACTIVE SCATTER PLOTS
#' ========================================

#' Create Interactive Scatter Plot with Plotly
#' @param data Dataset for plotting
#' @param x_var X-axis variable name
#' @param y_var Y-axis variable name
#' @param color_var Variable for color mapping (optional)
#' @param size_var Variable for size mapping (optional)
#' @param title Plot title
#' @return Interactive plotly object
create_interactive_scatter <- function(data, x_var, y_var, color_var = NULL, 
                                     size_var = NULL, title = "Interactive Scatter Plot") {
  
  # Create base plot
  p <- ggplot(data, aes_string(x = x_var, y = y_var))
  
  # Add color mapping if specified
  if (!is.null(color_var)) {
    p <- p + aes_string(color = color_var)
  }
  
  # Add size mapping if specified
  if (!is.null(size_var)) {
    p <- p + aes_string(size = size_var)
  }
  
  # Add points and styling
  p <- p + 
    geom_point(alpha = 0.7) +
    labs(title = title, x = tools::toTitleCase(gsub("_", " ", x_var)), 
         y = tools::toTitleCase(gsub("_", " ", y_var))) +
    theme_minimal()
  
  # Convert to interactive plotly
  interactive_plot <- ggplotly(p, tooltip = c("x", "y", "colour", "size"))
  
  # Customize layout
  interactive_plot <- interactive_plot %>%
    layout(
      title = list(text = title, font = list(size = 16)),
      xaxis = list(title = tools::toTitleCase(gsub("_", " ", x_var))),
      yaxis = list(title = tools::toTitleCase(gsub("_", " ", y_var))),
      hovermode = "closest"
    )
  
  return(interactive_plot)
}

#' Create 3D Scatter Plot
#' @param data Dataset for plotting
#' @param x_var X-axis variable
#' @param y_var Y-axis variable  
#' @param z_var Z-axis variable
#' @param color_var Color variable (optional)
#' @param title Plot title
#' @return Interactive 3D plotly object
create_3d_scatter <- function(data, x_var, y_var, z_var, color_var = NULL, 
                             title = "3D Interactive Scatter Plot") {
  
  # Prepare data
  plot_data <- data[, c(x_var, y_var, z_var)]
  
  if (!is.null(color_var)) {
    plot_data$color <- data[[color_var]]
  }
  
  # Create 3D scatter plot
  p <- plot_ly(data = plot_data, 
               x = ~get(x_var), 
               y = ~get(y_var), 
               z = ~get(z_var),
               type = "scatter3d",
               mode = "markers",
               marker = list(size = 5, opacity = 0.7))
  
  # Add color if specified
  if (!is.null(color_var)) {
    p <- p %>% add_trace(color = ~color)
  }
  
  # Customize layout
  p <- p %>%
    layout(
      title = title,
      scene = list(
        xaxis = list(title = tools::toTitleCase(gsub("_", " ", x_var))),
        yaxis = list(title = tools::toTitleCase(gsub("_", " ", y_var))),
        zaxis = list(title = tools::toTitleCase(gsub("_", " ", z_var)))
      )
    )
  
  return(p)
}

#' ========================================
#' 2. INTERACTIVE TIME SERIES
#' ========================================

#' Create Interactive Time Series Plot
#' @param data Dataset with time series data
#' @param date_col Date column name
#' @param value_col Value column name
#' @param group_col Grouping variable (optional)
#' @param title Plot title
#' @return Interactive time series plot
create_interactive_timeseries <- function(data, date_col, value_col, group_col = NULL,
                                        title = "Interactive Time Series") {
  
  # Ensure date column is Date type
  data[[date_col]] <- as.Date(data[[date_col]])
  
  # Create base plot
  if (!is.null(group_col)) {
    p <- ggplot(data, aes_string(x = date_col, y = value_col, color = group_col)) +
      geom_line(size = 1, alpha = 0.8) +
      geom_point(size = 2, alpha = 0.6) +
      labs(color = tools::toTitleCase(gsub("_", " ", group_col)))
  } else {
    p <- ggplot(data, aes_string(x = date_col, y = value_col)) +
      geom_line(size = 1, color = "steelblue", alpha = 0.8) +
      geom_point(size = 2, color = "steelblue", alpha = 0.6)
  }
  
  p <- p +
    labs(title = title, 
         x = "Date", 
         y = tools::toTitleCase(gsub("_", " ", value_col))) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Convert to interactive plotly with range selector
  interactive_plot <- ggplotly(p) %>%
    layout(
      title = list(text = title, font = list(size = 16)),
      xaxis = list(
        rangeslider = list(type = "date"),
        rangeselector = list(
          buttons = list(
            list(count = 1, label = "1m", step = "month", stepmode = "backward"),
            list(count = 3, label = "3m", step = "month", stepmode = "backward"),
            list(count = 6, label = "6m", step = "month", stepmode = "backward"),
            list(count = 1, label = "1y", step = "year", stepmode = "backward"),
            list(step = "all")
          )
        )
      )
    )
  
  return(interactive_plot)
}

#' ========================================
#' 3. INTERACTIVE HEATMAPS
#' ========================================

#' Create Interactive Correlation Heatmap
#' @param data Numeric dataset
#' @param title Plot title
#' @return Interactive correlation heatmap
create_interactive_correlation_heatmap <- function(data, title = "Interactive Correlation Heatmap") {
  
  # Calculate correlation matrix
  cor_matrix <- cor(data, use = "complete.obs")
  
  # Create interactive heatmap
  p <- plot_ly(
    x = colnames(cor_matrix),
    y = colnames(cor_matrix),
    z = cor_matrix,
    type = "heatmap",
    colorscale = "RdBu",
    zmid = 0,
    text = round(cor_matrix, 2),
    texttemplate = "%{text}",
    textfont = list(color = "white"),
    hovertemplate = "Correlation: %{z:.3f}<br>%{x} vs %{y}<extra></extra>"
  ) %>%
    layout(
      title = title,
      xaxis = list(title = "", tickangle = 45),
      yaxis = list(title = "")
    )
  
  return(p)
}

#' Create Interactive Heatmap for Any Matrix
#' @param matrix_data Matrix or data frame
#' @param title Plot title
#' @param color_scale Color scale for heatmap
#' @return Interactive heatmap
create_interactive_heatmap <- function(matrix_data, title = "Interactive Heatmap", 
                                     color_scale = "Viridis") {
  
  # Convert to matrix if needed
  if (is.data.frame(matrix_data)) {
    row_names <- rownames(matrix_data)
    col_names <- colnames(matrix_data)
    matrix_data <- as.matrix(matrix_data)
  } else {
    row_names <- rownames(matrix_data)
    col_names <- colnames(matrix_data)
  }
  
  # Create interactive heatmap
  p <- plot_ly(
    x = col_names,
    y = row_names,
    z = matrix_data,
    type = "heatmap",
    colorscale = color_scale,
    text = round(matrix_data, 2),
    texttemplate = "%{text}",
    hovertemplate = "Value: %{z:.3f}<br>Row: %{y}<br>Col: %{x}<extra></extra>"
  ) %>%
    layout(
      title = title,
      xaxis = list(title = "", tickangle = 45),
      yaxis = list(title = "")
    )
  
  return(p)
}

#' ========================================
#' 4. INTERACTIVE DATA TABLES
#' ========================================

#' Create Enhanced Interactive Data Table
#' @param data Dataset to display
#' @param title Table title
#' @param page_length Number of rows per page
#' @param searchable Enable search functionality
#' @param downloadable Enable download buttons
#' @return Interactive DT table
create_interactive_table <- function(data, title = "Interactive Data Table", 
                                   page_length = 10, searchable = TRUE, 
                                   downloadable = TRUE) {
  
  # Prepare extensions and buttons
  extensions <- c("Buttons", "ColReorder", "Responsive")
  buttons <- list(
    list(extend = "copy", text = "Copy"),
    list(extend = "csv", filename = "data"),
    list(extend = "excel", filename = "data"),
    list(extend = "pdf", filename = "data")
  )
  
  if (!downloadable) {
    extensions <- c("ColReorder", "Responsive")
    buttons <- NULL
  }
  
  # Create DT table
  dt_table <- DT::datatable(
    data,
    caption = htmltools::tags$caption(
      style = "caption-side: top; text-align: center; font-size: 16px; font-weight: bold;",
      title
    ),
    options = list(
      pageLength = page_length,
      autoWidth = TRUE,
      scrollX = TRUE,
      search = list(regex = TRUE, caseInsensitive = TRUE),
      dom = if (downloadable) "Blfrtip" else "lfrtip",
      buttons = buttons,
      columnDefs = list(
        list(className = "dt-center", targets = "_all")
      )
    ),
    extensions = extensions,
    filter = if (searchable) "top" else "none",
    class = "cell-border stripe hover"
  )
  
  # Format numeric columns
  numeric_cols <- which(sapply(data, is.numeric))
  if (length(numeric_cols) > 0) {
    dt_table <- dt_table %>%
      DT::formatRound(columns = numeric_cols, digits = 2)
  }
  
  return(dt_table)
}

#' Create Reactable Table (Alternative to DT)
#' @param data Dataset to display
#' @param title Table title
#' @param searchable Enable search
#' @param sortable Enable sorting
#' @param pagination Enable pagination
#' @return Reactable table
create_reactable_table <- function(data, title = "Interactive Reactable", 
                                 searchable = TRUE, sortable = TRUE, 
                                 pagination = TRUE) {
  
  if (!requireNamespace("reactable", quietly = TRUE)) {
    stop("Package 'reactable' is required for this function")
  }
  
  reactable::reactable(
    data,
    searchable = searchable,
    sortable = sortable,
    pagination = pagination,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(10, 25, 50, 100),
    defaultPageSize = 10,
    highlight = TRUE,
    bordered = TRUE,
    striped = TRUE,
    theme = reactable::reactableTheme(
      borderColor = "#dfe2e5",
      stripedColor = "#f6f8fa",
      highlightColor = "#f0f5ff",
      cellPadding = "8px 12px",
      style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
      searchInputStyle = list(width = "100%")
    ),
    columns = list(
      .default = reactable::colDef(
        align = "center",
        minWidth = 100,
        headerStyle = list(background = "#f7f7f7")
      )
    )
  )
}

#' ========================================
#' 5. DASHBOARD COMPONENTS
#' ========================================

#' Create Value Box for Dashboard
#' @param value Numeric value to display
#' @param title Box title
#' @param subtitle Box subtitle
#' @param color Box color
#' @param icon Icon name
#' @return HTML value box
create_value_box <- function(value, title, subtitle = NULL, color = "primary", icon = NULL) {
  
  # Format value based on type
  if (is.numeric(value)) {
    if (value > 1000000) {
      formatted_value <- paste0("$", round(value/1000000, 1), "M")
    } else if (value > 1000) {
      formatted_value <- paste0("$", round(value/1000, 1), "K")
    } else {
      formatted_value <- scales::comma(value)
    }
  } else {
    formatted_value <- as.character(value)
  }
  
  # Create HTML structure
  value_box_html <- htmltools::div(
    class = paste("card text-white", paste0("bg-", color)),
    style = "margin: 10px; min-height: 120px;",
    htmltools::div(
      class = "card-body",
      htmltools::div(
        class = "d-flex justify-content-between",
        htmltools::div(
          htmltools::h3(formatted_value, class = "card-title"),
          htmltools::p(title, class = "card-text"),
          if (!is.null(subtitle)) htmltools::tags$small(subtitle, class = "text-light")
        ),
        if (!is.null(icon)) {
          htmltools::div(
            class = "align-self-center",
            htmltools::tags$i(class = paste("fas", icon, "fa-2x"))
          )
        }
      )
    )
  )
  
  return(value_box_html)
}

#' Create Progress Bar
#' @param value Current value
#' @param max_value Maximum value
#' @param title Progress bar title
#' @param color Bar color
#' @return HTML progress bar
create_progress_bar <- function(value, max_value, title = "Progress", color = "success") {
  
  percentage <- round((value / max_value) * 100, 1)
  
  progress_html <- htmltools::div(
    class = "progress-container",
    style = "margin: 15px 0;",
    htmltools::div(
      class = "d-flex justify-content-between mb-1",
      htmltools::span(title),
      htmltools::span(paste0(percentage, "%"))
    ),
    htmltools::div(
      class = "progress",
      style = "height: 20px;",
      htmltools::div(
        class = paste("progress-bar", paste0("bg-", color)),
        role = "progressbar",
        style = paste0("width: ", percentage, "%; height: 100%;"),
        `aria-valuenow` = value,
        `aria-valuemin` = 0,
        `aria-valuemax` = max_value
      )
    )
  )
  
  return(progress_html)
}

#' ========================================
#' 6. SPECIALIZED INTERACTIVE PLOTS
#' ========================================

#' Create Interactive Sankey Diagram
#' @param data Data frame with source, target, and value columns
#' @param title Plot title
#' @return Interactive Sankey diagram
create_sankey_diagram <- function(data, title = "Interactive Sankey Diagram") {
  
  # Prepare nodes
  nodes <- data.frame(
    name = unique(c(data$source, data$target)),
    stringsAsFactors = FALSE
  )
  nodes$id <- 0:(nrow(nodes) - 1)
  
  # Prepare links
  links <- data %>%
    left_join(nodes, by = c("source" = "name")) %>%
    rename(source_id = id) %>%
    left_join(nodes, by = c("target" = "name")) %>%
    rename(target_id = id) %>%
    select(source_id, target_id, value)
  
  # Create Sankey diagram
  p <- plot_ly(
    type = "sankey",
    node = list(
      label = nodes$name,
      color = "blue",
      pad = 15,
      thickness = 20,
      line = list(color = "black", width = 0.5)
    ),
    link = list(
      source = links$source_id,
      target = links$target_id,
      value = links$value
    )
  ) %>%
    layout(
      title = title,
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
    )
  
  return(p)
}

#' Create Interactive Network Graph
#' @param nodes Node data frame with id and label columns
#' @param edges Edge data frame with from, to, and weight columns  
#' @param title Plot title
#' @return Interactive network visualization
create_network_graph <- function(nodes, edges, title = "Interactive Network Graph") {
  
  if (!requireNamespace("visNetwork", quietly = TRUE)) {
    stop("Package 'visNetwork' is required for this function")
  }
  
  # Create visNetwork
  network <- visNetwork::visNetwork(nodes, edges) %>%
    visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visNetwork::visPhysics(stabilization = FALSE) %>%
    visNetwork::visLayout(randomSeed = 42) %>%
    visNetwork::visNodes(
      shape = "circle",
      color = list(
        background = "lightblue",
        border = "darkblue",
        highlight = "yellow"
      ),
      shadow = TRUE
    ) %>%
    visNetwork::visEdges(
      arrows = "to",
      color = list(color = "gray", highlight = "red"),
      width = 2,
      shadow = TRUE
    ) %>%
    visNetwork::visInteraction(
      dragNodes = TRUE,
      dragView = TRUE,
      zoomView = TRUE
    )
  
  return(network)
}

#' Create Interactive Treemap
#' @param data Data frame with category and value columns
#' @param category_col Category column name
#' @param value_col Value column name
#' @param title Plot title
#' @return Interactive treemap
create_treemap <- function(data, category_col, value_col, title = "Interactive Treemap") {
  
  if (!requireNamespace("highcharter", quietly = TRUE)) {
    stop("Package 'highcharter' is required for this function")
  }
  
  # Create treemap using highcharter
  hc <- data %>%
    highcharter::hchart(
      "treemap",
      highcharter::hcaes(x = !!sym(category_col), value = !!sym(value_col), color = !!sym(value_col))
    ) %>%
    highcharter::hc_title(text = title) %>%
    highcharter::hc_colorAxis(stops = highcharter::color_stops(colors = viridis::viridis(10))) %>%
    highcharter::hc_tooltip(pointFormat = "Category: <b>{point.name}</b><br>Value: <b>{point.value}</b>")
  
  return(hc)
}

#' ========================================
#' 7. CROSSTALK INTEGRATION
#' ========================================

#' Create Linked Interactive Plots using Crosstalk
#' @param data Dataset for linked visualizations
#' @return List of linked plots
create_linked_visualizations <- function(data) {
  
  # Create shared data object
  shared_data <- crosstalk::SharedData$new(data)
  
  # Create filter controls
  filter_select <- crosstalk::filter_select(
    id = "category_filter",
    label = "Select Category:",
    sharedData = shared_data,
    group = ~category
  )
  
  filter_slider <- crosstalk::filter_slider(
    id = "value_filter",
    label = "Filter by X Value:",
    sharedData = shared_data,
    column = ~x_value,
    step = 1
  )
  
  # Create linked scatter plot
  scatter <- shared_data %>%
    plot_ly(x = ~x_value, y = ~y_value, color = ~category,
            type = "scatter", mode = "markers") %>%
    layout(title = "Linked Scatter Plot")
  
  # Create linked histogram
  histogram <- shared_data %>%
    plot_ly(x = ~x_value, color = ~category, type = "histogram") %>%
    layout(title = "Linked Histogram")
  
  # Create linked table
  data_table <- shared_data %>%
    DT::datatable(options = list(pageLength = 10))
  
  return(list(
    filters = list(category = filter_select, value = filter_slider),
    plots = list(scatter = scatter, histogram = histogram),
    table = data_table
  ))
}

#' ========================================
#' 8. ANIMATION FUNCTIONS
#' ========================================

#' Create Animated Scatter Plot
#' @param data Dataset with time dimension
#' @param x_var X variable
#' @param y_var Y variable
#' @param time_var Time variable for animation
#' @param color_var Color variable (optional)
#' @param title Plot title
#' @return Animated plotly plot
create_animated_scatter <- function(data, x_var, y_var, time_var, 
                                  color_var = NULL, title = "Animated Scatter Plot") {
  
  # Create base plot
  p <- plot_ly(
    data = data,
    x = ~get(x_var),
    y = ~get(y_var),
    frame = ~get(time_var),
    type = "scatter",
    mode = "markers",
    marker = list(size = 10, opacity = 0.7)
  )
  
  # Add color if specified
  if (!is.null(color_var)) {
    p <- p %>% add_trace(color = ~get(color_var))
  }
  
  # Add animation controls
  p <- p %>%
    animation_opts(
      frame = 1000,
      transition = 300,
      redraw = FALSE
    ) %>%
    animation_slider(
      currentvalue = list(prefix = paste(tools::toTitleCase(gsub("_", " ", time_var)), ": "))
    ) %>%
    layout(
      title = title,
      xaxis = list(title = tools::toTitleCase(gsub("_", " ", x_var))),
      yaxis = list(title = tools::toTitleCase(gsub("_", " ", y_var)))
    )
  
  return(p)
}

#' Create Animated Bar Chart Race
#' @param data Dataset with time and category dimensions
#' @param category_var Category variable
#' @param value_var Value variable
#' @param time_var Time variable
#' @param title Plot title
#' @param top_n Number of top categories to show
#' @return Animated bar chart race
create_bar_chart_race <- function(data, category_var, value_var, time_var,
                                 title = "Animated Bar Chart Race", top_n = 10) {
  
  # Prepare data for each time frame
  race_data <- data %>%
    group_by(!!sym(time_var)) %>%
    arrange(desc(!!sym(value_var))) %>%
    slice_head(n = top_n) %>%
    mutate(rank = row_number()) %>%
    ungroup()
  
  # Create animated bar chart
  p <- race_data %>%
    plot_ly(
      x = ~get(value_var),
      y = ~reorder(get(category_var), get(value_var)),
      frame = ~get(time_var),
      type = "bar",
      orientation = "h",
      text = ~get(value_var),
      textposition = "outside",
      marker = list(color = "steelblue")
    ) %>%
    layout(
      title = title,
      xaxis = list(title = tools::toTitleCase(gsub("_", " ", value_var))),
      yaxis = list(title = tools::toTitleCase(gsub("_", " ", category_var)))
    ) %>%
    animation_opts(frame = 1000, transition = 300)
  
  return(p)
}

#' ========================================
#' 9. EXPORT AND SHARING FUNCTIONS
#' ========================================

#' Save Interactive Plot as HTML
#' @param plot Plotly or htmlwidget object
#' @param filename Output filename
#' @param title HTML page title
#' @return Success message
save_interactive_plot <- function(plot, filename, title = "Interactive Plot") {
  
  # Create HTML page
  html_page <- htmltools::browsable(
    htmltools::tagList(
      htmltools::tags$head(
        htmltools::tags$title(title),
        htmltools::tags$meta(charset = "UTF-8"),
        htmltools::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0")
      ),
      htmltools::tags$body(
        htmltools::tags$h1(title, style = "text-align: center; color: #333;"),
        plot
      )
    )
  )
  
  # Save to file
  htmlwidgets::saveWidget(
    widget = html_page,
    file = filename,
    selfcontained = TRUE
  )
  
  cat("Interactive plot saved to:", filename, "\n")
  return(filename)
}

#' Create Dashboard Layout
#' @param plots List of plots to include
#' @param title Dashboard title
#' @return HTML dashboard
create_simple_dashboard <- function(plots, title = "Interactive Dashboard") {
  
  # Create dashboard structure
  dashboard <- htmltools::tagList(
    htmltools::tags$head(
      htmltools::tags$title(title),
      htmltools::tags$style(HTML("
        body { font-family: Arial, sans-serif; margin: 20px; }
        .dashboard-header { text-align: center; color: #333; margin-bottom: 30px; }
        .plot-container { margin: 20px 0; padding: 20px; border: 1px solid #ddd; border-radius: 5px; }
        .row { display: flex; flex-wrap: wrap; }
        .col { flex: 1; min-width: 400px; margin: 10px; }
      "))
    ),
    htmltools::tags$body(
      htmltools::tags$h1(title, class = "dashboard-header"),
      htmltools::tags$div(
        class = "row",
        lapply(plots, function(plot) {
          htmltools::tags$div(
            class = "col",
            htmltools::tags$div(
              class = "plot-container",
              plot
            )
          )
        })
      )
    )
  )
  
  return(dashboard)
}

#' ========================================
#' 10. DEMONSTRATION FUNCTIONS
#' ========================================

#' Generate Demo Data for Interactive Plots
generate_interactive_demo_data <- function() {
  
  set.seed(42)
  n <- 200
  
  # Main dataset
  demo_data <- data.frame(
    id = 1:n,
    x_value = rnorm(n, 50, 15),
    y_value = rnorm(n, 100, 25),
    z_value = rnorm(n, 75, 20),
    category = sample(c("A", "B", "C", "D"), n, replace = TRUE),
    size_metric = abs(rnorm(n, 10, 3)),
    date = seq(as.Date("2023-01-01"), length.out = n, by = "day"),
    revenue = cumsum(abs(rnorm(n, 1000, 500))),
    stringsAsFactors = FALSE
  )
  
  # Time series data
  ts_data <- data.frame(
    date = seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day"),
    sales = 1000 + 200 * sin(1:365 * 2 * pi / 365) + rnorm(365, 0, 100),
    marketing_spend = 500 + 100 * cos(1:365 * 2 * pi / 365) + rnorm(365, 0, 50),
    website_visits = 5000 + 1000 * sin(1:365 * 2 * pi / 365) + rnorm(365, 0, 500)
  )
  
  # Network data
  nodes <- data.frame(
    id = 1:10,
    label = paste("Node", 1:10),
    group = sample(c("Group A", "Group B", "Group C"), 10, replace = TRUE)
  )
  
  edges <- data.frame(
    from = sample(1:10, 15, replace = TRUE),
    to = sample(1:10, 15, replace = TRUE),
    weight = sample(1:5, 15, replace = TRUE)
  ) %>%
    filter(from != to) %>%
    distinct()
  
  # Sankey data
  sankey_data <- data.frame(
    source = c("A", "A", "B", "B", "C"),
    target = c("X", "Y", "X", "Z", "Z"),
    value = c(10, 15, 20, 25, 30)
  )
  
  return(list(
    main_data = demo_data,
    time_series = ts_data,
    nodes = nodes,
    edges = edges,
    sankey_data = sankey_data
  ))
}

#' Run Interactive Plotting Demo
demo_interactive_plots <- function() {
  
  cat("=== INTERACTIVE PLOTTING DEMONSTRATION ===\n\n")
  
  # Generate demo data
  demo_data <- generate_interactive_demo_data()
  
  cat("1. INTERACTIVE SCATTER PLOTS\n")
  cat(paste(rep("=", 40), collapse = ""), "\n")
  
  # Create interactive scatter plot
  scatter_plot <- create_interactive_scatter(
    demo_data$main_data, 
    "x_value", "y_value", 
    color_var = "category", 
    size_var = "size_metric",
    title = "Interactive Scatter Plot Demo"
  )
  
  cat("Created interactive scatter plot with color and size mapping\n")
  
  # Create 3D scatter plot
  scatter_3d <- create_3d_scatter(
    demo_data$main_data,
    "x_value", "y_value", "z_value",
    color_var = "category",
    title = "3D Interactive Scatter Plot Demo"
  )
  
  cat("Created 3D interactive scatter plot\n")
  
  cat("\n2. INTERACTIVE TIME SERIES\n")
  cat(paste(rep("=", 40), collapse = ""), "\n")
  
  # Create time series plot
  ts_plot <- create_interactive_timeseries(
    demo_data$time_series,
    "date", "sales",
    title = "Interactive Sales Time Series"
  )
  
  cat("Created interactive time series with range selector\n")
  
  # Multi-line time series
  ts_long <- demo_data$time_series %>%
    tidyr::pivot_longer(cols = c(sales, marketing_spend, website_visits), 
                       names_to = "metric", values_to = "value") %>%
    group_by(metric) %>%
    mutate(value = scale(value)[,1]) %>%  # Normalize for comparison
    ungroup()
  
  ts_multi <- create_interactive_timeseries(
    ts_long,
    "date", "value", "metric",
    title = "Multi-Metric Time Series (Normalized)"
  )
  
  cat("Created multi-line time series visualization\n")
  
  cat("\n3. INTERACTIVE HEATMAPS\n")
  cat(paste(rep("=", 40), collapse = ""), "\n")
  
  # Create correlation heatmap
  numeric_data <- demo_data$main_data %>% select_if(is.numeric)
  corr_heatmap <- create_interactive_correlation_heatmap(
    numeric_data,
    title = "Interactive Correlation Matrix"
  )
  
  cat("Created interactive correlation heatmap\n")
  
  cat("\n4. INTERACTIVE DATA TABLES\n")
  cat(paste(rep("=", 40), collapse = ""), "\n")
  
  # Create enhanced data table
  data_table <- create_interactive_table(
    demo_data$main_data,
    title = "Enhanced Interactive Data Table",
    page_length = 15,
    searchable = TRUE,
    downloadable = TRUE
  )
  
  cat("Created interactive data table with search and download features\n")
  
  # Create reactable table (if available)
  if (requireNamespace("reactable", quietly = TRUE)) {
    react_table <- create_reactable_table(
      demo_data$main_data %>% head(50),
      title = "Reactable Demo Table",
      searchable = TRUE
    )
    cat("Created modern reactable with advanced features\n")
  } else {
    react_table <- NULL
    cat("Reactable package not available, skipping reactable demo\n")
  }
  
  cat("\n5. SPECIALIZED VISUALIZATIONS\n")
  cat(paste(rep("=", 40), collapse = ""), "\n")
  
  # Create network graph (if available)
  if (requireNamespace("visNetwork", quietly = TRUE)) {
    network_graph <- create_network_graph(
      demo_data$nodes,
      demo_data$edges,
      title = "Interactive Network Visualization"
    )
    cat("Created interactive network graph\n")
  } else {
    network_graph <- NULL
    cat("visNetwork package not available, skipping network graph\n")
  }
  
  # Create Sankey diagram
  sankey_plot <- create_sankey_diagram(
    demo_data$sankey_data,
    title = "Interactive Sankey Diagram"
  )
  cat("Created interactive Sankey diagram\n")
  
  # Create treemap data
  treemap_data <- demo_data$main_data %>%
    group_by(category) %>%
    summarise(total_value = sum(size_metric), .groups = "drop")
  
  if (requireNamespace("highcharter", quietly = TRUE)) {
    treemap_plot <- create_treemap(
      treemap_data,
      "category", "total_value",
      title = "Interactive Treemap Demo"
    )
    cat("Created interactive treemap visualization\n")
  } else {
    treemap_plot <- NULL
    cat("highcharter package not available, skipping treemap\n")
  }
  
  cat("\n6. LINKED VISUALIZATIONS\n")
  cat(paste(rep("=", 40), collapse = ""), "\n")
  
  # Create linked visualizations
  linked_plots <- create_linked_visualizations(demo_data$main_data)
  cat("Created linked interactive visualizations with crosstalk\n")
  
  cat("\nInteractive Plotting Demo Complete!\n")
  cat("All plots are ready for viewing in RStudio Viewer or web browser.\n")
  
  return(list(
    scatter_plot = scatter_plot,
    scatter_3d = scatter_3d,
    time_series = ts_plot,
    multi_time_series = ts_multi,
    correlation_heatmap = corr_heatmap,
    data_table = data_table,
    reactable = react_table,
    network_graph = network_graph,
    sankey_plot = sankey_plot,
    treemap = treemap_plot,
    linked_plots = linked_plots,
    demo_data = demo_data
  ))
}

# Export key functions for package use
interactive_plots_exports <- list(
  create_interactive_scatter = create_interactive_scatter,
  create_3d_scatter = create_3d_scatter,
  create_interactive_timeseries = create_interactive_timeseries,
  create_interactive_correlation_heatmap = create_interactive_correlation_heatmap,
  create_interactive_heatmap = create_interactive_heatmap,
  create_interactive_table = create_interactive_table,
  create_reactable_table = create_reactable_table,
  create_value_box = create_value_box,
  create_progress_bar = create_progress_bar,
  create_sankey_diagram = create_sankey_diagram,
  create_network_graph = create_network_graph,
  create_treemap = create_treemap,
  create_linked_visualizations = create_linked_visualizations,
  create_animated_scatter = create_animated_scatter,
  create_bar_chart_race = create_bar_chart_race,
  save_interactive_plot = save_interactive_plot,
  create_simple_dashboard = create_simple_dashboard,
  generate_interactive_demo_data = generate_interactive_demo_data,
  demo_interactive_plots = demo_interactive_plots
)

# Run demo if script is executed directly
if (sys.nframe() == 0) {
  demo_results <- demo_interactive_plots()
  cat("\nDemo results stored in 'demo_results' variable\n")
  cat("Access individual plots with demo_results$scatter_plot, etc.\n")
}