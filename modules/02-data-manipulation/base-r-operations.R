# @title Base R Data Manipulation Operations
# @description Comprehensive guide to fundamental R data operations without external packages
# @author R Data Science Portfolio
# @date 2025

#' ========================================
#' BASE R DATA MANIPULATION FRAMEWORK
#' ========================================

# Core base R operations for data manipulation, subsetting, and transformation

#' ========================================
#' 1. DATA STRUCTURES AND CREATION
#' ========================================

#' Create Sample Dataset for Demonstrations
#' @param n Number of observations
#' @return Data frame with mixed data types
create_sample_dataset <- function(n = 1000) {
  set.seed(42)
  
  data.frame(
    id = 1:n,
    name = paste("Person", 1:n),
    age = sample(18:80, n, replace = TRUE),
    salary = round(rnorm(n, 50000, 15000), 2),
    department = sample(c("Sales", "Engineering", "Marketing", "HR", "Finance"), n, replace = TRUE),
    start_date = as.Date("2020-01-01") + sample(0:1460, n, replace = TRUE),
    performance_score = round(rnorm(n, 7.5, 1.5), 1),
    is_manager = sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.15, 0.85)),
    bonus_pct = ifelse(sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(0.3, 0.7)), 
                       round(runif(n, 0.05, 0.2), 3), 0),
    region = sample(c("North", "South", "East", "West"), n, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

#' ========================================
#' 2. DATA SUBSETTING AND INDEXING
#' ========================================

#' Demonstrate Base R Subsetting Operations
demonstrate_subsetting <- function() {
  
  cat("BASE R SUBSETTING OPERATIONS\n")
  cat("============================\n\n")
  
  # Create sample data
  df <- create_sample_dataset(100)
  
  cat("1. ROW SUBSETTING\n")
  cat("-----------------\n")
  
  # Numeric indexing
  first_10 <- df[1:10, ]
  cat("First 10 rows: df[1:10, ]\n")
  
  # Logical indexing
  high_earners <- df[df$salary > 60000, ]
  cat("High earners: df[df$salary > 60000, ]\n")
  cat("Number of high earners:", nrow(high_earners), "\n")
  
  # Multiple conditions
  senior_managers <- df[df$age > 40 & df$is_manager == TRUE, ]
  cat("Senior managers: df[df$age > 40 & df$is_manager == TRUE, ]\n")
  cat("Number of senior managers:", nrow(senior_managers), "\n")
  
  # Using which() for efficiency
  engineering_indices <- which(df$department == "Engineering")
  engineering_staff <- df[engineering_indices, ]
  cat("Engineering staff using which(): df[which(df$department == 'Engineering'), ]\n")
  
  cat("\n2. COLUMN SUBSETTING\n")
  cat("---------------------\n")
  
  # Single column
  names_vector <- df$name
  names_df <- df["name"]
  names_df2 <- df[, "name"]
  cat("Single column access: df$name, df['name'], df[, 'name']\n")
  
  # Multiple columns
  basic_info <- df[c("name", "age", "department")]
  basic_info2 <- df[, c("name", "age", "department")]
  cat("Multiple columns: df[c('name', 'age', 'department')]\n")
  
  # Exclude columns
  without_id <- df[, !names(df) %in% c("id")]
  cat("Exclude columns: df[, !names(df) %in% c('id')]\n")
  
  cat("\n3. CONDITIONAL SUBSETTING\n")
  cat("--------------------------\n")
  
  # Complex conditions
  top_performers <- df[df$performance_score > 8.5 & df$bonus_pct > 0, ]
  cat("Top performers with bonus: nrow =", nrow(top_performers), "\n")
  
  # Using subset() function
  sales_team <- subset(df, department == "Sales" & salary > 45000)
  cat("Sales team (high salary): nrow =", nrow(sales_team), "\n")
  
  # Missing value handling
  complete_cases <- df[complete.cases(df), ]
  cat("Complete cases: nrow =", nrow(complete_cases), "\n")
  
  return(list(
    original = df,
    high_earners = high_earners,
    senior_managers = senior_managers,
    top_performers = top_performers
  ))
}

#' ========================================
#' 3. DATA TRANSFORMATION
#' ========================================

#' Base R Data Transformation Operations
perform_base_transformations <- function(data) {
  
  cat("BASE R DATA TRANSFORMATIONS\n")
  cat("===========================\n\n")
  
  # Make a copy to avoid modifying original
  df <- data.frame(data)
  
  cat("1. CREATING NEW VARIABLES\n")
  cat("--------------------------\n")
  
  # Simple calculations
  df$annual_bonus <- df$salary * df$bonus_pct
  df$total_compensation <- df$salary + df$annual_bonus
  df$years_employed <- as.numeric(Sys.Date() - df$start_date) / 365.25
  
  # Conditional variables
  df$seniority_level <- ifelse(df$years_employed < 1, "Junior",
                              ifelse(df$years_employed < 3, "Mid", "Senior"))
  
  df$salary_category <- cut(df$salary, 
                           breaks = c(0, 40000, 60000, 80000, Inf),
                           labels = c("Low", "Medium", "High", "Very High"),
                           include.lowest = TRUE)
  
  # Complex conditional logic
  df$performance_category <- with(df, {
    ifelse(performance_score >= 9, "Excellent",
           ifelse(performance_score >= 8, "Good",
                  ifelse(performance_score >= 6, "Average", "Needs Improvement")))
  })
  
  cat("Created variables: annual_bonus, total_compensation, years_employed\n")
  cat("Categorical variables: seniority_level, salary_category, performance_category\n")
  
  cat("\n2. DATA TYPE CONVERSIONS\n")
  cat("------------------------\n")
  
  # Factor conversions
  df$department <- factor(df$department)
  df$region <- factor(df$region)
  df$performance_category <- factor(df$performance_category, 
                                   levels = c("Needs Improvement", "Average", "Good", "Excellent"),
                                   ordered = TRUE)
  
  # Numeric conversions
  df$age_numeric <- as.numeric(df$age)
  df$salary_thousands <- round(df$salary / 1000, 1)
  
  cat("Converted to factors: department, region, performance_category\n")
  cat("Numeric transformations: salary_thousands\n")
  
  cat("\n3. STRING OPERATIONS\n")
  cat("--------------------\n")
  
  # String manipulations
  df$name_upper <- toupper(df$name)
  df$name_length <- nchar(df$name)
  df$first_name <- sub(" .*", "", df$name)
  df$employee_code <- paste(substr(df$department, 1, 3), 
                           sprintf("%04d", df$id), sep = "-")
  
  cat("String operations: name_upper, name_length, first_name, employee_code\n")
  
  return(df)
}

#' ========================================
#' 4. DATA AGGREGATION
#' ========================================

#' Base R Aggregation Operations
perform_base_aggregation <- function(data) {
  
  cat("BASE R AGGREGATION OPERATIONS\n")
  cat("=============================\n\n")
  
  cat("1. SUMMARY STATISTICS\n")
  cat("---------------------\n")
  
  # Basic summaries
  salary_summary <- summary(data$salary)
  age_summary <- summary(data$age)
  
  cat("Salary summary:\n")
  print(salary_summary)
  cat("\nAge summary:\n")
  print(age_summary)
  
  # Custom summary function
  custom_summary <- function(x) {
    c(Mean = mean(x, na.rm = TRUE),
      Median = median(x, na.rm = TRUE),
      SD = sd(x, na.rm = TRUE),
      IQR = IQR(x, na.rm = TRUE),
      Min = min(x, na.rm = TRUE),
      Max = max(x, na.rm = TRUE))
  }
  
  performance_stats <- custom_summary(data$performance_score)
  cat("\nPerformance Score Statistics:\n")
  print(round(performance_stats, 2))
  
  cat("\n2. GROUP-BY OPERATIONS\n")
  cat("----------------------\n")
  
  # Using aggregate()
  dept_salary <- aggregate(salary ~ department, data = data, FUN = mean)
  names(dept_salary)[2] <- "avg_salary"
  cat("Average salary by department:\n")
  print(dept_salary)
  
  # Multiple aggregations
  dept_summary <- aggregate(cbind(salary, performance_score) ~ department, 
                           data = data, 
                           FUN = function(x) c(mean = mean(x), sd = sd(x)))
  cat("\nDepartment summary (mean and sd):\n")
  print(dept_summary)
  
  # Using tapply()
  region_performance <- tapply(data$performance_score, data$region, mean)
  cat("\nAverage performance by region:\n")
  print(round(region_performance, 2))
  
  # Using by()
  dept_detailed <- by(data$salary, data$department, summary)
  cat("\nDetailed salary summary by department:\n")
  print(dept_detailed)
  
  cat("\n3. CROSS-TABULATIONS\n")
  cat("--------------------\n")
  
  # Simple table
  dept_table <- table(data$department)
  cat("Department counts:\n")
  print(dept_table)
  
  # Cross-table
  dept_region_table <- table(data$department, data$region)
  cat("\nDepartment by Region cross-table:\n")
  print(dept_region_table)
  
  # Proportions
  dept_region_prop <- prop.table(dept_region_table, margin = 1)
  cat("\nDepartment by Region proportions (row percentages):\n")
  print(round(dept_region_prop, 3))
  
  return(list(
    salary_summary = salary_summary,
    dept_salary = dept_salary,
    dept_summary = dept_summary,
    region_performance = region_performance,
    dept_table = dept_table,
    dept_region_table = dept_region_table
  ))
}

#' ========================================
#' 5. DATA RESHAPING
#' ========================================

#' Base R Data Reshaping Operations
demonstrate_reshaping <- function(data) {
  
  cat("BASE R DATA RESHAPING\n")
  cat("=====================\n\n")
  
  cat("1. WIDE TO LONG TRANSFORMATION\n")
  cat("-------------------------------\n")
  
  # Create sample wide data
  wide_data <- data.frame(
    employee = paste("Emp", 1:10),
    Q1_sales = round(rnorm(10, 50000, 10000)),
    Q2_sales = round(rnorm(10, 55000, 10000)),
    Q3_sales = round(rnorm(10, 52000, 10000)),
    Q4_sales = round(rnorm(10, 58000, 10000))
  )
  
  cat("Original wide data shape:", dim(wide_data), "\n")
  
  # Reshape to long format using reshape()
  long_data <- reshape(wide_data,
                      varying = c("Q1_sales", "Q2_sales", "Q3_sales", "Q4_sales"),
                      v.names = "sales",
                      timevar = "quarter",
                      times = c("Q1", "Q2", "Q3", "Q4"),
                      direction = "long")
  
  # Clean up row names
  rownames(long_data) <- NULL
  
  cat("Long data shape:", dim(long_data), "\n")
  cat("First few rows of long data:\n")
  print(head(long_data))
  
  cat("\n2. LONG TO WIDE TRANSFORMATION\n")
  cat("-------------------------------\n")
  
  # Reshape back to wide
  wide_again <- reshape(long_data,
                       v.names = "sales",
                       idvar = "employee",
                       timevar = "quarter",
                       direction = "wide")
  
  cat("Reshaped back to wide:", dim(wide_again), "\n")
  
  cat("\n3. MANUAL RESHAPING TECHNIQUES\n")
  cat("-------------------------------\n")
  
  # Manual approach using split and do.call
  dept_performance <- split(data$performance_score, data$department)
  dept_matrix <- do.call(cbind, lapply(dept_performance, function(x) {
    c(mean = mean(x), median = median(x), sd = sd(x))
  }))
  
  cat("Department performance matrix:\n")
  print(round(dept_matrix, 2))
  
  return(list(
    wide_data = wide_data,
    long_data = long_data,
    wide_again = wide_again,
    dept_matrix = dept_matrix
  ))
}

#' ========================================
#' 6. DATA MERGING AND JOINING
#' ========================================

#' Base R Data Merging Operations
demonstrate_merging <- function() {
  
  cat("BASE R DATA MERGING\n")
  cat("===================\n\n")
  
  # Create sample datasets
  employees <- data.frame(
    emp_id = 1:10,
    name = paste("Employee", 1:10),
    dept_id = sample(1:3, 10, replace = TRUE),
    salary = round(rnorm(10, 50000, 10000))
  )
  
  departments <- data.frame(
    dept_id = 1:3,
    dept_name = c("Engineering", "Sales", "Marketing"),
    location = c("Building A", "Building B", "Building C")
  )
  
  projects <- data.frame(
    emp_id = c(1, 2, 2, 3, 4, 5, 5, 6),
    project_id = c("P001", "P002", "P003", "P001", "P002", "P003", "P004", "P001"),
    hours = c(40, 35, 20, 45, 30, 25, 15, 50)
  )
  
  cat("Sample datasets created:\n")
  cat("Employees:", nrow(employees), "rows\n")
  cat("Departments:", nrow(departments), "rows\n")
  cat("Projects:", nrow(projects), "rows\n")
  
  cat("\n1. INNER JOIN\n")
  cat("-------------\n")
  
  # Inner join - only matching records
  emp_dept_inner <- merge(employees, departments, by = "dept_id")
  cat("Inner join (employees + departments):", nrow(emp_dept_inner), "rows\n")
  
  cat("\n2. LEFT JOIN\n")
  cat("------------\n")
  
  # Left join - all employees, matching departments
  emp_dept_left <- merge(employees, departments, by = "dept_id", all.x = TRUE)
  cat("Left join (all employees):", nrow(emp_dept_left), "rows\n")
  
  cat("\n3. RIGHT JOIN\n")
  cat("-------------\n")
  
  # Right join - all departments, matching employees
  emp_dept_right <- merge(employees, departments, by = "dept_id", all.y = TRUE)
  cat("Right join (all departments):", nrow(emp_dept_right), "rows\n")
  
  cat("\n4. FULL OUTER JOIN\n")
  cat("------------------\n")
  
  # Full outer join - all records from both
  emp_dept_full <- merge(employees, departments, by = "dept_id", all = TRUE)
  cat("Full outer join:", nrow(emp_dept_full), "rows\n")
  
  cat("\n5. MANY-TO-MANY JOIN\n")
  cat("--------------------\n")
  
  # Join with projects (many-to-many relationship)
  emp_projects <- merge(employees, projects, by = "emp_id", all.x = TRUE)
  cat("Employee-Projects join:", nrow(emp_projects), "rows\n")
  
  cat("\n6. MULTIPLE TABLE JOIN\n")
  cat("----------------------\n")
  
  # Join all three tables
  complete_data <- merge(emp_dept_inner, projects, by = "emp_id", all.x = TRUE)
  cat("Complete dataset:", nrow(complete_data), "rows\n")
  
  return(list(
    employees = employees,
    departments = departments,
    projects = projects,
    emp_dept_inner = emp_dept_inner,
    emp_dept_left = emp_dept_left,
    complete_data = complete_data
  ))
}

#' ========================================
#' 7. ADVANCED BASE R OPERATIONS
#' ========================================

#' Advanced Base R Data Operations
demonstrate_advanced_operations <- function(data) {
  
  cat("ADVANCED BASE R OPERATIONS\n")
  cat("==========================\n\n")
  
  cat("1. APPLY FAMILY FUNCTIONS\n")
  cat("-------------------------\n")
  
  # Select numeric columns
  numeric_cols <- sapply(data, is.numeric)
  numeric_data <- data[, numeric_cols]
  
  # apply() - row and column operations
  col_means <- apply(numeric_data, 2, mean, na.rm = TRUE)
  cat("Column means using apply():\n")
  print(round(col_means, 2))
  
  row_sums <- apply(numeric_data[1:5, ], 1, sum, na.rm = TRUE)
  cat("\nFirst 5 row sums:\n")
  print(row_sums)
  
  # lapply() - list apply
  dept_splits <- split(data$salary, data$department)
  dept_summaries <- lapply(dept_splits, function(x) {
    c(mean = mean(x), median = median(x), sd = sd(x))
  })
  cat("\nDepartment salary summaries using lapply():\n")
  print(dept_summaries)
  
  # sapply() - simplified apply
  dept_means <- sapply(dept_splits, mean)
  cat("\nDepartment means using sapply():\n")
  print(round(dept_means, 2))
  
  cat("\n2. VECTORIZATION\n")
  cat("----------------\n")
  
  # Vectorized operations
  data$salary_scaled <- scale(data$salary)[, 1]
  data$salary_rank <- rank(data$salary)
  data$salary_percentile <- rank(data$salary) / length(data$salary)
  
  cat("Created: salary_scaled, salary_rank, salary_percentile\n")
  
  # Conditional vectorization
  data$bonus_eligible <- ifelse(data$performance_score > 7 & data$years_employed > 1, 
                               "Yes", "No")
  
  cat("Created: bonus_eligible\n")
  
  cat("\n3. MISSING DATA HANDLING\n")
  cat("------------------------\n")
  
  # Introduce some missing values for demonstration
  data_with_na <- data
  missing_indices <- sample(nrow(data_with_na), 20)
  data_with_na$salary[missing_indices[1:10]] <- NA
  data_with_na$performance_score[missing_indices[11:20]] <- NA
  
  # Identify missing data patterns
  missing_summary <- sapply(data_with_na, function(x) sum(is.na(x)))
  cat("Missing values by column:\n")
  print(missing_summary[missing_summary > 0])
  
  # Complete cases
  complete_rows <- complete.cases(data_with_na)
  cat("Complete cases:", sum(complete_rows), "out of", nrow(data_with_na), "\n")
  
  # Imputation strategies
  salary_median <- median(data_with_na$salary, na.rm = TRUE)
  data_with_na$salary[is.na(data_with_na$salary)] <- salary_median
  
  performance_mean <- mean(data_with_na$performance_score, na.rm = TRUE)
  data_with_na$performance_score[is.na(data_with_na$performance_score)] <- performance_mean
  
  cat("Applied median imputation for salary, mean imputation for performance\n")
  
  return(list(
    col_means = col_means,
    dept_summaries = dept_summaries,
    data_with_transformations = data,
    data_imputed = data_with_na
  ))
}

#' ========================================
#' 8. COMPREHENSIVE DEMONSTRATION
#' ========================================

#' Run Complete Base R Operations Demo
#' @param verbose Logical, whether to show detailed output
#' @export
run_base_r_demo <- function(verbose = TRUE) {
  
  if (verbose) {
    cat("\n", paste(rep("=", 60), collapse = ""), "\n")
    cat("         BASE R DATA MANIPULATION PORTFOLIO\n")
    cat(paste(rep("=", 60), collapse = ""), "\n\n")
  }
  
  # Create sample dataset
  sample_data <- create_sample_dataset(200)
  
  if (verbose) {
    cat("Created sample dataset with", nrow(sample_data), "observations\n")
    cat("Columns:", paste(names(sample_data), collapse = ", "), "\n\n")
  }
  
  # Demonstrate all operations
  results <- list()
  
  if (verbose) cat("1. SUBSETTING OPERATIONS\n")
  results$subsetting <- demonstrate_subsetting()
  
  if (verbose) cat("\n2. DATA TRANSFORMATIONS\n")
  results$transformations <- perform_base_transformations(sample_data)
  
  if (verbose) cat("\n3. AGGREGATION OPERATIONS\n")
  results$aggregations <- perform_base_aggregation(sample_data)
  
  if (verbose) cat("\n4. RESHAPING OPERATIONS\n")
  results$reshaping <- demonstrate_reshaping(sample_data)
  
  if (verbose) cat("\n5. MERGING OPERATIONS\n")
  results$merging <- demonstrate_merging()
  
  if (verbose) cat("\n6. ADVANCED OPERATIONS\n")
  results$advanced <- demonstrate_advanced_operations(results$transformations)
  
  if (verbose) {
    cat("\n", paste(rep("=", 60), collapse = ""), "\n")
    cat("       BASE R OPERATIONS DEMONSTRATION COMPLETE\n")
    cat(paste(rep("=", 60), collapse = ""), "\n")
  }
  
  invisible(results)
}

# Example usage
if (interactive()) {
  # Run comprehensive demonstration
  base_r_results <- run_base_r_demo(verbose = TRUE)
  
  # Access specific results
  # base_r_results$subsetting$high_earners
  # base_r_results$aggregations$dept_salary
  # base_r_results$merging$complete_data
}