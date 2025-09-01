# @title Comprehensive Unsupervised Learning Framework
# @description Advanced clustering, dimensionality reduction, and association analysis
# @author R Data Science Portfolio Developer
# @date 2025

#' ========================================
#' COMPREHENSIVE UNSUPERVISED LEARNING
#' ========================================

# Load required libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr, ggplot2, cluster, factoextra, FactoMineR, corrplot,
  pheatmap, dbscan, fpc, mclust, mixtools, apcluster, igraph,
  arules, arulesViz, Rtsne, umap, ica, NMF, plotly, 
  GGally, gridExtra, RColorBrewer, viridis
)

#' ========================================
#' 1. DATA PREPARATION FOR UNSUPERVISED LEARNING
#' ========================================

#' Prepare Data for Unsupervised Learning
#' @param data Input dataset
#' @param exclude_cols Columns to exclude from analysis
#' @param scale_data Whether to scale the data
#' @return Preprocessed data and metadata
prepare_unsupervised_data <- function(data, exclude_cols = NULL, scale_data = TRUE) {
  
  # Remove specified columns
  if (!is.null(exclude_cols)) {
    data <- data %>% select(-all_of(exclude_cols))
  }
  
  # Handle categorical variables
  categorical_cols <- sapply(data, function(x) is.character(x) | is.factor(x))
  numeric_cols <- sapply(data, is.numeric)
  
  # Store original data
  original_data <- data
  
  # Process categorical variables (create dummy variables)
  if (any(categorical_cols)) {
    cat_data <- data[, categorical_cols, drop = FALSE]
    
    # Create dummy variables
    dummy_data <- model.matrix(~ . - 1, data = cat_data)
    
    # Combine with numeric data
    if (any(numeric_cols)) {
      numeric_data <- data[, numeric_cols, drop = FALSE]
      processed_data <- cbind(numeric_data, dummy_data)
    } else {
      processed_data <- as.data.frame(dummy_data)
    }
  } else {
    processed_data <- data[, numeric_cols, drop = FALSE]
  }
  
  # Handle missing values
  missing_summary <- processed_data %>%
    summarise_all(~sum(is.na(.))) %>%
    tidyr::gather(variable, missing_count) %>%
    mutate(missing_pct = missing_count / nrow(processed_data) * 100)
  
  # Remove columns with too many missing values (>50%)
  high_missing_cols <- missing_summary$variable[missing_summary$missing_pct > 50]
  if (length(high_missing_cols) > 0) {
    processed_data <- processed_data %>% select(-all_of(high_missing_cols))
    warning(paste("Removed columns with >50% missing:", paste(high_missing_cols, collapse = ", ")))
  }
  
  # Impute remaining missing values with median
  processed_data <- processed_data %>%
    mutate_all(~ifelse(is.na(.), median(., na.rm = TRUE), .))
  
  # Scale data if requested
  if (scale_data) {
    scaling_params <- list(
      center = sapply(processed_data, mean),
      scale = sapply(processed_data, sd)
    )
    processed_data <- as.data.frame(scale(processed_data))
  } else {
    scaling_params <- NULL
  }
  
  # Remove any remaining non-finite values
  processed_data <- processed_data[complete.cases(processed_data), ]
  
  return(list(
    processed_data = processed_data,
    original_data = original_data,
    scaling_params = scaling_params,
    missing_summary = missing_summary,
    categorical_cols = names(original_data)[categorical_cols],
    numeric_cols = names(original_data)[numeric_cols]
  ))
}

#' ========================================
#' 2. CLUSTERING ANALYSIS
#' ========================================

#' Comprehensive Clustering Analysis
#' @param data Preprocessed data matrix
#' @param max_clusters Maximum number of clusters to test
#' @return Clustering results for multiple algorithms
perform_clustering_analysis <- function(data, max_clusters = 10) {
  
  clustering_results <- list()
  
  # 1. K-Means Clustering
  cat("Performing K-Means clustering...\n")
  
  # Determine optimal number of clusters
  wss <- map_dbl(1:max_clusters, ~{
    kmeans(data, centers = .x, nstart = 25)$tot.withinss
  })
  
  silhouette_scores <- map_dbl(2:max_clusters, ~{
    km <- kmeans(data, centers = .x, nstart = 25)
    ss <- silhouette(km$cluster, dist(data))
    mean(ss[, 3])
  })
  
  # Gap statistic
  gap_stat <- clusGap(data, FUN = kmeans, nstart = 25, K.max = max_clusters, B = 50)
  optimal_k_gap <- maxSE(gap_stat$Tab[, "gap"], gap_stat$Tab[, "SE.sim"])
  
  # Choose optimal k
  optimal_k_silhouette <- which.max(silhouette_scores) + 1
  optimal_k_elbow <- which.min(diff(diff(wss))) + 1
  
  # Final k-means with optimal k
  optimal_k <- optimal_k_silhouette  # Use silhouette method
  kmeans_final <- kmeans(data, centers = optimal_k, nstart = 25)
  
  clustering_results$kmeans <- list(
    model = kmeans_final,
    optimal_k = optimal_k,
    wss = wss,
    silhouette_scores = silhouette_scores,
    gap_stat = gap_stat,
    optimal_k_methods = list(
      silhouette = optimal_k_silhouette,
      elbow = optimal_k_elbow,
      gap = optimal_k_gap
    )
  )
  
  # 2. Hierarchical Clustering
  cat("Performing Hierarchical clustering...\n")
  
  dist_matrix <- dist(data)
  hclust_complete <- hclust(dist_matrix, method = "complete")
  hclust_ward <- hclust(dist_matrix, method = "ward.D2")
  hclust_average <- hclust(dist_matrix, method = "average")
  
  # Cut tree to get clusters
  hclust_clusters <- cutree(hclust_ward, k = optimal_k)
  
  clustering_results$hierarchical <- list(
    complete = hclust_complete,
    ward = hclust_ward,
    average = hclust_average,
    clusters = hclust_clusters,
    optimal_k = optimal_k
  )
  
  # 3. DBSCAN
  cat("Performing DBSCAN clustering...\n")
  
  # Determine eps parameter using k-distance plot
  kNN_dist <- kNNdist(data, k = 4)
  eps_estimate <- quantile(sort(kNN_dist), 0.95)
  
  dbscan_result <- dbscan(data, eps = eps_estimate, minPts = 4)
  
  clustering_results$dbscan <- list(
    model = dbscan_result,
    eps = eps_estimate,
    n_clusters = max(dbscan_result$cluster),
    noise_points = sum(dbscan_result$cluster == 0)
  )
  
  # 4. Gaussian Mixture Model
  cat("Performing Gaussian Mixture Model clustering...\n")
  
  # Find optimal number of components
  gmm_bic <- map_dbl(1:max_clusters, ~{
    BIC(Mclust(data, G = .x, verbose = FALSE))
  })
  
  optimal_gmm_k <- which.max(gmm_bic)
  gmm_final <- Mclust(data, G = optimal_gmm_k, verbose = FALSE)
  
  clustering_results$gmm <- list(
    model = gmm_final,
    optimal_k = optimal_gmm_k,
    bic_scores = gmm_bic
  )
  
  # 5. Affinity Propagation
  cat("Performing Affinity Propagation clustering...\n")
  
  ap_result <- tryCatch({
    apcluster(negDistMat(r = 2), data)
  }, error = function(e) {
    warning("Affinity Propagation failed, skipping...")
    NULL
  })
  
  if (!is.null(ap_result)) {
    clustering_results$affinity_propagation <- list(
      model = ap_result,
      n_clusters = length(ap_result@clusters)
    )
  }
  
  return(clustering_results)
}

#' Visualize Clustering Results
#' @param data Data matrix
#' @param clustering_results Output from perform_clustering_analysis
#' @return List of clustering visualization plots
visualize_clustering_results <- function(data, clustering_results) {
  
  plots <- list()
  
  # 1. Cluster validation metrics
  metrics_df <- data.frame(
    Method = c("Silhouette", "Elbow", "Gap Statistic"),
    Optimal_K = c(
      clustering_results$kmeans$optimal_k_methods$silhouette,
      clustering_results$kmeans$optimal_k_methods$elbow,
      clustering_results$kmeans$optimal_k_methods$gap
    )
  )
  
  plots$validation_metrics <- ggplot(metrics_df, aes(x = Method, y = Optimal_K, fill = Method)) +
    geom_col(alpha = 0.8) +
    geom_text(aes(label = Optimal_K), vjust = -0.5) +
    labs(title = "Optimal Number of Clusters by Different Methods",
         y = "Optimal K") +
    theme_minimal() +
    theme(legend.position = "none")
  
  # 2. Elbow plot
  elbow_df <- data.frame(
    k = 1:length(clustering_results$kmeans$wss),
    wss = clustering_results$kmeans$wss
  )
  
  plots$elbow <- ggplot(elbow_df, aes(x = k, y = wss)) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    geom_vline(xintercept = clustering_results$kmeans$optimal_k_methods$elbow, 
               color = "red", linetype = "dashed") +
    labs(title = "Elbow Method for Optimal K",
         x = "Number of Clusters (k)", y = "Total Within-Cluster Sum of Squares") +
    theme_minimal()
  
  # 3. Silhouette plot
  if (length(clustering_results$kmeans$silhouette_scores) > 0) {
    silhouette_df <- data.frame(
      k = 2:(length(clustering_results$kmeans$silhouette_scores) + 1),
      silhouette = clustering_results$kmeans$silhouette_scores
    )
    
    plots$silhouette <- ggplot(silhouette_df, aes(x = k, y = silhouette)) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      geom_vline(xintercept = clustering_results$kmeans$optimal_k_methods$silhouette, 
                 color = "red", linetype = "dashed") +
      labs(title = "Silhouette Analysis for Optimal K",
           x = "Number of Clusters (k)", y = "Average Silhouette Width") +
      theme_minimal()
  }
  
  # 4. Cluster scatter plots (using first two PCs)
  pca_result <- prcomp(data, scale = FALSE)  # Already scaled
  pca_data <- data.frame(pca_result$x[, 1:2])
  
  # K-means clusters
  pca_data$kmeans_cluster <- as.factor(clustering_results$kmeans$model$cluster)
  
  plots$kmeans_scatter <- ggplot(pca_data, aes(x = PC1, y = PC2, color = kmeans_cluster)) +
    geom_point(alpha = 0.7, size = 2) +
    labs(title = "K-Means Clustering Results (PCA Projection)",
         x = paste0("PC1 (", round(summary(pca_result)$importance[2, 1] * 100, 1), "%)"),
         y = paste0("PC2 (", round(summary(pca_result)$importance[2, 2] * 100, 1), "%)"),
         color = "Cluster") +
    theme_minimal()
  
  # Hierarchical clusters
  pca_data$hclust_cluster <- as.factor(clustering_results$hierarchical$clusters)
  
  plots$hclust_scatter <- ggplot(pca_data, aes(x = PC1, y = PC2, color = hclust_cluster)) +
    geom_point(alpha = 0.7, size = 2) +
    labs(title = "Hierarchical Clustering Results (PCA Projection)",
         x = paste0("PC1 (", round(summary(pca_result)$importance[2, 1] * 100, 1), "%)"),
         y = paste0("PC2 (", round(summary(pca_result)$importance[2, 2] * 100, 1), "%)"),
         color = "Cluster") +
    theme_minimal()
  
  # DBSCAN clusters
  pca_data$dbscan_cluster <- as.factor(clustering_results$dbscan$model$cluster)
  
  plots$dbscan_scatter <- ggplot(pca_data, aes(x = PC1, y = PC2, color = dbscan_cluster)) +
    geom_point(alpha = 0.7, size = 2) +
    labs(title = "DBSCAN Clustering Results (PCA Projection)",
         x = paste0("PC1 (", round(summary(pca_result)$importance[2, 1] * 100, 1), "%)"),
         y = paste0("PC2 (", round(summary(pca_result)$importance[2, 2] * 100, 1), "%)"),
         color = "Cluster") +
    theme_minimal()
  
  # 5. Dendrogram
  plots$dendrogram <- fviz_dend(clustering_results$hierarchical$ward, 
                               k = clustering_results$hierarchical$optimal_k,
                               cex = 0.5, 
                               main = "Hierarchical Clustering Dendrogram")
  
  return(plots)
}

#' ========================================
#' 3. DIMENSIONALITY REDUCTION
#' ========================================

#' Comprehensive Dimensionality Reduction Analysis
#' @param data Data matrix
#' @param n_components Number of components to retain
#' @return Dimensionality reduction results
perform_dimensionality_reduction <- function(data, n_components = 3) {
  
  dr_results <- list()
  
  # 1. Principal Component Analysis (PCA)
  cat("Performing PCA...\n")
  
  pca_result <- prcomp(data, scale = FALSE)  # Already scaled
  
  # Calculate explained variance
  explained_var <- summary(pca_result)$importance[2, ]
  cumulative_var <- summary(pca_result)$importance[3, ]
  
  dr_results$pca <- list(
    model = pca_result,
    explained_variance = explained_var,
    cumulative_variance = cumulative_var,
    loadings = pca_result$rotation,
    scores = pca_result$x[, 1:min(n_components, ncol(pca_result$x))]
  )
  
  # 2. Factor Analysis
  cat("Performing Factor Analysis...\n")
  
  # Determine optimal number of factors using parallel analysis
  fa_parallel <- fa.parallel(data, fm = "ml", fa = "fa", main = "Parallel Analysis")
  n_factors <- fa_parallel$nfact
  
  fa_result <- fa(data, nfactors = min(n_factors, n_components), rotate = "varimax")
  
  dr_results$factor_analysis <- list(
    model = fa_result,
    n_factors = n_factors,
    loadings = fa_result$loadings,
    scores = fa_result$scores
  )
  
  # 3. Independent Component Analysis (ICA)
  cat("Performing ICA...\n")
  
  ica_result <- fastICA(data, n.comp = min(n_components, ncol(data)))
  
  dr_results$ica <- list(
    model = ica_result,
    sources = ica_result$S,
    mixing_matrix = ica_result$A
  )
  
  # 4. t-SNE
  cat("Performing t-SNE...\n")
  
  set.seed(42)
  tsne_result <- Rtsne(data, dims = min(2, n_components), perplexity = min(30, nrow(data)/4), 
                      verbose = FALSE, max_iter = 500)
  
  dr_results$tsne <- list(
    model = tsne_result,
    embedding = tsne_result$Y
  )
  
  # 5. UMAP
  cat("Performing UMAP...\n")
  
  umap_config <- umap.defaults
  umap_config$n_components <- min(2, n_components)
  umap_result <- umap(data, config = umap_config)
  
  dr_results$umap <- list(
    model = umap_result,
    embedding = umap_result$layout
  )
  
  return(dr_results)
}

#' Visualize Dimensionality Reduction Results
#' @param dr_results Output from perform_dimensionality_reduction
#' @param clustering_results Optional clustering results for coloring
#' @return List of dimensionality reduction plots
visualize_dimensionality_reduction <- function(dr_results, clustering_results = NULL) {
  
  plots <- list()
  
  # 1. PCA explained variance
  pca_var_df <- data.frame(
    PC = 1:length(dr_results$pca$explained_variance),
    Explained_Variance = dr_results$pca$explained_variance,
    Cumulative_Variance = dr_results$pca$cumulative_variance
  )
  
  plots$pca_variance <- ggplot(pca_var_df, aes(x = PC)) +
    geom_col(aes(y = Explained_Variance), alpha = 0.7, fill = "steelblue") +
    geom_line(aes(y = Cumulative_Variance), color = "red", size = 1) +
    geom_point(aes(y = Cumulative_Variance), color = "red", size = 2) +
    scale_y_continuous(sec.axis = sec_axis(~., name = "Cumulative Variance")) +
    labs(title = "PCA Explained Variance",
         x = "Principal Component", y = "Explained Variance Ratio") +
    theme_minimal()
  
  # 2. PCA biplot
  if (ncol(dr_results$pca$scores) >= 2) {
    pca_df <- data.frame(dr_results$pca$scores[, 1:2])
    
    if (!is.null(clustering_results)) {
      pca_df$cluster <- as.factor(clustering_results$kmeans$model$cluster)
      color_aes <- aes(color = cluster)
    } else {
      color_aes <- NULL
    }
    
    plots$pca_biplot <- ggplot(pca_df, aes(x = PC1, y = PC2)) +
      geom_point(alpha = 0.7, size = 2, color_aes) +
      labs(title = "PCA Biplot",
           x = paste0("PC1 (", round(dr_results$pca$explained_variance[1] * 100, 1), "%)"),
           y = paste0("PC2 (", round(dr_results$pca$explained_variance[2] * 100, 1), "%)")) +
      theme_minimal()
  }
  
  # 3. t-SNE plot
  if (!is.null(dr_results$tsne$embedding)) {
    tsne_df <- data.frame(dr_results$tsne$embedding)
    names(tsne_df) <- c("tSNE1", "tSNE2")
    
    if (!is.null(clustering_results)) {
      tsne_df$cluster <- as.factor(clustering_results$kmeans$model$cluster)
      color_aes <- aes(color = cluster)
    } else {
      color_aes <- NULL
    }
    
    plots$tsne <- ggplot(tsne_df, aes(x = tSNE1, y = tSNE2)) +
      geom_point(alpha = 0.7, size = 2, color_aes) +
      labs(title = "t-SNE Visualization") +
      theme_minimal()
  }
  
  # 4. UMAP plot
  if (!is.null(dr_results$umap$embedding)) {
    umap_df <- data.frame(dr_results$umap$embedding)
    names(umap_df) <- c("UMAP1", "UMAP2")
    
    if (!is.null(clustering_results)) {
      umap_df$cluster <- as.factor(clustering_results$kmeans$model$cluster)
      color_aes <- aes(color = cluster)
    } else {
      color_aes <- NULL
    }
    
    plots$umap <- ggplot(umap_df, aes(x = UMAP1, y = UMAP2)) +
      geom_point(alpha = 0.7, size = 2, color_aes) +
      labs(title = "UMAP Visualization") +
      theme_minimal()
  }
  
  # 5. Loading plot for PCA
  if (ncol(dr_results$pca$loadings) >= 2) {
    loadings_df <- data.frame(
      Variable = rownames(dr_results$pca$loadings),
      PC1 = dr_results$pca$loadings[, 1],
      PC2 = dr_results$pca$loadings[, 2]
    )
    
    plots$pca_loadings <- ggplot(loadings_df, aes(x = PC1, y = PC2)) +
      geom_point(size = 3, alpha = 0.7) +
      geom_text(aes(label = Variable), vjust = -0.5, hjust = 0.5, size = 3) +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
      geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
      labs(title = "PCA Loadings Plot",
           x = "PC1 Loading", y = "PC2 Loading") +
      theme_minimal()
  }
  
  return(plots)
}

#' ========================================
#' 4. ASSOCIATION RULE MINING
#' ========================================

#' Association Rule Mining Analysis
#' @param data Transactional data (can be data frame or transactions object)
#' @param support Minimum support threshold
#' @param confidence Minimum confidence threshold
#' @param lift Minimum lift threshold
#' @return Association rules and analysis
perform_association_analysis <- function(data, support = 0.01, confidence = 0.5, lift = 1.2) {
  
  # Convert to transactions if necessary
  if (!is(data, "transactions")) {
    # Assume data is in basket format (each row is a transaction)
    if (is.data.frame(data)) {
      # Convert to logical matrix
      data_matrix <- as.matrix(data)
      data_matrix[is.na(data_matrix)] <- 0
      data_matrix <- (data_matrix > 0)
      data <- as(data_matrix, "transactions")
    } else {
      stop("Data must be a data frame or transactions object")
    }
  }
  
  # Generate frequent itemsets
  frequent_items <- apriori(data, parameter = list(support = support, target = "frequent itemsets"))
  
  # Generate association rules
  rules <- apriori(data, parameter = list(support = support, confidence = confidence))
  
  # Filter by lift
  rules_filtered <- subset(rules, lift >= lift)
  
  # Get rule quality measures
  rule_measures <- interestMeasure(rules_filtered, c("support", "confidence", "lift", "conviction"), 
                                  transactions = data)
  
  # Create summary
  rule_summary <- data.frame(
    LHS = labels(lhs(rules_filtered)),
    RHS = labels(rhs(rules_filtered)),
    Support = round(rule_measures$support, 4),
    Confidence = round(rule_measures$confidence, 4),
    Lift = round(rule_measures$lift, 4),
    Conviction = round(rule_measures$conviction, 4)
  )
  
  return(list(
    transactions = data,
    frequent_items = frequent_items,
    rules = rules_filtered,
    rule_summary = rule_summary,
    parameters = list(support = support, confidence = confidence, lift = lift)
  ))
}

#' ========================================
#' 5. DEMONSTRATION WITH SYNTHETIC DATA
#' ========================================

#' Generate Unsupervised Learning Demo Data
generate_unsupervised_demo_data <- function(n = 500) {
  
  set.seed(42)
  
  # Create three natural clusters
  cluster1 <- data.frame(
    x1 = rnorm(n/3, mean = 2, sd = 1),
    x2 = rnorm(n/3, mean = 2, sd = 1),
    x3 = rnorm(n/3, mean = 0, sd = 0.5),
    group = 1
  )
  
  cluster2 <- data.frame(
    x1 = rnorm(n/3, mean = -2, sd = 1),
    x2 = rnorm(n/3, mean = 3, sd = 1),
    x3 = rnorm(n/3, mean = 1, sd = 0.5),
    group = 2
  )
  
  cluster3 <- data.frame(
    x1 = rnorm(n/3 + n%%3, mean = 0, sd = 1),
    x2 = rnorm(n/3 + n%%3, mean = -2, sd = 1),
    x3 = rnorm(n/3 + n%%3, mean = -1, sd = 0.5),
    group = 3
  )
  
  # Combine clusters
  data <- rbind(cluster1, cluster2, cluster3)
  
  # Add additional variables with relationships
  data$x4 <- 0.5 * data$x1 + 0.3 * data$x2 + rnorm(n, 0, 0.2)
  data$x5 <- data$x2^2 + rnorm(n, 0, 0.5)
  
  # Add categorical variable
  data$category <- sample(c("A", "B", "C"), n, replace = TRUE, prob = c(0.4, 0.35, 0.25))
  
  # Add some noise variables
  data$noise1 <- rnorm(n, 0, 1)
  data$noise2 <- runif(n, -1, 1)
  
  return(data)
}

#' Generate Transaction Data for Association Analysis
generate_transaction_demo_data <- function(n_transactions = 1000, n_items = 20) {
  
  set.seed(42)
  
  # Create item names
  items <- paste0("Item_", LETTERS[1:n_items])
  
  # Generate transactions with varying basket sizes
  transactions <- list()
  
  for (i in 1:n_transactions) {
    # Random basket size (1 to 8 items)
    basket_size <- sample(1:8, 1, prob = c(0.1, 0.15, 0.2, 0.2, 0.15, 0.1, 0.07, 0.03))
    
    # Some items are more popular than others
    item_probs <- c(rep(0.1, 5), rep(0.05, 10), rep(0.02, 5))  # Popular, medium, rare items
    
    # Select items for this transaction
    selected_items <- sample(items, basket_size, replace = FALSE, prob = item_probs)
    transactions[[i]] <- selected_items
  }
  
  # Convert to transaction matrix
  all_items <- unique(unlist(transactions))
  transaction_matrix <- matrix(FALSE, nrow = n_transactions, ncol = length(all_items))
  colnames(transaction_matrix) <- all_items
  
  for (i in 1:n_transactions) {
    transaction_matrix[i, transactions[[i]]] <- TRUE
  }
  
  return(as.data.frame(transaction_matrix))
}

#' Run Complete Unsupervised Learning Demo
demo_unsupervised_learning <- function() {
  
  cat("=== COMPREHENSIVE UNSUPERVISED LEARNING DEMONSTRATION ===\n\n")
  
  # Generate demo data
  demo_data <- generate_unsupervised_demo_data(n = 500)
  
  cat("1. DATA PREPARATION\n")
  cat("="*50, "\n")
  
  # Prepare data (exclude true group labels for unsupervised learning)
  prepared_data <- prepare_unsupervised_data(demo_data, exclude_cols = "group", scale_data = TRUE)
  
  cat("Data shape:", nrow(prepared_data$processed_data), "x", ncol(prepared_data$processed_data), "\n")
  cat("Variables:", paste(names(prepared_data$processed_data), collapse = ", "), "\n")
  
  cat("\n2. CLUSTERING ANALYSIS\n")
  cat("="*50, "\n")
  
  # Perform clustering
  clustering_results <- perform_clustering_analysis(prepared_data$processed_data, max_clusters = 8)
  
  cat("Clustering Results Summary:\n")
  cat("- K-means optimal k:", clustering_results$kmeans$optimal_k, "\n")
  cat("- Hierarchical clusters:", clustering_results$hierarchical$optimal_k, "\n")
  cat("- DBSCAN clusters:", clustering_results$dbscan$n_clusters, "\n")
  cat("- DBSCAN noise points:", clustering_results$dbscan$noise_points, "\n")
  cat("- GMM optimal components:", clustering_results$gmm$optimal_k, "\n")
  
  if ("affinity_propagation" %in% names(clustering_results)) {
    cat("- Affinity Propagation clusters:", clustering_results$affinity_propagation$n_clusters, "\n")
  }
  
  cat("\n3. DIMENSIONALITY REDUCTION\n")
  cat("="*50, "\n")
  
  # Perform dimensionality reduction
  dr_results <- perform_dimensionality_reduction(prepared_data$processed_data, n_components = 3)
  
  cat("Dimensionality Reduction Results:\n")
  cat("- PCA explained variance (first 3 PCs):", 
      paste(round(dr_results$pca$explained_variance[1:3] * 100, 1), "%", collapse = ", "), "\n")
  cat("- PCA cumulative variance (first 3 PCs):", 
      round(dr_results$pca$cumulative_variance[3] * 100, 1), "%\n")
  cat("- Factor Analysis factors:", dr_results$factor_analysis$n_factors, "\n")
  
  cat("\n4. ASSOCIATION RULE MINING\n")
  cat("="*50, "\n")
  
  # Generate transaction data for association analysis
  transaction_data <- generate_transaction_demo_data(n_transactions = 1000, n_items = 15)
  
  # Perform association analysis
  association_results <- perform_association_analysis(transaction_data, 
                                                     support = 0.02, 
                                                     confidence = 0.3, 
                                                     lift = 1.1)
  
  cat("Association Rule Mining Results:\n")
  cat("- Number of transactions:", length(association_results$transactions), "\n")
  cat("- Number of frequent itemsets:", length(association_results$frequent_items), "\n")
  cat("- Number of association rules:", length(association_results$rules), "\n")
  
  if (nrow(association_results$rule_summary) > 0) {
    cat("Top 5 rules by lift:\n")
    top_rules <- association_results$rule_summary %>%
      arrange(desc(Lift)) %>%
      head(5)
    print(top_rules)
  }
  
  cat("\n5. CLUSTER VALIDATION\n")
  cat("="*50, "\n")
  
  # Compare with true clusters (for validation)
  true_clusters <- demo_data$group
  predicted_clusters <- clustering_results$kmeans$model$cluster
  
  # Adjusted Rand Index
  ari <- adjustedRandIndex(true_clusters, predicted_clusters)
  cat("Adjusted Rand Index (K-means vs true clusters):", round(ari, 3), "\n")
  
  # Silhouette analysis
  silhouette_avg <- mean(silhouette(predicted_clusters, dist(prepared_data$processed_data))[, 3])
  cat("Average Silhouette Width:", round(silhouette_avg, 3), "\n")
  
  cat("\nUnsupervised Learning Demo Complete!\n")
  
  return(list(
    demo_data = demo_data,
    prepared_data = prepared_data,
    clustering_results = clustering_results,
    dr_results = dr_results,
    association_results = association_results,
    validation_metrics = list(
      ari = ari,
      silhouette = silhouette_avg
    )
  ))
}

# Export key functions
unsupervised_learning_exports <- list(
  prepare_unsupervised_data = prepare_unsupervised_data,
  perform_clustering_analysis = perform_clustering_analysis,
  visualize_clustering_results = visualize_clustering_results,
  perform_dimensionality_reduction = perform_dimensionality_reduction,
  visualize_dimensionality_reduction = visualize_dimensionality_reduction,
  perform_association_analysis = perform_association_analysis,
  generate_unsupervised_demo_data = generate_unsupervised_demo_data,
  generate_transaction_demo_data = generate_transaction_demo_data,
  demo_unsupervised_learning = demo_unsupervised_learning
)

# Run demo if script is executed directly
if (sys.nframe() == 0) {
  demo_results <- demo_unsupervised_learning()
}