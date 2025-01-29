library(MASS)  # For multivariate normal distribution
library(class) # For k-nearest neighbors (kNN) for density estimation
library(pROC)  # For ROC analysis
set.seed(pi)
n <- 200  # Total number of observations
p <- 50   # Number of time points
time <- seq(0, 1, length.out = p)  # Time points
# Generate two classes functionals
group1_mean <- sin(2 * pi * time)  # Mean function for Group 1
group2_mean <- cos(2 * pi * time)  # Mean function for Group 2
# Simulate from Group 1
n1 <- n / 2
group1_data <- t(replicate(n1, group1_mean + rnorm(p, mean = 0, sd = 0.5)))
# Simulate from Group 2
n2 <- n / 2
group2_data <- t(replicate(n2, group2_mean + rnorm(p, mean = 0, sd = 0.5)))
# Combine data 
data <- rbind(group1_data, group2_data)
labels <- factor(c(rep(0, n1), rep(1, n2)))
# Split data into training (80%) and test (20%) sets
train_index <- sample(1:n, size = round(0.8 * n), replace = FALSE)
train_data <- data[train_index, ]
train_labels <- labels[train_index]
test_data <- data[-train_index, ]
test_labels <- labels[-train_index]

######################## 
######################## 
# Functional Bayes Classifier based the steps 
# intruduced in the article bellow:
#   Dai et al (2017) Optimal Bayes classifiers for functional data and density 
# ratios_Biometrika
######################## 
######################## 
# Step 1: Project data onto principal components
pca <- prcomp(train_data, scale = TRUE)
train_scores <- pca$x[, 1:5]  # Use first 5 principal components
test_scores <- predict(pca, test_data)[, 1:5]

# Step 2: Estimate densities for each group
group0_scores <- train_scores[train_labels == 0, ]
group1_scores <- train_scores[train_labels == 1, ]
# Kernel density estimation for each group
density0 <- density(group0_scores[, 1])  # Density for Group 0 (first PC)
density1 <- density(group1_scores[, 1])  # Density for Group 1 (first PC)

# Step 3: Classify test data using density ratios
bayes_predictions <- sapply(test_scores[, 1], function(x) {
  # Interpolate densities for Group 0 and Group 1
  prob0 <- approx(density0$x, density0$y, x, rule = 2)$y  # rule = 2: extrapolate to 0 outside range
  prob1 <- approx(density1$x, density1$y, x, rule = 2)$y  # rule = 2: extrapolate to 0 outside range
  
  # Handle cases where densities are 0 or NA
  if (is.na(prob0) || is.na(prob1) || prob0 == 0 || prob1 == 0) {
    return(ifelse(mean(train_labels == 1) > 0.5, 1, 0))  # Default to majority class
  } else {
    return(ifelse(prob1 / prob0 > 1, 1, 0))  # Classify based on density ratio
  }
})
# Misclassification rate for Bayes classifier
bayes_misclassification <- mean(bayes_predictions != test_labels)
# Bayes risk for Bayes classifier
bayes_risk <- bayes_misclassification

######################## 
######################## 
# Simple Logistic Regression
######################## 
########################
# Fit logistic regression model
logistic_model <- glm(train_labels ~ ., data = as.data.frame(train_scores), family = binomial)
# Predict on test set
logistic_predictions <- predict(logistic_model, newdata = as.data.frame(test_scores), type = "response")
logistic_predictions <- ifelse(logistic_predictions > 0.5, 1, 0)
# Misclassification rate for logistic regression
logistic_misclassification <- mean(logistic_predictions != test_labels)

# Bayes risk for logistic regression
logistic_risk <- logistic_misclassification

# Print results
cat("Bayes Classifier:\n")
cat("Misclassification Rate:", bayes_misclassification, "\n")
cat("Bayes Risk:", bayes_risk, "\n\n")

cat("Logistic Regression:\n")
cat("Misclassification Rate:", logistic_misclassification, "\n")
cat("Bayes Risk:", logistic_risk, "\n")










######################################
library(fda)
library(MASS)
library(kernlab)

# Function to generate simulated functional data
simulate_data <- function(n, J = 5) {
  t <- seq(0, 1, length.out = 50)  # 50 time points
  phi <- function(j, t) sqrt(2) * sin(j * pi * t)  # Basis functions
  
  # Generate scores from two different distributions
  X1_scores <- matrix(rnorm(n * J, mean = 0, sd = 1), n, J)
  X2_scores <- matrix(rnorm(n * J, mean = 1, sd = 1.5), n, J)  # Shifted mean
  
  # Construct functional data
  X1 <- X2 <- matrix(0, n, length(t))
  for (j in 1:J) {
    X1 <- X1 + X1_scores[, j] %*% t(phi(j, t))
    X2 <- X2 + X2_scores[, j] %*% t(phi(j, t))
  }
  
  # Combine into dataset
  data <- rbind(X1, X2)
  labels <- c(rep(0, n), rep(1, n))  # Class labels
  
  return(list(data = data, labels = labels, time_points = t))
}

# Function to split data into training and test sets
split_data <- function(data, labels, train_ratio = 0.8) {
  set.seed(42)
  n <- length(labels)
  train_idx <- sample(1:n, size = floor(train_ratio * n), replace = FALSE)
  
  train_data <- data[train_idx, ]
  test_data <- data[-train_idx, ]
  train_labels <- labels[train_idx]
  test_labels <- labels[-train_idx]
  
  return(list(train_data = train_data, test_data = test_data, 
              train_labels = train_labels, test_labels = test_labels))
}

# Function to estimate Bayes classifier based on functional principal components (FPCs)
compute_fpc_classifier <- function(train_data, train_labels, test_data, test_labels, J = 5) {
  # Remove constant columns
  non_constant_cols <- apply(train_data, 2, var) > 1e-10
  train_data <- train_data[, non_constant_cols, drop = FALSE]
  test_data <- test_data[, non_constant_cols, drop = FALSE]
  
  # Compute PCA
  pca_train <- prcomp(train_data, center = TRUE, scale. = TRUE)
  scores_train <- pca_train$x[, 1:J, drop = FALSE]
  scores_test <- predict(pca_train, newdata = test_data)[, 1:J, drop = FALSE]
  
  # Estimate density for each class (avoid zero densities)
  f1 <- lapply(1:J, function(j) density(scores_train[train_labels == 1, j], adjust = 1.5))
  f0 <- lapply(1:J, function(j) density(scores_train[train_labels == 0, j], adjust = 1.5))
  
  # Compute density ratios safely
  density_ratio <- function(x, f1, f0) {
    ratios <- sapply(1:J, function(j) {
      d1 <- approx(f1[[j]]$x, f1[[j]]$y + 1e-6, xout = x[j], rule = 2)$y  # Small smoothing
      d0 <- approx(f0[[j]]$x, f0[[j]]$y + 1e-6, xout = x[j], rule = 2)$y
      return(ifelse(d0 > 0, d1 / d0, 1))  # Avoid division by zero
    })
    return(prod(ratios, na.rm = TRUE))  # Ignore NA values
  }
  
  # Classify based on density ratio
  predicted <- sapply(1:nrow(scores_test), function(i) {
    ifelse(density_ratio(scores_test[i, ], f1, f0) > 1, 1, 0)
  })
  
  # Compute misclassification rate
  misclassification_rate <- mean(predicted != test_labels, na.rm = TRUE)
  
  return(misclassification_rate)
}



# Function to perform logistic regression classification
compute_logistic_regression <- function(train_data, train_labels, test_data, test_labels) {
  # Remove constant columns
  non_constant_cols <- apply(train_data, 2, var) > 1e-10
  train_data <- train_data[, non_constant_cols, drop = FALSE]
  test_data <- test_data[, non_constant_cols, drop = FALSE]
  
  # Compute PCA
  pca_train <- prcomp(train_data, center = TRUE, scale. = TRUE)
  scores_train <- pca_train$x[, 1:5, drop = FALSE]
  scores_test <- predict(pca_train, newdata = test_data)[, 1:5, drop = FALSE]
  
  # Fit logistic regression
  model <- glm(train_labels ~ ., data = data.frame(scores_train, train_labels), family = binomial)
  
  # Predict on test data
  probs <- predict(model, newdata = data.frame(scores_test), type = "response")
  predicted <- ifelse(probs > 0.5, 1, 0)
  
  # Compute misclassification rate
  misclassification_rate <- mean(predicted != test_labels, na.rm = TRUE)
  
  return(misclassification_rate)
}


# Simulate Data
set.seed(123)
data_info <- simulate_data(n = 20)  # Generate 100 samples per class
split <- split_data(data_info$data, data_info$labels)

# Compute Misclassification Rates
bayes_error <- compute_fpc_classifier(split$train_data, split$train_labels, 
                                      split$test_data, split$test_labels, J = 5)

logistic_error <- compute_logistic_regression(split$train_data, split$train_labels, 
                                              split$test_data, split$test_labels)

cat("Misclassification Rate (Bayes Classifier):", bayes_error, "\n")
cat("Misclassification Rate (Logistic Regression):", logistic_error, "\n")

# Compute Bayes Risk (Approximated)
bayes_risk <- min(mean(split$test_labels), 1 - mean(split$test_labels))

# Print Results
cat("Misclassification Rate (Bayes Classifier):", bayes_error, "\n")
cat("Misclassification Rate (Logistic Regression):", logistic_error, "\n")
cat("Approximate Bayes Risk:", bayes_risk, "\n")


library(ggplot2)
library(reshape2)

# Convert matrix data to long format for ggplot2
reshape_functional_data <- function(data_matrix, labels, set_type) {
  df_long <- melt(data_matrix)
  colnames(df_long) <- c("ID", "Time", "Value")
  df_long$Group <- factor(rep(labels, times = ncol(data_matrix)))  # Assign class labels
  df_long$Set <- set_type  # Training or Test set
  return(df_long)
}

# Prepare training and test sets
train_df <- reshape_functional_data(split$train_data, split$train_labels, "Train")
test_df  <- reshape_functional_data(split$test_data, split$test_labels, "Test")

# Merge both for visualization
full_df <- rbind(train_df, test_df)

# Define color mapping
color_mapping <- c("0_Train" = "gray", "1_Train" = "violet", 
                   "0_Test" = "gray", "1_Test" = "violet")

alpha_mapping <- c("0_Train" = 0.2, "1_Train" = 0.2, 
                   "0_Test" = 0.8, "1_Test" = 0.8)

# Combine group & set info for aesthetics
full_df$Group_Set <- paste(full_df$Group, full_df$Set, sep = "_")

# ggplot visualization
ggplot(full_df, aes(x = Time, y = Value, group = ID, color = Group_Set, alpha = Group_Set)) +
  geom_line(size = 0.5) +
  scale_color_manual(values = color_mapping) +
  scale_alpha_manual(values = alpha_mapping) +
  theme_minimal() +
  labs(title = "Functional Data Visualization (Train vs Test Set)",
       x = "Time", y = "Functional Value",
       color = "Group & Set", alpha = "Group & Set")
