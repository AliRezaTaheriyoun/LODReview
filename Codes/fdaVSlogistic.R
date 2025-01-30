library(fda)
library(MASS)
library(kernlab)
wd<-paste("~/Library/CloudStorage/Box-Box/GWU/Research/",
          "Longitudinal Review/TeXfiles/some_visualizations/",sep="")
set.seed(pi)
simulate_data <- function(n , J=5) {
  t <- seq(0, 1, length.out = 20)  # 50 time points
  phi1 <- function(t, j) sin(j * pi * t) + cos(j * pi * t/2)  # Basis functions
  phi2 <- function(t, j) cos(j * pi * t)  # Basis functions
  
  # Generate scores from two different distributions
  X1_scores <- matrix(rnorm(n * J, mean = 0, sd = 1), n, J)
  X2_scores <- matrix(rnorm(n * J, mean = 1, sd = 1.5), n, J)  # Shifted mean
  
  # Construct functional data
  X1 <- X2 <- matrix(0, n, length(t))
  for (j in 1:J) {
    X1 <- X1 + X1_scores[, j] %*% t(phi1(t,j))
    X2 <- X2 + X2_scores[, j] %*% t(phi2(t,j))
  }
  
  # Combine into dataset
  data <- rbind(X1, X2)
  labels <- c(rep(0, n), rep(1, n))  # Class labels
  
  return(list(data = data, labels = labels, time_points = t))
}
# Function to split data into training and test sets
split_data <- function(data, labels, train_ratio = 0.75) {
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
compute_fpc_classifier <- function(train_data, train_labels, test_data, 
                                   test_labels, J = 5) {
  # Remove constant columns
  non_constant_cols <- apply(train_data, 2, var) > 1e-10
  train_data <- train_data[, non_constant_cols, drop = FALSE]
  test_data <- test_data[, non_constant_cols, drop = FALSE]
  # PCA
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
  
  return(list(misclassification_rate=misclassification_rate,
              predicted=predicted))
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
  return(list(misclassification_rate = misclassification_rate, 
              predicted = predicted))
}
# Simulate Data
set.seed(123)
data_info <- simulate_data(n = 20)  # Generate 100 samples per class
split <- split_data(data_info$data, data_info$labels)
# Compute Misclassification Rates
bayes_classifier <- compute_fpc_classifier(split$train_data, split$train_labels, 
                                      split$test_data, split$test_labels, J = 5)
bayes_error <- bayes_classifier$misclassification_rate
bayes_predic <- bayes_classifier$predicted
logistic_classifier <- compute_logistic_regression(split$train_data, split$train_labels, 
                                              split$test_data, split$test_labels)
logistic_error <- logistic_classifier$misclassification_rate
logistic_predict <- logistic_classifier$predicted
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
test_df <- test_df[order(test_df$ID),]
test_df$Bayespred <- rep(bayes_predic,each=20)
test_df$BayesmissCLS <- ifelse(test_df$Group==test_df$Bayespred,"Correctly Classified","Misclassified")
test_df$logipred <- rep(logistic_predict,each=20)
test_df$logimissCLS <- ifelse(test_df$Group==test_df$logipred,"Correctly Classified","Misclassified")
# # Merge both for visualization
# full_df <- rbind(train_df, test_df)
# full_df$predictedGr <- ifelse(full_df$ID %in% unique(test_df$))
# Define color mapping
# color_mapping <- c("0_Train" = "gray", "1_Train" = "violet", 
#                    "0_Test" = "gray", "1_Test" = "violet")
# alpha_mapping <- c("0_Train" = 0.2, "1_Train" = 0.2, 
#                    "0_Test" = 0.5, "1_Test" = 0.5)
# color_mapping <- c("Correctly Classified" = "#033C5A", "Misclassified" = "#AA9868")
# set.seed(pi)
Bayesplot <- ggplot(data = train_df, aes(x = Time, y = Value, group = ID)) +
  # ggplot(data = train_df[train_df$ID %in% sample(unique(train_df$ID), 32, replace = F), ], 
  #                   aes(x = Time, y = Value, group = ID)) +
  geom_line(aes(color = "Training Sample"), linewidth = 0.3, alpha = 0.25) +  # Map color aesthetic
  geom_line(data = test_df, aes(x = Time, y = Value, group = ID, color = BayesmissCLS), 
            linewidth = 0.4, alpha = 0.3) +
  scale_color_manual(values = c("Training Sample" = "gray", 
                                "Correctly Classified" = "#033C5A", 
                                "Misclassified" = "red")) +
  labs(x = "Time", y = "(Relative) Abundance", color = "Legend") +  # Add legend title
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = .2),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "in"),
    legend.position = "right",
    legend.box = "vertical", 
    legend.direction = "vertical",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    # legend.title = element_text(size = 9),
    legend.spacing.y = unit(0, "lines")
  ) +
  guides(color = guide_legend(ncol = 1))

# set.seed(pi)
logiplot <- ggplot(data = train_df, aes(x = Time, y = Value, group = ID)) +
  # ggplot(data = train_df[train_df$ID %in% sample(unique(train_df$ID), 32, replace = F), ], 
  #        aes(x = Time, y = Value, group = ID)) +
  geom_line(aes(color = "Training Sample"), linewidth = 0.3, alpha = 0.25) +  # Map color aesthetic
  geom_line(data = test_df, aes(x = Time, y = Value, group = ID, color = logimissCLS), 
            linewidth = 0.4, alpha = 0.3) +
  scale_color_manual(values = c("Training Sample" = "gray", 
                                "Correctly Classified" = "#033C5A", 
                                "Misclassified" = "red")) +
  labs(x = "Time", y = "(Relative) Abundance", color = "Legend") +  # Add legend title
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = .2),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text.x = element_text(size = 8),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "in"),
    legend.position = "none"
    # legend.text = element_text(size = 8),
    # legend.title = element_text(size = 9)
  ) +
  guides(color = guide_legend(ncol = 1))
library(cowplot)
# legend <- cowplot::get_plot_component(
#   Bayesplot #+
#     # theme(legend.position = "right"),'guide-box-right',return_all = TRUE  # Ensure legend exists
# )
legend <- get_legend(Bayesplot)
Classification <- plot_grid(
  logiplot+theme(legend.position = "none"),# + theme(plot.margin = margin(0, .1, .1, .1)),
  Bayesplot+theme(legend.position = "none"),
  legend,#+
    # theme(
    #   # axis.title.y = element_blank(),
    #   # axis.text.y = element_blank(),
    #   # axis.ticks.y = element_blank(),
    #   # legend.position = c(0.9, 0.85),
    #   legend.text=element_text(size=8),
    #   legend.title = element_blank()),
  labels = c("a", "b"
             # , "c", "d", "e" , "f", "g", "h", "i", "j",
             # "k", "l", "m", "n", "o", "p", "q", "r", "s", "t",
             # "u", "v","w","x","y","z"
             ),  # Labels for each plot
  label_size = 11,                         # Font size for labels
  label_fontface = "bold",                 # Boldface for labels
  ncol = 3, # Adjust layout
  rel_widths = c(1, 1,.5) # Adjust legend width
)
Classification
ggsave(
  filename = paste(wd, "Classification.pdf", sep = ""), #device = "eps",
  plot = Classification,
  width = 6.2, height = 2.5, units = "in"
)
