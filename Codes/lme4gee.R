# Load required libraries
wd<-paste("~/Library/CloudStorage/Box-Box/GWU/Research/",
          "Longitudinal Review/TeXfiles/some_visualizations/",sep="")
library(lme4)
library(gamm4)
library(robustlmm)
library(ggplot2)
library(dplyr)
library(omicsArt)
library(MASS)
library(multcomp)   # CI for linear combinations of model coef
library(geepack)    # generalized estimating equations
library(ggeffects)  # marginal effects, adjusted predictions

# Set seed for reproducibility
set.seed(42)

# Simulate longitudinal data
n_subjects <- 35  # Number of subjects
n_timepoints <- 7  # Number of timepoints
time <- seq(0, 6, length.out = n_timepoints)

# Assign treatment (70% males, 30% females)
treatment <- sample(c("Case", "Control"), n_subjects, replace = TRUE, prob = c(0.7, 0.3))

# Simulate balanced data
balanced_data <- data.frame(
  Subject = rep(1:n_subjects, each = n_timepoints),
  Time = rep(time, n_subjects),
  Treatment = rep(treatment, each = n_timepoints),
  # RandomEffect = rep(rnorm(n_subjects, 0, 1), each = n_timepoints),
  RandomEffect = mvrnorm(n=1,mu=rep(0,n_i),Sigma = covmat(t_i)),
  Residual = rnorm(n_subjects * n_timepoints, 0, 0.5)
)
balanced_data$Response <- (2 + 0.5 * balanced_data$Time +
                             ifelse(balanced_data$Treatment == "Case", 0.3, -0.3) +  # Add fixed effect of treatment
                             balanced_data$RandomEffect + balanced_data$Residual)/10
covmat <- function(t_i){
  n_i <- length(t_i)
  covmatrix <- matrix(rep(0,n_i^2),n_i,n_i)
  for (i in 1:n_i){
    for (j in 1:n_i){
      covmatrix[i,j]=exp(-abs(t_i[i]-t_i[j]))
    }
  }
  return(covmatrix)
}
imSubject<-numeric(0)
imTime<-numeric(0)
imTreatment<-numeric(0)
imRandomEffect<-numeric(0)
imResidual<-numeric(0)
for (i in 1:n_subjects){
  (n_i <- sample(c(1:n_timepoints),1))
  (subj_i <- rep(i,n_i))
  # (t_i <- runif(n_i,min=0,max=7))
  (t_i <- sample(seq(0,6),n_i,replace = F))
  (treat_i <- rep(treatment[i],n_i))
  (randEff_i <- mvrnorm(n=1,mu=rep(0,n_i),Sigma = covmat(t_i)))
  (Residual_i = rnorm(n_i, 0, 0.5))
  imSubject<-c(imSubject,subj_i)
  imTime<-c(imTime,t_i)
  imTreatment<-c(imTreatment,treat_i)
  imRandomEffect<-c(imRandomEffect,randEff_i)
  imResidual<-c(imResidual,Residual_i)
}
imbalanced_data <- data.frame(
  Subject = imSubject,
  Time = imTime,
  Treatment = imTreatment,
  RandomEffect = imRandomEffect,
  Residual = imResidual
)

imbalanced_data$Response <- (2 + 0.5 * imbalanced_data$Time +
                             ifelse(imbalanced_data$Treatment == "Case", 0.3, -0.3) +  # Add fixed effect of treatment
                               imbalanced_data$RandomEffect + imbalanced_data$Residual)/10

# Fit models
# LME model on imbalanced data
lme_imbalanced <- lmer(Response ~ Time + Treatment + (1 | Subject), data = imbalanced_data)

# GAMM model on imbalanced data
gamm_imbalanced <- gamm4(Response ~ s(Time, bs = "cs",k=3) + Treatment, random = ~(1 | Subject), data = imbalanced_data)

# Robust LME model on imbalanced data
robust_imbalanced <- rlmer(Response ~ Time + Treatment + (1 | Subject), data = imbalanced_data)

# GEE model on imbalanced data
gee_imbalanced  <- geeglm(Response ~ Time + Treatment, data = imbalanced_data,
                    id = Subject, family = gaussian, corstr = "exchangeable")

# LME model on balanced data
lme_balanced <- lmer(Response ~ Time + Treatment + (1 | Subject), data = balanced_data)

# GAMM model on balanced data
gamm_balanced <- gamm4(Response ~ s(Time, bs = "cs",k=3) + Treatment, random = ~(1 | Subject), data = balanced_data)

# Robust LME model on balanced data
robust_balanced <- rlmer(Response ~ Time + Treatment + (1 | Subject), data = balanced_data)

# GEE model on balanced data
gee_balanced  <- geeglm(Response ~ Time + Treatment, data = balanced_data,
                          id = Subject, family = gaussian, corstr = "exchangeable")

# Add predictions to data
imbalanced_data <- imbalanced_data %>%
  mutate(
    LME_Predicted = predict(lme_imbalanced, newdata = imbalanced_data),
    GAMM_Predicted = predict(gamm_imbalanced$gam, newdata = imbalanced_data),
    Robust_Predicted = predict(robust_imbalanced, newdata = imbalanced_data),
    GEE_Predicted = predict(gee_imbalanced, newdata = imbalanced_data)
  )

balanced_data <- balanced_data %>%
  mutate(
    LME_Predicted = predict(lme_balanced, newdata = balanced_data),
    GAMM_Predicted = predict(gamm_balanced$gam, newdata = balanced_data),
    Robust_Predicted = predict(robust_balanced, newdata = balanced_data),
    GEE_Predicted = predict(gee_balanced, newdata = balanced_data)
  )

# Calculate mean predicted values for LME and Robust at each time point
mean_predictions_imbalanced <- imbalanced_data %>%
  group_by(Time) %>%
  summarize(
    Mean_LME = mean(LME_Predicted, na.rm = TRUE),
    Mean_GAMM = mean(GAMM_Predicted, na.rm = TRUE),
    Mean_GEE = mean(GEE_Predicted, na.rm = TRUE),
    LME_Lower = mean(LME_Predicted, na.rm = TRUE) - 1.96 * sd(LME_Predicted, na.rm = TRUE) / sqrt(n()),
    LME_Upper = mean(LME_Predicted, na.rm = TRUE) + 1.96 * sd(LME_Predicted, na.rm = TRUE) / sqrt(n()),
    GAMM_Lower = mean(GAMM_Predicted, na.rm = TRUE) - 1.96 * sd(GAMM_Predicted, na.rm = TRUE) / sqrt(n()),
    GAMM_Upper = mean(GAMM_Predicted, na.rm = TRUE) + 1.96 * sd(GAMM_Predicted, na.rm = TRUE) / sqrt(n()),
    GEE_Lower = mean(GEE_Predicted, na.rm = TRUE) - 1.96 * sd(GEE_Predicted, na.rm = TRUE) / sqrt(n()),
    GEE_Upper = mean(GEE_Predicted, na.rm = TRUE) + 1.96 * sd(GEE_Predicted, na.rm = TRUE) / sqrt(n())
  )

mean_predictions_balanced <- balanced_data %>%
  group_by(Time) %>%
  summarize(
    Mean_LME = mean(LME_Predicted, na.rm = TRUE),
    Mean_GAMM = mean(GAMM_Predicted, na.rm = TRUE),
    Mean_GEE = mean(GEE_Predicted, na.rm = TRUE),
    LME_Lower = mean(LME_Predicted, na.rm = TRUE) - 1.96 * sd(LME_Predicted, na.rm = TRUE) / sqrt(n()),
    LME_Upper = mean(LME_Predicted, na.rm = TRUE) + 1.96 * sd(LME_Predicted, na.rm = TRUE) / sqrt(n()),
    GAMM_Lower = mean(GAMM_Predicted, na.rm = TRUE) - 1.96 * sd(GAMM_Predicted, na.rm = TRUE) / sqrt(n()),
    GAMM_Upper = mean(GAMM_Predicted, na.rm = TRUE) + 1.96 * sd(GAMM_Predicted, na.rm = TRUE) / sqrt(n()),
    GEE_Lower = mean(GEE_Predicted, na.rm = TRUE) - 1.96 * sd(GEE_Predicted, na.rm = TRUE) / sqrt(n()),
    GEE_Upper = mean(GEE_Predicted, na.rm = TRUE) + 1.96 * sd(GEE_Predicted, na.rm = TRUE) / sqrt(n())
  )

# Updated plot 
imbalanced <- ggplot(imbalanced_data, aes(x = Time)) +
  # Observed points
  geom_point(data = imbalanced_data, aes(x = Time, y = Response, color = Treatment),  
             alpha = 1, size = 0.7) +
  # Realization lines for subjects
  geom_line(aes(y = Response, group = Subject, linetype = "Subject Realizations"), 
            color = "gray", alpha = 0.4) +
  # Mean LME predicted values
  geom_line(data = mean_predictions_imbalanced, aes(x = Time, y = Mean_LME, linetype = "Mean LME"),
            color = "red", size = 0.8) +
  geom_line(data = mean_predictions_imbalanced, aes(x = Time, y = Mean_GEE, linetype = "Mean GEE"),
            color = "darkblue", size = 0.8) +
  # Confidence intervals for LME
  geom_rect(data = mean_predictions, aes(xmin = Time - 0.05, xmax = Time + 0.05,
                                         ymin = LME_Lower, ymax = LME_Upper,
                                         fill = "LME CI"),
            alpha = 0.5, inherit.aes = FALSE) +
  # Confidence intervals for GAMM
  geom_rect(data = mean_predictions, aes(xmin = Time - 0.05, xmax = Time + 0.05,
                                         ymin = GAMM_Lower, ymax = GAMM_Upper,
                                         fill = "GAMM CI"),
            alpha = 0.5, inherit.aes = FALSE) +
  # Boxplot for LME predicted values
  geom_boxplot(aes(y = LME_Predicted, group = interaction(Time, "LME"),
                   fill = "LME Boxplot"),
               width = 0.15, color = "red", alpha = 0.2,
               position = position_nudge(x = -0.15), outlier.shape = NA) +
  # Boxplot for GAMM predicted values
  geom_boxplot(aes(y = GAMM_Predicted, group = interaction(Time, "GAMM"),
                   fill = "GAMM Boxplot"),
               width = 0.15, color = "blue", alpha = 0.2,
               position = position_nudge(x = 0.15), outlier.shape = NA) +
  # Smooth lines for LME and GAMM predictions
  geom_smooth(data = imbalanced_data, 
              aes(x = Time, y = LME_Predicted, color = Treatment, linetype = "LME Trend"), 
              size = 0.8, method = "lm", se = FALSE) +
  geom_smooth(data = imbalanced_data, 
              aes(x = Time, y = GAMM_Predicted, color = Treatment, linetype = "GEE Trend"), 
              size = 0.8, method = "lm", se = FALSE) +
  # Define scales for color, fill, and linetype
  scale_color_manual(name = "Treatment", 
                     values = c("Control" = "#AA9868", "Case" = "#033C5A")) +
  scale_fill_manual(name = "CIs and Boxplots",
                    values = c("LME CI" = "orange", "GEE CI" = "purple",
                               "LME Boxplot" = "red", "GEE Boxplot" = "blue")) +
  scale_linetype_manual(name = "Line Types",
                        values = c("Subject Realizations" = "solid",
                                   "Mean LME" = "dashed", "Mean GEE" = "dashed",
                                   "LME Trend" = "dotted", "GEE Trend" = "longdash")) +
  # Labels and theme
  labs(#title = "Model Comparisons on Imbalanced Data with Subject-Specific Realizations",
    y = "log(Relative abundance+1)", x = "Time") +
  theme_minimal() +
  theme_omicsEye()+
  theme(legend.position = "right",
        legend.key.width = unit(.4, "in"), legend.key.height =unit(.2, "in"),
        legend.key.size=unit(.9,"lines")) 
ggsave(filename = paste(wd,"imbalanced.pdf",sep=""), #device = "eps", 
       imbalanced,width = 7.2, heigh=6, units = "in")



balanced <- ggplot(balanced_data, aes(x = Time)) +
  # Observed points
  geom_point(data = balanced_data, aes(x = Time, y = Response, color = Treatment),  
             alpha = 1, size = 0.7) +
  # Realization lines for subjects
  geom_line(aes(y = Response, group = Subject, linetype = "Subject Realizations"), 
            color = "gray", alpha = 0.4) +
  # Mean LME predicted values
  geom_line(data = mean_predictions_balanced, aes(x = Time, y = Mean_LME, linetype = "Mean LME"),
            color = "red", size = 0.8) +
  geom_line(data = mean_predictions_balanced, aes(x = Time, y = Mean_GEE, linetype = "Mean GEE"),
            color = "darkblue", size = 0.8) +
  # Confidence intervals for LME
  geom_rect(data = mean_predictions, aes(xmin = Time - 0.05, xmax = Time + 0.05,
                                         ymin = LME_Lower, ymax = LME_Upper,
                                         fill = "LME CI"),
            alpha = 0.5, inherit.aes = FALSE) +
  # Confidence intervals for GAMM
  geom_rect(data = mean_predictions, aes(xmin = Time - 0.05, xmax = Time + 0.05,
                                         ymin = GAMM_Lower, ymax = GAMM_Upper,
                                         fill = "GAMM CI"),
            alpha = 0.5, inherit.aes = FALSE) +
  # Boxplot for LME predicted values
  geom_boxplot(aes(y = LME_Predicted, group = interaction(Time, "LME"),
                   fill = "LME Boxplot"),
               width = 0.15, color = "red", alpha = 0.2,
               position = position_nudge(x = -0.15), outlier.shape = NA) +
  # Boxplot for GAMM predicted values
  geom_boxplot(aes(y = GAMM_Predicted, group = interaction(Time, "GAMM"),
                   fill = "GAMM Boxplot"),
               width = 0.15, color = "blue", alpha = 0.2,
               position = position_nudge(x = 0.15), outlier.shape = NA) +
  # Smooth lines for LME and GAMM predictions
  geom_smooth(data = balanced_data, 
              aes(x = Time, y = LME_Predicted, color = Treatment, linetype = "LME Trend"), 
              size = 0.8, method = "lm", se = FALSE) +
  geom_smooth(data = balanced_data, 
              aes(x = Time, y = GAMM_Predicted, color = Treatment, linetype = "GEE Trend"), 
              size = 0.8, method = "lm", se = FALSE) +
  # Define scales for color, fill, and linetype
  scale_color_manual(name = "Treatment", 
                     values = c("Control" = "#AA9868", "Case" = "#033C5A")) +
  scale_fill_manual(name = "CIs and Boxplots",
                    values = c("LME CI" = "orange", "GAMM CI" = "purple",
                               "LME Boxplot" = "red", "GAMM Boxplot" = "blue")) +
  scale_linetype_manual(name = "Line Types",
                        values = c("Subject Realizations" = "solid",
                                   "Mean LME" = "dashed", "Mean GEE" = "dashed",
                                   "LME Trend" = "dotted", "GEE Trend" = "longdash")) +
  # Labels and theme
  labs(#title = "Model Comparisons on Imbalanced Data with Subject-Specific Realizations",
    y = "log(Relative abundance+1)", x = "Time") +
  theme_minimal() +
  theme_omicsEye()+
  theme(legend.position = "right",
        legend.key.width = unit(.4, "in"), legend.key.height =unit(.2, "in"),
        legend.key.size=unit(.9,"lines")) 
ggsave(filename = paste(wd,"balanced.pdf",sep=""), #device = "eps", 
       balanced,width = 7.2, heigh=6, units = "in")       