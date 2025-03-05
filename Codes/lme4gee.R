# Load required libraries

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
library(cowplot)
wd<-paste("~/Library/CloudStorage/Box-Box/GWU/Research/",
          "Longitudinal Review/TeXfiles/some_visualizations/",sep="")
covmat <- function(t_i){
  n_i <- length(t_i)
  covmatrix <- matrix(rep(0,n_i^2),n_i,n_i)
  for (i in 1:n_i){
    for (j in 1:n_i){
      # covmatrix[i,j]=exp(-abs(t_i[i]-t_i[j]))
      # covmatrix[i,j]=3*exp(-(t_i[i]-t_i[j])^2)
      covmatrix[i,j]=(0.3)^(abs(t_i[i]-t_i[j]))
    }
  }
  return(covmatrix)
}
# Set seed for reproducibility

# Simulate longitudinal data
n_subjects <- 100  # Number of subjects
n_timepoints <- 7  # Number of timepoints
time <- seq(0, 6, length.out = n_timepoints)
set.seed(pi)
# Assign treatment (70% males, 30% females)
treatment <- sample(c("Case", "Control"), n_subjects, replace = TRUE, prob = c(0.7, 0.3))
# Simulate balanced data
complete.data <- data.frame(
  Subject = rep(1:n_subjects, each = n_timepoints),
  Time = rep(time, n_subjects),
  Treatment = rep(treatment, each = n_timepoints),
  X1 = rnorm(n_subjects * n_timepoints, mean = 3, sd = 3),
  # RandomEffect = rep(rnorm(n_subjects, 0, 1), each = n_timepoints),
  RandomEffect1 = c(t(mvrnorm(n=n_subjects,mu=rep(0,n_timepoints),Sigma = covmat(time)))),
  RandomEffect2 = c(t(mvrnorm(n=n_subjects,mu=rep(0,n_timepoints),Sigma = covmat(time)))),
  Residual = rnorm(n_subjects * n_timepoints, 0, .1)
)
complete.data$Response <- (-2 + 0.5 * complete.data$Time +
                             ifelse(complete.data$Treatment == "Case", 0.4, -0.4) +  # Add fixed effect of treatment
                             0.3 * complete.data$X1 +  # Effect of the new fixed effect
                             complete.data$RandomEffect1+
                             complete.data$RandomEffect2*complete.data$Time +
                             complete.data$Residual)/1000-.02
Subject_balance <- sample(rep(1:n_subjects),20,replace = F)
balanced_data <- complete.data[complete.data$Subject %in% Subject_balance,]
rownames_imbalance <- sample(rownames(complete.data),nrow(balanced_data),replace = F)
imbalanced_data <- complete.data[rownames(complete.data) %in% rownames_imbalance,]

#####Another way to make imbalanced data. This method also gurantees the 
# imbalancedness in the sample size at each time point. 
# Define the number of observations you want at each time point
# For example, let's assume we have 7 time points and want fewer observations at the first and last time points
n_obs_per_time <- floor(c(25, 15, 15, 10, 10, 20, 5)*nrow(balanced_data)/100)  # Adjust these numbers as needed
n_obs_per_time[n_timepoints] <- nrow(balanced_data)-sum(n_obs_per_time[-n_timepoints])
# Initialize an empty list to store sampled rows
sampled_rows <- list()
# Sample rows for each time point
for (i in 1:length(time)) {
  # Extract rows corresponding to the current time point
  time_point_data <- complete.data[complete.data$Time == time[i], ]
  
  # Sample the desired number of rows for this time point
  # Ensure you don't sample more rows than are available
  n_samples <- min(n_obs_per_time[i], nrow(time_point_data))
  sampled_rows[[i]] <- time_point_data[sample(nrow(time_point_data), n_samples), ]
}

# Combine the sampled rows into a single data frame
imbalanced_data <- do.call(rbind, sampled_rows)

#####

# imSubject<-numeric(0)
# imTime<-numeric(0)
# imTreatment<-numeric(0)
# imRandomEffect<-numeric(0)
# imResidual<-numeric(0)
# for (i in 1:n_subjects){
#   (n_i <- sample(c(1:n_timepoints),1))
#   (subj_i <- rep(i,n_i))
#   # (t_i <- runif(n_i,min=0,max=7))
#   (t_i <- sample(seq(0,6),n_i,replace = F))
#   (treat_i <- rep(treatment[i],n_i))
#   (randEff_i <- mvrnorm(n=1,mu=rep(0,n_i),Sigma = covmat(t_i)))
#   (Residual_i = rnorm(n_i, 0, 0.5))
#   imSubject<-c(imSubject,subj_i)
#   imTime<-c(imTime,t_i)
#   imTreatment<-c(imTreatment,treat_i)
#   imRandomEffect<-c(imRandomEffect,randEff_i)
#   imResidual<-c(imResidual,Residual_i)
# }
# imbalanced_data <- data.frame(
#   Subject = imSubject,
#   Time = imTime,
#   Treatment = imTreatment,
#   RandomEffect = imRandomEffect,
#   Residual = imResidual
# )
# 
# imbalanced_data$Response <- (2 + 0.5 * imbalanced_data$Time +
#                              ifelse(imbalanced_data$Treatment == "Case", 0.3, -0.3) +  # Add fixed effect of treatment
#                                imbalanced_data$RandomEffect + imbalanced_data$Residual)/10

# Fit models
# LME model on imbalanced data
lme_imbalanced <- lmer(Response ~ Time + Treatment + X1 +(1 | Subject), data = imbalanced_data)

# GAMM model on imbalanced data
gamm_imbalanced <- gamm4(Response ~ s(Time, bs = "cs",k=3) + Treatment +X1, random = ~(1 | Subject), data = imbalanced_data)

# Robust LME model on imbalanced data
robust_imbalanced <- rlmer(Response ~ Time + Treatment + X1 + (1 | Subject), data = imbalanced_data)

# GEE model on imbalanced data
gee_imbalanced  <- geeglm(Response ~ Time + Treatment + X1, data = imbalanced_data,
                    id = Subject, family = gaussian, corstr = "ar1")

# LME model on balanced data
lme_balanced <- lmer(Response ~ Time + Treatment + X1 + (1 | Subject), data = balanced_data)

# GAMM model on balanced data
gamm_balanced <- gamm4(Response ~ s(Time, bs = "cs",k=3) + Treatment + X1, random = ~(1 | Subject), data = balanced_data)

# Robust LME model on balanced data
robust_balanced <- rlmer(Response ~ Time + Treatment + X1 + (1 | Subject), data = balanced_data)

# GEE model on balanced data
gee_balanced  <- geeglm(Response ~ Time + Treatment + X1, data = balanced_data,
                          id = Subject, family = gaussian, corstr = "ar1")

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
            color = "gray", alpha = 0.2) +
  # Mean LME predicted values
  geom_line(data = mean_predictions_imbalanced, aes(x = Time, y = Mean_LME, linetype = "Mean LME"),
            color = "red", size = 0.6) +
  geom_line(data = mean_predictions_imbalanced, aes(x = Time, y = Mean_GEE, linetype = "Mean GEE"),
            color = "darkblue", size = 0.6) +
  # Confidence intervals for LME
  geom_rect(data = mean_predictions_imbalanced, aes(xmin = Time - 0.05, xmax = Time + 0.05,
                                                    ymin = LME_Lower, ymax = LME_Upper,
                                                    fill = "LME CI"),
            alpha = 0.5, inherit.aes = FALSE) +
  # Confidence intervals for GAMM
  geom_rect(data = mean_predictions_imbalanced, aes(xmin = Time - 0.05, xmax = Time + 0.05,
                                                    ymin = GEE_Lower, ymax = GEE_Upper,
                                                    fill = "GEE CI"),
            alpha = 0.5, inherit.aes = FALSE) +
  # Boxplot for LME predicted values
  geom_boxplot(aes(y = LME_Predicted, group = interaction(Time, "LME"),
                   fill = "LME Boxplot"),
               width = 0.15, color = "red", alpha = 0.2,lwd=.2,
               position = position_nudge(x = -0.15), outlier.shape = NA) +
  # Boxplot for GAMM predicted values
  geom_boxplot(aes(y = GEE_Predicted, group = interaction(Time, "GEE"),
                   fill = "GEE Boxplot"),
               width = 0.15, color = "blue", alpha = 0.2,lwd=.2,
               position = position_nudge(x = 0.15), outlier.shape = NA) +
  # Smooth lines for LME and GAMM predictions
  geom_smooth(data = imbalanced_data, 
              aes(x = Time, y = LME_Predicted, color = Treatment, linetype = "LME Trend"), 
              size = 0.8, method = "lm", se = FALSE) +
  geom_smooth(data = imbalanced_data, 
              aes(x = Time, y = GEE_Predicted, color = Treatment, linetype = "GEE Trend"), 
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
  # theme_omicsEye() +
  theme(axis.line = element_line(colour = "black", size=.25, linetype = 1),
        legend.position = "right",
        legend.text = element_text(size=7),
        axis.title.x=element_text(size = 9),
        axis.text.x=element_text(size = 6),
        axis.title.y=element_text(size = 9),
        axis.text.y=element_text(size = 6),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        # legend.key.width = unit(.4, "in"), 
        # legend.key.height = unit(.2, "in"),
        # legend.key.size = unit(.9, "lines"),
        legend.box = "vertical",  # Optional to adjust box around legend
        legend.box.margin = margin(0, 0, 0, 10),  # Optional to tweak margin if necessary
        legend.direction = "vertical",  # Makes legend go vertically
        legend.box.spacing = unit(0, "lines")) +  # Space between items in legend
  guides(fill = guide_legend(ncol = 2), color = guide_legend(ncol = 2), linetype = guide_legend(ncol = 2))

# ggsave(filename = paste(wd,"imbalanced.pdf",sep=""), #device = "eps", 
#        imbalanced,width = 7.2, heigh=6, units = "in")



balanced <- ggplot(balanced_data, aes(x = Time)) +
  # Observed points
  geom_point(data = balanced_data, aes(x = Time, y = Response, color = Treatment),  
             alpha = 1, size = 0.7) +
  # Realization lines for subjects
  geom_line(aes(y = Response, group = Subject, linetype = "Subject Realizations"), 
            color = "gray", alpha = 0.2) +
  # Mean LME predicted values
  geom_line(data = mean_predictions_balanced, aes(x = Time, y = Mean_LME, linetype = "Mean LME"),
            color = "red", size = 0.6) +
  geom_line(data = mean_predictions_balanced, aes(x = Time, y = Mean_GEE, linetype = "Mean GEE"),
            color = "darkblue", size = 0.3) +
  # Confidence intervals for LME
  geom_rect(data = mean_predictions_balanced, aes(xmin = Time - 0.05, xmax = Time + 0.05,
                                         ymin = LME_Lower, ymax = LME_Upper,
                                         fill = "LME CI"),
            alpha = 0.5, inherit.aes = FALSE) +
  # Confidence intervals for GAMM
  geom_rect(data = mean_predictions_balanced, aes(xmin = Time - 0.05, xmax = Time + 0.05,
                                         ymin = GEE_Lower, ymax = GEE_Upper,
                                         fill = "GEE CI"),
            alpha = 0.5, inherit.aes = FALSE) +
  # Boxplot for LME predicted values
  geom_boxplot(data = balanced_data,aes(y = LME_Predicted, group = interaction(Time, "LME"),
                   fill = "LME Boxplot"),
               width = 0.15, color = "red", alpha = 0.2,lwd=.2,
               position = position_nudge(x = -0.15), outlier.shape = NA) +
  # Boxplot for GAMM predicted values
  geom_boxplot(data = balanced_data,aes(y = GEE_Predicted, group = interaction(Time, "GEE"),
                   fill = "GEE Boxplot"),
               width = 0.15, color = "blue", alpha = 0.2,lwd=.2,
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
  # theme_omicsEye()+
  theme(axis.line = element_line(colour = "black", size=.25, linetype = 1),
        legend.text = element_text(size=7),
        axis.title.x=element_text(size = 9),
        axis.text.x=element_text(size = 6),
        axis.title.y=element_text(size = 9),
        axis.text.y=element_text(size = 6),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",  # Optional to adjust box around legend
        legend.box.margin = margin(0, 0, 0, 0),  # Optional to tweak margin if necessary
        legend.direction = "horizontal",  # Makes legend go vertically
        legend.key.width = unit(.3, "in"), 
        legend.key.height =unit(.1, "in"),
        legend.key.size=unit(0,"lines"),
        legend.title = element_blank()
  )+
  # Arrange legend items in two rows
  guides(
    color = guide_legend(nrow = 2, order = 1),  # Arrange Treatment legend in 2 columns
    fill = guide_legend(nrow = 2, order = 2),   # Arrange CIs and Boxplots legend in 2 columns
    linetype = guide_legend(nrow = 2, order = 3) # Arrange Line Types legend in 2 columns
  )
# ggsave(filename = paste(wd,"balanced.pdf",sep=""), #device = "eps", 
#        balanced,width = 7.2, heigh=6, units = "in")   
shared_legend <- get_plot_component(balanced,'guide-box-bottom',return_all = TRUE)
balimbal_nolegend <- plot_grid(
  balanced  +theme(legend.position ="none")+
    xlab("Time")+
    ylab("log(Relative Abundance +1)")+ylim(c(-.03,-.01)),
  imbalanced +theme(legend.position ="none")+
    # theme(legend.position = c(-.2,.02),
    #                 legend.box = "horizontal",
    #                 legend.direction = "horizontal",
    #                 legend.text = element_text(size = 7),
    #                 legend.title=element_blank(),
    #                 legend.spacing.y = unit(0, "lines"))+
    # guides(fill = guide_legend(ncol = 2), color = guide_legend(ncol = 2), 
    #        linetype = guide_legend(ncol = 2))+
    xlab("Time")+
    ylab("log(Relative Abundance +1)")+ylim(c(-.03,-.01))+guides(fill=guide_legend(ncol=2)),#+
    # theme(legend.position = "right",
    #       # legend.key.width = unit(.4, "in"), 
    #       # legend.key.height = unit(.2, "in"),
    #       # legend.key.size = unit(.9, "lines"),
    #       legend.box = "vertical",  # Optional to adjust box around legend
    #       legend.box.margin = margin(0, 0, 0, 10),  # Optional to tweak margin if necessary
    #       legend.direction = "vertical",  # Makes legend go vertically
    #       legend.box.spacing = unit(0, "lines")) +  # Space between items in legend
    # guides(fill = guide_legend(ncol = 2), color = guide_legend(ncol = 2), 
    #        linetype = guide_legend(ncol = 2)),
  labels = c("a", "b", "c", "d", "e" , "f", "g", "h", "i", "j",
             "k", "l", "m", "n", "o", "p", "q", "r", "s", "t",
             "u", "v","w","x","y","z"),  # Labels for each plot
  label_size = 11,                         # Font size for labels
  label_fontface = "bold",                 # Boldface for labels
  ncol = 2                                 # Arrange the plots in 2 columns
)

balimbal <- plot_grid(
  balimbal_nolegend,
  shared_legend,
  ncol = 1,              # Arrange plots and legend in a single column
  rel_heights = c(1, 0.08) # Adjust the relative heights (plots take 90% of space, legend takes 10%)
)
ggsave(filename = paste(wd,"balimbal.pdf",sep=""), #device = "eps",
       balimbal,width = 7.2, heigh=4.5, units = "in")
