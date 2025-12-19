suppressPackageStartupMessages({
  library(lme4)
  library(geepack)
  library(MASS)
  library(dplyr)
})

covmat <- function(t_i){
  n_i <- length(t_i)
  covmatrix <- matrix(0, n_i, n_i)
  for (i in 1:n_i){
    for (j in 1:n_i){
      covmatrix[i,j] <- (0.3)^(abs(t_i[i]-t_i[j]))
    }
  }
  covmatrix
}

get_wald <- function(est, se, level = 0.95) {
  z <- qnorm(1 - (1 - level)/2)
  c(lower = est - z*se, upper = est + z*se)
}

simulate_one <- function(seed = NULL,
                         n_subjects = 120,
                         n_timepoints = 7,
                         n_balanced_subjects = 25,
                         # MUCH more imbalanced across time
                         imbalance_pct = c(1, 4, 10, 30, 30, 20, 5),
                         corstr = "ar1",
                         # Bigger true effects (and no /1000 shrink)
                         beta0 = -2.0,
                         beta_time = 1.5,
                         beta_trt  = 2.0,
                         beta_x1   = 1.0,
                         # Stronger random effects + noise
                         re1_scale = 1.0,
                         re2_scale = 1.2,
                         resid_sd  = 0.6) {
  
  if (!is.null(seed)) set.seed(seed)
  
  time <- seq(0, 6, length.out = n_timepoints)
  
  treatment <- sample(c("Case", "Control"), n_subjects, replace = TRUE, prob = c(0.7, 0.3))
  
  complete.data <- data.frame(
    Subject = rep(1:n_subjects, each = n_timepoints),
    Time = rep(time, n_subjects),
    Treatment = rep(treatment, each = n_timepoints),
    X1 = rnorm(n_subjects * n_timepoints, mean = 0, sd = 1),
    RandomEffect1 = re1_scale * c(t(mvrnorm(n = n_subjects,
                                            mu = rep(0, n_timepoints),
                                            Sigma = covmat(time)))),
    RandomEffect2 = re2_scale * c(t(mvrnorm(n = n_subjects,
                                            mu = rep(0, n_timepoints),
                                            Sigma = covmat(time)))),
    Residual = rnorm(n_subjects * n_timepoints, 0, resid_sd)
  )
  
  # Big signal; NO division by 1000
  complete.data$Response <- beta0 +
    beta_time * complete.data$Time +
    ifelse(complete.data$Treatment == "Case", beta_trt, -beta_trt) +
    beta_x1 * complete.data$X1 +
    complete.data$RandomEffect1 +
    complete.data$RandomEffect2 * complete.data$Time +
    complete.data$Residual
  
  # True parameter on Response scale
  beta_time_true <- beta_time
  
  # Balanced: pick subjects, keep all time points
  Subject_balance <- sample(1:n_subjects, n_balanced_subjects, replace = FALSE)
  balanced_data <- complete.data[complete.data$Subject %in% Subject_balance, , drop = FALSE]
  
  # Imbalanced: same total N but extreme time-wise imbalance
  N_bal <- nrow(balanced_data)
  n_obs_per_time <- floor(imbalance_pct * N_bal / 100)
  n_obs_per_time[n_timepoints] <- N_bal - sum(n_obs_per_time[-n_timepoints])
  
  sampled_rows <- vector("list", length(time))
  for (i in seq_along(time)) {
    tp <- time[i]
    time_point_data <- complete.data[complete.data$Time == tp, , drop = FALSE]
    n_samples <- min(n_obs_per_time[i], nrow(time_point_data))
    sampled_rows[[i]] <- time_point_data[sample(nrow(time_point_data), n_samples), , drop = FALSE]
  }
  imbalanced_data <- do.call(rbind, sampled_rows)
  
  fit_and_extract <- function(dat) {
    out <- list(
      lmm_est = NA_real_, lmm_se = NA_real_, lmm_ci = c(NA_real_, NA_real_),
      gee_est = NA_real_, gee_se = NA_real_, gee_ci = c(NA_real_, NA_real_)
    )
    
    # LMM
    lmm <- tryCatch(
      lmer(Response ~ Time + Treatment + X1 + (1 | Subject), data = dat, REML = FALSE),
      error = function(e) NULL
    )
    if (!is.null(lmm)) {
      cf <- summary(lmm)$coefficients
      if ("Time" %in% rownames(cf)) {
        out$lmm_est <- cf["Time","Estimate"]
        out$lmm_se  <- cf["Time","Std. Error"]
        out$lmm_ci  <- get_wald(out$lmm_est, out$lmm_se)
      }
    }
    
    # GEE
    gee <- tryCatch(
      geeglm(Response ~ Time + Treatment + X1,
             data = dat, id = Subject, family = gaussian, corstr = corstr),
      error = function(e) NULL
    )
    if (!is.null(gee)) {
      cf <- summary(gee)$coefficients
      if ("Time" %in% rownames(cf)) {
        out$gee_est <- cf["Time","Estimate"]
        out$gee_se  <- cf["Time","Std.err"]
        out$gee_ci  <- get_wald(out$gee_est, out$gee_se)
      }
    }
    
    out
  }
  
  bal <- fit_and_extract(balanced_data)
  imb <- fit_and_extract(imbalanced_data)
  
  data.frame(
    beta_time_true = beta_time_true,
    
    lmm_time_bal = bal$lmm_est, lmm_lo_bal = bal$lmm_ci[1], lmm_hi_bal = bal$lmm_ci[2],
    gee_time_bal = bal$gee_est, gee_lo_bal = bal$gee_ci[1], gee_hi_bal = bal$gee_ci[2],
    
    lmm_time_imb = imb$lmm_est, lmm_lo_imb = imb$lmm_ci[1], lmm_hi_imb = imb$lmm_ci[2],
    gee_time_imb = imb$gee_est, gee_lo_imb = imb$gee_ci[1], gee_hi_imb = imb$gee_ci[2]
  )
}

# ---- Run 200 reps ----
R <- 200
res <- do.call(rbind, lapply(1:R, function(r) simulate_one(seed = 5000 + r)))

safe_mean <- function(x) mean(x, na.rm = TRUE)
mse <- function(est, truth) safe_mean((est - truth)^2)
coverage <- function(lo, hi, truth) mean((lo <= truth) & (truth <= hi), na.rm = TRUE)
bias <- function(est, truth) safe_mean(est - truth)

beta_true <- unique(res$beta_time_true)

summary_tbl <- data.frame(
  Model = c("LMM", "GEE"),
  Bias_Balanced   = c(bias(res$lmm_time_bal, beta_true), bias(res$gee_time_bal, beta_true)),
  Bias_Imbalanced = c(bias(res$lmm_time_imb, beta_true), bias(res$gee_time_imb, beta_true)),
  MSE_Balanced    = c(mse(res$lmm_time_bal, beta_true), mse(res$gee_time_bal, beta_true)),
  MSE_Imbalanced  = c(mse(res$lmm_time_imb, beta_true), mse(res$gee_time_imb, beta_true)),
  Coverage_Balanced   = c(coverage(res$lmm_lo_bal, res$lmm_hi_bal, beta_true),
                          coverage(res$gee_lo_bal, res$gee_hi_bal, beta_true)),
  Coverage_Imbalanced = c(coverage(res$lmm_lo_imb, res$lmm_hi_imb, beta_true),
                          coverage(res$gee_lo_imb, res$gee_hi_imb, beta_true)),
  N_success_Balanced   = c(sum(!is.na(res$lmm_time_bal)), sum(!is.na(res$gee_time_bal))),
  N_success_Imbalanced = c(sum(!is.na(res$lmm_time_imb)), sum(!is.na(res$gee_time_imb)))
)

print(summary_tbl)

# Quick sanity check: how imbalanced are time counts?
# (Just to verify imbalance is extreme)
time_counts_imb <- table(res$beta_time_true)  # placeholder; see below if you want per-rep time counts
