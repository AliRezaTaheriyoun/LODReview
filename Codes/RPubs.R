library(labelled)   # labeling data
library(rstatix)    # summary statistics
library(ggpubr)     # convenient summary statistics and plots
library(GGally)     # advanced plot
library(car)        # useful for anova/wald test
library(Epi)        # easy getting CI for model coef/pred
library(lme4)       # linear mixed-effects models
library(lmerTest)   # test for linear mixed-effects models
library(emmeans)    # marginal means
library(multcomp)   # CI for linear combinations of model coef
library(geepack)    # generalized estimating equations
library(ggeffects)  # marginal effects, adjusted predictions
library(gt)         # nice tables

library(tidyverse)  # for everything (data manipulation, visualization, coding, and more)
theme_set(theme_minimal() + theme(legend.position = "bottom")) # theme for ggplot
#load("data/dental.RData")
load(url("http://alecri.github.io/downloads/data/dental.RData"))

head(dental)
dental_long <- pivot_longer(dental, cols = starts_with("y"), 
                            names_to = "measurement", values_to = "distance") %>% 
  mutate(
    age = parse_number(measurement),
    measurement = fct_inorder(paste("Measure at age", age))
  ) %>% 
  set_variable_labels(
    age = "Age of the child at measurement",
    measurement = "Label for time measurement",
    distance = "Measurement"
  )

head(dental_long)
group_by(dental_long, age) %>% 
  get_summary_stats(distance)
ggplot(dental_long, aes(measurement, distance, fill = measurement)) +
  geom_boxplot() +
  geom_jitter(width = 0.2) +
  guides(fill = "none") +
  labs(x = "", y = "Dental growth, mm")
group_by(dental_long, sex, measurement) %>% 
  get_summary_stats(distance, show = c("mean", "sd"))
ggplot(dental_long, aes(sex, distance, fill = measurement)) +
  geom_boxplot() +
  labs(x = "", y = "Dental growth, mm", fill = "")
group_by(dental_long, sex, measurement) %>% 
  summarise(mean_distance = mean(distance), .groups = "drop") %>% 
  ggplot(aes(sex, mean_distance, fill = measurement, label = round(mean_distance))) +
  geom_col(position = "dodge") +
  geom_text(position = position_dodge(width = 0.9), vjust = -0.5) +
  coord_flip() +
  labs(x = "", y = "Mean Dental growth, mm", fill = "")
# co-variance matrix
cov_obs <- select(dental, starts_with("y")) %>% 
  cov()
cov_obs
# correlation matrix
cov2cor(cov_obs)
ggpairs(select(dental, starts_with("y")), lower = list(continuous = "smooth"))
ggpairs(dental, mapping = aes(colour = sex), columns = 3:6,
        lower = list(continuous = "smooth"))
group_by(dental_long, sex, age) %>% 
  summarise(mean = list(mean_ci(distance)), .groups = "drop") %>% 
  unnest_wider(mean) %>% 
  mutate(agex = age - .05 + .05*(sex == "Boy")) %>% 
  ggplot(aes(agex, y, col = sex, shape = sex)) +
  geom_point() +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2) +
  geom_line() +
  labs(x = "Age, years", y = "Mean Dental growth, mm", shape = "Sex", col = "Sex")

# Fitting model in R
# 
# Linear mixed-effects models can be fitted using the lmer function in the lme4 package.
# The first part of the formula model specify the fixed effects part of the model, while the second in parantheses specify the random components.

lin_0 <- lmer(distance ~ 1 + (1 | id), data = dental_long)
summary(lin_0)
ci.lin(lin_0)
# The estimated marginal mean of the dental distance is  mm.
# The estimated variance of the random-effect reflecting between-subject variability is 3.752; the estimated variance of the error term reflecting within-subject variability is 4.930. The correlation between any two repeated measures (ICC) is equal to .

# Variance components can be tested using an ANOVA-like table that can be derived using the ranova function in the lmer test.

ranova(lin_0)


ggplot(dental_long, aes(id, distance)) +
  geom_point(aes(col = measurement, shape = measurement)) +
  geom_point(data = group_by(dental_long, id) %>%
               summarise(distance = mean(distance), .groups = "drop"),
             aes(col = "Mean", shape = "Mean"), size = 2.5) +
  geom_hline(yintercept = mean(dental_long$distance)) +
  scale_shape_manual(values = c(4, 19, 19, 19, 19)) +
  labs(x = "Child id", y = "Dental growth, mm", col = "Measurement", shape = "Measurement")
lin_age <- lmer(distance ~ measurement + (1 | id), data = dental_long)
summary(lin_age)
# The emmeans function can be usefull for computing marginal means with corresponding confidence intervals.

tidy(emmeans(lin_age, "measurement"), conf.int = TRUE)
dental_fit <- bind_cols(
  dental_long, pred_age = predict(lin_age, re.form = ~ 0)
)
ggplot(dental_fit, aes(age, distance)) +
  geom_line(aes(group = factor(id))) +
  geom_point(aes(y = pred_age), col = "blue", size = 2) + 
  labs(x = "Age, years", y = "Dental growth, mm")
lin_agesex <- lmer(distance ~ measurement + sex + (1 | id), data = dental_long)
summary(lin_agesex)
ci.lin(lin_agesex)
tidy(emmeans(lin_agesex, c("measurement", "sex")), conf.int = TRUE)
dental_fit$pred_agesex <- predict(lin_agesex, re.form = ~ 0)
ggplot(dental_fit, aes(age, distance)) +
  geom_line(aes(group = factor(id))) +
  geom_point(aes(y = pred_agesex, col = sex), size = 2) + 
  labs(x = "Age, years", y = "Dental growth, mm", col = "Sex")
lin_agesexinter <- lmer(distance ~ measurement*sex + (1 | id), data = dental_long)
lin_agesexinter
Anova(lin_agesexinter, type = 3)
tidy(emmeans(lin_agesexinter, c("measurement", "sex")), conf.int = TRUE)
dental_fit$pred_agesexinter <- predict(lin_agesexinter, re.form = ~ 0)
ggplot(dental_fit, aes(age, distance)) +
  geom_line(aes(group = factor(id))) +
  geom_point(aes(y = pred_agesexinter, col = sex), size = 2) + 
  labs(x = "Age, years", y = "Dental growth, mm", col = "Sex")
lin_agecsexinter <- lmer(distance ~ sex*age + (1 | id), data = dental_long)
summary(lin_agecsexinter)
lin_agec <- lmer(distance ~ age + (1 | id), data = dental_long)
lin_agec
sid <- c(10, 21)
expand.grid(
  age = seq(8, 14, .5),
  id = sid
) %>% 
  bind_cols(
    indiv_pred = predict(lin_agec, newdata = .),
    marg_pred = predict(lin_agec, newdata = ., re.form = ~ 0)
  ) %>% 
  left_join(
    filter(dental_long, id %in% sid), by = c("id", "age")
  ) %>% 
  ggplot(aes(age, indiv_pred, group = id, col = factor(id))) +
  geom_line() +
  geom_point(aes(y = distance)) +
  geom_line(aes(y = marg_pred, col = "Marginal"), lwd = 1.5) +
  labs(x = "Age, years", y = "Dental growth, mm", col = "Curve")
lin_agecr <- lmer(distance ~ age + (age | id), data = dental_long)
summary(lin_agecr)
VarCorr(lin_agecr)
as.data.frame(VarCorr(lin_agecr))
expand.grid(
  age = seq(8, 14, .5),
  id = unique(dental_long$id)
) %>% 
  bind_cols(
    indiv_pred = predict(lin_agecr, newdata = .),
    marg_pred = predict(lin_agecr, newdata = ., re.form = ~ 0)
  ) %>% 
  ggplot(aes(age, indiv_pred, group = id)) +
  geom_line(col = "grey") +
  geom_line(aes(y = marg_pred), col = "blue", lwd = 1.5) +
  labs(x = "Age, years", y = "Dental growth, mm")
lin_agecsexinterr <- lmer(distance ~ age*sex + (age | id), data = dental_long)
summary(lin_agecsexinterr)
expand.grid(
  age = seq(8, 14, .5),
  id = unique(dental_long$id)
) %>% 
  left_join(select(dental, sex, id), by = "id") %>% 
  bind_cols(
    indiv_pred = predict(lin_agecsexinterr, newdata = .),
    marg_pred = predict(lin_agecsexinterr, newdata = ., re.form = ~ 0)
  ) %>% 
  ggplot(aes(age, indiv_pred, group = id)) +
  geom_line(col = "grey") +
  geom_line(aes(y = marg_pred, col = sex), lwd = 1.5) +
  labs(x = "Age, measurements", y = "Mean Dental growth, mm")
gee_inter <- geeglm(distance ~ sex*age, data = dental_long,
                    id = id, family = gaussian, corstr = "exchangeable")
summary(gee_inter)
tidy(gee_inter, conf.int = TRUE)
K <- rbind(
  "mean response comparing boys vs. girls" = c(0, 1, 0, 0),
  "mean distance increases every 1 year increase of age among girls" = c(0, 0, 1, 0),
  "mean distance increases every 1 year increase of age among boys" = c(0, 0, 1, 1)
)
tidy(glht(gee_inter, linfct = K), conf.int = TRUE) %>% 
  gt() %>% 
  fmt_number(columns = -1,decimals = 2)
pred_geeinter <- ggpredict(gee_inter, terms = c("age", "sex"))
ggplot(pred_geeinter, aes(x, predicted, col = group)) + 
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = .2, col = NA) +
  labs(x = "Age, years", y = "Dental growth, mm", col = "Sex", fill = "Sex")
gee_inter_exch <- gee_inter 
gee_inter_ind <- geeglm(distance ~ sex*age, data = dental_long,
                        id = id, family = gaussian, corstr = "independence")
gee_inter_unstr <- geeglm(distance ~ sex*age, data = dental_long,
                          id = id, family = gaussian, corstr = "unstructured")
gee_inter_ar1 <- geeglm(distance ~ sex*age, data = dental_long,
                        id = id, family = gaussian, corstr = "ar1")
# comparison
QIC(gee_inter_exch, gee_inter_ind, gee_inter_unstr, gee_inter_ar1)
