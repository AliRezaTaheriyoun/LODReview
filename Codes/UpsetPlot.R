library(ComplexUpset)
library(ggplot2)
library(dplyr)
wd <- "~/Library/CloudStorage/Box-Box/Longitudinal_omics_review/TeXfiles/"
out_path <- paste0(wd,"some_visualizations/")
setwd(wd) 
data <- read.csv("VENV/table-2.csv")
data <- na.omit(data)
# Ensure 0/1 columns are logical (ComplexUpset works best with TRUE/FALSE)
binary_to_logical <- function(df, cols) {
  df %>% mutate(across(all_of(cols), ~ as.logical(.x)))
}
make_upset <- function(df, cols, out_file, width=3, height=3) {
  df2 <- binary_to_logical(df, cols)
  
  p <- upset(
    df2,
    cols,
    name = "Intersection",
    sort_intersections_by = "cardinality",
    sort_sets = "descending",
    min_size = 1,                 # set >1 if you want to hide tiny intersections
    width_ratio = 0.2
  ) +
    theme_minimal(base_size = 10) +
    theme(
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  
  ggsave(out_file, p, width = width, height = height)
  p
}
# 1) linear vs nonlinear etc. (your cols 2,3,4,7,8)
cols1 <- colnames(data)[c(2, 3, 4, 7, 8)]
make_upset(data, cols1, paste0(out_path,"linearnonlinear_upset.svg"))

# 2) unbalanced/missing/high_dimensional etc. (your cols 5,7,8,9,10)
cols2 <- colnames(data)[c(5, 7, 8, 9, 10)]
make_upset(data, cols2, paste0(out_path,"balanced_upset.svg"))

# 3) software_code/data etc. (your cols 3,4,6,11,12)
cols3 <- colnames(data)[c(3, 4, 6, 11, 12)]
make_upset(data, cols3, paste0(out_path,"codedata_upset.svg"))

# 4) Bayesian/Frequentist etc. (your cols 7,8,13,14,15)
cols4 <- colnames(data)[c(7, 8, 13, 14, 15)]
make_upset(data, cols4, paste0(out_path,"BayesianFreq_upset.svg"))

# 5) DEA/Survival etc. (your cols 15,16,17,18,24)
cols5 <- colnames(data)[c(15, 16, 17, 18, 24)]
make_upset(data, cols5, paste0(out_path,"DEASurv_upset.svg"))

# 6) omics layers etc. (your cols 17,19,21,22,24)
cols6 <- colnames(data)[c(17, 19, 21, 22, 24)]
make_upset(data, cols6, paste0(out_path,"omics_upset.svg"))
