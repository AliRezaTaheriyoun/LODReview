library(dplyr)
library(ggVennDiagram)  # Load the ggVennDiagram package
library(svglite)        # Load the svglite package for SVG output
library(ggplot2)

wd <- "~/Library/CloudStorage/Box-Box/Longitudinal_omics_review/TeXfiles/VENV/"
output_Path <- paste(wd)
setwd(wd)
data <- read.csv("table-2.csv")
data <- na.omit(data[1:27])

# Create a list of entries for each category
x1 <- list()
for (col in colnames(data)[c(2, 3, 4, 7, 8)]) {
  x1[[col]] <- data$entry[data[[col]] == 1]
}

# Create the Venn diagram using ggVennDiagram
venn_plot <- ggVennDiagram(
  x1,  # List of sets
  label = "both",  # Show both count and percentage
  label_alpha = 0,  # Transparent background for labels
  category.names = names(x1),  # Use the names of the list as category labels
  set_size = 5,  # Size of set labels
  label_size = 5  # Size of intersection labels
) +
  scale_fill_gradient(low = "white", high = "blue") +  # Customize fill colors
  theme(legend.position = "none")  # Remove the legend

# Save the plot as an SVG file
ggsave("linearnonlinear_ggplot.svg", venn_plot, width = 7.2, height = 7.2)
print(venn_plot)

#######
x2 <- list()
for (col in colnames(data)[c(5,7,8,9,10)]) {
  x2[[col]] <- data$entry[data[[col]] == 1]
}

# Create the Venn diagram using ggVennDiagram
venn_plot <- ggVennDiagram(
  x2,  # List of sets
  label = "both",  # Show both count and percentage
  label_alpha = 0,  # Transparent background for labels
  category.names = names(x2),  # Use the names of the list as category labels
  set_size = 5,  # Size of set labels
  label_size = 5  # Size of intersection labels
) +
  scale_fill_gradient(low = "white", high = "blue") +  # Customize fill colors
  theme(legend.position = "none")  # Remove the legend

# Save the plot as an SVG file
ggsave("balanced_ggplot.svg", venn_plot, width = 7.2, height = 7.2)

#######
x3 <- list()
for (col in colnames(data)[c(3,4,6,11,12)]) {
  x3[[col]] <- data$entry[data[[col]] == 1]
}

# Create the Venn diagram using ggVennDiagram
venn_plot <- ggVennDiagram(
  x3,  # List of sets
  label = "both",  # Show both count and percentage
  label_alpha = 0,  # Transparent background for labels
  category.names = names(x3),  # Use the names of the list as category labels
  set_size = 5,  # Size of set labels
  label_size = 5  # Size of intersection labels
) +
  scale_fill_gradient(low = "white", high = "blue") +  # Customize fill colors
  theme(legend.position = "none")  # Remove the legend

# Save the plot as an SVG file
ggsave("codedata_ggplot.svg", venn_plot, width = 7.2, height = 7.2)
#######
x4 <- list()
for (col in colnames(data)[c(7,8,13,14,15)]) {
  x4[[col]] <- data$entry[data[[col]] == 1]
}

# Create the Venn diagram using ggVennDiagram
venn_plot <- ggVennDiagram(
  x4,  # List of sets
  label = "both",  # Show both count and percentage
  label_alpha = 0,  # Transparent background for labels
  category.names = names(x4),  # Use the names of the list as category labels
  set_size = 5,  # Size of set labels
  label_size = 5  # Size of intersection labels
) +
  scale_fill_gradient(low = "white", high = "blue") +  # Customize fill colors
  theme(legend.position = "none")  # Remove the legend

# Save the plot as an SVG file
ggsave("BayesianFreq_ggplot.svg", venn_plot, width = 7.2, height = 7.2)
#######
x5 <- list()
for (col in colnames(data)[c(15,16,17,18,24)]) {
  x5[[col]] <- data$entry[data[[col]] == 1]
}

# Create the Venn diagram using ggVennDiagram
venn_plot <- ggVennDiagram(
  x5,  # List of sets
  label = "both",  # Show both count and percentage
  label_alpha = 0,  # Transparent background for labels
  category.names = names(x5),  # Use the names of the list as category labels
  set_size = 5,  # Size of set labels
  label_size = 5  # Size of intersection labels
) +
  scale_fill_gradient(low = "white", high = "blue") +  # Customize fill colors
  theme(legend.position = "none")  # Remove the legend

# Save the plot as an SVG file
ggsave("DEASurv_ggplot.svg", venn_plot, width = 7.2, height = 7.2)
#######
x6 <- list()
for (col in colnames(data)[c(17,19, 21, 22, 24)]) {
  x6[[col]] <- data$entry[data[[col]] == 1]
}

# Create the Venn diagram using ggVennDiagram
venn_plot <- ggVennDiagram(
  x6,  # List of sets
  label = "both",  # Show both count and percentage
  label_alpha = 0,  # Transparent background for labels
  category.names = names(x6),  # Use the names of the list as category labels
  set_size = 5,  # Size of set labels
  label_size = 5  # Size of intersection labels
) +
  scale_fill_gradient(low = "white", high = "blue") +  # Customize fill colors
  theme(legend.position = "none")  # Remove the legend

# Save the plot as an SVG file
ggsave("omics_ggplot.svg", venn_plot, width = 7.2, height = 7.2)


