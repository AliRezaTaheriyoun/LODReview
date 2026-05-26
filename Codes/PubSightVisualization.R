library(ggplot2)
library(ggtext)
library(viridis)
library(hrbrthemes)
library(devEMF)
wd <- paste("~/Library/CloudStorage/Box-Box/",
            "Longitudinal_omics_review/TeXfiles/",sep = "")
setwd(wd)
pubsight <- read.table("report2/pubmed_data.tsv",header = T , sep = '\t')
colnames(pubsight) <- c("Year" , "Count" , "Main term" , "Omics related field")
# Small multiple
# ggplot(pubsight, aes(fill=`Omics related field`, y=Count, x=Year)) + 
#   geom_bar(position="stack", stat="identity") +
#   scale_fill_viridis(discrete = T) +
#   scale_x_continuous(breaks = round(seq(min(pubsight$Year), max(pubsight$Year), by = 5),1))+
#   labs(title = "<span style='font-size: 22pt;'>(a)</font>")+
#   theme(plot.title = element_markdown())
  # ggtitle("Growth in the longitudinal studies") +
  # theme_ipsum() +
  # xlab("")

##################USED in manuscript#################
# To save in '.emf' format uncomment the following two lines and 
# the 'dev.off()' at the end of ggplot
# emf(paste(wd,"New draft/pubmed_ggplot.emf",sep=""), width = 7, height = 3.5, 
#     units = "in")
# Small multiple with specified figure dimensions
pubmed_ggplot <- ggplot(data=pubsight, aes(fill=`Omics related field`, y=Count, x=Year)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = TRUE) +
  scale_x_continuous(breaks = round(seq(1965 , 2025, by = 10), 1)) +
  labs(x="Year", y="No. of Publications") +  # Bold letter "b" only
  # Custom theme with axis lines and grid removal
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 8),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "in"),
    # Add x and y axis lines
    axis.line = element_line(size = .2, color = "black"),  # Black axis lines
    # Legend settings
    legend.position = c(0.25, 0.55),  # Position legend between top and middle left (x, y)
    legend.direction = "vertical",  # Vertical legend layout
    legend.title = element_blank(),  # Optionally, remove the legend title
    legend.text = element_text(size = 6.5),# Set legend font size to 9pt
    # Title customization
    legend.key.size = unit(.05,"in")#,
    # plot.title = element_markdown(hjust = -0.1)  # Adjust horizontal alignment
  )
ggsave(paste(wd,"some_visualizations/pubmed_ggplot.pdf",sep=""), #device = "eps",
       plot = pubmed_ggplot, width = 2.25, height = 1.4, units = "in")
scopus <- read.table("report/Scopus_data.csv",header = T , sep = ',')
colnames(scopus) <- c("Year" , "Count" , "Main term" , "Omics related field")
scopus <- scopus[scopus$Year != 2024,]
emf(paste(wd,"New draft/scopus_ggplot.emf",sep=""), width = 7, height = 3.5, 
    units = "in")
# Small multiple with specified figure dimensions
ggplot(scopus, aes(fill=`Omics related field`, y=Count, x=Year)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = TRUE) +
  scale_x_continuous(breaks = round(seq(min(pubsight$Year), max(pubsight$Year), by = 5), 1)) +
  labs(title = "<span style='font-size: 22pt;'><b>b</b></span>") +  # Bold letter "b" only
  
  # Custom theme with axis lines and grid removal
  theme_minimal(base_size = 9) +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_blank(),  # Remove background color
    plot.background = element_blank(),   # Remove background outside the plot
    
    # Add x and y axis lines
    axis.line = element_line(size = .5, color = "black"),  # Black axis lines
    axis.text = element_text(size = 12),  # Set font size for x and y axis numbers to 7pt
    axis.title = element_text(size = 16),  # Set font size for x and y axis labels to 9pt
    
    # Legend settings
    legend.position = c(0.15, 0.65),  # Position legend between top and middle left (x, y)
    legend.direction = "vertical",  # Vertical legend layout
    legend.title = element_blank(),  # Optionally, remove the legend title
    legend.text = element_text(size = 9),  # Set legend font size to 9pt
    
    # Title customization
    plot.title = element_markdown(hjust = -0.1)  # Adjust horizontal alignment
  )
dev.off()
# Set figure size to 3.5 x 7 inches
ggsave(paste(wd,"New draft/scopus_ggplot.eps",sep=""), 
       width = 7, height = 3.5, units = "in")

############################################
# NORMALIZED NUMBER OF PUBLICATTIONS
############################################

library(ggplot2)
library(ggtext)
library(viridis)
# install.packages("~/Downloads/hrbrthemes_0.8.7.tar", repos = NULL, type = "source",dependencies = TRUE)
library(hrbrthemes)
library(devEMF)
# install.packages("rentrez")
library(rentrez)
library(dplyr)
wd <- paste("~/Library/CloudStorage/Box-Box/",
            "Longitudinal_omics_review/TeXfiles/",sep = "")
setwd(wd)
# Function: total PubMed records in a given year
# This uses PubMed’s PDAT (publication date) and returns only the count (no records downloaded).
pubmed_total_by_year <- function(year) {
  query <- sprintf('("%d"[PDAT] : "%d"[PDAT])', year, year)
  res <- entrez_search(db = "pubmed", term = query, retmax = 0)
  as.integer(res$count)
}
# Run it for a range of years (e.g., 1965–2025)
years <- c(1960,1964,1965:2026)
counts <- sapply(years, function(y) {
  message("Year: ", y)
  Sys.sleep(0.34)  # be polite to NCBI; adjust if you have an API key
  pubmed_total_by_year(y)
})
pubmed_yearly_totals <- data.frame(
  year = years,
  pubmed_total = counts
)
head(pubmed_yearly_totals)
write.table(pubmed_yearly_totals, paste0(wd,"report2026/pubmed_total_records_by_year.tsv"), 
            row.names = FALSE, sep = "\t" , col.names = T)

pubsight <- read.table("report2026/pubmed_data.tsv",header = T , sep = '\t')
colnames(pubsight) <- c("Year" , "Count" , "Main term" , "Omics related field")
# Ensure numeric
pubsight$Year  <- as.integer(pubsight$Year)
pubsight$Count <- as.numeric(pubsight$Count)

# --- (A) Load precomputed total PubMed records per year (recommended) ---
# This file should have columns: year, pubmed_total
totals <- pubmed_yearly_totals
colnames(totals) <- c("Year", "TotalPubMed")   # enforce names
totals$Year <- as.integer(totals$Year)
totals$TotalPubMed <- as.numeric(totals$TotalPubMed)
# --- Join totals and normalize ---
pubsight_norm <- pubsight %>%
  left_join(totals, by = "Year") %>%
  mutate(
    NormCount = Count / TotalPubMed
  )
# if any years missing totals, stop early so you don't plot wrong values
if (any(is.na(pubsight_norm$TotalPubMed))) {
  missing_years <- sort(unique(pubsight_norm$Year[is.na(pubsight_norm$TotalPubMed)]))
  stop("Missing TotalPubMed for years: ", paste(missing_years, collapse = ", "))
}

# --- Plot normalized stacked bars ---
pubmed_ggplot_norm <- ggplot(data = pubsight_norm,
                             aes(fill = `Omics related field`, y = NormCount, x = Year)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_viridis(discrete = TRUE) +
  scale_x_continuous(breaks = seq(1966, 2026, by = 10)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 0.01)) +
  labs(x = "Year", y = "No. Pub./Total PubMed (per year)") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 8),
    axis.title.y = element_text(size = 5),
    axis.text.x  = element_text(size = 6),
    axis.text.y  = element_text(size = 6),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "in"),
    axis.line = element_line(size = .2, color = "black"),
    legend.position = c(0.25, 0.55),
    legend.direction = "vertical",
    legend.title = element_blank(),
    legend.text = element_text(size = 6.5),
    legend.key.size = unit(.05, "in")
  )
ggsave(paste(wd,"some_visualizations/pubmed_ggplot_norm.pdf",sep=""), #device = "eps",
       plot = pubmed_ggplot_norm, width = 2.25, height = 1.4, units = "in")
