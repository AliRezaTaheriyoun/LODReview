library(ggplot2)
library(ggtext)
library(viridis)
library(hrbrthemes)
library(devEMF)
wd <- paste("~/Library/CloudStorage/Box-Box/GWU/Research/",
            "Longitudinal Review/TeXfiles/",sep = "")
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
  labs(x="Year", y="No. of Documents") +  # Bold letter "b" only
  # Custom theme with axis lines and grid removal
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 7),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "in"),
    # Add x and y axis lines
    axis.line = element_line(size = .5, color = "black"),  # Black axis lines
    # Legend settings
    legend.position = c(0.2, 0.6),  # Position legend between top and middle left (x, y)
    legend.direction = "vertical",  # Vertical legend layout
    legend.title = element_blank(),  # Optionally, remove the legend title
    legend.text = element_text(size = 8),# Set legend font size to 9pt
    # Title customization
    legend.key.size = unit(.1,"in")#,
    # plot.title = element_markdown(hjust = -0.1)  # Adjust horizontal alignment
  )
ggsave(paste(wd,"some_visualizations/pubmed_ggplot.pdf",sep=""), #device = "eps",
       plot = pubmed_ggplot, width = 3, height = 1.5, units = "in")
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
