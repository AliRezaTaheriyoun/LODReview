library(plot.matrix)
library(devEMF)
m <- matrix(runif(n_row*n_col), nrow = n_row, ncol = n_col)
emf(paste(out_path,"some_visualizations/M1.emf",sep=""), width = 3.5, height = 3.5, 
    units = "in")
plot(m,xlab = "",  # Omit X-axis label
     ylab = "",  # Omit Y-axis label
     main = "")  # Omit title if any)
dev.off()


library(plot.matrix)
library(plotly)
library(devEMF)
out_path <- paste("/Users/alireza/Library/CloudStorage/Box-Box/GWU/Research/",
              "Longitudinal Review/TeXfiles/",sep = "")
# n_row<-7
# n_col<-5
set.seed(pi)
# m <- matrix(runif(n_row*n_col), nrow = n_row, ncol = n_col)
# 
# # Get x, y for the matrix plot (xy plane with colors)
# x <- rep(1:n_row, each = n_col)
# y <- rep(1:n_col, n_row)
# z_matrix <- rep(0, length(x))  # Set z = 0 for the flat matrix
# 
# # Get x and z values for the line plot (last column, mapped on z-axis)
# x_line <- 1:n_row
# z_line <- m[, n_col]
# 
# # Create 3D plot
# fig <- plot_ly()
# 
# # Add flat matrix on the xy plane with colors corresponding to matrix values
# fig <- fig %>% add_trace(
#   x = x, y = y, z = z_matrix, 
#   type = 'mesh3d', 
#   intensity = as.vector(m), 
#   colorscale = 'Viridis', 
#   showscale = TRUE,
#   contour = list(show = TRUE)
# )
# 
# # Add line plot for the last column values on z-axis
# fig <- fig %>% add_trace(
#   x = x_line, y = rep(n_col, n_row), z = z_line, 
#   type = 'scatter3d', mode = 'lines+markers', 
#   line = list(color = 'red', width = 5),
#   marker = list(size = 5)
# )
# 
# # Set axis labels
# fig <- fig %>% layout(scene = list(
#   xaxis = list(title = "X-axis"),
#   yaxis = list(title = "Y-axis"),
#   zaxis = list(title = "Z-axis (Last Column Values)")
# ))
# 
# # Show the plot
# fig
# emf(paste(out_path,"some_visualizations/M1.emf",sep=""), width = 3.5, height = 3.5, 
#     units = "in")
# plot(m,xlab = "",  # Omit X-axis label
#      ylab = "",  # Omit Y-axis label
#      main = "")  # Omit title if any)
# dev.off()
# 
# # Load necessary library
# library(plotly)
# 
# # Example 7x5 matrix
# n_row <- 7
# n_col <- 5
# m <- matrix(runif(n_row * n_col), nrow = n_row, ncol = n_col)
# 
# # Create 3D plot
# fig <- plot_ly()
# 
# # Add flat matrix on the xy plane with colors corresponding to matrix values
# fig <- fig %>% add_trace(
#   x = rep(1:n_row, each = n_col),  # X-coordinates
#   y = rep(1:n_col, n_row),  # Y-coordinates
#   z = rep(0, n_row * n_col),  # Z-coordinates set to 0 for flat matrix
#   type = 'scatter3d', 
#   mode = 'markers', 
#   marker = list(
#     size = 20,  # Size of the markers
#     color = as.vector(m),  # Color by matrix values
#     colorscale = 'Viridis',  # Color scale
#     colorbar = list(title = "Matrix Values")  # Color bar title
#   )
# )
# 
# # Get x and z values for the line plot (last column, mapped on z-axis)
# x_line <- 1:n_row
# z_line <- m[, n_col]
# 
# # Add line plot for the last column values on z-axis
# fig <- fig %>% add_trace(
#   x = x_line, 
#   y = rep(n_col + 0.5, n_row),  # Fixed y-value for the last column slightly above the matrix
#   z = z_line, 
#   type = 'scatter3d', mode = 'lines+markers', 
#   line = list(color = 'red', width = 5),
#   marker = list(size = 5)
# )
# 
# # Set axis labels
# fig <- fig %>% layout(scene = list(
#   xaxis = list(title = "X-axis"),
#   yaxis = list(title = "Y-axis"),
#   zaxis = list(title = "Z-axis (Last Column Values)")
# ))
# 
# # Show the plot
# fig
# 
# 
# # Load necessary library
# # Load necessary libraries
# library(plotly)
# library(viridis)
# 
# # Example 7x5 matrix
# n_row <- 7
# n_col <- 5
# m <- matrix(runif(n_row * n_col), nrow = n_row, ncol = n_col)
# 
# # Create 3D plot
# fig <- plot_ly()
# 
# # Add rectangles for the flat matrix on the xy plane
# for (i in 1:n_row) {
#   for (j in 1:n_col) {
#     # Get color based on matrix value
#     color <- viridis::viridis(1, alpha = 0.8, begin = 0, end = 1)[1] 
#     
#     # Create a filled rectangle
#     fig <- fig %>% add_trace(
#       x = c(i - 0.5, i + 0.5, i + 0.5, i - 0.5),  # X coordinates of rectangle corners
#       y = c(j - 0.5, j - 0.5, j + 0.5, j + 0.5),  # Y coordinates of rectangle corners
#       z = c(0, 0, 0, 0),  # Z coordinates for flat rectangle
#       type = 'mesh3d',
#       intensity = rep(m[i, j], 4),  # Color based on matrix value
#       colorscale = list(c(0, 1), viridis::viridis(100)),
#       showscale = FALSE
#     )
#   }
# }
# 
# # Get x and z values for the line plot (last column, mapped on z-axis)
# x_line <- 1:n_row
# z_line <- m[, n_col]
# 
# # Add line plot for the last column values on z-axis
# fig <- fig %>% add_trace(
#   x = x_line, 
#   y = rep(n_col + 0.5, n_row),  # Fixed y-value for the last column slightly above the matrix
#   z = z_line, 
#   type = 'scatter3d', mode = 'lines+markers', 
#   line = list(color = 'red', width = 5),
#   marker = list(size = 5)
# )
# 
# # Set axis labels
# fig <- fig %>% layout(scene = list(
#   xaxis = list(title = "X-axis"),
#   yaxis = list(title = "Y-axis"),
#   zaxis = list(title = "Z-axis (Last Column Values)")
# ))
# 
# # Show the plot
# fig
# 
# # Load necessary libraries
# library(plotly)
# library(viridis)
# 
# # Example 7x5 matrix
# n_row <- 7
# n_col <- 5
# m <- matrix(runif(n_row * n_col), nrow = n_row, ncol = n_col)
# 
# # Create 3D plot
# fig <- plot_ly()
# 
# # Define color scale based on matrix values
# colorscale <- viridis(100)  # Create a color scale with 100 colors
# 
# # Add rectangles for the flat matrix on the xy plane
# for (i in 1:n_row) {
#   for (j in 1:n_col) {
#     # Get color based on matrix value
#     value <- m[i, j]
#     color_index <- round(value * 99) + 1  # Scale to match color index
#     color <- colorscale[color_index]
#     
#     # Create a filled rectangle
#     fig <- fig %>% add_trace(
#       x = c(i - 0.5, i + 0.5, i + 0.5, i - 0.5),  # X coordinates of rectangle corners
#       y = c(j - 0.5, j - 0.5, j + 0.5, j + 0.5),  # Y coordinates of rectangle corners
#       z = c(0, 0, 0, 0),  # Z coordinates for flat rectangle
#       type = 'scatter3d',
#       mode = 'lines+text',
#       line = list(color = color, width = 1),  # Set border color for each rectangle
#       fill = list(color = color)  # Set fill color based on matrix value
#     )
#   }
# }
# 
# # Get x and z values for the line plot (last column, mapped on z-axis)
# x_line <- 1:n_row
# z_line <- m[, n_col]
# 
# # Add line plot for the last column values on z-axis
# fig <- fig %>% add_trace(
#   x = x_line, 
#   y = rep(n_col + 0.5, n_row),  # Fixed y-value for the last column slightly above the matrix
#   z = z_line, 
#   type = 'scatter3d', mode = 'lines+markers', 
#   line = list(color = 'red', width = 5),
#   marker = list(size = 5)
# )
# 
# # Set axis labels
# fig <- fig %>% layout(scene = list(
#   xaxis = list(title = "X-axis"),
#   yaxis = list(title = "Y-axis"),
#   zaxis = list(title = "Z-axis (Last Column Values)")
# ))
# 
# # Show the plotheatmap_data
# fig
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# library(plotly)
# library(viridis)
# 
# # Example 7x5 matrix
# n_row <- 7
# n_col <- 5
# m <- matrix(runif(n_row * n_col), nrow = n_row, ncol = n_col)
# 
# # Create 3D plot
# fig <- plot_ly()
# 
# # Define color scale based on matrix values
# colorscale <- viridis(100)  # Create a color scale with 100 colors
# 
# # Add filled rectangles for the flat matrix on the xy plane
# for (i in 1:n_row) {
#   for (j in 1:n_col) {
#     # Get the value and corresponding color
#     value <- m[i, j]
#     color_index <- round(value * 99) + 1  # Scale to match color index
#     color <- colorscale[color_index]
#     
#     # Create filled rectangle using add_surface
#     fig <- fig %>% add_surface(
#       z = matrix(c(0, 0, 0, 0, value, value, value, value), nrow = 2),  # Height of rectangle based on value
#       x = matrix(c(i - 0.5, i - 0.5, i + 0.5, i + 0.5), nrow = 2),  # X coordinates
#       y = matrix(c(j - 0.5, j + 0.5, j + 0.5, j - 0.5), nrow = 2),  # Y coordinates
#       colorscale = list(c(0, 1), c(color, color)),  # Fill color based on matrix value
#       showscale = FALSE  # Don't show scale for individual rectangles
#     )
#   }
# }
# 
# # Get x and z values for the line plot (last column, mapped on z-axis)
# x_line <- 1:n_row
# z_line <- m[, n_col]
# 
# # Add line plot for the last column values on z-axis
# fig <- fig %>% add_trace(
#   x = x_line, 
#   y = rep(n_col + 0.5, n_row),  # Fixed y-value for the last column slightly above the matrix
#   z = z_line, 
#   type = 'scatter3d', mode = 'lines+markers', 
#   line = list(color = 'red', width = 5),
#   marker = list(size = 5)
# )
# 
# # Set axis labels
# fig <- fig %>% layout(scene = list(
#   xaxis = list(title = "X-axis"),
#   yaxis = list(title = "Y-axis"),
#   zaxis = list(title = "Z-axis (Last Column Values)")
# ))
# 
# # Show the plot
# fig
# 
# 
# 
# 
# 
# 
# 
# # Load necessary libraries
# library(plotly)
# library(viridis)
# 
# # Example 7x5 matrix
# n_row <- 7
# n_col <- 5
# m <- matrix(runif(n_row * n_col), nrow = n_row, ncol = n_col)
# 
# # Create a 3D plot
# fig <- plot_ly()
# 
# # Define color scale based on matrix values
# colorscale <- viridis(100)  # Create a color scale with 100 colors
# 
# # Add filled rectangles for the matrix on the xy plane
# for (i in 1:n_row) {
#   for (j in 1:n_col) {
#     # Get the value and corresponding color
#     value <- m[i, j]
#     color_index <- round(value * 99) + 1  # Scale to match color index
#     color <- colorscale[color_index]
#     
#     # Define rectangle vertices for the filled rectangle
#     x_rect <- c(i - 0.5, i + 0.5, i + 0.5, i - 0.5)
#     y_rect <- c(j - 0.5, j - 0.5, j + 0.5, j + 0.5)
#     z_rect <- rep(0, 4)  # Set z to 0 for flat placement on the XY plane
#     
#     # Create a surface to represent the filled rectangle
#     fig <- fig %>% add_trace(
#       x = x_rect, 
#       y = y_rect, 
#       z = c(0, 0, value, value), 
#       type = 'scatter3d', 
#       mode = 'lines', 
#       fill = 'toself',
#       fillcolor = color,
#       line = list(color = color)
#     )
#   }
# }
# 
# # Get x and z values for the line plot (last column, mapped on z-axis)
# x_line <- 1:n_row
# z_line <- m[, n_col]
# 
# # Add line plot for the last column values on z-axis
# fig <- fig %>% add_trace(
#   x = x_line, 
#   y = rep(n_col + 0.5, n_row),  # Fixed y-value for the last column slightly above the matrix
#   z = z_line, 
#   type = 'scatter3d', mode = 'lines+markers', 
#   line = list(color = 'red', width = 5),
#   marker = list(size = 5)
# )
# 
# # Set axis labels
# fig <- fig %>% layout(scene = list(
#   xaxis = list(title = "X-axis"),
#   yaxis = list(title = "Y-axis"),
#   zaxis = list(title = "Z-axis (Last Column Values)")
# ))
# 
# # Show the plot
# fig
# 


library(plot.matrix)
library(devEMF)
# Define the output path
# out_path <- paste("/Users/alireza/Library/CloudStorage/Box-Box/GWU/Research/",
#                   "Longitudinal Review/TeXfiles/", sep = "")
out_path <- paste("C:/Users/stata/Box/GWU/Research/",
                  "Longitudinal Review/TeXfiles/", sep = "")

#######################################
#######################################
#######################################
#######################################
#######################################
#######################################
library(ggplot2)
library(tidyr)
library(dplyr)
library(patchwork)
library(omicsArt)
library(cowplot)
wd<-paste("~/Library/CloudStorage/Box-Box/GWU/Research/",
          "Longitudinal Review/TeXfiles/some_visualizations/",sep="")
combinedplot <- function(m,n_row,n_col,linelegend=F,linexlabel=F,lineylabel=F,
                         heatytick=F,heatlegend=F,heatxlabel=F,heatylabel=F){
  # Convert the matrix to a data frame for ggplot
  df <- as.data.frame(m)
  colnames(df) <- paste0("TP_", 0:(n_col - 1))  # Label columns as "TP_0", "TP_1", etc.
  df$Feature <- factor(1:n_row, levels = 1:n_row)  # Add feature identifiers
  
  # Long-format data for ggplot
  df_long <- df %>%
    pivot_longer(cols = starts_with("TP_"), names_to = "TimePoint", values_to = "Abundance") %>%
    mutate(TimePoint = as.numeric(sub("TP_", "", TimePoint)))
  df_long <- df_long %>%
    # mutate(Feature = paste0("Feature", Feature))
    mutate(Feature = factor(paste0("Feature", Feature), levels = paste0("Feature", 1:n_row)))
  # Define colors
  colors <- rainbow(n_row)
  
  # Line plot (top visualization)
  line_plot <-ggplot(df_long, aes(x = TimePoint, y = Abundance, group = Feature, color = Feature)) +
    geom_line(size = .5) +
    geom_point(size = 1) +
    scale_color_manual(values = colors) +
    theme_minimal() +
    # theme_omicsEye() +  # Assuming this theme is defined elsewhere
    theme(
      panel.border = element_rect(color = "black", fill = NA, size = .2), # Add a black box around the plot
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.x=element_blank(),
      axis.ticks.y=element_blank(),
      legend.position = "none",
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      plot.margin = margin(t=0, r=0, b=0, l=0,unit = "in")  # No margin
    )
  if(linexlabel){
    line_plot <- line_plot+labs(x="Time")+
      theme(axis.title.x=element_text(size = 9),
            axis.text.x = element_text(size = 6))
  }
  if(lineylabel){
    line_plot <- line_plot+labs(y="Abundance/Relative Abundance")+
      theme(axis.title.y=element_text(size = 9,angle = 90))
  }
  if(linelegend){
    line_plot <- line_plot + theme(
      legend.position = "right",
      legend.key.width = unit(0.4, "in"), 
      legend.key.height = unit(0.2, "in"),
      legend.key.size = unit(0, "lines"),
      legend.text = element_text(size=8))
  }
  
  heatmap_data <- df_long %>%
    # mutate(Feature = factor(Feature, levels = rev(levels(Feature))))  # Reverse order for heatmap
    mutate(Feature = factor(Feature, levels = rev(levels(Feature))))  # Reverse order for heatmap
  
  
  # Heatmap (bottom visualization)
  heatmap <- ggplot(heatmap_data, aes(x = TimePoint, y = Feature, fill = Abundance)) +
    geom_tile() +
    scale_fill_gradientn(colors = heat.colors(100)) +
    theme_minimal() +
    theme(
      panel.border = element_rect(color = "black", fill = NA, size = .2), # Add a black box around the plot
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.x=element_blank(),
      axis.ticks.y=element_blank(),
      legend.position = "none",
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      plot.margin = margin(t=0, r=0, b=0, l=0,unit = "in")  # No margin
    )
    if(heatxlabel){
      heatmap <- heatmap+labs(x="Time", fill = "Abundance")+
        theme(axis.title.x=element_text(size = 9),
              axis.text.x=element_text(size = 6))
    }
  if(heatylabel){
    heatmap <- heatmap+labs(y="")+
      theme(axis.title.y=element_text(size = 9),
            axis.text.y=element_text(size = 6))
  }
  if(heatlegend){
    heatmap <- heatmap + theme(
      legend.position = "right",
      legend.key.width = unit(0.4, "in"), 
      legend.key.height = unit(0.2, "in"),
      legend.key.size = unit(0, "lines"),
      legend.text = element_text(size = 8))
  }
  if(heatytick){
    heatmap <- heatmap + theme(
      axis.text.y =  element_text(size = 6))
  }
    # labs(x = "Time", fill = "Abundance")
  
  # Combine the plots with no space between
  combined_plot <- line_plot / heatmap + plot_layout(heights = c(1, 2)#, 
                                                     # guides = "collect"
                                                     )
  return(combined_plot)
}

set.seed(pi)
n_row <- 7
n_col <- 5
m1 <- matrix(c(rnorm(n_row * n_col-2*n_row),rnorm(n_row,3,1),rnorm(n_row,-1,2))+10, nrow = n_row, ncol = n_col)
m2 <- matrix(c(rnorm(n_row * n_col-n_row),rnorm(n_row,2,1))+10, nrow = n_row, ncol = n_col)
m3 <- matrix(c(rnorm(n_row * n_col))+10, nrow = n_row, ncol = n_col)
m4 <- matrix(c(rnorm(n_row * n_col-n_row),rnorm(n_row,2,1))+10, nrow = n_row, ncol = n_col)

combined_plot1<-combinedplot(m1,n_row,n_col,linelegend=F,linexlabel=F,lineylabel=F
                             ,heatytick=F,heatlegend=F,heatxlabel=T,heatylabel=F)
combined_plot2<-combinedplot(m2,n_row,n_col,linelegend=F,linexlabel=F,lineylabel=F
                             ,heatytick=F,heatlegend=F,heatxlabel=T,heatylabel=F)
combined_plot3<-combinedplot(m3,n_row,n_col,linelegend=F,linexlabel=F,lineylabel=F
                             ,heatytick=F,heatlegend=F,heatxlabel=T,heatylabel=F)
combined_plot4<-combinedplot(m4,n_row,n_col,linelegend=F,linexlabel=F,lineylabel=F
                             ,heatytick=F,heatlegend=F,heatxlabel=T,heatylabel=F)
tick<-combinedplot(m4,n_row,n_col,linelegend=T,linexlabel=F,lineylabel=F
                             ,heatytick=T,heatlegend=F,heatxlabel=T,heatylabel=T)
legend<-combinedplot(m4,n_row,n_col,linelegend=T,linexlabel=F,lineylabel=F
                     ,heatytick=T,heatlegend=F,heatxlabel=T,heatylabel=T)
ggsave(
  filename = paste(wd, "legend.pdf", sep = ""),
  plot = legend,
  width = 7.2/4, height = 4, units = "in"
)
ggsave(
  filename = paste(wd, "tick.pdf", sep = ""),
  plot = tick,
  width = 7.2/4, height = 1.8, units = "in"
)
ggsave(
  filename = paste(wd, "combined_plot1.pdf", sep = ""),
  plot = combined_plot1,
  width = 7.2/7, height = 1.8, units = "in"
)
ggsave(
  filename = paste(wd, "combined_plot2.pdf", sep = ""),
  plot = combined_plot2,
  width = 7.2/7, height = 1.8, units = "in"
)
ggsave(
  filename = paste(wd, "combined_plot3.pdf", sep = ""),
  plot = combined_plot3,
  width = 7.2/7, height = 1.8, units = "in"
)
ggsave(
  filename = paste(wd, "combined_plot4.pdf", sep = ""),
  plot = combined_plot4,
  width = 7.2/7, height = 1.8, units = "in"
)
library(cowplot)
legend <- cowplot::get_legend(combined_plot4)
LOD <- combined_plot1 + combined_plot2 +combined_plot3+combined_plot4+
  plot_layout(ncol = 4)
LOD <- plot_grid(
  combined_plot1,# + theme(plot.margin = margin(0, .1, .1, .1)),
  combined_plot2,# + theme(plot.margin = margin(.1, .1, .1, .1)),
  combined_plot3,# + theme(plot.margin = margin(.1, .1, .1, .1)),
  combined_plot4,# + theme(plot.margin = margin(.1, .1, .1, .1)),#+ 
    # theme(
    #   legend.position = "right",
    #   legend.key.width = unit(0.4, "in"), 
    #   legend.key.height = unit(0.2, "in"),
    #   legend.key.size = unit(0, "lines")),
  # + theme(legend.position = c(.01, 0.8),
  #                        legend.box = "vertical",
  #                        legend.direction = "vertical",
  #                        legend.text = element_text(size = 7),
  #                        legend.title=element_blank(),
  #                        legend.spacing.y = unit(0, "lines"),
  #                        axis.title.x = element_text(size = 8),
  #                        axis.title.y = element_text(size = 8))+
  #   guides(fill = guide_legend(ncol = 2), color = guide_legend(ncol = 2), 
  #          linetype = guide_legend(ncol = 2)),
  # labels = c("a", "b", "c", "d", "e" , "f", "g", "h", "i", "j",
  #            "k", "l", "m", "n", "o", "p", "q", "r", "s", "t",
  #            "u", "v","w","x","y","z"),  # Labels for each plot
  # label_size = 10,                         # Font size for labels
  # label_fontface = "bold",                 # Boldface for labels
  ncol = 4
  # align = "hv", # Align horizontally
  # axis = "tb",  # Align top and bottom axes
  # rel_widths = c(1, 1, 1, 1) # Equal widths for all plots
)
# LOD <- plot_grid(LOD, legend, ncol = 2, rel_heights = c(0.9, 0.1))

ggsave(
  filename = paste(wd, "LOD.pdf", sep = ""),
  plot = LOD,
  width = 7.2, height = 4, units = "in"
)
#######################################
#######################################
#######################################
#######################################
#######################################
#######################################
#######################################
set.seed(pi)
library(pracma)
t<-c(0,0,0,0,1,1,2,2,2,3,3,3,4)
n_row <- 7
n_col <- 13
m<-matrix(rep(0,n_row*n_col),n_row,n_col)
rndmean<-runif(n_row,-3,3)
for (i in 1:n_row) {
  m[i,]=c(rnorm(4,0+rndmean[i],1),rnorm(2,4+rndmean[i],.5),
          rnorm(3,-1+rndmean[i],1), rnorm(3,2+rndmean[i],1),
          rnorm(1,4+rndmean[i],1))
}
# Open the emf device before setting up the plot
library(devEMF)
emf(paste(out_path,"timecourse.emf",sep=""), width = 8, height = 3.5)
# layout(matrix(c(1, 2), nrow = 2), heights = c(1, 2))  # Adjust heights as necessary
colors <- rainbow(n_row)  # Use the 'rainbow()' palette for distinct colors
plot(t, m[1,], type = "n", ylim = c(min(m)-.1, max(m) + 0.1), 
     yaxt = 'n', xlab = "Time-points", ylab = "Relative bundance",cex.lab = 2.8,   # Increases the font size of x and y labels
     cex.axis = 2.5)  # Set up the empty plot
for (i in 1:n_row) {
  lines(t, m[i,], col = colors[i], lwd = 3,type="p", pch=20)
  # points(0:(n_col-1), m[i,], col = colors[i], pch = 32)
}
time<-matrix(rep(t,n_row),nrow = n_row, ncol = n_col,byrow = T)
resp<-Reshape(m,n_row*n_col,1)
covar<-Reshape(time,n_row*n_col,1)
newdata<-data.frame(cbind(covar,resp))
colnames(newdata)<-c("covar","resp")
meanvalues <- aggregate(resp~covar,data=newdata,FUN=mean)
lines(meanvalues$covar,meanvalues$resp,lwd=3,lty="dashed",col="red")
dev.off()

###################
###################
library(ggplot2)
library(tidyr)
library(dplyr)

# Simulate data
set.seed(pi)
t <- c(0, 0, 0, 0, 1, 1, 2, 2, 2, 3, 3, 3, 4)
n_row <- 7
n_col <- 13
m <- matrix(rep(0, n_row * n_col), n_row, n_col)
rndmean <- runif(n_row, -3, 3)

for (i in 1:n_row) {
  m[i, ] <- c(
    rnorm(4, 0 + rndmean[i], 1),
    rnorm(2, 4 + rndmean[i], 0.5),
    rnorm(3, -1 + rndmean[i], 1),
    rnorm(3, 2 + rndmean[i], 1),
    rnorm(1, 4 + rndmean[i], 1)
  )
}

# Reshape data for ggplot
time <- rep(t, times = n_row)
feature <- rep(paste0("Feature_", 1:n_row), each = n_col)
abundance <- as.vector(m)

df <- data.frame(TimePoint = time, Feature = feature, Abundance = abundance)

# Calculate mean values for dashed line
mean_values <- df %>%
  group_by(TimePoint) %>%
  summarize(MeanAbundance = mean(Abundance), .groups = "drop")

# Define colors for the features
colors <- rainbow(n_row)

# Create ggplot
timecourse <- ggplot(df, aes(x = TimePoint, y = Abundance, group = Feature, color = Feature)) +
  # geom_line(size = 1) +                            # Individual feature lines
  geom_point(size = 1) +                           # Individual feature points
  scale_color_manual(values = colors) +           # Custom colors
  geom_line(data = mean_values,                   # Mean line
            aes(x = TimePoint, y = MeanAbundance),
            inherit.aes = FALSE,                  # Prevent aesthetics inheritance
            color = "red", linetype = "dashed", size = .8) +
  labs(x = "Time", y = "Abundance/Relative Abundance") +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = .2), # Add a black box around the plot
    axis.title.x=element_text(size = 9),
    axis.text.x=element_text(size = 9),
    axis.title.y=element_text(size = 9),
    axis.text.y=element_text(size = 9),
    axis.ticks.x=element_blank(),
    axis.ticks.y=element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.margin = margin(t=0, r=0, b=0, l=0,unit = "in"),  # No margin
    legend.position = "right",
    legend.key.width = unit(0.4, "in"), 
    legend.key.height = unit(0.2, "in"),
    legend.key.size = unit(0, "lines"),
    legend.title = element_blank()
  )
ggsave(
  filename = paste(wd, "timecourse.pdf", sep = ""),
  plot = timecourse,
  width = 4, height = 1.9, units = "in"
)
